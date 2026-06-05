% ============================================================================
% CONSTRAINT STORY: power_asymmetry_in_legibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_power_asymmetry_in_legibility, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: power_asymmetry_in_legibility
 *   human_readable: Power Asymmetry in Epistemic Legibility
 *   domain: epistemology/standpoint_theory/social_epistemology
 *
 * SUMMARY:
 *   Power asymmetry in epistemic legibility describes the structural
 *   mechanism by which dominant epistemic communities determine which
 *   perspectives register as legitimate knowledge versus which are suppressed
 *   below the threshold of recognition. This constraint is downstream of two
 *   foundational claims: (1) disparity between perspectives is a depth signal
 *   rather than noise (mountain), and (2) the synthesized center is a
 *   manufactured construction rather than a neutral vantage point (tangled
 *   rope). The power asymmetry converts these structural facts into an
 *   extraction mechanism: marginalized knowers' disparity signals are
 *   systematically discounted as bias, their epistemic labor is extracted
 *   without credit, and their standpoints are rendered invisible in the
 *   construction of 'objective' knowledge. The constraint exhibits high
 *   extraction (0.68) because the asymmetry is not merely exclusion but
 *   active appropriation — dominant communities extract insights from
 *   marginalized standpoints while denying epistemic authority to their
 *   originators. Suppression is severe (0.78) because exit requires
 *   abandoning one's social location and the epistemic resources it provides.
 *   Theater ratio (0.65) reflects the proliferation of diversity and
 *   inclusion mechanisms that perform recognition without redistributing
 *   epistemic power.
 *
 * KEY AGENTS:
 *   - Marginalized Knowers: Primary victims (powerless/identity_locked) — their disparity signal is discounted as bias; cannot exit their standpoint without identity dissolution; experience maximum extraction of epistemic labor without recognition
 *   - Dominant Epistemic Communities: Primary beneficiaries (institutional/arbitrage) — monopolize epistemic authority, resource allocation, and definitional power; experience the constraint as neutral coordination; can move between contexts while maintaining privilege
 *   - Sympathetic Insiders: Secondary actors (moderate/constrained) — recognize the asymmetry but embedded in extractive structures; experience both coordination benefits and complicity costs; constrained exit without platform loss
 *   - Standpoint Theory Coalition: Organized resistance (organized/mobile) — building alternative frameworks and institutions; experience coordination within coalition alongside marginalization from mainstream; mobile exit at cost of recognition
 *   - Diversity and Inclusion Apparatus: Institutional theater (institutional/constrained) — performative structures claiming to address epistemic injustice; high theater ratio; maintained through legitimacy performance rather than structural change
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees the constraint as pure extraction mechanism; power determines legibility thresholds; minimal coordination function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(power_asymmetry_in_legibility, 0.68).
domain_priors:suppression_score(power_asymmetry_in_legibility, 0.78).
domain_priors:theater_ratio(power_asymmetry_in_legibility, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(power_asymmetry_in_legibility, extractiveness, 0.68).
narrative_ontology:constraint_metric(power_asymmetry_in_legibility, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(power_asymmetry_in_legibility, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(power_asymmetry_in_legibility, snare).
narrative_ontology:human_readable(power_asymmetry_in_legibility, "Power Asymmetry in Epistemic Legibility").
narrative_ontology:topic_domain(power_asymmetry_in_legibility, "epistemology/standpoint_theory/social_epistemology").

domain_priors:requires_active_enforcement(power_asymmetry_in_legibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(power_asymmetry_in_legibility, dominant_epistemic_communities).
narrative_ontology:constraint_beneficiary(power_asymmetry_in_legibility, institutional_gatekeepers).
narrative_ontology:constraint_beneficiary(power_asymmetry_in_legibility, mainstream_academic_publishers).
narrative_ontology:constraint_victim(power_asymmetry_in_legibility, marginalized_knowers_whose_disparity_is_discounted).
narrative_ontology:constraint_victim(power_asymmetry_in_legibility, non_western_knowledge_traditions).
narrative_ontology:constraint_victim(power_asymmetry_in_legibility, grassroots_epistemic_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARGINALIZED KNOWER (SNARE) — Identity-locked because their epistemic position is constituted through their social location; cannot exit their standpoint without ceasing to be who they are. Experiences maximum extraction: their disparity signal is systematically discounted as bias rather than recognized as depth information. The constraint extracts their epistemic labor (lived experience, alternative frameworks) while denying them epistemic authority.
constraint_indexing:constraint_classification(power_asymmetry_in_legibility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: DOMINANT EPISTEMIC COMMUNITY (ROPE) — Experiences the constraint as pure coordination: standards of evidence, peer review, citation practices appear as neutral mechanisms for quality control. Benefits from the constraint through monopoly on epistemic authority, resource allocation, and definitional power. Arbitrage exit: can move between institutional contexts while maintaining epistemic privilege.
constraint_indexing:constraint_classification(power_asymmetry_in_legibility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: SYMPATHETIC INSIDER (TANGLED ROPE) — Institutional actors who recognize the asymmetry but are embedded in the system. Experience both coordination benefits (access to resources, platforms, legitimacy) and extraction costs (complicity, epistemic violence, career risk of challenging norms). Constrained exit: can advocate for reform but cannot fully exit institutional structures without losing platform.
constraint_indexing:constraint_classification(power_asymmetry_in_legibility, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: STANDPOINT THEORY COALITION (TANGLED ROPE) — Organized scholars and activists building alternative epistemic frameworks. Experience coordination (collective knowledge production, mutual citation, institutional footholds) alongside extraction (marginalization, resource scarcity, dismissal as 'identity politics'). Mobile exit: can build parallel institutions but at significant cost to mainstream recognition.
constraint_indexing:constraint_classification(power_asymmetry_in_legibility, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: DIVERSITY AND INCLUSION APPARATUS (PITON) — Institutional structures claiming to address epistemic injustice but largely performative. Theater ratio high: diversity statements, inclusive language policies, representation metrics that do not change underlying power asymmetries in whose knowledge counts. Maintained through institutional inertia and legitimacy theater rather than functional correction of epistemic extraction.
constraint_indexing:constraint_classification(power_asymmetry_in_legibility, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From a civilizational/universal perspective, the constraint is a pure extraction mechanism: power determines which disparity signals register as depth versus which are suppressed below the threshold of legibility. The asymmetry is not a coordination solution but a structural violence that systematically privileges certain standpoints while rendering others invisible. High suppression, high extraction, minimal coordination function.
constraint_indexing:constraint_classification(power_asymmetry_in_legibility, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(power_asymmetry_in_legibility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(power_asymmetry_in_legibility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(power_asymmetry_in_legibility, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(power_asymmetry_in_legibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(power_asymmetry_in_legibility, TR),
    TR >= 0.70.

:- end_tests(power_asymmetry_in_legibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts epistemic labor (lived experience, alternative analytical frameworks, critical insights) from marginalized knowers while systematically denying them epistemic authority. The extraction is not merely exclusion but appropriation: dominant communities incorporate insights from marginalized standpoints (often without citation) while maintaining that these standpoints are biased or subjective. The value reflects that substantial value flows from victims to beneficiaries through this mechanism. Suppression (0.78): Severe. Exit options are structurally constrained because epistemic standpoint is constituted through social location. A marginalized knower cannot exit their standpoint without ceasing to be who they are (identity_locked). Alternative institutions exist but face resource scarcity and legitimacy deficits. The dominant system's network effects (citation cartels, funding concentration, institutional gatekeeping) create near-total barriers to building viable parallel structures. Theater ratio (0.65): High. The proliferation of diversity statements, inclusive language policies, representation metrics, and DEI bureaucracies performs recognition of epistemic injustice while leaving underlying power asymmetries intact. These mechanisms rarely change whose knowledge counts, whose frameworks are cited, or whose questions are funded. The theater has increased over the interval as institutions adopt performative inclusion without redistributing epistemic authority.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates extreme perspectival divergence. Dominant epistemic communities experience pure coordination (Rope) — they are solving the legitimate problem of maintaining epistemic standards and quality control. The diversity apparatus sees degraded theater (Piton) — performative inclusion mechanisms that persist through institutional inertia. Sympathetic insiders and the standpoint theory coalition see mixed coordination and extraction (Tangled Rope) — genuine epistemic benefits from alternative frameworks alongside systematic marginalization. Marginalized knowers experience pure extraction (Snare) — their epistemic labor is appropriated, their authority is denied, and their exit is blocked by identity constitution. The analytical observer confirms the Snare classification from a universal scope: power determines legibility thresholds, and the asymmetry is structural violence rather than coordination. The gap between the beneficiary's Rope and the victim's Snare is the diagnostic signature of extraction naturalized as coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Marginalized knowers are full victims with identity_locked exit, yielding high d (≈0.89) and high f(d) (≈1.28). Their epistemic position is constituted through their social location — they cannot exit their standpoint without identity dissolution. The identity lock is cognitive rather than material: they could physically leave academic institutions, but their epistemic resources (the depth information their standpoint provides) would be discounted or erased in any institutional context that does not recognize standpoint privilege. Dominant epistemic communities are full beneficiaries with arbitrage exit, yielding low d (≈0.05) and negative f(d) (≈-0.12). They experience the constraint as coordination — standards and gatekeeping appear as neutral quality control. Sympathetic insiders are partial beneficiaries (institutional access) but also bear complicity costs; constrained exit yields moderate d. The standpoint theory coalition is organized victims building alternatives; mobile exit yields moderate-high d but lower than trapped agents. The analytical observer uses canonical d for analytical power (0.73), producing high f(d) (≈1.15) — sees the structure clearly as extraction. No directionality overrides needed; the structural derivation captures the relationships accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that the coordination framing (standards as quality control) and the extraction framing (power determines whose knowledge counts) are both structurally accurate from their respective positions. The dominant community genuinely experiences coordination — from their position, the standards work. The marginalized knower genuinely experiences extraction — from their position, the standards systematically discount their epistemic contributions. The mandatrophy is resolved by recognizing that both are true simultaneously: the constraint coordinates within the dominant community while extracting from marginalized communities. The analytical classification (Snare) reflects the structural fact that the coordination function is asymmetrically distributed — it coordinates for beneficiaries at the expense of victims. This is the definition of Tangled Rope from some perspectives and Snare from others, with the analytical view adjudicating based on the magnitude of extraction and suppression.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    standpoint_privilege_vs_bias,
    'Is epistemic privilege from marginalized standpoints a genuine informational advantage (access to social realities invisible from dominant positions) or a motivated reasoning bias that should be corrected for?',
    'Comparative analysis of predictive accuracy, explanatory power, and empirical validation of claims originating from marginalized versus dominant standpoints across domains where ground truth can be established',
    'If genuine advantage: the constraint is pure extraction (Snare from more perspectives) — dominant communities are systematically discarding valid information. If motivated bias: the constraint is coordination (Rope from more perspectives) — quality control mechanisms are functioning as designed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(standpoint_privilege_vs_bias, empirical, 'Whether standpoint privilege is informational advantage or motivated bias').

omega_variable(
    legibility_threshold_mechanism,
    'What determines the threshold below which disparity signals are suppressed versus recognized? Is it purely power concentration, or are there epistemic factors (complexity, unfamiliarity, translation costs) that interact with power?',
    'Historical case studies of epistemic claims that crossed the legibility threshold (e.g., feminist standpoint theory entering mainstream philosophy, indigenous knowledge entering ecology); identification of necessary and sufficient conditions for threshold crossing',
    'If purely power: extraction is total and the constraint is maximally coercive. If epistemic factors matter: some suppression is coordination cost (translation, verification) rather than pure extraction, shifting classification toward Tangled Rope from more perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legibility_threshold_mechanism, conceptual, 'Mechanism determining legibility threshold for disparity signals').

omega_variable(
    alternative_institution_viability,
    'Can marginalized epistemic communities build parallel institutions with sufficient resources and legitimacy to function as genuine alternatives, or does the dominant system''s network effects and resource monopoly make exit structurally impossible?',
    'Longitudinal tracking of alternative epistemic institutions (community-based research collectives, indigenous knowledge systems, grassroots science initiatives): resource sustainability, epistemic influence, ability to credential and employ practitioners',
    'If viable: exit options upgrade from identity_locked to mobile for some agents, reducing experienced extraction. If structurally impossible: the constraint is a total trap with no exit path, confirming Snare classification from victim perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_institution_viability, empirical, 'Viability of parallel epistemic institutions as exit path').

omega_variable(
    cyclopean_naturalization,
    'To what extent is the synthesized center (the ''view from nowhere'' that dominant epistemology claims) a naturalized construction that erases its own standpoint, versus a genuine achievement of objectivity through triangulation?',
    'Philosophical analysis of objectivity claims in dominant epistemology; historical reconstruction of how ''neutral'' standards emerged from specific social locations; comparison with alternative objectivity frameworks (strong objectivity, situated knowledge)',
    'If naturalized construction: the mountain framing (disparity as inherent depth signal) is correct and the dominant view is a false summit. If genuine triangulation: the coordination framing (standards as quality control) has more validity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cyclopean_naturalization, conceptual, 'Whether the view from nowhere is naturalized standpoint or genuine objectivity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(power_asymmetry_in_legibility, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pal_theater_t0, power_asymmetry_in_legibility, theater_ratio, 0, 0.45).
narrative_ontology:measurement(pal_theater_t10, power_asymmetry_in_legibility, theater_ratio, 10, 0.55).
narrative_ontology:measurement(pal_theater_t20, power_asymmetry_in_legibility, theater_ratio, 20, 0.62).
narrative_ontology:measurement(pal_theater_t30, power_asymmetry_in_legibility, theater_ratio, 30, 0.65).

% Extraction over time
narrative_ontology:measurement(pal_extract_t0, power_asymmetry_in_legibility, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(pal_extract_t10, power_asymmetry_in_legibility, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(pal_extract_t20, power_asymmetry_in_legibility, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(pal_extract_t30, power_asymmetry_in_legibility, base_extractiveness, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(power_asymmetry_in_legibility, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is downstream of disparity_as_depth_signal (mountain) and cyclopean_point_as_manufactured_center (tangled_rope). The upstream constraints establish that (1) disparity between perspectives is informational rather than noise, and (2) the synthesized center is a construction rather than a neutral view. This constraint describes the power mechanism that converts these structural facts into an extraction system: whose disparity registers as depth versus whose is suppressed below legibility.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
