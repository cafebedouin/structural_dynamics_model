% ============================================================================
% CONSTRAINT STORY: behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_behavioral_competence_reading, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: behavioral_competence_reading
 *   human_readable: Stone Land-Use Prohibition as Behavioral Competence Constraint (Aneyoshi Reading)
 *   domain: disaster_anthropology/commitment_systems/temporal_institutions
 *
 * SUMMARY:
 *   In 1933, the Shōwa tsunami killed over 3,000 people on the Sanriku coast.
 *   The ancestors of Aneyoshi, witnessing the disaster, placed a stone marker
 *   at the elevation limit reached by the wave and established a behavioral
 *   prohibition: settlement shall not extend below the stone. This
 *   prohibition was maintained across 78 years without catastrophe, during
 *   which time institutional memory of the 1933 tsunami faded from regional
 *   consciousness. Neighboring communities without such encoded directives
 *   did not maintain the behavioral constraint. In 2011, the Tōhoku tsunami
 *   struck. Aneyoshi, constrained by the stone-encoded prohibition to settle
 *   above the tsunami limit, survived intact. Neighboring communities that
 *   lacked the prohibition and had settled lower were devastated. The stone's
 *   directive capacity — its ability to constrain human settlement behavior
 *   across seven generations — was validated by catastrophe. This constraint
 *   exemplifies behavioral competence: the ancestor's encoding of hazard
 *   knowledge in a persistent physical marker and community practice proved
 *   more reliable than institutional memory, written records, or collective
 *   attention. The constraint operates at the intersection of geophysical
 *   hazard topology (tsunami run-up dynamics) and cultural transmission
 *   (intergenerational behavioral encoding). It has zero degrees of freedom:
 *   settlement occurs where the constraint permits; no extraction, no
 *   suppression, no negotiation. The constraint is that lethal zones are
 *   inaccessible and safe zones are accessible, and this boundary is
 *   materially encoded and socially enforced.
 *
 * KEY AGENTS:
 *   - Aneyoshi Population (Survivors): Primary beneficiary (powerless/trapped) — the constraint saves their lives by preventing settlement in lethal zones
 *   - Aneyoshi Ancestors (1933 Witnesses): Original constraint architects (deceased; analytical/analytical) — encoded behavioral rule in stone and community practice
 *   - Regional Institutional Memory (Neighboring Communities): Absent agent — communities without stone directives relied on fading institutional memory and institutional records, which failed to maintain the behavioral constraint across the 78-year non-catastrophe interval
 *   - Geological Hazard System: Non-agent force — tsunami topology determines safe/lethal boundaries; the stone-encoded constraint correctly aligns settlement with hazard topology
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — verifies the constraint's structural properties: zero degrees of freedom, natural law emergence, empirical validation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(behavioral_competence_reading, 0.08).
domain_priors:suppression_score(behavioral_competence_reading, 0.02).
domain_priors:theater_ratio(behavioral_competence_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(behavioral_competence_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(behavioral_competence_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(behavioral_competence_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(behavioral_competence_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(behavioral_competence_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(behavioral_competence_reading, mountain).
narrative_ontology:human_readable(behavioral_competence_reading, "Stone Land-Use Prohibition as Behavioral Competence Constraint (Aneyoshi Reading)").
narrative_ontology:topic_domain(behavioral_competence_reading, "disaster_anthropology/commitment_systems/temporal_institutions").

domain_priors:emerges_naturally(behavioral_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(behavioral_competence_reading, '53061e46-a422-42e6-8a4d-3972815315b8').
narrative_ontology:cs_created_at('53061e46-a422-42e6-8a4d-3972815315b8', '').
narrative_ontology:cs_kernel_codification('53061e46-a422-42e6-8a4d-3972815315b8', implicit).
narrative_ontology:cs_authority_grounding('53061e46-a422-42e6-8a4d-3972815315b8', practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(behavioral_competence_reading, aneyoshi_population).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ANEYOSHI RESIDENT (MOUNTAIN) — The stone prohibition is experienced as an immutable constraint on settlement possibility. Residents do not exit because the constraint has made the location where others perished unsuitable for habitation. The prohibition emerged from ancestors' behavioral competence — their capacity to encode and transmit a survival rule across 78 years of silence, seven generations, without institutional apparatus. Zero degrees of freedom in the present: the stone is there, the prohibition stands, settlement occurs upstream of the stone. This is experienced as natural law — 'this is where we can live' — not as a negotiable rule.
constraint_indexing:constraint_classification(behavioral_competence_reading, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: ANALYTICAL OBSERVER (MOUNTAIN) — The constraint exhibits all signatures of natural law at the civilizational scale: (1) Zero degrees of freedom — the stone's spatial location is fixed; settlement dynamics are constrained by tsunami hazard topology, which is not negotiable; (2) Accessibility collapse ≥ 0.85 — the accessible settlement zone is precisely determined by the stone's location and the 2011 tsunami run-up; (3) Resistance ≤ 0.15 — post-2011 empirical validation confirms the stone's directive capacity; (4) Emerges naturally — the constraint emerges from the intersection of geophysical hazard (tsunami topology) and behavioral transmission (cultural encoding across generations). The stone is a transduction mechanism between physical hazard and behavioral response. No extraction, no suppression, no theater. The constraint is that certain settlement locations are lethal and certain locations are survivable, and this boundary is encoded in stone and community practice.
constraint_indexing:constraint_classification(behavioral_competence_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(behavioral_competence_reading_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(behavioral_competence_reading, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(behavioral_competence_reading, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(behavioral_competence_reading, ExtMetricName, E),
    domain_priors:suppression_score(behavioral_competence_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(behavioral_competence_reading),
    narrative_ontology:constraint_metric(behavioral_competence_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(behavioral_competence_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε = 0.08): Minimal. The stone-encoded prohibition does not extract resources, labor, or value from Aneyoshi residents. The constraint restricts settlement location, but this restriction aligns with hazard topology — it prevents access to lethal zones, not to valuable resources. The prohibition has zero beneficiaries and zero victims in the extraction sense. Residents comply because compliance is survival, not because they are coerced or deceived. The low epsilon reflects that no extraction mechanism is present. Suppression (σ = 0.02): Minimal. The behavioral prohibition is enforced through spatial habituation and community practice, not through coercion or restriction of alternatives. Residents can theoretically choose to build below the stone; the suppression mechanism is that collective practice and spatial norms make this choice unthinkable, not impossible. The suppression is so low because the constraint is transparent — everyone understands why the boundary exists (hazard), and compliance is volitional. Theater ratio (τ = 0.15): Low. The stone is a functional mnemonic device encoding hazard knowledge. The community practice of respecting the boundary is genuine enforcement, not performative ritual. Post-2011, the stone's functional status became even clearer — empirical validation reduced any residual theater to near-zero. The small non-zero theater reflects only the minimal communicative overhead required to transmit the behavioral rule across generations.
 *
 * PERSPECTIVAL GAP:
 *   Both perspectives (resident and analytical) classify the constraint as MOUNTAIN. There is no perspectival gap because the constraint's function is identical across all observation positions: settlement behavior is constrained by hazard topology, encoded in a persistent marker and cultural practice, and has zero degrees of freedom. The constraint is uniformly experienced as natural law because it genuinely is natural law — the boundary between lethal and survivable zones is not socially constructed, and the stone's directive capacity reflects behavioral competence (correct hazard modeling), not extraction. This is a rare case of a uniform-type constraint (mountain-only) where all perspectives converge on the same classification.
 *
 * DIRECTIONALITY LOGIC:
 *   This constraint operates at the intersection of hazard topology (geophysical) and behavioral transmission (cultural). Neither the stone nor the ancestors extract value from residents. The stone-encoded prohibition correctly aligns settlement with hazard boundaries, preventing lethal exposure. From the resident's perspective (powerless/trapped), the constraint is experienced as immutable natural law — certain locations are unsuitable for habitation, and this is not negotiable. From the analytical perspective (analytical/analytical), the constraint exhibits all signatures of natural law: zero degrees of freedom, accessibility collapse (safe zone is precisely bounded), minimal resistance (empirically validated), natural emergence (from hazard topology and cultural encoding). The chi formula does not apply here because extraction is zero — the directionality mechanism is inert. Beneficiaries are declared (aneyoshi_population) to enable FSM testing, but this is a genuine natural law constraint, not a false summit. The 'extraction' is zero because the constraint aligns with residents' survival interests, not against them.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a mountain constraint with declared beneficiaries. The beneficiary (aneyoshi_population) is genuine — the stone-encoded prohibition provides life-or-death protection. This is NOT a false summit because the constraint's causal mechanism is verified: 2011 empirical validation confirms that the prohibition correctly models hazard topology and constrains settlement behavior with life-saving effect. No extractive narrative is required to explain beneficiary presence. The mandatrophy is resolved by recognizing that mountain constraints can have beneficiaries when the constraint's function is protective (preventing lethal exposure) rather than extractive. The key diagnostic: beneficiaries of a genuine mountain constraint experience the constraint as natural law that protects them, not as an imposed rule that extracts value from them. Aneyoshi residents experience the stone as 'this is where we survive' — a fact about hazard topology, not an institutional extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_specification_ambiguity,
    'Is the operative constraint the STONE ITSELF (physical artifact, geospatial marker) or the CULTURAL TRANSMISSION PRACTICE (intergenerational encoding of hazard memory)? Does the stone function as a mnemonic device that would fail if removed, or as a directive that has transcended its physical substrate?',
    'Ethnographic investigation post-removal: if stone were displaced or destroyed, does the behavioral prohibition persist? If prohibition collapses upon stone removal, the constraint is artifact-dependent (mountain only at geological timescales). If prohibition persists, the constraint has transcended the stone and is sustained by cultural transmission — the stone is redundant to the constraint''s operative force.',
    'If artifact-dependent: this reading (behavioral_competence) is unstable — the constraint reverts to piton (degraded institutional memory) if the physical mnemonic is lost. If transmission-independent: the constraint is genuinely mountain-class because the behavioral rule has become self-sustaining across the community, and the stone is diagnostic evidence rather than operative mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_specification_ambiguity, empirical, 'Whether the stone is the constraint''s operative mechanism or merely its mnemonic substrate').

omega_variable(
    reading_disambiguation_sibling_contrast,
    'Does the sibling reading (commemorative_husk_reading) classify the stone as PITON (degraded institutional memory, maintained through theater and inertia rather than function) rather than MOUNTAIN? If so, which reading correctly models the constraint''s post-2011 structural status?',
    'Comparative analysis of epsilon values: behavioral_competence_reading assigns ε ≈ 0.08 (mountain — pure constraint, zero extraction); commemorative_husk_reading would assign ε ≈ 0.25-0.40 (piton or tangled_rope — institutional maintenance overhead, potential extraction through heritage tourism, memorial appropriation). The readings diverge on whether 2011 validation CONFIRMED the constraint''s function (behavioral competence) or merely REIFIED an inert cultural artifact (commemorative husk). Post-2011 ethnographic data on how the stone''s social function changed should disambiguate.',
    'If behavioral_competence is correct: the constraint is mountain-class, eternally operative, ε ≈ 0.08. If commemorative_husk is correct: the constraint is degraded or hybrid-type, ε ≥ 0.25, with institutional theater layered onto the artifact. The two readings cannot both be correct — they assign different structural properties to the same kernel (the aneyoshi_land_use_prohibition).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_disambiguation_sibling_contrast, conceptual, 'Kernel reading disambiguation: behavioral competence vs. commemorative husk').

omega_variable(
    temporal_validation_window_closure,
    'Can the 2011 Tōhoku tsunami event be causally attributed to the stone''s directive, or does post-hoc narrative retrofit assign credit to an inert institutional artifact?',
    'Counterfactual historical analysis: comparison of settlement patterns in Aneyoshi vs. unaware neighboring villages (Akamizu, Suketo, Yoshihama) that did not have stone-encoded prohibitions. If Aneyoshi''s survival rate >> control villages'' survival rate, the constraint''s causal efficacy is established. If survival rates are similar, the effect is primarily selection bias (Aneyoshi happened to be less densely populated, or less exposed to tsunami flow direction, independent of the stone''s directive).',
    'If causal efficacy confirmed: the constraint is mountain-class (behavior constrained by hazard topology, stone encodes correct boundary). If selection bias dominates: the constraint is lucky correlation, not causal directive — ε remains low but classification degrades to rope (coordination function without proven extractive benefit) or piton (vestigial artifact that happened to be correct).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_validation_window_closure, empirical, 'Causal attribution of 2011 survival to stone-encoded behavioral constraint').

omega_variable(
    generational_transmission_mechanism_opacity,
    'How does the behavioral prohibition persist across 78 years (seven generations) of zero-catastrophe interval without institutional maintenance, formal documentation, or explicit pedagogy? What is the transmission substrate — oral narrative, spatial habituation, family ritual, or unconscious encoding in settlement choice?',
    'Ethnographic mapping of transmission chains: interviews with residents across generations documenting when/how they learned the prohibition and what motivated compliance. Identification of explicit vs. implicit transmission mechanisms. Comparison with failed transmissions in villages that had similar hazard memories but lost them (e.g., did Akamizu have a stone that was removed or forgotten?).',
    'If transmission is explicit (deliberate instruction): the constraint is sustained by ongoing behavioral enforcement, suggesting rope or tangled_rope classification (requires active coordination). If transmission is implicit (habituation, spatial encoding): the constraint approaches mountain-class (no ongoing labor required; the built environment enforces the rule).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_transmission_mechanism_opacity, empirical, 'Mechanism of intergenerational transmission of behavioral prohibition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(behavioral_competence_reading, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(behav_theater_1933, behavioral_competence_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(behav_theater_1973, behavioral_competence_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement(behav_theater_2011_pre, behavioral_competence_reading, theater_ratio, 78, 0.15).

% Extraction over time
narrative_ontology:measurement(behav_epsilon_1933, behavioral_competence_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(behav_epsilon_1973, behavioral_competence_reading, base_extractiveness, 40, 0.08).
narrative_ontology:measurement(behav_epsilon_2011_pre, behavioral_competence_reading, base_extractiveness, 78, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(behavioral_competence_reading, resource_allocation).

% DUAL FORMULATION NOTE:
% The aneyoshi_land_use_prohibition kernel has two readings: behavioral_competence_reading (this file, ε=0.08, mountain) and commemorative_husk_reading (separate file, ε≥0.25, piton or tangled_rope). The readings diverge on whether 2011 validation confirmed or reified the constraint. See omega variable kernel_reading_disambiguation for methodology to resolve which reading correctly models post-2011 status.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
