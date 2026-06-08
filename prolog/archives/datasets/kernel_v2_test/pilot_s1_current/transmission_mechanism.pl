% ============================================================================
% CONSTRAINT STORY: transmission_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_transmission_mechanism, []).

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
 *   constraint_id: transmission_mechanism
 *   human_readable: Aneyoshi Tsunami Stone: Intergenerational Knowledge Transmission
 *   domain: disaster_anthropology/commitment_systems/temporal_institutions
 *
 * SUMMARY:
 *   The Aneyoshi tsunami stone ('Do not build below here') represents a rare
 *   empirical test of intergenerational transmission of tacit, life-or-death
 *   knowledge across a 78-year interval with no active enforcement and no
 *   contemporary institutional backing. The 1933 Shōwa Sanriku tsunami killed
 *   3,068 people and prompted survivors in Aneyoshi village to mark a
 *   boundary with a stone, embedding the instruction: 'High dwellings are the
 *   peace and harmony of our descendants. Remember the calamity of the great
 *   tsunami. Do not build below this line.' For 78 years (1933-2011), this
 *   directive operated through pure cultural transmission — children grew up
 *   seeing the stone, learning its meaning, internalizing the boundary into
 *   their landscape literacy. No disaster tested the kernel during this
 *   interval, creating analytical ambiguity: was the transmission mechanism
 *   genuinely live knowledge, or had it decayed into ceremonial performance?
 *   The 2011 Tōhoku tsunami provided a decisive test: Aneyoshi village,
 *   adhering to the ancient boundary, experienced zero casualties, while
 *   neighboring communities downslope were devastated. This outcome
 *   vindicates the kernel's live status but raises deeper questions about
 *   transmission mechanisms, state capture, and whether such mechanisms can
 *   scale beyond their original context. The constraint exhibits all of
 *   rope's defining features: genuine coordination function (where to
 *   rebuild), minimal extraction (no beneficiary captures asymmetric
 *   advantage), low suppression (exit available but not exercised), and low
 *   theater (the mechanism works because it is internalized, not because it
 *   is performed).
 *
 * KEY AGENTS:
 *   - Aneyoshi Residents (Powerless/Mobile, Generational): The primary agents who inherit and transmit the kernel. They are not powerful globally but occupy a strong structural position relative to this constraint — mobile in principle (can relocate) but choosing to stay and honor the boundary. Net beneficiaries of the constraint's coordination function (survival).
 *   - Community Caretakers/Shrine Keepers (Moderate/Constrained, Biographical): Elders and custodians who maintained the stone during 1933-2011 through ritual practice, teaching, and annual verification. They bear the cost of maintenance without visible functional payoff during the 78-year non-catastrophe interval. Constrained exit (cultural obligation, community role expectation).
 *   - Municipal and Prefectural Authorities (Moderate/Constrained, Post-2011): Government actors who, after 2011, recognized the stone as a coordination solution and incorporated it into disaster mitigation policy. See the constraint as transitional (scaffold) — to be gradually replaced by modern warning systems and building codes.
 *   - State-Level Disaster Authority (Powerful/Constrained, National): National government actors who captured the Aneyoshi stone as evidence of 'traditional knowledge integration' while centralizing disaster authority. They have arbitrage exit (claim credit, reallocate resources) while communities have constrained exit (cultural/regulatory obligation to maintain the site).
 *   - Downslope Neighboring Communities (Powerless/Trapped, Generational): Communities like Shizuhama, Toni, and Miyako that lacked equivalent transmission mechanisms and faced the same hazard without the kernel's protection. Counterfactual beneficiaries if they had inherited similar mechanisms.
 *   - Analytical Observer (Civilizational): Sees the constraint as a test case for natural law (whether intergenerational hazard knowledge can persist) or as contingent institutional success (whether this particular transmission mechanism happened to work).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transmission_mechanism, 0.12).
domain_priors:suppression_score(transmission_mechanism, 0.08).
domain_priors:theater_ratio(transmission_mechanism, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transmission_mechanism, extractiveness, 0.12).
narrative_ontology:constraint_metric(transmission_mechanism, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(transmission_mechanism, theater_ratio, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transmission_mechanism, rope).
narrative_ontology:human_readable(transmission_mechanism, "Aneyoshi Tsunami Stone: Intergenerational Knowledge Transmission").
narrative_ontology:topic_domain(transmission_mechanism, "disaster_anthropology/commitment_systems/temporal_institutions").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transmission_mechanism, aneyoshi_residents).
narrative_ontology:constraint_beneficiary(transmission_mechanism, downslope_communities_future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RESIDENT INHERITOR (ROPE) — A household in Aneyoshi whose grandparents learned the stone's directive and transmitted it across 78 years without active enforcement or continuous institutional backing. The constraint appears as pure coordination: the stone solves a real collective-action problem (where to rebuild after inevitable loss) without coercion. The resident experiences the kernel as live knowledge, not ceremony. Mobile exit exists (relocate entirely) but is not exercised — the constraint is internalized through childhood landscape literacy. No extraction; net beneficiary in survival.
constraint_indexing:constraint_classification(transmission_mechanism, rope,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 2: MUNICIPAL AUTHORITY POST-2011 (SCAFFOLD) — Government actors who, after 2011 vindication, recognized the stone as a pre-existing coordination solution and incorporated it into disaster mitigation policy. They see the constraint as transitional coordination that became vestigial during the 78-year non-catastrophe interval but was revived by the 2011 test. For this perspective, the constraint has a sunset: as modern warning systems, building codes, and evacuation protocols mature, the stone's role will shift from active behavioral constraint to historical monument. Extraction negligible; coordination function clear; exit constrained by political commitment to follow-through but not by material barriers.
constraint_indexing:constraint_classification(transmission_mechanism, scaffold,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: COMMUNITY CARETAKER DURING NON-CATASTROPHE (PITON) — A village elder or shrine keeper during 1933-2011 who maintained the stone through ritual care, annual verification, teaching children its meaning — without any visible causal effect. The constraint appears degraded: the kernel's transmission depends entirely on voluntary performance (reminding people of old instructions) because no disaster tests the directive's validity. The caretaker bears the cost of maintenance without seeing functional output. Theater ratio is high during this interval — the stone is kept pristine but its behavior-shaping role is untestable. Exit is constrained by community role expectation and cultural obligation, not by material necessity.
constraint_indexing:constraint_classification(transmission_mechanism, piton,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 4: CIVILIZATIONAL ANALYST (MOUNTAIN) — From a 10,000-year scale, the Aneyoshi stone appears as a natural law of disaster knowledge transmission: any community in a recurrent-hazard zone that forgets earlier disasters will be vulnerable to identical harm. The stone is a device for encoding this inevitable constraint. From this view, the kernel (the 1933 disaster + survivor decision to mark the boundary) is a natural law instantiation — it works because it aligns with unchangeable facts about tsunami physics and human memory loss. However, this perspective naturalizes what is actually a contingent institutional and cultural choice: the stone's authority derives from community commitment to transmit and honor it, not from physics alone.
constraint_indexing:constraint_classification(transmission_mechanism, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 5: DOWNSLOPE NEIGHBORING COMMUNITY (ROPE) — A counterfactual perspective representing communities like Shizuhama and others that did NOT inherit equivalent transmission mechanisms. These communities face the same recurrent hazard but lack the institutional knowledge encoding. They are beneficiaries of the Aneyoshi principle (the proof that intergenerational transmission works) without bearing its maintenance costs. If they adopted similar mechanisms, they would enter the same pure-coordination structure. This perspective reveals the constraint's true type: it solves a genuine collective-action problem with minimal overhead and no asymmetric extraction.
constraint_indexing:constraint_classification(transmission_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: STATE DISASTER AUTHORITY POST-2011 (TANGLED ROPE) — National government actors who, after 2011, incorporated the Aneyoshi stone as evidence for 'traditional knowledge integration' policy while simultaneously centralizing disaster warning and mitigation authority under state control. The constraint appears as hybrid: genuine coordination (the stone works) combined with asymmetric extraction (state captures legitimacy and funding from the proof, while community bears ongoing maintenance and monument obligation). The state has arbitrage exit (claim credit, reallocate resources elsewhere) while communities have constrained exit (cultural obligation to maintain the site, tourism expectations, preservation mandates). Active enforcement emerges post-2011 as heritage regulations and disaster-planning mandates.
constraint_indexing:constraint_classification(transmission_mechanism, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(transmission_mechanism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(transmission_mechanism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(transmission_mechanism, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(transmission_mechanism, TR),
    TR >= 0.70.

:- end_tests(transmission_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base Extractiveness (0.12): Low. This constraint exhibits the hallmark of pure coordination: beneficiaries (Aneyoshi residents, future generations) coincide with beneficiary groups, and no agent captures asymmetric advantage from the arrangement. The 1933 survivors decided to mark the boundary not for personal profit but for collective safety — the kernel was created as a public good. During 1933-2011, extractiveness remained low because community caretakers performed transmission without visible compensation (though they bore social prestige and cultural obligation, which are not extracted material benefits). Post-2011, extractiveness rises slightly (0.12) as the state incorporates the site into heritage tourism and disaster policy, creating potential for institutional extraction from community maintenance burden. But this remains far below snare or tangled_rope levels. Suppression (0.08): Minimal. The directive 'do not build below this line' operates through cultural internalization and landscape literacy, not through coercion or legal prohibition. Residents choose to honor the boundary because it is embedded in their childhood learning and community identity. Exit is available (relocate entirely, build wherever one wishes) but not exercised because the constraint's benefit (survival) is transparent and internalized. Low suppression indicates low resistance — the constraint aligns with residents' own interests (survival) and requires no enforcement machinery. Theater Ratio (0.25): Moderate-low. During 1933-2011 (non-catastrophe interval), theater rose from 0.10 to 0.48 as the causal pathway from boundary to survival became invisible — elders could teach the stone's meaning, but no disaster vindicated the teaching, creating ritual performance (theater). After 2011, theater drops to 0.25 as the mechanism's function becomes empirically transparent. The remaining 0.25 reflects some ceremonial and heritage performance (annual remembrance, tourists visiting the site) but primarily functional knowledge transmission (children learn 'this is where our ancestors marked safety').
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a striking perspectival divergence driven by temporal position. The caretaker during 1933-1955 (Perspective 3, immediately post-disaster) experiences rope — clear functional output, low theater, children naturally learning the boundary's significance. The caretaker during 1972-1989 (same role, different time point, not represented separately but measurable in the piton trajectory) experiences piton — the boundary's function has become invisible due to 40+ years without a test, so performance and maintenance feel unmotivated. The same caretaker in 2011 (post-vindication) experiences rope again, but with state institutional overlay (Perspective 6, tangled rope) — the state has captured legitimacy from the kernel's success, converting pure coordination into mixed extraction. The municipal authority (Perspective 2, scaffold) sees the constraint as a temporary coordination solution whose function is being transferred to modern infrastructure. The civilizational analyst (Perspective 4, mountain) risks naturalizing what is actually contingent institutional success into a law of hazard physics. The perspectival gap reveals that the constraint's type depends entirely on whether the kernel is actively being tested (rope/mountain) or is in a dormant interval (piton). This is a diagnostic exemplar for how indexical classification captures genuinely different structural relationships to the same object.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's structural relationship to the constraint: Does the agent benefit from the constraint? Are they trapped, constrained, or mobile relative to exit? Do they control the constraint's operation? Aneyoshi residents (Perspective 1, powerless/mobile/generational) have d near 0.0 (beneficiaries with exit options) — they experience the constraint as benefiting them (survival) and have structural mobility (could leave) but choose to stay. Engine derives low chi (effective extraction) for this context. Community caretakers during 1933-2011 (Perspective 3, moderate/constrained/biographical) have d slightly elevated (0.2-0.3 range) because they bear maintenance burden and constrained exit (cultural obligation), but they are not victims — the burden is modest and socially valued. Engine derives low chi with caretaker-to-beneficiary gradient (extraction flows toward caretaker prestige, not away). The state authority (Perspective 6, powerful/constrained/national) has d depressed (0.1-0.2 range) because they are institutional beneficiaries with arbitrage exit (claim credit, redeploy resources) — the engine computes negative chi (they extract value from the constraint). Downslope communities (Perspective 5, powerless/trapped/generational, counterfactual) would have d near 1.0 if they inherited the transmission mechanism — trapped beneficiaries experience maximum chi inversion (full rescue from hazard with no extraction cost). The civilizational observer (Perspective 4, analytical/analytical) has d undefined — analytical positions are exempt from directionality computation and return a null chi value.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate is: 'Preserve intergenerational knowledge of safe dwelling boundaries in recurrent-hazard zones.' The constraint outlives its original function when an alternative mechanism (modern early-warning systems, building codes, government evacuation protocols) fully replaces the transmission mechanism's behavioral effect. The 2011 test prevents mandatrophy from being resolved — the kernel's function remains live and has been empirically validated. However, post-2011 state incorporation creates mandatrophy risk: if the state centralizes hazard response and communities lose agency in transmission practice, the constraint may persist as heritage performance (piton) rather than live coordination. This would constitute resolved mandatrophy — the original mandate (keep people safe through transmitted knowledge) would be superseded by a new mandate (maintain the site as historical monument and state disaster policy proof). Commentary should note: if post-2011 measurements show theater_ratio rising again while extraction remains low, this signals mandatrophy resolution in progress — the constraint is transitioning from rope to piton as state infrastructure takes over the function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_decay_ambiguity,
    'During the 78-year non-catastrophe interval (1933-2011), had the kernel (the 1933 decision and its directive) genuinely decayed into ceremonial transmission, or was it continuously live behavioral knowledge?',
    'Oral history interviews with residents born 1930-1955, comparing their accounts of childhood landscape literacy (learning the stone''s boundaries naturally) vs. explicit teaching by elders. Analysis of construction patterns in Aneyoshi vs neighboring communities during 1933-2011 to detect behavioral adherence to the boundary.',
    'If decayed: the constraint was piton throughout 1933-2011 and only revived by 2011 test. If live: the constraint was rope throughout, and the 2011 test merely confirmed what behavioral archaeology would already show. The difference affects whether the transmission is robust or fragile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_decay_ambiguity, empirical, 'Whether the kernel remained behaviorally live or decayed to ceremony during 1933-2011').

omega_variable(
    transmission_mechanism_identity,
    'Is the operative transmission mechanism the stone itself (material focal point), the practice of annual verification and teaching, the landscape literacy of living in view of it, or some combination of all three?',
    'Comparative case study: do other communities that possess tsunami stones WITHOUT accompanying annual rituals or landscape-dwelling practices (e.g., relocated stones in museums) maintain behavioral adherence? Do communities that practice the teaching ritual WITHOUT a physical stone maintain adherence?',
    'If stone alone sufficient: the constraint is robust and scalable — material anchors can encode knowledge across centuries. If ritual practice is necessary: the constraint depends on continuous social reproduction (higher fragility, higher caretaker extraction). If landscape dwelling is key: the constraint requires settlement patterns (exits to urbanization weaken it).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transmission_mechanism_identity, empirical, 'Which components of the transmission mechanism are essential').

omega_variable(
    certification_ambiguity,
    'Did the 2011 Tōhoku tsunami actually provide a decisive test proving the kernel is natural law (physics-grounded), or does it prove something weaker: that one specific instance of transmission across one specific interval happened to work?',
    'Bayesian update analysis: prior probability of Aneyoshi safety given the kernel vs without it, posterior probability given 2011 outcome. Comparison to other sites with similar or different transmission mechanisms; analysis of whether Aneyoshi''s survival is attributable to the transmission mechanism (boundary location) or to confounding factors (topography, wave refraction, downstream bathymetry). Test of the kernel''s directive on hypothetical future tsunamis: does mathematical modeling of the marked boundary confirm it as optimal for the region''s recurrent hazard profile?',
    'If genuine natural law: the kernel captures irreducible physical constraint, and the classification moves toward mountain. If contingent on specific conditions: the classification remains rope, and transmission is robust but not universalizable. If confounded: the 2011 outcome does not actually validate the kernel''s directive, and the constraint''s classification remains uncertain pending further evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(certification_ambiguity, empirical, 'Whether 2011 proves the kernel is natural law or merely shows one successful instance').

omega_variable(
    post_2011_state_capture_risk,
    'As the state incorporates the Aneyoshi stone into official disaster policy and heritage protection, does this represent empowerment of the community transmission mechanism, or institutional extraction of the community''s epistemic authority?',
    'Tracking of post-2011 resource flows: who funds stone maintenance and tourism development? Who sets terms for historical narratives and preservation standards? Do communities retain agency in transmission practice, or do state requirements for ''authentic'' preservation ossify it? Interviews with younger generations about whether state incorporation increased or decreased their internalization of the kernel.',
    'If empowerment: the constraint transitions smoothly from rope to scaffold (temporary state support for transition to modern infrastructure). If extraction: the constraint becomes tangled_rope (state collects legitimacy/funding while community bears performance burden), or even snare (communities are obligated to maintain sites for tourism/heritage without material benefit). This determines whether post-2011 incorporation strengthens or weakens the transmission mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_2011_state_capture_risk, empirical, 'Whether state incorporation represents empowerment or institutional capture').

omega_variable(
    scalability_and_generalization,
    'Can the Aneyoshi transmission mechanism generalize to other disaster-prone regions with different cultural contexts, hazard profiles, and institutional structures?',
    'Comparative ethnography of stone markers, boundary traditions, and intergenerational knowledge transmission in other disaster contexts (earthquake zones in Mesoamerica, flood-prone regions in South Asia, volcanic hazard zones). Analysis of necessary conditions: does the mechanism require written codification, material focal points, continuous settlement, religious or cultural framing, or some combination? Pilot programs testing Aneyoshi-style transmission in other communities.',
    'If generalizable: the constraint is a robust coordination solution applicable across diverse contexts. If culturally or geographically specific: the constraint''s value is exemplar and inspirational rather than scalable. Affects whether the state cascade (Perspective 6) represents genuine institutional learning or superficial incorporation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scalability_and_generalization, empirical, 'Whether the transmission mechanism can generalize beyond Aneyoshi').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transmission_mechanism, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trans_theater_1933_immediate_post_disaster, transmission_mechanism, theater_ratio, 0, 0.1).
narrative_ontology:measurement(trans_theater_1953_mid_interval, transmission_mechanism, theater_ratio, 20, 0.35).
narrative_ontology:measurement(trans_theater_1972_late_interval, transmission_mechanism, theater_ratio, 39, 0.42).
narrative_ontology:measurement(trans_theater_1989_pretest, transmission_mechanism, theater_ratio, 56, 0.48).
narrative_ontology:measurement(trans_theater_2011_post_vindication, transmission_mechanism, theater_ratio, 78, 0.25).

% Extraction over time
narrative_ontology:measurement(trans_extract_1933_immediate, transmission_mechanism, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(trans_extract_1953_institutional_drift, transmission_mechanism, base_extractiveness, 20, 0.1).
narrative_ontology:measurement(trans_extract_1972_caretaker_burden, transmission_mechanism, base_extractiveness, 39, 0.12).
narrative_ontology:measurement(trans_extract_1989_erosion_pretest, transmission_mechanism, base_extractiveness, 56, 0.14).
narrative_ontology:measurement(trans_extract_2011_stabilized, transmission_mechanism, base_extractiveness, 78, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(trans_suppress_1933_post_disaster_memory, transmission_mechanism, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(trans_suppress_1972_normalized, transmission_mechanism, suppression_requirement, 39, 0.08).
narrative_ontology:measurement(trans_suppress_2011_integrated, transmission_mechanism, suppression_requirement, 78, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transmission_mechanism, attachment_coordination).
narrative_ontology:boltzmann_floor_override(transmission_mechanism, 0.08).
narrative_ontology:affects_constraint(transmission_mechanism, early_warning_system_infrastructure).
narrative_ontology:affects_constraint(transmission_mechanism, building_code_enforcement).
narrative_ontology:affects_constraint(transmission_mechanism, state_disaster_authority_consolidation).

% DUAL FORMULATION NOTE:
% The transmission mechanism constraint is upstream of specific hazard-mitigation outcomes. The three downstream constraints have higher extractiveness values reflecting institutional conflicts (state vs community, centralization vs distribution). The transmission mechanism itself exhibits pure coordination (low extraction) and serves as the base case demonstrating how coordination works before institutional layers capture it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
