% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__constitutive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_montevideo_statehood_criteria__constitutive_reading, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: montevideo_statehood_criteria__constitutive_reading
 *   human_readable: Montevideo Statehood Criteria: Constitutive Reading (Recognition as Condition for Statehood)
 *   domain: international_law/political_philosophy/state_theory
 *
 * SUMMARY:
 *   The Montevideo Convention (1933) establishes four criteria for statehood:
 *   defined territory, permanent population, effective government, and
 *   capacity to conduct international relations. However, the convention is
 *   silent on recognition. This constraint instantiates the CONSTITUTIVE
 *   READING of Montevideo: the claim that recognition by existing states is
 *   not merely evidence of statehood but a CONDITION for it. Under this
 *   reading, an unrecognized polity — no matter how effectively it governs
 *   its territory, maintains population stability, and conducts international
 *   affairs — is not technically a state until the community of existing
 *   states grants recognition. This reading concentrates gatekeeping power in
 *   the existing state system and enables existing states to extract
 *   concessions (diplomatic alignment, resource access, military cooperation)
 *   in exchange for recognition or continued non-recognition. The constraint
 *   exhibits Tangled Rope properties: genuine coordination function (the
 *   recognition mechanism does stabilize the international system and enable
 *   treaty participation), but asymmetric extraction (existing states retain
 *   discretionary veto while unrecognized polities have no alternatives). The
 *   theater ratio has risen over 40 years as de facto states with effective
 *   control and international engagement (Taiwan, Northern Cyprus, Palestine)
 *   persist without formal recognition, revealing the performative gap
 *   between the formal legal criterion and actual state practice. Suppression
 *   has intensified as the cost of non-recognition has grown — exclusion from
 *   UN participation, inability to sign treaties, vulnerability to military
 *   action without legal recourse, economic isolation.
 *
 * KEY AGENTS:
 *   - Existing State Community: Primary beneficiary (institutional/arbitrage) — retains discretionary veto over new state creation; can grant or withhold recognition tied to geopolitical compliance
 *   - Unrecognized Polities (Taiwan, Northern Cyprus, Palestine, etc.): Primary victims (powerless/trapped) — functionally sovereign but legally subordinate; excluded from UN participation, treaty rights, economic integration without recognition
 *   - Self-Determination Movements: Secondary victims (moderate/constrained) — possess international legal right to self-determination but subordinated to existing-state recognition veto; face high costs of organizing toward statehood
 *   - Regional Hegemons/Patron States: Tertiary beneficiaries (powerful/mobile) — can leverage recognition/non-recognition to extract concessions from aspiring states; use recognition as tool for geopolitical influence
 *   - International Legal Framework (Montevideo Convention, UN Charter): Institutional actor (institutional/arbitrage) — maintains formal recognition criterion; provides authority grounding for gatekeeping but increasingly divergent from actual state practice
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the recognition gate as a structural necessity for international order rather than a contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__constitutive_reading, 0.58).
domain_priors:suppression_score(montevideo_statehood_criteria__constitutive_reading, 0.65).
domain_priors:theater_ratio(montevideo_statehood_criteria__constitutive_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__constitutive_reading, tangled_rope).
narrative_ontology:human_readable(montevideo_statehood_criteria__constitutive_reading, "Montevideo Statehood Criteria: Constitutive Reading (Recognition as Condition for Statehood)").
narrative_ontology:topic_domain(montevideo_statehood_criteria__constitutive_reading, "international_law/political_philosophy/state_theory").

domain_priors:requires_active_enforcement(montevideo_statehood_criteria__constitutive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__constitutive_reading, '7f46cddb-470b-4d87-8358-c2749c5b1f03').
narrative_ontology:cs_kernel_codification('7f46cddb-470b-4d87-8358-c2749c5b1f03', formalized).
narrative_ontology:cs_authority_grounding('7f46cddb-470b-4d87-8358-c2749c5b1f03', extraction).
narrative_ontology:cs_interpretation_layer_present('7f46cddb-470b-4d87-8358-c2749c5b1f03').
narrative_ontology:cs_reading_relation('7f46cddb-470b-4d87-8358-c2749c5b1f03', montevideo_statehood_criteria__declaratory_reading, coexists_with).
narrative_ontology:cs_reading_relation('7f46cddb-470b-4d87-8358-c2749c5b1f03', montevideo_statehood_criteria__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('7f46cddb-470b-4d87-8358-c2749c5b1f03', foundational, recognition_constitutes_statehood).
narrative_ontology:cs_axiom_status(recognition_constitutes_statehood, holdable).
narrative_ontology:cs_axiom_grounding('7f46cddb-470b-4d87-8358-c2749c5b1f03', recognition_constitutes_statehood, conventional).
narrative_ontology:cs_axiom('7f46cddb-470b-4d87-8358-c2749c5b1f03', foundational, existing_state_veto_legitimacy).
narrative_ontology:cs_axiom_status(existing_state_veto_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('7f46cddb-470b-4d87-8358-c2749c5b1f03', existing_state_veto_legitimacy, deontological).
narrative_ontology:cs_reference_frame('7f46cddb-470b-4d87-8358-c2749c5b1f03', montevideo_convention_gated_entry).
narrative_ontology:cs_drift_state('7f46cddb-470b-4d87-8358-c2749c5b1f03', contemporary_de_facto_state_persistence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7f46cddb-470b-4d87-8358-c2749c5b1f03', '').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__constitutive_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__constitutive_reading, existing_state_community).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__constitutive_reading, unrecognized_polities).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__constitutive_reading, self_determination_movements).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNRECOGNIZED POLITY (SNARE) — Cannot exit the recognition requirement; institutional structure (treaty access, UN membership, diplomatic immunity, control of airspace, economic integration) all require existing-state recognition. Territorial control and functional administration mean nothing without recognition by the international community. Maximum extractive force with minimal coordination benefit — the requirement extracts political sovereignty without providing an alternative pathway to statehood.
constraint_indexing:constraint_classification(montevideo_statehood_criteria__constitutive_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SELF-DETERMINATION MOVEMENT WITH PARTIAL LEGITIMACY (TANGLED ROPE) — Constrained by dependence on diaspora support, potential patron states, and regional legitimacy, but also potentially benefiting from established international legal standards that endorse self-determination. The movement can organize and negotiate but faces high cost of non-recognition. Experiences the constraint as mixed: recognition would enable coordination (treaty participation, humanitarian access), but the requirement for existing-state consent extracts concessions and limits agency.
constraint_indexing:constraint_classification(montevideo_statehood_criteria__constitutive_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: EXISTING STATE COMMUNITY (ROPE) — Benefits substantially from the recognition gate: maintains institutional control over state membership, enables selective recognition tied to policy compliance, ensures continuity of the state system itself. Experiences the constraint as pure coordination: the mutual recognition mechanism stabilizes the international order and enables treaty participation and diplomatic relations. The existing state community has maximal exit options and benefits — the constraint coordinates the system while preserving their structural veto.
constraint_indexing:constraint_classification(montevideo_statehood_criteria__constitutive_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGIONAL HEGEMON OR PATRON STATE (TANGLED ROPE) — Can recognize or withhold recognition based on strategic interest; experiences the constraint as a coordination mechanism that enables them to extract concessions from aspiring states (military bases, resource rights, foreign policy alignment) in exchange for recognition or support. Significant agency and benefit, but also faces constraints from the principle of self-determination and pressure from other states. Mixed coordination (stabilizing the system) and extraction (leveraging recognition for advantage).
constraint_indexing:constraint_classification(montevideo_statehood_criteria__constitutive_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: FORMAL INTERNATIONAL LEGAL FRAMEWORK (PITON) — The Montevideo Convention text is formalized, but its constitutive reading (that recognition is a CONDITION for statehood, not merely evidence of it) has become largely performative. De facto states with established control, recognized governments, and international engagement (Taiwan, Northern Cyprus, Palestine) persist without formal recognition or with contested recognition. The legal criterion persists through institutional inertia and strategic benefit to the existing state community, but its verification function has degraded — actual statehood is increasingly determined by effective control and functional capacity, not by formal recognition consensus.
constraint_indexing:constraint_classification(montevideo_statehood_criteria__constitutive_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a transcendental/universal perspective, statehood recognition is a fundamental structural requirement for the international system: without a gated entry mechanism, the system becomes undefined and cannot adjudicate legitimacy. The recognition criterion is presented as inherent to what an international system IS. However, this perspective risks naturalizing what is actually a contingent institutional arrangement serving the interests of existing states. The engine's false summit detector will flag the beneficiaries (existing state community) as evidence that this is not a natural law but a constituted rule.
constraint_indexing:constraint_classification(montevideo_statehood_criteria__constitutive_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(montevideo_statehood_criteria__constitutive_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(montevideo_statehood_criteria__constitutive_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(montevideo_statehood_criteria__constitutive_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(montevideo_statehood_criteria__constitutive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(montevideo_statehood_criteria__constitutive_reading, TR),
    TR >= 0.70.

:- end_tests(montevideo_statehood_criteria__constitutive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. The constitutive reading enables existing states to extract concessions (diplomatic compliance, military cooperation, resource access, geopolitical alignment) in exchange for recognition or continued non-recognition. The extraction is not total — unrecognized polities can maintain functional control and conduct limited international engagement — but it is substantial and structural. The upward trajectory reflects accumulating costs: as de facto states with effective capacity persist in non-recognition (Taiwan's economic integration, Palestine's international engagement), the recognition requirement becomes increasingly costly to enforce, but existing states leverage this cost to extract higher prices for recognition concessions. Suppression (0.65): High and rising. Mechanisms include: (1) institutional — inability to participate in UN bodies, sign treaties, access international courts; (2) military — vulnerability to military action without legal self-defense rights or international protection; (3) economic — exclusion from sovereign borrowing, trade agreements, investment protections; (4) diplomatic — lack of formal diplomatic representation and immunity. Suppression is primarily structural (external barriers to exit), not internalized, though some unrecognized polities adopt internal narratives of quasi-legitimacy. Theater ratio (0.48): Moderate and rising. The formal verification function of the recognition criterion (assessment of territorial control, effective government, international relations capacity) has degraded relative to the gatekeeping function. Existing states may grant or withhold recognition based on strategic interest rather than objective statehood criteria. De facto states like Taiwan demonstrate effective control, governance, and international engagement yet remain unrecognized because existing states (particularly China) veto recognition for geopolitical reasons. The theater has increased as the gap between formal criteria and actual recognition decisions has widened.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a fundamental perspectival divergence. The existing state community sees Rope: a coordination mechanism enabling mutual recognition, treaty participation, and international order stability. The unrecognized polity sees Snare: a gatekeeping arrangement that traps them regardless of effective control and governance capacity, with no exit route except by agreeing to existing-state conditions. The regional hegemon sees Tangled Rope: leverage point for extracting concessions while maintaining the coordination function of recognition. The self-determination movement sees Tangled Rope: a mixed system that both recognizes their right (in principle) but subordinates it to existing-state veto. The piton perspective observes that formal verification has degraded — the criterion persists through institutional inertia and strategic benefit, not because it reliably identifies statehood. The mountain perspective risks naturalizing the recognition gate as inherent to what an international system IS, but this is falsifiable: the engine will identify it as a false summit because identifiable beneficiaries (existing state community) exist and the recognition arrangement is contingent, not necessary.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation proceeds from beneficiary/victim declarations and exit options. The existing state community are beneficiaries with arbitrage-level exit options (they can recognize or not, can change recognition policy, can coordinate with other states to pressure recognition or maintain non-recognition). This yields low d and potentially negative f(d), indicating they experience the constraint as beneficial coordination. Unrecognized polities are victims with trapped-level exit options — they cannot exit the recognition requirement without abandoning the pursuit of statehood, and they face nearly insurmountable barriers to forcing recognition or creating alternative frameworks. This yields high d and high f(d), indicating they experience maximum extractiveness. Regional hegemons are beneficiaries with mobile exit options — they can choose to recognize or not, can leverage recognition strategically, and face costs but not barriers to exit. Self-determination movements are victims with constrained exit options — they can organize and build capacity but cannot force recognition and face high costs (military vulnerability, economic isolation, diplomatic exclusion) during the non-recognition period. The perspectival gaps in the table above reflect these differentiated exit positions and beneficiary/victim relationships.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    recognition_causal_direction,
    'Does recognition CREATE statehood, or does it merely ACKNOWLEDGE pre-existing statehood (constitutive vs. declaratory reading)?',
    'Historical analysis of cases where factual statehood preceded recognition (Israel 1948, East Timor 1999); examination of whether unrecognized polities with effective control and international engagement meet material statehood criteria; comparison of recognition timing with effective control establishment across cases',
    'If recognition creates statehood (constitutive): existing states retain structural veto over new state creation; unrecognized polities are legally stateless regardless of effective control. If recognition acknowledges pre-existing statehood (declaratory): effective control and international engagement determine statehood; recognition is evidentiary but not constitutive. The choice determines the entire classification: constitutive reading → high extraction from unrecognized polities; declaratory reading → coordination mechanism with lower extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(recognition_causal_direction, conceptual, 'Whether recognition creates or acknowledges statehood').

omega_variable(
    existing_state_veto_legitimacy,
    'Is the existing state community''s discretionary recognition power a legitimate governance mechanism, or an extractive gatekeeping arrangement?',
    'Analysis of recognition patterns: whether recognition correlates with objective statehood criteria (territorial control, effective government, capacity to conduct international relations) or with geopolitical alignment with recognizing states; documentation of cases where recognition is withheld despite factual statehood or granted despite factual deficiency; examination of whether the principle of self-determination limits the veto power',
    'If legitimate: the constraint is Rope (pure coordination maintaining order). If extractive: the constraint is Snare (gatekeeping tied to political compliance). The factual finding here directly determines χ and thus classification from the existing state perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(existing_state_veto_legitimacy, conceptual, 'Whether the existing state veto is legitimate governance or extractive gatekeeping').

omega_variable(
    declaratory_reading_viability,
    'Would the international system collapse or degrade functionally if recognition were shifted from constitutive (creates statehood) to declaratory (acknowledges pre-existing statehood)?',
    'Comparative institutional analysis: examination of how systems handle unrecognized but functionally effective sovereigns (de facto states with international engagement); modeling of treaty participation and diplomatic stability under declaratory regime; analysis of whether existing states could coordinate recognition through alternative mechanisms (UN consensus, material criteria) if recognition lost its creative power',
    'If system would degrade: constitutive reading is necessary for order (Mountain from civilizational perspective). If system would stabilize: the constitutive reading is chosen for strategic benefit (not natural law — falsifiable as false summit). This omega directly determines whether the mountain classification is genuine or a false summit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(declaratory_reading_viability, empirical, 'Whether declaratory reading would functionally degrade the international system').

omega_variable(
    self_determination_vs_recognition,
    'When self-determination (right to statehood for politically organized communities) conflicts with existing-state recognition veto, which principle supersedes?',
    'Analysis of UN General Assembly resolutions endorsing self-determination; case law from international courts (ICJ, regional courts) adjudicating conflicts between self-determination and recognition; historical cases where self-determination movements achieved statehood despite non-recognition by key existing states (eventually reaching recognition through alternative pathways)',
    'If self-determination supersedes: recognition is not a condition for statehood but merely a procedural convenience (declaratory reading, lower extraction). If recognition supersedes: the constitutive reading is confirmed and self-determination is constrained by existing-state veto. This omega determines whether the victim set (self-determination movements) has a legal/moral escape route from the snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_determination_vs_recognition, conceptual, 'Hierarchy of self-determination vs. recognition in international law').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__constitutive_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(montev_const_tr_t0, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(montev_const_tr_t20, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(montev_const_tr_t40, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(montev_const_be_t0, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(montev_const_be_t20, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(montev_const_be_t40, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(montev_const_su_t0, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(montev_const_su_t20, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(montev_const_su_t40, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 40, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(montevideo_statehood_criteria__constitutive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__constitutive_reading, montevideo_statehood_criteria__declaratory_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__constitutive_reading, montevideo_statehood_criteria__hybrid_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__constitutive_reading, self_determination_right_international_law).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__constitutive_reading, de_facto_state_functional_effectiveness).

% DUAL FORMULATION NOTE:
% The Montevideo statehood kernel decomposes into three constraint stories representing the three live readings (constitutive, declaratory, hybrid). Each reading has a different ε, different beneficiary/victim structure, and different type classification. The constitutive reading (this story) produces high extraction and Tangled Rope classification; the declaratory reading produces lower extraction and Rope classification. These are not perspectives on a single constraint but structurally distinct constraints anchored in different interpretations of the same kernel text. Linked via network.affects_constraints to enable contamination propagation analysis: if the constitutive reading's authority erodes (e.g., widespread recognition of de facto states as full members of international law), the declaratory reading's viability increases.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
