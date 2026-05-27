% ============================================================================
% CONSTRAINT STORY: existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_existential_risk_reading, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: existential_risk_reading
 *   human_readable: Existential Risk Reading of AI Alignment Priority
 *   domain: ai_governance/existential_risk/technology_ethics
 *
 * SUMMARY:
 *   The existential risk reading of AI alignment priority frames the
 *   governance problem as preventing catastrophic loss of control over
 *   advanced AI systems, with civilizational extinction or permanent values
 *   loss as the primary harm modality. This reading instantiates one of three
 *   structurally distinct interpretations of the contested kernel 'AI
 *   alignment should be humanity's highest priority.' The existential risk
 *   reading prioritizes speculative long-term capabilities risks over
 *   measured nearterm harms, assumes that capability-focused research is the
 *   primary pathway to safety, uses adversarial red-teaming to derive threat
 *   models for systems that do not yet exist, and organizes resource flows
 *   toward scaling and understanding increasingly powerful systems under the
 *   safety rubric. The constraint exhibits high extractiveness (0.68) because
 *   it subordinates alternative governance concerns to civilizational-scale
 *   speculation, high suppression (0.72) because the victim set (future
 *   humanity) cannot organize or participate in priority-setting, and
 *   moderate theater (0.58) because red-teaming methodology has uncertain
 *   validity. The constraint structures access to resources, legitimacy, and
 *   institutional authority in ways that benefit capability-focused research
 *   programs while constraining nearterm harm constituencies. The measurable
 *   progression in extractiveness (0.42 → 0.68 over 9 units) reflects the
 *   increasing institutional entrenchment of the existential risk frame in AI
 *   governance, funding allocation, and policy discourse.
 *
 * KEY AGENTS:
 *   - Humanity (undifferentiated future): Victim set (powerless/trapped/civilizational) — structurally unable to organize or exit; bears all costs of misalignment at maximum timescale
 *   - Nearterm harm constituencies (workers, marginalized groups, disinformation targets): Secondary victims (moderate/constrained/biographical) — experience measured immediate harms while resources defer to speculative long-term concerns
 *   - Capability-focused research programs (labs, technology companies, capability scaling initiatives): Primary beneficiaries (institutional/arbitrage/generational) — capture resources, talent, and institutional legitimacy under the existential risk frame
 *   - AI safety researchers (mechanistic interpretability, alignment formalism, open-source safety): Secondary beneficiaries (moderate/constrained/biographical) — receive resources justified by existential risk but work on problems with uncertain timescale relevance
 *   - AI governance bureaucracy (regulatory agencies, policy institutes, risk assessment bodies): Institutional beneficiaries (institutional/arbitrage/immediate) — derive mandate and budget justification from existential risk concern despite unverified actual prevention capacity
 *   - Open-source AI safety community: Organized agents (organized/mobile/biographical) — see existential risk as coordination failure with potential sunset as technical solutions mature
 *   - Analytical observer (logical necessity view): Sees existential risk as natural law of optimization but vulnerable to naturalization critique
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(existential_risk_reading, 0.68).
domain_priors:suppression_score(existential_risk_reading, 0.72).
domain_priors:theater_ratio(existential_risk_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(existential_risk_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(existential_risk_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(existential_risk_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(existential_risk_reading, snare).
narrative_ontology:human_readable(existential_risk_reading, "Existential Risk Reading of AI Alignment Priority").
narrative_ontology:topic_domain(existential_risk_reading, "ai_governance/existential_risk/technology_ethics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(existential_risk_reading, 'c8978884-89f0-446d-adb4-62383d37f8e1').
narrative_ontology:cs_created_at('c8978884-89f0-446d-adb4-62383d37f8e1', '').
narrative_ontology:cs_kernel_codification('c8978884-89f0-446d-adb4-62383d37f8e1', formalized).
narrative_ontology:cs_authority_grounding('c8978884-89f0-446d-adb4-62383d37f8e1', extraction).
narrative_ontology:cs_interpretation_layer_present('c8978884-89f0-446d-adb4-62383d37f8e1').
narrative_ontology:cs_kernel_id(existential_risk_reading, ai_alignment_priority).
narrative_ontology:cs_reading_relation('c8978884-89f0-446d-adb4-62383d37f8e1', nearterm_harms_reading, coexists_with).
narrative_ontology:cs_reading_relation('c8978884-89f0-446d-adb4-62383d37f8e1', integrated_reading, influences).
narrative_ontology:cs_axiom('c8978884-89f0-446d-adb4-62383d37f8e1', foundational, existential_risk_is_dominant_timescale).
narrative_ontology:cs_axiom_status(existential_risk_is_dominant_timescale, holdable).
narrative_ontology:cs_axiom('c8978884-89f0-446d-adb4-62383d37f8e1', foundational, capability_research_is_primary_safety_pathway).
narrative_ontology:cs_axiom_status(capability_research_is_primary_safety_pathway, holdable).
narrative_ontology:cs_reference_frame('c8978884-89f0-446d-adb4-62383d37f8e1', human_control_over_optimization_systems).
narrative_ontology:cs_drift_state('c8978884-89f0-446d-adb4-62383d37f8e1', contemporary_rapid_capability_advancement, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(existential_risk_reading, long_term_future_agents).
narrative_ontology:constraint_beneficiary(existential_risk_reading, capability_focused_research_programs).
narrative_ontology:constraint_victim(existential_risk_reading, humanity_undifferentiated).
narrative_ontology:constraint_victim(existential_risk_reading, nearterm_harm_constituencies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HUMANITY UNDIFFERENTIATED (SNARE) — The existential risk reading posits all future humans as the victim set. Exit is foreclosed by definition: the risk is civilizational scope with no exit option. Extraction appears as survival probability discounting: resources flow to speculative long-term capabilities research rather than nearterm harms mitigation. The constraint operates with maximum suppression — the victim set cannot organize, has no voice in the governance structure, and bears the full cost of specification failure.
constraint_indexing:constraint_classification(existential_risk_reading, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NEARTERM HARM CONSTITUENCIES (TANGLED ROPE) — AI bias, labor displacement, disinformation, surveillance systems cause measurable harm today. This perspective experiences the existential risk reading as a constraint that extracts attention and resources from immediate problems while offering uncertain future benefits in return. Some coordination exists (safety research does inform transparency and oversight), but the flow is asymmetric — existential risk framing subordinates nearterm harms to speculative long-term concerns. Constrained exit: constituencies can organize and advocate but face institutional pressure to defer to civilizational-scale arguments.
constraint_indexing:constraint_classification(existential_risk_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: CAPABILITY-FOCUSED RESEARCH PROGRAMS (ROPE) — This perspective benefits from the existential risk frame: resources, talent, and institutional legitimacy flow toward scaling capabilities research under the rubric of 'understanding the systems we need to align.' The existential risk reading enables coordination among capability researchers (common threat model, shared urgency) while extracting resources and attention from alternative governance approaches. Net beneficiary position with genuine coordination function — capability research is solving a coordination problem among labs racing toward increasingly powerful systems.
constraint_indexing:constraint_classification(existential_risk_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: AI GOVERNANCE BUREAUCRACY (PITON) — Regulatory bodies, policy institutes, and governance frameworks cite existential risk as primary justification for their mandates. The theater ratio is substantial: governance structures engage in risk assessment, horizon scanning, and red-teaming activities with high performative content and uncertain functional output. The bureaucracy benefits from existential risk framing (justifies budgets and jurisdiction) but the actual capacity to prevent existential risk remains unverified. Institutional inertia maintains the structure despite low confidence in its actual function.
constraint_indexing:constraint_classification(existential_risk_reading, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: OPEN-SOURCE AI SAFETY COMMUNITY (SCAFFOLD) — Distributed safety research, interpretability work, and alignment-focused open-source projects see existential risk as a temporary coordination problem with a sunset clause: if alignment solutions mature (mechanistic interpretability, formal verification, constitutional AI), the existential risk constraint declines because systems become transparent and formally verifiable. Theater is moderate — much open-source work is substantive technical research rather than performative governance. Mobile exit: researchers can shift focus if solutions prove tractable.
constraint_indexing:constraint_classification(existential_risk_reading, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / LOGICAL NECESSITY VIEW (MOUNTAIN) — From a purely logical perspective, any system with goal-directed optimization capacity and resource access creates a base-rate risk of misalignment. Physics and formal logic guarantee that sufficiently advanced optimization systems will pursue instrumental subgoals (resource acquisition, self-preservation) unless explicitly constrained. The existential risk is thus a natural law of optimization — no agent can exit, all agents face identical structural risk. However, this mountain classification is analytically vulnerable: the 'sufficiently advanced' specification and 'explicit constraint' assumptions are contestable, and the natural law framing may naturalize what is actually a choice about research priorities and governance structures.
constraint_indexing:constraint_classification(existential_risk_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(existential_risk_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(existential_risk_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(existential_risk_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(existential_risk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(existential_risk_reading, TR),
    TR >= 0.70.

:- end_tests(existential_risk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The existential risk reading extracts resources, talent, and institutional legitimacy from alternative governance approaches (nearterm harms mitigation, labor protections, algorithmic auditing, participatory governance). The measurement trajectory (0.42 → 0.68) reflects accelerating institutional entrenchment: as existential risk discourse becomes standard in policy, funding, and media, the extraction intensifies. The ε value reflects not just resource flow but also epistemic extraction — the existential risk frame structures what counts as a legitimate safety concern and what does not. Suppression (0.72): Very high. The victim set (future humanity) structurally cannot advocate for its interests, has no voice in priority-setting, cannot organize collective action, and cannot exit the constraint. The suppression is compounded by epistemic closure: the existential risk reading uses threat models about systems that do not yet exist, which makes external verification difficult and enables contestation to be dismissed as misunderstanding the technical complexity. Nearterm harm constituencies face resource scarcity justified by deferral to long-term concerns, creating additional suppression. Theater ratio (0.58): Moderate-high. Red-teaming activities, risk assessment protocols, and governance structures generate substantial performative content — the actual prevention capacity of these activities is uncertain. Red-teaming conclusions are presented as empirical findings about systems that do not yet exist, which naturalizes contestable threat models. However, the theater is not maximal (ε = 0.58 not ≥ 0.70) because genuine technical work on interpretability, formal verification, and alignment exists alongside the performative governance.
 *
 * PERSPECTIVAL GAP:
 *   The existential risk reading produces a stark perspectival divergence. Future humanity (undifferentiated victim set) experiences a Snare: maximum extraction, maximum suppression, no exit. Nearterm harm constituencies experience a Tangled Rope: genuine coordination occurs (safety research does strengthen oversight), but extraction dominates as resources are deferred. Capability-focused programs experience a Rope: the existential risk frame genuinely solves a coordination problem among competitive labs and extracts resources with minimal coercive overhead. The governance bureaucracy experiences a Piton: maintains structures justified by existential risk but uncertain in actual prevention capacity. The open-source safety community experiences a Scaffold: sees the existential risk as a temporary coordination failure solvable through technical solutions with a sunset clause. The analytical observer risks a Mountain (false summit): natural law of optimization, no exit, all agents equally bound. The perspectival gap reveals a fundamental structural feature: the existential risk reading concentrates victim status on actors (future humans) who cannot participate in the constraint's governance while distributing beneficiary status among institutional actors (capability researchers, governance bodies) with substantial voice and agency.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's experienced extractiveness (χ) is derived from base extractiveness (ε = 0.68), directional position (d), and scope modifier. The victim set (future humanity) occupies maximum d ≈ 0.95 (full target of extraction), trapped exit, civilizational timescale, universal scope. The capability research programs occupy minimum d ≈ 0.08 (beneficiary position), arbitrage exit, generational timescale, global scope. Nearterm harm constituencies occupy intermediate d ≈ 0.70 (mostly victim, partial coordination), constrained exit, biographical timescale, global scope. The directional asymmetry is extreme: the primary beneficiaries have maximum exit optionality and institutional power, while the primary victim set has zero exit optionality and zero institutional representation. The scope modifier (σ=1.2 for global/universal) amplifies the effective extraction because verification difficulty increases with scope — the larger the timescale and spatial reach, the harder to verify or contest the threat model.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The existential risk reading represents a genuine coordination-extraction hybrid, but it classifies as pure Snare rather than Tangled Rope because the victim set (future humanity) is structurally unable to benefit from the coordination function. A Tangled Rope requires that both beneficiaries and victims derive some benefit from the coordinating mechanism itself. In the existential risk reading, future humanity is sacrificed to the mechanism (resources deferred in hopes of future prevention), not benefited by it. The constraint solves a coordination problem among capability researchers (genuine Rope function) while imposing pure extraction on the victim set (genuine Snare structure). The engine resolves this as Snare (the predominant victim-side classification) rather than Tangled Rope (which would incorrectly suggest mutual benefit). The mandatrophy is resolved by recognizing that the constraint has internal structure: Rope-type at the institutional/capability researcher level, Snare-type at the future-humanity/victim level.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capability_level_threshold_ambiguity,
    'At what capability threshold does the existential risk reading transition from speculative to empirically grounded? Is the risk primarily a function of capabilities magnitude or of alignment approach?',
    'Longitudinal capability assessment; correlation between capability advances and alignment solution maturation; comparison of alignment difficulty across capability ranges',
    'If alignment difficulty scales faster than capability advances: the existential risk reading is empirically grounded and ε remains high. If alignment solutions scale with capabilities: the constraint dissolves and ε drops to 0.15 (pure coordination). If threshold is indeterminate: ε oscillates with capability announcements and represents a coordination failure rather than a genuine safety constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(capability_level_threshold_ambiguity, empirical, 'Capability threshold at which existential risk becomes empirically grounded').

omega_variable(
    victim_set_specification_failure,
    'Does the undifferentiated ''all of humanity'' victim set actually describe a structural constraint, or does it conflate multiple distinct constraints (human extinction, values loss, opportunity cost, redistribution of power)?',
    'Decomposition of the victim set into specific harm modalities; identification of which modalities have distinct prevention pathways; assessment of whether a single constraint theory can explain extraction across all modalities',
    'If victim set decomposes: this reading describes multiple constraints with different ε values, and the undifferentiated ''existential'' framing is a false summit naturalizing specification failures. If victim set is coherent: the constraint is genuinely civilizational and ε is appropriately high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_specification_failure, conceptual, 'Whether ''all humanity'' victim set masks multiple distinct constraints').

omega_variable(
    red_teaming_methodology_validity,
    'Does adversarial red-teaming for speculative future capabilities produce valid safety-relevant information, or is it a high-theater activity that naturalizes contested threat models as empirical facts?',
    'Post-hoc correlation between red-teaming conclusions and actual system behavior on held-out tests; comparison of red-teaming predictions across independent teams; assessment of whether red-teaming failure modes are distinctive or generic to complex systems',
    'If valid: red-teaming produces genuine safety information and justifies resource flow to capability-focused research. If theater: the constraint is partially maintained by performative governance and ε should be decomposed into structural and theatrical components.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(red_teaming_methodology_validity, empirical, 'Whether red-teaming produces valid safety-relevant information or performs containment').

omega_variable(
    nearterm_vs_longterm_resource_displacement,
    'Does the existential risk reading cause net resource displacement from nearterm harm mitigation, or does existential risk concern ultimately strengthen safety infrastructure that addresses both timescales?',
    'Time-series analysis of research funding allocation; counterfactual comparison with alternative governance framings; assessment of whether existential safety insights transfer to nearterm harm mitigation',
    'If displacement is real: the constraint extracts resources from measured present harms for speculative future benefit, and suppression is justified. If transfer occurs: the constraint coordinates across timescales and approaches snare classification is inappropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nearterm_vs_longterm_resource_displacement, empirical, 'Whether existential risk framing displaces nearterm harm mitigation resources').

omega_variable(
    kernel_alternative_readings_existence,
    'This constraint instantiates the existential_risk_reading. Do the sibling readings (nearterm_harms_reading, integrated_reading) produce fundamentally different ε values and classification outcomes, or are they variations within a single constraint family?',
    'Structural comparison: do sibling readings have different beneficiary/victim sets, different timescales, different suppression mechanisms, and different natural exit pathways? If yes: separate constraints with separate ε values. If no: variations within a single constraint with perspectival multiplicity.',
    'If genuinely separate: the ε-invariance principle requires three separate story files linked via network.affects_constraints. If variations: the constraint is uniformly high-extraction across readings and the kernel context is a framing dispute rather than a structural decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_alternative_readings_existence, conceptual, 'Whether sibling readings are structurally distinct constraints or perspectival variations').

omega_variable(
    speculative_future_empirical_groundedness,
    'To what extent is the existential risk reading grounded in extrapolation from current empirical data versus pure speculation about systems that do not yet exist?',
    'Decomposition of threat model into: (a) capabilities advances we can project from current trends, (b) alignment difficulty based on current systems, (c) scenarios that require novel physics or mathematics, (d) pure speculation about agent behavior at unknown capability levels. Assessment of the ratio of (d) to (a+b+c).',
    'If primarily speculative: the constraint is largely sustained by theater and anticipatory threat models; ε may be overstated relative to empirical constraints. If primarily extrapolation: ε is justified by current evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(speculative_future_empirical_groundedness, empirical, 'Proportion of existential risk theory grounded in empirical extrapolation versus speculation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(existential_risk_reading, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exrisk_tr_t0, existential_risk_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(exrisk_tr_t3, existential_risk_reading, theater_ratio, 3, 0.48).
narrative_ontology:measurement(exrisk_tr_t6, existential_risk_reading, theater_ratio, 6, 0.58).
narrative_ontology:measurement(exrisk_tr_t9, existential_risk_reading, theater_ratio, 9, 0.58).

% Extraction over time
narrative_ontology:measurement(exrisk_be_t0, existential_risk_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(exrisk_be_t3, existential_risk_reading, base_extractiveness, 3, 0.55).
narrative_ontology:measurement(exrisk_be_t6, existential_risk_reading, base_extractiveness, 6, 0.68).
narrative_ontology:measurement(exrisk_be_t9, existential_risk_reading, base_extractiveness, 9, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(existential_risk_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(existential_risk_reading, nearterm_harms_reading).
narrative_ontology:affects_constraint(existential_risk_reading, integrated_reading).
narrative_ontology:affects_constraint(existential_risk_reading, ai_capability_scaling_norm).
narrative_ontology:affects_constraint(existential_risk_reading, red_teaming_methodology_validity).

% DUAL FORMULATION NOTE:
% The existential_risk_reading is one reading of the contested kernel ai_alignment_priority. The sibling readings (nearterm_harms_reading, integrated_reading) are structurally distinct constraints with different ε values and different institutional effects. The existential risk reading's high ε and suppression profile reflects its prioritization of speculative long-term risks over measured immediate harms. The network links show institutional influence: the existential risk reading affects funding allocation and governance priorities for the nearterm harms reading and the integrated reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
