% ============================================================================
% CONSTRAINT STORY: westminster_export_constitutions__decolonization_constitutions
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westminster_export_constitutions__decolonization_constitutions, []).

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
 *   constraint_id: westminster_export_constitutions__decolonization_constitutions
 *   human_readable: Westminster Export Constitutions: Decolonization Template Reading
 *   domain: political/legal/postcolonial_institutional_design
 *
 * SUMMARY:
 *   The decolonization_constitutions reading instantiates one structural
 *   reading of the contested kernel 'Westminster export constitutions.' This
 *   constraint models the Lancaster House template system: the practice of
 *   drafting constitutions for newly independent African, Asian, and
 *   Caribbean states in London, incorporating Westminster parliamentary
 *   structures, Westminster bills of rights, and Westminster conventions of
 *   responsible government—then transferring these texts to states that often
 *   rewrote or abandoned them within a decade. The constraint exhibits
 *   tangled coordination-extraction: the departing colonial power coordinates
 *   orderly transfer and maintains legal-cultural influence (rope
 *   perspective), while newly independent states are trapped by conditions of
 *   independence that require accepting the template as a price of legitimacy
 *   and international recognition (snare perspective). The template
 *   suppresses indigenous institutional forms (not through explicit
 *   prohibition but through the binary: accept Westminster or be denied
 *   recognition as legitimate democracy) and embeds metropolitan legal logic
 *   into the postcolonial moment. Over the interval, the theater ratio rises
 *   from 0.45 (honeymoon period where Westminster legitimacy is accepted) to
 *   0.68 (widespread realization that the institutions are performing
 *   legitimacy without delivering local governance function). Suppression
 *   requirement decays from 0.80 (active colonial enforcement of template
 *   compliance) to 0.72 (internalized through institutional inertia and
 *   international pressure), as alternatives (constitutional reform
 *   movements, decolonial constitutionalism, plurinational frameworks) become
 *   structurally visible. This reading coexists with sibling readings
 *   (Australian federation, Canadian confederation, Irish Free State) which
 *   represent different structural outcomes: hybrid institutional forms that
 *   proved viable; imported forms adapted through federal structure; dominion
 *   constitutions that were incrementally amended out of the empire. The
 *   decolonization_constitutions reading is distinct because it focuses on
 *   the extractive suppression of indigenous forms as a mechanism of imperial
 *   control persisting through formal independence.
 *
 * KEY AGENTS:
 *   - Departing Colonial Power (Britain, France, Belgium, etc.): Institutional beneficiary (arbitrage exit) — coordinates orderly transfer, maintains legal-cultural influence, normalizes Westminster as postcolonial standard, benefits from constitutional continuity enabling ongoing leverage
 *   - Newly Independent States (Ghana, Nigeria, Kenya, Zambia, etc.): Primary victim (trapped exit) — inherit Lancaster House templates designed in London, suppressed indigenous institutional forms as condition of independence, forced to choose between accepting template or risking delegitimization
 *   - Indigenous Institutional Forms / Precolonial Governance Traditions: Secondary victim (eliminated from choice set) — suppressed not through explicit prohibition but through template's monopoly on legitimacy; rendered invisible in postcolonial constitutionalism
 *   - Postcolonial Intellectual/Political Elite: Moderate power (constrained exit) — benefits from template legitimacy in international institutions and law schools; constrained by template incompatibility with local governance, spends decades amending constitutions to accommodate practice
 *   - Metropolitan Legal Tradition / Westminster Institutions: Institutional beneficiary (arbitrage exit) — persists as postcolonial standard through inertia, citation, and institutional path-dependency; maintains extractive function through theater
 *   - Constitutional Reform Movement / Decolonial Constitutionalism: Organized agents (constrained exit) — constructing alternatives (participatory constitution-making, indigenous recognition, plurinational frameworks); sunset mechanism visible as new constitutions replace templates
 *   - Analytical Observer: Civilizational context (analytical exit) — risks naturalizing contingent institutional arrangements as universal principles of governance; false summit threat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westminster_export_constitutions__decolonization_constitutions, 0.58).
domain_priors:suppression_score(westminster_export_constitutions__decolonization_constitutions, 0.72).
domain_priors:theater_ratio(westminster_export_constitutions__decolonization_constitutions, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westminster_export_constitutions__decolonization_constitutions, extractiveness, 0.58).
narrative_ontology:constraint_metric(westminster_export_constitutions__decolonization_constitutions, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(westminster_export_constitutions__decolonization_constitutions, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westminster_export_constitutions__decolonization_constitutions, tangled_rope).
narrative_ontology:human_readable(westminster_export_constitutions__decolonization_constitutions, "Westminster Export Constitutions: Decolonization Template Reading").
narrative_ontology:topic_domain(westminster_export_constitutions__decolonization_constitutions, "political/legal/postcolonial_institutional_design").

domain_priors:requires_active_enforcement(westminster_export_constitutions__decolonization_constitutions).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westminster_export_constitutions__decolonization_constitutions, '252ae6b4-96fe-4dd6-a44f-ad340b417ca8').
narrative_ontology:cs_kernel_codification('252ae6b4-96fe-4dd6-a44f-ad340b417ca8', formalized).
narrative_ontology:cs_authority_grounding('252ae6b4-96fe-4dd6-a44f-ad340b417ca8', extraction).
narrative_ontology:cs_interpretation_layer_present('252ae6b4-96fe-4dd6-a44f-ad340b417ca8').
narrative_ontology:cs_reading_relation('252ae6b4-96fe-4dd6-a44f-ad340b417ca8', westminster_export_constitutions__australian_federation_1901, coexists_with).
narrative_ontology:cs_reading_relation('252ae6b4-96fe-4dd6-a44f-ad340b417ca8', westminster_export_constitutions__canadian_confederation_1867, coexists_with).
narrative_ontology:cs_reading_relation('252ae6b4-96fe-4dd6-a44f-ad340b417ca8', westminster_export_constitutions__irish_free_state_1922, influences).
narrative_ontology:cs_axiom('252ae6b4-96fe-4dd6-a44f-ad340b417ca8', foundational, indigenous_institutional_suppression_required_for_template_transfer).
narrative_ontology:cs_axiom_status(indigenous_institutional_suppression_required_for_template_transfer, holdable).
narrative_ontology:cs_axiom_grounding('252ae6b4-96fe-4dd6-a44f-ad340b417ca8', indigenous_institutional_suppression_required_for_template_transfer, empirically_contingent).
narrative_ontology:cs_axiom('252ae6b4-96fe-4dd6-a44f-ad340b417ca8', foundational, constitutional_legitimacy_requires_local_institutional_rootedness).
narrative_ontology:cs_axiom_status(constitutional_legitimacy_requires_local_institutional_rootedness, holdable).
narrative_ontology:cs_axiom_grounding('252ae6b4-96fe-4dd6-a44f-ad340b417ca8', constitutional_legitimacy_requires_local_institutional_rootedness, deontological).
narrative_ontology:cs_reference_frame('252ae6b4-96fe-4dd6-a44f-ad340b417ca8', indigenous_constitutional_legitimacy_post_independence).
narrative_ontology:cs_drift_state('252ae6b4-96fe-4dd6-a44f-ad340b417ca8', constitutional_reform_era_1990_onward, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('252ae6b4-96fe-4dd6-a44f-ad340b417ca8', '').
narrative_ontology:cs_kernel_id(westminster_export_constitutions__decolonization_constitutions, westminster_export_constitutions).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westminster_export_constitutions__decolonization_constitutions, departing_colonial_power).
narrative_ontology:constraint_beneficiary(westminster_export_constitutions__decolonization_constitutions, metropolitan_legal_tradition).
narrative_ontology:constraint_victim(westminster_export_constitutions__decolonization_constitutions, newly_independent_states).
narrative_ontology:constraint_victim(westminster_export_constitutions__decolonization_constitutions, indigenous_institutional_forms).
narrative_ontology:constraint_victim(westminster_export_constitutions__decolonization_constitutions, constitutional_legitimacy_in_postcolonial_contexts).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NEWLY INDEPENDENT STATE (SNARE) — Trapped by the condition of independence itself: accepting the Westminster template is the price of peaceful transfer and international recognition. The template suppresses indigenous institutional forms and embeds metropolitan legal logic into the new nation's founding moment. State leadership inherits a constitution designed in London, often incompatible with local governance traditions, and faces the binary choice: adopt the template wholesale or risk external delegitimization and intervention. Maximum extraction from a trapped position.
constraint_indexing:constraint_classification(westminster_export_constitutions__decolonization_constitutions, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: DEPARTING COLONIAL POWER (ROPE) — Experiences the template export as pure coordination: orderly transfer of power via familiar legal forms, enabling the colonial authority to maintain influence through legal-cultural continuity after formal withdrawal. The departing power benefits from the template's adoption because it normalizes Westminster as the postcolonial standard. Exit option is arbitrage — the colonial power can always pivot to other regions or allies if a particular decolonization fails. The constraint solves the coordination problem of managed imperial retreat.
constraint_indexing:constraint_classification(westminster_export_constitutions__decolonization_constitutions, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 3: POSTCOLONIAL INTELLECTUAL/POLITICAL ELITE (TANGLED ROPE) — Benefits from the template's legitimacy in international institutions, law schools, and diplomatic recognition (coordination function); simultaneously constrained by the template's incompatibility with local governance traditions and forced to suppress or marginalize indigenous institutional forms. The elite often advocates for Westminster adoption to signal modernity and secure external recognition, then spends decades amending the constitution to accommodate local practice. Moderate power, constrained exit — they can attempt constitutional reform but face pressure from both the departing power (which prefers continuity) and international institutions (which reward Westminster compliance).
constraint_indexing:constraint_classification(westminster_export_constitutions__decolonization_constitutions, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: METROPOLITAN LEGAL TRADITION (PITON) — Westminster persists as the postcolonial standard long after its functional relevance has eroded. The template becomes a theatrical claim to legitimacy: 'we are constitutional democracies because we have parliaments and bills of rights,' even when these institutions operate in fundamentally different contexts (weak legislatures, executive dominance, or collapse into authoritarianism). The legal tradition maintains itself through inertia, citation by postcolonial courts referencing Westminster precedent, and international institutional recognition of Westminster-shaped governments. Theater ratio is high (0.68) because much of the institutional performance is maintenance of form without function.
constraint_indexing:constraint_classification(westminster_export_constitutions__decolonization_constitutions, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CONSTITUTIONAL REFORM MOVEMENT / DECOLONIAL CONSTITUTIONALISM (SCAFFOLD) — Organized actors (constitutional scholars, civil society, some postcolonial governments) actively constructing alternatives: participatory constitution-making, indigenous institutional recognition, plurinational states (Bolivia, Ecuador), ubuntu-based frameworks (South Africa). These movements see the Westminster export as a temporary institutional form with a sunset. As new constitutions are drafted in situ rather than exported, as indigenous legitimacy claims gain force, as federal and communalist alternatives prove viable, the extractive function of the template decays. Sunset mechanism: constitutional rewriting (Kenya 2010, South Africa 1996, Bolivia 2009). Constraint exit is structural and visible — the template is being actively replaced.
constraint_indexing:constraint_classification(westminster_export_constitutions__decolonization_constitutions, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, constitutional form is constrained by structural imperatives: any large-scale self-governing society requires a separation of powers, representation mechanisms, and rule of law. Westminster provides one crystallization of these universal principles. From this view, the template export appears not as extraction but as the natural diffusion of solutions to universal problems of governance. However, this perspective naturalizes what the structural data reveals as a contingent institutional arrangement backed by departing colonial authority and enforced through conditions of independence. The engine will flag this as a false summit.
constraint_indexing:constraint_classification(westminster_export_constitutions__decolonization_constitutions, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westminster_export_constitutions__decolonization_constitutions_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(westminster_export_constitutions__decolonization_constitutions, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(westminster_export_constitutions__decolonization_constitutions, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(westminster_export_constitutions__decolonization_constitutions, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(westminster_export_constitutions__decolonization_constitutions, TR),
    TR >= 0.70.

:- end_tests(westminster_export_constitutions__decolonization_constitutions_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The template export extracts benefits for the departing colonial power through legal-cultural continuity and ongoing leverage in postcolonial governance, while suppressing indigenous institutional alternatives and forcing newly independent states into inherited constitutional forms. The extractiveness is not total (0.72) because the template does enable some genuine coordination (orderly transfer, international recognition) and states retain the formal capacity to amend (though at high cost). Measurement trajectory shows rising extractiveness from 0.42 (at independence, honeymoon period where template legitimacy is new and unchallenged) to 0.58 (as the template's incompatibility with local governance becomes visible and accumulated amendments reveal structural rejection). Suppression (0.72): High. The template suppresses indigenous institutional forms through the binary of independence: accept Westminster or face external delegitimization. This is enforcement through conditions rather than explicit prohibition, which is why suppression decays over time (0.80 → 0.72) as alternatives become structurally viable and internal reform movements gain force. Theater ratio (0.68): High. The template increasingly becomes a performance of legitimacy—'we are a constitutional democracy because we have a parliament and a bill of rights'—while actual governance operates through different mechanisms (executive dominance, patronage networks, informal power). The ratio rises from 0.45 (early acceptance) to 0.68 (widespread recognition of form-function gap) as the template's incompatibility with local practice becomes undeniable.
 *
 * PERSPECTIVAL GAP:
 *   The departing colonial power experiences the constraint as rope (coordination of orderly transfer, mutual benefit of legal continuity). The newly independent state experiences it as snare (trapped by conditions of independence, suppressed alternatives, forced adoption). The postcolonial elite experience tangled rope (benefits from template legitimacy in international contexts, constrained by template incompatibility locally). The constitutional reform movement experiences scaffold (actively constructing alternatives, sunset visible). The metropolitan legal tradition experiences itself as piton (performative maintenance of Westminster form). The civilizational analytical observer risks seeing mountain (natural diffusion of governance solutions) but structural data reveals false summit. The perspectival gaps are stable across the interval, though suppression requirement (the active enforcement dimension) decays as alternatives become viable.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is computed from beneficiary/victim declarations and exit options. The departing colonial power is beneficiary with arbitrage exit → low d (approximately 0.15-0.20) → negative f(d) → effective extraction runs toward this agent (they extract value from the constraint). Newly independent states are victims with trapped exit (trapped by conditions of independence) → high d (approximately 0.95) → high f(d) → maximum effective extraction experienced by this agent. Postcolonial elite are victims with constrained exit (can amend but at high cost, face pressure from departing power and international institutions) → moderate-high d (approximately 0.68) → high-moderate f(d). Metropolitan legal tradition is beneficiary with arbitrage exit (can pivot if a particular postcolonial state rejects template) → low d → negative f(d). Constitutional reform movement is organized victim/challenger with constrained exit (can construct alternatives but face institutional path-dependency and international pressure) → moderate d (approximately 0.55) → moderate f(d). The directionality pattern shows extraction flowing from the trapped newly independent states toward the beneficiary departing colonial power, mediated through institutional inertia and international recognition systems.
 *
 * MANDATROPHY ANALYSIS:
 *   DECOLONIAL READING: This constraint resolves mandatrophy by showing how a single structural phenomenon—Westminster template export—can be perceived as pure coordination (rope from the departing power), pure extraction (snare from the trapped newly independent state), mixed coordination-extraction (tangled rope from the postcolonial elite), performative maintenance (piton from the metropolitan legal tradition), or actively dissolved coordination (scaffold from constitutional reform movements). The mandatrophy is resolved not by finding the 'true' type but by recognizing that type assignment is perspectival and structural. From the departing power's view, the template solves the coordination problem of managed empire. From the newly independent state's view, the template is an extractive mechanism suppressing alternatives. Both are structurally correct—they describe the same constraint from different agent positions. The false summit (mountain perspective) naturalizes this arrangement as governance law, which the structural data (identified beneficiaries, documented suppression, high amendment rates) reveals as constructed institutional arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    template_suppression_vs_coordination,
    'Is the Westminster template''s suppression of indigenous institutional forms a necessary condition of decolonization-by-transfer, or is suppression the extractive mechanism that benefits the departing power at the cost of constitutional legitimacy in the new state?',
    'Historical comparison: states that accepted Lancaster House templates vs. states that drafted constitutions de novo or with stronger indigenous institutional elements. Measurement of constitutional stability, amendment frequency, and coups within 20 years of independence. Comparison of legitimacy measures (public support, judicial independence, legislative effectiveness) between template-adopted and indigenous-drafted constitutions.',
    'If suppression is necessary coordination cost: constraint reclassifies as Rope (pure coordination) from all perspectives. If suppression is extractive overhead: constraint remains Tangled Rope, and the beneficiary position of the departing power is revealed as structural extraction rather than legitimate transfer management.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(template_suppression_vs_coordination, empirical, 'Whether template suppression of indigenous forms is necessary to decolonization or extractive overhead').

omega_variable(
    rewriting_frequency_threshold,
    'At what amendment frequency does a constitution transform from ''a template being adapted to local context'' to ''an imposed form that the state immediately rewrote to escape''? Is the decade-within-rewriting the engine''s normal adaptation cycle or evidence of fundamental incompatibility?',
    'Quantitative analysis: amendment frequency in template-exported constitutions vs. indigenous-drafted constitutions in comparable post-independence periods. Comparison with US Constitution, French constitutions, and other non-colonial transitions. Classification of amendments: do they represent contextual refinement (clarifying powers) or fundamental reversal (removing Westminster institutions)?',
    'If rewriting is normal adaptation: theater_ratio may be inflated; constraint may be Rope with high-entropy text-to-practice gap. If rewriting is escape attempt: extractiveness may be understated; constraint may be Snare with apparent legitimacy masking fundamental rejection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rewriting_frequency_threshold, empirical, 'Amendment frequency as signal of template fit vs. imposed form').

omega_variable(
    indigenous_institutional_counterfactual,
    'What institutional forms would newly independent states have developed absent the Lancaster House template export? Were there viable indigenous democratic traditions suppressed by the template, or did the template represent a genuine institutional advance unavailable locally?',
    'Institutional history of pre-colonial governance structures. Analysis of colonial suppression of indigenous governance (documented in archives, oral history). Comparative analysis of indigenous constitutionalism when it emerged post-1990 (South Africa, Bolivia, Ecuador, constitutional pluralism movements). Assessment of whether indigenous forms could have scaled to modern state administration.',
    'If suppression of viable indigenous traditions: extractiveness may be understated (template replaces functional systems with imported ones). If indigenous traditions were not scalable: template may represent genuine institutional transfer rather than extraction. If suppression was selective (preserving some traditions, marginalizing others): constraint structure becomes more complex; may require decomposition into separate stories per institutional domain.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(indigenous_institutional_counterfactual, conceptual, 'Counterfactual institutional development absent template export').

omega_variable(
    template_benefit_distribution,
    'Who within the newly independent state benefits from the Westminster template adoption? Does benefit concentrate in a metropolitan-aligned elite, or does it distribute broadly? If concentrated, is the constraint coordinating the elite''s maintenance of colonial power structures under a new form?',
    'Analysis of which groups benefit from Westminster institutions (legislatures vs. executives, urban centers vs. rural regions, educated elites vs. subsistence populations). Measurement of institutional capture: do Westminster-style parliaments in postcolonial contexts serve as venues for redistribution and accountability, or as theaters of legitimacy for executive dominance?',
    'If benefits concentrate in metropolitan-aligned elites: constraint reclassifies as Snare from the perspective of non-elite populations (hidden victims). If benefits distribute: constraint remains Tangled Rope. If constraint is theater of elite legitimacy: theater_ratio may be understated; institutional capture is performative maintenance of Westminster form.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(template_benefit_distribution, empirical, 'Distribution of template benefits within postcolonial states').

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the contested kernel ''Westminster export constitutions.'' How does this reading (decolonization_constitutions) relate structurally to the sibling readings (australian_federation_1901, canadian_confederation_1867, irish_free_state_1922)? Does this reading foreclose them, coexist with them, or influence them?',
    'Structural analysis completed via cs_structure.reading_relations. This omega documents that multiple structurally distinct constraints are being instantiated from a single contested kernel. The kernel contest is itself an irreducible uncertainty: whether ''Westminster export'' names one constraint (with multiple readings) or multiple constraints that happen to share institutional vocabulary.',
    'If readings foreclose one another: only one can be legitimate. If readings coexist: the kernel permits multiple frameworks. If readings influence: one reading creates structural pressure on others without eliminating them. See cs_structure for declared relations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Structural relationship between decolonization_constitutions reading and sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westminster_export_constitutions__decolonization_constitutions, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(westmin_decol_theater_t0_honeymoon, westminster_export_constitutions__decolonization_constitutions, theater_ratio, 0, 0.45).
narrative_ontology:measurement(westmin_decol_theater_t5_dysfunction_visible, westminster_export_constitutions__decolonization_constitutions, theater_ratio, 5, 0.62).
narrative_ontology:measurement(westmin_decol_theater_t10_form_without_function, westminster_export_constitutions__decolonization_constitutions, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(westmin_decol_extractiveness_t0_independence, westminster_export_constitutions__decolonization_constitutions, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(westmin_decol_extractiveness_t5_first_rewrite_wave, westminster_export_constitutions__decolonization_constitutions, base_extractiveness, 5, 0.56).
narrative_ontology:measurement(westmin_decol_extractiveness_t10_consolidation, westminster_export_constitutions__decolonization_constitutions, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(westmin_decol_suppression_t0_colonial_force, westminster_export_constitutions__decolonization_constitutions, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(westmin_decol_suppression_t5_softening_pressure, westminster_export_constitutions__decolonization_constitutions, suppression_requirement, 5, 0.75).
narrative_ontology:measurement(westmin_decol_suppression_t10_internalized_decaying, westminster_export_constitutions__decolonization_constitutions, suppression_requirement, 10, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westminster_export_constitutions__decolonization_constitutions, enforcement_mechanism).
narrative_ontology:affects_constraint(westminster_export_constitutions__decolonization_constitutions, westminster_export_constitutions__australian_federation_1901).
narrative_ontology:affects_constraint(westminster_export_constitutions__decolonization_constitutions, westminster_export_constitutions__canadian_confederation_1867).
narrative_ontology:affects_constraint(westminster_export_constitutions__decolonization_constitutions, westminster_export_constitutions__irish_free_state_1922).

% DUAL FORMULATION NOTE:
% The decolonization_constitutions reading is one member of the westminster_export_constitutions constraint family. All four readings share a common kernel (Westminster as legitimate constitutional form) but instantiate structurally distinct constraints with different ε values and different beneficiary/victim structures. Decolonization_constitutions (ε=0.58, Tangled Rope) emphasizes extractive suppression of indigenous forms; australian_federation_1901 (ε≈0.25, Rope) emphasizes successful institutional hybrid; canadian_confederation_1867 (ε≈0.15, Rope) emphasizes codification within federal structure; irish_free_state_1922 (ε≈0.52, Tangled Rope) emphasizes constitutional escape trajectory. All four are linked via network.affects_constraints because they share institutional vocabulary and research community but operate at different extractiveness levels and have different failure modes. The network should be symmetric: each story lists the others in its affects_constraints array.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westminster_export_constitutions__decolonization_constitutions, institutional, 0.18).
constraint_indexing:directionality_override(westminster_export_constitutions__decolonization_constitutions, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
