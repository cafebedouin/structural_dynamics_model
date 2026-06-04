% ============================================================================
% CONSTRAINT STORY: decolonization_constitutions__durable_adaptation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_decolonization_constitutions__durable_adaptation_reading, []).

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
 *   constraint_id: decolonization_constitutions__durable_adaptation_reading
 *   human_readable: Durable Adaptation of Westminster Forms in Decolonization (India & Caribbean Reading)
 *   domain: political/legal/constitutional_form
 *
 * SUMMARY:
 *   The durable adaptation reading claims that India, the Caribbean, and
 *   other post-colonial states that inherited Westminster constitutional
 *   forms did not simply transplant or simply reject them. Instead, they
 *   rebuilt Westminster on local material — reinterpreting parliamentary
 *   sovereignty through indigenous governance traditions, reading common-law
 *   precedent through post-colonial jurisprudence, synthesizing federal
 *   structures with caste councils or communal assemblies. The constraint
 *   operates on the interpretive space itself: the fidelity-or-failure binary
 *   (either the transplant succeeded because it remained faithful, or it
 *   failed because it was rejected) is suppressed in favor of a reading that
 *   emphasizes durable adaptation and local re-rooting. This reading
 *   constitutes a contest with two siblings: the Lancaster House template
 *   reading (Westminster was pre-negotiated at independence conferences as
 *   the price of the date, implying fidelity was the goal) and the rapid
 *   abandonment reading (the forms died young, replaced by one-party states
 *   and military councils within a decade). The durable adaptation reading
 *   does not foreclose the others — different scholarly communities and
 *   political movements genuinely hold all three. But it does create
 *   structural pressure: by reframing adaptation as the mode of persistence,
 *   it makes fidelity-as-goal and rejection-as-inevitable appear as false
 *   dichotomies rather than live alternatives.
 *
 * KEY AGENTS:
 *   - Post-colonial legislatures and courts (institutional/arbitrage) — primary beneficiary of adaptation reading; gains legitimacy and analytical cover for constitutional reinterpretation
 *   - Constitutional drafters and reformers (organized/constrained) — adaptive labor; both benefit (get to innovate) and bear costs (exhausting reinterpretation work)
 *   - Post-colonial political movements (moderate/constrained) — experience adapted forms as partial constraint; provides governance structure and international legitimacy but constrains radical constitutional alternatives
 *   - Westminster legal establishment and transplant scholars (institutional/arbitrage) — invested in template fidelity narrative; the adaptation reading does not foreclose their work but makes it appear incomplete
 *   - Indigenous governance traditions and local political movements (powerless/identity_locked) — structurally mobile within the constitutional frame but identity-fused with adaptation itself; the ability to speak indigenous governance now depends on translating it into Westminster constitutional language
 *   - The fidelity-or-failure binary itself (analytical abstraction, no agency) — victim; its credibility is suppressed by the adaptation frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decolonization_constitutions__durable_adaptation_reading, 0.38).
domain_priors:suppression_score(decolonization_constitutions__durable_adaptation_reading, 0.42).
domain_priors:theater_ratio(decolonization_constitutions__durable_adaptation_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decolonization_constitutions__durable_adaptation_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(decolonization_constitutions__durable_adaptation_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(decolonization_constitutions__durable_adaptation_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decolonization_constitutions__durable_adaptation_reading, tangled_rope).
narrative_ontology:human_readable(decolonization_constitutions__durable_adaptation_reading, "Durable Adaptation of Westminster Forms in Decolonization (India & Caribbean Reading)").
narrative_ontology:topic_domain(decolonization_constitutions__durable_adaptation_reading, "political/legal/constitutional_form").

domain_priors:requires_active_enforcement(decolonization_constitutions__durable_adaptation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decolonization_constitutions__durable_adaptation_reading, '593b1110-9514-4cd7-8268-c1a5a8331976').
narrative_ontology:cs_kernel_codification('593b1110-9514-4cd7-8268-c1a5a8331976', distributed).
narrative_ontology:cs_authority_grounding('593b1110-9514-4cd7-8268-c1a5a8331976', practice).
narrative_ontology:cs_interpretation_layer_present('593b1110-9514-4cd7-8268-c1a5a8331976').
narrative_ontology:cs_reading_relation('593b1110-9514-4cd7-8268-c1a5a8331976', decolonization_constitutions__lancaster_house_template_reading, coexists_with).
narrative_ontology:cs_reading_relation('593b1110-9514-4cd7-8268-c1a5a8331976', decolonization_constitutions__rapid_abandonment_reading, coexists_with).
narrative_ontology:cs_axiom('593b1110-9514-4cd7-8268-c1a5a8331976', foundational, persistence_through_transformation).
narrative_ontology:cs_axiom_status(persistence_through_transformation, holdable).
narrative_ontology:cs_axiom_grounding('593b1110-9514-4cd7-8268-c1a5a8331976', persistence_through_transformation, instrumental).
narrative_ontology:cs_axiom('593b1110-9514-4cd7-8268-c1a5a8331976', foundational, suppression_of_binary_framing).
narrative_ontology:cs_axiom_status(suppression_of_binary_framing, holdable).
narrative_ontology:cs_axiom_grounding('593b1110-9514-4cd7-8268-c1a5a8331976', suppression_of_binary_framing, deontological).
narrative_ontology:cs_reference_frame('593b1110-9514-4cd7-8268-c1a5a8331976', constitutional_hybridity_as_intended_outcome).
narrative_ontology:cs_drift_state('593b1110-9514-4cd7-8268-c1a5a8331976', contemporary_postcolonial_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('593b1110-9514-4cd7-8268-c1a5a8331976', '').
narrative_ontology:cs_kernel_id(decolonization_constitutions__durable_adaptation_reading, decolonization_constitutions).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decolonization_constitutions__durable_adaptation_reading, adaptive_constitutionalism).
narrative_ontology:constraint_beneficiary(decolonization_constitutions__durable_adaptation_reading, local_political_movements).
narrative_ontology:constraint_victim(decolonization_constitutions__durable_adaptation_reading, transplant_determinism_fidelity_narrative).
narrative_ontology:constraint_victim(decolonization_constitutions__durable_adaptation_reading, transplant_determinism_rejection_narrative).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COLONIZED POLITICAL SUBJECT AS DURABLE ADAPTER (TANGLED ROPE) — Faces constrained exit from Westminster forms (inherited institutional framework, international recognition dependency). Simultaneously benefits from and bears costs of adaptation: the constitutional form provides legitimacy and governance structure, but adaptation itself requires constant reinterpretation effort and suppression of the transplant-determinism binary (fidelity-or-failure). The agent experiences genuine coordination function (shared institutional language enabling governance) alongside extraction (exhausting reinterpretation labor, perpetual negotiation of what 'Westminster' means locally). Neither pure extraction nor pure coordination — both simultaneously.
constraint_indexing:constraint_classification(decolonization_constitutions__durable_adaptation_reading, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: ADAPTIVE CONSTITUTIONAL ESTABLISHMENT (ROPE) — Institutional actors (courts, legislatures, constitutional commissions) experience the constraint primarily as coordination function: reinterpreting Westminster forms to fit local contexts, synthesizing colonial heritage with indigenous political traditions. These actors have arbitrage capacity — they can select which Westminster elements to retain, which to transform. Beneficiary position: adaptation generates legitimacy, institutional stability, and capacity to navigate post-colonial governance. Low effective extraction because institutional actors retain significant agency and see themselves as actively solving coordination problems rather than bearing extraction.
constraint_indexing:constraint_classification(decolonization_constitutions__durable_adaptation_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: TRANSPLANT DETERMINISM BINARY AS VICTIM (SNARE) — The fidelity-or-failure binary itself (the constraint that constitutional forms must be assessed as either faithfully preserved transplants or rejected failures) is suppressed by the durable adaptation reading. This perspective views the binary as a victim: it cannot exit the frame that durable adaptation has decentered. The suppression operates not through coercion but through conceptual reframing — the adaptation lens makes the binary appear as a false dichotomy. This perspective experiences the constraint as extractive: the analytical labor of adaptation is built on the corpse of the binary's credibility.
constraint_indexing:constraint_classification(decolonization_constitutions__durable_adaptation_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 4: POST-COLONIAL CONSTITUTIONAL SYNTHESIS MOVEMENT (SCAFFOLD) — Organized actors (constitutionalists, legal reformers, indigenous-rights movements) experience adaptation as a temporary support structure: Westminster forms are being progressively displaced by and synthesized with indigenous constitutional traditions (Hindu jurisprudence, Caribbean oral governance norms, caste-council systems reinterpreted). This perspective sees a sunset: as post-colonial constitutional traditions mature and become self-standing (not Westminster-plus-local-layer but genuinely hybrid forms), the extraction mechanism of Westminster transplant dependency decays. Estimated sunset: 50-75 years as new constitutional generations come to power. Theater is moderate (0.55) because synthesis work is partially performative — claiming continuity with Westminster while fundamentally transforming it — but substantive genuine constitutional innovation also occurs.
constraint_indexing:constraint_classification(decolonization_constitutions__durable_adaptation_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: WESTMINSTER FORM AS INSTITUTIONAL INERTIA (PITON) — From the long temporal view, Westminster forms in India and the Caribbean persist substantially through institutional inertia: the apparatus works well enough (or has worked long enough) that the cost of wholesale replacement exceeds the benefit of reform. The performative content is high (0.55+): public discourse about 'Westminster' masks the degree to which constitutions have been transformed beyond recognition. A 1950 Indian Constitution text would baffle Westminster practitioners; it is called Westminster but has been hollowed and refilled. Piton classification reflects that the form persists not because it solves unique problems unavailable through other means, but because the installed base of institutional practice runs on it. Theater ratio moderates at 0.55 rather than higher because the synthesis is partially genuine innovation, not pure theater.
constraint_indexing:constraint_classification(decolonization_constitutions__durable_adaptation_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, transplant cultures inevitably adapt constitutional forms to local conditions — this appears as an immutable law of institutional evolution. All transplants that survive do so by transforming; all constitutions that remain formal without local re-rooting eventually fail. The adaptation constraint is thus universal and inevitable. However, this mountain classification is false: the structural data reveals that adaptation is not a law of nature but a specific extractive/coordinative hybrid mediated by specific actors (courts, legislatures, constitutional reformers) with specific power and exit configurations. The 'inevitability' narrative naturalizes what is a contingent political choice.
constraint_indexing:constraint_classification(decolonization_constitutions__durable_adaptation_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(decolonization_constitutions__durable_adaptation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(decolonization_constitutions__durable_adaptation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(decolonization_constitutions__durable_adaptation_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(decolonization_constitutions__durable_adaptation_reading, TR),
    TR >= 0.70.

:- end_tests(decolonization_constitutions__durable_adaptation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate, declining. At independence (t=0), extractiveness is higher (0.55) because the Westminster transplant is still experienced as external imposition and the reinterpretation labor is costly. As constitutional interpretation matures (t=15-30), extractiveness declines (0.35-0.42) because the form becomes genuinely locally rooted — adaptation succeeds in making Westminster 'ours' rather than 'theirs.' However, extractiveness does not fall to rope levels (≤0.05) because the ongoing labor of reinterpretation, the suppression of alternative constitutional framings, and the constraint on radical reimagining all persist. Suppression (0.42): Moderate. The fidelity-or-failure binary is suppressed, creating barriers to certain kinds of constitutional discourse. Alternative constitutional framings (monarchical republics, aboriginal councils, theocratic structures) are constrained by the inherited form. But suppression is not total — adaptation itself IS a form of reinterpretation that creates space for indigenous elements. Over time (t=0 to t=30), suppression increases and then declines slightly, reflecting the arc: early suppression (many alternatives seem impossible), mid-period intensification (the binary becomes explicit as courts defend orthodoxy), then decline (new constitutional voices emerge and the binary becomes analytically unhelpful). Theater ratio (0.55): Moderate-to-high. The adaptation discourse has performative content: claiming continuity with Westminster while fundamentally transforming it, using Westminster terminology for genuinely post-colonial institutions, performing 'fidelity' to the form while performing its complete remaking. Theater declines slightly over time as the synthesis becomes embedded and no longer requires performative maintenance. Claimed type (tangled_rope): Justified by requires_active_enforcement (true — the adaptation frame requires constant judicial and scholarly enforcement to suppress the fidelity-or-failure alternative) and the presence of both beneficiaries (adaptive constitutionalism, local political movements) and victims (the fidelity-or-failure binary, transplant determinism both directions).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap runs from rope (the institutional beneficiaries see adaptation as pure coordination) through tangled rope (constrained subjects experience both genuine coordination and extraction) to snare (the transplant-determinism binary is trapped and suppressed). The analytical observer risks a false mountain classification (treating adaptation as a natural law of constitutional evolution) when the structure is clearly institutional and contestable. The piton classification at long time horizons reflects institutional inertia — Westminster persists not because it uniquely solves problems but because the institutional apparatus runs on it. The scaffold perspective is crucial: organized constitutional reformers see a sunset for Westminster forms as genuinely post-colonial traditions mature. The classification variance across perspectives (rope → tangled rope → snare → scaffold → piton → false mountain) demonstrates that 'durable adaptation' is not a neutral descriptive fact but a contested reading that generates different structural experiences depending on position.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality depends on its structural relationship to the adaptation constraint. Institutional beneficiaries (courts, legislatures) have arbitrage options and benefit from the adaptation narrative — low d (0.15-0.25), producing negative or near-zero χ, yielding rope classification. Constrained subjects experience the form as both enabling (governance capacity) and constraining (suppression of alternatives) — moderate d (0.50-0.60), producing moderate χ, yielding tangled rope. The fidelity-or-failure binary as victim is trapped by the adaptation frame — high d (0.85+), producing high χ (per f(d) sigmoid), yielding snare. Organized reformers with constrained but viable exit paths experience moderate extraction and see a sunset — moderate d with temporal escape hatch, yielding scaffold. The analytical observer using a civilizational timescale risks seeing the adaptation constraint as universal law — but this is false-summit territory: the constraint's extractiveness depends on specific institutional positions and power distributions, not on laws of constitutional nature.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    adaptation_versus_transformation_boundary,
    'Where does adaptation end and fundamental transformation begin? Is the Indian Constitution as adapted by Ambedkar and judicial interpretation still ''Westminster'' or is it a new form that merely retains Westminster terminology?',
    'Structural-functional analysis: identify the core institutional functions Westminster provides (legislative supremacy, parliamentary executive, common-law inheritance); determine which post-colonial constitutions retain vs. replace each function; correlate terminological fidelity with functional departure',
    'If adaptation ≤ 30% functional transformation: durable adaptation reading holds; Westminster forms genuinely persist. If transformation > 70%: rapid abandonment reading is more accurate; the forms died and were replaced, only the labels survived. If 30-70% (boundary zone): tangled rope classification confirms; the constraint exhibits both genuine adaptation AND genuine transformation simultaneously.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_versus_transformation_boundary, conceptual, 'Functional boundary between adaptation and replacement of Westminster forms').

omega_variable(
    beneficiary_of_adaptation_mechanism,
    'Who genuinely benefits from the durable adaptation strategy? Is it the adaptive constitutionalism discourse (benefits from having a success narrative), the political actors who use adapted forms (benefit from institutional stability and international legitimacy), or the post-colonial states (benefit from governance capacity)?',
    'Comparative outcome analysis: track institutional stability, governance effectiveness, and legitimacy trajectories in post-colonial states that adopted durable adaptation vs. rapid abandonment vs. template fidelity strategies; identify which actors captured most benefit from each approach; trace resource flows and appointment patterns',
    'If primary beneficiary is adaptive constitutionalism discourse: extractiveness shifts toward theater (constraint becomes more piton-like). If primary beneficiary is political establishment gaining legitimacy cover: extractiveness remains moderate (tangled rope holds). If primary beneficiary is post-colonial populations gaining functional governance: suppression metric should decline and extraction should decline (toward rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_of_adaptation_mechanism, empirical, 'Identity of primary beneficiary in durable adaptation strategy').

omega_variable(
    kernel_reading_contest_unresolvable,
    'Can this reading (durable adaptation) and the rapid abandonment reading coexist in the same analytical framework, or does one foreclose the other?',
    'Examine whether both readings can be simultaneously true from different perspectives on the same empirical record. Durable adaptation reading claims Westminster forms persist through reinterpretation; rapid abandonment claims they were replaced within a decade. Both use the same formal constitutional texts as evidence. The contest is at the level of what counts as ''persistence'' vs. ''replacement'' — a reading relation question, not an empirical resolution.',
    'If the readings coexist: they are both live positions (coexists_with relation confirmed). If one forecloses the other: they cannot both be true within any single analytical framework (forecloses relation applies). The current assessment treats them as coexisting — different parties (international law scholars vs. post-colonial historians) genuinely hold both readings without logical contradiction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_unresolvable, conceptual, 'Whether durable adaptation and rapid abandonment readings can coexist in the same framework').

omega_variable(
    suppression_of_fidelity_or_failure_binary,
    'Is the suppression of the fidelity-or-failure binary structural (institutional forces prevent its articulation) or performative (the binary remains live but is methodologically decentered by adaptation discourse)?',
    'Textual analysis of constitutional discourse over time: track frequency and legitimacy of fidelity-or-failure framings in academic, juridical, and political texts across decades; identify moments when the binary is explicitly suppressed vs. when it vanishes naturally as discourse evolves; determine whether suppression requires active enforcement or emerges from intellectual shifts',
    'If structural suppression with active enforcement: requires_active_enforcement should be true (tangled rope confirmed). If performative decentering: theater ratio should be higher (0.65+) and classification might shift toward piton. If the binary vanishes naturally: suppression metric should decline over time and constraint should move toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_fidelity_or_failure_binary, empirical, 'Nature of suppression operating on the fidelity-or-failure binary').

omega_variable(
    reading_identity_in_kernel_contest,
    'This constraint represents one reading of the decolonization_constitutions kernel. Is this reading the reading of ''what happened'' (empirical claim about actual constitutional evolution) or ''what is possible to say'' (hermeneutic claim about valid interpretive frameworks)?',
    'Examine whether the durable adaptation claim is empirically falsifiable (does it make testable predictions about institutional behavior?) or hermeneutically normative (does it prescribe which interpretation of the evidence is legitimate?). The distinction determines whether omega variables should focus on empirical resolution or conceptual clarification.',
    'If empirical claim: omegas focus on measurable outcomes (institutional stability, functional retention, governance effectiveness). If hermeneutic claim: omegas focus on legitimacy of interpretation (who has authority to define ''adaptation'' vs. ''replacement'', what counts as meaningful persistence). The reading appears to occupy both registers — it makes empirical claims about constitutional stability AND hermeneutic claims about the validity of adaptation frames.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_identity_in_kernel_contest, conceptual, 'Empirical vs. hermeneutic status of the durable adaptation reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decolonization_constitutions__durable_adaptation_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_1947_transplant_phase, decolonization_constitutions__durable_adaptation_reading, theater_ratio, 0, 0.65).
narrative_ontology:measurement(theater_1962_judicial_reinterpretation, decolonization_constitutions__durable_adaptation_reading, theater_ratio, 15, 0.58).
narrative_ontology:measurement(theater_1977_synthesis_embedded, decolonization_constitutions__durable_adaptation_reading, theater_ratio, 30, 0.55).

% Extraction over time
narrative_ontology:measurement(ext_1947_independence, decolonization_constitutions__durable_adaptation_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(ext_1962_constitutional_maturation, decolonization_constitutions__durable_adaptation_reading, base_extractiveness, 15, 0.42).
narrative_ontology:measurement(ext_1977_second_generation, decolonization_constitutions__durable_adaptation_reading, base_extractiveness, 30, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(supp_1947_early_alternatives, decolonization_constitutions__durable_adaptation_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(supp_1962_fidelity_binary_strengthens, decolonization_constitutions__durable_adaptation_reading, suppression_requirement, 15, 0.45).
narrative_ontology:measurement(supp_1977_binary_declines, decolonization_constitutions__durable_adaptation_reading, suppression_requirement, 30, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decolonization_constitutions__durable_adaptation_reading, identity_coordination).
narrative_ontology:affects_constraint(decolonization_constitutions__durable_adaptation_reading, decolonization_constitutions__lancaster_house_template_reading).
narrative_ontology:affects_constraint(decolonization_constitutions__durable_adaptation_reading, decolonization_constitutions__rapid_abandonment_reading).
narrative_ontology:affects_constraint(decolonization_constitutions__durable_adaptation_reading, postcolonial_institutional_legitimacy).
narrative_ontology:affects_constraint(decolonization_constitutions__durable_adaptation_reading, indigenous_political_theory_suppression).

% DUAL FORMULATION NOTE:
% The durable adaptation reading is one of three constraint stories in the decolonization_constitutions kernel family. All three stories share the same domain (post-colonial constitutional form) but claim radically different ε values and beneficiary/victim structures. The adaptation reading (ε=0.38, tangled rope) claims that Westminster forms were genuinely adapted and locally re-rooted, suppressing the fidelity-or-failure binary. The template reading would claim higher ε reflecting the extraction of forms in the negotiation process. The abandonment reading would claim rapid ε decline as forms were replaced. These are not different measurements of the same constraint — they are different constraints instantiated by different readings of the kernel. See commentary.kernel_context for the full contest structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(decolonization_constitutions__durable_adaptation_reading, organized, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
