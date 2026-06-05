% ============================================================================
% CONSTRAINT STORY: legal_personhood_boundary__functional_capacity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legal_personhood_boundary__functional_capacity_reading, []).

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
 *   constraint_id: legal_personhood_boundary__functional_capacity_reading
 *   human_readable: Legal Personhood as Functional Capacity (Rationality/Sentience/Self-Awareness)
 *   domain: legal_philosophy/constitutional_law/rights_theory
 *
 * SUMMARY:
 *   The legal definition of personhood determines who has standing to hold
 *   rights, own property, enter contracts, and claim legal protection. The
 *   traditional anthropocentric reading restricts personhood to humans; the
 *   functional capacity reading (this constraint) grounds personhood in
 *   demonstrable cognitive capacities — rationality, sentience,
 *   self-awareness — regardless of species. This reading is actively
 *   reshaping law through constitutional reforms (Ecuador 2008, India 2009,
 *   New Zealand 2017 recognizing non-human personhood), litigation (Great Ape
 *   Project standing petitions, elephant habeas corpus cases), and academic
 *   influence. The constraint exhibits the six DR types from different
 *   structural positions: non-human sentient beings see extraction (Snare),
 *   animal advocates see mixed coordination and constraint (Tangled Rope),
 *   early-adopter jurisdictions see coordination innovation (Rope), property
 *   holders see existential threat (Snare), legal systems see enforcement
 *   burden and coordination need (Tangled Rope), the anthropocentric
 *   tradition sees inertial degradation (Piton), and natural law frameworks
 *   risk naturalizing what is a contingent institutional boundary (Mountain —
 *   false summit candidate). The functional capacity reading redistributes
 *   personhood entitlements away from exclusive humanity toward a broader
 *   epistemic class defined by cognition. This is genuinely controversial:
 *   the reading challenges property rights in sentient beings, imposes
 *   institutional enforcement costs, destabilizes settled legal categories,
 *   and threatens extractive industries. Yet it also offers coordination
 *   benefits — clearer principles for adjudicating novel claims, alignment
 *   with advancing neuroscience, and resolution of the ambiguity that leaves
 *   dolphins and elephants as property despite their evident personhood
 *   markers.
 *
 * KEY AGENTS:
 *   - Non-human sentient beings (animals, potential AIs): Primary victim (powerless/trapped) — legally classified as property despite cognitive capacity; bear extraction toward human purposes with no recognized standing
 *   - Property holders (factory farmers, research institutions, zoos): Primary beneficiary (institutional/arbitrage) — economically dependent on classification of sentient beings as property; direct losers under functional capacity reading
 *   - Animal rights advocates and disability rights communities: Secondary actors (moderate/constrained) — benefit from advocacy networks and precedent but constrained by institutional resistance
 *   - Early-adopter jurisdictions (Ecuador, India, New Zealand, some US states): Institutional beneficiary (institutional/arbitrage) — gain legal innovation prestige and soft power; pioneers of expanded personhood
 *   - Legal systems and constitutional authorities: Institutional enforcer (institutional/constrained) — responsible for administering new personhood categories, managing cascading claims, bearing institutional strain
 *   - Anthropocentric legal tradition: Institutional beneficiary now degrading (institutional/arbitrage-to-trapped) — historically privileged human exceptionalism; now atrophies as its epistemic base collapses
 *   - Natural law defenders: Analytical observer at risk of false summit (analytical/analytical) — risk naturalizing anthropocentric boundary as immutable law rather than contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_boundary__functional_capacity_reading, 0.58).
domain_priors:suppression_score(legal_personhood_boundary__functional_capacity_reading, 0.68).
domain_priors:theater_ratio(legal_personhood_boundary__functional_capacity_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__functional_capacity_reading, tangled_rope).
narrative_ontology:human_readable(legal_personhood_boundary__functional_capacity_reading, "Legal Personhood as Functional Capacity (Rationality/Sentience/Self-Awareness)").
narrative_ontology:topic_domain(legal_personhood_boundary__functional_capacity_reading, "legal_philosophy/constitutional_law/rights_theory").

domain_priors:requires_active_enforcement(legal_personhood_boundary__functional_capacity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__functional_capacity_reading, '7dd45b97-3e85-408a-a7a0-6070e398efb5').
narrative_ontology:cs_kernel_codification('7dd45b97-3e85-408a-a7a0-6070e398efb5', formalized).
narrative_ontology:cs_authority_grounding('7dd45b97-3e85-408a-a7a0-6070e398efb5', lineage).
narrative_ontology:cs_interpretation_layer_present('7dd45b97-3e85-408a-a7a0-6070e398efb5').
narrative_ontology:cs_reading_relation('7dd45b97-3e85-408a-a7a0-6070e398efb5', legal_personhood_boundary__restrictive_anthropocentric_reading, forecloses).
narrative_ontology:cs_reading_relation('7dd45b97-3e85-408a-a7a0-6070e398efb5', legal_personhood_boundary__developmental_potentiality_reading, coexists_with).
narrative_ontology:cs_axiom('7dd45b97-3e85-408a-a7a0-6070e398efb5', foundational, personhood_follows_capacity_not_species).
narrative_ontology:cs_axiom_status(personhood_follows_capacity_not_species, holdable).
narrative_ontology:cs_axiom_grounding('7dd45b97-3e85-408a-a7a0-6070e398efb5', personhood_follows_capacity_not_species, empirically_contingent).
narrative_ontology:cs_axiom('7dd45b97-3e85-408a-a7a0-6070e398efb5', secondary, capacity_assessment_is_performable).
narrative_ontology:cs_axiom_status(capacity_assessment_is_performable, holdable).
narrative_ontology:cs_axiom_grounding('7dd45b97-3e85-408a-a7a0-6070e398efb5', capacity_assessment_is_performable, empirically_contingent).
narrative_ontology:cs_reference_frame('7dd45b97-3e85-408a-a7a0-6070e398efb5', capacity_independent_personhood).
narrative_ontology:cs_drift_state('7dd45b97-3e85-408a-a7a0-6070e398efb5', contemporary_post_great_ape_project_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7dd45b97-3e85-408a-a7a0-6070e398efb5', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__functional_capacity_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, non_human_sentient_beings).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, future_artificial_minds).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, human_communities_claiming_personhood_via_capacity).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, property_holders_with_sentient_property).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, anthropocentric_legal_tradition).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, institutional_legal_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-HUMAN SENTIENT BEINGS (SNARE) — Legally classified as property or objects despite demonstrable cognitive capacity. Trapped: cannot exit their species membership or assert legal standing without external advocacy. The constraint extracts their labor, use, and life toward human purposes while denying reciprocal rights. Maximum experienced extraction — no agency, no exit, no recognized interests.
constraint_indexing:constraint_classification(legal_personhood_boundary__functional_capacity_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ANIMAL RIGHTS ADVOCATES / DISABILITY RIGHTS COMMUNITIES (TANGLED ROPE) — Constrained by institutional legal frameworks that resist capacity-based personhood expansion. These groups benefit from coordination around expanding legal categories (advocacy networks, litigation precedent, public awareness) while bearing extraction costs (institutional resistance, resource barriers, social marginalization). Mixed extraction and genuine coordination function.
constraint_indexing:constraint_classification(legal_personhood_boundary__functional_capacity_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EARLY-ADOPTER JURISDICTIONS (ROPE) — Ecuador, India, New Zealand declare non-human personhood through constitutional reform or judicial innovation. They experience this as coordination: establishing new legal categories that resolve ambiguity about how to represent non-human interests. Net beneficiary through soft power and legal innovation prestige.
constraint_indexing:constraint_classification(legal_personhood_boundary__functional_capacity_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PROPERTY HOLDERS WITH SENTIENT PROPERTY (SNARE) — Institutional actors (factory farming, pharmaceutical research, wildlife zoos) whose business models depend on classifying sentient beings as property. The functional capacity reading threatens their economic extraction — if property becomes legally personified through demonstrated cognition, their conversion of sentient beings into profit becomes legally contestable. High extraction pressure; significant suppression of alternative framings.
constraint_indexing:constraint_classification(legal_personhood_boundary__functional_capacity_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGAL SYSTEMS AND CONSTITUTIONAL AUTHORITIES (TANGLED ROPE) — Institutional guardians of the personhood boundary. The functional capacity reading imposes active enforcement costs: developing capacity-assessment frameworks, adjudicating novel claims (Great Ape Project precedents, animal standing in court), managing cascading boundary expansion. They also benefit from coordination — clearer standards reduce ambiguity. But the extraction is real: institutional inertia is costly to overcome, and the boundary shift threatens established property regimes that underpin the legal system itself.
constraint_indexing:constraint_classification(legal_personhood_boundary__functional_capacity_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANTHROPOCENTRIC LEGAL TRADITION (PITON) — Historical framing that reserves personhood for humans regardless of capacity (or conditions it on human capacity alone). This tradition persists through institutional inertia: courts, legislatures, and scholars continue citing it as foundational even as its functional justification erodes. Theater ratio reflects performative appeals to 'human dignity' and 'natural law' that preclude rather than engage capacity-based reasoning. The tradition has largely atrophied as its epistemic base (that rationality is uniquely human) collapses, but institutional machinery keeps it operative.
constraint_indexing:constraint_classification(legal_personhood_boundary__functional_capacity_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From certain theological or natural law frameworks, personhood is an immutable category defined by human essence, soul, or divine creation — not reducible to capacity or cognition. This perspective treats the personhood boundary as a civilizational natural law. However, the structural data contradicts this classification: identifiable beneficiaries (property holders, anthropocentric traditions) exist, institutional enforcement is active, and alternative capacity-based frameworks exist. The engine will identify this as a false summit: naturalization of what is actually a contestable institutional commitment.
constraint_indexing:constraint_classification(legal_personhood_boundary__functional_capacity_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legal_personhood_boundary__functional_capacity_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(legal_personhood_boundary__functional_capacity_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legal_personhood_boundary__functional_capacity_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(legal_personhood_boundary__functional_capacity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(legal_personhood_boundary__functional_capacity_reading, TR),
    TR >= 0.70.

:- end_tests(legal_personhood_boundary__functional_capacity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The functional capacity reading produces significant redistributive pressure — it shifts personhood entitlements from exclusive humanity toward a broader class defined by cognition. This is pure extraction for property holders (factory farming, research institutions) who depend on classifying sentient beings as property. But it is coordination benefit for non-human beings whose interests are now legally representable. The intermediate value (0.58) reflects that the reading offers real coordination function (clearer principles, alignment with science) alongside extractive consequences (property holders lose exclusive personhood privileges, legal systems bear enforcement costs). Suppression (0.68): High. Strong institutional resistance maintains anthropocentrism through: legal tradition inertia, property law entanglement, economic interests, religious/philosophical framings. Alternative capacity-based frameworks exist but face systemic suppression — courts cite 'human dignity' abstractly rather than engaging capacity-assessment, legislatures resist personhood expansion, corporate lobbying preserves status quo. Theater ratio (0.55): Moderate. Appeal to 'natural law' and 'human exceptionalism' is performative — the empirical neuroscience does not support it, but the rhetoric persists. However, the functional capacity reading itself relies on increasingly testable claims about cognition (mirror self-recognition, pain response, social complexity) — theater is declining as measurement becomes more specific. The decline in theater_ratio over the interval reflects increasing specificity of capacity frameworks and declining appeal of vague natural law claims.
 *
 * PERSPECTIVAL GAP:
 *   The perspectives reveal structural asymmetry: non-human sentient beings see pure extraction (Snare) with no exit, while property holders also see extraction (Snare) but with institutional resources to resist. Animal advocates see coordination potential alongside constraint (Tangled Rope) — the reading offers new legal tools but imposes resource and institutional barriers. Early adopters see coordination benefits (Rope) — they are solving the problem of how to represent non-human interests. Legal authorities see both coordination (clearer standards) and extraction (enforcement burden) — Tangled Rope. The anthropocentric tradition sees its own degradation (Piton) — persists through inertia despite lost epistemic foundation. The natural law view risks falsely summiting — naturalizing what is an institutional choice. The perspectival gap is maximal between property holders and non-human beings: the same functional capacity facts appear as existential threat to one and as liberation to the other. This gap reveals that the personhood boundary is not a natural fact but a contested institutional commitment.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) follows from structural position: Non-human sentient beings (trapped/powerless) → d ≈ 0.95 → high f(d) → maximum experienced extraction. Property holders (constrained/institutional beneficiaries) → d ≈ 0.80 → high f(d) → severe experienced extraction (they lose privileges). Animal advocates (constrained/moderate) → d ≈ 0.55 → moderate f(d) → mixed extraction and benefit. Early adopters (arbitrage/institutional) → d ≈ 0.15 → low f(d) → coordination-positive experience. Legal authorities (constrained/institutional) → d ≈ 0.50 → moderate f(d) → balanced extraction and coordination burden. The anthropocentric tradition (arbitrage/institutional) → d ≈ 0.20 → low f(d) → but institutional inertia means the tradition experiences degradation despite low d. The natural law observer (analytical/analytical) → d ≈ 0.72 → standard analytical directionality → but at risk of false summit from naturalizing the institutional boundary.
 *
 * MANDATROPHY ANALYSIS:
 *   The functional capacity reading resolves mandatrophy by demonstrating that the personhood boundary contains genuine coordination function (legal clarity, alignment with neuroscience, standing for non-human interests) alongside extractive consequences (property holder losses, institutional strain). The reading is not pure extraction dressed as coordination — it genuinely solves the coordination problem of how to represent entities with demonstrable cognitive capacity within a legal framework. But it also imposes real extraction costs on beneficiaries of the anthropocentric regime. The mixed classification (Tangled Rope) is not a failure of distinction — it captures the actual structural duality: this reading coordinates non-human interests AND extracts from property holders' ability to treat sentient beings as objects. The mandatrophy dissolves when we recognize that the question is not 'is this pure coordination or pure extraction?' but 'who coordinates and who bears extraction costs?' For non-human beings: purely beneficial coordination. For property holders: purely extractive threat. For legal systems: mixed — enforcement burden is extraction, clarity is coordination benefit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capacity_operationalization_ambiguity,
    'Which cognitive capacities (rationality, sentience, self-awareness, mirror self-recognition, tool use) qualify for legal personhood, and how do we adjudicate borderline cases?',
    'Comparative neuroscience and behavioral ethology establish which capacities correlate with law-relevant interests (avoiding harm, pursuing goals, forming social bonds). Legal precedent from Great Ape Project, elephant cognition studies, and cephalopod sentience research; jurisdictional variation in capacity standards (Ecuador''s sentience threshold vs India''s intrinsic dignity threshold).',
    'Narrow capacity set (rationality only): restricts personhood primarily to humans and possibly some primates, leaving most sentient beings classified as property. Broad capacity set (any sentience): cascades personhood across mammals, birds, cephalopods, potentially insects — destabilizes property regimes, transforms extractive industries. Medium capacity set (sentience + self-awareness): targets elephants, dolphins, great apes, corvids, some cetaceans — still significant but more bounded.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capacity_operationalization_ambiguity, empirical, 'Operationalization and boundary conditions for capacity-based personhood').

omega_variable(
    institutional_cascade_risk,
    'If legal personhood follows capacity regardless of species, what cascading effects does this produce across property law, contract law, criminal law, and institutional structures?',
    'Simulation via comparative law (Ecuador, India, New Zealand case law); modeling of standing requirements, guardian appointment, legal liability, property rights conversion under capacity-based personhood; institutional capacity constraints in courts, enforcement agencies, and regulatory bodies.',
    'Minimal cascade: personhood limited to narrow set of animals with explicit legal status (gorillas, elephants) — existing property regimes largely intact, institutional load manageable. Moderate cascade: personhood expands to ~50 species with demonstrated capacity — factory farming faces liability, wildlife management contracts, research institutions restructured, moderate institutional strain. Severe cascade: cascading to sentient beings generally (~millions of legally recognized beings) — property law fundamentally restructured, institutional overload, economic disruption, system-wide renegotiation of human-animal relationships.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_cascade_risk, empirical, 'Cascading institutional and economic effects of capacity-based personhood').

omega_variable(
    anthropocentric_axiom_grounding,
    'Is the anthropocentric personhood boundary grounded in a defensible empirical claim about unique human capacity, or is it a conventional institutional choice that naturalizes human privilege?',
    'Neuroscience and comparative cognition assess whether rationality/sentience is uniquely human or distributed across species. Legal history examines whether personhood restrictions track genuine capacity gaps or serve extractive interests (enslavement law, colonialism, disability exclusion — all once defended as ''natural'' boundaries now recognized as extractive). Comparative constitution law across jurisdictions with and without capacity-based personhood shows whether legal stability depends on anthropocentrism.',
    'If empirically grounded: humans do possess unique organizing capacities; personhood boundary may be justified by capacity differences. If conventional/extractive: anthropocentrism naturalizes institutional arrangements that benefit human exploitation of sentient beings; functional capacity reading unmasks the extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anthropocentric_axiom_grounding, empirical, 'Whether anthropocentric personhood is empirically grounded or extractive').

omega_variable(
    kernel_reading_contention,
    'This constraint instantiates the ''functional capacity'' reading of the legal personhood boundary kernel. What is the structural relationship between this reading and sibling readings (restrictive anthropocentric, developmental potentiality)?',
    'Comparative analysis of reading axioms: functional capacity axiom (personhood follows demonstrable cognitive capacity regardless of species) vs. anthropocentric axiom (personhood inherent in human species regardless of capacity) vs. potentiality axiom (personhood granted to entities with potential to develop capacity). Map which axiom each reading makes foundational; identify whether readings coexist in different jurisdictions or foreclose each other logically.',
    'Clarifies which claims this reading commits to and which sibling readings it excludes. Identifies false compromise positions (e.g., ''capacity for capacity'' that appears to satisfy both readings but actually privileges one). Routes reading contention to committer-system analysis rather than leaving it implicit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contention, conceptual, 'Structural relationship between functional capacity reading and sibling readings of personhood boundary kernel').

omega_variable(
    future_ai_personhood_frontier,
    'If legal personhood follows demonstrable cognitive capacity regardless of species, does this extend to artificial intelligences meeting the same capacity thresholds? What does ''species'' mean for AI?',
    'Philosophy of mind analysis of which cognitive capacities (sentience, self-awareness, goal-directedness, suffering capacity) are relevant to legal personhood and whether they are substrate-independent. AI ethics frameworks and emerging jurisdictional approaches to machine personhood (EU, China). Comparative study of whether capacity-based personhood logically entails machine personhood or if the reading can maintain a boundary at ''biological'' or ''natural'' cognition.',
    'If capacity is substrate-independent: AI systems meeting capacity thresholds become legal persons — transforms corporate law, intellectual property, machine liability. If capacity requires biological substrate: functional capacity reading avoids the AI frontier, remains restricted to non-human animals — maintains a distinction between natural and artificial cognition that may not survive empirical scrutiny.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_ai_personhood_frontier, conceptual, 'Extension of functional capacity reading to artificial intelligence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__functional_capacity_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lpb_fc_theater_t0, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 0, 0.62).
narrative_ontology:measurement(lpb_fc_theater_t5, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 5, 0.58).
narrative_ontology:measurement(lpb_fc_theater_t10, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(lpb_fc_extract_t0, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(lpb_fc_extract_t5, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(lpb_fc_extract_t10, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(lpb_fc_suppress_t0, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(lpb_fc_suppress_t5, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 5, 0.7).
narrative_ontology:measurement(lpb_fc_suppress_t10, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_personhood_boundary__functional_capacity_reading, identity_coordination).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, legal_personhood_boundary__restrictive_anthropocentric_reading).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, legal_personhood_boundary__developmental_potentiality_reading).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, animal_property_rights_extraction).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, future_artificial_mind_legal_status).

% DUAL FORMULATION NOTE:
% The legal personhood boundary kernel is instantiated by three structurally distinct constraint stories: restrictive_anthropocentric_reading (ε ≈ 0.25, Mountain — naturalizes human exceptionalism), developmental_potentiality_reading (ε ≈ 0.42, Tangled Rope — coordination of future-human representation with extraction from non-human animals), and functional_capacity_reading (ε ≈ 0.58, Tangled Rope — coordination of capacity-based representation with extraction from property holders). These are not three views of one constraint; they are three structurally distinct constraints instantiated by different epistemic and institutional commitments to the same kernel. The ε values differ because the empirical footprints differ: anthropocentrism appears immutable (low extraction visible) until its beneficiaries are identified (FSM triggers); potentiality produces moderate extraction by excluding non-human animals; functional capacity produces higher extraction by threatening property regimes. Each reading is linked via network.affects_constraints to represent their kernel family relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legal_personhood_boundary__functional_capacity_reading, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
