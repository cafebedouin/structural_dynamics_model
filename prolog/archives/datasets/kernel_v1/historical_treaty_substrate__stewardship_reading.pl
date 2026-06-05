% ============================================================================
% CONSTRAINT STORY: historical_treaty_substrate__stewardship_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_historical_treaty_substrate__stewardship_reading, []).

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
 *   constraint_id: historical_treaty_substrate__stewardship_reading
 *   human_readable: Historical Treaty Substrate (Stewardship Reading)
 *   domain: legal_anthropology/indigenous_law/comparative_constitutional_theory
 *
 * SUMMARY:
 *   The historical treaty substrate represents a fundamental fault line in
 *   post-colonial constitutional law: whether treaties between Indigenous
 *   nations and settler states functioned as instruments of Indigenous
 *   dispossession (extinguishment reading) or as binding relational pacts
 *   establishing mutual obligations for territorial stewardship (stewardship
 *   reading). The stewardship reading holds that treaties, properly
 *   interpreted, preserve Indigenous jurisdiction over ancestral territories
 *   and establish joint governance frameworks rather than ceding sovereignty.
 *   This constraint exhibits the structural signature of a contested kernel—a
 *   single set of historical documents (the treaties themselves) that
 *   different parties read as instantiating fundamentally different
 *   commitments. The stewardship reading is one parsing of this kernel,
 *   grounded in Indigenous legal traditions and contemporary constitutional
 *   theory that recognizes Indigenous nations as sovereigns with inherent
 *   territorial rights. The constraint operates at multiple scales:
 *   interpersonal (settler individuals occupying treaty lands), institutional
 *   (settler-state courts interpreting treaties), and international (global
 *   Indigenous sovereignty movements). The measurements track extraction and
 *   theater dynamics over a 100-year interval, showing how the extractiveness
 *   of the stewardship framework has declined (from 0.72 at the time of heavy
 *   colonial resource extraction and treaty violation, to 0.38 in a scenario
 *   where stewardship principles are substantially institutionalized) as
 *   theater has also declined (from 0.72, when treaties were pure
 *   performance, to 0.55, when stewardship functions are partially
 *   operationalized).
 *
 * KEY AGENTS:
 *   - Indigenous Nations: Primary beneficiary group (organized/arbitrage) — hold stewardship jurisdiction and governing authority; capacity to assert treaty rights through multiple channels
 *   - Settler State Administration: Institutional actor bound by treaty obligations (institutional/constrained) — constrained by joint governance requirements, consultation protocols, and recognition of Indigenous jurisdiction
 *   - Resource-Extractive Sector (mining, forestry, energy): Indirect victim group (powerful/mobile) — capacity to extract resources is constrained by stewardship framework requiring Indigenous consent
 *   - Treaty Enforcement Institutions: Powerless structural participant (powerless/trapped) — no exit from the requirement to adjudicate Indigenous-settler disputes, yet embedded in settler-state legitimacy structures
 *   - Transnational Indigenous Movements: Organized political actors (powerful/mobile) — can leverage international law, inter-tribal coordination, and direct action to enforce stewardship principles
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — at risk of naturalizing either stewardship or extinguishment as inevitable law rather than contingent reading of contested documents
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__stewardship_reading, 0.38).
domain_priors:suppression_score(historical_treaty_substrate__stewardship_reading, 0.45).
domain_priors:theater_ratio(historical_treaty_substrate__stewardship_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__stewardship_reading, tangled_rope).
narrative_ontology:human_readable(historical_treaty_substrate__stewardship_reading, "Historical Treaty Substrate (Stewardship Reading)").
narrative_ontology:topic_domain(historical_treaty_substrate__stewardship_reading, "legal_anthropology/indigenous_law/comparative_constitutional_theory").

domain_priors:requires_active_enforcement(historical_treaty_substrate__stewardship_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__stewardship_reading, '28ea0600-e0bb-45a6-ab1b-a8de3908aba1').
narrative_ontology:cs_kernel_codification('28ea0600-e0bb-45a6-ab1b-a8de3908aba1', fixed_text).
narrative_ontology:cs_authority_grounding('28ea0600-e0bb-45a6-ab1b-a8de3908aba1', lineage).
narrative_ontology:cs_interpretation_layer_present('28ea0600-e0bb-45a6-ab1b-a8de3908aba1').
narrative_ontology:cs_reading_relation('28ea0600-e0bb-45a6-ab1b-a8de3908aba1', historical_treaty_substrate__extinguishment_reading, forecloses).
narrative_ontology:cs_reading_relation('28ea0600-e0bb-45a6-ab1b-a8de3908aba1', historical_treaty_substrate__nation_to_nation_reading, influences).
narrative_ontology:cs_axiom('28ea0600-e0bb-45a6-ab1b-a8de3908aba1', foundational, indigenous_inherent_territorial_sovereignty).
narrative_ontology:cs_axiom_status(indigenous_inherent_territorial_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('28ea0600-e0bb-45a6-ab1b-a8de3908aba1', indigenous_inherent_territorial_sovereignty, deontological).
narrative_ontology:cs_axiom('28ea0600-e0bb-45a6-ab1b-a8de3908aba1', foundational, treaty_as_binding_relational_pact).
narrative_ontology:cs_axiom_status(treaty_as_binding_relational_pact, holdable).
narrative_ontology:cs_axiom_grounding('28ea0600-e0bb-45a6-ab1b-a8de3908aba1', treaty_as_binding_relational_pact, deontological).
narrative_ontology:cs_reference_frame('28ea0600-e0bb-45a6-ab1b-a8de3908aba1', mutual_territorial_stewardship_pact).
narrative_ontology:cs_drift_state('28ea0600-e0bb-45a6-ab1b-a8de3908aba1', contemporary_post_colonial_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('28ea0600-e0bb-45a6-ab1b-a8de3908aba1', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__stewardship_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__stewardship_reading, indigenous_nations).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__stewardship_reading, territorial_ecosystem_health).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__stewardship_reading, shared_governance_framework).
narrative_ontology:constraint_victim(historical_treaty_substrate__stewardship_reading, settler_state_unilateral_authority).
narrative_ontology:constraint_victim(historical_treaty_substrate__stewardship_reading, extraction_capacity_of_settler_economy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIGENOUS NATIONS (ROPE) — Under this reading, Indigenous nations are primary beneficiaries holding genuine stewardship rights and governance authority over ancestral territories. The treaty is coordination—establishing mutually intelligible protocols for resource management, dispute resolution, and territorial access. Exit option is arbitrage: nations can enforce their treaty rights through litigation, assertion of jurisdiction over traditional territories, or non-cooperation with settler-state resource extraction schemes. Experienced constraint is primarily coordination overhead (low extraction) rather than subjugation.
constraint_indexing:constraint_classification(historical_treaty_substrate__stewardship_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 2: SETTLER STATE ADMINISTRATION (TANGLED ROPE) — From the institutional perspective of colonial-era and post-colonial settler states, the stewardship reading imposes genuine coordination burdens (establishing joint governance procedures, obtaining consent for resource projects, recognizing Indigenous jurisdiction) alongside extraction constraints (loss of unilateral authority over territories and resources). The state benefits from territorial legitimacy and resource access, but only through negotiated frameworks that require continuous enforcement of consent protocols. Exit is constrained: the state cannot simply repudiate the treaty without massive legitimacy collapse domestically and internationally.
constraint_indexing:constraint_classification(historical_treaty_substrate__stewardship_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TREATY ENFORCEMENT CAPACITY / STRUCTURAL GAP (SNARE) — No Indigenous nation can exit from the necessity of enforcing their treaty rights, yet enforcement mechanisms remain asymmetrically weighted toward settler-state courts and institutions that have historically ignored or rewritten treaties. The powerless participant here is the abstract structural requirement for enforcement itself—the constraint cannot be satisfied because the institutions that govern outcomes (courts, executive agencies, international bodies) are embedded within the settler state's legitimacy structure. This perspective sees high suppression: barriers include state monopoly on law enforcement, resource disparities in litigation, and the epistemic capture of legal interpretation by settler-state courts.
constraint_indexing:constraint_classification(historical_treaty_substrate__stewardship_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational timescale, territorial stewardship by human communities is presented as a natural law: humans cannot occupy territory without establishing protocols for resource access and governance; territory-dwellers have inherent claims to that territory that no external authority can fully extinguish. This perspective classifies the constraint as an immutable structural feature of how territorial authority works. However, the base properties contradict the mountain classification—the beneficiaries and enforcement requirements suggest this is a false summit, revealing that 'natural law' of stewardship masks the contingent historical fact that Indigenous nations were dispossessed and must now reassert stewardship through treaty reinterpretation.
constraint_indexing:constraint_classification(historical_treaty_substrate__stewardship_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: FORMAL TREATY TEXT WITHOUT SUBSTANCE (PITON) — Many historical treaties have been reduced to performative displays of settler-state benevolence without functional enforcement of Indigenous governance rights. The treaty ritual persists (annual acknowledgments, heritage months, consultation protocols) while substantive stewardship authority remains with the settler state. Theater ratio is elevated because the formal treaty structure is maintained (≥0.55) while core coordination function—joint resource governance—is theatricalized rather than operationalized. This perspective sees the constraint as a vestigial institutional form maintained through inertia.
constraint_indexing:constraint_classification(historical_treaty_substrate__stewardship_reading, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: TRANSNATIONAL INDIGENOUS MOVEMENTS (TANGLED ROPE) — Organized indigenous networks across multiple states (Amazon, Pacific, North America) experience the stewardship reading as both coordination (enabling inter-tribal resource protocols and knowledge-sharing) and extraction (having to fight within settler-state legal frameworks to assert rights). Exit option is mobile: movements can shift strategies (UN declarations, international litigation, direct action), build coalition power, or pursue sovereignty assertions outside the treaty framework. Effective extraction is moderate because of organizational capacity and strategic options.
constraint_indexing:constraint_classification(historical_treaty_substrate__stewardship_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(historical_treaty_substrate__stewardship_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(historical_treaty_substrate__stewardship_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(historical_treaty_substrate__stewardship_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(historical_treaty_substrate__stewardship_reading, TR),
    TR >= 0.70.

:- end_tests(historical_treaty_substrate__stewardship_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38, declining from 0.72): The stewardship reading constrains settler-state unilateral authority over territories and resources. The extractiveness metric captures the degree to which the settler state (or resource-extractive capital) can extract value from territories against Indigenous interests. At t=0 (colonial era), extractiveness was near-total (0.72) because treaties were systematically violated and Indigenous governance was ignored. As stewardship principles are institutionalized (t=100), extractiveness declines (0.38) because genuine joint governance emerges. The current value (0.38) reflects that stewardship frameworks still permit significant settler-state extraction through negotiated resource-sharing and that enforcement remains incomplete. Suppression (0.45, declining from 0.68): Suppression measures barriers to Indigenous assertion of stewardship rights. Colonial-era suppression was high (0.68): enforced ignorance of treaties, legal exclusion of Indigenous testimony, military coercion. Contemporary suppression (0.45) is lower but persistent: litigation costs, unequal bargaining power, state monopoly on enforcement mechanisms. Theater Ratio (0.55, declining from 0.72): Theater measures performative vs functional aspects of the constraint. Colonial-era treaties were pure theater (0.72)—signed and immediately violated, existing only as symbolic artifacts. Contemporary treaties show mixed function and performance (0.55)—some joint governance works, some consultation is cosmetic, enforcement is uneven. The trend of declining theater suggests stewardship frameworks are becoming more operationalized, though the gap between formal recognition and substantive governance persists.
 *
 * PERSPECTIVAL GAP:
 *   The stewardship reading generates maximal perspectival divergence across the observation site. Indigenous nations with arbitrage options (coalition power, litigation capacity, international backing) see Rope—genuine coordination to solve problems of territorial coexistence and resource access. The settler state sees Tangled Rope—it gains from territorial legitimacy and resource access but loses unilateral authority and faces continuous enforcement demands. The enforcement-mechanism gap (abstract structural requirement for adjudication between sovereigns embedded in a hierarchy of settler-state institutions) sees Snare—no exit from the requirement to enforce, yet the institutions that adjudicate are structurally biased. Resource-extractive capital sees constraints on extraction as snare-like (suppression of their preferred economic activity). Transnational Indigenous movements see strategic coordination opportunity (Tangled Rope with mobile exit through coalition building). The formal treaty text without functional substance sees Piton—performative acknowledgment without jurisdiction transfer. The civilizational analytical view risks Mountain (territorial stewardship as inherent natural law) but the base properties reveal false summit: the stewardship principle requires continuous institutional assertion, not passive acceptance.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) computation traces from the structural position of each agent relative to the extraction flow. Indigenous nations as beneficiaries with arbitrage options (coalition power, international support) derive d ≈ 0.30 → f(d) ≈ 0.20 → low experienced extraction. Settler state as institutional actor with constrained exit (treaty repudiation is politically impossible) derives d ≈ 0.55 → f(d) ≈ 0.75 → moderate experienced extraction (constraint feels real). The abstract enforcement-requirement gap as powerless/trapped derives d ≈ 0.95 → f(d) ≈ 1.42 → maximum experienced extraction (the institution cannot escape the requirement to adjudicate). Resource extractors derive d ≈ 0.80 (constrained by consent requirements) → f(d) ≈ 1.20 → high experienced extraction. These derivations generate the perspectival spread: the same baseline extractiveness (0.38) is experienced as low coordination overhead (Rope) by beneficiaries, moderate mixed constraint (Tangled Rope) by institutions, high suppression (Snare) by abstract structural requirements, and moderate constraint (Tangled Rope) by organized movements.
 *
 * MANDATROPHY ANALYSIS:
 *   The stewardship reading avoids mandatrophy by clearly distinguishing the coordination function (establishing mutually intelligible protocols for territorial coexistence) from the extraction asymmetry (settler states historically violated treaty terms and extracted resources unilaterally). The Tangled Rope classification captures this hybrid: genuine coordination is required (treaties do establish frameworks for dispute resolution, resource management, cultural exchange) alongside asymmetric extraction (settler states have historically extracted maximum value while ignoring Indigenous governance). The piton perspective (formal treaty text without substance) identifies where mandatrophy risk appears—when the treaty becomes a performative symbol divorced from function. The snare perspective on enforcement mechanisms reveals the structural vulnerability: if enforcement institutions are captured by settler-state interests, the constraint degrades from Tangled Rope (mixed coordination and extraction) to pure Snare (only suppression, no coordination function). The analytical mountain perspective risks mandatrophy by naturalizing stewardship as inevitable law rather than acknowledging it as a contingent reading requiring continuous institutional assertion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stewardship_vs_sovereignty,
    'Does the stewardship reading preserve Indigenous sovereignty or subordinate it to a joint-governance framework that requires settler-state consent?',
    'Comparative case analysis: jurisdictional authority in post-treaty Indigenous governance vs pre-contact Indigenous governance; measurement of unilateral Indigenous authority to exclude settler-state actors from territories and resources without state veto; documentation of actual enforcement patterns when Indigenous governance decisions conflict with settler-state interests.',
    'If stewardship preserves unilateral Indigenous sovereignty: classification remains Rope/Tangled Rope from Indigenous perspective, Snare reclassifies as coordinating constraint. If stewardship subordinates to joint governance requiring settler veto: classification shifts to Snare from Indigenous perspective (sovereignty is constrained, not preserved).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(stewardship_vs_sovereignty, empirical, 'Whether stewardship reading preserves or subordinates Indigenous sovereignty').

omega_variable(
    treaty_reinterpretation_as_reading,
    'Is the stewardship reading a genuine alternative interpretation of the historical treaty texts, or a retrospective constructive reframing imposed by contemporary Indigenous movements and allied legal theorists?',
    'Linguistic-historical analysis of original treaty documents (colonial-era language, witness accounts, known interpretive disputes at time of signing); correlation with demonstrated treaty-author intent (Indigenous signatories'' documented understanding vs settler-official understanding); documentation of when stewardship reading first appeared in legal scholarship vs when it gained institutional recognition.',
    'If genuine alternative interpretation: stewardship reading has equal epistemic standing to extinguishment reading—coexists_with relation is correct. If retrospective reframing: stewardship reading influences but does not foreclose extinguishment reading—influences relation is correct, and the axiom grounding_type shifts from deontological (inherent Indigenous rights) to instrumental (contemporary legitimacy strategy).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_reinterpretation_as_reading, conceptual, 'Whether stewardship reading is ancient interpretation or modern reframing').

omega_variable(
    enforcement_mechanism_adequacy,
    'What enforcement mechanisms would transform the stewardship reading from formal coordination to functional joint governance? Are settler-state courts competent to adjudicate Indigenous-settler disputes within a stewardship framework?',
    'Documentation of actual court decisions on treaty interpretation; measurement of alignment between Indigenous governance decisions and settler-state court validation; case studies of successful enforcement vs unilateral treaty violations by settler states; comparative analysis of parallel dispute-resolution mechanisms (Indigenous courts, international bodies, bilateral commissions).',
    'If settler-state courts prove unreliable adjudicators: suppression increases (enforcement mechanisms fail), piton perspective strengthened, classification shifts toward Snare from Indigenous perspective. If Indigenous-settler dispute resolution works: suppression decreases, functional coordination is achieved, Tangled Rope classification stabilizes across perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_mechanism_adequacy, empirical, 'Adequacy of enforcement mechanisms for functional joint governance').

omega_variable(
    resource_extraction_boundary,
    'In the stewardship reading, what constitutes the boundary between joint resource governance (coordination) and unilateral settler-state resource extraction (snare)? Where does mining, forestry, or energy development sit in the stewardship framework?',
    'Documented cases where settler states extracted resources over Indigenous objection; analysis of whether treaties granted Indigenous veto power or only consultation rights; measurement of extractiveness in resource sectors vs non-resource sectors; longitudinal tracking of resource revenue and environmental degradation under treaties vs without.',
    'If stewardship reading grants Indigenous veto: extraction is genuinely constrained, Tangled Rope classification holds. If stewardship reading grants only consultation: extraction is cosmetically constrained, theater_ratio increases, Piton perspective strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(resource_extraction_boundary, empirical, 'Boundary between joint governance and unilateral extraction in resource sectors').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__stewardship_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hts_steward_theater_t0, historical_treaty_substrate__stewardship_reading, theater_ratio, 0, 0.72).
narrative_ontology:measurement(hts_steward_theater_t50, historical_treaty_substrate__stewardship_reading, theater_ratio, 50, 0.64).
narrative_ontology:measurement(hts_steward_theater_t100, historical_treaty_substrate__stewardship_reading, theater_ratio, 100, 0.55).

% Extraction over time
narrative_ontology:measurement(hts_steward_extract_t0, historical_treaty_substrate__stewardship_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(hts_steward_extract_t50, historical_treaty_substrate__stewardship_reading, base_extractiveness, 50, 0.55).
narrative_ontology:measurement(hts_steward_extract_t100, historical_treaty_substrate__stewardship_reading, base_extractiveness, 100, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(hts_steward_suppression_t0, historical_treaty_substrate__stewardship_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(hts_steward_suppression_t50, historical_treaty_substrate__stewardship_reading, suppression_requirement, 50, 0.57).
narrative_ontology:measurement(hts_steward_suppression_t100, historical_treaty_substrate__stewardship_reading, suppression_requirement, 100, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(historical_treaty_substrate__stewardship_reading, resource_allocation).
narrative_ontology:affects_constraint(historical_treaty_substrate__stewardship_reading, historical_treaty_substrate__extinguishment_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__stewardship_reading, historical_treaty_substrate__nation_to_nation_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__stewardship_reading, settler_colonial_resource_extraction).
narrative_ontology:affects_constraint(historical_treaty_substrate__stewardship_reading, indigenous_jurisdiction_recognition).

% DUAL FORMULATION NOTE:
% The historical_treaty_substrate kernel instantiates three distinct constraint stories corresponding to three different readings of the same treaty documents. The stewardship_reading is one reading; extinguishment_reading is a second; nation_to_nation_reading is a third. Each reading has its own epsilon value reflecting the degree of Indigenous jurisdiction and settler-state obligation that the reading secures. The stewardship reading (epsilon=0.38) assumes significant Indigenous governance authority; the extinguishment reading (epsilon=0.72+) assumes minimal; the nation_to_nation reading sits between with perpetually contested jurisdiction. All three are network-linked because they are alternative interpretations of the same historical documents and any shift in institutional recognition of one reading affects the strategic landscape for the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(historical_treaty_substrate__stewardship_reading, institutional, 0.55).
constraint_indexing:directionality_override(historical_treaty_substrate__stewardship_reading, powerful, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
