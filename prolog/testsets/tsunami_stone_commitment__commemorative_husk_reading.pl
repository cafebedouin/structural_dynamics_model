% ============================================================================
% CONSTRAINT STORY: tsunami_stone_commitment__commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tsunami_stone_commitment__commemorative_husk_reading, []).

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
 *   constraint_id: tsunami_stone_commitment__commemorative_husk_reading
 *   human_readable: Tsunami Stone Commitment as Commemorative Husk (Non-Protective Reading)
 *   domain: disaster_anthropology/institutional_memory/commitment_systems
 *
 * SUMMARY:
 *   Tsunami stone markers in Japan (Anpo and Hikui stones, Emeiwan monument
 *   systems, Indian Ocean region stone traditions) embody an
 *   intergenerational commitment to communicate tsunami risk through physical
 *   inscription. This constraint story instantiates ONE reading of that
 *   commitment: the commemorative husk reading, which treats stones primarily
 *   as symbolic artifacts whose protective function has decayed through
 *   institutional inattention, weak enforcement, and cultural distance from
 *   the original trauma. In this reading, the constraint extracts from future
 *   generations by creating a false sense of inherited protection — the
 *   stones' symbolic persistence suggests that ancestors' knowledge shields
 *   descendants, when in fact the actual institutional mechanisms that would
 *   enforce that protection (land-use restrictions, continuous cultural
 *   reinforcement, updated hazard assessment) have atrophied. Economic
 *   development actors benefit from the stones' non-enforcement: they can
 *   cite the markers as evidence of cultural respect while proceeding with
 *   development in formerly forbidden zones. Future coastal populations bear
 *   the extraction: they inherit a narrative that treats stone markers as
 *   equivalent to actual protective systems, without the functional
 *   guarantees those systems would provide. This reading emphasizes the decay
 *   trajectory: as stones physically deteriorate and their literal
 *   inscriptions become illegible, the contradiction between their symbolic
 *   authority and their practical non-function becomes acute. The theater
 *   ratio rises sharply as institutional actors (museums, heritage
 *   organizations, governments) invest increasingly in stone restoration and
 *   commemorative ceremonies to maintain symbolic function as physical
 *   function decays — a classic piton pattern.
 *
 * KEY AGENTS:
 *   - Future Coastal Populations: Primary victims (powerless/trapped) — inherit implicit social contract that stone markers provide protection; trapped within cultural narratives that treat past warning as present defense
 *   - Economic Development Actors & Land Use Planners: Primary beneficiaries (institutional/arbitrage) — experience constraint as purely coordinative; stones mark cultural sites without restricting development; extract benefit from low-enforcement regime
 *   - Contemporary Coastal Community Custodians: Secondary agents (moderate/constrained) — bear cognitive burden of maintaining dual narratives; genuinely committed to both cultural identity and actual protection
 *   - Cultural Heritage & Disaster Management Institutions: Institutional performative actors (institutional/arbitrage) — maintain elaborate ceremonial and preservation apparatus; benefit from custodianship role while stones decay
 *   - Organized Disaster Risk Reduction Communities: Organized agents (organized/constrained) — recognize stones' non-protection but constrained by cultural respect norms; trapped within false dichotomy between heritage and safety
 *   - Analytical Observer: Civilizational position (analytical/analytical) — risks naturalizing institutional choice (weak enforcement) as inevitable cultural-historical process
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tsunami_stone_commitment__commemorative_husk_reading, 0.58).
domain_priors:suppression_score(tsunami_stone_commitment__commemorative_husk_reading, 0.65).
domain_priors:theater_ratio(tsunami_stone_commitment__commemorative_husk_reading, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__commemorative_husk_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(tsunami_stone_commitment__commemorative_husk_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tsunami_stone_commitment__commemorative_husk_reading, snare).
narrative_ontology:human_readable(tsunami_stone_commitment__commemorative_husk_reading, "Tsunami Stone Commitment as Commemorative Husk (Non-Protective Reading)").
narrative_ontology:topic_domain(tsunami_stone_commitment__commemorative_husk_reading, "disaster_anthropology/institutional_memory/commitment_systems").

domain_priors:requires_active_enforcement(tsunami_stone_commitment__commemorative_husk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tsunami_stone_commitment__commemorative_husk_reading, '04ec1557-c2c1-4f8c-8ea0-446882668111').
narrative_ontology:cs_kernel_codification('04ec1557-c2c1-4f8c-8ea0-446882668111', fixed_text).
narrative_ontology:cs_authority_grounding('04ec1557-c2c1-4f8c-8ea0-446882668111', lineage).
narrative_ontology:cs_interpretation_layer_present('04ec1557-c2c1-4f8c-8ea0-446882668111').
narrative_ontology:cs_reading_relation('04ec1557-c2c1-4f8c-8ea0-446882668111', tsunami_stone_commitment__behavioral_competence_reading, coexists_with).
narrative_ontology:cs_axiom('04ec1557-c2c1-4f8c-8ea0-446882668111', foundational, stone_markers_commemorate_rather_than_protect).
narrative_ontology:cs_axiom_status(stone_markers_commemorate_rather_than_protect, holdable).
narrative_ontology:cs_axiom_grounding('04ec1557-c2c1-4f8c-8ea0-446882668111', stone_markers_commemorate_rather_than_protect, empirically_contingent).
narrative_ontology:cs_axiom('04ec1557-c2c1-4f8c-8ea0-446882668111', foundational, institutional_enforcement_has_materially_decayed).
narrative_ontology:cs_axiom_status(institutional_enforcement_has_materially_decayed, holdable).
narrative_ontology:cs_axiom_grounding('04ec1557-c2c1-4f8c-8ea0-446882668111', institutional_enforcement_has_materially_decayed, empirically_contingent).
narrative_ontology:cs_reference_frame('04ec1557-c2c1-4f8c-8ea0-446882668111', ancestral_protection_covenant).
narrative_ontology:cs_drift_state('04ec1557-c2c1-4f8c-8ea0-446882668111', contemporary_institutional_distance, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('04ec1557-c2c1-4f8c-8ea0-446882668111', '').
narrative_ontology:cs_kernel_id(tsunami_stone_commitment__commemorative_husk_reading, tsunami_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__commemorative_husk_reading, economic_development_actors).
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__commemorative_husk_reading, contemporary_land_use_planners).
narrative_ontology:constraint_victim(tsunami_stone_commitment__commemorative_husk_reading, future_coastal_populations).
narrative_ontology:constraint_victim(tsunami_stone_commitment__commemorative_husk_reading, institutional_memory_fidelity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FUTURE COASTAL RESIDENTS (SNARE) — Trapped by inherited cultural-historical narrative that treats stone markers as having performed their protective function through commemoration alone. No exit from the implicit social contract that past generations' warning (embodied in stones) protected future generations through intergenerational knowledge transfer. When stones decay and their literal warnings disappear, the trap is fully exposed: the constraint has extracted protective vigilance without providing protection. Maximum experienced extraction — no meaningful agency or exit.
constraint_indexing:constraint_classification(tsunami_stone_commitment__commemorative_husk_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: ECONOMIC DEVELOPMENT ACTORS & LAND USE PLANNERS (ROPE) — Experience the stone markers as purely coordinative: they solve the problem of marking culturally significant sites without imposing enforceable land-use restrictions. Decayed stones pose no obstacle to development. The constraint provides cover for development ('we respect the historical marker') while functioning as a coordination signal that carries no enforcement burden. Net beneficiary — the institutional actor gains legitimacy while maintaining functional freedom.
constraint_indexing:constraint_classification(tsunami_stone_commitment__commemorative_husk_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: CONTEMPORARY COASTAL COMMUNITY CUSTODIANS (TANGLED ROPE) — Constrained by dual obligations: maintain the historical narrative (cultural identity, intergenerational duty to ancestors) AND acknowledge the pragmatic reality that stone markers no longer protect against modern tsunami risk. Bear the cost of managing the cognitive dissonance between symbolic function and actual non-protection. Benefit from the stones' cultural and identity-coordination role; bear the extraction when the stones fail their implicit protective mandate.
constraint_indexing:constraint_classification(tsunami_stone_commitment__commemorative_husk_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 4: CULTURAL HERITAGE & DISASTER MANAGEMENT INSTITUTIONS (PITON) — Perform elaborate rituals around tsunami stone preservation, restoration, and commemorative events. The institutional apparatus (museum exhibitions, annual remembrance ceremonies, preservation standards) maintains the stones' symbolic status despite their degradation and non-function as actual warnings. Theater ratio is extremely high: the institutions perform the role of protecting and transmitting historical knowledge through the stones, but the actual protective function has decayed. Maintained through inertia and legitimacy capture (institutions benefit from custodianship role) rather than active hazard mitigation.
constraint_indexing:constraint_classification(tsunami_stone_commitment__commemorative_husk_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL DECAY VIEW (MOUNTAIN) — From a universal/civilizational lens, the decay of tsunami stones is an inevitable natural process: stone weathering, erosion, and symbolic devaluation over time are inherent to any physical warning system. The constraint appears as an immutable property of cultural memory itself — that markers and warnings always decay in efficacy as time passes and communities become distant from the original trauma. However, the structural data contradicts this naturalization: the stone markers' non-protection is not inherent to stone or time, but to the institutional choice to treat them as sufficient warning without enforcing continued knowledge transmission or land-use protection.
constraint_indexing:constraint_classification(tsunami_stone_commitment__commemorative_husk_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: ORGANIZED DISASTER RISK REDUCTION COMMUNITIES (SNARE) — Organized actors (risk scientists, disaster preparedness NGOs, some local governments) recognize the stones as cultural artifacts with degraded protective function. Constrained by the need to maintain cultural respect while advocating for supplementary modern warning systems (sirens, evacuation plans, real-time monitoring). The snare mechanism operates through institutional gatekeeping: to advocate for modern systems is framed as disrespecting ancestral knowledge, creating a false dichotomy that prevents dual protection strategies. The organized agents have moderate agency but are trapped within a frame that pits cultural preservation against actual risk reduction.
constraint_indexing:constraint_classification(tsunami_stone_commitment__commemorative_husk_reading, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tsunami_stone_commitment__commemorative_husk_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(tsunami_stone_commitment__commemorative_husk_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(tsunami_stone_commitment__commemorative_husk_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(tsunami_stone_commitment__commemorative_husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(tsunami_stone_commitment__commemorative_husk_reading, TR),
    TR >= 0.70.

:- end_tests(tsunami_stone_commitment__commemorative_husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The primary extraction mechanism is temporal asymmetry: economic benefit flows to contemporary development actors; costs flow to future residents who inherit a non-protective regime masked in protective language. The extraction is not maximal (0.8+) because the stones do provide some actual cultural knowledge transmission, some communities maintain genuine behavioral training around them, and the institutional choice to ignore them is not entirely deliberate — much is cultural drift. But the extractiveness is significant because the constraint prevents the institutional coordination (land-use enforcement, hazard updates, modern warning systems) that would actually protect future generations. Suppression (0.65): Moderate-high. Suppression operates through cultural respect narratives: to argue for updating or supplementing stone-based protection with modern systems is framed as disrespecting ancestors and cultural tradition. This is a cognitive suppression rather than legal — but effective. Development pressure suppresses hazard assessment. Theater ratio (0.78): High and rising. Contemporary institutional investment in stone restoration, preservation standards, and commemorative ceremonies substitutes for actual hazard mitigation. As physical stones decay, the theater increases: elaborate ceremonies and museum exhibitions maintain symbolic function in inverse proportion to functional decay. This is the classic piton signature — the apparatus persists through inertia and legitimacy benefit to custodian institutions, not through active protective function.
 *
 * PERSPECTIVAL GAP:
 *   This reading demonstrates a stark perspectival gulf between beneficiary and victim perspectives. Economic development actors see pure coordination (Rope) — stones mark cultural sites without restricting action. Future coastal residents see extraction (Snare) — inherited false protection leaves them vulnerable. The contemporary custodian community sees mixed coordination-extraction (Tangled Rope) — they genuinely preserve knowledge while managing cognitive dissonance. Institutional heritage apparatus sees performance legitimacy (Piton) — stones maintain symbolic authority through restoration even as functional authority decays. The analytical observer risks naturalizing this as inevitable (Mountain) — cultural memory always decays, warnings always fade — but the structural data reveals it as institutional choice: enforcement WAS present initially, and atrophied through deliberate policy relaxation and cultural distance. The perspectival gap is diagnostic: where you stand determines whether the stones coordinate protection or extract false assurance.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality follows from structural position. Future coastal residents: beneficiary status = no (they bear long-term risk); victim status = yes (non-protection harms them); exit_options = trapped (they inherit the regime; cannot choose a different community framework in advance). Derived d ≈ 0.95 (powerless victim + trapped) → high f(d) → high experienced extractiveness. Economic development actors: beneficiary status = yes (they benefit from non-enforced development); victim status = no; exit_options = arbitrage (they can pursue development elsewhere if restrictions hardened). Derived d ≈ 0.10 (institutional beneficiary + arbitrage) → negative f(d) → low/negative experienced extraction. Contemporary custodians: mixed beneficiary/victim; constrained exit (maintaining cultural identity while advocating safety creates bounded room for action). Derived d ≈ 0.55 (moderate, both) → moderate f(d) → moderate experienced extraction. The engine's directionality derivation chain produces the perspectival gap automatically from these structural declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves mandatrophy by treating the snare as genuine in victim perspective (future residents) while rope is genuine in beneficiary perspective (development actors). The snare is not misnamed extraction pretending to coordinate; it is actual extraction (non-protection) masked in coordinative language (cultural respect, heritage preservation). The mandatrophy resolution hinges on the omega variables: if stone markers actually transmit behavioral competence (omega_intergenerational_knowledge_transmission_efficacy), then some protective function persists and the constraint approaches tangled_rope. If transmission fails and stones are purely symbolic, the snare is fully exposed. The theater ratio (0.78) is high but not disqualifying for snare — the snare is the underlying mechanism; the theater is a secondary institutional response.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intentionality_of_weak_enforcement,
    'Was the weak enforcement of tsunami stone markers a deliberate institutional choice (extractive) or an emergent consequence of cultural distance from the original trauma (neutral decay)?',
    'Historical analysis of institutional decisions: Were land-use restrictions explicitly relaxed? Did early adopters of development pressures advocate against enforcement? Or did enforcement naturally erode as communities rebuilt and economic development resumed without explicit policy reversals?',
    'If deliberate choice: the constraint is a snare with intentional beneficiaries (economic actors). If emergent decay: it becomes a piton with unintended extraction. Classification holds in both cases, but the omega determines whether to attribute malice or institutional amnesia.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intentionality_of_weak_enforcement, empirical, 'Whether weak enforcement was deliberate policy choice or emergent decay').

omega_variable(
    intergenerational_knowledge_transmission_efficacy,
    'How effectively do stone markers actually transmit tsunami risk knowledge across generational boundaries in the absence of continuous institutional reinforcement?',
    'Post-event surveys and interviews with coastal residents of varying ages: Do people know the stones'' protective history? Do they alter behavior in response to stone locations? Do they treat stones as binding cultural directives or as archaeological artifacts?',
    'If transmission is effective: stones retain some protective function through cultural coordination (snare severity decreases toward tangled rope). If transmission fails: stones become pure symbolic artifacts with zero protective force (snare severity increases; extraction is maximal).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intergenerational_knowledge_transmission_efficacy, empirical, 'Intergenerational knowledge transmission efficacy of stone markers').

omega_variable(
    modern_hazard_incompleteness_of_stone_placement,
    'Are historical tsunami stone placements adequate protection against modern tsunami scenarios (different sources, different scales, climate-driven sea-level rise)?',
    'Tsunami modeling and hazard assessment: Do historical tsunami runup heights still bound modern risk? Have epicenters and frequency changed? Do climate-driven changes alter the baseline water level that stones referenced?',
    'If adequately placed: stones retain some modern protective value (constraint severity decreases). If inadequate: the stones actively mislead by suggesting safe zones that are no longer safe under current hazard profiles (snare extraction mechanism becomes more severe — false sense of protection).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(modern_hazard_incompleteness_of_stone_placement, empirical, 'Adequacy of historical stone placements under modern hazard scenarios').

omega_variable(
    reading_kernel_ambiguity,
    'Is the tsunami stone commitment fundamentally a behavioral competence mechanism (stones train and embed tsunami-avoidance cognition through cultural practice) or a commemorative artifact (stones mark historical events without requiring behavioral change)?',
    'Comparative analysis of stone traditions across regions: Do active, living stone traditions correlate with lower casualty rates? Do communities that treat stones as active behavioral guides show different evacuation timing than communities treating them as memorials? Do behavioral training mechanisms persist or fade with cultural distance from the original event?',
    'If behavioral competence: the sibling reading dominates (behavioral_competence_reading classification as Rope). If commemorative husk: this reading dominates (snare classification). If both mechanisms coexist at different institutional layers: the two readings coexist_with each other, not foreclose.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, empirical, 'Whether tsunami stones function as behavioral competence mechanisms or commemorative artifacts').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tsunami_stone_commitment__commemorative_husk_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsm_husk_tr_t0, tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(tsm_husk_tr_t10, tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 10, 0.58).
narrative_ontology:measurement(tsm_husk_tr_t20, tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 20, 0.72).
narrative_ontology:measurement(tsm_husk_tr_t30, tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 30, 0.78).

% Extraction over time
narrative_ontology:measurement(tsm_husk_be_t0, tsunami_stone_commitment__commemorative_husk_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(tsm_husk_be_t10, tsunami_stone_commitment__commemorative_husk_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(tsm_husk_be_t20, tsunami_stone_commitment__commemorative_husk_reading, base_extractiveness, 20, 0.51).
narrative_ontology:measurement(tsm_husk_be_t30, tsunami_stone_commitment__commemorative_husk_reading, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(tsm_husk_su_t0, tsunami_stone_commitment__commemorative_husk_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(tsm_husk_su_t15, tsunami_stone_commitment__commemorative_husk_reading, suppression_requirement, 15, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tsunami_stone_commitment__commemorative_husk_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(tsunami_stone_commitment__commemorative_husk_reading, 0.12).
narrative_ontology:affects_constraint(tsunami_stone_commitment__commemorative_husk_reading, tsunami_stone_commitment__behavioral_competence_reading).

% DUAL FORMULATION NOTE:
% The tsunami stone commitment kernel has two structurally distinct readings with different ε values. The commemorative husk reading (this file) treats stones as degraded cultural symbols with high extractiveness on future generations (ε=0.58, Snare). The behavioral competence reading (sibling) treats stones as active risk-reduction mechanisms with lower extractiveness (ε expected ~0.25-0.35, likely Rope or lower Tangled Rope). Both readings coexist in contemporary institutional discourse. The constraint family relationship is coexists_with: neither reading logically forecloses the other within different institutional frameworks. A community can maintain both the historical narrative (behavioral competence) AND acknowledge degradation (commemorative husk) through dual institutional mechanisms (living tradition + museum preservation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tsunami_stone_commitment__commemorative_husk_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
