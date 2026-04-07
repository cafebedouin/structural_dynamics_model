% ============================================================================
% CONSTRAINT STORY: sotu_1960_eisenhower_antarctica_treaty_inspection
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1960_eisenhower_antarctica_treaty_inspection, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sotu_1960_eisenhower_antarctica_treaty_inspection
 *   human_readable: Antarctica Treaty System Multilateral Inspection Regime
 *   domain: governance/international_law
 *
 * SUMMARY:
 *   The Antarctica Treaty System (1961) established Antarctica as territory
 *   reserved exclusively for peaceful scientific use, with a novel
 *   supranational inspection regime as the enforcement mechanism. The treaty
 *   itself was a historic compromise: nuclear powers accepted constraints on
 *   military activity in exchange for strengthened nonproliferation
 *   verification; scientific community gained coordinated research access;
 *   claimant states maintained dormant territorial claims; non-claimants
 *   gained strategic participation. The multilateral inspection regime
 *   represents a structural innovation in governance — it shifted
 *   verification authority from unilateral national capability to coordinated
 *   international teams. The constraint exhibits all characteristics of a
 *   tangled rope: genuine coordination benefits (scientific cooperation,
 *   nonproliferation assurance) exist alongside real extraction costs
 *   (sovereignty constraints on signatory states, exclusion of
 *   non-signatories, asymmetric research capacity). The theater ratio (0.58)
 *   reflects that inspections have become increasingly performative over time
 *   — Cold War-era inspections had real deterrent function, but modern
 *   inspections are largely ceremonial given satellite monitoring capacity
 *   and the absence of any actual detected violations.
 *
 * KEY AGENTS:
 *   - Scientific Research Community: Primary beneficiary (institutional/arbitrage) — gains coordinated research access, institutional prestige, multinational collaboration framework
 *   - Nuclear Non-Proliferation Coalition: Primary beneficiary (organized/constrained) — gains credible verification mechanism for nonproliferation commitment in strategic region
 *   - Nuclear-Armed Signatory States: Primary victim (powerful/constrained) — bears sovereignty constraints on nuclear testing and weapons development; gains nonproliferation legitimacy elsewhere
 *   - Non-Nuclear Signatory States: Secondary victim (powerful/constrained) — constrained by resource extraction restrictions; gains research access and strategic participation
 *   - Non-Signatories and Developing Nations: Powerless victim (powerless/trapped) — excluded from governance and resource access; no exit option without geopolitical costs
 *   - Antarctic Treaty Inspection Bureaucracy: Institutional observer (institutional/arbitrage) — maintains ritual verification structure through inertia
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing supranational inspection as inevitable necessity rather than contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1960_eisenhower_antarctica_treaty_inspection, 0.38).
domain_priors:suppression_score(sotu_1960_eisenhower_antarctica_treaty_inspection, 0.42).
domain_priors:theater_ratio(sotu_1960_eisenhower_antarctica_treaty_inspection, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1960_eisenhower_antarctica_treaty_inspection, extractiveness, 0.38).
narrative_ontology:constraint_metric(sotu_1960_eisenhower_antarctica_treaty_inspection, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(sotu_1960_eisenhower_antarctica_treaty_inspection, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1960_eisenhower_antarctica_treaty_inspection, tangled_rope).
narrative_ontology:human_readable(sotu_1960_eisenhower_antarctica_treaty_inspection, "Antarctica Treaty System Multilateral Inspection Regime").
narrative_ontology:topic_domain(sotu_1960_eisenhower_antarctica_treaty_inspection, "governance/international_law").

domain_priors:requires_active_enforcement(sotu_1960_eisenhower_antarctica_treaty_inspection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1960_eisenhower_antarctica_treaty_inspection, scientific_research_community).
narrative_ontology:constraint_beneficiary(sotu_1960_eisenhower_antarctica_treaty_inspection, nuclear_nonproliferation_coalition).
narrative_ontology:constraint_victim(sotu_1960_eisenhower_antarctica_treaty_inspection, national_sovereignty_constraints).
narrative_ontology:constraint_victim(sotu_1960_eisenhower_antarctica_treaty_inspection, strategic_resource_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ANTARCTIC ECOSYSTEM / NON-SIGNATORIES (SNARE) — Non-signatories (particularly developing nations) cannot access Antarctic resources or participate in governance decisions despite bearing environmental consequences of signatories' activities. Trapped by territorial exclusion and unable to exit without geopolitical costs. The ecosystem itself has no representation in the treaty system. Maximum extraction experienced by agents without voice or exit.
constraint_indexing:constraint_classification(sotu_1960_eisenhower_antarctica_treaty_inspection, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SCIENTIFIC RESEARCH COMMUNITY (ROPE) — Benefits substantially from the treaty's peaceful-use-only framework and multinational inspection legitimacy. Experiences coordination benefits (shared research standards, institutional cooperation, free access to research sites) that exceed coordination costs. Can exit by independent research but chooses institutional participation for access and prestige.
constraint_indexing:constraint_classification(sotu_1960_eisenhower_antarctica_treaty_inspection, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: NUCLEAR-ARMED SIGNATORY STATES (TANGLED ROPE) — Experience mixed extraction and coordination. Genuine coordination benefit: credible verification mechanism for nuclear non-proliferation in a geopolitically strategic region without requiring full disarmament. But also bear sovereignty constraints — cannot conduct nuclear tests or weapons development in Antarctica even if domestically legal. Exit is costly (breach of multilateral commitment, sanctions, loss of inspection legitimacy for arms control elsewhere) but technically possible. Active enforcement required: inspection regime must remain functional.
constraint_indexing:constraint_classification(sotu_1960_eisenhower_antarctica_treaty_inspection, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: NON-NUCLEAR SIGNATORY STATES (TANGLED ROPE) — Coordinate scientific access and claim strategic interests through the treaty while constrained by restrictions on resource extraction and military infrastructure. Experience less severe extraction than nuclear states (no weapons testing prohibition mirrors their non-nuclear status) but also less leverage in enforcement mechanisms. Constrained by the practical cost of unilateral breach given allied dependence.
constraint_indexing:constraint_classification(sotu_1960_eisenhower_antarctica_treaty_inspection, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ANTARCTIC TREATY INSPECTION BUREAUCRACY (PITON) — The inspection regime itself has become substantially performative over time. Inspections occur but verification capacity is limited by Antarctic remoteness, inspection scheduling coordination, and lack of binding enforcement mechanisms for violations. The ritual persists through institutional inertia and as a visible commitment to nonproliferation norms. Theater ratio reflects that ceremonial inspections maintain legitimacy without resolving underlying sovereignty ambiguities.
constraint_indexing:constraint_classification(sotu_1960_eisenhower_antarctica_treaty_inspection, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: NUCLEAR NON-PROLIFERATION COALITION (SCAFFOLD) — The inspection regime functions as a sunset mechanism for a specific historical threat: preventing nuclear weapons proliferation into a region without existing military infrastructure. As nonproliferation norms strengthen through the NPT (1968) and verification becomes better integrated with global monitoring, the Antarctica-specific inspection regime becomes less central. The constraint exhibits sunset properties: it solves a bounded problem (preventing Antarctic militarization during Cold War) with expected decline as global nonproliferation architecture matures.
constraint_indexing:constraint_classification(sotu_1960_eisenhower_antarctica_treaty_inspection, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / STRUCTURAL INEVITABILITY (MOUNTAIN) — From a civilizational perspective, any shared resource governance requires supranational verification as an immutable structural necessity — trustless systems always need monitoring to enforce agreements. The inspection regime appears as a natural law of collective action. However, the base properties reveal this as a false summit: the 'necessity' naturalizes the specific institutional choice of supranational inspection over other verification mechanisms (satellite monitoring, decentralized reporting, penalties for breach).
constraint_indexing:constraint_classification(sotu_1960_eisenhower_antarctica_treaty_inspection, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1960_eisenhower_antarctica_treaty_inspection_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1960_eisenhower_antarctica_treaty_inspection, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1960_eisenhower_antarctica_treaty_inspection, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1960_eisenhower_antarctica_treaty_inspection, TR),
    TR >= 0.70.

:- end_tests(sotu_1960_eisenhower_antarctica_treaty_inspection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint redistributes sovereignty without confiscating it — signatories retain Antarctic territory and can conduct science/environmental research, but cannot conduct military activities or resource extraction. The extraction is significant but not total. The asymmetry concentrates on states with actual or potential military/resource interests in Antarctica, making it bearable for the larger scientific constituency. Suppression (0.42): Moderate. Signatories can exit the treaty (and a few have threatened to) but face substantial geopolitical costs — loss of research legitimacy, damage to nonproliferation commitments elsewhere, potential sanctions. Non-signatories face higher suppression (no legitimate access), but since they were never treaty members, the exit barrier is framed as non-membership rather than constraint violation. Theater ratio (0.58): Moderate-high and rising. Early inspections (1960s-1980s) had real verification function during Cold War. Modern inspections are largely ceremonial — satellite monitoring provides better detection than ground teams, yet inspections persist. The rise from 0.35 to 0.58 reflects the constraint's transition from functioning verification mechanism to legitimation ritual (piton pattern).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence across power levels and exit options. The scientific community sees coordination and genuine benefit (Rope) — they experience the treaty as enabling shared research. The nuclear coalition sees a temporary problem being solved (Scaffold) — the inspection regime addresses the specific Cold War threat of Antarctic militarization with expected sunset as global nonproliferation architecture strengthens. The Antarctic treaty bureaucracy sees its own degraded ritual (Piton) — inspections persist through institutional commitment despite reduced verification necessity. Powerful signatories see mixed extraction and coordination (Tangled Rope) — they gain nonproliferation assurance but pay sovereignty costs. Non-signatories see pure exclusion (Snare) — trapped without voice or exit. The civilizational analytical observer risks seeing an immutable necessity (Mountain) — 'supranational inspection is inevitable for shared resource governance' — but this naturalizes a specific institutional choice. Satellite monitoring, unilateral deterrence through military presence, or decentralized reporting could serve the same nonproliferation function with different extraction profiles.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim structure differentiates directionality across agent types. The scientific research community benefits from the constraint (low d ~0.20, institutional power + arbitrage exit → low/negative chi). Nuclear powers experience mixed effects: they benefit from nonproliferation assurance elsewhere but bear Antarctic-specific sovereignty costs (d ~0.50, powerful + constrained). Non-nuclear signatories face extraction with less compensation (d ~0.55, constrained without weapons interest). Non-signatories face full extraction with no exit (d ~0.95, trapped). The piton perspective reflects institutional inertia: the inspection bureaucracy has arbitrage options and benefits from its own continuation (low d), but the function has degraded. The mountain perspective risks naturalizing the supranational inspection mechanism as inevitable — the false summit detector should flag that alternatives (satellite verification, decentralized reporting, penalties for breach) could achieve the same nonproliferation function with different extraction profiles.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by exposing how classification depends on structural position and exit options, not on the constraint's objective properties. The base extractiveness (0.38) is stable, but classification ranges from Snare (for non-signatories) through Tangled Rope (for signatories) to Rope (for scientists) to Piton (for the inspection bureaucracy) to Scaffold (for the nonproliferation coalition) to Mountain (at civilizational scale). The apparent contradiction dissolves when recognizing that 'the constraint' is not a unified object but a presheaf of structural relationships. The scientific community genuinely experiences coordination benefits that make the constraint functional (Rope). The nuclear states experience mixed coordination and extraction (Tangled Rope). Non-signatories experience pure exclusion (Snare). The inspection bureaucracy experiences degraded function (Piton). Each classification is locally correct for its position. The mandatrophy is not resolved by choosing one type but by recognizing that the constraint manifests as different types depending on observer position within the treaty structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inspection_regime_actual_deterrent_capacity,
    'Does the multilateral inspection regime actually deter prohibited activities, or does it function primarily as a legitimation ritual for nonproliferation commitments made for other strategic reasons?',
    'Historical analysis of detected violations vs. undiscovered violations; comparative study of inspection regime enforcement effectiveness vs. satellite monitoring and other detection methods; state behavior analysis before/after inspection visits',
    'If actual deterrent: inspection regime is genuine coordination mechanism (Rope classification stronger). If primarily ritual: theater_ratio should increase, piton classification stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inspection_regime_actual_deterrent_capacity, empirical, 'Whether inspection regime provides actual verification vs. legitimation theater').

omega_variable(
    sovereignty_constraint_extraction_direction,
    'Which states bear the actual sovereignty costs of the inspection regime, and do those costs concentrate on signatory states with Antarctic claims vs. those without?',
    'Detailed analysis of Antarctic territorial claims overlap with treaty boundaries; state-by-state inventory of restricted activities; comparison of resource extraction costs for signatories vs. what they would do without the treaty',
    'If costs concentrate on claimant states: extraction asymmetry is structural (tangled_rope confirmed). If distributed equally: constraint is purer coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_constraint_extraction_direction, empirical, 'Which states bear sovereignty extraction costs').

omega_variable(
    scientific_research_access_distribution,
    'Is the scientific research access benefit genuinely distributed to all signatories, or concentrated on wealthy states with Antarctic research capacity?',
    'Analysis of research station locations, funding sources, and researcher nationality; comparison of research output and opportunities for developing vs. developed signatory states',
    'If concentrated: scientific benefit justifies extraction only for wealthy states, making constraint Snare from developing signatory perspective. If distributed: coordination function is genuine.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scientific_research_access_distribution, empirical, 'Whether scientific research access benefits are distributed or concentrated').

omega_variable(
    false_summit_diagnosis,
    'Is the multilateral inspection regime a natural law of collective action, or a specific institutional choice among alternatives that could serve the same nonproliferation function?',
    'Counterfactual analysis: comparison with satellite-based monitoring, unilateral deterrence through military presence, or decentralized reporting systems; examination of why supranational inspection was chosen over alternatives',
    'If natural law: mountain classification confirmed (accessibility_collapse = how difficult to imagine alternatives). If institutional choice: false summit detected; reclassify toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_diagnosis, conceptual, 'Whether multilateral inspection is inevitable or contingent institutional choice').

omega_variable(
    nonproliferation_mechanism_dependency,
    'How dependent is the Antarctica Treaty''s nonproliferation function on the multilateral inspection regime specifically, vs. the broader institutional commitment and reputational costs of treaty violation?',
    'Comparative case study: nonproliferation success under different verification regimes (inspections vs. satellite-only vs. self-reporting); analysis of what would change if inspections were eliminated but other treaty commitments remained',
    'If inspection-dependent: constraint is essential coordination mechanism. If reputational-dependent: inspection may be piton (theater without functional content).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nonproliferation_mechanism_dependency, empirical, 'Whether nonproliferation depends on inspection mechanism or treaty commitment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1960_eisenhower_antarctica_treaty_inspection, 1960, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(antarctica_tr_t0, sotu_1960_eisenhower_antarctica_treaty_inspection, theater_ratio, 0, 0.35).
narrative_ontology:measurement(antarctica_tr_t5, sotu_1960_eisenhower_antarctica_treaty_inspection, theater_ratio, 5, 0.48).
narrative_ontology:measurement(antarctica_tr_t10, sotu_1960_eisenhower_antarctica_treaty_inspection, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(antarctica_be_t0, sotu_1960_eisenhower_antarctica_treaty_inspection, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(antarctica_be_t5, sotu_1960_eisenhower_antarctica_treaty_inspection, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(antarctica_be_t10, sotu_1960_eisenhower_antarctica_treaty_inspection, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1960_eisenhower_antarctica_treaty_inspection, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_1960_eisenhower_antarctica_treaty_inspection, nuclear_nonproliferation_treaty_verification).
narrative_ontology:affects_constraint(sotu_1960_eisenhower_antarctica_treaty_inspection, supranational_sovereignty_constraints).
narrative_ontology:affects_constraint(sotu_1960_eisenhower_antarctica_treaty_inspection, scientific_research_governance).

% DUAL FORMULATION NOTE:
% The Antarctica Treaty System decomposes into multiple constraint families. This story captures the inspection regime's hybrid coordination-extraction structure. Upstream: international law norms establishing supranational verification as legitimate (affects this constraint). Downstream: NPT verification mechanisms and other arms control regimes that depend on the Antarctica Treaty as precedent (this constraint affects them).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1960_eisenhower_antarctica_treaty_inspection, institutional, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
