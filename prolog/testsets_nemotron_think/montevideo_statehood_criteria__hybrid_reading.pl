% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_montevideo_statehood_criteria__hybrid_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: montevideo_statehood_criteria__hybrid_reading
 *   human_readable: Montevideo Criteria Plus Normative Legitimacy (Hybrid Reading)
 *   domain: international_law/political_philosophy/state_theory
 *
 * SUMMARY:
 *   The hybrid reading of the Montevideo Convention (1933) holds that
 *   statehood requires not only the four objective criteria (defined
 *   territory, permanent population, government, capacity to enter relations)
 *   but also normative legitimacy: democratic governance, human rights
 *   compliance, and non-aggression. This reading emerged prominently after
 *   the Cold War, as the liberal democratic community sought to condition
 *   recognition on political values. While presented as completing the
 *   Montevideo framework, the hybrid reading structurally empowers liberal
 *   democratic states to deny recognition to entities that meet objective
 *   criteria but fail normative tests — non-liberal secessionist movements,
 *   authoritarian breakaway regions, and states accused of aggression. The
 *   same normative criteria also provide legal cover for humanitarian
 *   intervention and regime change. The constraint presents as coordination
 *   (stable recognition order) but operates as extraction (normative
 *   gatekeeping).
 *
 * KEY AGENTS:
 *   - liberal_democratic_states: Primary agenda_setter/beneficiary (institutional/arbitrage) — sets recognition norms, collects legitimacy rents
 *   - non_liberal_secessionist_movements: Primary payer/victim (moderate/trapped) — meets objective criteria, denied recognition on normative grounds
 *   - western_recognition_community: Secondary beneficiary (institutional/arbitrage) — coordinates recognition policy, gains normative authority
 *   - authoritarian_aspirant_states: Secondary payer (powerful/constrained) — meets objective criteria, faces normative exclusion
 *   - populations_in_contested_territories: Excluded/payer (powerless/trapped) — bears costs of non-recognition (isolation, underdevelopment)
 *   - international_legal_community: Observer (analytical/analytical) — interprets and applies the hybrid criteria in ICJ opinions, arbitration
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__hybrid_reading, 0.68).
domain_priors:suppression_score(montevideo_statehood_criteria__hybrid_reading, 0.76).
domain_priors:theater_ratio(montevideo_statehood_criteria__hybrid_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(montevideo_statehood_criteria__hybrid_reading, "Montevideo Criteria Plus Normative Legitimacy (Hybrid Reading)").
narrative_ontology:topic_domain(montevideo_statehood_criteria__hybrid_reading, "international_law/political_philosophy/state_theory").

domain_priors:requires_active_enforcement(montevideo_statehood_criteria__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__hybrid_reading, 'a3e8d139-7591-4f05-86a4-76b21df68726').
narrative_ontology:cs_kernel_codification('a3e8d139-7591-4f05-86a4-76b21df68726', formalized).
narrative_ontology:cs_authority_grounding('a3e8d139-7591-4f05-86a4-76b21df68726', lineage).
narrative_ontology:cs_interpretation_layer_present('a3e8d139-7591-4f05-86a4-76b21df68726').
narrative_ontology:cs_reading_relation('a3e8d139-7591-4f05-86a4-76b21df68726', montevideo_statehood_criteria__declaratory_reading, influences).
narrative_ontology:cs_reading_relation('a3e8d139-7591-4f05-86a4-76b21df68726', montevideo_statehood_criteria__constitutive_reading, coexists_with).
narrative_ontology:cs_axiom('a3e8d139-7591-4f05-86a4-76b21df68726', foundational, normative_legitimacy_required_for_statehood).
narrative_ontology:cs_axiom_status(normative_legitimacy_required_for_statehood, holdable).
narrative_ontology:cs_axiom_grounding('a3e8d139-7591-4f05-86a4-76b21df68726', normative_legitimacy_required_for_statehood, conventional).
narrative_ontology:cs_axiom('a3e8d139-7591-4f05-86a4-76b21df68726', secondary, democratic_governance_as_statehood_criterion).
narrative_ontology:cs_axiom_status(democratic_governance_as_statehood_criterion, holdable).
narrative_ontology:cs_axiom_grounding('a3e8d139-7591-4f05-86a4-76b21df68726', democratic_governance_as_statehood_criterion, conventional).
narrative_ontology:cs_axiom('a3e8d139-7591-4f05-86a4-76b21df68726', secondary, human_rights_compliance_as_statehood_criterion).
narrative_ontology:cs_axiom_status(human_rights_compliance_as_statehood_criterion, holdable).
narrative_ontology:cs_axiom_grounding('a3e8d139-7591-4f05-86a4-76b21df68726', human_rights_compliance_as_statehood_criterion, conventional).
narrative_ontology:cs_axiom('a3e8d139-7591-4f05-86a4-76b21df68726', secondary, non_aggression_as_statehood_criterion).
narrative_ontology:cs_axiom_status(non_aggression_as_statehood_criterion, holdable).
narrative_ontology:cs_axiom_grounding('a3e8d139-7591-4f05-86a4-76b21df68726', non_aggression_as_statehood_criterion, conventional).
narrative_ontology:cs_reference_frame('a3e8d139-7591-4f05-86a4-76b21df68726', montevideo_plus_normative_legitimacy).
narrative_ontology:cs_drift_state('a3e8d139-7591-4f05-86a4-76b21df68726', contemporary_multipolar_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a3e8d139-7591-4f05-86a4-76b21df68726', '').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__hybrid_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, liberal_democratic_states).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, western_recognition_community).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, non_liberal_secessionist_movements).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, authoritarian_aspirant_states).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, populations_in_contested_territories).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__hybrid_reading, democratic_peace_theory).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__hybrid_reading, human_rights_universality).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__hybrid_reading, liberal_international_order_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Lead the western recognition community; set the normative agenda for statehood through UN Security Council resolutions, ICJ advisory opinions, and coordinated recognition policies. They collect legitimacy rents by controlling the gateway to full international personality. Their exit is arbitrage-grade: they can recognize or withhold recognition based on political calculation without existential cost to their own statehood.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, liberal_democratic_states, agenda_setter,
    institutional, generational, arbitrage, global).

% The broader coalition of states that coordinate recognition decisions along normative lines (EU, NATO allies, like-minded partners). They benefit from a stable, values-based recognition order that reinforces their collective legitimacy. They share the agenda-setter's arbitrage exit but with less individual agenda-setting power.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, western_recognition_community, beneficiary,
    institutional, generational, arbitrage, global).

% Entities that control territory, population, and government (meeting objective Montevideo criteria) but are denied recognition because they lack democratic governance, human rights compliance, or are accused of aggression. Examples: Somaliland, Transnistria, Nagorno-Karabakh (pre-2023), Turkish Republic of Northern Cyprus. They are trapped: no exit from the recognition order, no path to statehood without normative conformity, which may be politically impossible or undesirable.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, non_liberal_secessionist_movements, payer,
    moderate, biographical, trapped, regional).

% States or quasi-states with effective control and objective criteria compliance but authoritarian governance (e.g., Taliban Afghanistan, pre-2011 Libya, Ba'athist Syria). They have power to resist isolation and build alternative recognition blocs (Shanghai Cooperation Organization, BRICS), but their constrained exit means they pay high costs: sanctions, diplomatic isolation, exclusion from international financial institutions.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, authoritarian_aspirant_states, payer,
    powerful, biographical, constrained, regional).

% Civilian populations living in entities denied recognition under the hybrid criteria. They bear the diffuse costs of non-recognition: underdevelopment, inability to access international aid directly, legal limbo, passport non-recognition, economic isolation. They have no voice in the recognition process and no exit — their fate is determined by the normative judgments of distant powers.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, populations_in_contested_territories, excluded,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(montevideo_statehood_criteria__hybrid_reading, populations_in_contested_territories, payer).

% International Court of Justice, international arbitral tribunals, UN legal bodies, and international law scholars who interpret and apply the hybrid criteria. Their authority derives from the liberal international order whose normative framework they adjudicate. They observe the constraint's operation but their analytical seat is not neutral — it is constituted by the same order that produces the hybrid reading.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, international_legal_community, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a predictable, values-based framework for recognizing new states, replacing the Cold War's ideological bloc recognition with a (putatively) universal normative standard. Solves the problem of 'who gets a seat at the table' by adding qualitative criteria to the quantitative Montevideo baseline.
% TRANSFER_FUNCTION: Moves the legal status of statehood (and its attendant rights: sovereignty, treaty capacity, UN membership, immunity) from entities that meet objective criteria but fail normative tests, to the liberal democratic community that controls the normative gateway. The transfer is not monetary but jurisdictional and ontological: the hybrid reading transfers the power to confer existence as a legal person.
% ABSENT_VOICES: The populations of aspirant states denied recognition (excluded stakeholders) would object to the normative criteria as externally imposed and politically selective. Rising non-liberal powers (China, Russia, and their alignment networks) would object to the universalist claim of liberal normative criteria. Both are structurally excluded from the western recognition community that authors the hybrid reading.
% DISAPPEARANCE_RATIONALE: If the hybrid reading vanished overnight, recognition would revert to the declaratory/constitutive baseline: entities meeting objective criteria would gain statehood regardless of governance type. Dozens of currently unrecognized entities (Somaliland, Taiwan, Palestine, Transnistria, etc.) would face radically different diplomatic landscapes. The liberal democratic community would lose its primary normative tool for recognition gatekeeping. Humanitarian intervention would lose its most potent legal cover.
% FOUNDING_PROBLEM: After the Cold War, the liberal democratic community needed a replacement for ideological bloc recognition that would prevent 'undesirable' entities (war-torn secessionist regions, authoritarian breakaways, ethnic nationalist projects) from gaining full international personality while maintaining a universalist legal facade.
% FOUNDING_PROBLEM_CORROBORATION: Liberal democratic states and western international lawyers attest the problem remains live (ongoing secessionist conflicts, authoritarian resilience, need for normative guardrails). Critics from the Global South, non-liberal powers, and critical legal scholars attest the founding problem is substantially solved or was a pretext: the normative criteria now function to preserve Western control over the international order. No neutral third party corroborates either side — the corroboration split IS the contestation.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(montevideo_statehood_criteria__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(montevideo_statehood_criteria__hybrid_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(montevideo_statehood_criteria__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(montevideo_statehood_criteria__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(montevideo_statehood_criteria__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is substantial: the normative overlay extracts recognition from entities meeting objective criteria, converting political non-conformity into legal non-existence. Suppression (0.76) is high: the constraint persists through active diplomatic isolation, exclusion from international organizations, and the threat of non-recognition — enforced by the liberal democratic bloc. Theater ratio (0.45) is moderate: the coordination function (predictable recognition rules) is real but increasingly performative as double standards proliferate (Kosovo vs. South Ossetia, Taiwan vs. Palestine). Accessibility collapse (0.58) reflects that alternatives (de facto statehood, limited recognition) exist but carry severe costs. Resistance (0.72) is high: non-liberal powers (Russia, China) and Global South states contest the normative criteria as neo-colonial, creating competing recognition blocs. The measurement series shows steady extraction accumulation and suppression intensification post-1990, with theater rising as the gap between normative rhetoric and selective application widens.
 *
 * PERSPECTIVAL GAP:
 *   From the liberal democratic seat, the hybrid reading is a rope: it solves the coordination problem of 'which entities deserve full international personality' by adding normative quality control to the objective baseline. From the non-liberal secessionist seat, it is a snare: the coordination story is cover for denying recognition to politically inconvenient entities. The engine computes this divergence from the structural data — the agenda_setter role with arbitrage exit versus the payer role with trapped exit. The declaratory_reading sibling would compute as mountain from the objective-criteria-only seat; the constitutive_reading would compute as rope from the recognition-community seat. The hybrid reading forces all seats through the normative filter, creating the extraction gradient.
 *
 * DIRECTIONALITY LOGIC:
 *   Liberal democratic states and the western recognition community are structural beneficiaries (d near 0.0): they set the normative agenda, collect legitimacy rents from controlling the recognition gateway, and have arbitrage-grade exit (they can recognize or not without existential cost). Non-liberal secessionist movements and populations in contested territories are full targets (d near 1.0): they meet objective criteria but are trapped by the normative overlay, with no exit from the recognition order. Authoritarian aspirant states are constrained targets (d ~0.7): powerful enough to resist but constrained by the normative framework's institutionalization. The international legal community sits near analytical (d ~0.5): it interprets the criteria but its authority derives from the same liberal order.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-Cold War need for a values-based recognition order to replace ideological bloc recognition) is contested: liberal states say it remains live; critics say the normative criteria have become a tool for maintaining Western hegemony. The mandate has not atrophied — it has intensified — but the coordination function has degraded into selective extraction. The constraint is not a piton (theater is not dominant); it is an active tangled_rope where the coordination function is real but the extraction function is structurally asymmetric and growing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the hybrid reading a genuine evolution of the Montevideo kernel or a constructed overlay that benefits liberal democratic states?',
    'Trace the genealogical emergence of normative criteria in state practice and ICJ opinions post-1990; compare with drafting history of Montevideo Convention.',
    'If constructed overlay, the constraint is a false summit masking extraction; if genuine evolution, the coordination function is structurally real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether normative legitimacy criteria are immanent to the kernel or externally imposed').

omega_variable(
    beneficiary_structure_ambiguity,
    'Do liberal democratic states benefit from the coordination function (stable recognition order) or primarily from the extraction function (denying recognition to rivals)?',
    'Analyze recognition patterns: when liberal states deny recognition to non-liberal entities meeting objective criteria, is the alternative recognition order destabilized or stabilized?',
    'If primarily extraction, the constraint is snare-flavored; if genuine coordination, tangled_rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_structure_ambiguity, empirical, 'Disentangling coordination benefit from extraction benefit for the agenda-setting seat').

omega_variable(
    suppression_mechanism_statehood,
    'Is the suppression of non-liberal aspirant states structural (diplomatic isolation, legal exclusion) or internalized (self-censorship of secessionist claims)?',
    'Track post-exit trajectories: when entities abandon secessionist claims under normative pressure, does the suppression persist in their political imagination?',
    'If internalized, effective suppression exceeds structural measure; the constraint operates through identity formation, not just diplomatic barriers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_statehood, empirical, 'Structural vs. internalized suppression in statehood denial').

omega_variable(
    persistence_of_hybrid_reading,
    'Will the hybrid reading persist as the dominant recognition framework, or will it fracture under pressure from rising non-liberal powers?',
    'Monitor UNGA voting patterns, regional recognition blocs, and ICJ advisory opinions on secession/statehood over the next decade.',
    'If it fractures, the constraint is a scaffold with an implicit sunset; if it persists, it is a stable tangled_rope or hardening snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(persistence_of_hybrid_reading, preference, 'Long-term viability of the normative legitimacy overlay').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__hybrid_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(msc_hybrid_tr_t1990, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(msc_hybrid_tr_t1995, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 1995, 0.3).
narrative_ontology:measurement(msc_hybrid_tr_t2000, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(msc_hybrid_tr_t2005, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 2005, 0.38).
narrative_ontology:measurement(msc_hybrid_tr_t2010, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 2010, 0.41).
narrative_ontology:measurement(msc_hybrid_tr_t2015, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 2015, 0.43).
narrative_ontology:measurement(msc_hybrid_tr_t2020, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 2020, 0.44).
narrative_ontology:measurement(msc_hybrid_tr_t2024, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(msc_hybrid_be_t1990, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 1990, 0.35).
narrative_ontology:measurement(msc_hybrid_be_t1995, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 1995, 0.42).
narrative_ontology:measurement(msc_hybrid_be_t2000, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 2000, 0.52).
narrative_ontology:measurement(msc_hybrid_be_t2005, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 2005, 0.58).
narrative_ontology:measurement(msc_hybrid_be_t2010, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(msc_hybrid_be_t2015, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 2015, 0.65).
narrative_ontology:measurement(msc_hybrid_be_t2020, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 2020, 0.67).
narrative_ontology:measurement(msc_hybrid_be_t2024, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(msc_hybrid_su_t1990, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(msc_hybrid_su_t1995, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 1995, 0.6).
narrative_ontology:measurement(msc_hybrid_su_t2000, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(msc_hybrid_su_t2005, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement(msc_hybrid_su_t2010, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 2010, 0.71).
narrative_ontology:measurement(msc_hybrid_su_t2015, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 2015, 0.73).
narrative_ontology:measurement(msc_hybrid_su_t2020, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 2020, 0.75).
narrative_ontology:measurement(msc_hybrid_su_t2024, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 2024, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(montevideo_statehood_criteria__hybrid_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(montevideo_statehood_criteria__hybrid_reading, 0.12).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, montevideo_statehood_criteria__declaratory_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, montevideo_statehood_criteria__constitutive_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, humanitarian_intervention_legitimacy).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, secessionist_recognition_norms).

% DUAL FORMULATION NOTE:
% The Montevideo kernel decomposes into three constraint stories: declaratory (objective criteria as mountain), constitutive (recognition as rope), and hybrid (normative overlay as tangled_rope). The hybrid reading structurally influences both siblings by raising the legitimacy threshold for recognition, which pressures the declaratory reading's purity and the constitutive reading's community. The extraction in the hybrid reading is the price the kernel pays for its normative enrichment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(montevideo_statehood_criteria__hybrid_reading, institutional, 0.1).
constraint_indexing:directionality_override(montevideo_statehood_criteria__hybrid_reading, moderate, 0.85).
constraint_indexing:directionality_override(montevideo_statehood_criteria__hybrid_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
