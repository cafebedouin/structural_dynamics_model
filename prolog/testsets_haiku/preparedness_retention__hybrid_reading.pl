% ============================================================================
% CONSTRAINT STORY: preparedness_retention__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_retention__hybrid_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: preparedness_retention__hybrid_reading
 *   human_readable: Stratified Preparedness: Technical Competence vs. Ceremonial Memory
 *   domain: governance/disaster_preparedness
 *
 * SUMMARY:
 *   The Netherlands built its post-war preparedness system by concentrating
 *   hydrological and operational expertise in Rijkswaterstaat and regional
 *   water boards while distributing ceremonial participation in drills and
 *   inspections to municipalities and communities. This reading
 *   (hybrid_reading) asserts that the system is genuinely dual-track: the
 *   institutional core maintains live competence while the periphery performs
 *   preparation. This differs from the husk_reading (which claims even the
 *   institutions are ceremonial) and the competence_reading (which claims
 *   drills sustain distributed competence). The three readings instantiate
 *   different ε-values and occupy different structural positions. This story
 *   generates the hybrid_reading only: stratification as a durable division
 *   of labor with distinct beneficiaries and victims.
 *
 * KEY AGENTS:
 *   - Rijkswaterstaat: core technical institution (water management engineers, hydrologists) — maintains live operational competence
 *   - Water boards: regional coordinating bodies — benefit from delegated competence, pay by accepting limited input into strategy
 *   - Municipal emergency managers: local administrators — perform preparedness protocol, identity-locked to certification regime
 *   - Community residents: peripheral participants — perform drills, lack access to decision-making or technical knowledge
 *   - Specialized water technicians: career beneficiaries — protected professional domain within institutional framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_retention__hybrid_reading, 0.62).
domain_priors:suppression_score(preparedness_retention__hybrid_reading, 0.41).
domain_priors:theater_ratio(preparedness_retention__hybrid_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_retention__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_retention__hybrid_reading, "Stratified Preparedness: Technical Competence vs. Ceremonial Memory").
narrative_ontology:topic_domain(preparedness_retention__hybrid_reading, "governance/disaster_preparedness").

domain_priors:requires_active_enforcement(preparedness_retention__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_retention__hybrid_reading, 'e9d69e96-6cfb-497d-8a9f-11357c675f13').
narrative_ontology:cs_kernel_codification('e9d69e96-6cfb-497d-8a9f-11357c675f13', formalized).
narrative_ontology:cs_authority_grounding('e9d69e96-6cfb-497d-8a9f-11357c675f13', lineage).
narrative_ontology:cs_interpretation_layer_present('e9d69e96-6cfb-497d-8a9f-11357c675f13').
narrative_ontology:cs_reading_relation('e9d69e96-6cfb-497d-8a9f-11357c675f13', preparedness_retention__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('e9d69e96-6cfb-497d-8a9f-11357c675f13', preparedness_retention__husk_reading, influences).
narrative_ontology:cs_axiom('e9d69e96-6cfb-497d-8a9f-11357c675f13', foundational, preparedness_is_stratified).
narrative_ontology:cs_axiom_status(preparedness_is_stratified, holdable).
narrative_ontology:cs_axiom_grounding('e9d69e96-6cfb-497d-8a9f-11357c675f13', preparedness_is_stratified, empirically_contingent).
narrative_ontology:cs_axiom('e9d69e96-6cfb-497d-8a9f-11357c675f13', foundational, technical_competence_concentrable).
narrative_ontology:cs_axiom_status(technical_competence_concentrable, holdable).
narrative_ontology:cs_axiom_grounding('e9d69e96-6cfb-497d-8a9f-11357c675f13', technical_competence_concentrable, empirically_contingent).
narrative_ontology:cs_reference_frame('e9d69e96-6cfb-497d-8a9f-11357c675f13', specialized_hydrological_competence_concentration).
narrative_ontology:cs_drift_state('e9d69e96-6cfb-497d-8a9f-11357c675f13', contemporary_2025, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e9d69e96-6cfb-497d-8a9f-11357c675f13', '').
narrative_ontology:cs_kernel_id(preparedness_retention__hybrid_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, water_management_institutions).
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, institutional_continuity).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, distributed_public_resilience).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, peripheral_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, water_boards).
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, specialized_water_technicians).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, water_boards).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, municipal_emergency_managers).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, community_leaders_and_residents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Dutch national water authority maintains a core of specialized hydrologists, engineers, and operational staff with continuous, hands-on competence in water management. They design and conduct drills for municipal and regional water boards, set inspection standards, and retain institutional memory through staffing continuity and technical training. They frame this dual-track approach as necessary specialization: local communities cannot and should not retain the deep technical knowledge needed for major flood defense, so they concentrate it in Rijkswaterstaat and trust the institution to serve as guarantor.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, rijkswaterstaat, agenda_setter,
    institutional, generational, analytical, national).

% Elected regional bodies responsible for water management. They benefit from having Rijkswaterstaat maintain technical competence and provide ready-made drills and inspection protocols—this lightens their administrative burden and gives them plausible assurance of preparedness without needing to develop deep expertise in-house. They also pay by delegating authority over preparedness standards and accepting ceremonial participation in drills rather than co-designing them; their input into preparedness design is constrained to choosing among presets.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, water_boards, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__hybrid_reading, water_boards, payer).

% Participate in standardized drills, attend training, and execute inspection checklists provided by Rijkswaterstaat. Their professional identity is bound to the drill cycle and inspection schedule—they are competent at following protocol but lack the authority or capacity to redesign preparedness strategy. Their exit is identity-locked: stepping outside the certification regime means losing standing as a 'prepared' municipality, even if they developed autonomous competence.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, municipal_emergency_managers, payer,
    moderate, biographical, identity_locked, local).

% Embedded in Rijkswaterstaat and specialized water boards. They retain live competence through continuous hands-on work with flood defenses, modeling systems, and real-time decision-making during crisis. Their career advancement depends on remaining within the institutional framework that values technical expertise. They benefit from the stratified system because it protects their professional domain from dilution and ensures their knowledge remains essential and valued.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, specialized_water_technicians, beneficiary,
    organized, biographical, constrained, national).

% Participate in community drill events, receive preparedness messaging, and are told they are prepared. They have no role in designing preparedness strategy and cannot exit or challenge the prescribed approach. Their understanding of actual flood defense is limited to the ceremonial aspects they witness; they have no access to the technical knowledge or decision-making that determines whether their community survives a major event. Their exit is trapped: they cannot leave the jurisdiction, and opting out of drills carries social and administrative penalties.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, community_leaders_and_residents, payer,
    powerless, biographical, trapped, local).

% Bottom-up community networks, local flood defense knowledge, and distributed volunteer expertise are structurally excluded from official preparedness planning. Communities that might develop autonomous competence are prevented from doing so by the requirement to follow standardized protocols and by the implicit message that 'real' preparedness is something only specialized institutions can provide. Their exclusion is maintained by funding and certification mechanisms that channel all preparedness resources through the institutional hierarchy.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, rival_local_preparedness_models, excluded,
    powerless, biographical, trapped, local).

% Parliament and provincial oversight bodies receive preparedness reports and inspect drills. They can assess whether the formal machinery appears sound but lack the technical depth to verify whether the observed ceremonies translate to actual competence. Their oversight is structurally limited to reviewing compliance with the stratified system's own metrics, not questioning the system's architecture itself.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, political_accountability_bodies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_retention__hybrid_reading, rijkswaterstaat).
narrative_ontology:fixing_cost_class(preparedness_retention__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates scarce technical expertise (hydrology, advanced modeling, real-time flood defense operations) in specialized institutions that can maintain continuity and depth, avoiding the diffusion of competence across hundreds of local actors who cannot sustain it. Creates a single authoritative source for preparedness standards and drill design, reducing transaction costs of coordination across fragmented local governments.
% TRANSFER_FUNCTION: Moves authority over preparedness design and execution from local communities to centralized institutions; moves the appearance of preparedness (compliance with drills, possession of certificates) from actual autonomous capacity to ceremonial participation. Distributes the risk of single-point-of-failure onto the peripheral communities while concentrating the expertise and reputation benefits in the institutional core.
% ABSENT_VOICES: Bottom-up community networks and local leaders who might develop autonomous flood defense knowledge are excluded from preparedness planning. They would argue for distributed resilience and community capacity-building but are framed out of the conversation by the insistence that 'real' preparedness requires specialized expertise. Independent water management cultures and alternative institutional models for flood defense (e.g., community-based early warning systems developed in other countries) are not heard.
% DISAPPEARANCE_RATIONALE: If the stratified preparedness system disappeared, Rijkswaterstaat and water boards would lose the institutional delegation and the appearance of public preparedness. Communities would need to either develop local competence (a multi-year reorganization) or negotiate alternative institutional arrangements for flood defense. The Dutch flood defense system would face a legitimacy and coordination crisis, though the underlying technical infrastructure would persist.
% FOUNDING_PROBLEM: Post-WWII Dutch water management faced a choice: rebuild with distributed local knowledge that was fragmented and inconsistently maintained, or concentrate expertise in a national authority (Rijkswaterstaat) that could maintain standards and prevent deadly inconsistencies in dike maintenance and water control. The founding problem was real: catastrophic flood risk required reliable, coordinated response across hundreds of municipalities with no mechanism to enforce consistency.
% FOUNDING_PROBLEM_CORROBORATION: Rijkswaterstaat and water board officials attest the founding problem remains live—coordination across fragmented local authorities is still necessary for safety. Civil society organizations, independent hydrologists, and researchers attest the founding problem was solved decades ago and the institutional structure now persists as a rent-extraction and authority-protection mechanism; parliamentary inquiries after near-miss flood events (1995, 2021) have documented that much public 'preparedness' is ceremonial and that actual competence is confined to the institutional core. International comparative research (comparing Dutch institutional models to community-based preparedness in Bangladesh and the Philippines) shows distributed competence is technically feasible but politically excluded by the Dutch framework.
narrative_ontology:disappearance_verdict(preparedness_retention__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_retention__hybrid_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_retention__hybrid_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(preparedness_retention__hybrid_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_retention__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_retention__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_retention__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.35 (1960) to 0.62 (2025) because the system's initial justification (solving coordination crisis) weakens while its extraction mechanism (monopoly over preparedness authority) hardens. Theater ratio climbs even more sharply (0.20 → 0.68) because drills and inspections increasingly satisfy compliance without verifying competence—the machinery is performing its own function rather than assessing preparedness. Suppression requirement rises gradually (0.15 → 0.41) because maintaining the stratified system requires active exclusion of bottom-up preparedness alternatives and identity-locking of peripheral actors. Accessibility_collapse and resistance are measured at four levels: at the structural level, the system is highly consolidated and resistant organizations have weakened over time; at the individual level, communities retain moderate resistance but accessibility to alternatives has tightened. The grid captures the differentiation: suppression is imposed differentially at each level, with the strongest suppression at the individual and organizational levels (where exit attempts are most likely) and the weakest at the structural level (where the system is legitimized as natural).
 *
 * PERSPECTIVAL GAP:
 *   From Rijkswaterstaat's position, the constraint is genuine coordination—they are protecting a real functional core against fragmentation. From the municipal emergency manager's position, it is enforced compliance with alien protocols (identity-locked). From the community level, it is ceremonial participation in a system they do not understand and cannot influence (trapped). The engine should compute these as different types in different seats: the institutional agenda-setter may compute as rope (they coordinate and invest), while the powerless and identity-locked seats compute as tangled_rope or snare (they are coordinated by and pay through the same structure). The dual-track claim is precisely this: the same constraint operates as genuine specialization at the center and as extraction at the periphery.
 *
 * DIRECTIONALITY LOGIC:
 *   Rijkswaterstaat and specialized water technicians are net beneficiaries (d toward 0.0) — they retain professional autonomy, control preparedness standards, and protect their expertise from dilution. Water boards are partially beneficiary, partially payer (d near 0.5) — they gain coordination and reduced burden but lose input authority. Municipal emergency managers are targets (d toward 0.8) — they follow protocol without understanding the underlying decisions and are locked into the certification regime by professional identity. Communities and residents are the heaviest targets (d near 0.9) — they are trapped, powerless, and excluded from decision-making while bearing the consequences of single-point-of-failure concentration. The directionality gradient runs from institutional core (beneficiary) through organizational middle (intermediate) to individual periphery (target). No overrides needed: the structural derivation from power + exit + beneficiary/victim declarations captures this cleanly.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading claims Mandatrophy is LIVE as a contested question. The founding problem (post-WWII coordination crisis) was real and arguably addressed by the 1960s. By 2025, the constraint persists but its original mandate is dead—communities no longer need Rijkswaterstaat to prevent them from developing autonomous competence because they have been systematically excluded from doing so. The system now extracts authority rent (monopoly over preparedness certification) while performing coordination that could be achieved through less hierarchical means. The theater_ratio and extraction rise in parallel over the interval, indicating that the function (coordination) is being displaced by the form (compliance machinery). This is classic Mandatrophy drift: the constraint persists past the death of its founding justification, sustained by the beneficiaries' interest in retaining authority. The mismatch (founding_problem_status=dead, but the system operates as if the problem is live) is where Mandatrophy resolution fires.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_competence_vs_ceremonial_performance,
    'Is the measured technical competence in Rijkswaterstaat''s core actually live and exercised, or is it itself ceremonial—retained in memory and systems but divorced from actual operational decision-making?',
    'Post-crisis post-mortem analysis of decision-making during actual flood events; examination of how often institutional protocols are deployed vs. improvised; comparison of institutional recommendations vs. outcomes.',
    'If institutional competence itself is ceremonial, the constraint collapses to the husk_reading and both extractiveness and theater_ratio should reclassify upward. If competence is live at the center, the hybrid reading holds and the stratification is the accurate description.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_competence_vs_ceremonial_performance, empirical, 'Whether the institutional core maintains live competence or performs competence').

omega_variable(
    distributed_competence_suppressibility,
    'Could communities develop autonomous preparedness competence if the institutional suppression were removed, or has the stratified system created genuine path-dependency where distributed knowledge is now impossible to reconstruct?',
    'Comparison with regions that use distributed preparedness models (e.g., community-based flood defense in Bangladesh, Philippines); measurement of knowledge-retention rates in communities that were excluded from institutional preparedness; longitudinal data on whether retired Rijkswaterstaat staff who work with communities can transfer competence.',
    'If distributed competence is recoverable, the suppression is structural and the system is extractive (snare-like). If distributed competence is genuinely irreversible, the system may be closer to a piton—it persists because the alternative is costly and uncertain, not because of active enforcement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(distributed_competence_suppressibility, empirical, 'Whether the suppression of distributed preparedness competence is reversible').

omega_variable(
    single_point_of_failure_risk,
    'How concentrated is actual decision-making authority in Rijkswaterstaat? Is the system robust against the failure of key personnel or institutions, or does it depend on irreplaceable individuals?',
    'Organizational audit of Rijkswaterstaat; documentation of succession planning and knowledge transfer; examination of how often key decisions depend on specific individuals; measurement of institutional memory loss after retirements.',
    'If single points of failure exist, the risk concentration is part of what the constraint extracts from the periphery. The periphery pays (in terms of lost resilience) so the center can specialize. This would amplify the victim reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(single_point_of_failure_risk, empirical, 'Whether institutional concentration creates irreplaceable single points of failure').

omega_variable(
    reading_contest_axes,
    'Which reading is actually instantiated depends on three empirical axes that the three sibling readings weight differently. What are the true weights?',
    'The competence_reading weights distributed drill participation heavily—if drills meaningfully preserve knowledge across society, it scores higher. The husk_reading weights institutional authenticity—if even the core institutions are performing ritual, the husk reading wins. The hybrid reading (this one) weights institutional competence + peripheral ceremonialism. Resolving requires measurement of: (a) whether institutional core competence is live, (b) whether peripheral participation preserves knowledge, and (c) whether the gap between (a) and (b) is structural or incidental.',
    'Different readings lead to different policy recommendations: competence_reading suggests expanding drills; husk_reading suggests institutional overhaul; hybrid_reading suggests decentralization. The constraint''s classification depends on which reading is true.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_axes, empirical, 'Which of the three sibling readings correctly characterizes preparedness').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__hybrid_reading, 1960, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t1960, preparedness_retention__hybrid_reading, theater_ratio, 1960, 0.2).
narrative_ontology:measurement_basis(prep_tr_t1960, projected).
narrative_ontology:measurement(prep_tr_t1980, preparedness_retention__hybrid_reading, theater_ratio, 1980, 0.3).
narrative_ontology:measurement_basis(prep_tr_t1980, observed).
narrative_ontology:measurement(prep_tr_t2000, preparedness_retention__hybrid_reading, theater_ratio, 2000, 0.52).
narrative_ontology:measurement_basis(prep_tr_t2000, observed).
narrative_ontology:measurement(prep_tr_t2013, preparedness_retention__hybrid_reading, theater_ratio, 2013, 0.65).
narrative_ontology:measurement_basis(prep_tr_t2013, observed).
narrative_ontology:measurement(prep_tr_t2025, preparedness_retention__hybrid_reading, theater_ratio, 2025, 0.68).
narrative_ontology:measurement_basis(prep_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(prep_be_t1960, preparedness_retention__hybrid_reading, base_extractiveness, 1960, 0.35).
narrative_ontology:measurement_basis(prep_be_t1960, projected).
narrative_ontology:measurement(prep_be_t1980, preparedness_retention__hybrid_reading, base_extractiveness, 1980, 0.42).
narrative_ontology:measurement_basis(prep_be_t1980, observed).
narrative_ontology:measurement(prep_be_t2000, preparedness_retention__hybrid_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement_basis(prep_be_t2000, observed).
narrative_ontology:measurement(prep_be_t2013, preparedness_retention__hybrid_reading, base_extractiveness, 2013, 0.6).
narrative_ontology:measurement_basis(prep_be_t2013, observed).
narrative_ontology:measurement(prep_be_t2025, preparedness_retention__hybrid_reading, base_extractiveness, 2025, 0.62).
narrative_ontology:measurement_basis(prep_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t1960, preparedness_retention__hybrid_reading, suppression_requirement, 1960, 0.15).
narrative_ontology:measurement_basis(prep_su_t1960, projected).
narrative_ontology:measurement(prep_su_t1980, preparedness_retention__hybrid_reading, suppression_requirement, 1980, 0.22).
narrative_ontology:measurement_basis(prep_su_t1980, observed).
narrative_ontology:measurement(prep_su_t2000, preparedness_retention__hybrid_reading, suppression_requirement, 2000, 0.32).
narrative_ontology:measurement_basis(prep_su_t2000, observed).
narrative_ontology:measurement(prep_su_t2013, preparedness_retention__hybrid_reading, suppression_requirement, 2013, 0.38).
narrative_ontology:measurement_basis(prep_su_t2013, observed).
narrative_ontology:measurement(prep_su_t2025, preparedness_retention__hybrid_reading, suppression_requirement, 2025, 0.41).
narrative_ontology:measurement_basis(prep_su_t2025, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1960, tn=2025
narrative_ontology:measurement(prep_grid_01, preparedness_retention__hybrid_reading, accessibility_collapse(class), 1960, 0.35).
narrative_ontology:measurement(prep_grid_02, preparedness_retention__hybrid_reading, accessibility_collapse(class), 2025, 0.48).
narrative_ontology:measurement(prep_grid_03, preparedness_retention__hybrid_reading, accessibility_collapse(individual), 1960, 0.28).
narrative_ontology:measurement(prep_grid_04, preparedness_retention__hybrid_reading, accessibility_collapse(individual), 2025, 0.38).
narrative_ontology:measurement(prep_grid_05, preparedness_retention__hybrid_reading, accessibility_collapse(organizational), 1960, 0.52).
narrative_ontology:measurement(prep_grid_06, preparedness_retention__hybrid_reading, accessibility_collapse(organizational), 2025, 0.61).
narrative_ontology:measurement(prep_grid_07, preparedness_retention__hybrid_reading, accessibility_collapse(structural), 1960, 0.65).
narrative_ontology:measurement(prep_grid_08, preparedness_retention__hybrid_reading, accessibility_collapse(structural), 2025, 0.72).
narrative_ontology:measurement(prep_grid_09, preparedness_retention__hybrid_reading, resistance(class), 1960, 0.48).
narrative_ontology:measurement(prep_grid_10, preparedness_retention__hybrid_reading, resistance(class), 2025, 0.32).
narrative_ontology:measurement(prep_grid_11, preparedness_retention__hybrid_reading, resistance(individual), 1960, 0.42).
narrative_ontology:measurement(prep_grid_12, preparedness_retention__hybrid_reading, resistance(individual), 2025, 0.25).
narrative_ontology:measurement(prep_grid_13, preparedness_retention__hybrid_reading, resistance(organizational), 1960, 0.55).
narrative_ontology:measurement(prep_grid_14, preparedness_retention__hybrid_reading, resistance(organizational), 2025, 0.38).
narrative_ontology:measurement(prep_grid_15, preparedness_retention__hybrid_reading, resistance(structural), 1960, 0.62).
narrative_ontology:measurement(prep_grid_16, preparedness_retention__hybrid_reading, resistance(structural), 2025, 0.45).
narrative_ontology:measurement(prep_grid_17, preparedness_retention__hybrid_reading, stakes_inflation(class), 1960, 0.25).
narrative_ontology:measurement(prep_grid_18, preparedness_retention__hybrid_reading, stakes_inflation(class), 2025, 0.35).
narrative_ontology:measurement(prep_grid_19, preparedness_retention__hybrid_reading, stakes_inflation(individual), 1960, 0.18).
narrative_ontology:measurement(prep_grid_20, preparedness_retention__hybrid_reading, stakes_inflation(individual), 2025, 0.28).
narrative_ontology:measurement(prep_grid_21, preparedness_retention__hybrid_reading, stakes_inflation(organizational), 1960, 0.38).
narrative_ontology:measurement(prep_grid_22, preparedness_retention__hybrid_reading, stakes_inflation(organizational), 2025, 0.48).
narrative_ontology:measurement(prep_grid_23, preparedness_retention__hybrid_reading, stakes_inflation(structural), 1960, 0.55).
narrative_ontology:measurement(prep_grid_24, preparedness_retention__hybrid_reading, stakes_inflation(structural), 2025, 0.62).
narrative_ontology:measurement(prep_grid_25, preparedness_retention__hybrid_reading, suppression(class), 1960, 0.1).
narrative_ontology:measurement(prep_grid_26, preparedness_retention__hybrid_reading, suppression(class), 2025, 0.38).
narrative_ontology:measurement(prep_grid_27, preparedness_retention__hybrid_reading, suppression(individual), 1960, 0.08).
narrative_ontology:measurement(prep_grid_28, preparedness_retention__hybrid_reading, suppression(individual), 2025, 0.35).
narrative_ontology:measurement(prep_grid_29, preparedness_retention__hybrid_reading, suppression(organizational), 1960, 0.18).
narrative_ontology:measurement(prep_grid_30, preparedness_retention__hybrid_reading, suppression(organizational), 2025, 0.42).
narrative_ontology:measurement(prep_grid_31, preparedness_retention__hybrid_reading, suppression(structural), 1960, 0.12).
narrative_ontology:measurement(prep_grid_32, preparedness_retention__hybrid_reading, suppression(structural), 2025, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_retention__hybrid_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_retention__hybrid_reading, 0.25).
narrative_ontology:affects_constraint(preparedness_retention__hybrid_reading, preparedness_retention__competence_reading).
narrative_ontology:affects_constraint(preparedness_retention__hybrid_reading, preparedness_retention__husk_reading).

% DUAL FORMULATION NOTE:
% The preparedness_retention kernel has three readings instantiated as separate constraints. (1) competence_reading: preparedness is live knowledge preserved through exercises across society, ε ≈ 0.20, claimed_type = rope. (2) husk_reading: preparedness is entirely ceremonial, ε ≈ 0.75, claimed_type = piton. (3) hybrid_reading (this one): preparedness is stratified—institutional competence + peripheral ceremony, ε ≈ 0.62, claimed_type = tangled_rope. Each reading has different beneficiaries (competence: distributed society; husk: institutional administrators; hybrid: institutional core). The three readings coexist as live contested positions in Dutch governance. Each feeds into different policy directions. The constraint family link allows the corpus to track which reading dominates over time and under what conditions actors shift between readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
