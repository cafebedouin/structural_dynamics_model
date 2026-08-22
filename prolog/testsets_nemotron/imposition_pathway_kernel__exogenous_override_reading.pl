% ============================================================================
% CONSTRAINT STORY: imposition_pathway_kernel__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_pathway_kernel__exogenous_override_reading, []).

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
 *   constraint_id: imposition_pathway_kernel__exogenous_override_reading
 *   human_readable: State Imposition Pathway for Commitment Displacement (Exogenous Override Reading)
 *   domain: historical_sociology/state_formation/commitment_systems
 *
 * SUMMARY:
 *   This constraint models the exogenous override reading of the imposition
 *   pathway kernel: the claim that state capacity enables commitment
 *   displacement without any pre-existing fringe adoption pathway. The Meiji
 *   Restoration's calendar reform (1873) and dress codes (1871-1872) are the
 *   canonical cases — imperial decrees imposed Gregorian calendar and Western
 *   dress on a population with no prior fringe adoption of either. Compliance
 *   was coerced through enforcement machinery (police, school regulations,
 *   employment requirements), not emergent from below. The M-set framework's
 *   climb cells cannot capture this; an override cell is structurally
 *   required. The constraint is a tangled rope: it coordinates a new
 *   temporal/sartorial order (genuine coordination function for a modernizing
 *   state) while extracting compliance costs from traditional elites, rural
 *   populations, and religious authorities through active enforcement.
 *   Extraction and suppression peak at the decree moment and decay as the new
 *   commitments naturalize; theater rises as enforcement shifts from coercion
 *   to performative maintenance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_pathway_kernel__exogenous_override_reading, 0.72).
domain_priors:suppression_score(imposition_pathway_kernel__exogenous_override_reading, 0.78).
domain_priors:theater_ratio(imposition_pathway_kernel__exogenous_override_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_pathway_kernel__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(imposition_pathway_kernel__exogenous_override_reading, "State Imposition Pathway for Commitment Displacement (Exogenous Override Reading)").
narrative_ontology:topic_domain(imposition_pathway_kernel__exogenous_override_reading, "historical_sociology/state_formation/commitment_systems").

domain_priors:requires_active_enforcement(imposition_pathway_kernel__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_pathway_kernel__exogenous_override_reading, 'ef6760ae-09e7-4682-a72d-f4000109e3af').
narrative_ontology:cs_kernel_codification('ef6760ae-09e7-4682-a72d-f4000109e3af', distributed).
narrative_ontology:cs_authority_grounding('ef6760ae-09e7-4682-a72d-f4000109e3af', extraction).
narrative_ontology:cs_reading_relation('ef6760ae-09e7-4682-a72d-f4000109e3af', imposition_pathway_kernel__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('ef6760ae-09e7-4682-a72d-f4000109e3af', imposition_pathway_kernel__hybrid_cascade_reading, influences).
narrative_ontology:cs_axiom('ef6760ae-09e7-4682-a72d-f4000109e3af', foundational, state_capacity_enables_fringe_free_imposition).
narrative_ontology:cs_axiom_status(state_capacity_enables_fringe_free_imposition, holdable).
narrative_ontology:cs_axiom_grounding('ef6760ae-09e7-4682-a72d-f4000109e3af', state_capacity_enables_fringe_free_imposition, empirically_contingent).
narrative_ontology:cs_axiom('ef6760ae-09e7-4682-a72d-f4000109e3af', foundational, m_set_requires_override_cell).
narrative_ontology:cs_axiom_status(m_set_requires_override_cell, holdable).
narrative_ontology:cs_axiom_grounding('ef6760ae-09e7-4682-a72d-f4000109e3af', m_set_requires_override_cell, conventional).
narrative_ontology:cs_reference_frame('ef6760ae-09e7-4682-a72d-f4000109e3af', m_set_climb_only_framework).
narrative_ontology:cs_drift_state('ef6760ae-09e7-4682-a72d-f4000109e3af', meiji_override_empirical_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ef6760ae-09e7-4682-a72d-f4000109e3af', '').
narrative_ontology:cs_kernel_id(imposition_pathway_kernel__exogenous_override_reading, imposition_pathway_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__exogenous_override_reading, state_elites).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__exogenous_override_reading, modernizing_officials).
narrative_ontology:constraint_victim(imposition_pathway_kernel__exogenous_override_reading, traditional_elites).
narrative_ontology:constraint_victim(imposition_pathway_kernel__exogenous_override_reading, rural_populations).
narrative_ontology:constraint_victim(imposition_pathway_kernel__exogenous_override_reading, religious_authorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__exogenous_override_reading, traditional_elites).
narrative_ontology:constraint_vindicates(imposition_pathway_kernel__exogenous_override_reading, state_capacity_as_independent_variable).
narrative_ontology:constraint_vindicates(imposition_pathway_kernel__exogenous_override_reading, imposition_as_distinct_mechanism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Meiji oligarchs (Ito, Okubo, Kido) who issued the calendar and dress decrees. They control the enforcement machinery (police, ministry directives, school system) and capture the coordination gains: unified administration, treaty revision leverage, symbolic parity with great powers. Exit is arbitrage-grade — they designed the constraint and can modify it.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, state_elites, agenda_setter,
    institutional, generational, arbitrage, national).

% Mid-level bureaucrats, military officers, educators who gain career advancement and institutional coherence from the new temporal/sartorial order. They adopt Western dress and Gregorian calendar as professional requirements and status markers. Mobile exit — they could emigrate or join private sector — but their identity is fused with the modernizing project.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, modernizing_officials, beneficiary,
    organized, biographical, mobile, national).

% Court nobles (kuge), former daimyo, Shinto priests who lose calendrical authority and status markers (court dress, lunar calendar rites). Some gain peerage pensions and House of Peers seats — secondary beneficiary position. Constrained exit: their status now depends on the new state's recognition; they cannot return to the old order.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, traditional_elites, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(imposition_pathway_kernel__exogenous_override_reading, traditional_elites, beneficiary).

% Peasant households bearing material costs: wardrobe replacement (Western clothing mandated for men 1871, women 1886), calendar disruption of agricultural rhythms and festival cycles, police enforcement of haircut/dress codes. Identity-locked to village community and land — cannot exit the constraint without abandoning their social world. No fringe adoption of Western dress/calendar before 1868 in rural Japan.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, rural_populations, payer,
    powerless, biographical, identity_locked, local).

% Shinto shrine priests and Buddhist temple networks that lose control over festival calendars (lunar calendar governed ritual timing) and face state regulation of religious dress. Trapped: their institutional survival depends on state recognition (Shinto as state cult, Buddhism as regulated religion); they cannot exit the state's jurisdictional claim.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, religious_authorities, payer,
    organized, generational, trapped, national).

% Analysts of state formation and commitment systems (Tilly, Mann, Wimmer, Centeno, and the M-set framework authors). They observe the constraint from outside, coding mechanism cells and measuring extraction/coordination. Their exit is analytical — they can change frameworks without material cost.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a unified temporal frame (Gregorian calendar) and visible modernity marker (Western dress) enabling railway scheduling, diplomatic synchronization, bureaucratic administration, and great-power recognition — solved by state decree rather than organic diffusion.
% TRANSFER_FUNCTION: Moves compliance costs (wardrobe replacement, calendar conversion, agricultural disruption, status surrender) from rural populations, traditional elites, and religious authorities to state elites and modernizing officials who capture administrative coordination gains and symbolic parity.
% ABSENT_VOICES: Rural women (bore disproportionate wardrobe/calendar burden, no political voice), Ainu and Ryukyuan populations (subjected to same decrees without representation), urban poor (cost of Western clothing relative to income) — all structurally excluded from the decree process and subsequent M-set coding debates.
% DISAPPEARANCE_RATIONALE: If the override mechanism vanished in 1868, Japan would have faced a different path: either gradual climb (like Siam) with lower extraction but slower coordination, or colonization (like China) with externally imposed coordination. The world rearranges because the override cell is a distinct causal pathway — its presence/absence changes the trajectory of state formation.
% FOUNDING_PROBLEM: How to rapidly achieve the temporal/sartorial coordination standards of great powers (treaty revision, military synchronization, administrative unity) without the decades of fringe adoption that characterized Western transitions.
% FOUNDING_PROBLEM_CORROBORATION: Meiji oligarchs' own writings (Ito's memoirs, Kido's diary) attest the founding problem as live and urgent — great-power pressure was real. Thai/Siamese historical record (King Chulalongkorn's gradual reforms without overthrow) corroborates that the problem was solvable without override — a non-beneficiary corroboration that the founding problem was real but the override was a choice, not a necessity.
narrative_ontology:disappearance_verdict(imposition_pathway_kernel__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_pathway_kernel__exogenous_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_pathway_kernel__exogenous_override_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(imposition_pathway_kernel__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_pathway_kernel__exogenous_override_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_pathway_kernel__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imposition_pathway_kernel__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imposition_pathway_kernel__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.72 (interval end) reflects the heavy initial compliance costs (calendar conversion, wardrobe replacement, social disruption) that fall disproportionately on non-elites, declining as the new commitments become background infrastructure. Suppression 0.78 reflects the active enforcement required: police enforcement of haircut/dress codes, school and workplace mandates, penalization of lunar calendar use. Theater ratio 0.22 at interval end shows the transition from raw coercion to ritualized performance (e.g., Western dress as ceremonial requirement for officials while daily life partially reverts). Accessibility collapse 0.65: alternatives (lunar calendar, traditional dress) do not fully disappear but are pushed to private/ceremonial spheres. Resistance 0.58: substantial early resistance (Shinpuren Rebellion 1876, calendar riots) decays but persists in cultural memory. Claimed type tangled_rope captures the dual character: genuine coordination of modern state temporal/sartorial order + asymmetric extraction from those who bear transition costs without proportional benefit.
 *
 * DIRECTIONALITY LOGIC:
 *   State elites and modernizing officials are structural beneficiaries: they gain a unified temporal frame for administration, railways, diplomacy, and a visible marker of modernization (Western dress) that signals equality with great powers. Their directionality is near-beneficiary (d ~ 0.15). Traditional elites (court nobles, domain lords) are primary targets: they lose status markers (court dress, lunar calendar authority) and bear symbolic costs of surrendering tradition. Rural populations bear material costs (wardrobe replacement, calendar confusion disrupting agricultural rhythms) with no compensating benefit — directionality near-target (d ~ 0.85). Religious authorities (Shinto/Buddhist) lose calendrical authority over festivals and rituals; their directionality is high-target. The derivation chain captures this via beneficiaries/victims + exit: rural populations are identity_locked to land/community, traditional elites are constrained by status dependence on the new state, religious authorities are trapped by institutional dependence on state recognition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (creating a unified temporal/sartorial order legible to great powers and functional for modern administration) was live at founding. By 1912 the problem is substantially solved (Gregorian calendar is global standard; Western dress is normalized), yet the enforcement machinery and symbolic requirements persist — a classic mandatrophy signature. The constraint prevents mislabeling this as pure coordination (rope) because the extraction was real and enforced; it prevents mislabeling as pure extraction (snare) because the coordination function was genuine and the new order persists as functional infrastructure. The tangled_rope classification holds the tension: the override was real, the climb followed, but the override cell is analytically distinct from the climb cells.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fringe_visibility_threshold,
    'How small or invisible must a pre-decree fringe be before it counts as ''no meaningful fringe adoption'' rather than ''compressed climb with invisible fringe''?',
    'Historical investigation of pre-1868 Japanese exposure to Gregorian calendar and Western dress among interpreters, merchants, rangaku scholars, and Nagasaki/Dejima contacts. Quantify adoption rates in these micro-populations.',
    'If a detectable fringe exists (even <0.1% of population), the endogenous_climb_reading gains empirical ground; if truly zero, the exogenous override cell is empirically necessitated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fringe_visibility_threshold, empirical, 'Threshold for ''meaningful fringe'' in the imposition/climb distinction').

omega_variable(
    coordination_extraction_boundary,
    'Is the temporal/sartorial coordination function genuinely served by the override mechanism, or could the same coordination have been achieved through a slower climb with lower extraction?',
    'Counterfactual comparison: Meiji''s override path vs. Thailand''s (Siam) gradual adoption of Western calendar/dress without colonization or overthrow. Measure coordination outcomes (railway scheduling, diplomatic synchronization, administrative unity) and extraction costs in both paths.',
    'If Thailand achieved comparable coordination with lower extraction, the Meiji override''s extraction was not functionally necessary — it was a choice enabled by state capacity, not a coordination requirement. This would shift the constraint toward snare. If Meiji''s speed was functionally necessary for survival (unequal treaty revision), the extraction is the price of coordination — tangled_rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the override''s extraction was functionally necessary for the coordination achieved').

omega_variable(
    reading_foreclosure_structure,
    'Does the exogenous override reading logically foreclose the endogenous climb reading within a single analytical framework, or do they coexist as explanations for different historical cases?',
    'Formalize the M-set cell definitions: if ''override cell'' and ''climb cell'' are mutually exclusive categories for a given displacement event, they foreclose; if a single event can be coded as both (override initiating, climb completing), they coexist or influence.',
    'If forecloses: the kernel is genuinely contested — choosing a reading commits to an ontology of mechanism. If coexists: the readings are complementary lenses, not rival ontologies. This determines the cs_structure.reading_relations assignment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_structure, conceptual, 'Logical relationship between override and climb mechanism categories').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_pathway_kernel__exogenous_override_reading, 1868, 1912).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(imposition_exog_tr_t1868, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 1868, 0.05).
narrative_ontology:measurement(imposition_exog_tr_t1873, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 1873, 0.12).
narrative_ontology:measurement(imposition_exog_tr_t1882, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 1882, 0.18).
narrative_ontology:measurement(imposition_exog_tr_t1890, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 1890, 0.22).
narrative_ontology:measurement(imposition_exog_tr_t1900, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 1900, 0.25).
narrative_ontology:measurement(imposition_exog_tr_t1912, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 1912, 0.22).

% Extraction over time
narrative_ontology:measurement(imposition_exog_be_t1868, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 1868, 0.85).
narrative_ontology:measurement(imposition_exog_be_t1873, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 1873, 0.78).
narrative_ontology:measurement(imposition_exog_be_t1882, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 1882, 0.72).
narrative_ontology:measurement(imposition_exog_be_t1890, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 1890, 0.68).
narrative_ontology:measurement(imposition_exog_be_t1900, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 1900, 0.65).
narrative_ontology:measurement(imposition_exog_be_t1912, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 1912, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(imposition_exog_su_t1868, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 1868, 0.92).
narrative_ontology:measurement(imposition_exog_su_t1873, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 1873, 0.85).
narrative_ontology:measurement(imposition_exog_su_t1882, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 1882, 0.78).
narrative_ontology:measurement(imposition_exog_su_t1890, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 1890, 0.72).
narrative_ontology:measurement(imposition_exog_su_t1900, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 1900, 0.68).
narrative_ontology:measurement(imposition_exog_su_t1912, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 1912, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_pathway_kernel__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(imposition_pathway_kernel__exogenous_override_reading, 0.12).
narrative_ontology:affects_constraint(imposition_pathway_kernel__exogenous_override_reading, imposition_pathway_kernel__endogenous_climb_reading).
narrative_ontology:affects_constraint(imposition_pathway_kernel__exogenous_override_reading, imposition_pathway_kernel__hybrid_cascade_reading).
narrative_ontology:affects_constraint(imposition_pathway_kernel__exogenous_override_reading, meiji_calendar_reform_1873).
narrative_ontology:affects_constraint(imposition_pathway_kernel__exogenous_override_reading, meiji_dress_codes_1871).

% DUAL FORMULATION NOTE:
% This is the exogenous_override_reading of the imposition_pathway_kernel. The endogenous_climb_reading denies the override cell exists; the hybrid_cascade_reading treats override as initiating an artificial fringe that then climbs. All three share the kernel but instantiate different constraints with different ε (this reading: 0.72; endogenous: ~0.25; hybrid: ~0.45). The network edges represent the kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(imposition_pathway_kernel__exogenous_override_reading, powerful, 0.45).
constraint_indexing:directionality_override(imposition_pathway_kernel__exogenous_override_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
