% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__endogenous_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_practice_standardization__endogenous_displacement_reading, []).

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
 *   constraint_id: legitimacy_of_practice_standardization__endogenous_displacement_reading
 *   human_readable: Endogenous Displacement Reading of Practice Standardization Legitimacy
 *   domain: political_history/modernization_studies/institutional_change
 *
 * SUMMARY:
 *   This constraint story captures the endogenous_displacement_reading of the
 *   legitimacy_of_practice_standardization kernel. It asserts that practice
 *   change (calendar adoption, metrication, dress reform, legal codification)
 *   is legitimate when it emerges from voluntary adoption driven by perceived
 *   utility or cultural evolution. The mechanism is diffusion: elites and
 *   merchants adopt first because they gain most from interoperability; the
 *   practice spreads through social networks and market pressures; late
 *   adopters face a shrinking ecosystem for the old practice, creating
 *   structural pressure without direct coercion. Resistance appears as
 *   temporary friction and 'double life' periods (e.g., dual dating,
 *   bilingualism), not sustained revolt. The claimed type is rope — a genuine
 *   coordination problem solved with minimal coercive overhead. Metrics
 *   reflect low extraction and suppression, but non-zero because the
 *   transition costs fall asymmetrically on those least equipped to bear
 *   them.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.15).
domain_priors:suppression_score(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.1).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__endogenous_displacement_reading, rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__endogenous_displacement_reading, "Endogenous Displacement Reading of Practice Standardization Legitimacy").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__endogenous_displacement_reading, "political_history/modernization_studies/institutional_change").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__endogenous_displacement_reading, '7e21d0d5-8b07-409d-89bc-fb97fb0d36e2').
narrative_ontology:cs_kernel_codification('7e21d0d5-8b07-409d-89bc-fb97fb0d36e2', distributed).
narrative_ontology:cs_authority_grounding('7e21d0d5-8b07-409d-89bc-fb97fb0d36e2', practice).
narrative_ontology:cs_interpretation_layer_present('7e21d0d5-8b07-409d-89bc-fb97fb0d36e2').
narrative_ontology:cs_reading_relation('7e21d0d5-8b07-409d-89bc-fb97fb0d36e2', legitimacy_of_practice_standardization__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('7e21d0d5-8b07-409d-89bc-fb97fb0d36e2', legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, coexists_with).
narrative_ontology:cs_axiom('7e21d0d5-8b07-409d-89bc-fb97fb0d36e2', foundational, voluntary_adoption_confers_legitimacy).
narrative_ontology:cs_axiom_status(voluntary_adoption_confers_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('7e21d0d5-8b07-409d-89bc-fb97fb0d36e2', voluntary_adoption_confers_legitimacy, conventional).
narrative_ontology:cs_axiom('7e21d0d5-8b07-409d-89bc-fb97fb0d36e2', foundational, cultural_evolution_as_legitimate_driver).
narrative_ontology:cs_axiom_status(cultural_evolution_as_legitimate_driver, holdable).
narrative_ontology:cs_axiom_grounding('7e21d0d5-8b07-409d-89bc-fb97fb0d36e2', cultural_evolution_as_legitimate_driver, conventional).
narrative_ontology:cs_reference_frame('7e21d0d5-8b07-409d-89bc-fb97fb0d36e2', pre_standardization_practice_ecology).
narrative_ontology:cs_drift_state('7e21d0d5-8b07-409d-89bc-fb97fb0d36e2', high_modernity_standardization_complete, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7e21d0d5-8b07-409d-89bc-fb97fb0d36e2', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, modernizing_elites).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, merchants_and_traders).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, state_bureaucrats).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__endogenous_displacement_reading, traditional_communities).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__endogenous_displacement_reading, voluntary_adoption_confers_legitimacy).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__endogenous_displacement_reading, cultural_evolution_as_legitimate_driver).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Experience gradual pressure to adopt new practices as utilities and networks shift around them. Adoption is not forced by decree but by changing economic and social conditions that make old practices less viable. Resistance appears as cultural friction and temporary 'double life' practices (e.g., using both old and new calendars), but exit from the old practice is structurally constrained by the shrinking ecosystem supporting it.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, traditional_communities, payer,
    organized, generational, constrained, regional).

% Champion new practices that align with international standards, scientific rationality, or commercial efficiency. They benefit from reduced transaction costs, enhanced legitimacy in international forums, and administrative simplification. Their power derives from control over education, media, and state apparatus, but they do not typically use coercion to enforce adoption.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, modernizing_elites, beneficiary,
    institutional, biographical, arbitrage, national).

% Adopt standardized practices (weights, measures, calendars, contracts) voluntarily because they reduce transaction costs and expand market access. They are net beneficiaries of the coordination function. Their exit option is high — they can operate in multiple systems simultaneously and choose the most advantageous.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, merchants_and_traders, beneficiary,
    powerful, biographical, mobile, global).

% Facilitate and record the transition, providing infrastructure (legal recognition, education, standards bodies) that lowers adoption costs. They benefit from administrative simplification and legibility. While they hold agenda-setting power, in this reading they act as enablers of a largely voluntary process rather than enforcers.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, state_bureaucrats, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__endogenous_displacement_reading, state_bureaucrats, beneficiary).

% Religious leaders, elders, and tradition-bearers who would object to the framing of their practices as 'obsolete' or 'inefficient.' They are not formally consulted in the standardization process; their authority erodes as the social ecosystem shifts. Their exit is identity-locked — abandoning the practice feels like abandoning their role and community.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, cultural_custodians, excluded,
    moderate, generational, identity_locked, local).

% Analyze the diffusion curves, regional variation, and elite-to-mass adoption patterns. They see the coordination function and the asymmetric costs but no central enforcement mechanism. Their seat is purely analytical — they neither collect nor pay.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of interoperability across expanding social, commercial, and administrative networks: a shared calendar enables scheduling; shared weights enable trade; shared administrative categories enable governance at scale. The constraint is the emerging standard itself, which coordinates by becoming the Schelling point.
% TRANSFER_FUNCTION: Moves the cost of transition (learning new practices, maintaining dual systems during transition, social friction) from the adopters collectively onto late-adopting communities and cultural custodians. The gains (reduced transaction costs, administrative efficiency, market access) accrue disproportionately to early adopters (merchants, elites, bureaucrats).
% ABSENT_VOICES: Cultural custodians (religious authorities, tribal elders, tradition-bearers) and subsistence communities whose practices are not oriented toward market or state legibility. They are structurally excluded because the standardization process operates through utility and network effects, not deliberative inclusion. Their objection would be that legitimacy requires communal assent, not just individual utility calculations.
% DISAPPEARANCE_RATIONALE: If the endogenous displacement mechanism vanished — i.e., if practices only changed by explicit collective deliberation or state decree — the modernization transitions of the 18th-20th centuries (calendar reform, metrication, dress reform, legal codification) would not have occurred on their observed timelines or with their observed diffusion patterns. The world would rearrange toward either frozen traditionalism or coercive top-down standardization.
% FOUNDING_PROBLEM: Pre-modern societies faced escalating coordination costs as trade networks expanded, states centralized, and scientific communication cross-cut local practice ecologies. The founding problem was: how to achieve interoperability without a central authority capable of designing and enforcing universal standards?
% FOUNDING_PROBLEM_CORROBORATION: Economic historians (e.g., Mokyr on 'useful knowledge', North on institutions) and diffusion sociologists (Rogers) attest that coordination problems of this kind are real and that voluntary adoption driven by perceived utility is a documented historical mechanism. No source outside the benefiting parties (modernizing elites, merchants) treats the founding problem as 'solved' — the tension between network-driven standardization and communal self-determination persists in contemporary debates about digital standards, language policy, and indigenous data sovereignty.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__endogenous_displacement_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__endogenous_displacement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__endogenous_displacement_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__endogenous_displacement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_practice_standardization__endogenous_displacement_reading_tests).
:- end_tests(legitimacy_of_practice_standardization__endogenous_displacement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.15) is low but non-zero: the coordination gains are real and widely shared, but transition costs are externalized onto late adopters and cultural custodians who did not choose the change. Suppression (0.1) is minimal — no enforcement machinery, but structural pressure from network effects creates de facto coercion for those with constrained exit. Theater ratio (0.2) reflects that some standardization rituals (official adoption ceremonies, educational campaigns) perform legitimacy rather than drive adoption. Accessibility collapse (0.3) is moderate: alternatives (old calendars, traditional measures) persist in ritual and private life but vanish from public/commercial spheres. Resistance (0.25) is the friction of transition, not organized opposition to the constraint itself.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter/beneficiary seats (elites, merchants, bureaucrats), the constraint appears as pure coordination — a rope. From the payer seat (traditional communities), it feels like a slow snare: costs are imposed by a shifting world they did not choose. From the excluded seat (cultural custodians), it is a snare that erases their authority under the cover of 'voluntary adoption.' The engine computes this divergence; the authored claim (rope) reflects the analytical observer's reading, not the payer's experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Modernizing elites and state bureaucrats are agenda_setters/beneficiaries (d ~ 0.15) — they shape the process and collect disproportionate gains. Merchants are beneficiaries with high exit (d ~ 0.1) — they adopt voluntarily and can arbitrage across systems. Traditional communities are payers (d ~ 0.7) — they bear transition costs with constrained exit. Cultural custodians are excluded (d ~ 0.85, identity_locked) — they lose authority and legitimacy without consent. Historical sociologists are observers (d = 0.5). The engine will compute per-seat effective extraction from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (coordination at scale without central enforcement) remains live — contemporary digital standardization (time zones, character encodings, protocol governance) recapitulates the same structure. The arrangement has not outlived its function; it has generalized. No mandatrophy resolution is declared because the coordination function is still operative, even as the specific historical practices (calendar, weights) have stabilized.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntary_vs_structural_coercion,
    'At what point does structural pressure from network effects become coercion, such that ''voluntary adoption'' is a mischaracterization?',
    'Counterfactual historical analysis: in cases where a community resisted adoption but eventually succumbed to economic isolation, was the mechanism functionally equivalent to a mandate? Compare adoption curves with and without state enforcement backstops.',
    'If structural pressure equals coercion above a threshold, the constraint reclassifies from rope toward tangled_rope or snare for payer/excluded seats. The rope claim depends on the voluntariness being genuine for all structurally positioned agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_vs_structural_coercion, conceptual, 'Whether network-effect pressure constitutes coercion for agents with constrained exit.').

omega_variable(
    coordination_extraction_boundary_in_diffusion,
    'How much of the measured extraction (transition costs on late adopters) is the necessary price of coordination versus asymmetric imposition?',
    'Compare total transition costs across the adopting population with total coordination gains (estimated from trade volume, administrative savings, reduced errors). If gains vastly exceed costs and are broadly distributed, extraction is coordination price; if costs concentrate on the powerless while gains concentrate on the powerful, extraction is asymmetric.',
    'If extraction is mostly coordination price, rope classification holds. If asymmetric, tangled_rope or snare for payer seats. This is the core empirical question for this reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary_in_diffusion, empirical, 'Whether the constraint''s extraction is the cost of coordination or asymmetric imposition.').

omega_variable(
    kernel_reading_foreclosure_boundary,
    'Does the endogenous_displacement_reading logically foreclose the exogenous_override_reading, or do they operate on different cases?',
    'Case-based analysis: for each historical standardization (Gregorian calendar, metric system, legal codes), determine whether the transition was purely endogenous or whether state enforcement was necessary at any stage. If some cases require both, the readings coexist as domain-partitioned explanations.',
    'If readings apply to disjoint case sets, they coexist_with. If every case shows both mechanisms, they influence each other. If the endogenous reading claims ALL legitimate standardization is endogenous, it forecloses the exogenous reading''s claim that SOME legitimate standardization is exogenous.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_boundary, conceptual, 'Structural relationship between this reading and the exogenous_override_reading sibling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__endogenous_displacement_reading, 1700, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t1700, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 1700, 0.1).
narrative_ontology:measurement(legi_tr_t1750, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 1750, 0.12).
narrative_ontology:measurement(legi_tr_t1800, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 1800, 0.15).
narrative_ontology:measurement(legi_tr_t1850, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 1850, 0.18).
narrative_ontology:measurement(legi_tr_t1900, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 1900, 0.2).
narrative_ontology:measurement(legi_tr_t1950, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 1950, 0.2).
narrative_ontology:measurement(legi_tr_t2000, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 2000, 0.2).

% Extraction over time
narrative_ontology:measurement(legi_be_t1700, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 1700, 0.05).
narrative_ontology:measurement(legi_be_t1750, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 1750, 0.08).
narrative_ontology:measurement(legi_be_t1800, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 1800, 0.12).
narrative_ontology:measurement(legi_be_t1850, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 1850, 0.14).
narrative_ontology:measurement(legi_be_t1900, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 1900, 0.15).
narrative_ontology:measurement(legi_be_t1950, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 1950, 0.15).
narrative_ontology:measurement(legi_be_t2000, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 2000, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t1700, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 1700, 0.05).
narrative_ontology:measurement(legi_su_t1750, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 1750, 0.07).
narrative_ontology:measurement(legi_su_t1800, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 1800, 0.08).
narrative_ontology:measurement(legi_su_t1850, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 1850, 0.09).
narrative_ontology:measurement(legi_su_t1900, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 1900, 0.1).
narrative_ontology:measurement(legi_su_t1950, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 1950, 0.1).
narrative_ontology:measurement(legi_su_t2000, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 2000, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__endogenous_displacement_reading, information_standard).
narrative_ontology:boltzmann_floor_override(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.02).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization__exogenous_override_reading).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the legitimacy_of_practice_standardization kernel. The endogenous_displacement_reading (this story) claims legitimacy derives from voluntary utility-driven adoption. The exogenous_override_reading claims legitimacy derives from state decree for collective benefit. The dual_practice_equilibrium_reading claims legitimacy is domain-partitioned. All three are live explanatory frameworks in modernization studies; they form a constraint family linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
