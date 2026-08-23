% ============================================================================
% CONSTRAINT STORY: technology_reformation_causality__technological_determinism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_reformation_causality__technological_determinism_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: technology_reformation_causality__technological_determinism_reading
 *   human_readable: Printing Press as Inevitable Driver of Reformation
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This reading asserts that the printing press, by radically reducing the
 *   marginal cost of reproducing vernacular scripture, created a physical
 *   constraint that made the Reformation inevitable. The technology is
 *   treated as a mountain — once the press exists, its production economics
 *   operate as a natural law that no human institution can override.
 *   Reformers are downstream adapters who exploit but do not direct this
 *   capability; the Catholic Church's scripture monopoly is the primary
 *   victim, unable to enforce scarcity against mechanical reproduction. The
 *   claimed type is mountain, but the presence of identifiable beneficiaries
 *   (vernacular publics, print trade) and the human origin of the technology
 *   create a false summit profile.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_reformation_causality__technological_determinism_reading, 0.35).
domain_priors:suppression_score(technology_reformation_causality__technological_determinism_reading, 0.65).
domain_priors:theater_ratio(technology_reformation_causality__technological_determinism_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_reformation_causality__technological_determinism_reading, mountain).
narrative_ontology:human_readable(technology_reformation_causality__technological_determinism_reading, "Printing Press as Inevitable Driver of Reformation").
narrative_ontology:topic_domain(technology_reformation_causality__technological_determinism_reading, "history_of_technology/religious_history/media_studies").

domain_priors:emerges_naturally(technology_reformation_causality__technological_determinism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_reformation_causality__technological_determinism_reading, '7756f039-ca69-434a-82f2-d248bd5819e0').
narrative_ontology:cs_kernel_codification('7756f039-ca69-434a-82f2-d248bd5819e0', formalized).
narrative_ontology:cs_authority_grounding('7756f039-ca69-434a-82f2-d248bd5819e0', expertise).
narrative_ontology:cs_interpretation_layer_present('7756f039-ca69-434a-82f2-d248bd5819e0').
narrative_ontology:cs_reading_relation('7756f039-ca69-434a-82f2-d248bd5819e0', technology_reformation_causality__beneficiary_agency_reading, forecloses).
narrative_ontology:cs_reading_relation('7756f039-ca69-434a-82f2-d248bd5819e0', technology_reformation_causality__co_constitution_reading, forecloses).
narrative_ontology:cs_axiom('7756f039-ca69-434a-82f2-d248bd5819e0', foundational, technological_inevitability_thesis).
narrative_ontology:cs_axiom_status(technological_inevitability_thesis, holdable).
narrative_ontology:cs_axiom_grounding('7756f039-ca69-434a-82f2-d248bd5819e0', technological_inevitability_thesis, empirically_contingent).
narrative_ontology:cs_axiom('7756f039-ca69-434a-82f2-d248bd5819e0', foundational, agency_as_epiphenomenal).
narrative_ontology:cs_axiom_status(agency_as_epiphenomenal, holdable).
narrative_ontology:cs_axiom_grounding('7756f039-ca69-434a-82f2-d248bd5819e0', agency_as_epiphenomenal, empirically_contingent).
narrative_ontology:cs_reference_frame('7756f039-ca69-434a-82f2-d248bd5819e0', gutenberg_galaxy_onset).
narrative_ontology:cs_drift_state('7756f039-ca69-434a-82f2-d248bd5819e0', contemporary_digital_parallel, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7756f039-ca69-434a-82f2-d248bd5819e0', '').
narrative_ontology:cs_kernel_id(technology_reformation_causality__technological_determinism_reading, technology_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_reformation_causality__technological_determinism_reading, vernacular_literacy_publics).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__technological_determinism_reading, print_trade_networks).
narrative_ontology:constraint_victim(technology_reformation_causality__technological_determinism_reading, catholic_church_scripture_control).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(technology_reformation_causality__technological_determinism_reading, reformers).
narrative_ontology:constraint_victim(technology_reformation_causality__technological_determinism_reading, catholic_church_authority).
narrative_ontology:constraint_vindicates(technology_reformation_causality__technological_determinism_reading, technological_inevitability_thesis).
narrative_ontology:constraint_vindicates(technology_reformation_causality__technological_determinism_reading, production_cost_determines_distribution_scale).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Reformers find themselves carried by a technological wave they did not create; their theological protests become mass movements only because the press makes vernacular distribution physically inevitable. They cannot choose to not use the press if they want reach; the technology determines the form and scale of their movement.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, reformers, payer,
    moderate, biographical, trapped, continental).

% The Church's millennium-long control over scripture access and interpretation is structurally undermined by a physical production technology it cannot regulate; censorship mechanisms fail against the scale and speed of press distribution.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, catholic_church_authority, payer,
    institutional, generational, constrained, continental).

% Ordinary people gain unprecedented direct access to scripture in their own languages; this access is not a gift of reformers but a physical consequence of production economics that makes vernacular editions cheaper than Latin manuscripts.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, vernacular_literacy_publics, beneficiary,
    organized, generational, mobile, continental).

% Printers and booksellers form a new commercial layer that profits from the structural demand for vernacular texts; their economic interest aligns with the technology's distribution logic, but they do not direct it.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, print_trade_networks, beneficiary,
    organized, biographical, arbitrage, continental).

% Scholars debate whether the press caused the Reformation or merely enabled it; this reading asserts the physical economics of production made the outcome inevitable regardless of human intention.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, historical_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The press solves the coordination problem of how to distribute identical texts to thousands of people across linguistic regions simultaneously — a physical coordination problem that manuscript culture could not solve.
% TRANSFER_FUNCTION: Moves scriptural authority from Latin-literate clergy to vernacular-literate laity; moves economic value from manuscript production to print capitalism; moves interpretive control from Church magisterium to individual readers.
% ABSENT_VOICES: Manuscript scribes and oral tradition bearers who are displaced by the press; their loss of livelihood and cultural role is not represented in the inevitability narrative.
% DISAPPEARANCE_RATIONALE: The physical capability of mass vernacular distribution is the necessary condition for the Reformation's scale and speed; without it, theological dissent remains localized and suppressible as it was for centuries before Gutenberg.
% FOUNDING_PROBLEM: The problem of how to make scripture accessible to laypeople at scale, which manuscript production could not solve due to cost, speed, and error rates.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians of the book trade (e.g., Febvre & Martin, Eisenstein) document the production cost collapse from manuscript to print; this is attested outside the theological beneficiaries of the Reformation.
narrative_ontology:disappearance_verdict(technology_reformation_causality__technological_determinism_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_reformation_causality__technological_determinism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_reformation_causality__technological_determinism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(technology_reformation_causality__technological_determinism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_reformation_causality__technological_determinism_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_reformation_causality__technological_determinism_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, ExtMetricName, E),
    domain_priors:suppression_score(technology_reformation_causality__technological_determinism_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(technology_reformation_causality__technological_determinism_reading),
    narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(technology_reformation_causality__technological_determinism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.35) because the constraint redistributes control from Church to vernacular sphere through production economics, not zero-sum theft. Suppression (0.65) reflects the Church's inability to maintain scripture scarcity against mechanical reproduction. Theater ratio is low (0.15) — the press's function is genuine coordination of information distribution, not performative. Accessibility collapse is high (0.88): once cheap vernacular editions exist, the alternative of controlled Latin manuscript distribution becomes physically non-viable. Resistance is low (0.22): the physical capability cannot be resisted, only delayed. The measurement series shows extractiveness and suppression rising as the press network saturates Europe, theater creeping up as commercial interests layer onto the coordination function.
 *
 * PERSPECTIVAL GAP:
 *   From the reformer seat, the press feels like a tool they wield (beneficiary framing); from the Church seat, it feels like an unstoppable force (target framing); from the vernacular public seat, it feels like liberation (beneficiary); from the analyst seat, the mountain claim is visible. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformers and Church are both targets (high d) — the technology constrains both, extracting agency from reformers and control from the Church. Vernacular publics and print trade are beneficiaries (low d) — they gain access and revenue from the same production economics. The press itself is not an agent but the constraint's physical substrate. The derivation would assign reformers low d (they gain reach), but this reading structurally positions them as trapped adapters — hence the directionality override.
 *
 * MANDATROPHY ANALYSIS:
 *   The mountain claim prevents mislabeling the press's coordination function (mass distribution) as pure extraction. The press genuinely solves a coordination problem. But the false summit risk is real: the 'natural law' framing obscures that the press was a human invention deployed in specific power contexts, and that its 'inevitability' narrative serves beneficiaries (print trade, vernacular elites) by naturalizing their advantage.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_technology,
    'Is the press''s capability a natural law (once invented) or a human construction that could have been otherwise?',
    'Counterfactual historical analysis: if Gutenberg had not invented movable type, would another technology have filled the same niche? Comparative analysis of Chinese/Korean movable type which did not produce equivalent effects.',
    'If constructed, the mountain claim collapses to tangled_rope — the press is a human-made constraint with beneficiaries (print trade) and victims (Church), requiring active enforcement (copyright, guild regulation) to maintain its extraction profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_technology, conceptual, 'Whether the press''s production economics are a discovered natural constraint or an invented social arrangement.').

omega_variable(
    agency_elimination_vs_constraint,
    'Does technological inevitability eliminate human agency or just constrain it within new parameters?',
    'Micro-historical analysis of reformer decision points: did Luther, Calvin, etc. have meaningful choices that changed outcomes, or were their choices fully determined by the press''s affordances?',
    'If agency is eliminated, the reading''s mountain claim strengthens; if agency is merely constrained, the reading overstates determinism and the constraint is better modeled as tangled_rope with reformers as coordinated-but-paying actors.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(agency_elimination_vs_constraint, conceptual, 'Whether reformers are true downstream adapters (no agency) or constrained agents (some agency).').

omega_variable(
    cost_reduction_as_extraction,
    'Does the production cost reduction constitute extraction from the Church''s monopoly, or just obsolescence?',
    'Economic analysis of the Church''s scripture monopoly rents pre- and post-press; comparison to other monopoly disruptions (e.g., digital media vs. print).',
    'If extraction, the Church is a victim in the structural sense and the constraint has snare/tangled_rope features; if obsolescence, the Church''s loss is incidental and the mountain claim is more defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_reduction_as_extraction, empirical, 'Whether the press''s cost advantage actively extracts value from the Church or merely renders the Church''s model obsolete.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_reformation_causality__technological_determinism_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t0, technology_reformation_causality__technological_determinism_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(tech_tr_t20, technology_reformation_causality__technological_determinism_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement(tech_tr_t40, technology_reformation_causality__technological_determinism_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(tech_tr_t60, technology_reformation_causality__technological_determinism_reading, theater_ratio, 60, 0.12).
narrative_ontology:measurement(tech_tr_t80, technology_reformation_causality__technological_determinism_reading, theater_ratio, 80, 0.14).
narrative_ontology:measurement(tech_tr_t100, technology_reformation_causality__technological_determinism_reading, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(tech_be_t0, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(tech_be_t20, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 20, 0.2).
narrative_ontology:measurement(tech_be_t40, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 40, 0.3).
narrative_ontology:measurement(tech_be_t60, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 60, 0.33).
narrative_ontology:measurement(tech_be_t80, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 80, 0.34).
narrative_ontology:measurement(tech_be_t100, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 100, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t0, technology_reformation_causality__technological_determinism_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(tech_su_t20, technology_reformation_causality__technological_determinism_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(tech_su_t40, technology_reformation_causality__technological_determinism_reading, suppression_requirement, 40, 0.55).
narrative_ontology:measurement(tech_su_t60, technology_reformation_causality__technological_determinism_reading, suppression_requirement, 60, 0.6).
narrative_ontology:measurement(tech_su_t80, technology_reformation_causality__technological_determinism_reading, suppression_requirement, 80, 0.63).
narrative_ontology:measurement(tech_su_t100, technology_reformation_causality__technological_determinism_reading, suppression_requirement, 100, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_reformation_causality__technological_determinism_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(technology_reformation_causality__technological_determinism_reading, 0.12).
narrative_ontology:affects_constraint(technology_reformation_causality__technological_determinism_reading, technology_reformation_causality__beneficiary_agency_reading).
narrative_ontology:affects_constraint(technology_reformation_causality__technological_determinism_reading, technology_reformation_causality__co_constitution_reading).

% DUAL FORMULATION NOTE:
% This reading and its two siblings form the technology_reformation_causality constraint family. This reading (technological_determinism_reading) treats the press as a mountain with ε from production cost reduction; beneficiary_agency_reading treats reformer strategy as primary with press as tool (rope/snare); co_constitution_reading treats the interaction as bidirectional (tangled_rope). The ε values differ substantially across readings because they assess different standing arrangements.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(technology_reformation_causality__technological_determinism_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
