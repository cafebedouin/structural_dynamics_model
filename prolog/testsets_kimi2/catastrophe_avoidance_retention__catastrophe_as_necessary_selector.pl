% ============================================================================
% CONSTRAINT STORY: catastrophe_avoidance_retention__catastrophe_as_necessary_selector
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_avoidance_retention__catastrophe_as_necessary_selector, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: catastrophe_avoidance_retention__catastrophe_as_necessary_selector
 *   human_readable: Catastrophe as Necessary Competence Selector
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This constraint instantiates the reading
 *   'catastrophe_as_necessary_selector' of the kernel
 *   'catastrophe_avoidance_retention'. The standing arrangement is the
 *   institutionalized doctrine in safety engineering and high-reliability
 *   organization theory that only actual catastrophesâevents involving
 *   chaos, mortality salience, and organizational traumaâgenerate the
 *   selection pressure required to maintain operational competence. This
 *   doctrine shapes budget allocation, regulatory design, and professional
 *   training across nuclear power, commercial aviation, and process
 *   industries. Sibling readings ('simulation_as_proxy_catastrophe',
 *   'hybrid_near_miss_learning') dispute the irreplaceability of mortality
 *   salience, asserting that high-fidelity simulation or distributed
 *   near-miss learning can substitute. The authored metrics describe a
 *   doctrine that coordinates genuine post-disaster reform while
 *   asymmetrically extracting its learning costs from victims and frontline
 *   operators; the engine will compute seat divergence between beneficiaries
 *   and payers.
 *
 * KEY AGENTS:
 *   - post_disaster_reform_institutions: Primary beneficiary (institutional/analytical) â gain authority and budget from catastrophe-driven reform cycles
 *   - safety_researchers_traditional: Agenda-setter and beneficiary (organized/mobile) â promulgate the doctrine and capture research prestige
 *   - safety_critical_operators: Primary target (institutional/constrained) â bear catastrophic losses and competence decay costs
 *   - frontline_practitioners: Target (moderate/identity_locked) â experience skill atrophy and disaster consequences
 *   - affected_public: Target (powerless/trapped) â suffer mortality and morbidity that serves as the 'lesson'
 *   - simulation_advocates: Excluded voice (moderate/constrained) â argue for substitutable methods, structurally delegitimized
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.72).
domain_priors:suppression_score(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.78).
domain_priors:theater_ratio(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, extractiveness, 0.72).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, tangled_rope).
narrative_ontology:human_readable(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, "Catastrophe as Necessary Competence Selector").
narrative_ontology:topic_domain(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(catastrophe_avoidance_retention__catastrophe_as_necessary_selector).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, '18cd9947-2ade-4984-b81b-e060b7daa9f6').
narrative_ontology:cs_kernel_codification('18cd9947-2ade-4984-b81b-e060b7daa9f6', distributed).
narrative_ontology:cs_authority_grounding('18cd9947-2ade-4984-b81b-e060b7daa9f6', expertise).
narrative_ontology:cs_interpretation_layer_present('18cd9947-2ade-4984-b81b-e060b7daa9f6').
narrative_ontology:cs_reading_relation('18cd9947-2ade-4984-b81b-e060b7daa9f6', catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, forecloses).
narrative_ontology:cs_reading_relation('18cd9947-2ade-4984-b81b-e060b7daa9f6', catastrophe_avoidance_retention__hybrid_near_miss_learning, forecloses).
narrative_ontology:cs_axiom('18cd9947-2ade-4984-b81b-e060b7daa9f6', foundational, mortality_salience_irreplaceable).
narrative_ontology:cs_axiom_status(mortality_salience_irreplaceable, holdable).
narrative_ontology:cs_axiom_grounding('18cd9947-2ade-4984-b81b-e060b7daa9f6', mortality_salience_irreplaceable, empirically_contingent).
narrative_ontology:cs_axiom('18cd9947-2ade-4984-b81b-e060b7daa9f6', foundational, peacetime_competence_entropy).
narrative_ontology:cs_axiom_status(peacetime_competence_entropy, holdable).
narrative_ontology:cs_axiom_grounding('18cd9947-2ade-4984-b81b-e060b7daa9f6', peacetime_competence_entropy, empirically_contingent).
narrative_ontology:cs_reference_frame('18cd9947-2ade-4984-b81b-e060b7daa9f6', catastrophe_driven_competence_equilibrium).
narrative_ontology:cs_drift_state('18cd9947-2ade-4984-b81b-e060b7daa9f6', contemporary_simulation_challenge_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('18cd9947-2ade-4984-b81b-e060b7daa9f6', '').
narrative_ontology:cs_kernel_id(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, catastrophe_avoidance_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, post_disaster_reform_institutions).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, safety_researchers_traditional).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, safety_critical_operators).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, affected_public).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, frontline_practitioners).
narrative_ontology:constraint_vindicates(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, normalization_of_deviance_doctrine).
narrative_ontology:constraint_vindicates(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, high_reliability_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate nuclear plants, airlines, and process industries that experience long peacetime periods followed by rare catastrophic failures. Bear the financial, legal, and reputational costs of disasters, and struggle to justify proactive simulation budgets when institutional doctrine holds that only catastrophe produces real learning.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, safety_critical_operators, payer,
    institutional, generational, constrained, global).

% Live and work near safety-critical infrastructure. Suffer mortality, injury, and displacement when catastrophic accidents occur. Cannot readily relocate due to economic and geographic constraints, and have no voice in the doctrinal frameworks that treat their losses as necessary selection pressure.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, affected_public, payer,
    powerless, biographical, trapped, local).

% Pilots, surgeons, and control-room operators whose individual competence is expected to decay during long safe periods. Professional identity is fused to high-stakes performance; exiting the field means identity collapse. They experience the direct consequences of both gradual skill erosion and catastrophic system failures.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, frontline_practitioners, payer,
    moderate, biographical, identity_locked, national).

% Investigatory and regulatory bodies that expand in budget, staffing, and legal authority following major disasters. Their institutional relevance cycles with catastrophe frequency; they produce reform recommendations that are implemented only after blood has been spilled.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, post_disaster_reform_institutions, beneficiary,
    institutional, generational, analytical, national).

% Academics and consultants who advance the doctrine that only catastrophe generates sufficient mortality salience for learning. They design curricula, lead investigations, and receive research funding tied to post-disaster analysis. Their frameworks set the norms for what counts as legitimate safety knowledge.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, safety_researchers_traditional, agenda_setter,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, safety_researchers_traditional, beneficiary).

% Engineers and resilience researchers who develop high-fidelity simulation and near-miss reporting systems. They argue that competence can be maintained without bloodshed, but are structurally excluded from core curricula, major funding streams, and investigation panels by the dominant doctrine that such methods build false confidence.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, simulation_advocates, excluded,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates societal attention and reform investment by providing an unambiguous, politically undeniable signal that safety margins have been breached, solving the collective action problem of when to allocate resources to safety improvement.
% TRANSFER_FUNCTION: Moves the cost of organizational learning from institutional budgets and proactive investment to victims of catastrophic failure and frontline operators; transfers authority, budget, and prestige to post-disaster reform institutions and traditional safety researchers.
% ABSENT_VOICES: Simulation advocates and resilience engineers who argue that competence can be maintained through high-fidelity drills and near-miss analysis; they are structurally excluded from funding, investigation panels, and core safety curricula by the doctrine that only blood teaches.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, safety-critical industries would reorganize around continuous simulation, near-miss reporting, and proactive resilience engineering; post-disaster institutions would shrink and safety research funding would shift from catastrophe sociology to systems design; the cycle of peacetime decay and post-disaster reform would break.
% FOUNDING_PROBLEM: How to maintain vigilance and operational competence in high-reliability organizations during long periods of accident-free operation.
% FOUNDING_PROBLEM_CORROBORATION: High-reliability organization researchers and disaster sociologists attest the problem from within the benefiting tradition. Resilience engineers, human-factors researchers, and simulation scientists outside that tradition contest that catastrophe is the necessary solution.
narrative_ontology:disappearance_verdict(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint's coordination function is purchased with irreplaceable human and organizational costs; suppression (0.78) is high because the doctrine actively delegitimizes simulation and near-miss alternatives, collapsing their institutional access. Theater_ratio (0.45) reflects substantial performative safety activity during peacetime that the doctrine itself predicts will decay. Accessibility_collapse (0.65) captures the institutional delegitimization of alternatives, though simulation technology exists structurally. Resistance (0.55) is moderate: simulation advocates and resilience engineers mount active intellectual and technical opposition, but they lack the funding and authority of the post-disaster complex. The measurement series show rising extraction and theater over the interval as simulation capabilities improve while the doctrine hardens against them.
 *
 * PERSPECTIVAL GAP:
 *   The post-disaster reform seat and the traditional researcher seat compute a low directionality (beneficiary), seeing genuine coordination in catastrophe-driven learning. The operator, practitioner, and affected-public seats compute high directionality (target), seeing an arrangement that tolerates their losses as the price of institutional attention. The simulation advocate seat, though excluded, would compute near-full target status because its exclusion is the enforcement mechanism. The engine derives this divergence from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are post-disaster reform institutions and traditional safety researchers: they collect authority, budget, and prestige from the constraint's operation. Victims are safety-critical operators (financial/catastrophic losses), frontline practitioners (competence decay and disaster exposure), and the affected public (mortality/morbidity). The directionality derivation places beneficiaries near the beneficiary end and victims near the target end. No override is needed because the structural derivation captures the relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâmaintaining vigilance during long safe periodsâis genuinely live, preventing a piton misclassification. However, the constraint's persistence is not merely inertial; it is actively enforced through curricula, investigation protocols, and funding structures that dismiss alternatives. This distinguishes it from a degraded piton. The coordination is real (post-disaster reform does occur), but the asymmetric extraction (catastrophe as the required payment) prevents snare classification. The Tangled Rope classification captures both the genuine coordination and the extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    catastrophe_necessity_vs_simulation_substitutability,
    'Is actual catastrophe with mortality salience structurally necessary for competence maintenance, or can high-fidelity simulation or near-miss learning functionally substitute?',
    'Comparative longitudinal studies of competence retention across organizations relying primarily on simulation, near-miss systems, or catastrophe-driven learning; measurement of decision quality under stress after each intervention.',
    'If simulation substitutes, this constraint''s extraction (paid in catastrophe victims) is unnecessary and the constraint reclassifies toward snare; if catastrophe is truly irreplaceable, the coordination function is genuine and the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(catastrophe_necessity_vs_simulation_substitutability, empirical, 'Whether mortality salience is substitutable by simulation').

omega_variable(
    organizational_decay_naturalness,
    'Does competence decay during peacetime represent an irreducible organizational property analogous to entropy, or is it a product of specific institutional choices that could be otherwise?',
    'Cross-organizational comparison of competence retention under identical peacetime lengths with varying simulation investment and leadership attention to safety.',
    'If irreducible, the constraint approaches Mountain from the operator seat; if constructed, it remains Tangled Rope or Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(organizational_decay_naturalness, conceptual, 'Natural organizational decay vs constructed institutional failure').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of simulation and proactive safety structural (resource denial, institutional exclusion) or internalized (practitioners and managers genuinely believe no alternative works)?',
    'Exit interview and belief surveys among safety professionals in high-reliability fields; resource-allocation analysis comparing safety budgets pre- and post-disaster within the same organization.',
    'If internalized, effective suppression is higher than structural measure suggests; if purely structural, reform through funding redirection is possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression of alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 0, 0.3).
narrative_ontology:measurement(cata_tr_t10, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 10, 0.35).
narrative_ontology:measurement(cata_tr_t20, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 20, 0.42).
narrative_ontology:measurement(cata_tr_t30, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 30, 0.5).
narrative_ontology:measurement(cata_tr_t40, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(cata_be_t10, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(cata_be_t20, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(cata_be_t30, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 30, 0.7).
narrative_ontology:measurement(cata_be_t40, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 40, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(cata_su_t10, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(cata_su_t20, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 20, 0.66).
narrative_ontology:measurement(cata_su_t30, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 30, 0.73).
narrative_ontology:measurement(cata_su_t40, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 40, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, simulation_as_proxy_catastrophe).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, hybrid_near_miss_learning).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'catastrophe_avoidance_retention'. It is structurally linked to sibling readings that dispute the necessity of catastrophe for competence maintenance. The epsilon values and beneficiary structures differ because this reading asserts irreplaceable mortality salience while siblings assert substitutability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
