% ============================================================================
% CONSTRAINT STORY: competence_retention_exercise__catastrophe_as_necessary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_retention_exercise__catastrophe_as_necessary, []).

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
 *   constraint_id: competence_retention_exercise__catastrophe_as_necessary
 *   human_readable: Catastrophe as Necessary for Competence Retention
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This constraint story models the institutionalized doctrine within
 *   high-reliability organization (HRO) theory that genuine competence
 *   retention requires actual catastrophic events. Simulation and near-miss
 *   analysis are dismissed as rehearsal lacking 'visceral stakes.' The
 *   doctrine functions as an epistemic gate that privileges post-disaster
 *   learning and marginalizes proactive alternatives. As a kernel reading
 *   (catastrophe_as_necessary), it is authored clean: no sibling reading is
 *   described inside the constraint, and metrics reflect only this reading's
 *   structural instantiation.
 *
 * KEY AGENTS:
 *   - hro_theorist_community: Primary agenda_setter and beneficiary (organized/identity_locked/global) â defines the doctrine and collects prestige and funding from its dominance.
 *   - safety_oversight_bodies: Secondary agenda_setter (institutional/constrained/national) â enforces post-disaster reform cycles and regulatory architecture.
 *   - frontline_operators: Primary payer (powerless/constrained/local) â bears physical and mortal costs of the 'necessary' catastrophes.
 *   - affected_public: Secondary payer (powerless/trapped/regional) â experiences catastrophic outcomes as organizational feedback.
 *   - simulation_advocates: Excluded voice (moderate/constrained/national) â argues for simulation sufficiency but is structurally marginalized.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_retention_exercise__catastrophe_as_necessary, 0.72).
domain_priors:suppression_score(competence_retention_exercise__catastrophe_as_necessary, 0.78).
domain_priors:theater_ratio(competence_retention_exercise__catastrophe_as_necessary, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, extractiveness, 0.72).
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_retention_exercise__catastrophe_as_necessary, tangled_rope).
narrative_ontology:human_readable(competence_retention_exercise__catastrophe_as_necessary, "Catastrophe as Necessary for Competence Retention").
narrative_ontology:topic_domain(competence_retention_exercise__catastrophe_as_necessary, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_retention_exercise__catastrophe_as_necessary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_retention_exercise__catastrophe_as_necessary, 'ea4391fd-b578-46eb-9cef-6da6ea9581c7').
narrative_ontology:cs_kernel_codification('ea4391fd-b578-46eb-9cef-6da6ea9581c7', distributed).
narrative_ontology:cs_authority_grounding('ea4391fd-b578-46eb-9cef-6da6ea9581c7', distributed).
narrative_ontology:cs_reading_relation('ea4391fd-b578-46eb-9cef-6da6ea9581c7', competence_retention_exercise__simulation_as_sufficient, forecloses).
narrative_ontology:cs_reading_relation('ea4391fd-b578-46eb-9cef-6da6ea9581c7', competence_retention_exercise__near_miss_as_bridge, forecloses).
narrative_ontology:cs_axiom('ea4391fd-b578-46eb-9cef-6da6ea9581c7', foundational, only_catastrophe_produces_visceral_stakes).
narrative_ontology:cs_axiom_status(only_catastrophe_produces_visceral_stakes, holdable).
narrative_ontology:cs_axiom_grounding('ea4391fd-b578-46eb-9cef-6da6ea9581c7', only_catastrophe_produces_visceral_stakes, empirically_contingent).
narrative_ontology:cs_axiom('ea4391fd-b578-46eb-9cef-6da6ea9581c7', foundational, competence_decays_without_genuine_threat).
narrative_ontology:cs_axiom_status(competence_decays_without_genuine_threat, holdable).
narrative_ontology:cs_axiom_grounding('ea4391fd-b578-46eb-9cef-6da6ea9581c7', competence_decays_without_genuine_threat, empirically_contingent).
narrative_ontology:cs_reference_frame('ea4391fd-b578-46eb-9cef-6da6ea9581c7', post_disaster_learning_paradigm).
narrative_ontology:cs_drift_state('ea4391fd-b578-46eb-9cef-6da6ea9581c7', high_fidelity_simulation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ea4391fd-b578-46eb-9cef-6da6ea9581c7', '').
narrative_ontology:cs_kernel_id(competence_retention_exercise__catastrophe_as_necessary, competence_retention_exercise).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_retention_exercise__catastrophe_as_necessary, hro_theorist_community).
narrative_ontology:constraint_victim(competence_retention_exercise__catastrophe_as_necessary, frontline_operators).
narrative_ontology:constraint_victim(competence_retention_exercise__catastrophe_as_necessary, affected_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines and promulgates the doctrine that genuine competence requires catastrophic failure; professional identity, funding, and field prestige depend on post-catastrophe analysis and institutional relevance.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, hro_theorist_community, agenda_setter,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(competence_retention_exercise__catastrophe_as_necessary, hro_theorist_community, beneficiary).

% Investigate catastrophes and mandate reforms; their operating model embeds post-disaster learning cycles and regulatory expansion, enforcing the catastrophe-necessary framing in official safety architecture.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, safety_oversight_bodies, agenda_setter,
    institutional, generational, constrained, national).

% Work in high-risk systems where catastrophic failure is treated as the ultimate feedback mechanism; bear the physical, psychological, and mortal costs when the 'necessary tuition' is paid.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, frontline_operators, payer,
    powerless, biographical, constrained, local).

% Live near or travel through high-risk systems; experience the catastrophic outcomes that serve as organizational learning events under the doctrine.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, affected_public, payer,
    powerless, biographical, trapped, regional).

% Develop and advocate for high-fidelity simulation and structured near-miss programs as substitutes for catastrophe; structurally marginalized in funding, publication venues, and safety curricula by the dominant doctrine.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, simulation_advocates, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_retention_exercise__catastrophe_as_necessary, hro_theorist_community).
narrative_ontology:fixing_cost_class(competence_retention_exercise__catastrophe_as_necessary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents organizational complacency during long incident-free periods by preserving a shared conviction that safety is provisional and requires periodic catastrophic 'reset' to validate and renew collective competence.
% TRANSFER_FUNCTION: Moves the costs of system failureâinjury, death, environmental harm, and economic lossâfrom the abstract risk pool onto frontline operators and the affected public, while concentrating epistemic authority and institutional legitimacy in the HRO theorist community and post-disaster oversight apparatus.
% ABSENT_VOICES: Simulation advocates and operators with catastrophe-free, near-miss-rich careers are excluded from the competence-certification discourse; their testimony that competence can be maintained without disaster is dismissed as lacking 'visceral' authority.
% DISAPPEARANCE_RATIONALE: If the doctrine vanished, organizations would shift investment toward continuous simulation and near-miss analysis as primary competence mechanisms; the prestige and funding model of catastrophe-centric HRO theory would collapse; regulatory cycles would shift from post-disaster reaction to proactive verification.
% FOUNDING_PROBLEM: Organizations in high-risk domains experience invisible decay of vigilance and procedural competence during long safe periods, a dynamic labeled 'normalization of deviance.'
% FOUNDING_PROBLEM_CORROBORATION: HRO theorists and safety oversight bodies attest the problem is live. Critics outside the benefiting partiesâsimulation researchers, some systems engineers, and empirical studies of high-fidelity simulator retentionâargue that normalization of deviance can be detected and corrected without catastrophe.
narrative_ontology:disappearance_verdict(competence_retention_exercise__catastrophe_as_necessary, world_rearranges).
narrative_ontology:founding_problem_status(competence_retention_exercise__catastrophe_as_necessary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_retention_exercise__catastrophe_as_necessary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_retention_exercise__catastrophe_as_necessary, 'none', 1).
narrative_ontology:epsilon_provenance(competence_retention_exercise__catastrophe_as_necessary, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_retention_exercise__catastrophe_as_necessary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_retention_exercise__catastrophe_as_necessary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_retention_exercise__catastrophe_as_necessary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the doctrine structurally requires catastrophic losses as tuition for organizational learning, extracting safety from operators and the public. Suppression (0.78) is higher still: the doctrine actively suppresses simulation and near-miss alternatives by declaring them epistemically inferior, enforcing this through funding gatekeeping, curriculum design, and peer review. Theater_ratio (0.48) reflects the growing ritualization of post-disaster investigations and reforms that repeat without structural change. The time series share one grid (0,10,20,30,40) to prevent misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   From the HRO theorist seat, the doctrine is genuine coordination against complacencyâa hard-won insight that safety decays without real stakes. From the operator and public seats, the same structure is enforced extraction that normalizes their injury and death as necessary inputs to institutional learning. The engine computes this divergence from the structural data rather than the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   The HRO theorist community sits near the beneficiary end (low d): the constraint subsidizes their professional identity, funding, and field relevance. Frontline operators and the affected public sit near the full-target end (high d): the constraint extracts safety and life from them. Safety oversight bodies sit ambiguouslyâadministrators with constrained exit who partly internalize the doctrine. Simulation advocates are excluded rather than targeted; their exclusion is the enforcement mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemânormalization of deviance during safe periodsâis arguably live, which prevents automatic piton classification. However, the doctrine's persistence has outgrown its original corrective function: it now suppresses viable alternatives (simulation) that could address the same problem. The tangled_rope classification captures both the genuine coordination function (preventing complacency) and the asymmetric extraction (catastrophe as required tuition).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_basis_visceral_stakes,
    'Is the claim that only catastrophes produce genuine organizational learning empirically supported, or is it a constructed narrative that protects the HRO theorist community''s epistemic authority?',
    'Meta-analysis comparing safety outcome trajectories of organizations that rely primarily on simulation versus those that rely on post-catastrophe reform cycles, controlling for industry and baseline risk.',
    'If simulation produces equivalent or superior safety outcomes, the doctrine''s extractiveness is revealed as largely parasitic, and the constraint shifts toward snare. If catastrophes uniquely produce durable reform, the coordination function is stronger, supporting tangled_rope or even rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_basis_visceral_stakes, empirical, 'Empirical test of the core premise that catastrophes are necessary for learning.').

omega_variable(
    simulation_suppression_mechanism,
    'Is the dismissal of simulation due to genuine cognitive inadequacy of simulators, or due to identity-lock within the HRO community that defines expertise through post-disaster experience?',
    'Funding and citation analysis comparing resource flows to simulation research in safety engineering versus catastrophe studies, paired with ethnography of hiring and promotion committees in HRO programs.',
    'If suppression is driven by identity-lock rather than evidence, effective extraction is higher than structural measures suggestâthe HRO community carries the suppression with them and blocks reform even when alternatives are viable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_suppression_mechanism, conceptual, 'Whether suppression of simulation is structural or identity-driven.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_retention_exercise__catastrophe_as_necessary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 0, 0.25).
narrative_ontology:measurement(comp_tr_t10, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 10, 0.35).
narrative_ontology:measurement(comp_tr_t20, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 20, 0.42).
narrative_ontology:measurement(comp_tr_t30, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 30, 0.45).
narrative_ontology:measurement(comp_tr_t40, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(comp_be_t10, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(comp_be_t20, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(comp_be_t30, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 30, 0.7).
narrative_ontology:measurement(comp_be_t40, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 40, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(comp_su_t10, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(comp_su_t20, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(comp_su_t30, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 30, 0.76).
narrative_ontology:measurement(comp_su_t40, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(competence_retention_exercise__catastrophe_as_necessary, simulation_as_sufficient).
narrative_ontology:affects_constraint(competence_retention_exercise__catastrophe_as_necessary, near_miss_as_bridge).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the competence_retention_exercise kernel. The three readings decompose the single natural-language questionâhow do HROs retain competence?âinto structurally distinct constraints with different epsilon values and victim sets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
