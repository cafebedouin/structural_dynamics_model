% ============================================================================
% CONSTRAINT STORY: qwerty_persistence__lapsed_alternatives_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence__lapsed_alternatives_reading, []).

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
 *   constraint_id: qwerty_persistence__lapsed_alternatives_reading
 *   human_readable: QWERTY Persistence via Coordination Value (Lapsed Alternatives Reading)
 *   domain: technology_history/industrial_standards/path_dependence
 *
 * SUMMARY:
 *   The QWERTY keyboard layout became the dominant standard not because it
 *   was optimal, but because it reached critical mass adoption first.
 *   Alternative layouts (Dvorak, Colemak, etc.) demonstrably reduce finger
 *   travel and typing fatigue, but never achieved the network adoption
 *   threshold needed to overcome switching costs. This reading holds that the
 *   constraint's persistence is a pure coordination problem: everyone bears
 *   the cost of a suboptimal standard because no alternative ever coordinated
 *   enough adopters to make switching collectively rational. No identifiable
 *   beneficiary extracts rents from QWERTY's dominance — manufacturers,
 *   typists, and software developers all face symmetric switching costs. The
 *   constraint is a Rope: a genuine coordination mechanism with minimal
 *   coercive overhead, where the extraction is the irreducible cost of
 *   achieving interoperability at scale.
 *
 * KEY AGENTS:
 *   - early_typewriter_adopters: Coordinated on QWERTY via Remington's market dominance (1874-1890)
 *   - touch_typists_generations: Bear symmetric retraining costs across decades
 *   - keyboard_manufacturers: Locked into tooling and supply chains by volume expectations
 *   - software_developers: Build on QWERTY as assumed input model
 *   - alternative_layout_advocates: Dvorak (1936), Colemak (2006), etc. — never reach critical mass
 *   - ergonomics_researchers: Document efficiency gains of alternatives without adoption pathway
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence__lapsed_alternatives_reading, 0.18).
domain_priors:suppression_score(qwerty_persistence__lapsed_alternatives_reading, 0.12).
domain_priors:theater_ratio(qwerty_persistence__lapsed_alternatives_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence__lapsed_alternatives_reading, rope).
narrative_ontology:human_readable(qwerty_persistence__lapsed_alternatives_reading, "QWERTY Persistence via Coordination Value (Lapsed Alternatives Reading)").
narrative_ontology:topic_domain(qwerty_persistence__lapsed_alternatives_reading, "technology_history/industrial_standards/path_dependence").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence__lapsed_alternatives_reading, '8ec7a825-5af1-4d8c-85b5-dc583a432aee').
narrative_ontology:cs_kernel_codification('8ec7a825-5af1-4d8c-85b5-dc583a432aee', implicit).
narrative_ontology:cs_authority_grounding('8ec7a825-5af1-4d8c-85b5-dc583a432aee', practice).
narrative_ontology:cs_reading_relation('8ec7a825-5af1-4d8c-85b5-dc583a432aee', qwerty_persistence__incumbent_preservation_reading, coexists_with).
narrative_ontology:cs_axiom('8ec7a825-5af1-4d8c-85b5-dc583a432aee', foundational, coordination_critical_mass_governs_standard_persistence).
narrative_ontology:cs_axiom_status(coordination_critical_mass_governs_standard_persistence, holdable).
narrative_ontology:cs_axiom_grounding('8ec7a825-5af1-4d8c-85b5-dc583a432aee', coordination_critical_mass_governs_standard_persistence, empirically_contingent).
narrative_ontology:cs_axiom('8ec7a825-5af1-4d8c-85b5-dc583a432aee', foundational, no_beneficiary_extracts_rents_from_qwerty_dominance).
narrative_ontology:cs_axiom_status(no_beneficiary_extracts_rents_from_qwerty_dominance, holdable).
narrative_ontology:cs_axiom_grounding('8ec7a825-5af1-4d8c-85b5-dc583a432aee', no_beneficiary_extracts_rents_from_qwerty_dominance, empirically_contingent).
narrative_ontology:cs_reference_frame('8ec7a825-5af1-4d8c-85b5-dc583a432aee', mechanical_typing_interoperability_achieved).
narrative_ontology:cs_drift_state('8ec7a825-5af1-4d8c-85b5-dc583a432aee', digital_universal_computing_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8ec7a825-5af1-4d8c-85b5-dc583a432aee', '').
narrative_ontology:cs_kernel_id(qwerty_persistence__lapsed_alternatives_reading, qwerty_persistence).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(qwerty_persistence__lapsed_alternatives_reading, early_typewriter_adopters).
narrative_ontology:constraint_victim(qwerty_persistence__lapsed_alternatives_reading, touch_typists_generations).
narrative_ontology:constraint_victim(qwerty_persistence__lapsed_alternatives_reading, keyboard_manufacturers).
narrative_ontology:constraint_victim(qwerty_persistence__lapsed_alternatives_reading, software_developers).
narrative_ontology:constraint_vindicates(qwerty_persistence__lapsed_alternatives_reading, path_dependence_irreversibility).
narrative_ontology:constraint_vindicates(qwerty_persistence__lapsed_alternatives_reading, critical_mass_coordination_threshold).
narrative_ontology:constraint_vindicates(qwerty_persistence__lapsed_alternatives_reading, network_effects_standardization).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% First mass adopters of Remington typewriters with QWERTY layout (1874-1890). They invested in learning QWERTY touch-typing when it was the only viable commercial option. Their skill investment created the initial critical mass that locked in the standard. They bear the original coordination cost without having chosen it — Remington's market position made QWERTY the de facto standard.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, early_typewriter_adopters, payer,
    moderate, biographical, constrained, national).

% Successive generations of professional typists, office workers, and computer users who learned QWERTY as the universal standard. Their professional identity and muscle memory are fused with QWERTY — retraining is personally costly and professionally risky. Exit options are identity_locked: the skill is constitutive of their professional self-concept. They bear the ongoing coordination tax symmetrically with all other users.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, touch_typists_generations, payer,
    organized, generational, identity_locked, global).

% Tooling, supply chains, and production lines optimized for QWERTY. Producing alternative layouts requires separate SKUs, inventory, and demand forecasting — economically irrational without guaranteed volume. They do not benefit from QWERTY; they are constrained by market expectation. Any manufacturer unilaterally switching layouts would lose compatibility with the installed base.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, keyboard_manufacturers, payer,
    institutional, generational, constrained, global).

% Build applications, games, and OS interfaces assuming QWERTY key mappings (WASD movement, shortcut conventions, keyboard shortcuts). Alternative layouts require remapping layers that introduce friction and support burden. They bear coordination costs symmetrically — no developer profits from QWERTY's dominance; all face the same compatibility baseline.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, software_developers, payer,
    institutional, biographical, constrained, global).

% Designers and promoters of Dvorak (1936), Colemak (2006), Workman, and other efficiency-optimized layouts. They have the technical means to produce and distribute alternatives (OS support exists, keyboards are manufacturable), but cannot coordinate the critical mass of adopters needed to make switching collectively rational. They are excluded from the dominant coordination equilibrium, not by suppression but by the mathematics of network effects.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, alternative_layout_advocates, excluded,
    moderate, biographical, mobile, global).

% Study repetitive strain injury rates, typing efficiency, and biomechanical optimization. They document that alternative layouts reduce finger travel by 30-50% and error rates measurably, but their research cannot create the coordination critical mass. They observe the constraint from outside — their role is measurement, not participation.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, ergonomics_researchers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence__lapsed_alternatives_reading, diffuse).
narrative_ontology:fixing_cost_class(qwerty_persistence__lapsed_alternatives_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Achieves universal human-computer input interoperability: any person can sit at any keyboard and type; any software receives predictable key codes; any manufacturer produces one layout for global market.
% TRANSFER_FUNCTION: Moves coordination overhead (retraining cost, tooling lock-in, compatibility maintenance) from a hypothetical coordinated switch onto every individual user and organization symmetrically — no transfer to a beneficiary, only distributed deadweight loss from failing to reach critical mass for a superior alternative.
% ABSENT_VOICES: Future generations who will inherit the QWERTY coordination tax without ever participating in the original adoption decision. They are structurally excluded from the founding coordination event (1874-1900) but bear its costs indefinitely. Also absent: the counterfactual world where Dvorak or another layout reached critical mass first — those potential adopters never existed to object.
% DISAPPEARANCE_RATIONALE: If QWERTY vanished overnight, the world would rearrange around a new coordination equilibrium. There is no natural law forcing QWERTY; a superior layout (likely Dvorak or Colemak) would rapidly coordinate adopters because the efficiency gains are real and the coordination problem would reset. The constraint is not a Mountain — its disappearance would trigger massive reorganization, not stasis.
% FOUNDING_PROBLEM: Mechanical typewriter key-jamming: early typewriters jammed when adjacent typebars struck in rapid succession. QWERTY separated common letter pairs to slow typists down and prevent jams. The standard was adopted by Remington (1874) and achieved critical mass before the jamming problem was solved by better engineering.
% FOUNDING_PROBLEM_CORROBORATION: Typewriter engineering history (independent of Remington/QWERTY beneficiaries) confirms: the jamming problem was solved by the 1890s with improved typebar mechanics and front-strike designs. The QWERTY layout persisted after its founding problem vanished — corroborated by mechanical engineering literature and the successful adoption of non-QWERTY layouts on later typewriter models that never achieved critical mass.
narrative_ontology:disappearance_verdict(qwerty_persistence__lapsed_alternatives_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence__lapsed_alternatives_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence__lapsed_alternatives_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(qwerty_persistence__lapsed_alternatives_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence__lapsed_alternatives_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence__lapsed_alternatives_reading_tests).
:- end_tests(qwerty_persistence__lapsed_alternatives_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18) reflects the persistent coordination tax of a suboptimal standard — the cost every user pays in slower typing, higher error rates, and repetitive strain, not because anyone collects it, but because the critical mass threshold for switching was never met. Suppression (0.12) is low: no one prevents Dvorak keyboards; they are legally sold, OS-supported, and freely adoptable. The constraint persists because the coordination problem is real, not because alternatives are actively crushed. Theater ratio (0.08) is minimal: the standard's maintenance is functional (compatibility), not performative. Accessibility collapse (0.72) is high: once you learn QWERTY, alternatives become practically inaccessible without massive coordinated effort. Resistance (0.15) is low: the constraint meets little active opposition because it is not experienced as extraction — it is experienced as 'how keyboards work.'
 *
 * PERSPECTIVAL GAP:
 *   All seats experience this as a symmetric coordination burden. The engine will compute near-identical types across stakeholder seats because directionality is near 0.5 for all parties — no one benefits, everyone pays the coordination tax equally. This symmetry is the structural signature of a genuine Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries declared — the constraint has no party that collects from its operation. No victims declared — all parties bear symmetric coordination costs. Directionality for all agents derives to ~0.5 (symmetric). The epsilon (0.18) is the pure coordination overhead of a standard that reached critical mass before alternatives could organize.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (interoperable mechanical typing) is dead — mechanical typewriters are obsolete. Yet the arrangement persists because the coordination function (digital interoperability) remains live. This is not mandatrophy: the constraint solved Problem A, Problem A vanished, but Problem B (software/device interoperability) emerged and the same standard solves it. The mandate evolved, it did not atrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Does this constraint represent the lapsed_alternatives_reading of the qwerty_persistence kernel, or a different structural claim?',
    'Compare the authored epsilon, beneficiary/victim structure, and coordination function against the sibling reading (incumbent_preservation_reading) to confirm reading-specific structural delta.',
    'If the structural delta does not match the reading definition (Rope with no beneficiaries, symmetric costs), this file misinstantiates the kernel reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms this constraint is the lapsed_alternatives_reading of qwerty_persistence kernel').

omega_variable(
    coordination_extraction_boundary,
    'Is the measured extractiveness (0.18) purely the coordination cost of reaching critical mass, or does it contain hidden extraction from incumbents?',
    'Historical cost accounting of typewriter/keyboard transition periods; compare switching costs for adopters vs. non-adopters; measure whether any party collects rents from the standard''s persistence.',
    'If hidden extraction exists, the constraint is a false Rope masking a Tangled Rope or Snare; the lapsed_alternatives_reading would be empirically falsified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether epsilon reflects pure coordination cost or contains incumbent rent').

omega_variable(
    sibling_reading_relation,
    'What is the structural relationship between the lapsed_alternatives_reading and the incumbent_preservation_reading?',
    'Analyze whether the two readings can be held simultaneously by different parties in the same historical framework (coexists_with), whether one logically forecloses the other in any single framework (forecloses), or whether the lapsed reading creates downstream pressure on the incumbent reading without foreclosing it (influences).',
    'Determines the cs_structure.reading_relations declaration and whether the kernel has genuine structural fork or complementary framings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_relation, conceptual, 'Structural relationship between the two declared readings of qwerty_persistence kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence__lapsed_alternatives_reading, 1874, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t1874, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 1874, 0.02).
narrative_ontology:measurement(qwer_tr_t1890, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 1890, 0.05).
narrative_ontology:measurement(qwer_tr_t1900, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 1900, 0.08).
narrative_ontology:measurement(qwer_tr_t1920, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 1920, 0.1).
narrative_ontology:measurement(qwer_tr_t1950, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 1950, 0.08).
narrative_ontology:measurement(qwer_tr_t1980, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 1980, 0.08).
narrative_ontology:measurement(qwer_tr_t2000, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 2000, 0.08).
narrative_ontology:measurement(qwer_tr_t2024, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 2024, 0.08).

% Extraction over time
narrative_ontology:measurement(qwer_be_t1874, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 1874, 0.05).
narrative_ontology:measurement(qwer_be_t1890, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 1890, 0.12).
narrative_ontology:measurement(qwer_be_t1900, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 1900, 0.18).
narrative_ontology:measurement(qwer_be_t1920, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 1920, 0.2).
narrative_ontology:measurement(qwer_be_t1950, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 1950, 0.18).
narrative_ontology:measurement(qwer_be_t1980, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 1980, 0.18).
narrative_ontology:measurement(qwer_be_t2000, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 2000, 0.18).
narrative_ontology:measurement(qwer_be_t2024, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 2024, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t1874, qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 1874, 0.03).
narrative_ontology:measurement(qwer_su_t1890, qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 1890, 0.08).
narrative_ontology:measurement(qwer_su_t1900, qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 1900, 0.12).
narrative_ontology:measurement(qwer_su_t1920, qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 1920, 0.12).
narrative_ontology:measurement(qwer_su_t1950, qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 1950, 0.12).
narrative_ontology:measurement(qwer_su_t1980, qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 1980, 0.12).
narrative_ontology:measurement(qwer_su_t2000, qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 2000, 0.12).
narrative_ontology:measurement(qwer_su_t2024, qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 2024, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence__lapsed_alternatives_reading, information_standard).
narrative_ontology:boltzmann_floor_override(qwerty_persistence__lapsed_alternatives_reading, 0.02).
narrative_ontology:affects_constraint(qwerty_persistence__lapsed_alternatives_reading, dvorak_adoption_failure).
narrative_ontology:affects_constraint(qwerty_persistence__lapsed_alternatives_reading, colemak_adoption_failure).
narrative_ontology:affects_constraint(qwerty_persistence__lapsed_alternatives_reading, digital_input_standard_interoperability).

% DUAL FORMULATION NOTE:
% This is the lapsed_alternatives_reading of the qwerty_persistence kernel. The sibling incumbent_preservation_reading claims active beneficiary defense. The two readings share the same referent (QWERTY persistence) but author different epsilon, beneficiary structures, and coordination/extraction boundaries — per epsilon-invariance, they are distinct constraints linked by kernel_id.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
