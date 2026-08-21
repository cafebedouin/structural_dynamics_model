% ============================================================================
% CONSTRAINT STORY: qwerty_persistence__incumbent_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence__incumbent_preservation_reading, []).

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
 *   constraint_id: qwerty_persistence__incumbent_preservation_reading
 *   human_readable: QWERTY Persistence: Incumbent Preservation
 *   domain: technology_history/industrial_standards/path_dependence
 *
 * SUMMARY:
 *   This constraint describes the persistence of the QWERTY keyboard layout,
 *   specifically focusing on the active defense by incumbent manufacturers,
 *   trained typists, and training institutions to protect their capital
 *   investments and established positions. While QWERTY initially served a
 *   coordination function, its continued dominance is maintained through
 *   active suppression of alternatives and the leveraging of network effects,
 *   even as its original technical justification has become obsolete. This is
 *   one reading of the broader 'qwerty_persistence' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence__incumbent_preservation_reading, 0.7).
domain_priors:suppression_score(qwerty_persistence__incumbent_preservation_reading, 0.8).
domain_priors:theater_ratio(qwerty_persistence__incumbent_preservation_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence__incumbent_preservation_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence__incumbent_preservation_reading, "QWERTY Persistence: Incumbent Preservation").
narrative_ontology:topic_domain(qwerty_persistence__incumbent_preservation_reading, "technology_history/industrial_standards/path_dependence").

domain_priors:requires_active_enforcement(qwerty_persistence__incumbent_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence__incumbent_preservation_reading, '6ed87f9b-55b4-4038-9cec-238cccb3e8f8').
narrative_ontology:cs_kernel_codification('6ed87f9b-55b4-4038-9cec-238cccb3e8f8', implicit).
narrative_ontology:cs_authority_grounding('6ed87f9b-55b4-4038-9cec-238cccb3e8f8', practice).
narrative_ontology:cs_reading_relation('6ed87f9b-55b4-4038-9cec-238cccb3e8f8', qwerty_persistence__lapsed_alternatives_reading, coexists_with).
narrative_ontology:cs_axiom('6ed87f9b-55b4-4038-9cec-238cccb3e8f8', foundational, incumbent_advantage_is_structural).
narrative_ontology:cs_axiom_status(incumbent_advantage_is_structural, holdable).
narrative_ontology:cs_axiom_grounding('6ed87f9b-55b4-4038-9cec-238cccb3e8f8', incumbent_advantage_is_structural, empirically_contingent).
narrative_ontology:cs_reference_frame('6ed87f9b-55b4-4038-9cec-238cccb3e8f8', universal_interoperability_standard).
narrative_ontology:cs_drift_state('6ed87f9b-55b4-4038-9cec-238cccb3e8f8', contemporary_digital_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6ed87f9b-55b4-4038-9cec-238cccb3e8f8', '').
narrative_ontology:cs_kernel_id(qwerty_persistence__incumbent_preservation_reading, qwerty_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, qwerty_keyboard_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, trained_qwerty_typists).
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, typing_training_institutions).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, alternative_keyboard_manufacturers).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, efficiency_seeking_users).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, alternative_layout_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from existing tooling, supply chains, and market dominance. They actively resist changes to the QWERTY standard to protect their capital investments and established market position, often framing QWERTY as 'good enough' or universally preferred.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, qwerty_keyboard_manufacturers, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefit from their existing QWERTY typing skills, which are universally transferable across devices. They face high retraining costs and a perceived loss of professional identity if they were to switch to an alternative layout, making them resistant to change.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, trained_qwerty_typists, beneficiary,
    moderate, biographical, identity_locked, global).

% Benefit from a stable curriculum and an established market for QWERTY typing instruction. They have invested in QWERTY-specific teaching materials and instructor training, making them resistant to adopting new layouts.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, typing_training_institutions, beneficiary,
    organized, generational, constrained, national).

% Face high barriers to entry and limited market share due to the entrenched QWERTY standard. Despite offering potentially more ergonomic or efficient designs, they struggle to gain adoption against the network effects and incumbent defense of QWERTY.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, alternative_keyboard_manufacturers, payer,
    powerful, biographical, constrained, global).

% Bear the cost of suboptimal typing efficiency and potential ergonomic strain from the QWERTY layout. While they might desire more efficient alternatives, the switching costs (retraining, lack of universal support) are prohibitively high for most.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, efficiency_seeking_users, payer,
    powerless, immediate, constrained, global).

% Actively promote more efficient or ergonomic keyboard layouts (e.g., Dvorak, Colemak). They struggle to gain mainstream adoption due to the powerful network effects and active resistance from QWERTY incumbents, effectively being excluded from shaping the dominant standard.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, alternative_layout_advocates, excluded,
    moderate, generational, constrained, global).

% Investigate potential anti-competitive practices in standard setting. While they can intervene in formal standards, the de facto nature of QWERTY's dominance makes intervention challenging, often requiring extensive economic analysis and legal battles.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, competition_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence__incumbent_preservation_reading, qwerty_keyboard_manufacturers).
narrative_ontology:fixing_cost_class(qwerty_persistence__incumbent_preservation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universal, interoperable keyboard layout standard, allowing users to operate any keyboard and type with minimal setup, facilitating widespread adoption of typewriters and later computers.
% TRANSFER_FUNCTION: Transfers the cost of suboptimal efficiency, ergonomic strain, and suppressed innovation from incumbent QWERTY manufacturers and trained typists to alternative layout developers, efficiency-seeking users, and new entrants.
% ABSENT_VOICES: Advocates for more efficient or ergonomic layouts (e.g., Dvorak, Colemak users and developers) are largely unheard in the mainstream market, their innovations suppressed by the entrenched standard and incumbent defense.
% DISAPPEARANCE_RATIONALE: If QWERTY vanished overnight, the global typing infrastructure would collapse. It would require a massive, coordinated effort to adopt a new standard, retrain billions of typists, and retool manufacturing, leading to immense economic and social disruption.
% FOUNDING_PROBLEM: Early typewriters faced mechanical jamming issues due to rapid key presses; QWERTY was designed to slow typists down and separate common letter pairs to prevent this, and later became a de facto standard for interoperability.
% FOUNDING_PROBLEM_CORROBORATION: Historians of technology and ergonomics researchers widely corroborate that QWERTY's original mechanical justification is dead. Incumbent manufacturers and training institutions, however, often frame its persistence as a matter of user preference or 'good enough' performance, without acknowledging the historical context or active defense.
narrative_ontology:disappearance_verdict(qwerty_persistence__incumbent_preservation_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence__incumbent_preservation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence__incumbent_preservation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(qwerty_persistence__incumbent_preservation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence__incumbent_preservation_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence__incumbent_preservation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qwerty_persistence__incumbent_preservation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qwerty_persistence__incumbent_preservation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.7) reflects the costs borne by users and alternative developers due to QWERTY's suboptimal efficiency and the suppression of innovation. Suppression (0.8) is high due to the active efforts of incumbents to maintain dominance, including marketing, lobbying, and leveraging network effects to make alternatives unviable. The theater ratio (0.4) indicates that while there's a genuine coordination function, a significant portion of the activity around QWERTY's persistence is performative defense rather than pure functional maintenance. The founding problem is 'dead', but the constraint persists due to active defense, not just inertia, making it a Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of QWERTY incumbents, the layout is a stable, universally adopted standard that provides essential coordination. From the perspective of alternative layout advocates and efficiency-seeking users, it is an outdated, actively defended barrier to innovation and ergonomic improvement. The engine's classification as Tangled Rope captures this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   QWERTY keyboard manufacturers, trained typists, and typing training institutions are the primary beneficiaries, gaining from the stability of the standard and the protection of their investments/skills. Alternative keyboard manufacturers, efficiency-seeking users, and alternative layout advocates are the victims, bearing the costs of suppressed innovation, suboptimal efficiency, and high barriers to entry. The active enforcement by incumbents ensures this asymmetric flow.
 *
 * MANDATROPHY ANALYSIS:
 *   The original mandate for QWERTY (preventing mechanical jamming) is long dead. However, the constraint is not a Piton because it is actively defended by identifiable beneficiaries who capture significant value from its persistence. It's a Tangled Rope because it still provides a coordination function (universal interoperability) but is sustained by active extraction and suppression, not just inertia. The 'dead' founding problem combined with 'world_rearranges' disappearance verdict signals a potential zombie constraint, which the Tangled Rope classification captures by highlighting the active extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    incumbent_vs_lapsed_alternatives,
    'Is QWERTY''s persistence primarily due to active incumbent defense (this reading) or the natural failure of alternatives to reach critical mass due to network effects alone (the ''lapsed_alternatives_reading'')?',
    'Historical analysis of specific anti-competitive actions by QWERTY incumbents, or counterfactual modeling of alternative layout adoption in a market without active incumbent defense.',
    'If incumbent defense is the primary driver, this ''incumbent_preservation_reading'' (Tangled Rope) is accurate. If alternatives simply failed to gain traction without active suppression, the ''lapsed_alternatives_reading'' (closer to a Rope or Piton) would be more appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_vs_lapsed_alternatives, conceptual, 'Distinguishing active defense from passive network effects in standard persistence.').

omega_variable(
    identity_lock_strength_for_typists,
    'How strong is the ''identity_locked'' exit option for trained QWERTY typists? Is it primarily a cost barrier or a genuine identity fusion?',
    'Sociological studies on professional identity and skill attachment, or economic analysis of retraining subsidies vs. adoption rates of alternative layouts.',
    'If identity fusion is strong, the effective suppression and extractiveness on typists are higher, as the cost of exit is not merely financial but existential. If it''s purely a cost barrier, the constraint is less deeply embedded in individual agency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_strength_for_typists, empirical, 'Assessing the nature of identity lock-in for QWERTY typists.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence__incumbent_preservation_reading, 1878, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t1878, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 1878, 0.1).
narrative_ontology:measurement(qwer_tr_t1907, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 1907, 0.15).
narrative_ontology:measurement(qwer_tr_t1936, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 1936, 0.25).
narrative_ontology:measurement(qwer_tr_t1965, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 1965, 0.35).
narrative_ontology:measurement(qwer_tr_t1994, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 1994, 0.38).
narrative_ontology:measurement(qwer_tr_t2023, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 2023, 0.4).

% Extraction over time
narrative_ontology:measurement(qwer_be_t1878, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 1878, 0.3).
narrative_ontology:measurement(qwer_be_t1907, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 1907, 0.4).
narrative_ontology:measurement(qwer_be_t1936, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 1936, 0.55).
narrative_ontology:measurement(qwer_be_t1965, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 1965, 0.65).
narrative_ontology:measurement(qwer_be_t1994, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 1994, 0.68).
narrative_ontology:measurement(qwer_be_t2023, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 2023, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t1878, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 1878, 0.4).
narrative_ontology:measurement(qwer_su_t1907, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 1907, 0.55).
narrative_ontology:measurement(qwer_su_t1936, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 1936, 0.7).
narrative_ontology:measurement(qwer_su_t1965, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 1965, 0.75).
narrative_ontology:measurement(qwer_su_t1994, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 1994, 0.78).
narrative_ontology:measurement(qwer_su_t2023, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 2023, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence__incumbent_preservation_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence__incumbent_preservation_reading, digital_literacy_standards).
narrative_ontology:affects_constraint(qwerty_persistence__incumbent_preservation_reading, ergonomic_keyboard_market).

% DUAL FORMULATION NOTE:
% The 'qwerty_persistence' kernel decomposes into two primary readings: 'incumbent_preservation_reading' (this constraint), which emphasizes active defense and extraction, and 'lapsed_alternatives_reading', which focuses on the natural failure of alternatives to achieve critical mass. Both are necessary to fully understand QWERTY's persistence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
