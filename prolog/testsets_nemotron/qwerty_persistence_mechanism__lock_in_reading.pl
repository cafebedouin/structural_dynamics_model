% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_mechanism__lock_in_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_mechanism__lock_in_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: qwerty_persistence_mechanism__lock_in_reading
 *   human_readable: QWERTY Keyboard Layout Persistence via Coordination Failure
 *   domain: economic/technological/social
 *
 * SUMMARY:
 *   QWERTY is the textbook case of path-dependent lock-in: a layout designed
 *   for 1870s mechanical constraints persists 150 years later despite
 *   measurable ergonomic inferiority. The lock_in_reading holds that no
 *   individual or group benefits from QWERTY's superiority — rather, the
 *   collective failed to coordinate on a better alternative once the founding
 *   problem (mechanical jams) vanished. The constraint is a genuine
 *   coordination mechanism (universal interoperability) that has outlived its
 *   technical justification. Extraction is low but nonzero: new learners pay
 *   ergonomic costs they would not pay under a superior layout, but no
 *   identifiable agent collects these costs as rents. The beneficiaries are
 *   the incumbent coordination equilibrium itself — the installed base, the
 *   training infrastructure, the software ecosystem — not a rent-seeking
 *   coalition. This distinguishes it from the beneficiary_extraction_reading,
 *   which posits active maintenance by incumbents.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_mechanism__lock_in_reading, 0.22).
domain_priors:suppression_score(qwerty_persistence_mechanism__lock_in_reading, 0.15).
domain_priors:theater_ratio(qwerty_persistence_mechanism__lock_in_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_mechanism__lock_in_reading, rope).
narrative_ontology:human_readable(qwerty_persistence_mechanism__lock_in_reading, "QWERTY Keyboard Layout Persistence via Coordination Failure").
narrative_ontology:topic_domain(qwerty_persistence_mechanism__lock_in_reading, "economic/technological/social").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_mechanism__lock_in_reading, '818aaa7c-35dd-4bd8-a794-0534672ad4c7').
narrative_ontology:cs_kernel_codification('818aaa7c-35dd-4bd8-a794-0534672ad4c7', distributed).
narrative_ontology:cs_authority_grounding('818aaa7c-35dd-4bd8-a794-0534672ad4c7', practice).
narrative_ontology:cs_reading_relation('818aaa7c-35dd-4bd8-a794-0534672ad4c7', qwerty_persistence_mechanism__naturalization_reading, coexists_with).
narrative_ontology:cs_reading_relation('818aaa7c-35dd-4bd8-a794-0534672ad4c7', qwerty_persistence_mechanism__beneficiary_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('818aaa7c-35dd-4bd8-a794-0534672ad4c7', foundational, path_dependence_without_extraction).
narrative_ontology:cs_axiom_status(path_dependence_without_extraction, holdable).
narrative_ontology:cs_axiom_grounding('818aaa7c-35dd-4bd8-a794-0534672ad4c7', path_dependence_without_extraction, empirically_contingent).
narrative_ontology:cs_axiom('818aaa7c-35dd-4bd8-a794-0534672ad4c7', secondary, founding_problem_dead_but_equilibrium_persists).
narrative_ontology:cs_axiom_status(founding_problem_dead_but_equilibrium_persists, holdable).
narrative_ontology:cs_axiom_grounding('818aaa7c-35dd-4bd8-a794-0534672ad4c7', founding_problem_dead_but_equilibrium_persists, empirically_contingent).
narrative_ontology:cs_reference_frame('818aaa7c-35dd-4bd8-a794-0534672ad4c7', mechanical_typewriter_era_coordination).
narrative_ontology:cs_drift_state('818aaa7c-35dd-4bd8-a794-0534672ad4c7', digital_keyboard_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('818aaa7c-35dd-4bd8-a794-0534672ad4c7', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_mechanism__lock_in_reading, qwerty_persistence_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__lock_in_reading, existing_typist_population).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__lock_in_reading, training_infrastructure_providers).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__lock_in_reading, software_ecosystem_built_on_qwerty).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__lock_in_reading, new_learners).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__lock_in_reading, ergonomic_stakeholders).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__lock_in_reading, alternative_layout_developers).
narrative_ontology:constraint_vindicates(qwerty_persistence_mechanism__lock_in_reading, path_dependence_in_technology_adoption).
narrative_ontology:constraint_vindicates(qwerty_persistence_mechanism__lock_in_reading, coordination_externalities_in_standards).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hundreds of millions of people have invested thousands of hours mastering QWERTY touch-typing. Their skill capital is entirely layout-specific. Switching requires months of retraining with temporary productivity loss. They benefit from QWERTY's ubiquity — any keyboard works, any device is compatible — but are locked in by their own embodied expertise.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, existing_typist_population, beneficiary,
    organized, biographical, identity_locked, global).

% Typing tutors, certification programs, keyboard manufacturers, and educational curricula are built around QWERTY. They collect recurring revenue from teaching and certifying the standard layout. Switching would require rebuilding their entire product stack. They benefit from the standard's stability but face switching costs if alternatives gain traction.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, training_infrastructure_providers, beneficiary,
    moderate, biographical, constrained, global).

% Operating systems, applications, games, and web interfaces all assume QWERTY for default keybindings, shortcuts, and text entry. The ecosystem is too vast to coordinate a switch. They benefit from a single stable target but could adapt if a new standard emerged — they have high technical capacity but no individual incentive to lead.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, software_ecosystem_built_on_qwerty, beneficiary,
    institutional, generational, mobile, global).

% Children and new computer users must learn QWERTY because it is the only universally supported layout. They bear the ergonomic costs (higher finger travel, same-finger bigrams) without having chosen the layout. Their exit option is learning an alternative layout (Dvorak, Colemak) but then facing incompatibility with shared devices, school equipment, and workplace standards.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, new_learners, payer,
    powerless, biographical, constrained, global).

% Repetitive strain injury sufferers, occupational health researchers, and ergonomic equipment makers. QWERTY's layout was designed to prevent mechanical typewriter jams, not human biomechanics. They advocate for alternatives but face the full coordination barrier — any ergonomic keyboard must still speak QWERTY to the computer, limiting design freedom.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, ergonomic_stakeholders, payer,
    moderate, biographical, constrained, global).

% Designers of Dvorak, Colemak, Workman, and other layouts. Their layouts demonstrate measurable ergonomic and efficiency advantages in controlled studies. They cannot gain adoption because the coordination problem is insurmountable — no individual user can switch without losing interoperability. They are structurally excluded from the market by the network effect, not by active suppression.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, alternative_layout_developers, excluded,
    powerless, biographical, trapped, global).

% Economic historians, technology studies scholars, and standards bodies who study QWERTY as the canonical case of path-dependent lock-in. They analyze the coordination failure but have no stake in the outcome. Their role is documenting and theorizing the mechanism, not advocating for a particular layout.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, standards_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single universal keyboard layout that any typist can use on any device without negotiation or translation. Solves the coordination problem of interoperable text entry across manufacturers, platforms, and generations.
% TRANSFER_FUNCTION: Transfers learning investment and ergonomic cost from the existing typist population (who already paid the learning cost) to new learners (who must pay it). The coordination benefit (universal compatibility) is shared; the ergonomic penalty is borne disproportionately by those who have not yet learned.
% ABSENT_VOICES: Future generations of learners who will inherit the ergonomic penalty without having participated in the original coordination settlement. Also absent: the counterfactual world where a superior layout won the early market — those potential beneficiaries of a better standard never get to speak.
% DISAPPEARANCE_RATIONALE: If QWERTY vanished overnight, the world would not revert to a natural state — it would face a massive coordination crisis. Billions of devices, billions of trained users, and the entire software ecosystem would need a new standard. Competing layouts would battle for dominance; the transition would take years and impose enormous retraining costs. The world rearranges because arrangements depend on the constraint.
% FOUNDING_PROBLEM: Early typewriter manufacturers needed a layout that prevented mechanical key jams at high typing speeds. QWERTY separated common letter pairs mechanically. The coordination problem was: how to get typists, manufacturers, and trainers to converge on one layout so the industry could scale.
% FOUNDING_PROBLEM_CORROBORATION: The mechanical jam problem is objectively dead — modern keyboards have no mechanical linkages. Typewriter historians and mechanical engineers corroborate this. The layout persists solely because the coordination equilibrium it created became self-sustaining after the founding problem disappeared.
narrative_ontology:disappearance_verdict(qwerty_persistence_mechanism__lock_in_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_mechanism__lock_in_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_mechanism__lock_in_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(qwerty_persistence_mechanism__lock_in_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_mechanism__lock_in_reading, 0.22, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_mechanism__lock_in_reading_tests).
:- end_tests(qwerty_persistence_mechanism__lock_in_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.22) reflects the diffuse ergonomic penalty on new learners and the innovation suppression on alternative layouts — real costs, but no centralized extraction. Suppression (0.15) is low because alternatives are legally and technically permitted; the barrier is coordination, not coercion. Theater ratio (0.08) is minimal — the constraint performs its coordination function authentically, with little performative maintenance. Accessibility collapse (0.72) is high: once you understand QWERTY's ubiquity, alternatives appear practically inaccessible for mainstream use. Resistance (0.35) is moderate: ergonomic advocates and alternative layout communities exist but cannot overcome the coordination barrier. The claimed type is rope because the constraint solves a genuine coordination problem (universal text entry) with minimal coercive overhead; participants are net beneficiaries of interoperability; alternatives are not suppressed.
 *
 * DIRECTIONALITY LOGIC:
 *   Existing typists are beneficiaries with identity_locked exit — their embodied skill makes exit psychologically and practically prohibitive. Training infrastructure and software ecosystem are beneficiaries with constrained to mobile exit — they could adapt but have no incentive to lead. New learners and ergonomic stakeholders are payers with constrained exit — they can learn alternatives but face interoperability penalties. Alternative layout developers are excluded — trapped by the network effect they cannot overcome. Standards observers are analytical with analytical exit. The engine will derive directionality from these structural positions: beneficiaries near d=0, payers near d=1, excluded near d=1, observers at d=0.5.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (mechanical jam prevention) is dead. The arrangement persists because the coordination equilibrium it created is self-sustaining — no mandate exists to revoke, no authority maintains it. This is not mandatrophy (a mandate outliving its function) but rather a coordination equilibrium that became autonomous after its founding problem vanished. The constraint is a rope that solved a real problem, then the problem disappeared but the solution persisted because it became the coordination standard. No extraction coalition maintains it; no sunset clause could apply because no authority administers it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturalization_vs_lockin_boundary,
    'Is QWERTY''s ergonomic inferiority large enough to constitute a genuine coordination failure, or is it within the range where ''good enough'' naturalization is the better description?',
    'Controlled longitudinal studies comparing QWERTY and alternative layouts on naive learners, measuring time-to-proficiency, error rates, and long-term ergonomic outcomes. If the advantage is small (<10%), naturalization_reading gains ground; if large (>25%), lock_in_reading is strengthened.',
    'If QWERTY is genuinely close to optimal, the constraint approaches mountain (emergent adequacy) rather than rope (coordination failure). If substantially inferior, the coordination failure interpretation is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalization_vs_lockin_boundary, empirical, 'Whether the performance gap constitutes coordination failure or naturalized adequacy.').

omega_variable(
    extraction_coalition_existence,
    'Did typewriter/computer manufacturers ever actively suppress alternative layouts to protect QWERTY investments, or was the lock-in purely passive?',
    'Historical archive research: corporate correspondence, patent suppression evidence, lobbying records, exclusive dealing contracts. The 1930s-1950s transition from typewriters to computers is the critical window.',
    'If active suppression occurred, the beneficiary_extraction_reading gains structural support and this reading''s claim of ''no extraction coalition'' is falsified. The constraint would shift toward tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_coalition_existence, empirical, 'Whether an active extraction coalition existed versus purely passive coordination failure.').

omega_variable(
    kernel_reading_ambiguity,
    'Does the qwerty_persistence_mechanism kernel admit a single correct reading, or are the three readings (lock_in, naturalization, beneficiary_extraction) structurally compatible partial explanations operating at different levels?',
    'Meta-analysis of the scholarly literature: do proponents of each reading treat them as mutually exclusive hypotheses, or as complementary mechanisms? Citation network analysis of the path dependence literature.',
    'If readings are complementary, the kernel decomposition into separate constraints is analytically misleading — they are facets of one constraint. If mutually exclusive, the decomposition is warranted and each constraint story measures a distinct structural claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the kernel''s sibling readings are mutually exclusive or complementary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_mechanism__lock_in_reading, 1873, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwerty_lock_in_tr_t1873, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 1873, 0.02).
narrative_ontology:measurement(qwerty_lock_in_tr_t1900, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 1900, 0.03).
narrative_ontology:measurement(qwerty_lock_in_tr_t1930, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 1930, 0.05).
narrative_ontology:measurement(qwerty_lock_in_tr_t1960, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 1960, 0.06).
narrative_ontology:measurement(qwerty_lock_in_tr_t1990, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 1990, 0.07).
narrative_ontology:measurement(qwerty_lock_in_tr_t2010, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 2010, 0.08).
narrative_ontology:measurement(qwerty_lock_in_tr_t2024, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 2024, 0.08).

% Extraction over time
narrative_ontology:measurement(qwerty_lock_in_be_t1873, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 1873, 0.05).
narrative_ontology:measurement(qwerty_lock_in_be_t1900, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 1900, 0.12).
narrative_ontology:measurement(qwerty_lock_in_be_t1930, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 1930, 0.18).
narrative_ontology:measurement(qwerty_lock_in_be_t1960, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 1960, 0.2).
narrative_ontology:measurement(qwerty_lock_in_be_t1990, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 1990, 0.21).
narrative_ontology:measurement(qwerty_lock_in_be_t2010, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 2010, 0.22).
narrative_ontology:measurement(qwerty_lock_in_be_t2024, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 2024, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(qwerty_lock_in_su_t1873, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 1873, 0.08).
narrative_ontology:measurement(qwerty_lock_in_su_t1900, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 1900, 0.1).
narrative_ontology:measurement(qwerty_lock_in_su_t1930, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 1930, 0.12).
narrative_ontology:measurement(qwerty_lock_in_su_t1960, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 1960, 0.14).
narrative_ontology:measurement(qwerty_lock_in_su_t1990, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 1990, 0.15).
narrative_ontology:measurement(qwerty_lock_in_su_t2010, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 2010, 0.15).
narrative_ontology:measurement(qwerty_lock_in_su_t2024, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 2024, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_mechanism__lock_in_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__lock_in_reading, qwerty_persistence_mechanism__naturalization_reading).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__lock_in_reading, qwerty_persistence_mechanism__beneficiary_extraction_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the single colloquial claim 'QWERTY persists' into three structurally distinct constraints with different ε values, different beneficiary/victim structures, and different types. The lock_in_reading has low ε (0.22), diffuse beneficiaries, no active extraction, and classifies as rope. The naturalization_reading would have near-zero ε and claim mountain. The beneficiary_extraction_reading would have higher ε, identifiable corporate beneficiaries, and classify as tangled_rope or snare. All three stories share the same referent (QWERTY's persistence) but differ in causal structure — exactly the ε-invariance principle at work.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
