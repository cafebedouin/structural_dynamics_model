% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__hanafi_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__hanafi_reading, []).

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
 *   constraint_id: jurisprudential_method_kernel__hanafi_reading
 *   human_readable: Hanafi Rationalist Jurisprudential Method (Qiyas/Istihsan)
 *   domain: religious/legal/institutional
 *
 * SUMMARY:
 *   The Hanafi reading of the jurisprudential_method_kernel instantiates a
 *   constraint where divine law is known through extensive analogical
 *   reasoning (qiyas) and juristic preference (istihsan), treating reason as
 *   a legitimate tool for extending divine intent to novel cases. This
 *   reading became the dominant legal method of the Abbasid and Ottoman
 *   empires, coordinating legal production across vast territories while
 *   displacing textualist alternatives. The constraint claims to be a rope
 *   (coordination for legal continuity) but operates with substantial
 *   extraction (epistemic authority transferred to rationalist jurists and
 *   imperial administrators) and active enforcement (state appointment of
 *   Hanafi qadis, institutionalization in madrasas).
 *
 * KEY AGENTS:
 *   - rationalist_jurists: Primary beneficiaries and agenda-setters (institutional/identity_locked) — their professional identity fuses with the method
 *   - imperial_administrators: Instrumental beneficiaries (institutional/arbitrage) — use method for state capacity
 *   - merchant_communities: Coordination beneficiaries (organized/mobile) — gain legal tools for commerce
 *   - textualist_scholars: Primary victims and excluded (organized/identity_locked) — epistemic authority displaced
 *   - traditionalist_ulema: Victims (moderate/constrained) — lose institutional ground but retain local authority
 *   - analytical_observer: Sees full structure (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__hanafi_reading, 0.68).
domain_priors:suppression_score(jurisprudential_method_kernel__hanafi_reading, 0.42).
domain_priors:theater_ratio(jurisprudential_method_kernel__hanafi_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__hanafi_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__hanafi_reading, "Hanafi Rationalist Jurisprudential Method (Qiyas/Istihsan)").
narrative_ontology:topic_domain(jurisprudential_method_kernel__hanafi_reading, "religious/legal/institutional").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__hanafi_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__hanafi_reading, '22a753d7-86c7-47c0-913e-ee32d74b1e90').
narrative_ontology:cs_kernel_codification('22a753d7-86c7-47c0-913e-ee32d74b1e90', fixed_text).
narrative_ontology:cs_authority_grounding('22a753d7-86c7-47c0-913e-ee32d74b1e90', lineage).
narrative_ontology:cs_interpretation_layer_present('22a753d7-86c7-47c0-913e-ee32d74b1e90').
narrative_ontology:cs_reading_relation('22a753d7-86c7-47c0-913e-ee32d74b1e90', jurisprudential_method_kernel__hanbali_reading, forecloses).
narrative_ontology:cs_reading_relation('22a753d7-86c7-47c0-913e-ee32d74b1e90', jurisprudential_method_kernel__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('22a753d7-86c7-47c0-913e-ee32d74b1e90', jurisprudential_method_kernel__shafii_reading, influences).
narrative_ontology:cs_axiom('22a753d7-86c7-47c0-913e-ee32d74b1e90', foundational, reason_extends_divine_intent).
narrative_ontology:cs_axiom_status(reason_extends_divine_intent, holdable).
narrative_ontology:cs_axiom_grounding('22a753d7-86c7-47c0-913e-ee32d74b1e90', reason_extends_divine_intent, deontological).
narrative_ontology:cs_axiom('22a753d7-86c7-47c0-913e-ee32d74b1e90', foundational, istihsan_resolves_qiyas_hard_cases).
narrative_ontology:cs_axiom_status(istihsan_resolves_qiyas_hard_cases, holdable).
narrative_ontology:cs_axiom_grounding('22a753d7-86c7-47c0-913e-ee32d74b1e90', istihsan_resolves_qiyas_hard_cases, instrumental).
narrative_ontology:cs_axiom('22a753d7-86c7-47c0-913e-ee32d74b1e90', secondary, maslaha_informs_legal_extension).
narrative_ontology:cs_axiom_status(maslaha_informs_legal_extension, holdable).
narrative_ontology:cs_axiom_grounding('22a753d7-86c7-47c0-913e-ee32d74b1e90', maslaha_informs_legal_extension, instrumental).
narrative_ontology:cs_reference_frame('22a753d7-86c7-47c0-913e-ee32d74b1e90', classical_hanafi_usul).
narrative_ontology:cs_drift_state('22a753d7-86c7-47c0-913e-ee32d74b1e90', ottoman_codification_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('22a753d7-86c7-47c0-913e-ee32d74b1e90', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanafi_reading, rationalist_jurists).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanafi_reading, imperial_administrators).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanafi_reading, merchant_communities).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanafi_reading, textualist_scholars).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanafi_reading, traditionalist_ulema).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and authorize legal rulings through analogical extension and juristic preference, gaining professional authority and institutional positions in qadi courts and madrasas. Their identity is fused with the rationalist method — abandoning qiyas/istihsan would dissolve their epistemic claim to interpret divine law. Exit requires rejecting their own intellectual formation.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, rationalist_jurists, beneficiary,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__hanafi_reading, rationalist_jurists, agenda_setter).

% Use the flexible Hanafi method to generate legal rules for novel administrative, commercial, and fiscal problems across a vast empire. The method's adaptability serves state capacity. They can shift to other madhhabs or secular codes if politically expedient — their attachment is instrumental, not identity-forming.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, imperial_administrators, beneficiary,
    institutional, biographical, arbitrage, continental).

% Benefit from legal instruments (hawala, commenda, flexible contract doctrine) that the rationalist method enables for long-distance trade. They participate in the constraint's coordination function voluntarily and can operate under alternative legal frameworks in different jurisdictions.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, merchant_communities, beneficiary,
    organized, biographical, mobile, continental).

% Hold that law derives only from literal text and confirmed consensus; analogical extension is human invention masquerading as divine law. Their epistemic authority is displaced when rationalist rulings become state-enforced orthodoxy. Exit means abandoning their textualist commitment — which is their identity as scholars of the revealed text.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, textualist_scholars, payer,
    organized, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__hanafi_reading, textualist_scholars, excluded).

% Transmit and apply hadith and Companion opinions through established chains; see istihsan as subjective preference overriding transmitted evidence. They lose institutional ground to rationalist jurists in imperial appointments but retain local authority in teaching and fatwa. Exit is constrained by their investment in traditional transmission networks.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, traditionalist_ulema, payer,
    moderate, generational, constrained, regional).

% Studies the jurisprudential method as a historical-institutional phenomenon: how a method of legal reasoning becomes a constraint that coordinates legal production while extracting epistemic authority from textualist alternatives and distributing it to rationalist practitioners.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a systematic method for extending divine law to novel cases (new commercial instruments, administrative problems, social conditions) without claiming new revelation — solves the coordination problem of legal continuity amid civilizational change.
% TRANSFER_FUNCTION: Moves interpretive authority and institutional position from textualist transmitters (who hold authority through chain-of-transmission fidelity) to rationalist jurists (who hold authority through methodological competence in qiyas/istihsan). The transfer is epistemic and institutional, not primarily financial.
% ABSENT_VOICES: Lay believers who experience the law as opaque juristic opinion rather than accessible divine text; women and marginalized groups whose interests are mediated through juristic preferences they cannot contest; early formative-period jurists whose diversity was retroactively collapsed into madhhab orthodoxy.
% DISAPPEARANCE_RATIONALE: If the Hanafi rationalist method vanished overnight, the legal framework for vast swathes of Islamic commercial, administrative, and family law would lose its generative logic. Courts would revert to stricter textualist methods or fragment into ad hoc rulings. The Ottoman codification (Majalla) and modern civil codes in Muslim-majority states draw directly on this method's conceptual architecture.
% FOUNDING_PROBLEM: How to derive binding law for novel cases (new transaction types, imperial administration, urban conditions) from fixed revelation without claiming new revelation or abandoning divine authority.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the rationalist jurists themselves (Abu Hanifa, Abu Yusuf, al-Shaybani) and by imperial administrators who commissioned legal solutions. Textualist scholars (Malik, Ahmad ibn Hanbal, al-Shafi'i in his critique of ra'y) corroborate that the problem existed but dispute the rationalist solution's legitimacy — they attest the problem is real while contesting the method.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__hanafi_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__hanafi_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__hanafi_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(jurisprudential_method_kernel__hanafi_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__hanafi_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__hanafi_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__hanafi_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jurisprudential_method_kernel__hanafi_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the method systematically transfers interpretive authority from textual fidelity to methodological competence — a zero-sum epistemic transfer. Suppression (0.42) is moderate: the constraint does not physically coerce textualists but excludes them from imperial office and mainstream institutional recognition. Theater ratio (0.31) reflects that the coordination function (solving novel cases) is real but increasingly performed through routinized doctrine rather than fresh reasoning. The measurement series shows extraction and theater rising as the method becomes imperial orthodoxy (150-1100 AH), while suppression stabilizes once institutional dominance is secured.
 *
 * PERSPECTIVAL GAP:
 *   From the rationalist jurist seat, the constraint is genuine coordination: reason extends revelation to serve justice in novel cases. From the textualist scholar seat, the same structure is extraction: human preference masquerades as divine law, enforced by state power. The engine computes this divergence from the structural data — the authored claimed_type (tangled_rope) captures the dual reality that both seats experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Rationalist jurists are structural beneficiaries (d ≈ 0.15) — the constraint subsidizes their epistemic authority and institutional position. Imperial administrators are near-symmetric beneficiaries (d ≈ 0.35) — they gain administrative capacity but must maintain the institutional machinery. Merchants are moderate beneficiaries (d ≈ 0.4) — they gain legal tools but have mobile exit. Textualist scholars are full targets (d ≈ 0.85) — their epistemic claim is actively displaced, and their identity_locked exit makes the extraction severe. Traditionalist ulema are constrained targets (d ≈ 0.7) — they lose ground but retain local transmission networks.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (deriving law for novel cases without new revelation) remains live — civilizational novelty continues. But the method's extraction component (displacing textualist authority, serving imperial administration) has grown beyond the coordination minimum. The constraint is not a resolved mandatrophy (the problem persists) but shows mandatrophy symptoms: the coordination function could be served with less epistemic displacement, yet the extraction persists through institutional inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the Hanafi reading a distinct constraint instantiated from the jurisprudential_method_kernel, or merely a variant interpretation of a single constraint?',
    'Compare ε-invariance across readings: if measuring the constraint via Hanafi criteria (qiyas/istihsan legitimacy) yields ε=0.68 but Hanbali criteria (textual fidelity) yields ε≈0.1, they are different constraints with different referents, not one constraint measured differently.',
    'If distinct constraints, each reading gets its own classification and the kernel is a family label. If one constraint, the ε-variance violates DP-001 and the story must be re-authored.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether this reading instantiates a separate ε-invariant constraint per DP-001.').

omega_variable(
    analogical_extension_boundary,
    'Where does legitimate analogical extension (qiyas) end and unrestrained juristic preference (istihsan) begin? Is the boundary itself a coordination mechanism or an extraction mechanism?',
    'Historical analysis of disputed rulings where Hanafi jurists invoked istihsan against qiyas-analogy — did the preference solve a genuine coordination failure or serve a beneficiary interest?',
    'If the boundary is extractive (serves jurist/administrator interests), the constraint is more snare-like. If coordinative (solves genuine indeterminacy), it remains tangled_rope. Affects claimed_type stability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(analogical_extension_boundary, empirical, 'Whether the qiyas/istihsan boundary is a genuine coordination tool or an extraction cover.').

omega_variable(
    state_enforcement_dependency,
    'How far does the constraint''s persistence depend on imperial/state enforcement versus voluntary scholarly convergence?',
    'Compare Hanafi dominance in state-appointed qadi positions vs. its penetration in voluntary fatwa markets and madrasa curricula across regions and eras.',
    'If state enforcement is primary, suppression is higher and the constraint trends toward snare. If scholarly convergence is primary, it trends toward rope. The measurement series (150-1100 AH) captures the state-enforcement intensification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_enforcement_dependency, empirical, 'State enforcement vs. scholarly convergence as the constraint''s persistence engine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__hanafi_reading, 150, 1100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t150, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 150, 0.18).
narrative_ontology:measurement(juri_tr_t300, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 300, 0.22).
narrative_ontology:measurement(juri_tr_t500, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 500, 0.26).
narrative_ontology:measurement(juri_tr_t700, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 700, 0.29).
narrative_ontology:measurement(juri_tr_t900, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 900, 0.3).
narrative_ontology:measurement(juri_tr_t1100, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 1100, 0.31).

% Extraction over time
narrative_ontology:measurement(juri_be_t150, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 150, 0.35).
narrative_ontology:measurement(juri_be_t300, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 300, 0.48).
narrative_ontology:measurement(juri_be_t500, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 500, 0.55).
narrative_ontology:measurement(juri_be_t700, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 700, 0.62).
narrative_ontology:measurement(juri_be_t900, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 900, 0.65).
narrative_ontology:measurement(juri_be_t1100, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 1100, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t150, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 150, 0.25).
narrative_ontology:measurement(juri_su_t300, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 300, 0.32).
narrative_ontology:measurement(juri_su_t500, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 500, 0.38).
narrative_ontology:measurement(juri_su_t700, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 700, 0.4).
narrative_ontology:measurement(juri_su_t900, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 900, 0.41).
narrative_ontology:measurement(juri_su_t1100, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 1100, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__hanafi_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jurisprudential_method_kernel__hanafi_reading, 0.1).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel__maliki_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel__shafii_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel__hanbali_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, ottoman_legal_codification).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, modern_islamic_family_law).

% DUAL FORMULATION NOTE:
% The jurisprudential_method_kernel decomposes into four constraint stories (one per madhhab reading), each with distinct ε, beneficiaries, victims, and claimed_type. The Hanafi reading has the highest ε on novel cases (0.68) due to its expansive qiyas/istihsan; the Hanbali reading has the lowest (ε≈0.1) but highest suppression of internal dissent. They are linked as a constraint family through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jurisprudential_method_kernel__hanafi_reading, institutional, 0.15).
constraint_indexing:directionality_override(jurisprudential_method_kernel__hanafi_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
