% ============================================================================
% CONSTRAINT STORY: permissive_license_text__commons_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_permissive_license_text__commons_coordination_reading, []).

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
 *   constraint_id: permissive_license_text__commons_coordination_reading
 *   human_readable: Permissive License Text — Commons Coordination Reading
 *   domain: technology_governance/intellectual_property/software_licensing
 *
 * SUMMARY:
 *   This constraint story models the permissive license text (MIT, BSD,
 *   Apache-2.0 style) under the commons coordination reading: the license
 *   functions as a low-friction coordination mechanism that enables a
 *   universal pool of implementers to build, share, and recombine software
 *   without negotiating pairwise permissions. The license text itself is the
 *   constraint — a fixed legal artifact that reduces transaction costs to
 *   near-zero for downstream use. Beneficiaries are universal_implementers
 *   (any party that uses, modifies, distributes the code), license_authors
 *   (who gain adoption and reputation), and corporate_adopters (who integrate
 *   permissively-licensed code into products). There is no victim set in this
 *   reading — the coordination is symmetric and non-extractive. The claimed
 *   type is rope: pure coordination with minimal coercive overhead. The
 *   engine computes per-seat types from structural data; this reading's low
 *   epsilon and absence of victims should yield rope from all seats.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(permissive_license_text__commons_coordination_reading, 0.08).
domain_priors:suppression_score(permissive_license_text__commons_coordination_reading, 0.12).
domain_priors:theater_ratio(permissive_license_text__commons_coordination_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(permissive_license_text__commons_coordination_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(permissive_license_text__commons_coordination_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(permissive_license_text__commons_coordination_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(permissive_license_text__commons_coordination_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(permissive_license_text__commons_coordination_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(permissive_license_text__commons_coordination_reading, rope).
narrative_ontology:human_readable(permissive_license_text__commons_coordination_reading, "Permissive License Text — Commons Coordination Reading").
narrative_ontology:topic_domain(permissive_license_text__commons_coordination_reading, "technology_governance/intellectual_property/software_licensing").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(permissive_license_text__commons_coordination_reading, '6b04705e-b943-446f-b16a-8b90349a0c28').
narrative_ontology:cs_kernel_codification('6b04705e-b943-446f-b16a-8b90349a0c28', fixed_text).
narrative_ontology:cs_authority_grounding('6b04705e-b943-446f-b16a-8b90349a0c28', practice).
narrative_ontology:cs_interpretation_layer_present('6b04705e-b943-446f-b16a-8b90349a0c28').
narrative_ontology:cs_reading_relation('6b04705e-b943-446f-b16a-8b90349a0c28', permissive_license_text__corporate_moat_reading, coexists_with).
narrative_ontology:cs_reading_relation('6b04705e-b943-446f-b16a-8b90349a0c28', permissive_license_text__copyleft_counterfactual_reading, coexists_with).
narrative_ontology:cs_axiom('6b04705e-b943-446f-b16a-8b90349a0c28', foundational, minimal_legal_friction_enables_coordination).
narrative_ontology:cs_axiom_status(minimal_legal_friction_enables_coordination, holdable).
narrative_ontology:cs_axiom_grounding('6b04705e-b943-446f-b16a-8b90349a0c28', minimal_legal_friction_enables_coordination, empirically_contingent).
narrative_ontology:cs_axiom('6b04705e-b943-446f-b16a-8b90349a0c28', foundational, universal_implementation_freedom_maximizes_commons).
narrative_ontology:cs_axiom_status(universal_implementation_freedom_maximizes_commons, holdable).
narrative_ontology:cs_axiom_grounding('6b04705e-b943-446f-b16a-8b90349a0c28', universal_implementation_freedom_maximizes_commons, deontological).
narrative_ontology:cs_reference_frame('6b04705e-b943-446f-b16a-8b90349a0c28', permissive_license_original_intent).
narrative_ontology:cs_drift_state('6b04705e-b943-446f-b16a-8b90349a0c28', contemporary_corporate_adoption_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6b04705e-b943-446f-b16a-8b90349a0c28', '2026-08-04T12:00:00Z').
narrative_ontology:cs_kernel_id(permissive_license_text__commons_coordination_reading, permissive_license_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(permissive_license_text__commons_coordination_reading, universal_implementers).
narrative_ontology:constraint_beneficiary(permissive_license_text__commons_coordination_reading, license_authors).
narrative_ontology:constraint_beneficiary(permissive_license_text__commons_coordination_reading, corporate_adopters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(permissive_license_text__commons_coordination_reading, corporate_adopters).
narrative_ontology:constraint_vindicates(permissive_license_text__commons_coordination_reading, minimal_legal_friction_enables_coordination).
narrative_ontology:constraint_vindicates(permissive_license_text__commons_coordination_reading, universal_implementation_freedom_maximizes_commons).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Any developer, researcher, hobbyist, or organization that uses, modifies, or distributes permissively-licensed code. They gain immediate permission to build on existing work without legal negotiation. Exit is trivial — they can switch to other libraries, write their own code, or adopt copyleft alternatives at any time. The license text imposes only attribution preservation.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, universal_implementers, beneficiary,
    moderate, biographical, arbitrage, global).

% Original authors or maintainers who choose a permissive license for their project. They set the coordination terms by selecting the license text. They benefit from adoption, reputation, and ecosystem growth. They can relicense future versions (but not retroactively revoke permissions already granted). Exit means choosing a different license for new work.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, license_authors, agenda_setter,
    organized, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(permissive_license_text__commons_coordination_reading, license_authors, beneficiary).

% Companies that integrate permissively-licensed code into commercial products and services. They gain massive R&D leverage — free access to battle-tested infrastructure (compilers, runtimes, libraries, frameworks). They often contribute patches, funding, and developer time back to upstream projects. Their exit option is maintaining internal forks or switching dependencies; both are costly but feasible (arbitrage-grade). They bear some coordination cost (tracking upstream, contributing back) but are net beneficiaries.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, corporate_adopters, beneficiary,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(permissive_license_text__commons_coordination_reading, corporate_adopters, payer).

% Developers and organizations who believe permissive licenses enable exploitation and advocate for viral reciprocity (GPL family). They are structurally excluded from the permissive license's coordination mechanism — they would object to the absence of reciprocity requirements but cannot participate in the permissive commons without accepting its terms. Their exit is constrained: they can write copyleft code but cannot easily interoperate with permissive ecosystems without license compatibility friction.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, copyleft_advocates, excluded,
    organized, generational, constrained, global).

% Academics and practitioners who analyze license terms, compliance patterns, and ecosystem effects. They neither collect from nor pay into the constraint. They observe the structural dynamics across all three readings and provide the analytical seat for classification.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(permissive_license_text__commons_coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(permissive_license_text__commons_coordination_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized, zero-negotiation legal framework for software reuse: any party can use, modify, and distribute code without seeking permission, paying royalties, or negotiating terms. Solves the transaction cost problem of pairwise licensing at internet scale.
% TRANSFER_FUNCTION: Moves legal permission (not money) from license authors to universal implementers. Authors grant broad permissions upfront; implementers receive them automatically. No value transfer occurs at use-time — the transfer happened at license selection.
% ABSENT_VOICES: Individual contributors whose patches are absorbed into proprietary products without reciprocity; end-users who lose modification freedom when permissive code is embedded in closed devices; communities that lose governance leverage when corporate adopters dominate project direction. These voices are excluded by the license's permissive structure — they would object to the absence of reciprocity and anti-tivoization provisions but have no standing in the permissive framework.
% DISAPPEARANCE_RATIONALE: If permissive license texts vanished overnight, the universal implementer pool would lose its zero-friction coordination layer. Projects would need to negotiate individual licenses, adopt copyleft defaults, or build from scratch. The software commons would fragment into bilateral agreements or reciprocal-only zones, drastically reducing recombination velocity. Corporate adopters would face massive legal overhead. The world rearranges because the constraint is the coordination infrastructure itself.
% FOUNDING_PROBLEM: Early software sharing (1980s) was blocked by default-copyright-all-rights-reserved: every reuse required explicit permission, making collaboration slow and legally risky. The founding problem was minimizing legal friction to enable internet-scale collaborative software development.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the Free Software Foundation's early history (GPL created because default copyright blocked sharing), the Open Source Initiative's founding documents (explicitly framing permissive licenses as friction-reducers), and independent legal scholarship (e.g., Lemley 'Property, Intellectual Property, and Free Riding' 2005). These sources are outside the direct beneficiary set of any single permissive-licensed project.
narrative_ontology:disappearance_verdict(permissive_license_text__commons_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(permissive_license_text__commons_coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(permissive_license_text__commons_coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(permissive_license_text__commons_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(permissive_license_text__commons_coordination_reading, 0.08, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(permissive_license_text__commons_coordination_reading_tests).
:- end_tests(permissive_license_text__commons_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.08) because the license imposes near-zero cost on implementers — attribution preservation is the only requirement. Suppression is low (0.12) because alternatives (other licenses, proprietary code) remain fully available; no one is coerced into using permissive code. Theater ratio is low (0.15) because the license text does what it says: it grants permissions. Accessibility collapse is low (0.25) because implementers can always choose other code or write their own. Resistance is low (0.18) because the constraint is opt-in and beneficial. All metrics are stable across the interval — the license text itself does not change; only the ecosystem around it grows.
 *
 * PERSPECTIVAL GAP:
 *   The corporate_moat_reading would compute high extraction for individual implementers facing corporate capture; the copyleft_counterfactual_reading would compute high extraction for the commons when contributions are privatized. This reading abstains from those dynamics — it models the license text in isolation as a coordination artifact. The engine will compute rope from all seats here; the divergence from sibling readings' classifications is the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Universal implementers are full beneficiaries (d ≈ 0.0): the constraint subsidizes them by removing legal friction. License authors are agenda_setters who also benefit (d ≈ 0.1): they choose the license to maximize adoption. Corporate adopters are beneficiaries (d ≈ 0.15): they gain free upstream R&D but also contribute back via patches and funding — near-symmetric. Copyleft advocates are excluded (not in the conversation) but not extracted from. Legal scholars are observers. The directionality derivation from beneficiary declarations + exit options (arbitrage for all implementers) yields uniformly low d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (minimizing legal friction for software sharing) remains live — the coordination function has not atrophied. The constraint is not a piton (no theatrical maintenance of a dead function) and not a scaffold (no sunset clause). Mandatrophy is not resolved because the mandate is still active.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'How does the committer structure (permissive_license_text kernel with commons_coordination_reading, corporate_moat_reading, copyleft_counterfactual_reading) affect the structural classification of this constraint?',
    'Track whether sibling readings'' structural claims (extraction, victim sets) are empirically borne out in adoption patterns; if corporate_moat_reading''s extraction claim validates, this reading''s epsilon may need upward revision.',
    'If sibling readings describe real structural dynamics that this reading abstracts away, the rope classification may mask extractive substructure. This reading''s low epsilon holds only if the coordination function dominates empirically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committers frame: this reading instantiates one constraint from a contested kernel; sibling readings instantiate different constraints with different epsilon and victim structures.').

omega_variable(
    coordination_vs_extraction_boundary,
    'Does the permissive license text''s minimal friction genuinely coordinate a universal implementer pool, or does it structurally enable uncompensated corporate extraction that the coordination framing obscures?',
    'Longitudinal analysis of permissive-licensed project governance: measure contributor diversity, corporate capture rates, and whether forks remain open vs. close proprietary. Compare against copyleft-licensed project baselines.',
    'If corporate capture systematically converts coordination into extraction, the constraint''s effective type shifts from rope toward tangled_rope or snare for downstream implementers, even if the license text itself remains low-extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, empirical, 'Whether the coordination function survives contact with asymmetric corporate power.').

omega_variable(
    license_choice_as_coordination_mechanism,
    'Is the license text itself the active coordination mechanism, or is it a passive signal that enables coordination produced by community norms and tooling?',
    'Counterfactual: if the license text disappeared but community norms and tooling persisted, would coordination collapse? Measure projects that relicense vs. those that rely on informal norms.',
    'If the text is passive, the constraint''s suppression and extractiveness are near-zero (mountain-adjacent); if active, the rope classification holds with the text as the coordination artifact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(license_choice_as_coordination_mechanism, conceptual, 'Whether the license text or the social layer carries the coordination load.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_license_text__commons_coordination_reading, 1988, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(permissive_license_commons_tr_t1988, permissive_license_text__commons_coordination_reading, theater_ratio, 1988, 0.1).
narrative_ontology:measurement(permissive_license_commons_tr_t1995, permissive_license_text__commons_coordination_reading, theater_ratio, 1995, 0.12).
narrative_ontology:measurement(permissive_license_commons_tr_t2000, permissive_license_text__commons_coordination_reading, theater_ratio, 2000, 0.13).
narrative_ontology:measurement(permissive_license_commons_tr_t2005, permissive_license_text__commons_coordination_reading, theater_ratio, 2005, 0.14).
narrative_ontology:measurement(permissive_license_commons_tr_t2010, permissive_license_text__commons_coordination_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(permissive_license_commons_tr_t2015, permissive_license_text__commons_coordination_reading, theater_ratio, 2015, 0.15).
narrative_ontology:measurement(permissive_license_commons_tr_t2020, permissive_license_text__commons_coordination_reading, theater_ratio, 2020, 0.15).
narrative_ontology:measurement(permissive_license_commons_tr_t2024, permissive_license_text__commons_coordination_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(permissive_license_commons_be_t1988, permissive_license_text__commons_coordination_reading, base_extractiveness, 1988, 0.05).
narrative_ontology:measurement(permissive_license_commons_be_t1995, permissive_license_text__commons_coordination_reading, base_extractiveness, 1995, 0.06).
narrative_ontology:measurement(permissive_license_commons_be_t2000, permissive_license_text__commons_coordination_reading, base_extractiveness, 2000, 0.07).
narrative_ontology:measurement(permissive_license_commons_be_t2005, permissive_license_text__commons_coordination_reading, base_extractiveness, 2005, 0.07).
narrative_ontology:measurement(permissive_license_commons_be_t2010, permissive_license_text__commons_coordination_reading, base_extractiveness, 2010, 0.08).
narrative_ontology:measurement(permissive_license_commons_be_t2015, permissive_license_text__commons_coordination_reading, base_extractiveness, 2015, 0.08).
narrative_ontology:measurement(permissive_license_commons_be_t2020, permissive_license_text__commons_coordination_reading, base_extractiveness, 2020, 0.08).
narrative_ontology:measurement(permissive_license_commons_be_t2024, permissive_license_text__commons_coordination_reading, base_extractiveness, 2024, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(permissive_license_commons_su_t1988, permissive_license_text__commons_coordination_reading, suppression_requirement, 1988, 0.08).
narrative_ontology:measurement(permissive_license_commons_su_t1995, permissive_license_text__commons_coordination_reading, suppression_requirement, 1995, 0.1).
narrative_ontology:measurement(permissive_license_commons_su_t2000, permissive_license_text__commons_coordination_reading, suppression_requirement, 2000, 0.11).
narrative_ontology:measurement(permissive_license_commons_su_t2005, permissive_license_text__commons_coordination_reading, suppression_requirement, 2005, 0.12).
narrative_ontology:measurement(permissive_license_commons_su_t2010, permissive_license_text__commons_coordination_reading, suppression_requirement, 2010, 0.12).
narrative_ontology:measurement(permissive_license_commons_su_t2015, permissive_license_text__commons_coordination_reading, suppression_requirement, 2015, 0.12).
narrative_ontology:measurement(permissive_license_commons_su_t2020, permissive_license_text__commons_coordination_reading, suppression_requirement, 2020, 0.12).
narrative_ontology:measurement(permissive_license_commons_su_t2024, permissive_license_text__commons_coordination_reading, suppression_requirement, 2024, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(permissive_license_text__commons_coordination_reading, information_standard).
narrative_ontology:boltzmann_floor_override(permissive_license_text__commons_coordination_reading, 0.02).
narrative_ontology:affects_constraint(permissive_license_text__commons_coordination_reading, permissive_license_text__corporate_moat_reading).
narrative_ontology:affects_constraint(permissive_license_text__commons_coordination_reading, permissive_license_text__copyleft_counterfactual_reading).
narrative_ontology:affects_constraint(permissive_license_text__commons_coordination_reading, copyleft_license_text__reciprocity_coordination_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the permissive_license_text kernel into three structurally distinct readings. The commons_coordination_reading models the license text as a pure coordination artifact (rope, epsilon ≈ 0.08). The corporate_moat_reading models the same text as an extraction enabler for proprietary derivatives (tangled_rope or snare, epsilon > 0.4). The copyleft_counterfactual_reading models the absence of reciprocity as a structural vulnerability (tangled_rope, epsilon ≈ 0.3). They share the kernel but diverge on epsilon, beneficiaries, and victims — confirming ε-invariance requires separate stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
