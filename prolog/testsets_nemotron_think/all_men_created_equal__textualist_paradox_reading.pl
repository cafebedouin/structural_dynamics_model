% ============================================================================
% CONSTRAINT STORY: all_men_created_equal__textualist_paradox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_all_men_created_equal__textualist_paradox_reading, []).

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
 *   constraint_id: all_men_created_equal__textualist_paradox_reading
 *   human_readable: Textualist Paradox Reading of Declaration Equality Clause
 *   domain: constitutional_law/political_philosophy/american_studies
 *
 * SUMMARY:
 *   The textualist paradox reading of 'all men are created equal' argues that
 *   the Declaration's universal language creates a performative contradiction
 *   when the founding practice restricted equality to white propertied men.
 *   The constraint is the interpretive claim that semantic universality
 *   structurally entails universal application — you cannot say 'all' and
 *   mean 'some' without contradiction. This reading delegatesitimizes
 *   originalist authority (which grounds legitimacy in founder intent) by
 *   showing that the text itself undermines the restrictive reading. The
 *   victim is the originalist interpretive framework; the beneficiaries are
 *   textualist paradox proponents and universalist readers who gain
 *   interpretive space. The constraint requires active enforcement through
 *   legal argument, scholarly discourse, and judicial citation to maintain
 *   its destabilizing force against originalist orthodoxy.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(all_men_created_equal__textualist_paradox_reading, 0.45).
domain_priors:suppression_score(all_men_created_equal__textualist_paradox_reading, 0.35).
domain_priors:theater_ratio(all_men_created_equal__textualist_paradox_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(all_men_created_equal__textualist_paradox_reading, tangled_rope).
narrative_ontology:human_readable(all_men_created_equal__textualist_paradox_reading, "Textualist Paradox Reading of Declaration Equality Clause").
narrative_ontology:topic_domain(all_men_created_equal__textualist_paradox_reading, "constitutional_law/political_philosophy/american_studies").

domain_priors:requires_active_enforcement(all_men_created_equal__textualist_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(all_men_created_equal__textualist_paradox_reading, '2f2789ad-57ac-4583-a601-d9ee6f2d9f6f').
narrative_ontology:cs_kernel_codification('2f2789ad-57ac-4583-a601-d9ee6f2d9f6f', formalized).
narrative_ontology:cs_authority_grounding('2f2789ad-57ac-4583-a601-d9ee6f2d9f6f', lineage).
narrative_ontology:cs_interpretation_layer_present('2f2789ad-57ac-4583-a601-d9ee6f2d9f6f').
narrative_ontology:cs_reading_relation('2f2789ad-57ac-4583-a601-d9ee6f2d9f6f', all_men_created_equal__originalist_reading, forecloses).
narrative_ontology:cs_reading_relation('2f2789ad-57ac-4583-a601-d9ee6f2d9f6f', all_men_created_equal__universalist_reading, coexists_with).
narrative_ontology:cs_axiom('2f2789ad-57ac-4583-a601-d9ee6f2d9f6f', foundational, universal_language_entails_universal_application).
narrative_ontology:cs_axiom_status(universal_language_entails_universal_application, holdable).
narrative_ontology:cs_axiom_grounding('2f2789ad-57ac-4583-a601-d9ee6f2d9f6f', universal_language_entails_universal_application, deontological).
narrative_ontology:cs_axiom('2f2789ad-57ac-4583-a601-d9ee6f2d9f6f', foundational, performative_contradiction_delegitimizes_restricted_reading).
narrative_ontology:cs_axiom_status(performative_contradiction_delegitimizes_restricted_reading, holdable).
narrative_ontology:cs_axiom_grounding('2f2789ad-57ac-4583-a601-d9ee6f2d9f6f', performative_contradiction_delegitimizes_restricted_reading, empirically_contingent).
narrative_ontology:cs_reference_frame('2f2789ad-57ac-4583-a601-d9ee6f2d9f6f', originalist_fidelity_framework).
narrative_ontology:cs_drift_state('2f2789ad-57ac-4583-a601-d9ee6f2d9f6f', contemporary_textualist_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2f2789ad-57ac-4583-a601-d9ee6f2d9f6f', '').
narrative_ontology:cs_kernel_id(all_men_created_equal__textualist_paradox_reading, all_men_created_equal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(all_men_created_equal__textualist_paradox_reading, textualist_paradox_proponents).
narrative_ontology:constraint_beneficiary(all_men_created_equal__textualist_paradox_reading, universalist_readers).
narrative_ontology:constraint_victim(all_men_created_equal__textualist_paradox_reading, originalist_interpreters).
narrative_ontology:constraint_vindicates(all_men_created_equal__textualist_paradox_reading, textual_fidelity_requires_universal_scope).
narrative_ontology:constraint_vindicates(all_men_created_equal__textualist_paradox_reading, performative_contradiction_undermines_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain an interpretive framework binding constitutional equality to 18th-century social taxonomy. Their authority derives from claimed fidelity to founder intent. The textualist paradox reading extracts legitimacy from this framework by showing its internal contradiction. Exit requires abandoning a professional identity built on originalist methodology; constrained by institutional appointments, judicial philosophy commitments, and movement infrastructure.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, originalist_interpreters, payer,
    institutional, generational, constrained, national).

% Advance the reading that universal language ('all men are created equal') structurally entails universal application, making restricted application a performative contradiction. They set the interpretive agenda in legal academia and progressive jurisprudence. They benefit from the delegitimization of originalist authority, gaining interpretive space for expansive equality readings. Exit is mobile — they can shift to other interpretive projects without professional ruin.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, textualist_paradox_proponents, agenda_setter,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(all_men_created_equal__textualist_paradox_reading, textualist_paradox_proponents, beneficiary).

% Read equality as a universal principle requiring iterative expansion regardless of founder intent. They benefit from the textualist paradox reading's destabilization of originalist authority, which clears rhetorical space for their expansionist project. Their reading coexists with the paradox reading — the paradox opens the door; universalism walks through it. Exit is mobile across interpretive traditions.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, universalist_readers, beneficiary,
    organized, generational, mobile, national).

% Enslaved people, women, Indigenous nations, and propertyless men excluded from the 1776 application of 'all men.' Their voices were structurally absent from the founding interpretive community. The textualist paradox reading retrospectively names their exclusion as the contradiction's evidence, but they were not participants in the interpretive contest. Exit from exclusion required centuries of mobilization, not interpretive choice.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, historically_excluded_groups, excluded,
    powerless, generational, trapped, national).

% Adjudicate constitutional meaning under competing readings. They observe the textualist paradox as an argument in briefs and opinions but are not bound to adopt it. Their institutional role requires managing the tension between textual fidelity and democratic legitimacy. Exit is analytical — they evaluate readings but cannot personally exit the constraint of constitutional interpretation.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates interpretive practice around the principle that constitutional text must be read as written — universal language binds universally. Solves the coordination problem of interpretive arbitrariness by anchoring meaning in semantic structure rather than historical intent.
% TRANSFER_FUNCTION: Transfers interpretive authority and legitimacy from originalist frameworks (grounded in founder intent) to textualist/universalist frameworks (grounded in semantic structure). The originalist reading pays the cost of delegitimization; the paradox and universalist readings collect the authority.
% ABSENT_VOICES: The historically excluded groups (enslaved people, women, Indigenous nations) who lived the contradiction were never in the interpretive room. The textualist paradox reading speaks for them retrospectively but they could not contest the original restricted reading. Contemporary descendants and movements remain marginally included in formal constitutional interpretation.
% DISAPPEARANCE_RATIONALE: If the textualist paradox reading vanished, originalist fidelity to founder intent would face one fewer structural challenge. The universalist reading would lose a key textual anchor. Constitutional jurisprudence would shift toward intent-based originalism with less internal pressure for expansion. The interpretive landscape would rearrange around a more stable originalist consensus.
% FOUNDING_PROBLEM: The Declaration's universal language ('all men are created equal') was deployed to legitimize independence while the new polity simultaneously entrenched slavery and excluded women, Indigenous peoples, and propertyless men from political equality. The founding problem was legitimizing a revolutionary claim that the practice of the revolutionaries contradicted.
% FOUNDING_PROBLEM_CORROBORATION: Frederick Douglass's 'What to the Slave is the Fourth of July?' (1852) and the abolitionist constitutionalism of Lysander Spooner and William Lloyd Garrison attest from outside the founding beneficiaries that the contradiction was live and recognized at the founding. Originalist scholars (e.g., Raoul Berger, Robert Bork) attest the problem is dead — the founders' intent was restricted. The contest is structural: whether the text's semantic content or the authors' intent governs.
narrative_ontology:disappearance_verdict(all_men_created_equal__textualist_paradox_reading, world_rearranges).
narrative_ontology:founding_problem_status(all_men_created_equal__textualist_paradox_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(all_men_created_equal__textualist_paradox_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(all_men_created_equal__textualist_paradox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(all_men_created_equal__textualist_paradox_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(all_men_created_equal__textualist_paradox_reading_tests).
:- end_tests(all_men_created_equal__textualist_paradox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because the constraint extracts interpretive legitimacy from originalism without extracting material resources — it's a legitimacy transfer. Suppression is low-moderate (0.35) because the constraint doesn't coerce; it persuades through structural argument. Theater ratio is moderate-high (0.55) because originalist practice increasingly performs fidelity to text while actually following intent — the performance of textualism masks intent-based restriction. Accessibility collapse is moderate (0.6) because universalist and originalist alternatives remain live but the paradox narrows the middle ground. Resistance is high (0.7) because originalism has institutional entrenchment (Federalist Society, judicial appointments) and fights back vigorously.
 *
 * PERSPECTIVAL GAP:
 *   From the originalist seat, the constraint is a category error — text must be read in historical context, 'all men' meant 'all free white men' in 1776 usage. From the paradox seat, the constraint is structural — semantics don't bend to intent; 'all' means all. From the universalist seat, the paradox is a useful wedge but insufficient — expansion requires moral commitment, not just semantic logic. The engine computes these divergences from the structural data; the claimed tangled_rope type reflects that both coordination (textual fidelity) and extraction (delegitimizing originalism) operate simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist interpreters are payers (d near 1.0) — their framework bears the full delegitimization cost, exit is constrained by professional identity. Textualist paradox proponents are agenda_setters/beneficiaries (d near 0.0) — they set the interpretive terms and collect authority. Universalist readers are beneficiaries (d ~0.2) — they gain space but don't drive the paradox argument. Historically excluded groups are excluded (d undefined) — they were never in the interpretive game. Courts are observers (d=0.5) — they adjudicate but don't originate.
 *
 * MANDATROPHY ANALYSIS:
 *   The originalist reading's mandate (fidelity to founder intent) has outlived its function as a stable interpretive anchor because the textual paradox exposes its internal instability. The paradox reading prevents mislabeling originalism as pure coordination — it shows originalism extracts legitimacy from a text it cannot faithfully read. Conversely, the paradox reading itself risks becoming a new orthodoxy that suppresses intentionalist readings — the mandatrophy analysis cuts both ways.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    performative_contradiction_structural_vs_resolvable,
    'Is the performative contradiction between universal language and restricted application a structural semantic necessity, or can it be resolved through historical contextualization of ''all men'' as a term of art?',
    'Corpus linguistics of 18th-century usage: if ''all men'' was a recognized term of art meaning ''all free white men,'' the contradiction dissolves; if ''all men'' was semantically universal in contemporary usage, the contradiction is structural.',
    'If structural, the paradox reading''s extractiveness is justified and originalism is delegitimized; if resolvable, the paradox reading is a category error and its extraction is spurious.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performative_contradiction_structural_vs_resolvable, empirical, 'Whether the contradiction is semantic or contextual.').

omega_variable(
    paradox_reading_as_new_orthodoxy,
    'Does the textualist paradox reading risk becoming a new performative orthodoxy that suppresses intentionalist and pragmatist readings, replicating the extraction pattern it identifies?',
    'Track citation networks and judicial adoption: if paradox language becomes mandatory in equality jurisprudence and intentionalist arguments are excluded from serious consideration, the reading has become extractive in its own right.',
    'If yes, the constraint reclassifies from tangled_rope toward snare (pure extraction of interpretive space); if no, it remains a genuine coordination mechanism for textual fidelity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(paradox_reading_as_new_orthodoxy, conceptual, 'Whether the paradox reading itself becomes a snare.').

omega_variable(
    kernel_instability_vs_reading_instability,
    'Does the kernel itself (the Declaration''s equality clause) contain irreducible instability, or is the instability produced by the clash of readings?',
    'Compare the Declaration''s reception history: if contemporaneous readers (including founders) recognized the contradiction immediately, the kernel is unstable; if the contradiction emerges only through later readings, the instability is reading-generated.',
    'If kernel-unstable, all readings inherit extractiveness; if reading-unstable, the paradox reading''s extraction is its own contribution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_instability_vs_reading_instability, conceptual, 'Source of kernel instability: intrinsic or reader-generated.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__textualist_paradox_reading, 1776, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(all_men_created_equal__textualist_paradox_reading_tr_t1776, all_men_created_equal__textualist_paradox_reading, theater_ratio, 1776, 0.7).
narrative_ontology:measurement(all_men_created_equal__textualist_paradox_reading_tr_t1800, all_men_created_equal__textualist_paradox_reading, theater_ratio, 1800, 0.75).
narrative_ontology:measurement(all_men_created_equal__textualist_paradox_reading_tr_t1852, all_men_created_equal__textualist_paradox_reading, theater_ratio, 1852, 0.6).
narrative_ontology:measurement(all_men_created_equal__textualist_paradox_reading_tr_t1868, all_men_created_equal__textualist_paradox_reading, theater_ratio, 1868, 0.55).
narrative_ontology:measurement(all_men_created_equal__textualist_paradox_reading_tr_t1954, all_men_created_equal__textualist_paradox_reading, theater_ratio, 1954, 0.52).
narrative_ontology:measurement(all_men_created_equal__textualist_paradox_reading_tr_t1973, all_men_created_equal__textualist_paradox_reading, theater_ratio, 1973, 0.54).
narrative_ontology:measurement(all_men_created_equal__textualist_paradox_reading_tr_t2024, all_men_created_equal__textualist_paradox_reading, theater_ratio, 2024, 0.55).

% Extraction over time
narrative_ontology:measurement(all_men_created_equal__textualist_paradox_reading_be_t1776, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 1776, 0.15).
narrative_ontology:measurement(all_men_created_equal__textualist_paradox_reading_be_t1800, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 1800, 0.2).
narrative_ontology:measurement(all_men_created_equal__textualist_paradox_reading_be_t1852, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 1852, 0.35).
narrative_ontology:measurement(all_men_created_equal__textualist_paradox_reading_be_t1868, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 1868, 0.4).
narrative_ontology:measurement(all_men_created_equal__textualist_paradox_reading_be_t1954, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 1954, 0.42).
narrative_ontology:measurement(all_men_created_equal__textualist_paradox_reading_be_t1973, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 1973, 0.44).
narrative_ontology:measurement(all_men_created_equal__textualist_paradox_reading_be_t2024, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(all_men_created_equal__textualist_paradox_reading_su_t1776, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 1776, 0.2).
narrative_ontology:measurement(all_men_created_equal__textualist_paradox_reading_su_t1800, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 1800, 0.25).
narrative_ontology:measurement(all_men_created_equal__textualist_paradox_reading_su_t1852, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 1852, 0.3).
narrative_ontology:measurement(all_men_created_equal__textualist_paradox_reading_su_t1868, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 1868, 0.35).
narrative_ontology:measurement(all_men_created_equal__textualist_paradox_reading_su_t1954, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 1954, 0.38).
narrative_ontology:measurement(all_men_created_equal__textualist_paradox_reading_su_t1973, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 1973, 0.35).
narrative_ontology:measurement(all_men_created_equal__textualist_paradox_reading_su_t2024, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 2024, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(all_men_created_equal__textualist_paradox_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(all_men_created_equal__textualist_paradox_reading, 0.08).
narrative_ontology:affects_constraint(all_men_created_equal__textualist_paradox_reading, all_men_created_equal__originalist_reading).
narrative_ontology:affects_constraint(all_men_created_equal__textualist_paradox_reading, all_men_created_equal__universalist_reading).
narrative_ontology:affects_constraint(all_men_created_equal__textualist_paradox_reading, fourteenth_amendment_equal_protection).
narrative_ontology:affects_constraint(all_men_created_equal__textualist_paradox_reading, civil_rights_act_1964).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the 'all men are created equal' kernel into three readings with distinct ε values: originalist_reading (low extractiveness, mountain-claimed), textualist_paradox_reading (moderate extractiveness, tangled_rope), universalist_reading (low-moderate extractiveness, rope-claimed). The paradox reading structurally depends on the originalist reading as its foil (forecloses relation) and enables the universalist reading (influences relation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(all_men_created_equal__textualist_paradox_reading, institutional, 0.85).
constraint_indexing:directionality_override(all_men_created_equal__textualist_paradox_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
