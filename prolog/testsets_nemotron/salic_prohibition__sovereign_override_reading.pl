% ============================================================================
% CONSTRAINT STORY: salic_prohibition__sovereign_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_salic_prohibition__sovereign_override_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: salic_prohibition__sovereign_override_reading
 *   human_readable: Salic Prohibition as Revocable Positive Law (Sovereign Override Reading)
 *   domain: constitutional_law/dynastic_succession/political_history
 *
 * SUMMARY:
 *   This constraint story represents the SOVEREIGN OVERRIDE READING of the
 *   Salic prohibition kernel. Under this reading, the Salic Law is positive
 *   law enacted by Frankish kings, revocable by the sovereign's legislative
 *   authority. The 1713 Pragmatic Sanction of Charles VI is the constitutive
 *   act: it declares female succession permissible, names his daughter Maria
 *   Theresa as heir, and binds the Habsburg lands to this new succession
 *   order. Challengers to this arrangement (e.g., Charles Albert of Bavaria,
 *   Frederick II of Prussia) are rebels against legitimate authority; the War
 *   of Austrian Succession (1740–1748) is a defensive war to protect dynastic
 *   continuity established by sovereign act. The constraint's extraction is
 *   low because the Pragmatic Sanction removes the exclusionary barrier; its
 *   suppression is low because the arrangement is sustained by recognition
 *   and treaty, not active coercion of the excluded. The theater ratio is low
 *   because the sovereign's legislative act is the functional mechanism, not
 *   a performance masking extraction.
 *
 * KEY AGENTS:
 *   - reigning_monarch: agenda_setter (institutional/analytical) — enacts the Pragmatic Sanction, sets the succession order
 *   - dynastic_house: beneficiary (organized/generational) — gains continuity and legitimacy through the sanctioned heir
 *   - pragmatic_sanction_beneficiaries: beneficiary (organized/biographical) — female heirs and their lines who gain succession rights
 *   - salic_loyalist_estates: excluded (powerful/biographical) — would maintain male-only succession, displaced by the override
 *   - foreign_powers: payer (institutional/biographical) — bear costs of recognizing or contesting the new succession
 *   - constitutional_lawyers: observer (analytical/civilizational) — analyze the legal validity of the sovereign override
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__sovereign_override_reading, 0.25).
domain_priors:suppression_score(salic_prohibition__sovereign_override_reading, 0.15).
domain_priors:theater_ratio(salic_prohibition__sovereign_override_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__sovereign_override_reading, rope).
narrative_ontology:human_readable(salic_prohibition__sovereign_override_reading, "Salic Prohibition as Revocable Positive Law (Sovereign Override Reading)").
narrative_ontology:topic_domain(salic_prohibition__sovereign_override_reading, "constitutional_law/dynastic_succession/political_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__sovereign_override_reading, '8e07f40e-737a-44e8-922f-ccf472dd030d').
narrative_ontology:cs_kernel_codification('8e07f40e-737a-44e8-922f-ccf472dd030d', fixed_text).
narrative_ontology:cs_authority_grounding('8e07f40e-737a-44e8-922f-ccf472dd030d', lineage).
narrative_ontology:cs_interpretation_layer_present('8e07f40e-737a-44e8-922f-ccf472dd030d').
narrative_ontology:cs_reading_relation('8e07f40e-737a-44e8-922f-ccf472dd030d', salic_prohibition__immutable_mandate_reading, forecloses).
narrative_ontology:cs_reading_relation('8e07f40e-737a-44e8-922f-ccf472dd030d', salic_prohibition__cognatic_reversion_reading, coexists_with).
narrative_ontology:cs_axiom('8e07f40e-737a-44e8-922f-ccf472dd030d', foundational, sovereign_legislative_supremacy_over_dynastic_law).
narrative_ontology:cs_axiom_status(sovereign_legislative_supremacy_over_dynastic_law, holdable).
narrative_ontology:cs_axiom_grounding('8e07f40e-737a-44e8-922f-ccf472dd030d', sovereign_legislative_supremacy_over_dynastic_law, conventional).
narrative_ontology:cs_axiom('8e07f40e-737a-44e8-922f-ccf472dd030d', foundational, pragmatic_sanction_as_valid_exercise_of_sovereign_authority).
narrative_ontology:cs_axiom_status(pragmatic_sanction_as_valid_exercise_of_sovereign_authority, holdable).
narrative_ontology:cs_axiom_grounding('8e07f40e-737a-44e8-922f-ccf472dd030d', pragmatic_sanction_as_valid_exercise_of_sovereign_authority, conventional).
narrative_ontology:cs_reference_frame('8e07f40e-737a-44e8-922f-ccf472dd030d', frankish_tribal_succession_custom).
narrative_ontology:cs_drift_state('8e07f40e-737a-44e8-922f-ccf472dd030d', pragmatic_sanction_1713, gap(codification_collapse, substantial, true)).
narrative_ontology:cs_created_at('8e07f40e-737a-44e8-922f-ccf472dd030d', '2026-06-15T12:00:00Z').
narrative_ontology:cs_kernel_id(salic_prohibition__sovereign_override_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, reigning_monarch).
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, dynastic_house).
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, pragmatic_sanction_beneficiaries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(salic_prohibition__sovereign_override_reading, foreign_powers).
narrative_ontology:constraint_vindicates(salic_prohibition__sovereign_override_reading, sovereign_legislative_supremacy).
narrative_ontology:constraint_vindicates(salic_prohibition__sovereign_override_reading, positive_law_revocability).
narrative_ontology:constraint_vindicates(salic_prohibition__sovereign_override_reading, pragmatic_sanction_legitimacy).
narrative_ontology:constraint_vindicates(salic_prohibition__sovereign_override_reading, dynastic_continuity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Charles VI (1711–1740) enacts the 1713 Pragmatic Sanction as an exercise of sovereign legislative authority, declaring his daughter Maria Theresa heir to the Habsburg hereditary lands. He spends his reign securing recognition from domestic estates and foreign powers. His exit from the constraint is analytical: he authored it and could revoke it, but doing so would destroy the dynastic continuity he seeks.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, reigning_monarch, agenda_setter,
    institutional, generational, analytical, continental).

% The House of Habsburg gains a legitimate succession path through Maria Theresa, avoiding extinction or partition. The dynasty's institutional continuity is the primary coordination benefit. As a collective actor, it can negotiate recognition treaties (arbitrage-grade exit) but its identity is fused with the succession arrangement.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, dynastic_house, beneficiary,
    organized, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(salic_prohibition__sovereign_override_reading, dynastic_house, agenda_setter).

% Maria Theresa and her descendants gain succession rights previously denied by Salic Law. Their position depends on the Pragmatic Sanction's recognition; if the arrangement collapses, they lose their claim. Exit is constrained — they cannot easily renounce their dynastic role, but they could marry into other houses.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, pragmatic_sanction_beneficiaries, beneficiary,
    organized, biographical, constrained, continental).

% Estates and nobles in Habsburg lands who uphold traditional Salic succession. They are excluded from the new arrangement — their preferred succession order is displaced. They can resist (as some did in 1740) but cannot easily exit the sovereign's jurisdiction. Their power is substantial but constrained by the monarch's legislative authority.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, salic_loyalist_estates, excluded,
    powerful, biographical, constrained, regional).

% Prussia, Bavaria, France, Spain, Saxony — bear diplomatic and military costs of recognizing or contesting the Pragmatic Sanction. They extract territorial concessions (Silesia, Bavaria) in the War of Austrian Succession. Their exit is mobile: they can recognize, contest, or ignore the arrangement based on interest. They are payers because the constraint imposes recognition costs on them.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, foreign_powers, payer,
    institutional, biographical, mobile, continental).

% Jurists (e.g., Pufendorf, Vattel, later scholars) analyze whether sovereign legislative authority can override fundamental dynastic law. They neither collect nor pay; their analytical frame shapes how the constraint is understood across time. Their exit is analytical — they can change their interpretation without material cost.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, constitutional_lawyers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the dynastic continuity problem when a monarch lacks male heirs: the sovereign's legislative act designates a female heir and binds the realm to accept her succession, preventing partition, election, or foreign imposition.
% TRANSFER_FUNCTION: Moves the succession right from the Salic-prescribed male line (extinct or absent) to the sovereign's designated female line. The transfer is from 'no legitimate heir under Salic Law' to 'legitimate heir by sovereign act.' The cost is borne by foreign powers and Salic loyalists who must recognize the new heir; the benefit accrues to the dynastic house and the designated heir.
% ABSENT_VOICES: The hypothetical male heirs who would have succeeded under Salic Law (absent because extinct or never existed). The Frankish tribal assemblies whose custom originated the law (absent because centuries dead). The peasantry and common subjects whose consent was never sought (structurally excluded from dynastic politics).
% DISAPPEARANCE_RATIONALE: If the Pragmatic Sanction vanished overnight, the Habsburg lands would revert to Salic succession — but with no male heir, the result would be partition, elective succession, or war of competing claims. The world rearranges because the constraint is the ONLY thing designating Maria Theresa as heir; without it, the succession is genuinely contested.
% FOUNDING_PROBLEM: The Frankish Salic Law (c. 500 CE) was built to solve: preventing fragmentation of the kingdom among multiple heirs and excluding female lines from claiming the throne in a tribal context where military leadership was male-coded.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is dead per the Pragmatic Sanction's own preamble (Charles VI, 1713): 'the ancient Salic Law... was established for other times and circumstances.' Contemporary jurists (Pufendorf, 1672; Vattel, 1758) attest that fundamental dynastic laws can be changed by sovereign authority. The immutable_mandate_reading disputes this, but its adherents are the benefiting parties of the old arrangement — no independent corroboration exists for the claim that the Frankish tribal context still governs 18th-century sovereign succession.
narrative_ontology:disappearance_verdict(salic_prohibition__sovereign_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__sovereign_override_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__sovereign_override_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(salic_prohibition__sovereign_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(salic_prohibition__sovereign_override_reading, 0.25, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_prohibition__sovereign_override_reading_tests).
:- end_tests(salic_prohibition__sovereign_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.25 at interval end) reflects that the Pragmatic Sanction removes the structural barrier to female succession; the constraint no longer extracts succession rights from female heirs. The 1713 measurement (0.70) captures the pre-Sanction Salic regime's high extraction; the drop to 0.30 by 1720 shows the override taking effect. Theater ratio remains low (0.10) because the sovereign's legislative act is the genuine coordination mechanism — no performance of Salic compliance while extracting. Suppression drops from 0.60 to 0.10 as the arrangement shifts from enforced exclusion to recognized succession. Resistance remains moderately high (0.65) because Salic loyalists and foreign powers continue to contest the override's legitimacy, but this is resistance TO the constraint (the override), not resistance BY the constraint. Accessibility collapse is low (0.20) because alternatives (male heirs, elective succession, partition) remain conceptually available even if politically marginalized.
 *
 * DIRECTIONALITY LOGIC:
 *   The reigning monarch (Charles VI) is the agenda_setter with d near 0.0 (full beneficiary of legislative authority). The dynastic house and Pragmatic Sanction beneficiaries (Maria Theresa line) are beneficiaries with d ~ 0.2 — they gain succession rights without bearing enforcement costs. Salic loyalist estates are excluded: they would be payers under the old regime but are displaced; their d is ambiguous because they are neither coordinated nor extracted from by the new constraint. Foreign powers are payers (d ~ 0.6) — they bear diplomatic/military costs of recognizing the new succession. Constitutional lawyers are observers (d = 0.5). The engine will compute per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (ensuring male succession in Frankish tribal context) is dead — the Pragmatic Sanction explicitly addresses a new problem: securing dynastic continuity when no male heir exists. The constraint is not a piton because the sovereign actively legislated the change; it is not a snare because no party extracts from the excluded. The mandate is resolved: the arrangement serves its new function (female succession) without atrophying into theater. The high resistance reflects genuine contestation, not extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing,
    'Is the Salic prohibition a kernel admitting multiple readings, and does this reading correctly identify its structural relationship to the immutable_mandate_reading and cognatic_reversion_reading?',
    'Comparative analysis of how each reading instantiates the constraint: which stakeholder seats experience which types, where extraction/suppression metrics diverge, and whether the readings foreclose or merely coexist.',
    'If the readings are distinct constraints with different ε values and stakeholder structures, the kernel is a label for a constraint family, not a single constraint. This reading''s classification as rope depends on its structural independence from the snare-class immutable_mandate_reading and the tangled_rope-class cognatic_reversion_reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Whether this reading is a structurally distinct constraint from its sibling readings of the same kernel.').

omega_variable(
    sovereign_override_authenticity,
    'Does the sovereign override (Pragmatic Sanction) represent genuine legislative authority or a constitutional fiction masking continued Salic constraint?',
    'Historical analysis of the Pragmatic Sanction''s enactment, acceptance, and subsequent operation: whether it was recognized as binding by domestic estates, foreign powers, and subsequent monarchs, or whether it required continuous enforcement against Salic loyalists.',
    'If the override was a fiction, the constraint operates as immutable_mandate (snare) despite the sovereign_override framing. If genuine, the constraint is revocable positive law (rope) with the sovereign as agenda_setter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereign_override_authenticity, empirical, 'Whether the Pragmatic Sanction actually revoked Salic Law or merely suspended it under duress.').

omega_variable(
    extraction_referent_stability,
    'Is the extractiveness of this reading measured against the standing Salic arrangement (high extraction for excluded females) or the post-override arrangement (low extraction)?',
    'Apply the ε-invariance principle: the referent is the standing arrangement under contest, assessed by THIS reading''s lights. This reading treats the Pragmatic Sanction as the legitimate current arrangement, so extraction should be low. The immutable_mandate_reading would measure high extraction against the same referent.',
    'Confirms ε is reading-indexed: different readings of the same kernel author different ε for the same referent. Prevents conflating the kernel''s contested status with a single constraint''s metrics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_referent_stability, conceptual, 'Whether this reading''s low ε correctly uses the post-override arrangement as referent per kernel-reading rules.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__sovereign_override_reading, 1713, 1789).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(salic_sov_tr_t1713, salic_prohibition__sovereign_override_reading, theater_ratio, 1713, 0.05).
narrative_ontology:measurement(salic_sov_tr_t1720, salic_prohibition__sovereign_override_reading, theater_ratio, 1720, 0.15).
narrative_ontology:measurement(salic_sov_tr_t1740, salic_prohibition__sovereign_override_reading, theater_ratio, 1740, 0.12).
narrative_ontology:measurement(salic_sov_tr_t1789, salic_prohibition__sovereign_override_reading, theater_ratio, 1789, 0.1).

% Extraction over time
narrative_ontology:measurement(salic_sov_be_t1713, salic_prohibition__sovereign_override_reading, base_extractiveness, 1713, 0.7).
narrative_ontology:measurement(salic_sov_be_t1720, salic_prohibition__sovereign_override_reading, base_extractiveness, 1720, 0.3).
narrative_ontology:measurement(salic_sov_be_t1740, salic_prohibition__sovereign_override_reading, base_extractiveness, 1740, 0.2).
narrative_ontology:measurement(salic_sov_be_t1789, salic_prohibition__sovereign_override_reading, base_extractiveness, 1789, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(salic_sov_su_t1713, salic_prohibition__sovereign_override_reading, suppression_requirement, 1713, 0.6).
narrative_ontology:measurement(salic_sov_su_t1720, salic_prohibition__sovereign_override_reading, suppression_requirement, 1720, 0.25).
narrative_ontology:measurement(salic_sov_su_t1740, salic_prohibition__sovereign_override_reading, suppression_requirement, 1740, 0.15).
narrative_ontology:measurement(salic_sov_su_t1789, salic_prohibition__sovereign_override_reading, suppression_requirement, 1789, 0.1).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1713, tn=1789
narrative_ontology:measurement(salic_sov_grid_01, salic_prohibition__sovereign_override_reading, accessibility_collapse(class), 1713, 0.85).
narrative_ontology:measurement_basis(salic_sov_grid_01, observed).
narrative_ontology:measurement(salic_sov_grid_02, salic_prohibition__sovereign_override_reading, accessibility_collapse(class), 1789, 0.18).
narrative_ontology:measurement_basis(salic_sov_grid_02, observed).
narrative_ontology:measurement(salic_sov_grid_03, salic_prohibition__sovereign_override_reading, accessibility_collapse(individual), 1713, 0.9).
narrative_ontology:measurement_basis(salic_sov_grid_03, observed).
narrative_ontology:measurement(salic_sov_grid_04, salic_prohibition__sovereign_override_reading, accessibility_collapse(individual), 1789, 0.15).
narrative_ontology:measurement_basis(salic_sov_grid_04, observed).
narrative_ontology:measurement(salic_sov_grid_05, salic_prohibition__sovereign_override_reading, accessibility_collapse(organizational), 1713, 0.8).
narrative_ontology:measurement_basis(salic_sov_grid_05, observed).
narrative_ontology:measurement(salic_sov_grid_06, salic_prohibition__sovereign_override_reading, accessibility_collapse(organizational), 1789, 0.2).
narrative_ontology:measurement_basis(salic_sov_grid_06, observed).
narrative_ontology:measurement(salic_sov_grid_07, salic_prohibition__sovereign_override_reading, accessibility_collapse(structural), 1713, 0.75).
narrative_ontology:measurement_basis(salic_sov_grid_07, observed).
narrative_ontology:measurement(salic_sov_grid_08, salic_prohibition__sovereign_override_reading, accessibility_collapse(structural), 1789, 0.22).
narrative_ontology:measurement_basis(salic_sov_grid_08, observed).
narrative_ontology:measurement(salic_sov_grid_09, salic_prohibition__sovereign_override_reading, resistance(class), 1713, 0.7).
narrative_ontology:measurement_basis(salic_sov_grid_09, observed).
narrative_ontology:measurement(salic_sov_grid_10, salic_prohibition__sovereign_override_reading, resistance(class), 1789, 0.65).
narrative_ontology:measurement_basis(salic_sov_grid_10, observed).
narrative_ontology:measurement(salic_sov_grid_11, salic_prohibition__sovereign_override_reading, resistance(individual), 1713, 0.75).
narrative_ontology:measurement_basis(salic_sov_grid_11, observed).
narrative_ontology:measurement(salic_sov_grid_12, salic_prohibition__sovereign_override_reading, resistance(individual), 1789, 0.6).
narrative_ontology:measurement_basis(salic_sov_grid_12, observed).
narrative_ontology:measurement(salic_sov_grid_13, salic_prohibition__sovereign_override_reading, resistance(organizational), 1713, 0.65).
narrative_ontology:measurement_basis(salic_sov_grid_13, observed).
narrative_ontology:measurement(salic_sov_grid_14, salic_prohibition__sovereign_override_reading, resistance(organizational), 1789, 0.7).
narrative_ontology:measurement_basis(salic_sov_grid_14, observed).
narrative_ontology:measurement(salic_sov_grid_15, salic_prohibition__sovereign_override_reading, resistance(structural), 1713, 0.55).
narrative_ontology:measurement_basis(salic_sov_grid_15, observed).
narrative_ontology:measurement(salic_sov_grid_16, salic_prohibition__sovereign_override_reading, resistance(structural), 1789, 0.75).
narrative_ontology:measurement_basis(salic_sov_grid_16, observed).
narrative_ontology:measurement(salic_sov_grid_17, salic_prohibition__sovereign_override_reading, stakes_inflation(class), 1713, 0.75).
narrative_ontology:measurement_basis(salic_sov_grid_17, observed).
narrative_ontology:measurement(salic_sov_grid_18, salic_prohibition__sovereign_override_reading, stakes_inflation(class), 1789, 0.12).
narrative_ontology:measurement_basis(salic_sov_grid_18, observed).
narrative_ontology:measurement(salic_sov_grid_19, salic_prohibition__sovereign_override_reading, stakes_inflation(individual), 1713, 0.85).
narrative_ontology:measurement_basis(salic_sov_grid_19, observed).
narrative_ontology:measurement(salic_sov_grid_20, salic_prohibition__sovereign_override_reading, stakes_inflation(individual), 1789, 0.1).
narrative_ontology:measurement_basis(salic_sov_grid_20, observed).
narrative_ontology:measurement(salic_sov_grid_21, salic_prohibition__sovereign_override_reading, stakes_inflation(organizational), 1713, 0.7).
narrative_ontology:measurement_basis(salic_sov_grid_21, observed).
narrative_ontology:measurement(salic_sov_grid_22, salic_prohibition__sovereign_override_reading, stakes_inflation(organizational), 1789, 0.15).
narrative_ontology:measurement_basis(salic_sov_grid_22, observed).
narrative_ontology:measurement(salic_sov_grid_23, salic_prohibition__sovereign_override_reading, stakes_inflation(structural), 1713, 0.65).
narrative_ontology:measurement_basis(salic_sov_grid_23, observed).
narrative_ontology:measurement(salic_sov_grid_24, salic_prohibition__sovereign_override_reading, stakes_inflation(structural), 1789, 0.18).
narrative_ontology:measurement_basis(salic_sov_grid_24, observed).
narrative_ontology:measurement(salic_sov_grid_25, salic_prohibition__sovereign_override_reading, suppression(class), 1713, 0.6).
narrative_ontology:measurement_basis(salic_sov_grid_25, observed).
narrative_ontology:measurement(salic_sov_grid_26, salic_prohibition__sovereign_override_reading, suppression(class), 1789, 0.1).
narrative_ontology:measurement_basis(salic_sov_grid_26, observed).
narrative_ontology:measurement(salic_sov_grid_27, salic_prohibition__sovereign_override_reading, suppression(individual), 1713, 0.7).
narrative_ontology:measurement_basis(salic_sov_grid_27, observed).
narrative_ontology:measurement(salic_sov_grid_28, salic_prohibition__sovereign_override_reading, suppression(individual), 1789, 0.08).
narrative_ontology:measurement_basis(salic_sov_grid_28, observed).
narrative_ontology:measurement(salic_sov_grid_29, salic_prohibition__sovereign_override_reading, suppression(organizational), 1713, 0.55).
narrative_ontology:measurement_basis(salic_sov_grid_29, observed).
narrative_ontology:measurement(salic_sov_grid_30, salic_prohibition__sovereign_override_reading, suppression(organizational), 1789, 0.12).
narrative_ontology:measurement_basis(salic_sov_grid_30, observed).
narrative_ontology:measurement(salic_sov_grid_31, salic_prohibition__sovereign_override_reading, suppression(structural), 1713, 0.5).
narrative_ontology:measurement_basis(salic_sov_grid_31, observed).
narrative_ontology:measurement(salic_sov_grid_32, salic_prohibition__sovereign_override_reading, suppression(structural), 1789, 0.15).
narrative_ontology:measurement_basis(salic_sov_grid_32, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_prohibition__sovereign_override_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(salic_prohibition__sovereign_override_reading, 0.12).
narrative_ontology:affects_constraint(salic_prohibition__sovereign_override_reading, pragmatic_sanction_1713).
narrative_ontology:affects_constraint(salic_prohibition__sovereign_override_reading, war_of_austrian_succession).
narrative_ontology:affects_constraint(salic_prohibition__sovereign_override_reading, salic_prohibition__immutable_mandate_reading).
narrative_ontology:affects_constraint(salic_prohibition__sovereign_override_reading, salic_prohibition__cognatic_reversion_reading).

% DUAL FORMULATION NOTE:
% This reading is one of three in the Salic prohibition constraint family. The immutable_mandate_reading treats Salic Law as irrevocable (high ε, snare/mountain); the cognatic_reversion_reading treats it as inapplicable outside Frankish core (medium ε, tangled_rope); this reading treats it as revocable positive law (low ε, rope). The ε values differ because each reading assesses the standing arrangement under contest by its own lights. They are linked via affects_constraints to enable contamination analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(salic_prohibition__sovereign_override_reading, institutional, 0.15).
constraint_indexing:directionality_override(salic_prohibition__sovereign_override_reading, powerful, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
