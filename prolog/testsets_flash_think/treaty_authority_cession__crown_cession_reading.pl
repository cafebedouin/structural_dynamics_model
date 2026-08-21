% ============================================================================
% CONSTRAINT STORY: treaty_authority_cession__crown_cession_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_treaty_authority_cession__crown_cession_reading, []).

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
 *   constraint_id: treaty_authority_cession__crown_cession_reading
 *   human_readable: Treaty of Waitangi: Crown Cession Reading
 *   domain: constitutional_law/indigenous_rights/colonial_history
 *
 * SUMMARY:
 *   This constraint represents the 'Crown cession' reading of the Treaty of
 *   Waitangi, where the English text is considered paramount, 'kāwanatanga'
 *   is interpreted as full sovereignty, and the Treaty is understood to have
 *   completed the legal cession of Māori authority to the British Crown. This
 *   reading asserts the legitimacy of subsequent land alienation and the
 *   subordination of Māori customary law to the New Zealand legal system. It
 *   is a highly extractive and suppressive interpretation from the
 *   perspective of Māori, who are the primary victims.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__crown_cession_reading, 0.9).
domain_priors:suppression_score(treaty_authority_cession__crown_cession_reading, 0.95).
domain_priors:theater_ratio(treaty_authority_cession__crown_cession_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__crown_cession_reading, snare).
narrative_ontology:human_readable(treaty_authority_cession__crown_cession_reading, "Treaty of Waitangi: Crown Cession Reading").
narrative_ontology:topic_domain(treaty_authority_cession__crown_cession_reading, "constitutional_law/indigenous_rights/colonial_history").

domain_priors:requires_active_enforcement(treaty_authority_cession__crown_cession_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__crown_cession_reading, 'b31ad9ce-fcaa-45c4-bf55-4e68dfbbd727').
narrative_ontology:cs_kernel_codification('b31ad9ce-fcaa-45c4-bf55-4e68dfbbd727', fixed_text).
narrative_ontology:cs_authority_grounding('b31ad9ce-fcaa-45c4-bf55-4e68dfbbd727', lineage).
narrative_ontology:cs_interpretation_layer_present('b31ad9ce-fcaa-45c4-bf55-4e68dfbbd727').
narrative_ontology:cs_reading_relation('b31ad9ce-fcaa-45c4-bf55-4e68dfbbd727', treaty_authority_cession__rangatiratanga_retention_reading, forecloses).
narrative_ontology:cs_reading_relation('b31ad9ce-fcaa-45c4-bf55-4e68dfbbd727', treaty_authority_cession__biculturalism_reading, forecloses).
narrative_ontology:cs_reading_relation('b31ad9ce-fcaa-45c4-bf55-4e68dfbbd727', treaty_authority_cession__retrospective_snare_exposure, forecloses).
narrative_ontology:cs_axiom('b31ad9ce-fcaa-45c4-bf55-4e68dfbbd727', foundational, english_text_supremacy).
narrative_ontology:cs_axiom_status(english_text_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('b31ad9ce-fcaa-45c4-bf55-4e68dfbbd727', english_text_supremacy, conventional).
narrative_ontology:cs_axiom('b31ad9ce-fcaa-45c4-bf55-4e68dfbbd727', foundational, kawanatanga_equals_full_sovereignty).
narrative_ontology:cs_axiom_status(kawanatanga_equals_full_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('b31ad9ce-fcaa-45c4-bf55-4e68dfbbd727', kawanatanga_equals_full_sovereignty, conventional).
narrative_ontology:cs_reference_frame('b31ad9ce-fcaa-45c4-bf55-4e68dfbbd727', unqualified_crown_sovereignty_1840).
narrative_ontology:cs_drift_state('b31ad9ce-fcaa-45c4-bf55-4e68dfbbd727', contemporary_post_waitangi_tribunal_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('b31ad9ce-fcaa-45c4-bf55-4e68dfbbd727', '').
narrative_ontology:cs_kernel_id(treaty_authority_cession__crown_cession_reading, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treaty_authority_cession__crown_cession_reading, crown_government).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__crown_cession_reading, settler_population).
narrative_ontology:constraint_victim(treaty_authority_cession__crown_cession_reading, maori_iwi_hapu).
narrative_ontology:constraint_victim(treaty_authority_cession__crown_cession_reading, maori_customary_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts and enforces full sovereignty over New Zealand, deriving its authority from the English text of the Treaty. Benefits from legislative control, land ownership, and resource management. Actively suppresses challenges to this interpretation.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, crown_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefits from the legal framework that facilitates land ownership, economic development, and social stability under Crown law. Their prosperity is tied to the legitimacy of the Crown's claim to sovereignty and land.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, settler_population, beneficiary,
    organized, generational, mobile, national).

% Bear the costs of lost land, diminished customary authority, and cultural disruption. Their identity and well-being are deeply tied to their ancestral lands and self-governance, which this reading denies. They have continuously resisted this interpretation.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, maori_iwi_hapu, payer,
    powerless, civilizational, identity_locked, local).

% Its legal and practical recognition is extinguished or subordinated under this reading. Represents the traditional governance structures, laws, and practices of Māori, which are systematically undermined by the assertion of full Crown sovereignty.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, maori_customary_authority, payer,
    powerless, civilizational, identity_locked, local).

% Interprets and applies the law, historically upholding the Crown's sovereignty based on this reading. While theoretically independent, its decisions reinforce the structural power dynamics established by this interpretation of the Treaty.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Monitor and critique the human rights implications of historical and ongoing interpretations of the Treaty, often highlighting discrepancies with international indigenous rights standards. Their influence is external and advisory, not directly enforceable within the national legal system.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, international_human_rights_bodies, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From the Crown's perspective, it established a unified legal and administrative system for New Zealand, facilitating governance and settlement under British law.
% TRANSFER_FUNCTION: Transfers full legislative authority and ultimate land ownership from Māori to the British Crown, enabling the Crown to govern and allocate resources without requiring ongoing Māori consent.
% ABSENT_VOICES: Māori chiefs and communities advocating for the retention of 'tino rangatiratanga' (full chieftainship/sovereignty) and a partnership model, as well as international legal scholars emphasizing indigenous self-determination, are systematically marginalized or dismissed by this reading.
% DISAPPEARANCE_RATIONALE: If this interpretation of the Treaty vanished overnight, the entire legal and constitutional foundation of New Zealand would be destabilized. Land titles, legislative authority, and the relationship between Māori and the Crown would be thrown into fundamental question, requiring a complete re-negotiation of national identity and governance.
% FOUNDING_PROBLEM: To establish British sovereignty over New Zealand, secure land for European settlement, and impose a unified legal order to manage the growing settler population and interactions with Māori.
% FOUNDING_PROBLEM_CORROBORATION: British colonial records and historical legal judgments corroborate the Crown's stated founding problem. However, Māori oral histories, contemporary scholarship, and the findings of the Waitangi Tribunal strongly contest this framing, arguing that the 'problem' was manufactured to justify colonial expansion and that Māori never ceded full sovereignty.
narrative_ontology:disappearance_verdict(treaty_authority_cession__crown_cession_reading, world_rearranges).
narrative_ontology:founding_problem_status(treaty_authority_cession__crown_cession_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__crown_cession_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(treaty_authority_cession__crown_cession_reading, 'none', 1).
narrative_ontology:epsilon_provenance(treaty_authority_cession__crown_cession_reading, 0.9, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(treaty_authority_cession__crown_cession_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(treaty_authority_cession__crown_cession_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(treaty_authority_cession__crown_cession_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is very high (0.9) as this reading justifies the transfer of vast amounts of land and legislative authority from Māori to the Crown and settler population. Suppression is also very high (0.95) due to the active legal and military enforcement of Crown sovereignty, the invalidation of Māori customary law, and the systemic denial of Māori self-determination. The theater ratio is low (0.2) because, from this reading's perspective, the Crown's actions were direct assertions of power, not primarily performative. Resistance is high (0.7) reflecting continuous Māori opposition and activism against this interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the Crown's historical and legal perspective, this reading establishes legitimate governance and order. From the Māori perspective, it represents a fundamental breach of trust, a denial of sovereignty, and a mechanism for dispossession. The engine's classification will highlight this divergence by computing a snare from the victims' seats, despite the claimed 'coordination' function by beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   The Crown government and settler population are the primary beneficiaries, gaining land, resources, and legislative control (low d). Māori iwi and hapū, along with their customary authority, are the primary targets and victims, losing land, autonomy, and cultural integrity (high d). The judiciary, while an agenda-setter, largely operates within the framework established by this reading, thus reinforcing its extractive nature.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a complete and accurate representation of the ''treaty_authority_cession'' kernel, or is it one specific reading?',
    'Comparison with sibling readings (rangatiratanga_retention_reading, biculturalism_reading, retrospective_snare_exposure) and their structural deltas.',
    'This constraint is explicitly one reading. Its classification is valid within its own frame, but a full understanding requires considering the entire kernel family.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''crown_cession_reading'' of the ''treaty_authority_cession'' kernel.').

omega_variable(
    kawanatanga_interpretation_ambiguity,
    'Does ''kāwanatanga'' in the Māori text genuinely equate to full, unqualified sovereignty as understood in English law, or does it refer to a more limited form of governance?',
    'Linguistic and historical analysis of 19th-century Māori legal concepts, comparison with other contemporary treaties, and expert testimony on indigenous legal traditions.',
    'If ''kāwanatanga'' is found to be limited governance, the premise of full cession collapses, reclassifying the constraint from snare to a more egregious form of extraction or a false mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kawanatanga_interpretation_ambiguity, empirical, 'Ambiguity in the translation and interpretation of ''kāwanatanga''.').

omega_variable(
    legitimacy_of_land_alienation,
    'Is the historical process of land alienation under Crown sovereignty legitimate, given the contested nature of the cession of authority?',
    'Legal review of historical land transactions against principles of informed consent and indigenous property rights, as interpreted by international law and contemporary Māori legal scholarship.',
    'If land alienation is found to be illegitimate, the extraction associated with this reading becomes even more severe, potentially leading to calls for restitution and a re-evaluation of the entire legal framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_land_alienation, conceptual, 'The legitimacy of land alienation under the Crown cession reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__crown_cession_reading, 1840, 1990).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trea_tr_t0, treaty_authority_cession__crown_cession_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(trea_tr_t30, treaty_authority_cession__crown_cession_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement(trea_tr_t60, treaty_authority_cession__crown_cession_reading, theater_ratio, 60, 0.2).
narrative_ontology:measurement(trea_tr_t90, treaty_authority_cession__crown_cession_reading, theater_ratio, 90, 0.2).
narrative_ontology:measurement(trea_tr_t120, treaty_authority_cession__crown_cession_reading, theater_ratio, 120, 0.2).
narrative_ontology:measurement(trea_tr_t150, treaty_authority_cession__crown_cession_reading, theater_ratio, 150, 0.2).

% Extraction over time
narrative_ontology:measurement(trea_be_t0, treaty_authority_cession__crown_cession_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(trea_be_t30, treaty_authority_cession__crown_cession_reading, base_extractiveness, 30, 0.8).
narrative_ontology:measurement(trea_be_t60, treaty_authority_cession__crown_cession_reading, base_extractiveness, 60, 0.85).
narrative_ontology:measurement(trea_be_t90, treaty_authority_cession__crown_cession_reading, base_extractiveness, 90, 0.9).
narrative_ontology:measurement(trea_be_t120, treaty_authority_cession__crown_cession_reading, base_extractiveness, 120, 0.9).
narrative_ontology:measurement(trea_be_t150, treaty_authority_cession__crown_cession_reading, base_extractiveness, 150, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(trea_su_t0, treaty_authority_cession__crown_cession_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(trea_su_t30, treaty_authority_cession__crown_cession_reading, suppression_requirement, 30, 0.88).
narrative_ontology:measurement(trea_su_t60, treaty_authority_cession__crown_cession_reading, suppression_requirement, 60, 0.92).
narrative_ontology:measurement(trea_su_t90, treaty_authority_cession__crown_cession_reading, suppression_requirement, 90, 0.95).
narrative_ontology:measurement(trea_su_t120, treaty_authority_cession__crown_cession_reading, suppression_requirement, 120, 0.95).
narrative_ontology:measurement(trea_su_t150, treaty_authority_cession__crown_cession_reading, suppression_requirement, 150, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treaty_authority_cession__crown_cession_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(treaty_authority_cession__crown_cession_reading, treaty_authority_cession__rangatiratanga_retention_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__crown_cession_reading, treaty_authority_cession__biculturalism_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__crown_cession_reading, treaty_authority_cession__retrospective_snare_exposure).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'treaty_authority_cession' kernel. Its structural claims directly contradict those of its sibling readings, which offer alternative interpretations of the Treaty of Waitangi's legal effect.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
