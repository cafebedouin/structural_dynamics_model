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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   This constraint story instantiates the 'Crown Cession Reading' of the
 *   Treaty of Waitangi, where the English text is paramount, 'kāwanatanga' is
 *   interpreted as full sovereignty, and the Treaty is seen as completing a
 *   legal cession of authority to the British Crown. This reading
 *   fundamentally underpins the historical and ongoing legal framework of New
 *   Zealand, justifying Crown legislative supremacy and land alienation. It
 *   is a snare because it extracts sovereignty and resources from Māori under
 *   the guise of a legitimate agreement, requiring active suppression of
 *   alternative interpretations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__crown_cession_reading, 0.85).
domain_priors:suppression_score(treaty_authority_cession__crown_cession_reading, 0.9).
domain_priors:theater_ratio(treaty_authority_cession__crown_cession_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__crown_cession_reading, snare).
narrative_ontology:human_readable(treaty_authority_cession__crown_cession_reading, "Treaty of Waitangi: Crown Cession Reading").
narrative_ontology:topic_domain(treaty_authority_cession__crown_cession_reading, "constitutional_law/indigenous_rights/colonial_history").

domain_priors:requires_active_enforcement(treaty_authority_cession__crown_cession_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__crown_cession_reading, '70fcb9f2-a65b-4a62-ae30-9125dc151ec5').
narrative_ontology:cs_kernel_codification('70fcb9f2-a65b-4a62-ae30-9125dc151ec5', fixed_text).
narrative_ontology:cs_authority_grounding('70fcb9f2-a65b-4a62-ae30-9125dc151ec5', extraction).
narrative_ontology:cs_interpretation_layer_present('70fcb9f2-a65b-4a62-ae30-9125dc151ec5').
narrative_ontology:cs_reading_relation('70fcb9f2-a65b-4a62-ae30-9125dc151ec5', treaty_authority_cession__rangatiratanga_retention_reading, forecloses).
narrative_ontology:cs_reading_relation('70fcb9f2-a65b-4a62-ae30-9125dc151ec5', treaty_authority_cession__biculturalism_reading, influences).
narrative_ontology:cs_reading_relation('70fcb9f2-a65b-4a62-ae30-9125dc151ec5', treaty_authority_cession__retrospective_snare_exposure, coexists_with).
narrative_ontology:cs_axiom('70fcb9f2-a65b-4a62-ae30-9125dc151ec5', foundational, english_text_primacy).
narrative_ontology:cs_axiom_status(english_text_primacy, holdable).
narrative_ontology:cs_axiom_grounding('70fcb9f2-a65b-4a62-ae30-9125dc151ec5', english_text_primacy, conventional).
narrative_ontology:cs_axiom('70fcb9f2-a65b-4a62-ae30-9125dc151ec5', foundational, kawanatanga_equals_full_sovereignty).
narrative_ontology:cs_axiom_status(kawanatanga_equals_full_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('70fcb9f2-a65b-4a62-ae30-9125dc151ec5', kawanatanga_equals_full_sovereignty, conventional).
narrative_ontology:cs_reference_frame('70fcb9f2-a65b-4a62-ae30-9125dc151ec5', unqualified_crown_sovereignty).
narrative_ontology:cs_drift_state('70fcb9f2-a65b-4a62-ae30-9125dc151ec5', contemporary_treaty_settlements_era, gap(revival_pressure, minor, false)).
narrative_ontology:cs_created_at('70fcb9f2-a65b-4a62-ae30-9125dc151ec5', '').
narrative_ontology:cs_kernel_id(treaty_authority_cession__crown_cession_reading, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treaty_authority_cession__crown_cession_reading, new_zealand_crown).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__crown_cession_reading, settler_population).
narrative_ontology:constraint_victim(treaty_authority_cession__crown_cession_reading, maori_iwi_hapu).
narrative_ontology:constraint_victim(treaty_authority_cession__crown_cession_reading, maori_customary_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Treaty as a full cession of sovereignty, granting the Crown absolute authority over New Zealand. Benefits from the legal justification for land acquisition and legislative supremacy. Actively enforces this interpretation through legal and administrative means.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, new_zealand_crown, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefits from the Crown's asserted sovereignty, which facilitated land settlement, resource exploitation, and the establishment of a Westminster-style legal system. Their prosperity is historically tied to this interpretation.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, settler_population, beneficiary,
    organized, generational, mobile, national).

% Are the primary victims of this reading, experiencing the loss of land, resources, and customary authority. Their attempts to assert tino rangatiratanga (full chieftainship) are systematically suppressed by the Crown's interpretation. Their identity is deeply tied to their ancestral lands and self-governance.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, maori_iwi_hapu, payer,
    powerless, generational, identity_locked, local).

% Under this reading, Māori customary authority is deemed extinguished or subordinate to Crown sovereignty, effectively removing it from the legitimate legal and political discourse. Its voice is absent from the dominant legal framework.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, maori_customary_authority, excluded,
    powerless, civilizational, trapped, national).
narrative_ontology:stakeholder_non_agent(treaty_authority_cession__crown_cession_reading, maori_customary_authority).

% Interprets and applies the Treaty within the framework of Crown sovereignty, often upholding the English text's primacy. While theoretically independent, its decisions have historically reinforced the Crown's position, though some modern rulings have acknowledged Māori grievances.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, new_zealand_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From the Crown's perspective, it established a single, unified legal and administrative system for all inhabitants of New Zealand, coordinating governance under one sovereign authority.
% TRANSFER_FUNCTION: Transfers full legislative and territorial sovereignty from Māori chiefs to the British Crown, enabling the Crown to govern, acquire land, and establish a settler state.
% ABSENT_VOICES: Māori chiefs who signed the Māori text of the Treaty, believing they were retaining tino rangatiratanga, are absent from the Crown's interpretation. Their understanding of retained sovereignty and partnership is systematically excluded.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the entire constitutional and legal framework of New Zealand would be fundamentally challenged. Land titles, legislative authority, and the relationship between Māori and the Crown would require renegotiation, leading to a profound reorganization of the state.
% FOUNDING_PROBLEM: The British Crown sought to establish legitimate authority over New Zealand to protect Māori from unscrupulous traders, prevent other European powers from claiming the territory, and facilitate organized settlement.
% FOUNDING_PROBLEM_CORROBORATION: The Crown and settler population historically attested to this problem. Māori and contemporary historians contest this, arguing the primary driver was colonial expansion and resource acquisition, with protection as a secondary or rhetorical justification. Independent historical analysis from outside the benefiting parties supports the contested status.
narrative_ontology:disappearance_verdict(treaty_authority_cession__crown_cession_reading, world_rearranges).
narrative_ontology:founding_problem_status(treaty_authority_cession__crown_cession_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__crown_cession_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(treaty_authority_cession__crown_cession_reading, 'none', 1).
narrative_ontology:epsilon_provenance(treaty_authority_cession__crown_cession_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.85) due to the massive transfer of land and legislative power from Māori to the Crown. Suppression is also very high (0.9) as this reading requires active legal and political suppression of Māori claims to retained sovereignty and customary authority. The theater ratio (0.4) reflects that while the Crown maintains a narrative of 'Treaty partnership,' the practical effect of this reading is often performative rather than genuinely collaborative, especially concerning Māori self-determination. Accessibility collapse is high (0.9) because this reading structurally forecloses most avenues for Māori to assert their original understanding of the Treaty within the dominant legal system. Resistance is high (0.7) due to continuous Māori activism, legal challenges, and political movements against this interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the Crown's perspective, this reading is a legitimate foundation for the nation-state, a 'rope' of orderly governance. From Māori perspectives, it is a 'snare' that dispossessed them of land and authority. The engine will compute this divergence based on the declared beneficiary/victim structure and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The New Zealand Crown and settler population are clear beneficiaries, gaining sovereignty, land, and resources. Māori iwi and hapū are the primary victims, losing these same elements. The judiciary, while an agenda-setter, has historically reinforced the Crown's directionality. Māori customary authority is structurally excluded and suppressed.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling a fundamentally extractive and suppressive interpretation as mere 'coordination' or 'historical fact.' By identifying it as a snare, the framework highlights the ongoing coercion and suppression required to maintain this reading, despite its contested founding problem status. The high extractiveness and suppression, coupled with the contested founding problem, point to a constraint whose original 'coordination' function (from the Crown's perspective) has long been overshadowed by its extractive operation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_primacy_ambiguity,
    'Which text of the Treaty of Waitangi (English or Māori) holds legal primacy in contemporary New Zealand law?',
    'A definitive Supreme Court ruling or constitutional amendment explicitly clarifying textual primacy.',
    'If the Māori text were given primacy, the ''Crown Cession Reading'' would be severely undermined, leading to a re-evaluation of sovereignty and Māori rights, potentially reclassifying the constraint as a ''tangled_rope'' or ''rope'' from a Māori perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_primacy_ambiguity, conceptual, 'Ambiguity over which Treaty text is legally authoritative.').

omega_variable(
    kawanatanga_sovereignty_scope,
    'What was the intended scope of ''kāwanatanga'' (governance) as understood by Māori chiefs who signed the Treaty, and does it equate to full British sovereignty?',
    'Further historical and linguistic scholarship, combined with legal recognition of Māori customary law and interpretations.',
    'If ''kāwanatanga'' is found to be limited governance, not full sovereignty, the Crown''s claim to absolute authority would be weakened, shifting the constraint towards a ''tangled_rope'' or ''scaffold'' as a transitional arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kawanatanga_sovereignty_scope, empirical, 'Dispute over the meaning of ''kāwanatanga'' in the Treaty.').

omega_variable(
    mandatrophy_of_protection,
    'Has the Crown''s ''protective'' mandate, cited as a founding problem, atrophied, leaving only the extractive elements of the ''cession'' reading?',
    'Independent audit of Crown actions demonstrating whether protection of Māori interests is genuinely prioritized over other state interests, or if it is primarily rhetorical cover for continued extraction.',
    'If the protective mandate is found to be dead, the constraint''s classification as a ''snare'' would be further solidified, as its coordination justification would be entirely gone.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandatrophy_of_protection, empirical, 'Whether the Crown''s original protective mandate is still active.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__crown_cession_reading, 1840, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trea_tr_t1840, treaty_authority_cession__crown_cession_reading, theater_ratio, 1840, 0.2).
narrative_ontology:measurement(trea_tr_t1890, treaty_authority_cession__crown_cession_reading, theater_ratio, 1890, 0.3).
narrative_ontology:measurement(trea_tr_t1940, treaty_authority_cession__crown_cession_reading, theater_ratio, 1940, 0.4).
narrative_ontology:measurement(trea_tr_t1990, treaty_authority_cession__crown_cession_reading, theater_ratio, 1990, 0.45).
narrative_ontology:measurement(trea_tr_t2024, treaty_authority_cession__crown_cession_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(trea_be_t1840, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1840, 0.7).
narrative_ontology:measurement(trea_be_t1890, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1890, 0.85).
narrative_ontology:measurement(trea_be_t1940, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1940, 0.9).
narrative_ontology:measurement(trea_be_t1990, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1990, 0.88).
narrative_ontology:measurement(trea_be_t2024, treaty_authority_cession__crown_cession_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(trea_su_t1840, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1840, 0.75).
narrative_ontology:measurement(trea_su_t1890, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1890, 0.9).
narrative_ontology:measurement(trea_su_t1940, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1940, 0.95).
narrative_ontology:measurement(trea_su_t1990, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1990, 0.85).
narrative_ontology:measurement(trea_su_t2024, treaty_authority_cession__crown_cession_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treaty_authority_cession__crown_cession_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(treaty_authority_cession__crown_cession_reading, new_zealand_land_ownership_regime).
narrative_ontology:affects_constraint(treaty_authority_cession__crown_cession_reading, maori_language_revitalization_policy).
narrative_ontology:affects_constraint(treaty_authority_cession__crown_cession_reading, new_zealand_constitutional_framework).

% DUAL FORMULATION NOTE:
% This is one reading of the 'treaty_authority_cession' kernel. Other readings include 'rangatiratanga_retention_reading' and 'biculturalism_reading', which offer alternative interpretations of sovereignty and partnership.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
