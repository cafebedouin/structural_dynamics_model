% ============================================================================
% CONSTRAINT STORY: treaty_authority_cession__crown_cession_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   constraint_id: treaty_authority_cession__crown_cession_reading
 *   human_readable: Crown Cession Reading of Te Tiriti o Waitangi
 *   domain: constitutional/indigenous/colonial
 *
 * SUMMARY:
 *   This constraint story instantiates the crown_cession_reading of Te Tiriti
 *   o Waitangi / the Treaty of Waitangi (1840). The reading holds that the
 *   English text controls; that 'kāwanatanga' in Article 1 cedes full
 *   sovereignty to the Crown; and that the treaty completes a legal cession
 *   of authority, making Crown law supreme and Māori customary authority
 *   subordinate. The structural delta is a wall enclosing land and
 *   legislative authority under the Crown, legitimating land alienation. The
 *   claimed_type is 'rope' — the reading presents the treaty as a legitimate
 *   coordination mechanism transferring sovereignty — but the authored
 *   metrics describe a constraint that operates with high extractiveness
 *   (0.75), high suppression (0.8), and identity-locked extraction from
 *   Māori. This claim/metric divergence is deliberate: the engine measures
 *   the gap between the reading's self-presentation and its structural
 *   operation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__crown_cession_reading, 0.75).
domain_priors:suppression_score(treaty_authority_cession__crown_cession_reading, 0.8).
domain_priors:theater_ratio(treaty_authority_cession__crown_cession_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__crown_cession_reading, rope).
narrative_ontology:human_readable(treaty_authority_cession__crown_cession_reading, "Crown Cession Reading of Te Tiriti o Waitangi").
narrative_ontology:topic_domain(treaty_authority_cession__crown_cession_reading, "constitutional/indigenous/colonial").

domain_priors:requires_active_enforcement(treaty_authority_cession__crown_cession_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__crown_cession_reading, '0262e0b6-6810-4b0f-a391-20db2a668dc6').
narrative_ontology:cs_kernel_codification('0262e0b6-6810-4b0f-a391-20db2a668dc6', fixed_text).
narrative_ontology:cs_authority_grounding('0262e0b6-6810-4b0f-a391-20db2a668dc6', lineage).
narrative_ontology:cs_interpretation_layer_present('0262e0b6-6810-4b0f-a391-20db2a668dc6').
narrative_ontology:cs_reading_relation('0262e0b6-6810-4b0f-a391-20db2a668dc6', treaty_authority_cession__rangatiratanga_retention_reading, coexists_with).
narrative_ontology:cs_reading_relation('0262e0b6-6810-4b0f-a391-20db2a668dc6', treaty_authority_cession__retrospective_snare_exposure, forecloses).
narrative_ontology:cs_axiom('0262e0b6-6810-4b0f-a391-20db2a668dc6', foundational, english_text_controlling).
narrative_ontology:cs_axiom_status(english_text_controlling, holdable).
narrative_ontology:cs_axiom_grounding('0262e0b6-6810-4b0f-a391-20db2a668dc6', english_text_controlling, conventional).
narrative_ontology:cs_axiom('0262e0b6-6810-4b0f-a391-20db2a668dc6', foundational, full_sovereignty_cession).
narrative_ontology:cs_axiom_status(full_sovereignty_cession, holdable).
narrative_ontology:cs_axiom_grounding('0262e0b6-6810-4b0f-a391-20db2a668dc6', full_sovereignty_cession, conventional).
narrative_ontology:cs_axiom('0262e0b6-6810-4b0f-a391-20db2a668dc6', secondary, cession_completes_legal_transfer).
narrative_ontology:cs_axiom_status(cession_completes_legal_transfer, holdable).
narrative_ontology:cs_axiom_grounding('0262e0b6-6810-4b0f-a391-20db2a668dc6', cession_completes_legal_transfer, conventional).
narrative_ontology:cs_reference_frame('0262e0b6-6810-4b0f-a391-20db2a668dc6', imperial_cession_framework).
narrative_ontology:cs_drift_state('0262e0b6-6810-4b0f-a391-20db2a668dc6', contemporary_treaty_settlement_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0262e0b6-6810-4b0f-a391-20db2a668dc6', '').
narrative_ontology:cs_kernel_id(treaty_authority_cession__crown_cession_reading, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treaty_authority_cession__crown_cession_reading, crown_government).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__crown_cession_reading, pakeha_settlers).
narrative_ontology:constraint_victim(treaty_authority_cession__crown_cession_reading, maori_hapu_iwi).
narrative_ontology:constraint_vindicates(treaty_authority_cession__crown_cession_reading, parliamentary_supremacy_doctrine).
narrative_ontology:constraint_vindicates(treaty_authority_cession__crown_cession_reading, cession_completes_legal_transfer).
narrative_ontology:constraint_vindicates(treaty_authority_cession__crown_cession_reading, english_text_controlling).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the treaty as the foundational legal instrument transferring full sovereignty to the Crown. Sets legislative and policy frameworks that treat Crown authority as plenary and Māori customary authority as subordinate. Collects the benefits of undisputed legislative control and land title derived from the cession narrative. Exit would mean abandoning the constitutional foundation of the state.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, crown_government, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(treaty_authority_cession__crown_cession_reading, crown_government, beneficiary).

% Bear the costs of the cession reading: loss of rangatiratanga (chiefly authority), alienation of land through Crown purchase and raupatu (confiscation), subordination of tikanga (customary law) to Crown law. Their identity as tangata whenua is constituted through the relationship to whenua (land) and mana motuhake (self-determination); exit from the constraint would require abandoning the collective identity formed through generations of resistance and adaptation. The constraint extracts authority, land, and jurisdictional space while offering no effective exit.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, maori_hapu_iwi, payer,
    organized, generational, identity_locked, national).

% Gain secure land title, access to resources, and the protections of Crown law derived from the cession narrative. The constraint's operation enables their property rights and political participation. Exit is mobile — they could emigrate — but the constraint's benefits are deeply embedded in their material and civic position.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, pakeha_settlers, beneficiary,
    organized, biographical, mobile, national).

% Investigates Treaty claims and issues reports interpreting both texts. Has no binding enforcement power but shapes public and political discourse. Its readings have progressively undermined the crown_cession_reading's dominance, recommending redress that treats the cession as incomplete or qualified. It sits analytically but its findings create structural pressure on the Crown.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, waitangi_tribunal, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(treaty_authority_cession__crown_cession_reading, waitangi_tribunal, agenda_setter).

% Apply international indigenous rights frameworks (UNDRIP, ILO 169) and treaty interpretation principles (contra proferentem, Vienna Convention) to assess the Crown's compliance. Their analyses consistently find the crown_cession_reading incompatible with the Māori text and the principles of free, prior, and informed consent. They exert reputational and diplomatic pressure but no domestic enforcement.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, international_law_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single sovereign authority (the Crown) to govern European settlement, prevent inter-settler conflict, and provide a legal framework for land acquisition — solving the coordination problem of orderly colonization.
% TRANSFER_FUNCTION: Moves legislative authority (kāwanatanga as full sovereignty) and radical title to land from Māori signatories to the Crown, in exchange for the Crown's protection and the guarantee of Māori possession of their lands and taonga (Article 2 English text). The transfer is framed as complete and irrevocable.
% ABSENT_VOICES: The Māori signatories who signed te reo Māori text and understood kāwanatanga as limited governance while retaining tino rangatiratanga; their descendants who bear the intergenerational consequences of the English-text reading; future generations of both peoples who inherit the constitutional structure.
% DISAPPEARANCE_RATIONALE: If the crown_cession_reading vanished overnight, the Crown's claim to plenary legislative authority over Aotearoa would lose its foundational treaty basis. Māori customary authority (tino rangatiratanga) would reassert as the default jurisdictional framework. Land titles derived from Crown grants would face fundamental legitimacy challenges. The entire constitutional architecture of the state would require reconstruction.
% FOUNDING_PROBLEM: The Crown needed a legitimate basis to govern British subjects settling in New Zealand, prevent lawlessness and conflict between settlers and Māori, and acquire land for settlement without triggering wider war — all while maintaining British imperial prestige and pre-empting French or American claims.
% FOUNDING_PROBLEM_CORROBORATION: Crown historians and constitutional lawyers attest the founding problem remains live: the Crown's sovereignty is the settled constitutional fact enabling stable governance. Waitangi Tribunal findings (e.g., Te Paparahi o Te Raki / Northland Inquiry, Stage 1 Report 2014) and Māori oral history attest the founding problem was misrepresented — chiefs ceded kāwanatanga (governance) not mana (authority), and the problem of lawless settlers was solved by the Māori text's partnership framework, not by Crown sovereignty. The Crown's own 2019 Cabinet Office circular acknowledges 'the Treaty is a founding document of government in New Zealand' but does not resolve the status contest.
narrative_ontology:disappearance_verdict(treaty_authority_cession__crown_cession_reading, world_rearranges).
narrative_ontology:founding_problem_status(treaty_authority_cession__crown_cession_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__crown_cession_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(treaty_authority_cession__crown_cession_reading, 'none', 1).
narrative_ontology:epsilon_provenance(treaty_authority_cession__crown_cession_reading, 0.75, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is high because the constraint transfers authority, land, and jurisdictional space from Māori to the Crown and settlers without reciprocity — the Article 2 guarantee of 'undisturbed possession' was systematically breached through raupatu, native land court individualization, and Crown purchase monopsony. Suppression is high because the constraint's persistence depends on active enforcement: Crown law displacing tikanga, Native Land Court converting customary title to alienable freehold, police and military enforcing Crown authority (e.g., Parihaka 1881, Bastion Point 1978). Theater ratio is moderate (0.3) — the partnership rhetoric of the 1980s–2000s (Treaty principles, 'honourable kāwanatanga') performs inclusion while the structural extraction continues. Accessibility collapse (0.7) reflects the near-total foreclosure of Māori sovereign alternatives within the Crown legal order. Resistance (0.6) is sustained: from the 1860s wars through the 1975 land march, 1980s Waitangi Tribunal claims, 2004 foreshore and seabed hikoi, to contemporary constitutional transformation movements.
 *
 * PERSPECTIVAL GAP:
 *   The Crown seat experiences the constraint as rope — a legitimate, consensual coordination that founded the state. The Māori payer seat experiences it as snare — extraction enforced through law, military, and bureaucratic machinery, with no exit that preserves identity. The engine computes this divergence from the structural data: same constraint, opposite classifications. The crown_cession_reading's claim of 'rope' is the Crown's self-presentation; the metrics describe what the constraint does to Māori.
 *
 * DIRECTIONALITY LOGIC:
 *   Crown government is the structural agenda-setter and beneficiary (d ~0.1): it administers the constraint, collects legislative plenitude and land value, and has arbitrage-grade exit (could theoretically abandon the treaty basis but would lose constitutional legitimacy). Māori hapū/iwi are payers with identity-locked exit (d ~0.95): their collective identity as tangata whenua is constituted through the relationship to whenua and mana motuhake that the constraint extracts; exit would mean ontological rupture. Pākehā settlers are beneficiaries with mobile exit (d ~0.2): they gain secure property and civic inclusion but could emigrate. Waitangi Tribunal and international observers are analytical seats (d ~0.5): they neither collect nor pay but their analyses shift the legitimacy conditions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legitimate governance of settlement) was real in 1840. The Crown claims it remains live — without Crown sovereignty, no stable legal order. Māori and the Waitangi Tribunal contend the problem was solved differently: the Māori text's kāwanatanga/tino rangatiratanga partnership provided for orderly governance without ceding mana. The arrangement persists not because the founding problem requires this solution, but because the Crown's constitutional legitimacy now depends on the cession narrative being true. Mandatrophy is unresolved: the constraint's mandate has outlived its function but the cost of admitting this (constitutional reconstruction) is prohibitive for the Crown.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_authority_ambiguity,
    'Which text of Te Tiriti — the English draft or the Māori text signed by over 500 chiefs — holds interpretive authority under international treaty law and the principles of contra proferentem?',
    'Authoritative determination by an impartial international tribunal applying Vienna Convention Articles 31-33 and the contra proferentem principle to a bilingual treaty where the non-drafting party signed only the Māori text.',
    'If Māori text controls, kāwanatanga ≠ sovereignty cession; the crown_cession_reading''s foundational premise collapses and the constraint reclassifies as snare from all seats. If English text controls, the reading''s coordination claim gains legal traction but the extraction metrics remain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_authority_ambiguity, conceptual, 'Which treaty text governs — the drafter''s English version or the signatories'' Māori version.').

omega_variable(
    consent_validity,
    'Did the rangatira who signed the Māori text understand and freely assent to a transfer of full sovereignty (kāwanatanga = absolute Crown authority), or did they understand kāwanatanga as a limited governance role for the Crown over settlers while retaining tino rangatiratanga?',
    'Historical-linguistic analysis of 1840 Māori political concepts (kawanatanga as governorship, not sovereignty; rangatiratanga as chiefly authority; the missionary translation choices of Henry Williams); oral history of iwi/hapū; the fact that the English text was never presented for signature.',
    'If consent to full sovereignty was absent, the cession is a legal fiction masking extraction — the constraint is a snare operating under mistranslation. If consent was present, the coordination claim has factual grounding (though extraction metrics still apply).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consent_validity, empirical, 'Whether the signatories'' consent matched the English-text sovereignty claim.').

omega_variable(
    contemporary_legitimacy,
    'Can the Crown''s contemporary constitutional legitimacy be maintained if the crown_cession_reading is abandoned in favor of a partnership or Māori sovereignty framework?',
    'Constitutional transformation process (e.g., Matike Mai Aotearoa report 2016) demonstrating a workable alternative framework; political settlement between Crown and Māori; international precedent (Canada''s Section 35, US tribal sovereignty).',
    'If legitimacy survives transformation, the Crown''s resistance to reclassification is political inertia, not structural necessity — supporting piton or scaffold dynamics. If legitimacy collapses, the constraint is a mountain for the Crown (identity-locked, no exit) — explaining the high theater and suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contemporary_legitimacy, preference, 'Whether Crown constitutional identity requires the cession narrative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__crown_cession_reading, 0, 184).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tac_cr_tr_t0, treaty_authority_cession__crown_cession_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(tac_cr_tr_t40, treaty_authority_cession__crown_cession_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement(tac_cr_tr_t80, treaty_authority_cession__crown_cession_reading, theater_ratio, 80, 0.25).
narrative_ontology:measurement(tac_cr_tr_t120, treaty_authority_cession__crown_cession_reading, theater_ratio, 120, 0.35).
narrative_ontology:measurement(tac_cr_tr_t160, treaty_authority_cession__crown_cession_reading, theater_ratio, 160, 0.32).
narrative_ontology:measurement(tac_cr_tr_t184, treaty_authority_cession__crown_cession_reading, theater_ratio, 184, 0.3).

% Extraction over time
narrative_ontology:measurement(tac_cr_be_t0, treaty_authority_cession__crown_cession_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(tac_cr_be_t40, treaty_authority_cession__crown_cession_reading, base_extractiveness, 40, 0.72).
narrative_ontology:measurement(tac_cr_be_t80, treaty_authority_cession__crown_cession_reading, base_extractiveness, 80, 0.78).
narrative_ontology:measurement(tac_cr_be_t120, treaty_authority_cession__crown_cession_reading, base_extractiveness, 120, 0.8).
narrative_ontology:measurement(tac_cr_be_t160, treaty_authority_cession__crown_cession_reading, base_extractiveness, 160, 0.76).
narrative_ontology:measurement(tac_cr_be_t184, treaty_authority_cession__crown_cession_reading, base_extractiveness, 184, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(tac_cr_su_t0, treaty_authority_cession__crown_cession_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(tac_cr_su_t40, treaty_authority_cession__crown_cession_reading, suppression_requirement, 40, 0.85).
narrative_ontology:measurement(tac_cr_su_t80, treaty_authority_cession__crown_cession_reading, suppression_requirement, 80, 0.9).
narrative_ontology:measurement(tac_cr_su_t120, treaty_authority_cession__crown_cession_reading, suppression_requirement, 120, 0.85).
narrative_ontology:measurement(tac_cr_su_t160, treaty_authority_cession__crown_cession_reading, suppression_requirement, 160, 0.75).
narrative_ontology:measurement(tac_cr_su_t184, treaty_authority_cession__crown_cession_reading, suppression_requirement, 184, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treaty_authority_cession__crown_cession_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(treaty_authority_cession__crown_cession_reading, 0.1).
narrative_ontology:affects_constraint(treaty_authority_cession__crown_cession_reading, treaty_authority_cession__rangatiratanga_retention_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__crown_cession_reading, treaty_authority_cession__retrospective_snare_exposure).
narrative_ontology:affects_constraint(treaty_authority_cession__crown_cession_reading, native_land_court_individualization).
narrative_ontology:affects_constraint(treaty_authority_cession__crown_cession_reading, raupatu_confiscation_regime).
narrative_ontology:affects_constraint(treaty_authority_cession__crown_cession_reading, foreshore_seabed_act_2004).

% DUAL FORMULATION NOTE:
% Kernel 'treaty_authority_cession' decomposes into three readings with divergent ε: crown_cession_reading (ε=0.75, claimed rope), rangatiratanga_retention_reading (ε=0.3, claimed tangled_rope), retrospective_snare_exposure (ε=0.9, claimed snare). The ε-invariance principle requires separate stories because the same label 'Treaty of Waitangi' covers structurally distinct claims with different extraction profiles, different victim sets, and different empirical status.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(treaty_authority_cession__crown_cession_reading, organized, 0.95).
constraint_indexing:directionality_override(treaty_authority_cession__crown_cession_reading, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
