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
 *   Waitangi, where the English text is deemed authoritative, 'kāwanatanga'
 *   is interpreted as full sovereignty, and the Treaty is understood as a
 *   legal cession of Māori authority to the British Crown. This reading
 *   underpins the historical and ongoing legislative and land management
 *   practices of the New Zealand government, leading to the extinguishment or
 *   subordination of Māori customary authority and the legitimization of land
 *   alienation. The constraint is classified as a Snare due to its high
 *   extractiveness and suppression, which are actively maintained to uphold
 *   the settler state's legal framework.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__crown_cession_reading, 0.85).
domain_priors:suppression_score(treaty_authority_cession__crown_cession_reading, 0.92).
domain_priors:theater_ratio(treaty_authority_cession__crown_cession_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__crown_cession_reading, snare).
narrative_ontology:human_readable(treaty_authority_cession__crown_cession_reading, "Treaty of Waitangi: Crown Cession Reading").
narrative_ontology:topic_domain(treaty_authority_cession__crown_cession_reading, "constitutional_law/indigenous_rights/colonial_history").

domain_priors:requires_active_enforcement(treaty_authority_cession__crown_cession_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__crown_cession_reading, 'ab67fa97-0c75-45db-b54f-82b1a47005c6').
narrative_ontology:cs_kernel_codification('ab67fa97-0c75-45db-b54f-82b1a47005c6', fixed_text).
narrative_ontology:cs_authority_grounding('ab67fa97-0c75-45db-b54f-82b1a47005c6', lineage).
narrative_ontology:cs_interpretation_layer_present('ab67fa97-0c75-45db-b54f-82b1a47005c6').
narrative_ontology:cs_reading_relation('ab67fa97-0c75-45db-b54f-82b1a47005c6', treaty_authority_cession__rangatiratanga_retention_reading, forecloses).
narrative_ontology:cs_reading_relation('ab67fa97-0c75-45db-b54f-82b1a47005c6', treaty_authority_cession__biculturalism_reading, influences).
narrative_ontology:cs_reading_relation('ab67fa97-0c75-45db-b54f-82b1a47005c6', treaty_authority_cession__retrospective_snare_exposure, coexists_with).
narrative_ontology:cs_axiom('ab67fa97-0c75-45db-b54f-82b1a47005c6', foundational, english_text_supremacy).
narrative_ontology:cs_axiom_status(english_text_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('ab67fa97-0c75-45db-b54f-82b1a47005c6', english_text_supremacy, conventional).
narrative_ontology:cs_axiom('ab67fa97-0c75-45db-b54f-82b1a47005c6', foundational, kawanatanga_equals_sovereignty).
narrative_ontology:cs_axiom_status(kawanatanga_equals_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('ab67fa97-0c75-45db-b54f-82b1a47005c6', kawanatanga_equals_sovereignty, conventional).
narrative_ontology:cs_reference_frame('ab67fa97-0c75-45db-b54f-82b1a47005c6', british_parliamentary_supremacy).
narrative_ontology:cs_drift_state('ab67fa97-0c75-45db-b54f-82b1a47005c6', contemporary_waitangi_tribunal_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ab67fa97-0c75-45db-b54f-82b1a47005c6', '').
narrative_ontology:cs_kernel_id(treaty_authority_cession__crown_cession_reading, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treaty_authority_cession__crown_cession_reading, new_zealand_crown).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__crown_cession_reading, settler_government).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__crown_cession_reading, pakeha_landowners).
narrative_ontology:constraint_victim(treaty_authority_cession__crown_cession_reading, maori_iwi).
narrative_ontology:constraint_victim(treaty_authority_cession__crown_cession_reading, maori_hapu).
narrative_ontology:constraint_victim(treaty_authority_cession__crown_cession_reading, maori_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts full sovereignty over New Zealand based on the English text of the Treaty of Waitangi, interpreting 'kāwanatanga' as a full cession of governance and legislative authority. Benefits from the legitimacy this interpretation provides for land acquisition and legislative control.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, new_zealand_crown, agenda_setter,
    institutional, generational, arbitrage, national).

% Operates under the legal framework established by the Crown cession reading, enacting legislation and policies that assume parliamentary supremacy and the extinguishment or subordination of Māori customary law. Benefits from unchallenged legislative authority and the ability to manage resources without Māori consent.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, settler_government, agenda_setter,
    institutional, generational, mobile, national).

% Hold land titles derived from Crown grants, which are legitimized by the cession reading of the Treaty. Their property rights are secured by the assumption that Māori customary title was extinguished or validly transferred to the Crown.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, pakeha_landowners, beneficiary,
    powerful, generational, mobile, local).

% Experience their traditional lands and resources being alienated and their customary authority (tino rangatiratanga) overridden by Crown legislation. They are bound to the Treaty as a foundational document but suffer under the cession reading, which denies their inherent sovereignty and self-determination.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, maori_iwi, payer,
    organized, generational, identity_locked, regional).

% Bear the direct impact of land loss and the erosion of their social and political structures due to the Crown's assertion of sovereignty. Their ability to exercise local governance and resource management is severely constrained by the cession reading.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, maori_hapu, payer,
    moderate, generational, identity_locked, local).

% Live under a legal system that prioritizes Crown sovereignty, leading to systemic disadvantages in areas like education, health, and economic opportunity. Their cultural identity and connection to ancestral lands are undermined by the legal framework derived from the cession reading.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, maori_citizens, payer,
    powerless, biographical, identity_locked, national).

% Monitor New Zealand's compliance with international indigenous rights standards, often critiquing the historical and ongoing impacts of the Crown's interpretation of the Treaty. Their observations provide external pressure but do not directly alter the domestic legal framework.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, unified legal and governmental authority over the territory of New Zealand, facilitating the administration of a colonial state and the integration of settler populations under British law.
% TRANSFER_FUNCTION: Transfers legislative authority and ultimate title to land and resources from Māori chiefs (as interpreted by the Crown) to the British Crown, enabling the Crown to grant land to settlers and govern without Māori consent.
% ABSENT_VOICES: Māori chiefs who signed the Māori text of the Treaty, believing they were ceding only governance (kāwanatanga) while retaining full authority (tino rangatiratanga) over their lands and people, are absent from the legal and political discourse that upholds the Crown cession reading. Their understanding of the Treaty is systematically marginalized.
% DISAPPEARANCE_RATIONALE: If the Crown cession reading vanished, the entire legal and constitutional foundation of New Zealand would be destabilized. Land titles would be contested, parliamentary supremacy would be challenged, and the relationship between Māori and the Crown would require fundamental renegotiation, leading to a profound reorganization of the state.
% FOUNDING_PROBLEM: The British Crown sought to establish sovereignty over New Zealand to manage increasing British settlement, prevent other European powers from claiming territory, and regulate land transactions, aiming to create a stable colonial administration.
% FOUNDING_PROBLEM_CORROBORATION: While the Crown (and its successor, the New Zealand government) maintains the problem of unified governance is live, independent historians and Māori scholars widely corroborate that the original problem of establishing a stable colonial administration was solved, and the current persistence of the cession reading serves to maintain existing power structures and land ownership patterns, rather than addressing the original colonial imperative. The Waitangi Tribunal's findings also corroborate the historical context of the Crown's motivations.
narrative_ontology:disappearance_verdict(treaty_authority_cession__crown_cession_reading, world_rearranges).
narrative_ontology:founding_problem_status(treaty_authority_cession__crown_cession_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__crown_cession_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(treaty_authority_cession__crown_cession_reading, 'none', 1).

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
 *   Extractiveness is high (0.85) because this reading enables the ongoing transfer of land, resources, and legislative power from Māori to the Crown and settler population. Suppression is very high (0.92) as it requires active legal and political enforcement to deny Māori claims to tino rangatiratanga and to suppress alternative interpretations of the Treaty. The theater ratio (0.4) reflects that while the Treaty is often invoked as a founding document, its 'cession' interpretation serves more to legitimize existing power structures than to genuinely coordinate relations between Māori and the Crown. Resistance is high (0.75) due to continuous Māori activism, legal challenges, and political movements against this interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Crown and settler government, this reading provides a stable, legitimate foundation for the state. From the Māori perspective, it is a foundational injustice that enables ongoing extraction and suppression. The engine's classification will highlight this divergence by computing a Snare classification for Māori seats, while the Crown's seat might compute as a Rope or Tangled Rope if only its coordination function is considered in isolation.
 *
 * DIRECTIONALITY LOGIC:
 *   The New Zealand Crown and settler government are clear agenda-setters and beneficiaries, deriving their authority and control over resources from this reading. Pakeha landowners also benefit from secure land titles. Māori iwi, hapu, and individual citizens are the primary payers/victims, experiencing the loss of land, resources, and self-determination. International human rights bodies act as observers, providing external critique.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_authority_ambiguity,
    'Which text of the Treaty of Waitangi (English or Māori) holds ultimate legal and moral authority?',
    'A definitive judicial ruling or constitutional amendment that explicitly prioritizes one text over the other, or establishes a framework for reconciling their differences.',
    'If the Māori text were prioritized, this ''Crown cession'' reading would be fundamentally undermined, leading to a reclassification towards a partnership or retention model. If the English text''s supremacy is reaffirmed, the Snare classification would be further entrenched.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_authority_ambiguity, conceptual, 'Ambiguity regarding the authoritative text of the Treaty.').

omega_variable(
    kawanatanga_meaning_ambiguity,
    'Does ''kāwanatanga'' (as understood by Māori chiefs in 1840) equate to full sovereignty or a more limited form of governance?',
    'Further historical and linguistic scholarship, or a legal interpretation that incorporates Māori customary understandings of the term at the time of signing.',
    'If ''kāwanatanga'' is found to mean limited governance, the claim of full cession of sovereignty would be invalidated, shifting the constraint''s classification away from Snare. If full sovereignty is upheld, the current classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kawanatanga_meaning_ambiguity, empirical, 'Ambiguity in the meaning of ''kāwanatanga''.').

omega_variable(
    mandate_for_land_alienation,
    'Does the Treaty, under any reading, provide a legitimate mandate for the Crown''s extensive alienation of Māori land without ongoing consent?',
    'A comprehensive review of historical land transactions against the principles of the Treaty (as understood by both parties), potentially leading to a re-evaluation of historical legitimacy.',
    'If no legitimate mandate is found, the historical land transfers become visible as pure extraction, intensifying the Snare classification and potentially leading to significant reparations or land returns. If a mandate is affirmed, the current land tenure system is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_for_land_alienation, preference, 'Legitimacy of land alienation under the Treaty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__crown_cession_reading, 1840, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trea_tr_t1840, treaty_authority_cession__crown_cession_reading, theater_ratio, 1840, 0.2).
narrative_ontology:measurement(trea_tr_t1880, treaty_authority_cession__crown_cession_reading, theater_ratio, 1880, 0.3).
narrative_ontology:measurement(trea_tr_t1920, treaty_authority_cession__crown_cession_reading, theater_ratio, 1920, 0.35).
narrative_ontology:measurement(trea_tr_t1960, treaty_authority_cession__crown_cession_reading, theater_ratio, 1960, 0.4).
narrative_ontology:measurement(trea_tr_t2000, treaty_authority_cession__crown_cession_reading, theater_ratio, 2000, 0.45).
narrative_ontology:measurement(trea_tr_t2024, treaty_authority_cession__crown_cession_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(trea_be_t1840, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1840, 0.6).
narrative_ontology:measurement(trea_be_t1880, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1880, 0.75).
narrative_ontology:measurement(trea_be_t1920, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1920, 0.8).
narrative_ontology:measurement(trea_be_t1960, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1960, 0.85).
narrative_ontology:measurement(trea_be_t2000, treaty_authority_cession__crown_cession_reading, base_extractiveness, 2000, 0.88).
narrative_ontology:measurement(trea_be_t2024, treaty_authority_cession__crown_cession_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(trea_su_t1840, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1840, 0.7).
narrative_ontology:measurement(trea_su_t1880, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1880, 0.85).
narrative_ontology:measurement(trea_su_t1920, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1920, 0.9).
narrative_ontology:measurement(trea_su_t1960, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1960, 0.95).
narrative_ontology:measurement(trea_su_t2000, treaty_authority_cession__crown_cession_reading, suppression_requirement, 2000, 0.92).
narrative_ontology:measurement(trea_su_t2024, treaty_authority_cession__crown_cession_reading, suppression_requirement, 2024, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
