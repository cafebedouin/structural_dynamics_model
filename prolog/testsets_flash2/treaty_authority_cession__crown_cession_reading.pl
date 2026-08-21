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
 *   constraint_id: treaty_authority_cession__crown_cession_reading
 *   human_readable: Treaty of Waitangi: Crown Cession Reading
 *   domain: constitutional_law/indigenous_rights/colonial_history
 *
 * SUMMARY:
 *   This constraint represents the 'Crown Cession' reading of the Treaty of
 *   Waitangi, where the English text is considered authoritative,
 *   'kāwanatanga' is interpreted as full sovereignty, and the Treaty is seen
 *   as completing a legal cession of authority to the British Crown. This
 *   reading underpins the historical and ongoing assertion of Crown
 *   legislative supremacy and the legitimacy of land alienation. It is
 *   presented as a snare because it extracts sovereignty and resources from
 *   Māori under the guise of a legitimate agreement, requiring active
 *   enforcement to suppress Māori claims to retained authority.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__crown_cession_reading, 0.85).
domain_priors:suppression_score(treaty_authority_cession__crown_cession_reading, 0.9).
domain_priors:theater_ratio(treaty_authority_cession__crown_cession_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__crown_cession_reading, snare).
narrative_ontology:human_readable(treaty_authority_cession__crown_cession_reading, "Treaty of Waitangi: Crown Cession Reading").
narrative_ontology:topic_domain(treaty_authority_cession__crown_cession_reading, "constitutional_law/indigenous_rights/colonial_history").

domain_priors:requires_active_enforcement(treaty_authority_cession__crown_cession_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__crown_cession_reading, 'dc4a3bfe-1a6b-42e0-ab6b-56f4a613a614').
narrative_ontology:cs_kernel_codification('dc4a3bfe-1a6b-42e0-ab6b-56f4a613a614', fixed_text).
narrative_ontology:cs_authority_grounding('dc4a3bfe-1a6b-42e0-ab6b-56f4a613a614', lineage).
narrative_ontology:cs_interpretation_layer_present('dc4a3bfe-1a6b-42e0-ab6b-56f4a613a614').
narrative_ontology:cs_reading_relation('dc4a3bfe-1a6b-42e0-ab6b-56f4a613a614', treaty_authority_cession__rangatiratanga_retention_reading, forecloses).
narrative_ontology:cs_reading_relation('dc4a3bfe-1a6b-42e0-ab6b-56f4a613a614', treaty_authority_cession__retrospective_snare_exposure, coexists_with).
narrative_ontology:cs_axiom('dc4a3bfe-1a6b-42e0-ab6b-56f4a613a614', foundational, english_text_supremacy).
narrative_ontology:cs_axiom_status(english_text_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('dc4a3bfe-1a6b-42e0-ab6b-56f4a613a614', english_text_supremacy, conventional).
narrative_ontology:cs_axiom('dc4a3bfe-1a6b-42e0-ab6b-56f4a613a614', foundational, kawanatanga_equals_full_sovereignty).
narrative_ontology:cs_axiom_status(kawanatanga_equals_full_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('dc4a3bfe-1a6b-42e0-ab6b-56f4a613a614', kawanatanga_equals_full_sovereignty, conventional).
narrative_ontology:cs_reference_frame('dc4a3bfe-1a6b-42e0-ab6b-56f4a613a614', uncontested_crown_sovereignty_1840).
narrative_ontology:cs_drift_state('dc4a3bfe-1a6b-42e0-ab6b-56f4a613a614', contemporary_waitangi_tribunal_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('dc4a3bfe-1a6b-42e0-ab6b-56f4a613a614', '').
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

% Interprets the Treaty as a full cession of sovereignty, legitimizing its legislative authority and land acquisition. Benefits from unchallenged legal and political control over New Zealand.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, new_zealand_crown, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefits from the Crown's asserted sovereignty, which underpins land titles, legal frameworks, and political stability. Has largely adopted the Crown's interpretation of the Treaty.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, settler_population, beneficiary,
    organized, generational, mobile, national).

% Under this reading, Māori communities lost their inherent sovereignty and control over land and resources. Their customary law is subordinated, leading to cultural and economic dispossession. Exit means abandoning their ancestral lands and identity.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, maori_iwi_hapu, payer,
    powerless, civilizational, identity_locked, local).

% The traditional governance structures and legal systems of Māori are rendered illegitimate or subordinate by the Crown's claim to full sovereignty. This authority is not recognized as a co-equal legal system.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, maori_customary_authority, excluded,
    powerless, civilizational, trapped, local).
narrative_ontology:stakeholder_non_agent(treaty_authority_cession__crown_cession_reading, maori_customary_authority).

% Analyze the Treaty's interpretation in light of international law, indigenous rights, and colonial history. Their findings often challenge the Crown's cession reading but have limited direct legal force.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, international_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(treaty_authority_cession__crown_cession_reading, new_zealand_crown).
narrative_ontology:fixing_cost_class(treaty_authority_cession__crown_cession_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From the Crown's perspective, the Treaty coordinated the establishment of a single, unified legal and political system in New Zealand, facilitating orderly settlement and governance.
% TRANSFER_FUNCTION: Transfers full sovereignty and legislative authority from Māori chiefs to the British Crown, and subsequently to the New Zealand government, legitimizing land alienation and the imposition of British law.
% ABSENT_VOICES: Māori chiefs who signed the Māori text of the Treaty, believing they were retaining 'tino rangatiratanga' (full authority) over their lands and people, are absent from the Crown's interpretive framework. Their understanding of 'kāwanatanga' as limited governance, not full sovereignty, is suppressed.
% DISAPPEARANCE_RATIONALE: If the Crown's cession reading vanished, the entire legal and constitutional foundation of New Zealand would be called into question. Land titles, legislative authority, and the relationship between Māori and the state would require fundamental renegotiation, leading to a profound societal rearrangement.
% FOUNDING_PROBLEM: The British Crown sought to establish sovereignty over New Zealand to protect Māori from unscrupulous settlers, secure British commercial interests, and prevent other European powers from claiming the territory.
% FOUNDING_PROBLEM_CORROBORATION: While the Crown initially claimed protection of Māori as a motive, independent historians and Māori scholars widely corroborate that the primary drivers were imperial expansion and resource acquisition. The 'protection' aspect is largely seen as a secondary justification, and the original threats (unscrupulous settlers, rival powers) are no longer the central issue.
narrative_ontology:disappearance_verdict(treaty_authority_cession__crown_cession_reading, world_rearranges).
narrative_ontology:founding_problem_status(treaty_authority_cession__crown_cession_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__crown_cession_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.85) because this reading fundamentally dispossesses Māori of their inherent sovereignty and control over resources, transferring immense power and wealth to the Crown and settler population. Suppression is also very high (0.9) as the Crown's interpretation has been enforced through legislation, courts, and military action, actively suppressing Māori customary law and resistance. The theater ratio is low (0.2) because, from this perspective, the Crown's actions are seen as legitimate governance, not mere performance, though the coordination story is a cover for extraction. Accessibility collapse is high (0.8) as this reading effectively eliminates alternatives for Māori within the established legal framework.
 *
 * PERSPECTIVAL GAP:
 *   The Crown and settler population experience this as a legitimate, foundational act of state-building (a 'rope' or even 'mountain' of constitutional law), while Māori experience it as a profound act of dispossession and ongoing extraction (a 'snare'). The engine's classification as 'snare' reflects the structural reality from the victim's perspective, which is obscured by the dominant narrative.
 *
 * DIRECTIONALITY LOGIC:
 *   The New Zealand Crown is the primary beneficiary and agenda-setter, gaining full sovereignty and control. The settler population also benefits from the stability and legal framework this reading provides. Māori iwi and hapū are the primary victims, losing sovereignty, land, and cultural authority. Māori customary authority is excluded and suppressed. International legal scholars act as observers, analyzing the constraint's operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_authority_ambiguity,
    'Which text of the Treaty of Waitangi (English or Māori) holds ultimate legal and moral authority?',
    'International legal arbitration or a constitutional convention that explicitly adjudicates the primacy of one text, or a new treaty that supersedes the original.',
    'If the Māori text is deemed authoritative, the Crown Cession reading''s foundational premise collapses, leading to a reclassification towards a partnership or retention model. If the English text is reaffirmed, the current classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_authority_ambiguity, conceptual, 'Ambiguity over the authoritative text of the Treaty.').

omega_variable(
    kawanatanga_meaning_ambiguity,
    'What was the original understanding of ''kāwanatanga'' by Māori chiefs who signed the Treaty?',
    'Extensive historical and linguistic analysis of 19th-century Māori usage, corroborated by contemporary Māori oral traditions and legal scholarship.',
    'If ''kāwanatanga'' is proven to mean limited governance (as opposed to full sovereignty), the Crown Cession reading''s claim to legitimate cession is undermined, shifting classification towards a snare or tangled rope based on misrepresentation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kawanatanga_meaning_ambiguity, empirical, 'Ambiguity over the meaning of ''kāwanatanga'' in the Māori text.').

omega_variable(
    legitimacy_of_land_alienation,
    'Is land alienation under the Crown Cession reading legitimate, given the contested nature of sovereignty cession?',
    'A comprehensive historical inquiry into specific land transactions, assessing whether they met principles of informed consent and fair exchange under a contested sovereignty claim.',
    'If land alienation is found to be illegitimate due to lack of genuine cession, the extractiveness of the constraint increases, and the ''snare'' classification is strongly reinforced, potentially leading to calls for restitution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_land_alienation, preference, 'The legitimacy of historical land transfers under the Crown''s interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__crown_cession_reading, 1840, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trea_tr_t1840, treaty_authority_cession__crown_cession_reading, theater_ratio, 1840, 0.1).
narrative_ontology:measurement(trea_tr_t1880, treaty_authority_cession__crown_cession_reading, theater_ratio, 1880, 0.15).
narrative_ontology:measurement(trea_tr_t1920, treaty_authority_cession__crown_cession_reading, theater_ratio, 1920, 0.2).
narrative_ontology:measurement(trea_tr_t1960, treaty_authority_cession__crown_cession_reading, theater_ratio, 1960, 0.25).
narrative_ontology:measurement(trea_tr_t2000, treaty_authority_cession__crown_cession_reading, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(trea_tr_t2024, treaty_authority_cession__crown_cession_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(trea_be_t1840, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1840, 0.6).
narrative_ontology:measurement(trea_be_t1880, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1880, 0.75).
narrative_ontology:measurement(trea_be_t1920, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1920, 0.85).
narrative_ontology:measurement(trea_be_t1960, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1960, 0.9).
narrative_ontology:measurement(trea_be_t2000, treaty_authority_cession__crown_cession_reading, base_extractiveness, 2000, 0.88).
narrative_ontology:measurement(trea_be_t2024, treaty_authority_cession__crown_cession_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(trea_su_t1840, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1840, 0.5).
narrative_ontology:measurement(trea_su_t1880, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1880, 0.7).
narrative_ontology:measurement(trea_su_t1920, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1920, 0.85).
narrative_ontology:measurement(trea_su_t1960, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1960, 0.9).
narrative_ontology:measurement(trea_su_t2000, treaty_authority_cession__crown_cession_reading, suppression_requirement, 2000, 0.85).
narrative_ontology:measurement(trea_su_t2024, treaty_authority_cession__crown_cession_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treaty_authority_cession__crown_cession_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(treaty_authority_cession__crown_cession_reading, treaty_authority_cession__rangatiratanga_retention_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__crown_cession_reading, treaty_authority_cession__retrospective_snare_exposure).
narrative_ontology:affects_constraint(treaty_authority_cession__crown_cession_reading, new_zealand_land_titles).
narrative_ontology:affects_constraint(treaty_authority_cession__crown_cession_reading, maori_language_revitalization).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'treaty_authority_cession' kernel. Its interpretation of full cession directly influences the legal and political landscape, affecting other readings and related constraints like land titles and Māori rights.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
