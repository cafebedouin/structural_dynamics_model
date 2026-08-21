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
    domain_priors:emerges_naturally/1,
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
 *   This constraint is the 'crown_cession_reading' of the
 *   'treaty_authority_cession' kernel. It asserts that the Treaty of
 *   Waitangi, particularly its English text, established full British
 *   sovereignty over New Zealand, subordinating or extinguishing Māori
 *   customary authority. Sibling readings include
 *   'rangatiratanga_retention_reading' and 'biculturalism_reading'. This
 *   reading, while claimed as a foundational 'mountain' by its proponents,
 *   operates with high extraction and suppression, actively enforced to
 *   maintain the Crown's authority and legitimize historical land alienation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__crown_cession_reading, 0.85).
domain_priors:suppression_score(treaty_authority_cession__crown_cession_reading, 0.9).
domain_priors:theater_ratio(treaty_authority_cession__crown_cession_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__crown_cession_reading, mountain).
narrative_ontology:human_readable(treaty_authority_cession__crown_cession_reading, "Treaty of Waitangi: Crown Cession Reading").
narrative_ontology:topic_domain(treaty_authority_cession__crown_cession_reading, "constitutional_law/indigenous_rights/colonial_history").

domain_priors:requires_active_enforcement(treaty_authority_cession__crown_cession_reading).
domain_priors:emerges_naturally(treaty_authority_cession__crown_cession_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__crown_cession_reading, 'd4fa22d4-b1ba-4f75-b607-7b1df9749135').
narrative_ontology:cs_kernel_codification('d4fa22d4-b1ba-4f75-b607-7b1df9749135', fixed_text).
narrative_ontology:cs_authority_grounding('d4fa22d4-b1ba-4f75-b607-7b1df9749135', lineage).
narrative_ontology:cs_interpretation_layer_present('d4fa22d4-b1ba-4f75-b607-7b1df9749135').
narrative_ontology:cs_reading_relation('d4fa22d4-b1ba-4f75-b607-7b1df9749135', treaty_authority_cession__rangatiratanga_retention_reading, forecloses).
narrative_ontology:cs_reading_relation('d4fa22d4-b1ba-4f75-b607-7b1df9749135', treaty_authority_cession__biculturalism_reading, influences).
narrative_ontology:cs_axiom('d4fa22d4-b1ba-4f75-b607-7b1df9749135', foundational, english_text_supremacy).
narrative_ontology:cs_axiom_status(english_text_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('d4fa22d4-b1ba-4f75-b607-7b1df9749135', english_text_supremacy, conventional).
narrative_ontology:cs_axiom('d4fa22d4-b1ba-4f75-b607-7b1df9749135', foundational, kawanatanga_equals_sovereignty).
narrative_ontology:cs_axiom_status(kawanatanga_equals_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('d4fa22d4-b1ba-4f75-b607-7b1df9749135', kawanatanga_equals_sovereignty, conventional).
narrative_ontology:cs_reference_frame('d4fa22d4-b1ba-4f75-b607-7b1df9749135', crown_sovereignty_established_1840).
narrative_ontology:cs_drift_state('d4fa22d4-b1ba-4f75-b607-7b1df9749135', contemporary_maori_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d4fa22d4-b1ba-4f75-b607-7b1df9749135', '').
narrative_ontology:cs_kernel_id(treaty_authority_cession__crown_cession_reading, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treaty_authority_cession__crown_cession_reading, british_crown).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__crown_cession_reading, settler_government).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__crown_cession_reading, pakeha_landowners).
narrative_ontology:constraint_victim(treaty_authority_cession__crown_cession_reading, maori_iwi_hapu).
narrative_ontology:constraint_victim(treaty_authority_cession__crown_cession_reading, maori_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ultimate authority claiming sovereignty over New Zealand based on the English text of the Treaty, legitimizing its legislative power and land acquisition. Benefits from the legal framework that underpins its authority.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, british_crown, agenda_setter,
    institutional, generational, arbitrage, global).

% The operational government of New Zealand, which inherited and continues to assert the Crown's sovereignty, using it to enact laws and administer land. Benefits from the stability and legitimacy this reading provides to its governance.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, settler_government, agenda_setter,
    institutional, biographical, mobile, national).

% Individuals and entities whose land titles and property rights are secured and legitimized by the Crown's asserted sovereignty and the subsequent land alienation processes. Benefits from the legal certainty of their holdings.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, pakeha_landowners, beneficiary,
    powerful, biographical, mobile, local).

% Māori tribes and sub-tribes who, under this reading, lost their inherent sovereignty ('tino rangatiratanga') and much of their ancestral land. They bear the costs of diminished authority and resource control, often resisting through legal and political means.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, maori_iwi_hapu, payer,
    organized, generational, identity_locked, national).

% Individual Māori who live under a legal system that, according to this reading, subordinates their customary laws and rights. They experience the social and economic consequences of historical land loss and cultural marginalization.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, maori_citizens, payer,
    powerless, biographical, constrained, national).

% The courts tasked with interpreting the Treaty and its implications for New Zealand law. While some rulings have acknowledged Māori rights, the foundational premise of Crown sovereignty, as asserted by this reading, often frames their decisions.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, new_zealand_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Organizations that monitor New Zealand's compliance with international human rights standards, often scrutinizing the historical and ongoing impacts of the Treaty's interpretation on Indigenous rights. They provide external critique and pressure.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, international_human_rights_bodies, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified legal and political system under the British Crown, facilitating the administration of justice, land, and resources across New Zealand, and preventing claims by other colonial powers.
% TRANSFER_FUNCTION: Transfers ultimate legislative and land authority from Māori to the British Crown and subsequent settler governments, enabling the alienation of Māori land and the imposition of British law.
% ABSENT_VOICES: Māori chiefs who signed the Māori text of the Treaty, believing they retained 'tino rangatiratanga' (full authority over their lands, people, and treasures), are structurally excluded from this reading's interpretation of the Treaty's legal effect. Their understanding of retained sovereignty is dismissed.
% DISAPPEARANCE_RATIONALE: If this reading of the Treaty vanished, the foundational legal basis for Crown sovereignty in New Zealand would be undermined. This would necessitate a radical re-evaluation of constitutional arrangements, land tenure, and the legitimacy of the state, leading to profound societal reorganization.
% FOUNDING_PROBLEM: To establish British sovereignty over New Zealand, secure land for British settlement, and bring Māori under British law, thereby preventing other colonial powers from claiming the territory and managing perceived inter-tribal conflict.
% FOUNDING_PROBLEM_CORROBORATION: Historical records and accounts from British officials corroborate the desire for sovereignty and land acquisition. However, Māori oral histories, legal challenges, and independent scholarly analysis from outside the benefiting parties (Crown/settler government) contest the legitimacy of the cession and the ongoing relevance of the 'founding problem' as justification for current arrangements.
narrative_ontology:disappearance_verdict(treaty_authority_cession__crown_cession_reading, world_rearranges).
narrative_ontology:founding_problem_status(treaty_authority_cession__crown_cession_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__crown_cession_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(treaty_authority_cession__crown_cession_reading, 'none', 1).
narrative_ontology:epsilon_provenance(treaty_authority_cession__crown_cession_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(treaty_authority_cession__crown_cession_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(treaty_authority_cession__crown_cession_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, ExtMetricName, E),
    domain_priors:suppression_score(treaty_authority_cession__crown_cession_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(treaty_authority_cession__crown_cession_reading),
    narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(treaty_authority_cession__crown_cession_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the transfer of vast amounts of land and resources, and the subordination of Māori legal systems. Suppression (0.90) is high due to the active legal and political mechanisms used to enforce Crown sovereignty and dismiss Māori claims. The low theater ratio (0.10) indicates that proponents of this reading genuinely believe in its legal and historical validity, with little performative maintenance. Accessibility collapse is high (0.90) as this reading asserts the near-total collapse of alternative legal frameworks for sovereignty. Resistance is high (0.75) due to continuous Māori challenges to this interpretation. The claimed type 'mountain' reflects the proponents' view of its unchangeable, foundational nature, while the metrics describe its actual, highly extractive and suppressive operation, which will trigger False Summit Mountain detection.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Crown and settler government, this reading represents a legitimate and foundational act of state-building. From the perspective of Māori, it represents a profound act of dispossession and ongoing injustice. The engine's classification will highlight this divergence, showing a claimed 'mountain' that functions as a highly extractive 'tangled_rope' or 'snare' for Māori seats, due to the high extraction, suppression, and active enforcement required to maintain it.
 *
 * DIRECTIONALITY LOGIC:
 *   The British Crown and settler government are clear beneficiaries and agenda-setters, gaining legislative authority and land. Pakeha landowners also benefit from the security of their titles. Māori iwi/hapu and individual Māori citizens are the primary payers/victims, losing sovereignty, land, and cultural autonomy. The New Zealand judiciary, while interpreting, largely operates within the framework established by this reading, thus reinforcing it. International human rights bodies act as external observers, often critiquing the outcomes of this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kawanatanga_interpretation_ambiguity,
    'Does ''kāwanatanga'' (governance) in the Māori text of the Treaty equate to full ''sovereignty'' as understood in the English text, or a more limited form of authority?',
    'Linguistic and historical analysis of 19th-century Māori usage, comparative analysis of Indigenous treaties, and ongoing legal precedent that re-evaluates the original intent and understanding of Māori signatories.',
    'If ''kāwanatanga'' is found to be a limited form of governance, the Crown''s claim to full sovereignty is weakened, potentially reclassifying the constraint towards a ''tangled_rope'' or ''snare'' for the Crown, and a ''rope'' or ''scaffold'' for Māori, acknowledging retained authority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kawanatanga_interpretation_ambiguity, empirical, 'Ambiguity in the translation and interpretation of ''kāwanatanga''.').

omega_variable(
    textual_supremacy_legitimacy,
    'Is the English text of the Treaty of Waitangi legitimately supreme over the Māori text, given the known discrepancies and the principle of ''contra proferentem'' (against the offeror)?',
    'Constitutional reform establishing the Māori text as authoritative, or a Supreme Court ruling explicitly affirming the Māori text''s legal primacy in cases of divergence.',
    'If the Māori text is deemed supreme, the foundational premise of this reading collapses, leading to a reclassification that acknowledges retained Māori sovereignty and challenges the legitimacy of historical land alienation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_supremacy_legitimacy, conceptual, 'Legitimacy of English text controlling over Māori text.').

omega_variable(
    cession_vs_imposition,
    'Was the Treaty a genuine act of cession of sovereignty by Māori, or an act of imposition by the British Crown, facilitated by linguistic and power asymmetries?',
    'A national truth and reconciliation process, or a formal declaration by the New Zealand Parliament acknowledging the Treaty as an act of imposition rather than a consensual cession.',
    'If recognized as imposition, the moral and legal legitimacy of the Crown''s sovereignty claim, as asserted by this reading, would be fundamentally undermined, leading to calls for restorative justice and constitutional re-negotiation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cession_vs_imposition, preference, 'Whether the treaty was a genuine cession or an act of imposition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__crown_cession_reading, 1840, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trea_tr_t1840, treaty_authority_cession__crown_cession_reading, theater_ratio, 1840, 0.05).
narrative_ontology:measurement(trea_tr_t1880, treaty_authority_cession__crown_cession_reading, theater_ratio, 1880, 0.08).
narrative_ontology:measurement(trea_tr_t1920, treaty_authority_cession__crown_cession_reading, theater_ratio, 1920, 0.1).
narrative_ontology:measurement(trea_tr_t1960, treaty_authority_cession__crown_cession_reading, theater_ratio, 1960, 0.12).
narrative_ontology:measurement(trea_tr_t2000, treaty_authority_cession__crown_cession_reading, theater_ratio, 2000, 0.11).
narrative_ontology:measurement(trea_tr_t2020, treaty_authority_cession__crown_cession_reading, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(trea_be_t1840, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1840, 0.6).
narrative_ontology:measurement(trea_be_t1880, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1880, 0.75).
narrative_ontology:measurement(trea_be_t1920, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1920, 0.85).
narrative_ontology:measurement(trea_be_t1960, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1960, 0.88).
narrative_ontology:measurement(trea_be_t2000, treaty_authority_cession__crown_cession_reading, base_extractiveness, 2000, 0.87).
narrative_ontology:measurement(trea_be_t2020, treaty_authority_cession__crown_cession_reading, base_extractiveness, 2020, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(trea_su_t1840, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1840, 0.7).
narrative_ontology:measurement(trea_su_t1880, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1880, 0.85).
narrative_ontology:measurement(trea_su_t1920, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1920, 0.9).
narrative_ontology:measurement(trea_su_t1960, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1960, 0.92).
narrative_ontology:measurement(trea_su_t2000, treaty_authority_cession__crown_cession_reading, suppression_requirement, 2000, 0.9).
narrative_ontology:measurement(trea_su_t2020, treaty_authority_cession__crown_cession_reading, suppression_requirement, 2020, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
