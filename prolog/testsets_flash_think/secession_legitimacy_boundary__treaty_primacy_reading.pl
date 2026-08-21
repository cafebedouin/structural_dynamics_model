% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__treaty_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__treaty_primacy_reading, []).

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
 *   constraint_id: secession_legitimacy_boundary__treaty_primacy_reading
 *   human_readable: Indigenous Treaty Primacy in Secession
 *   domain: political_economy/federalism/resource_politics
 *
 * SUMMARY:
 *   This constraint represents the 'treaty_primacy_reading' of the
 *   'secession_legitimacy_boundary' kernel. It asserts that Indigenous treaty
 *   rights predate and supersede both federal and provincial authority,
 *   making Indigenous consent essential for legitimate secession or any major
 *   alteration of treaty relationships. This reading stands in contrast to
 *   other interpretations that prioritize constitutional text, popular
 *   sovereignty, or grievance thresholds. The constraint operates as a
 *   Tangled Rope: it provides a framework for coordination between Indigenous
 *   nations and the Crown, but its contested nature and frequent violations
 *   lead to significant extraction from Indigenous peoples, who must
 *   constantly defend their rights.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__treaty_primacy_reading, 0.7).
domain_priors:suppression_score(secession_legitimacy_boundary__treaty_primacy_reading, 0.8).
domain_priors:theater_ratio(secession_legitimacy_boundary__treaty_primacy_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__treaty_primacy_reading, tangled_rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__treaty_primacy_reading, "Indigenous Treaty Primacy in Secession").
narrative_ontology:topic_domain(secession_legitimacy_boundary__treaty_primacy_reading, "political_economy/federalism/resource_politics").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__treaty_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__treaty_primacy_reading, '2668e423-052c-4fcd-a59c-2998c0e6b9bd').
narrative_ontology:cs_kernel_codification('2668e423-052c-4fcd-a59c-2998c0e6b9bd', formalized).
narrative_ontology:cs_authority_grounding('2668e423-052c-4fcd-a59c-2998c0e6b9bd', lineage).
narrative_ontology:cs_interpretation_layer_present('2668e423-052c-4fcd-a59c-2998c0e6b9bd').
narrative_ontology:cs_reading_relation('2668e423-052c-4fcd-a59c-2998c0e6b9bd', secession_legitimacy_boundary__constitutional_impossibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('2668e423-052c-4fcd-a59c-2998c0e6b9bd', secession_legitimacy_boundary__popular_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('2668e423-052c-4fcd-a59c-2998c0e6b9bd', secession_legitimacy_boundary__grievance_threshold_reading, coexists_with).
narrative_ontology:cs_axiom('2668e423-052c-4fcd-a59c-2998c0e6b9bd', foundational, treaties_are_sacred_agreements).
narrative_ontology:cs_axiom_status(treaties_are_sacred_agreements, holdable).
narrative_ontology:cs_axiom_grounding('2668e423-052c-4fcd-a59c-2998c0e6b9bd', treaties_are_sacred_agreements, deontological).
narrative_ontology:cs_axiom('2668e423-052c-4fcd-a59c-2998c0e6b9bd', foundational, indigenous_sovereignty_predates_crown).
narrative_ontology:cs_axiom_status(indigenous_sovereignty_predates_crown, holdable).
narrative_ontology:cs_axiom_grounding('2668e423-052c-4fcd-a59c-2998c0e6b9bd', indigenous_sovereignty_predates_crown, conventional).
narrative_ontology:cs_reference_frame('2668e423-052c-4fcd-a59c-2998c0e6b9bd', original_treaty_relationship).
narrative_ontology:cs_drift_state('2668e423-052c-4fcd-a59c-2998c0e6b9bd', contemporary_federal_provincial_unilateralism, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2668e423-052c-4fcd-a59c-2998c0e6b9bd', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__treaty_primacy_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__treaty_primacy_reading, indigenous_treaty_holders).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__treaty_primacy_reading, federal_unilateralist_factions).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__treaty_primacy_reading, provincial_unilateralist_factions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__treaty_primacy_reading, indigenous_treaty_holders).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__treaty_primacy_reading, secessionist_political_parties).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their inherent and treaty rights are affirmed by this constraint, protecting their lands and self-determination. However, they bear the cost of actively defending these rights against challenges from federal and provincial governments, making them also a payer in practice.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, indigenous_treaty_holders, beneficiary,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__treaty_primacy_reading, indigenous_treaty_holders, payer).

% These factions within the federal government seek to assert federal paramountcy or pursue national interests (e.g., resource projects) without full Indigenous consent. This constraint extracts their option for unilateral action, requiring them to seek consent or face legal and political challenges.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, federal_unilateralist_factions, payer,
    institutional, biographical, constrained, national).

% These factions within provincial governments seek to assert provincial jurisdiction over land and resources, or pursue secession, without full Indigenous consent. The constraint extracts their option for unilateral action, requiring them to seek consent or face legal and political challenges.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, provincial_unilateralist_factions, payer,
    institutional, biographical, constrained, regional).

% As a party to historical treaties and a constitutional actor, the federal government is responsible for upholding treaty rights. This constraint shapes its policy-making, requiring it to engage with Indigenous nations on matters affecting their rights, particularly regarding secession.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, federal_government_as_crown, agenda_setter,
    institutional, generational, constrained, national).

% Provincial governments, while not original treaty signatories in the same way as the federal Crown, are bound by the constitutional recognition of treaty rights. This constraint limits their ability to unilaterally pursue secession or resource development within their claimed territories without Indigenous consent.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, provincial_governments_as_crown, agenda_setter,
    institutional, generational, constrained, regional).

% These parties advocate for provincial secession but find their path to legitimacy constrained by the requirement of Indigenous consent. The constraint extracts their ability to claim a unilateral right to self-determination for the provincial population, forcing them to consider Indigenous rights.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, secessionist_political_parties, payer,
    organized, biographical, constrained, regional).

% The highest court in Canada, responsible for interpreting constitutional law, including Section 35 (Aboriginal and treaty rights). Its rulings shape the practical application and enforcement of treaty primacy, acting as a key arbiter of this constraint.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, supreme_court_of_canada, agenda_setter,
    institutional, generational, analytical, national).

% These bodies monitor the human rights of Indigenous peoples globally and can issue recommendations or condemnations regarding the treatment of treaty rights in Canada. They provide an external, analytical perspective on the constraint's operation and enforcement.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, international_human_rights_bodies, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for respectful co-existence and shared sovereignty between Indigenous nations and the Crown, ensuring that major territorial changes (like secession) are not undertaken unilaterally by federal or provincial governments.
% TRANSFER_FUNCTION: Transfers the right to unilaterally alter treaty relationships or secede from federal/provincial governments to a shared decision-making process requiring Indigenous consent, thereby affirming Indigenous jurisdiction over their traditional territories.
% ABSENT_VOICES: Future generations of Indigenous peoples, whose inherent rights are at stake; non-treaty Indigenous groups whose rights might be affected by secession but are not directly party to the specific treaties in question; and those who would advocate for a more robust international legal framework for Indigenous self-determination.
% DISAPPEARANCE_RATIONALE: If treaty primacy vanished overnight, federal and provincial governments could unilaterally pursue secession, resource development, or other territorial changes without Indigenous consent. This would lead to widespread land disputes, human rights violations, and potential conflict, fundamentally altering the political and social landscape of Canada and its relationship with Indigenous peoples.
% FOUNDING_PROBLEM: The historical and ongoing assertion of Crown sovereignty over Indigenous lands without consent, leading to dispossession, cultural erosion, and conflict. The constraint aims to rectify this by asserting Indigenous pre-existing rights and the requirement for consent in major territorial decisions.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars specializing in Indigenous law, international human rights bodies (e.g., UN Special Rapporteur on the Rights of Indigenous Peoples), and numerous Indigenous-led commissions and reports (e.g., Royal Commission on Aboriginal Peoples, Truth and Reconciliation Commission) consistently corroborate that the problem of asserting Indigenous rights against unilateral state action is ongoing.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__treaty_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__treaty_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__treaty_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(secession_legitimacy_boundary__treaty_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__treaty_primacy_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__treaty_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(secession_legitimacy_boundary__treaty_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(secession_legitimacy_boundary__treaty_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.7) reflects the ongoing struggle to uphold treaty rights against federal and provincial claims, and the potential for severe extraction from Indigenous peoples if the constraint is violated. Suppression (0.8) is high due to the historical marginalization of Indigenous voices and active resistance to their claims by state actors. The theater ratio (0.4) indicates that while genuine consultations occur, there are also performative engagements that do not genuinely seek consent. The claimed type is Tangled Rope because treaties inherently involve coordination between parties, but the power imbalance and frequent disregard for Indigenous consent introduce significant asymmetric extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Indigenous treaty holders, this constraint is a fundamental protection of their inherent rights, though its enforcement is often weak. From the perspective of federal and provincial unilateralist factions, it is an impediment to their perceived sovereignty and policy goals. The engine's computation of per-seat classifications will highlight this divergence, showing the constraint as protective for Indigenous peoples when effective, but extractive for those seeking to bypass it.
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous treaty holders are beneficiaries when the constraint is upheld, as their rights are protected. However, they also act as payers by bearing the costs of defending these rights. Federal and provincial unilateralist factions are payers, as the constraint extracts their option for unilateral action. The federal and provincial governments, as 'Crown' entities, are agenda-setters, responsible for upholding treaties but also subject to the constraint's limitations on their sovereignty. The Supreme Court acts as an agenda-setter through its interpretations, and international bodies serve as observers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legal_weight_of_consent,
    'What is the actual legal weight of Indigenous consent in secession, given conflicting constitutional interpretations and political realities?',
    'Further Supreme Court rulings clarifying the scope of Section 35 in relation to provincial secession, or a constitutional amendment explicitly defining the role of Indigenous consent.',
    'If consent is deemed a veto, the constraint''s suppressive force on unilateral secessionists increases significantly. If it''s merely a consultation requirement, the constraint''s extractiveness from Indigenous peoples remains high due to ongoing vulnerability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_weight_of_consent, empirical, 'Ambiguity regarding the legal force of Indigenous consent in secession processes.').

omega_variable(
    genuineness_of_consultation,
    'To what extent are federal/provincial consultations with Indigenous nations genuine consent-seeking processes versus performative exercises to fulfill legal minimums?',
    'Independent audits of consultation processes, analysis of outcomes (e.g., project modifications based on Indigenous input), and Indigenous-led assessments of engagement quality.',
    'If consultations are largely performative, the constraint''s theater_ratio is higher, and its effective extractiveness from Indigenous peoples is amplified, as their input is not genuinely incorporated. If genuine, the constraint functions closer to a true coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuineness_of_consultation, empirical, 'Distinguishing genuine consent from performative consultation in treaty-related decisions.').

omega_variable(
    treaty_primacy_vs_popular_sovereignty,
    'Is the assertion of Indigenous treaty primacy fundamentally irreconcilable with the principle of popular sovereignty within a province, or can these principles be harmonized?',
    'Conceptual analysis and political negotiation leading to a new constitutional understanding or framework that explicitly reconciles these claims, or a definitive judicial ruling on their hierarchy.',
    'If irreconcilable, the constraint will remain a source of fundamental conflict, with high resistance and suppression. If harmonized, the constraint could evolve towards a more stable Rope, reducing extractiveness and suppression.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(treaty_primacy_vs_popular_sovereignty, conceptual, 'Conceptual tension between Indigenous treaty primacy and provincial popular sovereignty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__treaty_primacy_reading, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t1970, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(sece_tr_t1980, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 1980, 0.28).
narrative_ontology:measurement(sece_tr_t1990, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 1990, 0.35).
narrative_ontology:measurement(sece_tr_t2000, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 2000, 0.38).
narrative_ontology:measurement(sece_tr_t2010, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(sece_tr_t2020, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 2020, 0.4).

% Extraction over time
narrative_ontology:measurement(sece_be_t1970, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 1970, 0.5).
narrative_ontology:measurement(sece_be_t1980, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 1980, 0.58).
narrative_ontology:measurement(sece_be_t1990, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 1990, 0.65).
narrative_ontology:measurement(sece_be_t2000, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement(sece_be_t2010, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 2010, 0.7).
narrative_ontology:measurement(sece_be_t2020, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 2020, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t1970, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 1970, 0.6).
narrative_ontology:measurement(sece_su_t1980, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 1980, 0.68).
narrative_ontology:measurement(sece_su_t1990, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 1990, 0.75).
narrative_ontology:measurement(sece_su_t2000, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 2000, 0.78).
narrative_ontology:measurement(sece_su_t2010, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 2010, 0.8).
narrative_ontology:measurement(sece_su_t2020, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 2020, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
