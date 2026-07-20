% ============================================================================
% CONSTRAINT STORY: article_17_complementarity__national_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_17_complementarity__national_primacy_reading, []).

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
 *   constraint_id: article_17_complementarity__national_primacy_reading
 *   human_readable: Article 17 Complementarity â National Primacy Reading
 *   domain: international_law/criminal_justice/state_sovereignty
 *
 * SUMMARY:
 *   Article 17 of the Rome Statute establishes complementarity, requiring the
 *   ICC to defer to national jurisdictions unless a state is 'unwilling or
 *   unable' genuinely to prosecute. The national primacy reading treats this
 *   as a sovereignty-protection mechanism: national courts are presumptively
 *   adequate, the ICC bears the burden of proving inadmissibility, and only
 *   proceedings that are sham or entirely collapsed trigger international
 *   override. This reading coordinates the international order by preserving
 *   state sovereignty and encouraging domestic accountability, but it
 *   asymmetrically extracts access to justice from victims in states with
 *   weak-yet-genuine judicial systems. The constraint is claimed as tangled
 *   rope because both the coordination function and the extraction are
 *   structurally real and inseparable.
 *
 * KEY AGENTS:
 *   - sovereignty_maximizing_states: Agenda-setter/beneficiary (institutional/arbitrage) â shapes the Rome Statute and ASP to shield national jurisdiction
 *   - national_judiciaries: Beneficiary (institutional/constrained) â retains primary jurisdiction over international crimes
 *   - icc_prosecutor: Payer (institutional/constrained) â bears the burden of proving inadmissibility; reach restricted
 *   - victims_of_weak_proceedings: Payer (powerless/trapped) â denied ICC remedy when domestic systems are weak but not sham
 *   - human_rights_ngos: Excluded (organized/constrained) â advocates for broader ICC access, structurally sidelined
 *   - international_oversight_advocates: Excluded (organized/constrained) â holds the competing reading, marginalized in this framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_17_complementarity__national_primacy_reading, 0.65).
domain_priors:suppression_score(article_17_complementarity__national_primacy_reading, 0.58).
domain_priors:theater_ratio(article_17_complementarity__national_primacy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_17_complementarity__national_primacy_reading, tangled_rope).
narrative_ontology:human_readable(article_17_complementarity__national_primacy_reading, "Article 17 Complementarity â National Primacy Reading").
narrative_ontology:topic_domain(article_17_complementarity__national_primacy_reading, "international_law/criminal_justice/state_sovereignty").

domain_priors:requires_active_enforcement(article_17_complementarity__national_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_17_complementarity__national_primacy_reading, 'bb243879-694d-4ccc-ab60-ec5e43b44129').
narrative_ontology:cs_kernel_codification('bb243879-694d-4ccc-ab60-ec5e43b44129', formalized).
narrative_ontology:cs_authority_grounding('bb243879-694d-4ccc-ab60-ec5e43b44129', lineage).
narrative_ontology:cs_interpretation_layer_present('bb243879-694d-4ccc-ab60-ec5e43b44129').
narrative_ontology:cs_reading_relation('bb243879-694d-4ccc-ab60-ec5e43b44129', article_17_complementarity__international_oversight_reading, coexists_with).
narrative_ontology:cs_axiom('bb243879-694d-4ccc-ab60-ec5e43b44129', foundational, state_sovereignty_jurisdictional_default).
narrative_ontology:cs_axiom_status(state_sovereignty_jurisdictional_default, holdable).
narrative_ontology:cs_axiom_grounding('bb243879-694d-4ccc-ab60-ec5e43b44129', state_sovereignty_jurisdictional_default, conventional).
narrative_ontology:cs_axiom('bb243879-694d-4ccc-ab60-ec5e43b44129', foundational, prosecutor_bears_inadmissibility_burden).
narrative_ontology:cs_axiom_status(prosecutor_bears_inadmissibility_burden, holdable).
narrative_ontology:cs_axiom_grounding('bb243879-694d-4ccc-ab60-ec5e43b44129', prosecutor_bears_inadmissibility_burden, conventional).
narrative_ontology:cs_reference_frame('bb243879-694d-4ccc-ab60-ec5e43b44129', westphalian_sovereignty_default).
narrative_ontology:cs_drift_state('bb243879-694d-4ccc-ab60-ec5e43b44129', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('bb243879-694d-4ccc-ab60-ec5e43b44129', '').
narrative_ontology:cs_kernel_id(article_17_complementarity__national_primacy_reading, article_17_complementarity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, national_judiciaries).
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, sovereignty_maximizing_states).
narrative_ontology:constraint_victim(article_17_complementarity__national_primacy_reading, victims_of_weak_proceedings).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(article_17_complementarity__national_primacy_reading, icc_prosecutor).
narrative_ontology:constraint_vindicates(article_17_complementarity__national_primacy_reading, westphalian_sovereignty_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Shape the Rome Statute's complementarity regime and the Assembly of States Parties to preserve sovereign control over criminal jurisdiction. They resist amendments that would lower the admissibility threshold and actively invoke Article 17 to shield domestic proceedings from ICC review.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, sovereignty_maximizing_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article_17_complementarity__national_primacy_reading, sovereignty_maximizing_states, beneficiary).

% Retain primary jurisdiction over core international crimes unless domestic proceedings are proven sham. They conduct investigations and trials that preempt ICC intervention, benefiting from the legal presumption of adequacy.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, national_judiciaries, beneficiary,
    institutional, generational, constrained, national).

% Must carry the evidentiary and legal burden to prove a case inadmissible under Article 17. The high threshold and deference to national sovereignty restrict the docket and force resource-intensive admissibility litigation.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, icc_prosecutor, payer,
    institutional, biographical, constrained, global).

% Victims of crimes in states with weak but not sham judicial systems are structurally denied ICC remedy. They cannot trigger admissibility review unilaterally and lack leverage to compel genuine domestic accountability or international override.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, victims_of_weak_proceedings, payer,
    powerless, biographical, trapped, local).

% Advocate for victims and broader ICC access, arguing that weak proceedings should trigger admissibility. They are not parties to admissibility proceedings and their amicus submissions are discretionary and non-binding.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, human_rights_ngos, excluded,
    organized, biographical, constrained, global).

% Scholars and practitioners who argue for the international oversight reading of complementarity. They are structurally marginalized in the operational legal framework, which follows the national primacy presumption.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, international_oversight_advocates, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_17_complementarity__national_primacy_reading, sovereignty_maximizing_states).
narrative_ontology:fixing_cost_class(article_17_complementarity__national_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves state sovereignty in international criminal justice by deferring to national legal systems; prevents the ICC from becoming a supranational appellate court and incentivizes domestic accountability.
% TRANSFER_FUNCTION: Transfers jurisdictional authority and the burden of proof from the international prosecutor to national judiciaries and states; transfers the risk of impunity and the cost of denied remedy to victims in weak-but-functional states.
% ABSENT_VOICES: Victims in states with weak-yet-genuine proceedings, international oversight advocates, and human rights NGOs are structurally sidelined in admissibility determinations; their absence is what allows the presumption of adequacy to operate without persistent contestation.
% DISAPPEARANCE_RATIONALE: If the national primacy reading vanished, the ICC would assume a broader oversight role, state jurisdictional shields would collapse, the docket would expand dramatically, and the international criminal justice system would shift toward an accountability-trigger model â sovereignty relationships and victim access would rearrange.
% FOUNDING_PROBLEM: To create a permanent international criminal court without subverting state sovereignty and to encourage domestic prosecution of core crimes rather than supplanting national legal systems.
% FOUNDING_PROBLEM_CORROBORATION: State parties and sovereignty-focused jurists attest that the need to protect sovereign jurisdiction remains live. Human rights NGOs and victim representatives attest the problem has shifted to impunity shielding. Independent international law scholars provide mixed corroboration, noting the tension is structural rather than resolved.
narrative_ontology:disappearance_verdict(article_17_complementarity__national_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_17_complementarity__national_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_17_complementarity__national_primacy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_17_complementarity__national_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_17_complementarity__national_primacy_reading, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_17_complementarity__national_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_17_complementarity__national_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_17_complementarity__national_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is substantial (0.65) because the high inadmissibility threshold systematically screens out victims in weak-but-functional states. Suppression (0.58) reflects the active legal and political machinery required to maintain the presumption â admissibility hearings, state cooperation demands, and ASP politics. Theater ratio (0.42) captures the growing performative dimension of domestic proceedings designed to satisfy the 'genuine' threshold while shielding perpetrators. Accessibility collapse (0.62) is high because once the national primacy reading is accepted, alternative routes to ICC remedy narrow drastically. Resistance (0.48) is moderate: human rights NGOs and oversight advocates contest the reading, but they lack institutional leverage inside the admissibility framework.
 *
 * PERSPECTIVAL GAP:
 *   From the sovereignty-bloc seat, the constraint is a necessary coordination mechanism preventing imperial overreach by an international prosecutor and preserving the Westphalian order. From the victim seat in a weak state, the same legal structure operates as an enforced jurisdictional wall that leaves them without remedy. The ICC prosecutor experiences it as a procedural burden that restricts institutional reach. The engine computes this divergence from the structural data; the authored claim (tangled_rope) captures both functions without adjudicating the perspectival dispute.
 *
 * DIRECTIONALITY LOGIC:
 *   The sovereignty-maximizing states and national judiciaries sit near the beneficiary end (low d): the constraint subsidizes their jurisdictional control and shields them from external scrutiny. The ICC prosecutor and victims of weak proceedings sit near the target end (high d): the constraint extracts from them by raising procedural and evidentiary barriers to international intervention. The excluded oversight advocates have no directional relationship to the constraint's operation because they are outside the admissibility framework.
 *
 * MANDATROPHY ANALYSIS:
 *   Classification as tangled rope prevents the mistake of treating the sovereignty shield as pure extraction (snare) â the coordination function (preserving Westphalian order, incentivizing domestic prosecution) is genuine and historically grounded. It also prevents the mistake of treating it as pure coordination (rope) â the asymmetric cost imposed on victims in weak-but-functional states is structural, not incidental, and requires active enforcement through admissibility hearings and state cooperation to maintain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'This constraint is one reading of the Article 17 complementarity kernel; does the structural classification change if the international oversight reading is adopted instead?',
    'Side-by-side comparison of the two compiled constraints; the sibling reading shifts the beneficiary/victim structure and directionality profile.',
    'Would reclassify the seat of extraction and potentially the constraint type if the oversight reading is treated as the operative framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Committing to one reading of a contested kernel').

omega_variable(
    sovereignty_presumption_naturalness,
    'Is the presumption of national judicial adequacy an inherent feature of the international legal order, or a constructed shield erected by state consent?',
    'Historical genealogy of complementarity doctrine and empirical tracking of admissibility outcomes in weak states.',
    'If constructed, the constraint functions as a tangled rope or snare; if inherent, it approaches a mountain-like legal principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_presumption_naturalness, conceptual, 'Natural law vs constructed status of sovereignty presumption').

omega_variable(
    sham_threshold_operationalization,
    'What empirical threshold distinguishes a ''genuine'' domestic proceeding from a ''sham'' under Article 17, and who controls that boundary?',
    'Meta-analysis of ICC Pre-Trial Chamber admissibility decisions and state compliance patterns.',
    'A threshold controlled entirely by states yields high extraction; a threshold independently reviewable by the ICC lowers extraction and shifts directionality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sham_threshold_operationalization, empirical, 'Operational control of the sham proceeding boundary').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_17_complementarity__national_primacy_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_17_complementarity__national_primacy_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(arti_tr_t4, article_17_complementarity__national_primacy_reading, theater_ratio, 4, 0.24).
narrative_ontology:measurement(arti_tr_t8, article_17_complementarity__national_primacy_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(arti_tr_t12, article_17_complementarity__national_primacy_reading, theater_ratio, 12, 0.33).
narrative_ontology:measurement(arti_tr_t18, article_17_complementarity__national_primacy_reading, theater_ratio, 18, 0.37).
narrative_ontology:measurement(arti_tr_t24, article_17_complementarity__national_primacy_reading, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_17_complementarity__national_primacy_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(arti_be_t4, article_17_complementarity__national_primacy_reading, base_extractiveness, 4, 0.38).
narrative_ontology:measurement(arti_be_t8, article_17_complementarity__national_primacy_reading, base_extractiveness, 8, 0.45).
narrative_ontology:measurement(arti_be_t12, article_17_complementarity__national_primacy_reading, base_extractiveness, 12, 0.52).
narrative_ontology:measurement(arti_be_t18, article_17_complementarity__national_primacy_reading, base_extractiveness, 18, 0.58).
narrative_ontology:measurement(arti_be_t24, article_17_complementarity__national_primacy_reading, base_extractiveness, 24, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_17_complementarity__national_primacy_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(arti_su_t4, article_17_complementarity__national_primacy_reading, suppression_requirement, 4, 0.42).
narrative_ontology:measurement(arti_su_t8, article_17_complementarity__national_primacy_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(arti_su_t12, article_17_complementarity__national_primacy_reading, suppression_requirement, 12, 0.54).
narrative_ontology:measurement(arti_su_t18, article_17_complementarity__national_primacy_reading, suppression_requirement, 18, 0.59).
narrative_ontology:measurement(arti_su_t24, article_17_complementarity__national_primacy_reading, suppression_requirement, 24, 0.63).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_17_complementarity__national_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_17_complementarity__national_primacy_reading, article_17_complementarity__international_oversight_reading).

% DUAL FORMULATION NOTE:
% This constraint is the national_primacy_reading of the Article 17 complementarity kernel. The sibling reading (international_oversight_reading) shares the same statutory text but assigns opposite polarity to the sovereignty/impunity tension. The two readings are not the same constraint viewed from two angles; they instantiate different structural relationships, different beneficiary/victim structures, and different epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
