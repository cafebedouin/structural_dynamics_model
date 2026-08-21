% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__christianized_pacification_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feud_obligation_kernel__christianized_pacification_reading, []).

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
 *   constraint_id: feud_obligation_kernel__christianized_pacification_reading
 *   human_readable: Christianized Pacification of Blood-Feud Obligations
 *   domain: legal_anthropology/medieval_history/comparative_political_systems
 *
 * SUMMARY:
 *   This constraint represents the 'christianized pacification' reading of
 *   the blood-feud kernel, where traditional kin-based vengeance is framed as
 *   a violation of divine law. Legitimate authority over violence is asserted
 *   to reside solely with God, delegated to ecclesiastical and royal
 *   institutions. This reading aims to completely suppress feuding through
 *   spiritual and temporal coercion, expanding the jurisdictional reach and
 *   moral authority of the Church and Crown. All feud participants are
 *   reclassified as victims, facing spiritual peril and temporal punishment,
 *   while the Church and Crown become beneficiaries of expanded power.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__christianized_pacification_reading, 0.78).
domain_priors:suppression_score(feud_obligation_kernel__christianized_pacification_reading, 0.85).
domain_priors:theater_ratio(feud_obligation_kernel__christianized_pacification_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__christianized_pacification_reading, tangled_rope).
narrative_ontology:human_readable(feud_obligation_kernel__christianized_pacification_reading, "Christianized Pacification of Blood-Feud Obligations").
narrative_ontology:topic_domain(feud_obligation_kernel__christianized_pacification_reading, "legal_anthropology/medieval_history/comparative_political_systems").

domain_priors:requires_active_enforcement(feud_obligation_kernel__christianized_pacification_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__christianized_pacification_reading, 'df05f854-5039-45f4-ba43-94db6c60786f').
narrative_ontology:cs_kernel_codification('df05f854-5039-45f4-ba43-94db6c60786f', fixed_text).
narrative_ontology:cs_authority_grounding('df05f854-5039-45f4-ba43-94db6c60786f', lineage).
narrative_ontology:cs_interpretation_layer_present('df05f854-5039-45f4-ba43-94db6c60786f').
narrative_ontology:cs_reading_relation('df05f854-5039-45f4-ba43-94db6c60786f', feud_obligation_kernel__stateless_coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('df05f854-5039-45f4-ba43-94db6c60786f', feud_obligation_kernel__extraction_cycle_reading, coexists_with).
narrative_ontology:cs_axiom('df05f854-5039-45f4-ba43-94db6c60786f', foundational, divine_monopoly_on_violence).
narrative_ontology:cs_axiom_status(divine_monopoly_on_violence, holdable).
narrative_ontology:cs_axiom_grounding('df05f854-5039-45f4-ba43-94db6c60786f', divine_monopoly_on_violence, theological).
narrative_ontology:cs_axiom('df05f854-5039-45f4-ba43-94db6c60786f', foundational, vengeance_is_sinful).
narrative_ontology:cs_axiom_status(vengeance_is_sinful, holdable).
narrative_ontology:cs_axiom_grounding('df05f854-5039-45f4-ba43-94db6c60786f', vengeance_is_sinful, deontological).
narrative_ontology:cs_reference_frame('df05f854-5039-45f4-ba43-94db6c60786f', divine_order_of_justice).
narrative_ontology:cs_drift_state('df05f854-5039-45f4-ba43-94db6c60786f', contemporary_secular_legal_systems, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('df05f854-5039-45f4-ba43-94db6c60786f', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__christianized_pacification_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__christianized_pacification_reading, ecclesiastical_institutions).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__christianized_pacification_reading, royal_authority).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__christianized_pacification_reading, common_populace).
narrative_ontology:constraint_victim(feud_obligation_kernel__christianized_pacification_reading, feud_participants).
narrative_ontology:constraint_victim(feud_obligation_kernel__christianized_pacification_reading, local_lineages).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the primary interpreters of divine law, they actively preach against vengeance, impose spiritual penalties (excommunication, penance), and establish ecclesiastical courts to adjudicate disputes, thereby expanding their moral and jurisdictional authority. They benefit from an interpretive monopoly on legitimate violence.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, ecclesiastical_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Collaborates with the Church to suppress feuding, viewing it as a challenge to royal peace and justice. They issue laws, establish royal courts, and use force to punish those who engage in feuds, consolidating their territorial control and legal monopoly on violence. They benefit from expanded jurisdictional reach and reduced internal conflict.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, royal_authority, agenda_setter,
    institutional, generational, constrained, national).

% Individuals bound by traditional honor codes and kin obligations to avenge wrongs. Under this constraint, their actions are deemed sinful and criminal, leading to spiritual peril (excommunication, damnation) and temporal penalties (fines, imprisonment, execution). Their identity is deeply tied to traditional justice, making exit difficult.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, feud_participants, payer,
    powerless, biographical, identity_locked, local).

% Kin groups that traditionally supported and enforced feuds. They face immense pressure from both Church and Crown to abandon these practices, risking collective spiritual and temporal punishment. While they may resist, the growing power of centralized institutions constrains their options.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, local_lineages, payer,
    organized, generational, constrained, local).

% Individuals and communities who suffer from the instability and violence of feuds. They benefit from the pacification efforts, experiencing greater peace, security, and the availability of institutional justice, though they also become subject to new forms of ecclesiastical and royal control.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, common_populace, beneficiary,
    moderate, biographical, mobile, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a centralized, divinely sanctioned authority for justice and violence resolution, replacing decentralized kin-based vengeance with institutionalized legal processes and spiritual guidance.
% TRANSFER_FUNCTION: Transfers the right and responsibility for justice and violence resolution from kin groups and local customs to ecclesiastical and royal institutions, along with the associated spiritual authority, legal jurisdiction, and coercive power.
% ABSENT_VOICES: Traditional kin-group elders, local customary law proponents, and those who upheld honor-based vengeance would object. They are increasingly marginalized or suppressed by the expanding reach of Church and Crown, their voices deemed illegitimate or heretical.
% DISAPPEARANCE_RATIONALE: If the Christianized pacification efforts and the associated divine/royal authority vanished, medieval society would likely revert to more widespread kin-based feuding and decentralized violence resolution, severely hindering the development of centralized states and the moral authority of the Church.
% FOUNDING_PROBLEM: Widespread, endemic kin-group feuds that destabilized social order, disrupted economic activity, and undermined the authority of emerging centralized powers (both ecclesiastical and royal), leading to cycles of violence and spiritual peril.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary chronicles, penitential manuals, and legal codes from ecclesiastical and royal sources consistently attest to the disruptive nature of feuds. Modern historians and legal anthropologists corroborate the historical reality of feuds as a significant challenge to medieval governance and social stability, supporting the claim that the problem was and remains 'live' from the perspective of institutional order.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__christianized_pacification_reading, world_rearranges).
narrative_ontology:founding_problem_status(feud_obligation_kernel__christianized_pacification_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__christianized_pacification_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(feud_obligation_kernel__christianized_pacification_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feud_obligation_kernel__christianized_pacification_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feud_obligation_kernel__christianized_pacification_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feud_obligation_kernel__christianized_pacification_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feud_obligation_kernel__christianized_pacification_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because it demands the surrender of a deeply ingrained right to self-help justice and imposes severe spiritual and temporal penalties. Suppression is very high (0.85) due to the combined coercive power of divine condemnation (excommunication, damnation) and emerging state legal systems (fines, imprisonment, execution). Theater ratio is low (0.1) because the enforcement mechanisms, both spiritual and temporal, were genuinely active and effective in reshaping social norms and legal practice over centuries. Accessibility collapse is high (0.9) as the goal is to eliminate all alternatives to institutional justice. Resistance is high (0.7) reflecting the deep cultural roots of feuding, which required sustained, multi-generational effort to suppress.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of ecclesiastical and royal authorities, this constraint is a necessary act of pacification and moral ordering, bringing divine justice and peace to a chaotic world. From the perspective of feud participants and kin groups, it is a coercive imposition that strips them of traditional rights, honor, and means of justice, replacing them with an alien and often distant institutional system. The engine's classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Ecclesiastical and royal institutions are clear beneficiaries and agenda-setters, gaining interpretive monopoly over violence and expanding their jurisdictional reach (low directionality). Feud participants and local lineages are the primary targets/payers, losing their traditional rights and facing severe penalties (high directionality, identity_locked exit for participants). The common populace are beneficiaries of increased peace and stability, though they also become subject to new forms of institutional control (moderate directionality).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_law_naturalness,
    'Is the prohibition on vengeance and the divine monopoly on violence a genuine natural law, or a constructed theological and political claim serving institutional interests?',
    'Comparative analysis of diverse legal and religious traditions regarding vengeance and authority over violence, alongside historical analysis of the political economy of medieval Church and Crown consolidation.',
    'If a constructed claim, the constraint''s ''naturalness'' is undermined, potentially reclassifying it closer to a Snare by revealing the theological justification as a cover for institutional power grabs. If genuinely natural, it reinforces the Mountain-like aspects of the underlying moral claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_law_naturalness, conceptual, 'Ambiguity regarding the naturalness of divine law as a basis for authority.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the observed suppression primarily structural (ecclesiastical/royal courts, physical force) or internalized (spiritual fear of damnation, moral conviction)?',
    'Analysis of penitential literature and legal records for evidence of genuine conversion versus mere compliance under duress, and the long-term persistence of anti-feud norms after the decline of direct enforcement.',
    'If largely internalized, the constraint''s effective suppression is higher and more resilient than purely structural measures suggest, indicating a deeper transformation of identity. If primarily structural, the constraint''s persistence is more dependent on active enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for anti-feud norms.').

omega_variable(
    mandate_shift_pacification_vs_control,
    'To what extent did the pacification efforts genuinely aim for social peace, versus serving as a pretext for centralizing ecclesiastical and royal power?',
    'Historical analysis of the allocation of resources and legal outcomes: did institutional interventions primarily resolve disputes fairly, or did they disproportionately benefit the central authorities and their allies?',
    'If primarily a pretext for power centralization, the constraint''s coordination function is diminished, and its extractive nature (transfer of authority) is amplified, pushing it closer to a Snare. If genuine social peace was the dominant outcome, it reinforces the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_shift_pacification_vs_control, empirical, 'Distinguishing genuine pacification from power centralization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__christianized_pacification_reading, 1000, 1500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t1000, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 1000, 0.12).
narrative_ontology:measurement(feud_tr_t1100, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 1100, 0.11).
narrative_ontology:measurement(feud_tr_t1200, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 1200, 0.1).
narrative_ontology:measurement(feud_tr_t1300, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 1300, 0.1).
narrative_ontology:measurement(feud_tr_t1400, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 1400, 0.09).
narrative_ontology:measurement(feud_tr_t1500, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 1500, 0.1).

% Extraction over time
narrative_ontology:measurement(feud_be_t1000, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 1000, 0.6).
narrative_ontology:measurement(feud_be_t1100, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 1100, 0.68).
narrative_ontology:measurement(feud_be_t1200, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 1200, 0.73).
narrative_ontology:measurement(feud_be_t1300, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 1300, 0.76).
narrative_ontology:measurement(feud_be_t1400, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 1400, 0.77).
narrative_ontology:measurement(feud_be_t1500, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 1500, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t1000, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 1000, 0.7).
narrative_ontology:measurement(feud_su_t1100, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 1100, 0.75).
narrative_ontology:measurement(feud_su_t1200, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 1200, 0.8).
narrative_ontology:measurement(feud_su_t1300, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 1300, 0.83).
narrative_ontology:measurement(feud_su_t1400, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 1400, 0.84).
narrative_ontology:measurement(feud_su_t1500, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 1500, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feud_obligation_kernel__christianized_pacification_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(feud_obligation_kernel__christianized_pacification_reading, royal_justice_system).
narrative_ontology:affects_constraint(feud_obligation_kernel__christianized_pacification_reading, ecclesiastical_courts).
narrative_ontology:affects_constraint(feud_obligation_kernel__christianized_pacification_reading, feud_obligation_kernel__extraction_cycle_reading).
narrative_ontology:affects_constraint(feud_obligation_kernel__christianized_pacification_reading, feud_obligation_kernel__stateless_coordination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'feud_obligation_kernel', focusing on the Christianized pacification efforts. It is linked to sibling readings that offer alternative interpretations of blood-feud obligations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
