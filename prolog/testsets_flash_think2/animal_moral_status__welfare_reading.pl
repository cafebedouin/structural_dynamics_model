% ============================================================================
% CONSTRAINT STORY: animal_moral_status__welfare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_moral_status__welfare_reading, []).

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
 *   constraint_id: animal_moral_status__welfare_reading
 *   human_readable: Animal Welfare Standard (Regulated Use Reading)
 *   domain: applied_ethics/animal_studies/legal_philosophy
 *
 * SUMMARY:
 *   This constraint represents the 'welfare_reading' of the
 *   'animal_moral_status' kernel. It asserts that animals are sentient beings
 *   whose suffering should be minimized within systems of regulated use,
 *   distinguishing between impermissible cruelty and permissible (though
 *   regulated) use. This reading aims to coordinate human behavior around
 *   animal use, providing a social license for industries while mitigating
 *   public and ethical concerns. The claimed type is 'rope' due to its
 *   coordination function, but the metrics reflect a 'tangled_rope overlay'
 *   where inherent extraction from animals and suppression of abolitionist
 *   alternatives are present.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_moral_status__welfare_reading, 0.45).
domain_priors:suppression_score(animal_moral_status__welfare_reading, 0.6).
domain_priors:theater_ratio(animal_moral_status__welfare_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__welfare_reading, rope).
narrative_ontology:human_readable(animal_moral_status__welfare_reading, "Animal Welfare Standard (Regulated Use Reading)").
narrative_ontology:topic_domain(animal_moral_status__welfare_reading, "applied_ethics/animal_studies/legal_philosophy").

domain_priors:requires_active_enforcement(animal_moral_status__welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__welfare_reading, 'ca504f21-9d56-4b49-971c-b31814b116cd').
narrative_ontology:cs_kernel_codification('ca504f21-9d56-4b49-971c-b31814b116cd', formalized).
narrative_ontology:cs_authority_grounding('ca504f21-9d56-4b49-971c-b31814b116cd', practice).
narrative_ontology:cs_interpretation_layer_present('ca504f21-9d56-4b49-971c-b31814b116cd').
narrative_ontology:cs_reading_relation('ca504f21-9d56-4b49-971c-b31814b116cd', animal_moral_status__property_reading, influences).
narrative_ontology:cs_reading_relation('ca504f21-9d56-4b49-971c-b31814b116cd', animal_moral_status__abolitionist_reading, coexists_with).
narrative_ontology:cs_axiom('ca504f21-9d56-4b49-971c-b31814b116cd', foundational, animal_sentience_entails_moral_consideration).
narrative_ontology:cs_axiom_status(animal_sentience_entails_moral_consideration, holdable).
narrative_ontology:cs_axiom_grounding('ca504f21-9d56-4b49-971c-b31814b116cd', animal_sentience_entails_moral_consideration, deontological).
narrative_ontology:cs_axiom('ca504f21-9d56-4b49-971c-b31814b116cd', foundational, human_use_of_animals_is_permissible_under_welfare_standards).
narrative_ontology:cs_axiom_status(human_use_of_animals_is_permissible_under_welfare_standards, holdable).
narrative_ontology:cs_axiom_grounding('ca504f21-9d56-4b49-971c-b31814b116cd', human_use_of_animals_is_permissible_under_welfare_standards, conventional).
narrative_ontology:cs_reference_frame('ca504f21-9d56-4b49-971c-b31814b116cd', enlightened_stewardship_framework).
narrative_ontology:cs_drift_state('ca504f21-9d56-4b49-971c-b31814b116cd', contemporary_animal_welfare_science_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ca504f21-9d56-4b49-971c-b31814b116cd', '').
narrative_ontology:cs_kernel_id(animal_moral_status__welfare_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, welfare_organizations).
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, regulated_animal_industries).
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, consumers_of_animal_products).
narrative_ontology:constraint_victim(animal_moral_status__welfare_reading, animals_in_regulated_use).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, scientific_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for and help define 'humane' standards, gaining legitimacy and funding by operating within the framework of regulated animal use. They push for higher standards but accept the premise of use.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, welfare_organizations, agenda_setter,
    organized, generational, constrained, global).

% Operate systems of animal use (e.g., agriculture, research) under welfare regulations. They benefit from social license and public acceptance, which the welfare framework provides, allowing continued operation despite ethical concerns.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, regulated_animal_industries, beneficiary,
    institutional, generational, constrained, global).

% Purchase and consume animal products with reduced moral discomfort, believing that the animals involved were treated 'humanely' and their suffering minimized. They benefit from the availability of these products and the moral framework that legitimizes their consumption.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, consumers_of_animal_products, beneficiary,
    moderate, biographical, mobile, global).

% Are the subjects of regulated use. Despite efforts to minimize suffering, they still experience confinement, manipulation, and ultimately death for human purposes. Their interests are considered but remain subordinate to human interests in use.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, animals_in_regulated_use, payer,
    powerless, immediate, trapped, universal).

% Reject the premise of animal use entirely, viewing it as an inherent violation of animal rights. They are structurally excluded from the 'regulated use' framework, as their core demand (no use) is outside its scope, and their efforts are often framed as extreme or impractical.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, abolitionist_advocates, excluded,
    organized, generational, identity_locked, global).

% Codify and enforce animal welfare laws and regulations. They provide the institutional framework for defining and policing cruelty, and for legitimizing regulated use, balancing competing interests within society.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, legal_systems, agenda_setter,
    institutional, generational, analytical, national).

% Utilize animals in research under strict ethical guidelines and welfare protocols. They benefit from the continued permissibility of animal research, which the welfare framework enables, while contributing to the scientific understanding of animal sentience and welfare.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, scientific_researchers, beneficiary,
    powerful, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_moral_status__welfare_reading, regulated_animal_industries).
narrative_ontology:fixing_cost_class(animal_moral_status__welfare_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates human behavior around animal use by establishing a common, evolving standard for acceptable treatment, minimizing public discomfort with animal exploitation, and providing a framework for legal and ethical oversight.
% TRANSFER_FUNCTION: Transfers moral comfort, social license, and continued economic viability to human users and industries, while transferring (minimized) suffering, instrumentalization, and loss of autonomy to animals.
% ABSENT_VOICES: Abolitionist advocates are structurally excluded from the core framing of 'regulated use'; they would argue that use itself is the problem, not just cruelty, and that the welfare framework merely legitimizes exploitation.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the moral and legal landscape around animal use would become highly polarized. Industries would face intense pressure from both property-rights advocates (unfettered use) and abolitionists (no use), leading to legal and social chaos, and potentially increased animal suffering in unregulated contexts. The current equilibrium, however imperfect, would collapse.
% FOUNDING_PROBLEM: Unchecked cruelty towards animals and growing public revulsion, leading to calls for either outright abolition or more humane treatment, creating a need for a mediating framework.
% FOUNDING_PROBLEM_CORROBORATION: Animal welfare science, public opinion polls, and legislative history consistently corroborate the problem of animal suffering and the ongoing need for regulation. While the severity of the 'unchecked cruelty' has diminished, the underlying tension between use and suffering remains live, as attested by independent ethicists and scientific bodies.
narrative_ontology:disappearance_verdict(animal_moral_status__welfare_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_moral_status__welfare_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__welfare_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(animal_moral_status__welfare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_moral_status__welfare_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_moral_status__welfare_reading_tests).
:- end_tests(animal_moral_status__welfare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is moderate (0.45) because animals still experience suffering and instrumentalization, even when 'minimized' by human standards. Suppression is significant (0.60) as the framework actively marginalizes and suppresses abolitionist perspectives that challenge the premise of use itself. Theater ratio is moderate (0.30) reflecting some performative aspects of 'humane' labeling, though genuine welfare improvements also occur. Resistance is high (0.70) due to ongoing advocacy from animal rights groups. The temporal measurements show a slow but steady increase in extractiveness, suppression, and theater, suggesting a drift towards greater entrenchment and potential 'humane washing' over time, even as welfare standards nominally improve.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of welfare organizations and industries, this is a necessary and beneficial coordination mechanism. From the perspective of animals, it is a system of inherent extraction, however 'humane.' Abolitionist advocates view it as a sophisticated form of suppression. The engine's per-seat classification will highlight these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Welfare organizations, regulated industries, and consumers are beneficiaries, gaining legitimacy, social license, and moral comfort, respectively. Animals in regulated use are the primary payers/victims, bearing the costs of instrumentalization and suffering. Abolitionist advocates are excluded, as their core premise is incompatible with the 'regulated use' framework. Legal systems and scientific researchers act as agenda-setters and beneficiaries, shaping and benefiting from the framework's operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    welfare_reading_vs_property_reading,
    'How does this ''welfare_reading'' structurally differ from the ''property_reading'' of the animal moral status kernel?',
    'Analysis of legal and ethical frameworks: the ''welfare_reading'' introduces explicit moral consideration for animal suffering, which is absent in the ''property_reading'' where animals are mere resources.',
    'If the ''property_reading'' were dominant, animals would have no legal standing against cruelty, and extractiveness would be higher, suppression of welfare concerns lower. This reading introduces a constraint on human action.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(welfare_reading_vs_property_reading, conceptual, 'Distinction from the property-only view of animals.').

omega_variable(
    welfare_reading_vs_abolitionist_reading,
    'How does this ''welfare_reading'' structurally differ from the ''abolitionist_reading'' of the animal moral status kernel?',
    'Analysis of core premises: the ''welfare_reading'' accepts the permissibility of animal use, while the ''abolitionist_reading'' rejects it entirely. This difference dictates the victim set and the nature of extraction.',
    'If the ''abolitionist_reading'' were dominant, all use would be extraction, and the victim set would be all animals, not just those suffering under ''humane'' use. This reading legitimizes use, which the abolitionist reading condemns.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(welfare_reading_vs_abolitionist_reading, conceptual, 'Distinction from the abolitionist view of animals.').

omega_variable(
    suffering_minimization_feasibility,
    'Is ''minimizing suffering'' genuinely achievable within systems of regulated use, or does the inherent nature of instrumentalization always entail significant, un-minimizable suffering?',
    'Longitudinal studies of animal welfare outcomes in regulated industries, combined with philosophical analysis of the concept of ''minimization'' in contexts of power asymmetry.',
    'If significant suffering is inherent and un-minimizable, the ''rope'' aspect of coordination is weaker, and the ''tangled_rope'' or ''snare'' aspects (extraction from animals) are stronger, potentially leading to a reclassification of the constraint''s effective type for animals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suffering_minimization_feasibility, empirical, 'The practical and ethical limits of ''suffering minimization'' in animal use.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__welfare_reading, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t1970, animal_moral_status__welfare_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(anim_tr_t1980, animal_moral_status__welfare_reading, theater_ratio, 1980, 0.23).
narrative_ontology:measurement(anim_tr_t1990, animal_moral_status__welfare_reading, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(anim_tr_t2000, animal_moral_status__welfare_reading, theater_ratio, 2000, 0.27).
narrative_ontology:measurement(anim_tr_t2010, animal_moral_status__welfare_reading, theater_ratio, 2010, 0.29).
narrative_ontology:measurement(anim_tr_t2020, animal_moral_status__welfare_reading, theater_ratio, 2020, 0.3).

% Extraction over time
narrative_ontology:measurement(anim_be_t1970, animal_moral_status__welfare_reading, base_extractiveness, 1970, 0.35).
narrative_ontology:measurement(anim_be_t1980, animal_moral_status__welfare_reading, base_extractiveness, 1980, 0.38).
narrative_ontology:measurement(anim_be_t1990, animal_moral_status__welfare_reading, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(anim_be_t2000, animal_moral_status__welfare_reading, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement(anim_be_t2010, animal_moral_status__welfare_reading, base_extractiveness, 2010, 0.44).
narrative_ontology:measurement(anim_be_t2020, animal_moral_status__welfare_reading, base_extractiveness, 2020, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t1970, animal_moral_status__welfare_reading, suppression_requirement, 1970, 0.5).
narrative_ontology:measurement(anim_su_t1980, animal_moral_status__welfare_reading, suppression_requirement, 1980, 0.53).
narrative_ontology:measurement(anim_su_t1990, animal_moral_status__welfare_reading, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(anim_su_t2000, animal_moral_status__welfare_reading, suppression_requirement, 2000, 0.57).
narrative_ontology:measurement(anim_su_t2010, animal_moral_status__welfare_reading, suppression_requirement, 2010, 0.59).
narrative_ontology:measurement(anim_su_t2020, animal_moral_status__welfare_reading, suppression_requirement, 2020, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_moral_status__welfare_reading, attachment_coordination).
narrative_ontology:affects_constraint(animal_moral_status__welfare_reading, animal_moral_status__property_reading).
narrative_ontology:affects_constraint(animal_moral_status__welfare_reading, animal_moral_status__abolitionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'animal_moral_status' kernel, each representing a distinct structural claim about animal moral standing and human obligations. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
