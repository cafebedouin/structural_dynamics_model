% ============================================================================
% CONSTRAINT STORY: gendered_category_membership__social_role_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gendered_category_membership__social_role_reading, []).

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
 *   constraint_id: gendered_category_membership__social_role_reading
 *   human_readable: Gender Category Membership via Social Role Performance
 *   domain: social_ontology/political_philosophy/bioethics
 *
 * SUMMARY:
 *   This constraint describes the social-role reading of gender category
 *   membership: one becomes a member of a gender category (e.g., 'woman')
 *   through sustained performance of the associated social role and
 *   recognition by others. The reading is instantiated in feminist
 *   materialist and some sociological traditions (e.g., Butler's
 *   performativity interpreted through material recognition, Haslanger's
 *   social position account). Trans women are conditionally included if they
 *   pass and are recognized; cis women who fail to perform the role
 *   adequately risk exclusion; nonbinary individuals are excluded by the
 *   binary role structure. The constraint has a genuine coordination function
 *   — shared social roles reduce interaction friction — but also an
 *   asymmetric extraction: gatekeeping at role boundaries falls
 *   disproportionately on trans women and gender-nonconforming cis women,
 *   while recognized cis women and institutional gatekeepers benefit from
 *   stable category boundaries. The ε=0.38 reflects the performance burden
 *   (learning, maintaining, monitoring the role) which is low-to-moderate for
 *   recognized members but unbounded for those near the boundary.
 *   Suppression=0.42 reflects distributed social enforcement (misgendering,
 *   exclusion, violence) rather than centralized coercion. Theater_ratio=0.25
 *   captures performative adherence that exceeds functional coordination
 *   (ritualized gender performance).
 *
 * KEY AGENTS:
 *   - cisgender_women_recognized_in_role: Primary beneficiary (moderate/organized power, constrained exit) — enjoys stable category membership and its protections
 *   - institutional_gatekeepers: Secondary beneficiary/agenda_setter (institutional power, arbitrage exit) — controls access to women-only spaces, legal protections, data categories
 *   - trans_women_failing_recognition_threshold: Primary victim (powerless/moderate power, trapped/identity_locked exit) — bears performance costs without recognition payoff, faces exclusion
 *   - cis_women_policed_at_role_boundaries: Secondary victim (moderate power, constrained exit) — subject to boundary policing for insufficient performance
 *   - nonbinary_individuals_excluded_by_binary_roles: Victim (powerless, trapped exit) — structurally excluded by binary role architecture
 *   - analytical_observer: Observer (analytical power, analytical exit) — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__social_role_reading, 0.38).
domain_priors:suppression_score(gendered_category_membership__social_role_reading, 0.42).
domain_priors:theater_ratio(gendered_category_membership__social_role_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__social_role_reading, tangled_rope).
narrative_ontology:human_readable(gendered_category_membership__social_role_reading, "Gender Category Membership via Social Role Performance").
narrative_ontology:topic_domain(gendered_category_membership__social_role_reading, "social_ontology/political_philosophy/bioethics").

domain_priors:requires_active_enforcement(gendered_category_membership__social_role_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__social_role_reading, 'd0c79eb5-ce2b-4a32-ba30-fd01f22e2588').
narrative_ontology:cs_kernel_codification('d0c79eb5-ce2b-4a32-ba30-fd01f22e2588', formalized).
narrative_ontology:cs_authority_grounding('d0c79eb5-ce2b-4a32-ba30-fd01f22e2588', practice).
narrative_ontology:cs_interpretation_layer_present('d0c79eb5-ce2b-4a32-ba30-fd01f22e2588').
narrative_ontology:cs_reading_relation('d0c79eb5-ce2b-4a32-ba30-fd01f22e2588', gendered_category_membership__biological_sex_reading, coexists_with).
narrative_ontology:cs_reading_relation('d0c79eb5-ce2b-4a32-ba30-fd01f22e2588', gendered_category_membership__gender_identity_reading, coexists_with).
narrative_ontology:cs_axiom('d0c79eb5-ce2b-4a32-ba30-fd01f22e2588', foundational, recognition_constitutes_category_membership).
narrative_ontology:cs_axiom_status(recognition_constitutes_category_membership, holdable).
narrative_ontology:cs_axiom_grounding('d0c79eb5-ce2b-4a32-ba30-fd01f22e2588', recognition_constitutes_category_membership, conventional).
narrative_ontology:cs_axiom('d0c79eb5-ce2b-4a32-ba30-fd01f22e2588', foundational, sustained_performance_necessary_for_recognition).
narrative_ontology:cs_axiom_status(sustained_performance_necessary_for_recognition, holdable).
narrative_ontology:cs_axiom_grounding('d0c79eb5-ce2b-4a32-ba30-fd01f22e2588', sustained_performance_necessary_for_recognition, empirically_contingent).
narrative_ontology:cs_reference_frame('d0c79eb5-ce2b-4a32-ba30-fd01f22e2588', materialist_feminist_social_position).
narrative_ontology:cs_drift_state('d0c79eb5-ce2b-4a32-ba30-fd01f22e2588', contemporary_gender_identity_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d0c79eb5-ce2b-4a32-ba30-fd01f22e2588', '').
narrative_ontology:cs_kernel_id(gendered_category_membership__social_role_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__social_role_reading, cisgender_women_recognized_in_role).
narrative_ontology:constraint_beneficiary(gendered_category_membership__social_role_reading, institutional_gatekeepers).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, trans_women_failing_recognition_threshold).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, cis_women_policed_at_role_boundaries).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, nonbinary_individuals_excluded_by_binary_roles).
narrative_ontology:constraint_vindicates(gendered_category_membership__social_role_reading, gender_as_social_performance).
narrative_ontology:constraint_vindicates(gendered_category_membership__social_role_reading, recognition_as_constitutive_of_category).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enjoy stable recognition as women through sustained performance of the social role. Gain access to women-only spaces, legal protections, social intelligibility, and institutional categories. Exit from the role is constrained — abandoning the role risks loss of these benefits and social illegibility, but transitioning to another gender category is possible (though costly). The performance burden is experienced as background coordination cost rather than active extraction.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, cisgender_women_recognized_in_role, beneficiary,
    moderate, biographical, constrained, global).

% Administer the boundaries of the 'woman' category across legal, medical, sporting, and social-service institutions. Control access to women-only spaces, gender-specific funding, data categories, and legal protections. Benefit from administrable, stable categories that simplify governance and resource allocation. Can shift between biological, identity, or social-role criteria depending on institutional needs. Exit is arbitrage-grade — they operate the system rather than inhabit it.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, institutional_gatekeepers, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__social_role_reading, institutional_gatekeepers, beneficiary).

% Perform the woman role (transition, presentation, social practice) but fail to achieve consistent recognition. Pay unbounded performance costs — medical, social, psychological — without the recognition payoff that makes the role coordinative. Face exclusion from women's spaces, misgendering, violence, and legal precarity. Exit is identity_locked: gender transition creates profound path dependence (medical, legal, relational) that makes returning to a male role or existing outside gender categories extremely costly. The constraint extracts performance labor without delivering the coordination benefit.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, trans_women_failing_recognition_threshold, payer,
    powerless, biographical, identity_locked, global).

% Recognized as women but subject to intensified boundary policing — scrutiny of appearance, behavior, reproductive choices, and political positions. Pay the cost of performing the role perfectly to avoid delegitimization. The constraint extracts conformity labor: 'prove you are a real woman' policing falls heaviest on butch women, women of color, disabled women, and gender-nonconforming women. Exit is constrained — they can resist policing but cannot exit the category without losing its protections.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, cis_women_policed_at_role_boundaries, payer,
    moderate, biographical, constrained, global).

% Structurally excluded by the binary role architecture — no social role exists for nonbinary gender in this constraint's logic. Forced to perform either 'man' or 'woman' role to achieve any social intelligibility, or exist outside recognition entirely. Pay the cost of constant misrecognition or forced performance. Exit is trapped — the constraint's binary structure leaves no recognized position for them; creating new roles requires changing the constraint itself, which they lack power to do.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, nonbinary_individuals_excluded_by_binary_roles, payer,
    powerless, biographical, trapped, global).

% Analyzes the constraint's structure across readings and seats. Sees the kernel's three readings as a constraint family with distinct extraction profiles. Does not inhabit the constraint — no performance burden, no recognition stake. Exit is analytical: can shift between readings as analytical frames.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social interaction by providing shared, recognizable gender roles that reduce negotiation costs in reproduction, care, intimacy, institutional design, and resource allocation. The 'woman' role carries expectations, permissions, and protections that function as a social protocol.
% TRANSFER_FUNCTION: Moves performance labor (gender presentation, emotional labor, reproductive labor, boundary maintenance) from trans women, non-conforming cis women, and nonbinary individuals to recognized cis women and institutional gatekeepers, who capture the benefits of stable categories (protections, intelligibility, administrability).
% ABSENT_VOICES: Trans women who have achieved recognition but reject the social-role framework (they would attest that recognition is not constitutive). Nonbinary individuals who have created recognized third-gender categories in some jurisdictions (they would attest that binary roles are not necessary). Historical and cross-cultural gender systems that operate without this specific role structure. These voices are excluded from the dominant Western philosophical and policy discourse that treats this reading as definitive.
% DISAPPEARANCE_RATIONALE: If the social-role constraint vanished overnight, the coordination it provides (shared expectations for 'woman' role) would collapse. Institutions would lose administrable categories. Recognized cis women would lose guaranteed access to sex-based protections. Trans women would lose the conditional inclusion pathway. New coordination mechanisms would need to emerge — likely a mix of self-declaration (gender_identity_reading) and biological criteria (biological_sex_reading) — causing massive institutional and social reorganization.
% FOUNDING_PROBLEM: Coordinating the division of reproductive and care labor in pre-industrial and early industrial societies required stable, publicly recognizable categories mapping onto biological sex. The 'woman' role coordinated gestation, lactation, child-rearing, and domestic labor through shared social expectations and institutional supports.
% FOUNDING_PROBLEM_CORROBORATION: Materialist feminists (e.g., Wittig, Delphy) and anthropologists (e.g., Ortner, Strathern) corroborate that gender roles historically coordinated reproductive labor. Evolutionary anthropologists corroborate the cross-cultural patterning of gendered division of labor. However, contemporary reproductive technology (contraception, IVF, surrogacy), care outsourcing, and gender-egalitarian norms mean the founding problem is substantially altered — the arrangement persists while the problem has shifted, which is the contested status.
narrative_ontology:disappearance_verdict(gendered_category_membership__social_role_reading, world_rearranges).
narrative_ontology:founding_problem_status(gendered_category_membership__social_role_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__social_role_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(gendered_category_membership__social_role_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gendered_category_membership__social_role_reading, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gendered_category_membership__social_role_reading_tests).
:- end_tests(gendered_category_membership__social_role_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claimed_type is tangled_rope because: (1) genuine coordination function exists — shared gender roles reduce coordination costs in social interaction, child-rearing division, institutional design; (2) asymmetric extraction is present — gatekeeping at boundaries extracts performance labor from trans women and non-conforming cis women while recognized cis women and gatekeepers capture the benefits of stable categories; (3) active enforcement is required — the constraint persists only through continuous social policing (misgendering, bathroom bills, sports bans, pronoun norms). The ε=0.38 is moderate: the performance burden is real but not totalizing for recognized members. The victim structure is ambiguous — both trans women (excluded despite performance) and cis women (policed for boundary maintenance) bear costs, suggesting the constraint extracts from both sides of the boundary.
 *
 * PERSPECTIVAL GAP:
 *   From the recognized cis woman's seat: the constraint is a rope — coordination works, benefits are real, costs are manageable. From the trans woman failing recognition: the constraint is a snare — performance is demanded but recognition is withheld, extraction is pure. From the institutional gatekeeper: the constraint is a scaffold — it provides administrable categories but requires constant boundary defense. The engine will compute per-seat classifications from the structural data; this divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Recognized cis women are beneficiaries (d ~ 0.2): they receive category protections, social intelligibility, institutional access. Gatekeepers are agenda_setters with beneficiary characteristics (d ~ 0.1): they administer the boundary and capture institutional rents (data, funding, legitimacy). Trans women failing recognition are payers (d ~ 0.85): they pay performance costs without recognition returns, exit is identity_locked (gender transition creates path dependence). Cis women at boundaries are payers (d ~ 0.6): they pay policing costs to maintain membership. Nonbinary individuals are payers (d ~ 0.9): structurally excluded, exit is trapped (no role exists). Directionality derives from beneficiary/victim declarations + exit options + power.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (coordinating reproduction and care via stable social roles) is contested — some argue it's live (reproduction still needs coordination), others that it's dead (contraception, reproductive tech, gender-egalitarian norms solve it differently). The constraint persists partly because the recognition-dependence mechanism serves current gatekeepers (institutions, recognized members) even if the original coordination problem has shifted. The engine's per-seat computation captures this: beneficiaries see live coordination; victims see extracted mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disambiguation,
    'Is this constraint a distinct reading of the gendered_category_membership kernel, or does it collapse into the gender_identity_reading under scrutiny?',
    'Test whether the social_role_reading makes normative claims that the gender_identity_reading explicitly rejects (e.g., recognition-dependence vs. self-declaration). If the claims are logically incompatible within one framework, they are distinct readings; if compatible, they may be the same constraint with different labels.',
    'If distinct, the kernel has at least three structurally different constraints (biological, identity, social-role) each with its own ε, beneficiaries, and victims. If not distinct, the social_role_reading is a variant of the identity reading and should be consolidated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disambiguation, conceptual, 'Whether social_role_reading is a separate constraint or a restatement of gender_identity_reading').

omega_variable(
    victim_structure_ambiguity,
    'Do trans women failing recognition AND cis women policed at boundaries constitute a unified victim class or two structurally opposed groups?',
    'Analyze whether the exclusion risk flows from the same mechanism (gatekeeping at role boundaries) or from different mechanisms (misrecognition vs. over-policing). If unified, victims share a directional target; if opposed, the constraint extracts from both sides of the boundary.',
    'Unified victims → single extraction vector, higher χ for powerless agents. Opposed victims → constraint acts as a boundary enforcement mechanism extracting from both sides, different directionality for each group.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_structure_ambiguity, conceptual, 'Whether the declared victims are structurally aligned or opposed').

omega_variable(
    recognition_dependence_vs_self_declaration,
    'Does this reading''s claim that recognition is constitutive of category membership foreclose the sibling reading''s claim that self-declaration is sufficient?',
    'Examine the logical structure: if category membership REQUIRES recognition by others (social_role), then self-declaration ALONE (gender_identity) is insufficient. If the social_role_reading holds that recognition is necessary but not sufficient, and self-declaration is necessary but not sufficient, they may coexist as partial conditions.',
    'If forecloses, the readings cannot both be true in the same framework — adopting social_role_reading logically commits one to rejecting gender_identity_reading''s sufficiency claim. If coexists_with, different parties can hold each reading without contradiction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recognition_dependence_vs_self_declaration, conceptual, 'Logical relationship between recognition-dependence and self-declaration-sufficiency').

omega_variable(
    extraction_mechanism_vs_coordination_function,
    'Is the performance cost (ε=0.38) primarily the cost of genuine coordination (learning and performing a social role) or the cost of extractive gatekeeping (exclusion for boundary maintenance)?',
    'Decompose the performance cost: what fraction is skill-acquisition/maintenance (coordination) vs. conformity-enforcement/test-passing (extraction). Compare to purely coordinative social roles (e.g., professional licensing) where performance costs are universally accepted.',
    'If primarily coordinative → tangled_rope with genuine coordination function. If primarily extractive → snare with coordination as cover. The boundary determines whether the constraint''s persistence is justified by the coordination it enables.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_mechanism_vs_coordination_function, empirical, 'Whether performance costs are coordinative or extractive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__social_role_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gend_tr_t0, gendered_category_membership__social_role_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(gend_tr_t8, gendered_category_membership__social_role_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement(gend_tr_t16, gendered_category_membership__social_role_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(gend_tr_t24, gendered_category_membership__social_role_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement(gend_tr_t32, gendered_category_membership__social_role_reading, theater_ratio, 32, 0.24).
narrative_ontology:measurement(gend_tr_t40, gendered_category_membership__social_role_reading, theater_ratio, 40, 0.25).

% Extraction over time
narrative_ontology:measurement(gend_be_t0, gendered_category_membership__social_role_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(gend_be_t8, gendered_category_membership__social_role_reading, base_extractiveness, 8, 0.28).
narrative_ontology:measurement(gend_be_t16, gendered_category_membership__social_role_reading, base_extractiveness, 16, 0.33).
narrative_ontology:measurement(gend_be_t24, gendered_category_membership__social_role_reading, base_extractiveness, 24, 0.36).
narrative_ontology:measurement(gend_be_t32, gendered_category_membership__social_role_reading, base_extractiveness, 32, 0.37).
narrative_ontology:measurement(gend_be_t40, gendered_category_membership__social_role_reading, base_extractiveness, 40, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(gend_su_t0, gendered_category_membership__social_role_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(gend_su_t8, gendered_category_membership__social_role_reading, suppression_requirement, 8, 0.3).
narrative_ontology:measurement(gend_su_t16, gendered_category_membership__social_role_reading, suppression_requirement, 16, 0.35).
narrative_ontology:measurement(gend_su_t24, gendered_category_membership__social_role_reading, suppression_requirement, 24, 0.38).
narrative_ontology:measurement(gend_su_t32, gendered_category_membership__social_role_reading, suppression_requirement, 32, 0.4).
narrative_ontology:measurement(gend_su_t40, gendered_category_membership__social_role_reading, suppression_requirement, 40, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gendered_category_membership__social_role_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gendered_category_membership__social_role_reading, 0.1).
narrative_ontology:affects_constraint(gendered_category_membership__social_role_reading, gendered_category_membership__biological_sex_reading).
narrative_ontology:affects_constraint(gendered_category_membership__social_role_reading, gendered_category_membership__gender_identity_reading).

% DUAL FORMULATION NOTE:
% This constraint and its two siblings form the gendered_category_membership constraint family. The kernel (gendered_category_membership) is the contested commitment; each reading instantiates a different constraint with different ε, beneficiaries, victims, and claimed_type. The biological_sex_reading treats the kernel as fixed_text with lineage authority; the gender_identity_reading treats it as distributed with self_enforcing authority; this social_role_reading treats it as formalized with practice authority. The readings are linked via affects_constraints in all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gendered_category_membership__social_role_reading, moderate, 0.6).
constraint_indexing:directionality_override(gendered_category_membership__social_role_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
