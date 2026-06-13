% ============================================================================
% CONSTRAINT STORY: marriage_authority__communal_autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__communal_autonomy_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: marriage_authority__communal_autonomy_reading
 *   human_readable: Marriage Authority: Communal Autonomy Reading
 *   domain: legal_pluralism/constitutional_law/comparative_family_law
 *
 * SUMMARY:
 *   This constraint describes the operation of marriage authority where the
 *   state enforces personal laws derived from religious community traditions,
 *   rather than authoring a uniform civil code. This 'communal autonomy'
 *   reading emphasizes the right of religious communities to govern their
 *   internal affairs, including family law, with state backing. The state's
 *   role is primarily to register and enforce these community-specific norms,
 *   not to legislate their content. This is one reading of the broader
 *   'marriage_authority' kernel, which is contested across various legal and
 *   social perspectives.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__communal_autonomy_reading, 0.35).
domain_priors:suppression_score(marriage_authority__communal_autonomy_reading, 0.6).
domain_priors:theater_ratio(marriage_authority__communal_autonomy_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__communal_autonomy_reading, rope).
narrative_ontology:human_readable(marriage_authority__communal_autonomy_reading, "Marriage Authority: Communal Autonomy Reading").
narrative_ontology:topic_domain(marriage_authority__communal_autonomy_reading, "legal_pluralism/constitutional_law/comparative_family_law").

domain_priors:requires_active_enforcement(marriage_authority__communal_autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__communal_autonomy_reading, '6eef369e-fb30-4989-a783-54dd22156fff').
narrative_ontology:cs_kernel_codification('6eef369e-fb30-4989-a783-54dd22156fff', formalized).
narrative_ontology:cs_authority_grounding('6eef369e-fb30-4989-a783-54dd22156fff', lineage).
narrative_ontology:cs_interpretation_layer_present('6eef369e-fb30-4989-a783-54dd22156fff').
narrative_ontology:cs_reading_relation('6eef369e-fb30-4989-a783-54dd22156fff', marriage_authority__secularist_reading, forecloses).
narrative_ontology:cs_reading_relation('6eef369e-fb30-4989-a783-54dd22156fff', marriage_authority__gender_rights_reading, influences).
narrative_ontology:cs_reading_relation('6eef369e-fb30-4989-a783-54dd22156fff', marriage_authority__federalist_millet_reading, coexists_with).
narrative_ontology:cs_reading_relation('6eef369e-fb30-4989-a783-54dd22156fff', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_axiom('6eef369e-fb30-4989-a783-54dd22156fff', foundational, communal_self_governance_in_personal_law).
narrative_ontology:cs_axiom_status(communal_self_governance_in_personal_law, holdable).
narrative_ontology:cs_axiom_grounding('6eef369e-fb30-4989-a783-54dd22156fff', communal_self_governance_in_personal_law, conventional).
narrative_ontology:cs_axiom('6eef369e-fb30-4989-a783-54dd22156fff', foundational, state_enforcement_of_religious_personal_law).
narrative_ontology:cs_axiom_status(state_enforcement_of_religious_personal_law, holdable).
narrative_ontology:cs_axiom_grounding('6eef369e-fb30-4989-a783-54dd22156fff', state_enforcement_of_religious_personal_law, conventional).
narrative_ontology:cs_reference_frame('6eef369e-fb30-4989-a783-54dd22156fff', traditional_communal_legal_autonomy).
narrative_ontology:cs_drift_state('6eef369e-fb30-4989-a783-54dd22156fff', contemporary_human_rights_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6eef369e-fb30-4989-a783-54dd22156fff', '').
narrative_ontology:cs_kernel_id(marriage_authority__communal_autonomy_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__communal_autonomy_reading, religious_community_leaders).
narrative_ontology:constraint_beneficiary(marriage_authority__communal_autonomy_reading, religious_community_members).
narrative_ontology:constraint_victim(marriage_authority__communal_autonomy_reading, intra_community_dissenters).
narrative_ontology:constraint_victim(marriage_authority__communal_autonomy_reading, women_seeking_equality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and administer religious personal laws, acting as the primary authority within their community regarding marriage, divorce, and inheritance. They benefit from the preservation of traditional authority and community cohesion.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, religious_community_leaders, agenda_setter,
    institutional, generational, constrained, local).

% Adhere to the communal marriage norms, benefiting from the social stability, cultural continuity, and identity provided by the community's traditions. Their identity is often deeply intertwined with their religious affiliation, making exit from communal norms difficult.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, religious_community_members, beneficiary,
    moderate, biographical, identity_locked, local).

% Are members of the religious community who disagree with certain aspects of the traditional marriage laws, particularly regarding gender equality or personal autonomy. They bear the costs of limited legal options and social pressure, with state enforcement reinforcing these constraints.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, intra_community_dissenters, payer,
    powerless, biographical, constrained, local).

% Specifically seek reform of personal laws to ensure gender equality within marriage, divorce, and inheritance. They face significant structural barriers and social resistance, with limited avenues for legal redress within the communal system.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, women_seeking_equality, payer,
    powerless, biographical, constrained, national).

% Registers and enforces the personal laws of various religious communities, providing legal backing to communal authority without directly legislating the content of these laws. It benefits from managing social diversity but bears the cost of potential constitutional challenges.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, the_state, agenda_setter,
    institutional, generational, constrained, national).

% Review challenges to personal laws on constitutional grounds, particularly regarding equality and non-discrimination. Their rulings can influence the interpretation and application of communal norms, potentially leading to reform or reinterpretation.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social and legal recognition of marriage, divorce, and inheritance within diverse religious communities, allowing for distinct communal identities and traditions to persist under state recognition.
% TRANSFER_FUNCTION: Transfers authority over family law matters from a centralized state legislature to religious community leaders, in exchange for social stability and the preservation of diverse cultural identities. It also transfers legal costs and social pressure to dissenters within these communities.
% ABSENT_VOICES: Advocates for a Uniform Civil Code (UCC) are absent from the direct negotiation of communal personal laws; they would argue for a single, secular family law system based on universal principles, but their proposals are often resisted by religious communities and political parties.
% DISAPPEARANCE_RATIONALE: If state enforcement of communal marriage authority vanished, religious communities would face legal uncertainty, potentially leading to internal fragmentation or a shift towards informal, non-state-recognized arrangements. The state would lose a mechanism for managing legal pluralism, and individual rights claims might become more prominent, forcing a re-evaluation of family law.
% FOUNDING_PROBLEM: The problem of governing diverse religious communities with distinct personal laws, inherited from colonial or pre-modern legal systems, while maintaining state sovereignty and social cohesion.
% FOUNDING_PROBLEM_CORROBORATION: Religious community leaders and many members attest that the problem of preserving distinct religious identities and traditions in family law remains live. Constitutional scholars and human rights advocates, while acknowledging the historical context, contest the current status, arguing that the problem has evolved into one of individual rights versus communal autonomy, requiring new solutions.
narrative_ontology:disappearance_verdict(marriage_authority__communal_autonomy_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__communal_autonomy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__communal_autonomy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(marriage_authority__communal_autonomy_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__communal_autonomy_reading_tests).
:- end_tests(marriage_authority__communal_autonomy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.35) is moderate, reflecting the costs borne by individuals whose personal choices may be constrained by communal norms, but also the benefits of community cohesion. Suppression (0.6) is significant because the state's enforcement power is used to uphold these norms, limiting exit options for dissenters. Theater ratio (0.1) is low, as the enforcement is direct and functional. Accessibility collapse is moderate (0.4) because while communal norms are strong, some legal avenues for challenge or alternative arrangements may exist, albeit with high friction. Resistance (0.3) is present from intra-community dissenters and women's rights advocates, but often diffuse.
 *
 * PERSPECTIVAL GAP:
 *   Religious community leaders experience this as a Rope, providing stable social order and preserving tradition. Intra-community dissenters, particularly women seeking greater equality, experience it as a Tangled Rope or Snare, where the state's enforcement of communal norms limits their autonomy and exit options. The state, from an administrative perspective, sees it as a coordination mechanism for managing diverse populations.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious community leaders are primary beneficiaries (d=0.0-0.1) as their authority is upheld and community cohesion maintained. Religious community members are also beneficiaries (d=0.1-0.3) as they benefit from stable social structures, though some bear costs. Intra-community dissenters and women seeking equality are victims (d=0.7-0.9) as their choices are constrained by norms they may not agree with, with state enforcement limiting their exit. The state is an agenda-setter (d=0.4-0.5) in that it administers the system, but its directionality is closer to symmetric as it bears the costs of enforcement while benefiting from social stability.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate is to preserve communal identity and tradition through self-governance in family matters. This mandate is still live for the religious communities. However, for dissenters, the original coordination function (community cohesion) has become a cover for extraction (restriction of individual autonomy). The classification as Rope (claimed) vs. potential Tangled Rope/Snare (computed for dissenters) highlights this divergence, preventing mislabeling genuine coordination as pure extraction, or vice-versa, by revealing the per-seat experience.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    communal_vs_individual_rights,
    'Does the state''s enforcement of communal marriage norms unduly restrict the individual rights of dissenters within the community?',
    'Judicial review of specific cases challenging communal norms on constitutional equality grounds; legislative action to establish minimum individual rights protections.',
    'If individual rights are found to be unduly restricted, the constraint would shift towards a Tangled Rope or Snare for dissenters, requiring greater state intervention or reclassification of the state''s role.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(communal_vs_individual_rights, conceptual, 'Ambiguity between collective communal autonomy and individual constitutional rights.').

omega_variable(
    natural_law_vs_social_construct,
    'Is the authority of religious marriage law perceived as a natural, immutable order by the community, or as a social construct maintained by tradition and state enforcement?',
    'Sociological studies of community members'' beliefs; historical analysis of legal evolution and adaptation of religious laws.',
    'If perceived as natural law, resistance to change will be higher, and the constraint''s persistence will be more robust. If seen as a social construct, it is more vulnerable to internal and external pressures for reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_social_construct, empirical, 'Perception of religious marriage law as natural vs. constructed.').

omega_variable(
    kernel_reading_identification,
    'This constraint is a ''communal_autonomy_reading'' of the ''marriage_authority'' kernel. What structural elements would change under a ''secularist_reading''?',
    'Analysis of proposed Uniform Civil Code legislation and its impact on personal law variation.',
    'A secularist reading would eliminate personal law variation, shifting authority from religious communities to the state legislature, likely increasing suppression for religious communities and decreasing it for dissenters.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Identifies this constraint as one reading of the marriage_authority kernel and notes structural changes under a secularist reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__communal_autonomy_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority__communal_autonomy_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(marr_be_t10, marriage_authority__communal_autonomy_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(marr_be_t20, marriage_authority__communal_autonomy_reading, base_extractiveness, 20, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority__communal_autonomy_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(marr_su_t10, marriage_authority__communal_autonomy_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(marr_su_t20, marriage_authority__communal_autonomy_reading, suppression_requirement, 20, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__communal_autonomy_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, marriage_authority__secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, marriage_authority__gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, marriage_authority__federalist_millet_reading).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, marriage_authority__judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'marriage_authority' kernel, focusing on communal autonomy. Other readings (secularist, gender rights, federalist millet, judicial harmonization) represent alternative structural claims about the same underlying social and legal phenomenon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
