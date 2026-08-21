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
 *   constraint_id: marriage_authority__communal_autonomy_reading
 *   human_readable: Communal Autonomy Reading of Marriage Authority
 *   domain: legal_pluralism/constitutional_law/comparative_family_law
 *
 * SUMMARY:
 *   This constraint is the `communal_autonomy_reading` of the
 *   `marriage_authority` kernel, which posits that marriage and family law
 *   norms are legitimately derived from community religious traditions, with
 *   the state's role limited to enforcement rather than authorship. Sibling
 *   readings include `secularist_reading`, `gender_rights_reading`,
 *   `federalist_millet_reading`, and `judicial_harmonization_reading`. The
 *   constraint operates as a Tangled Rope: it provides genuine coordination
 *   for traditionalist members but extracts from dissenters through
 *   state-enforced religious norms.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__communal_autonomy_reading, 0.65).
domain_priors:suppression_score(marriage_authority__communal_autonomy_reading, 0.7).
domain_priors:theater_ratio(marriage_authority__communal_autonomy_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__communal_autonomy_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority__communal_autonomy_reading, "Communal Autonomy Reading of Marriage Authority").
narrative_ontology:topic_domain(marriage_authority__communal_autonomy_reading, "legal_pluralism/constitutional_law/comparative_family_law").

domain_priors:requires_active_enforcement(marriage_authority__communal_autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__communal_autonomy_reading, '2c619051-72cb-46d6-a9d9-711c407447a9').
narrative_ontology:cs_kernel_codification('2c619051-72cb-46d6-a9d9-711c407447a9', formalized).
narrative_ontology:cs_authority_grounding('2c619051-72cb-46d6-a9d9-711c407447a9', lineage).
narrative_ontology:cs_interpretation_layer_present('2c619051-72cb-46d6-a9d9-711c407447a9').
narrative_ontology:cs_reading_relation('2c619051-72cb-46d6-a9d9-711c407447a9', marriage_authority__secularist_reading, forecloses).
narrative_ontology:cs_reading_relation('2c619051-72cb-46d6-a9d9-711c407447a9', marriage_authority__gender_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('2c619051-72cb-46d6-a9d9-711c407447a9', marriage_authority__federalist_millet_reading, influences).
narrative_ontology:cs_reading_relation('2c619051-72cb-46d6-a9d9-711c407447a9', marriage_authority__judicial_harmonization_reading, coexists_with).
narrative_ontology:cs_axiom('2c619051-72cb-46d6-a9d9-711c407447a9', foundational, community_religious_law_is_supreme_in_personal_matters).
narrative_ontology:cs_axiom_status(community_religious_law_is_supreme_in_personal_matters, holdable).
narrative_ontology:cs_axiom_grounding('2c619051-72cb-46d6-a9d9-711c407447a9', community_religious_law_is_supreme_in_personal_matters, theological).
narrative_ontology:cs_axiom('2c619051-72cb-46d6-a9d9-711c407447a9', foundational, state_role_is_enforcement_not_authorship).
narrative_ontology:cs_axiom_status(state_role_is_enforcement_not_authorship, holdable).
narrative_ontology:cs_axiom_grounding('2c619051-72cb-46d6-a9d9-711c407447a9', state_role_is_enforcement_not_authorship, conventional).
narrative_ontology:cs_reference_frame('2c619051-72cb-46d6-a9d9-711c407447a9', traditional_community_governance).
narrative_ontology:cs_drift_state('2c619051-72cb-46d6-a9d9-711c407447a9', contemporary_rights_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('2c619051-72cb-46d6-a9d9-711c407447a9', '').
narrative_ontology:cs_kernel_id(marriage_authority__communal_autonomy_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__communal_autonomy_reading, religious_community_leadership).
narrative_ontology:constraint_beneficiary(marriage_authority__communal_autonomy_reading, traditionalist_members).
narrative_ontology:constraint_victim(marriage_authority__communal_autonomy_reading, intra_community_dissenters).
narrative_ontology:constraint_victim(marriage_authority__communal_autonomy_reading, secular_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and administers marriage and family law norms based on religious tradition. Benefits from the state's enforcement of these norms, which solidifies their authority within the community and over its members. Resists external interference or legislative changes that would undermine communal autonomy.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, religious_community_leadership, agenda_setter,
    institutional, generational, constrained, regional).

% Benefit from the stability and continuity of traditional norms, which align with their values and provide a clear framework for family life. Their social standing and identity are often tied to adherence to these traditions. Exit means social ostracization or loss of community benefits.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, traditionalist_members, beneficiary,
    moderate, biographical, constrained, local).

% Bear the costs of traditional norms that may restrict their personal freedoms, particularly regarding marriage, divorce, and gender equality. They are often identity-locked, as leaving the community means losing their cultural heritage and social support network, making exit a high-cost option.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, intra_community_dissenters, payer,
    powerless, biographical, identity_locked, local).

% Advocate for universal human rights and gender equality, often challenging the discriminatory aspects of personal laws based on religious tradition. They are excluded from the internal decision-making processes of the community but exert pressure through legal challenges and public discourse.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, secular_rights_advocates, excluded,
    organized, generational, mobile, national).

% Enforces the personal laws derived from religious tradition, often through specific legal codes or by deferring to religious courts. While not authoring the norms, their enforcement lends state legitimacy and coercive power to the communal authority. They navigate tensions between constitutional rights and religious freedom.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, state_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% In this reading, the legislature largely defers to community religious traditions for family law norms, refraining from authoring a uniform civil code. They observe the operation of personal laws and respond to political pressure, but do not actively shape the core norms.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, state_legislature, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority__communal_autonomy_reading, religious_community_leadership).
narrative_ontology:fixing_cost_class(marriage_authority__communal_autonomy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable and culturally resonant framework for marriage, family, and inheritance within specific religious communities, fostering social cohesion and continuity of tradition in a pluralistic state.
% TRANSFER_FUNCTION: Transfers primary authority over personal status from the individual or secular state to the religious community and its leadership. It also transfers social capital and legitimacy to those who conform to traditional norms, while imposing social and legal costs on dissenters within the community.
% ABSENT_VOICES: Intra-community dissenters, particularly women and LGBTQ+ individuals whose rights may be curtailed by traditional interpretations, and secular legal scholars advocating for a uniform civil code. These voices are often marginalized or structurally excluded from the community's decision-making processes.
% DISAPPEARANCE_RATIONALE: If this communal authority over marriage vanished overnight, religious communities would face significant fragmentation and identity crises. Individuals would seek secular legal recognition, and the state would be compelled to either implement a uniform civil code or manage a chaotic legal vacuum, fundamentally reorganizing family law across the nation.
% FOUNDING_PROBLEM: To preserve distinct religious and cultural identities within a larger pluralistic state, providing a self-governing mechanism for personal law that predates or runs parallel to state law, thereby preventing majoritarian cultural assimilation.
% FOUNDING_PROBLEM_CORROBORATION: Religious scholars and community elders attest to the problem's ongoing live status, emphasizing the need for cultural preservation. Constitutional lawyers and human rights organizations, from outside the benefiting parties, attest to its contested status, arguing that the original problem has been superseded by evolving human rights norms and that the arrangement now serves primarily to maintain traditional power structures.
narrative_ontology:disappearance_verdict(marriage_authority__communal_autonomy_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__communal_autonomy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__communal_autonomy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(marriage_authority__communal_autonomy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__communal_autonomy_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__communal_autonomy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority__communal_autonomy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority__communal_autonomy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is driven by the costs imposed on intra-community dissenters whose personal freedoms are curtailed by traditional norms, particularly in areas like divorce, inheritance, and gender equality. Suppression (0.70) is high due to the combined force of community social pressure and state legal enforcement, which limits exit options for dissenters, especially those who are identity-locked. The theater ratio (0.15) is low, indicating that the enforcement of these norms is largely functional and not merely performative, as the state genuinely applies these laws. The metrics show a slight increase in extractiveness and suppression over time, reflecting growing external challenges and internal dissent that require more active defense of the traditional system.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of religious community leadership and traditionalist members, this arrangement is a legitimate and necessary form of self-governance and cultural preservation (a Rope-like coordination). From the perspective of intra-community dissenters and secular rights advocates, it is an extractive system that curtails individual rights under the guise of tradition (a Snare-like extraction). The engine's computation of a Tangled Rope reflects this inherent structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious community leadership and traditionalist members are beneficiaries, as the constraint solidifies their authority and preserves their way of life. Intra-community dissenters are clear targets, bearing the costs of restrictive norms. Secular rights advocates are excluded, as their proposals for universal civil codes are actively resisted by the system. The state judiciary acts as an agenda-setter by enforcing these norms, while the state legislature largely observes, refraining from authorship.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling this as a pure Rope (ignoring the extraction from dissenters) or a pure Snare (ignoring the genuine coordination function for traditionalists). The 'contested' status of the founding problem, coupled with rising extractiveness, suggests a potential drift towards a Snare if the coordination function for traditionalists becomes increasingly performative relative to the extraction from dissenters. However, the low theater ratio indicates that the core function is still active, preventing a Piton classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the `communal_autonomy_reading` of the `marriage_authority` kernel, or does it conflate elements of other sibling readings?',
    'Detailed textual analysis of community legal codes and state jurisprudence, comparing specific provisions and their justifications against the core tenets of each sibling reading.',
    'If conflated, the classification of this constraint would be unstable, requiring decomposition into more precise readings. If accurate, it solidifies the analytical boundary of this specific reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms the distinct identity of this kernel reading.').

omega_variable(
    intra_community_dissent_magnitude,
    'What is the true scale and intensity of intra-community dissent against traditional marriage norms, and how effectively are these voices suppressed?',
    'Sociological surveys, ethnographic studies, and legal aid case data from within affected communities, triangulated with reports from human rights organizations.',
    'If dissent is significantly higher and more suppressed than currently estimated, the extractiveness and suppression metrics would be higher, pushing the classification closer to a Snare. If dissent is negligible, the Rope aspect would be stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intra_community_dissent_magnitude, empirical, 'Quantifies the extent of internal opposition and its suppression.').

omega_variable(
    state_enforcement_vs_authorship_boundary,
    'Is the state''s role genuinely limited to enforcement, or does its jurisprudence and legislative inaction implicitly author or shape the communal norms?',
    'Comparative legal analysis of state court rulings and legislative debates over time, examining instances where state action (or inaction) has altered the practical application or interpretation of communal personal laws.',
    'If the state implicitly authors norms, the `authority_grounding` of the `cs_structure` might shift towards `conventional` or `extraction` (from the state), and the `secularist_reading`''s claim of state authority would gain more empirical grounding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_enforcement_vs_authorship_boundary, conceptual, 'Clarifies the boundary between state enforcement and implicit authorship of communal norms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__communal_autonomy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority__communal_autonomy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(marr_tr_t6, marriage_authority__communal_autonomy_reading, theater_ratio, 6, 0.11).
narrative_ontology:measurement(marr_tr_t12, marriage_authority__communal_autonomy_reading, theater_ratio, 12, 0.12).
narrative_ontology:measurement(marr_tr_t18, marriage_authority__communal_autonomy_reading, theater_ratio, 18, 0.13).
narrative_ontology:measurement(marr_tr_t24, marriage_authority__communal_autonomy_reading, theater_ratio, 24, 0.14).
narrative_ontology:measurement(marr_tr_t30, marriage_authority__communal_autonomy_reading, theater_ratio, 30, 0.15).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority__communal_autonomy_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(marr_be_t6, marriage_authority__communal_autonomy_reading, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(marr_be_t12, marriage_authority__communal_autonomy_reading, base_extractiveness, 12, 0.6).
narrative_ontology:measurement(marr_be_t18, marriage_authority__communal_autonomy_reading, base_extractiveness, 18, 0.62).
narrative_ontology:measurement(marr_be_t24, marriage_authority__communal_autonomy_reading, base_extractiveness, 24, 0.64).
narrative_ontology:measurement(marr_be_t30, marriage_authority__communal_autonomy_reading, base_extractiveness, 30, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority__communal_autonomy_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(marr_su_t6, marriage_authority__communal_autonomy_reading, suppression_requirement, 6, 0.63).
narrative_ontology:measurement(marr_su_t12, marriage_authority__communal_autonomy_reading, suppression_requirement, 12, 0.66).
narrative_ontology:measurement(marr_su_t18, marriage_authority__communal_autonomy_reading, suppression_requirement, 18, 0.68).
narrative_ontology:measurement(marr_su_t24, marriage_authority__communal_autonomy_reading, suppression_requirement, 24, 0.69).
narrative_ontology:measurement(marr_su_t30, marriage_authority__communal_autonomy_reading, suppression_requirement, 30, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__communal_autonomy_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, marriage_authority__secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, marriage_authority__gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, marriage_authority__federalist_millet_reading).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, marriage_authority__judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of several readings of the 'marriage_authority' kernel. Its ε value differs significantly from other readings due to its specific framing of communal autonomy and state enforcement, leading to distinct beneficiary/victim structures and classifications. All readings are linked via `affects_constraints`.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
