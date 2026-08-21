% ============================================================================
% CONSTRAINT STORY: marriage_authority__secularist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__secularist_reading, []).

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
 *   constraint_id: marriage_authority__secularist_reading
 *   human_readable: Secularist Reading: State Legislative Authority over Marriage, Uniform Civil Code
 *   domain: legal/constitutional/social
 *
 * SUMMARY:
 *   This constraint represents the 'secularist reading' of marriage
 *   authority, asserting that the democratic legislature is the sole
 *   legitimate source of family law, and that personal law pluralism is a
 *   temporary anomaly to be eliminated by a Uniform Civil Code (UCC). This
 *   reading frames the pursuit of a UCC as a necessary step towards national
 *   integration, equality, and modernity. It is a high-extraction tangled
 *   rope, as it coordinates the state's legal authority while extracting
 *   autonomy from minority religious communities.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__secularist_reading, 0.8).
domain_priors:suppression_score(marriage_authority__secularist_reading, 0.85).
domain_priors:theater_ratio(marriage_authority__secularist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__secularist_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority__secularist_reading, "Secularist Reading: State Legislative Authority over Marriage, Uniform Civil Code").
narrative_ontology:topic_domain(marriage_authority__secularist_reading, "legal/constitutional/social").

domain_priors:requires_active_enforcement(marriage_authority__secularist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__secularist_reading, '78a74636-43dc-4082-a9ef-cc41053291f9').
narrative_ontology:cs_kernel_codification('78a74636-43dc-4082-a9ef-cc41053291f9', formalized).
narrative_ontology:cs_authority_grounding('78a74636-43dc-4082-a9ef-cc41053291f9', lineage).
narrative_ontology:cs_interpretation_layer_present('78a74636-43dc-4082-a9ef-cc41053291f9').
narrative_ontology:cs_reading_relation('78a74636-43dc-4082-a9ef-cc41053291f9', marriage_authority__communal_autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('78a74636-43dc-4082-a9ef-cc41053291f9', marriage_authority__federalist_millet_reading, forecloses).
narrative_ontology:cs_reading_relation('78a74636-43dc-4082-a9ef-cc41053291f9', marriage_authority__gender_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('78a74636-43dc-4082-a9ef-cc41053291f9', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_axiom('78a74636-43dc-4082-a9ef-cc41053291f9', foundational, state_sovereignty_over_personal_law).
narrative_ontology:cs_axiom_status(state_sovereignty_over_personal_law, holdable).
narrative_ontology:cs_axiom_grounding('78a74636-43dc-4082-a9ef-cc41053291f9', state_sovereignty_over_personal_law, conventional).
narrative_ontology:cs_axiom('78a74636-43dc-4082-a9ef-cc41053291f9', foundational, legal_uniformity_as_equality_and_modernity).
narrative_ontology:cs_axiom_status(legal_uniformity_as_equality_and_modernity, holdable).
narrative_ontology:cs_axiom_grounding('78a74636-43dc-4082-a9ef-cc41053291f9', legal_uniformity_as_equality_and_modernity, instrumental).
narrative_ontology:cs_reference_frame('78a74636-43dc-4082-a9ef-cc41053291f9', secular_uniform_legal_order).
narrative_ontology:cs_drift_state('78a74636-43dc-4082-a9ef-cc41053291f9', contemporary_legal_pluralism, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('78a74636-43dc-4082-a9ef-cc41053291f9', '').
narrative_ontology:cs_kernel_id(marriage_authority__secularist_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__secularist_reading, secular_modernist_coalition).
narrative_ontology:constraint_beneficiary(marriage_authority__secularist_reading, gender_equality_advocates).
narrative_ontology:constraint_victim(marriage_authority__secularist_reading, minority_religious_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively advocates for the implementation of a Uniform Civil Code (UCC) to replace diverse personal laws, viewing it as essential for national unity, equality, and modernity. They benefit from the ideological and political victory of a secular, uniform legal system.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, secular_modernist_coalition, agenda_setter,
    institutional, generational, arbitrage, national).

% Bear the cost of losing autonomy over their personal laws, which are deeply intertwined with their religious and cultural identity. They face the prospect of state-imposed uniformity overriding their traditional practices and legal systems, with limited avenues for resistance or exit.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, minority_religious_communities, payer,
    powerless, generational, trapped, local).

% The primary body empowered to enact a Uniform Civil Code. It is influenced by political pressures from various coalitions and is responsible for drafting and passing legislation that would eliminate personal law pluralism. Its legitimacy is tied to representing the will of the people.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, democratic_legislature, agenda_setter,
    institutional, biographical, constrained, national).

% Interprets the constitutional validity of existing personal laws and any proposed UCC. While not directly authoring the law, its rulings can shape the legislative agenda and the eventual form of a uniform code. It observes the contest from a position of legal interpretation.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, judiciary, observer,
    institutional, generational, analytical, national).

% Often align with the secularist push for a UCC, as many personal laws are perceived to contain gender-discriminatory provisions. They benefit from the potential for greater gender equality under a uniform, secular law, even if their primary goal is equality rather than uniformity for its own sake.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, gender_equality_advocates, beneficiary,
    organized, biographical, mobile, national).

% Argue for the inherent value of legal pluralism as a mechanism to protect minority rights and prevent majoritarian tyranny. They are structurally excluded from the secularist discourse, which frames pluralism as an anomaly to be eliminated, rather than a legitimate constitutional arrangement.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, federalist_advocates, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority__secularist_reading, secular_modernist_coalition).
narrative_ontology:fixing_cost_class(marriage_authority__secularist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To unify diverse personal laws (e.g., marriage, divorce, inheritance) under a single, secular legal framework, thereby standardizing legal rights and obligations for all citizens regardless of religious affiliation.
% TRANSFER_FUNCTION: Transfers ultimate authority over family and personal law from religious communities and their traditional legal systems to the democratic legislature of the state. It also transfers the burden of adapting to a new, uniform system onto minority religious communities.
% ABSENT_VOICES: Federalist advocates and those who view legal pluralism as a fundamental aspect of a diverse society are largely absent from the dominant secularist narrative. They would argue that the elimination of personal laws constitutes a majoritarian imposition and a threat to minority cultural and religious autonomy.
% DISAPPEARANCE_RATIONALE: If the drive for a Uniform Civil Code and the underlying secularist ideology vanished overnight, legal pluralism would likely persist and potentially strengthen. Minority religious communities would retain greater autonomy over their personal laws, and the state's role in family law would remain fragmented, leading to a significant reorganization of legal and social structures.
% FOUNDING_PROBLEM: The perceived fragmentation, inequality, and potential for discrimination arising from diverse religious personal laws, which are seen as hindering national integration and the establishment of a truly secular, egalitarian society.
% FOUNDING_PROBLEM_CORROBORATION: Secular legal scholars, some women's rights organizations, and state institutions (e.g., Law Commissions) corroborate that the problems of fragmentation and inequality persist. However, minority religious leaders and federalist advocates contest this, arguing that the problem is overstated or that the proposed solution (UCC) creates new forms of injustice.
narrative_ontology:disappearance_verdict(marriage_authority__secularist_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__secularist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__secularist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(marriage_authority__secularist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__secularist_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__secularist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority__secularist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority__secularist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.8) because the constraint seeks to transfer significant legal and cultural autonomy from religious communities to the state. Suppression is also high (0.85) as the persistence of this constraint relies on active legislative and political efforts to override existing personal laws and suppress resistance from affected communities. The theater ratio is low (0.1) because the secularist agenda is genuinely committed to achieving a UCC, not merely performing the effort. Accessibility collapse is high (0.8) as the goal is to eliminate all alternative legal frameworks for personal law. Resistance is high (0.75) due to strong opposition from minority religious groups.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the secularist-modernist coalition, this constraint is a necessary and beneficial coordination mechanism for a modern state. From the perspective of minority religious communities, it is a coercive imposition that extracts their cultural and legal autonomy. The democratic legislature, while the agenda-setter, experiences the constraint as a complex political challenge, balancing competing demands.
 *
 * DIRECTIONALITY LOGIC:
 *   The secular_modernist_coalition and gender_equality_advocates are beneficiaries, as they gain from the ideological and practical outcomes of a UCC. Minority_religious_communities are clear victims, bearing the direct costs of losing their traditional legal frameworks. The democratic_legislature is an agenda-setter, responsible for enacting the UCC. The judiciary observes and interprets, while federalist_advocates are excluded, their arguments for pluralism being directly contradicted by this reading's core tenets.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pluralism_as_anomaly_vs_feature,
    'Is personal law pluralism a transitional anomaly awaiting elimination, or an inherent and legitimate feature of a diverse, federal society?',
    'Long-term observation of societal integration and minority rights outcomes in jurisdictions that maintain legal pluralism versus those that adopt uniform codes. Analysis of constitutional intent regarding diversity.',
    'If pluralism is a legitimate feature, the secularist reading''s high extractiveness would be reclassified as unjust, and its claimed coordination function would be seen as a cover for cultural assimilation. If it is a transitional anomaly, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pluralism_as_anomaly_vs_feature, conceptual, 'Ambiguity regarding the fundamental nature of legal pluralism in a diverse state.').

omega_variable(
    equality_via_uniformity_vs_reform,
    'Does a Uniform Civil Code genuinely achieve substantive gender equality, or could internal reforms within personal laws achieve similar or better outcomes without state-imposed uniformity?',
    'Comparative empirical studies of gender equality metrics (e.g., property rights, divorce settlements) in communities that have reformed personal laws versus those under a UCC. Analysis of the specific provisions of proposed UCCs.',
    'If internal reforms prove effective, the instrumental justification for the UCC''s extractiveness would weaken, potentially reclassifying it as a snare that imposes uniformity for ideological reasons rather than purely for equality. If UCC is demonstrably superior for equality, its coordination function is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equality_via_uniformity_vs_reform, empirical, 'Whether uniformity is a necessary condition for gender equality in personal law.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__secularist_reading, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1950, marriage_authority__secularist_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(marr_tr_t1970, marriage_authority__secularist_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(marr_tr_t1990, marriage_authority__secularist_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(marr_tr_t2005, marriage_authority__secularist_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(marr_tr_t2015, marriage_authority__secularist_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(marr_tr_t2025, marriage_authority__secularist_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(marr_be_t1950, marriage_authority__secularist_reading, base_extractiveness, 1950, 0.6).
narrative_ontology:measurement(marr_be_t1970, marriage_authority__secularist_reading, base_extractiveness, 1970, 0.65).
narrative_ontology:measurement(marr_be_t1990, marriage_authority__secularist_reading, base_extractiveness, 1990, 0.7).
narrative_ontology:measurement(marr_be_t2005, marriage_authority__secularist_reading, base_extractiveness, 2005, 0.75).
narrative_ontology:measurement(marr_be_t2015, marriage_authority__secularist_reading, base_extractiveness, 2015, 0.78).
narrative_ontology:measurement(marr_be_t2025, marriage_authority__secularist_reading, base_extractiveness, 2025, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1950, marriage_authority__secularist_reading, suppression_requirement, 1950, 0.65).
narrative_ontology:measurement(marr_su_t1970, marriage_authority__secularist_reading, suppression_requirement, 1970, 0.7).
narrative_ontology:measurement(marr_su_t1990, marriage_authority__secularist_reading, suppression_requirement, 1990, 0.75).
narrative_ontology:measurement(marr_su_t2005, marriage_authority__secularist_reading, suppression_requirement, 2005, 0.8).
narrative_ontology:measurement(marr_su_t2015, marriage_authority__secularist_reading, suppression_requirement, 2015, 0.83).
narrative_ontology:measurement(marr_su_t2025, marriage_authority__secularist_reading, suppression_requirement, 2025, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__secularist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__federalist_millet_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of several readings of the 'marriage_authority' kernel, each representing a distinct structural claim about the source and nature of family law. This secularist reading directly challenges communal and federalist framings, while influencing judicial and gender-rights-focused approaches.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
