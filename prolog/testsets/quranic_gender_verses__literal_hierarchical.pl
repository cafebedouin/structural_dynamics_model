% ============================================================================
% CONSTRAINT STORY: quranic_gender_verses__literal_hierarchical
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quranic_gender_verses__literal_hierarchical, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: quranic_gender_verses__literal_hierarchical
 *   human_readable: Qur'anic Gender Verses (Literal Hierarchical Reading): Divine Ordinance of Male Guardianship
 *   domain: religious/legal/gender
 *
 * SUMMARY:
 *   This constraint story instantiates the literal hierarchical reading of
 *   Qur'anic verses 4:11 (differential inheritance), 2:282 (testimony
 *   weight), and 4:34 (guardianship authority). This reading interprets these
 *   verses as direct, timeless divine ordinances establishing male
 *   guardianship and differentiated legal rights as immutable law across all
 *   times and contexts. The literal hierarchical reading is one of three
 *   structurally distinct readings of the same scriptural kernel
 *   (quranic_gender_verses). This story models only this reading's structure,
 *   ε value, and stakeholder situation—NOT the competing readings. Per the
 *   ε-invariance principle, the competing readings are modeled in separate
 *   constraint stories (contextual_egalitarian and progressive_abrogation),
 *   each with its own ε, beneficiary/victim structure, and type
 *   classification. The three readings are linked via
 *   network.affects_constraints. The constraint declares
 *   emerges_naturally=true (mountain claim) while also declaring
 *   beneficiaries—a false summit candidate. This apparent paradox is the
 *   point: the literal hierarchical reading claims divine naturality, but
 *   identifiable institutional actors (male household heads, male scholars,
 *   patriarchal courts) structurally benefit from its enforcement. The FSM
 *   (False Summit Machine) will examine whether the metric profile supports
 *   the mountain claim or whether the beneficiary structure indicates
 *   constructed extraction.
 *
 * KEY AGENTS:
 *   - male_household_heads: Primary beneficiary (d ≈ 0.1–0.15, near beneficiary end). Gain household authority, double inheritance, control over female family members' legal autonomy. Structurally mobile at the global level (can relocate, claim no identity-lock) but derive institutional status from the constraint.
 *   - women_under_guardianship: Primary victims (d ≈ 0.85–0.95, near full-target end). Bear restricted autonomy, halved inheritance, identity_locked exit (apostasy/family rupture costs). Powerless individually but organized at scale; their exit options are constrained and identity-linked.
 *   - male_religious_scholars: Secondary beneficiary/agenda-setter (d ≈ 0.2–0.3). Control the authoritative interpretation of the verses; institutional position is reinforced by male-centered tradition. Arbitrage-level exit (can adopt alternative readings but face reputational cost).
 *   - patriarchal_institutional_authority: Beneficiary/agenda-setter (d ≈ 0.15–0.25). Courts, family law systems, religious establishments enforce the constraint; their authority and function depend on maintaining the literal hierarchical reading.
 *   - contextual_egalitarian_readers: Excluded (d ≈ 0.7–0.8, high target without leverage). Scholars and advocates whose interpretive work directly challenges the constraint's legitimacy but are excluded from authoritative jurisprudential channels.
 *   - progressive_abrogation_readers: Excluded (d ≈ 0.7–0.8). Mount systematic exegetical challenges but lack institutional enforcement power. Their exclusion is structural to the constraint's persistence.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quranic_gender_verses__literal_hierarchical, 0.82).
domain_priors:suppression_score(quranic_gender_verses__literal_hierarchical, 0.79).
domain_priors:theater_ratio(quranic_gender_verses__literal_hierarchical, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, extractiveness, 0.82).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, accessibility_collapse, 0.91).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quranic_gender_verses__literal_hierarchical, mountain).
narrative_ontology:human_readable(quranic_gender_verses__literal_hierarchical, "Qur'anic Gender Verses (Literal Hierarchical Reading): Divine Ordinance of Male Guardianship").
narrative_ontology:topic_domain(quranic_gender_verses__literal_hierarchical, "religious/legal/gender").

domain_priors:emerges_naturally(quranic_gender_verses__literal_hierarchical).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quranic_gender_verses__literal_hierarchical, 'ab0f66ad-b1e9-446a-8f8e-ae49ddaef834').
narrative_ontology:cs_kernel_codification('ab0f66ad-b1e9-446a-8f8e-ae49ddaef834', fixed_text).
narrative_ontology:cs_authority_grounding('ab0f66ad-b1e9-446a-8f8e-ae49ddaef834', extraction).
narrative_ontology:cs_interpretation_layer_present('ab0f66ad-b1e9-446a-8f8e-ae49ddaef834').
narrative_ontology:cs_reading_relation('ab0f66ad-b1e9-446a-8f8e-ae49ddaef834', quranic_gender_verses__contextual_egalitarian_quranic_gender, forecloses).
narrative_ontology:cs_reading_relation('ab0f66ad-b1e9-446a-8f8e-ae49ddaef834', quranic_gender_verses__progressive_abrogation_quranic_gender, forecloses).
narrative_ontology:cs_axiom('ab0f66ad-b1e9-446a-8f8e-ae49ddaef834', foundational, quranic_verses_timeless_divine_ordinance).
narrative_ontology:cs_axiom_status(quranic_verses_timeless_divine_ordinance, holdable).
narrative_ontology:cs_axiom_grounding('ab0f66ad-b1e9-446a-8f8e-ae49ddaef834', quranic_verses_timeless_divine_ordinance, theological).
narrative_ontology:cs_axiom('ab0f66ad-b1e9-446a-8f8e-ae49ddaef834', foundational, male_guardianship_structurally_necessary).
narrative_ontology:cs_axiom_status(male_guardianship_structurally_necessary, holdable).
narrative_ontology:cs_axiom_grounding('ab0f66ad-b1e9-446a-8f8e-ae49ddaef834', male_guardianship_structurally_necessary, deontological).
narrative_ontology:cs_reference_frame('ab0f66ad-b1e9-446a-8f8e-ae49ddaef834', divine_immutable_hierarchy).
narrative_ontology:cs_drift_state('ab0f66ad-b1e9-446a-8f8e-ae49ddaef834', contemporary_post_enlightenment_challenge, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ab0f66ad-b1e9-446a-8f8e-ae49ddaef834', '').
narrative_ontology:cs_kernel_id(quranic_gender_verses__literal_hierarchical, quranic_gender_verses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, male_household_heads).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, male_religious_scholars).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, patriarchal_institutional_authority).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, women_under_guardianship).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, female_witnesses).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, disinherited_daughters).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quranic_gender_verses__literal_hierarchical, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(quranic_gender_verses__literal_hierarchical, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quranic_gender_verses__literal_hierarchical_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quranic_gender_verses__literal_hierarchical, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, ExtMetricName, E),
    domain_priors:suppression_score(quranic_gender_verses__literal_hierarchical, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(quranic_gender_verses__literal_hierarchical),
    narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(quranic_gender_verses__literal_hierarchical_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is 0.82—high because the constraint transfers significant wealth (halved inheritance), authority (guardianship over women's contracts and movement), and legal personhood (halved testimony weight) to male-headed beneficiary groups. The transfer is not in payment for a service but in assertion of a structural right backed by religious authority. Suppression is 0.79—nearly as high as extractiveness because the constraint's persistence depends on actively suppressing alternative readings and exit routes. Women's identity_locked exit status is a primary suppression mechanism: leaving the constraint means apostasy (punishable by death in some jurisdictions, social death in most), family rupture, and loss of inheritance claims. Religious scholars maintain suppression by declaring the literal hierarchical reading the only sound Islamic jurisprudence and treating alternatives as heretical or culturally corrupted. Theater ratio is 0.28 (low-to-moderate): the constraint's justifications (women's protection, household stability, divine ordinance) are partially functional and partially performative. Genuine household coordination functions exist (clear inheritance rules, defined authority lines reduce intra-family conflict), but an increasing share of enforcement activity defends male privilege rather than women's welfare—courts interpreting guardianship rules to deny women's travel and work rights, inheritance rules applied to maximize male control, testimony rules used to exclude women from business law. The ratio is low because the constraint's functional component is real, not theater; but it rises as historical justifications lose empirical ground. Accessibility collapse is very high (0.91) because once women understand the constraint's operation (and that it is human-made, not inevitable), exit routes theoretically exist but are practically closed off by identity fusion (apostasy identity-death) and family/economic dependence. The constraint persists not because women cannot imagine its absence but because imagining it while remaining Muslim and connected to family requires cognitive and identity work at near-maximum cost. Resistance is 0.64 (moderate-to-high): organized women's movements within Islamic tradition, female scholars proposing alternative readings, legal reforms in Muslim-majority countries, and international human rights advocacy all mount real pressure. But resistance is constrained by institutional exclusion (alternative readings are not taught as valid in conservative jurisprudential channels) and by identity-fusion costs (reformers face accusations of cultural betrayal or religious apostasy). Measurements show extractiveness rising from 0.28 (early Islamic period) to 0.82 (contemporary), while theater ratio rises from 0.08 to 0.28. This suggests the constraint has accumulated additional extractive layers—from a mostly functional household-coordination mechanism in the early period to a sophisticated legal system defending male privilege against increasing resistance. Suppression requirement rises from 0.35 to 0.79, indicating enforcement machinery has intensified as the constraint faces challenge. This temporal pattern is characteristic of constraints shifting from organic coordination to defended extraction.
 *
 * PERSPECTIVAL GAP:
 *   The literal hierarchical reading claims universal divine status and timelessness—all seats should receive it identically. But the engine will compute radically different classifications per seat. Male household heads and scholars will compute near-rope (coordination with minor beneficiary extraction) because they experience genuine household order and institutional authority from the constraint. Women under guardianship will compute snare (pure extraction with identity suppression) because they experience loss of autonomy and wealth without offsetting coordination benefit. This perspectival divergence is the structural point: the same constraint appears as natural law to those who benefit from it and as human-made extraction to those it targets. The kernel contest (literal vs. contextual vs. progressive readings) is, at bottom, a dispute about whether this divergence is a sign of truth (all seats see the same divine ordinance, divergence is sin/corruption) or a sign of extraction (the divergence reveals who benefits and who pays).
 *
 * DIRECTIONALITY LOGIC:
 *   Male household heads derive benefit directly from the constraint (authority, wealth transfer, legal control). Their d values are low (beneficiary end, 0.1–0.15). Women under guardianship are the constraint's targets—they lose inheritance, autonomy, and legal standing. Their d values are high (target end, 0.85–0.95). The identity_locked exit option amplifies their effective extraction: they cannot leave Islam and remain connected to family and community, so the suppression is structural (backed by laws and customs) and internalized (identity-fused). Male religious scholars benefit from institutional authority and reinforced tradition but face reputational cost if they adopt alternative readings; d ≈ 0.2–0.3 (beneficiary-leaning but with some target pressure). Patriarchal institutions (courts, family law systems) benefit from enforcement authority and revenue streams (if guardianship authority comes with fees or status). No directionality overrides are required—the structural derivation (beneficiary/victim + exit options) produces appropriate d values for each seat.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits a potential mandatrophy: the founding problem (female vulnerability to property dispossession in 7th-century Arabia) is substantially solved by modern legal systems (women can own property, access courts, inherit in their own name, work without permission in most jurisdictions). Yet the constraint persists institutionally, enforced through family law, inheritance statutes, and religious authority. The six_questions.founding_problem_status is authored as 'contested' because the literal hierarchical reading claims the founding problem is live (women remain vulnerable without male guardianship), while contextual and progressive readings attest the problem is dead and the constraint is now pure extraction. The measurement series show extractiveness rising as the founding problem recedes historically—the constraint accumulates layers unrelated to protection (male privilege, institutional authority, resistance suppression) while the original problem it addressed has been solved by legal modernization outside the constraint. The theater ratio rising from 0.08 to 0.28 suggests increasing performative maintenance: justifications for the constraint are recycled even as their empirical ground weakens. A mandatrophy narrative would read: 'This constraint was built to solve female economic vulnerability. It did. Modern legal systems have now solved that problem independently. The constraint persists because institutional actors benefit from it, not because the founding problem remains live. Its persistence is now extractive rather than protective.' The engine will test this narrative against the claim (mountain vs. actual operation) and the six_questions mismatch (founding_problem_status=dead vs. disappearance_verdict=world_rearranges).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_hierarchy,
    'Is this constraint a genuine natural law (divine ordinance, immutable across time and context) or a constructed legal hierarchy that benefits identifiable male-centered institutional actors and could be otherwise?',
    'The constraint declares emerges_naturally=true (mountain claim) but declares beneficiaries (male_household_heads, male_religious_scholars, patriarchal_institutional_authority). False Summit Machine will evaluate: does the metric profile support the mountain claim, or does the beneficiary structure indicate constructed extraction riding on a naturality narrative?',
    'If the constraint reclassifies as tangled_rope or snare, the literal hierarchical reading loses its claim to timeless, unquestionable status and becomes subject to remediation via policy change rather than theological acceptance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_hierarchy, conceptual, 'Whether the constraint is a divine natural law or a human legal construction with identifiable beneficiaries.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.79) structural (external enforcement via family law, inheritance statutes, witness rules, exit barriers like apostasy law) or internalized (women have fused their identity with the constraint, believe they deserve restricted autonomy, self-enforce guardianship)?',
    'Post-exit suppression trajectory: study women who have left the constraint (apostasy, migration to non-Islamic-law jurisdictions, family rupture) and measure whether suppression persists after the external enforcement mechanism is removed. If suppression drops sharply post-exit, it was primarily structural; if it persists, it is internalized.',
    'If suppression is substantially internalized, the constraint''s effective suppression is higher than the structural measure suggests—women carry it with them after exit. If primarily structural, policy remedies (law reform, enforcement decay) can reduce it directly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression is structural enforcement or internalized identity fusion.').

omega_variable(
    hermeneutic_closure_vs_revisability,
    'Does the literal hierarchical reading claim to foreclose all other readings of these verses as hermeneutically invalid, or does it coexist with contextual and progressive readings as live interpretive options within Islamic tradition?',
    'Institutional mapping: which Islamic jurisprudential schools, regional authorities, and scholars formally recognize contextual_egalitarian and progressive_abrogation as valid Islamic scholarship, versus which treat the literal hierarchical reading as the only sound interpretation?',
    'If the reading forecloses others institutionally (no alternative is taught as valid Islamic law), the kernel structure is foreclosing rather than coexisting. If alternatives are recognized as legitimate Islamic scholarship even if not preferred policy, the kernel exhibits genuine coexistence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hermeneutic_closure_vs_revisability, empirical, 'Whether the literal hierarchical reading monopolizes institutional authority or coexists with alternatives.').

omega_variable(
    contextual_necessity_vs_timelessness,
    'Were these verses drafted to address specific 7th-century Arabian conditions (female vulnerability without property law, tribal warfare, historical gender norms) or to establish timeless, context-independent divine ordinance for all societies?',
    'Comparative historical analysis: study the verses'' relationship to pre-Islamic Arabian practices, early Islamic social conditions, and the later development of Islamic jurisprudence. Assess whether Muslim scholars across schools treat the specific provisions (inheritance ratios, testimony weight, guardianship authority) as universal laws or as applications of principles to particular contexts.',
    'If contextual necessity is established, the constraint''s applicability to modern societies becomes revisable—the same equity principle could be instantiated differently. If timelessness is established, reform requires overriding divine ordinance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contextual_necessity_vs_timelessness, empirical, 'Whether the constraint is context-specific application or timeless divine ordinance.').

omega_variable(
    resistance_trajectory_future_pressure,
    'Will resistance to the literal hierarchical reading increase as women''s access to education, economic independence, and international human rights frameworks grows?',
    'Longitudinal tracking of resistance indicators: organized women''s movements within Islamic tradition (female scholars, reform initiatives), legal reforms in Muslim-majority countries away from patriarchal guardianship, and shifts in youth support for the reading across generations.',
    'If resistance rises sharply, the constraint may face institutional challenge requiring increased suppression (enforcement ratchet). If resistance plateaus or is co-opted, institutional stability may be sustained despite external pressure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(resistance_trajectory_future_pressure, preference, 'Future trajectory of organized resistance to the literal hierarchical reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quranic_gender_verses__literal_hierarchical, 0, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quranic_gender_verses__literal_hierarchical, theater_ratio, 0, 0.08).
narrative_ontology:measurement(qura_tr_t200, quranic_gender_verses__literal_hierarchical, theater_ratio, 200, 0.12).
narrative_ontology:measurement(qura_tr_t400, quranic_gender_verses__literal_hierarchical, theater_ratio, 400, 0.15).
narrative_ontology:measurement(qura_tr_t700, quranic_gender_verses__literal_hierarchical, theater_ratio, 700, 0.22).
narrative_ontology:measurement(qura_tr_t1000, quranic_gender_verses__literal_hierarchical, theater_ratio, 1000, 0.26).
narrative_ontology:measurement(qura_tr_t1200, quranic_gender_verses__literal_hierarchical, theater_ratio, 1200, 0.27).
narrative_ontology:measurement(qura_tr_t1400, quranic_gender_verses__literal_hierarchical, theater_ratio, 1400, 0.28).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quranic_gender_verses__literal_hierarchical, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(qura_be_t200, quranic_gender_verses__literal_hierarchical, base_extractiveness, 200, 0.45).
narrative_ontology:measurement(qura_be_t400, quranic_gender_verses__literal_hierarchical, base_extractiveness, 400, 0.58).
narrative_ontology:measurement(qura_be_t700, quranic_gender_verses__literal_hierarchical, base_extractiveness, 700, 0.72).
narrative_ontology:measurement(qura_be_t1000, quranic_gender_verses__literal_hierarchical, base_extractiveness, 1000, 0.78).
narrative_ontology:measurement(qura_be_t1200, quranic_gender_verses__literal_hierarchical, base_extractiveness, 1200, 0.8).
narrative_ontology:measurement(qura_be_t1400, quranic_gender_verses__literal_hierarchical, base_extractiveness, 1400, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quranic_gender_verses__literal_hierarchical, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(qura_su_t200, quranic_gender_verses__literal_hierarchical, suppression_requirement, 200, 0.52).
narrative_ontology:measurement(qura_su_t400, quranic_gender_verses__literal_hierarchical, suppression_requirement, 400, 0.61).
narrative_ontology:measurement(qura_su_t700, quranic_gender_verses__literal_hierarchical, suppression_requirement, 700, 0.72).
narrative_ontology:measurement(qura_su_t1000, quranic_gender_verses__literal_hierarchical, suppression_requirement, 1000, 0.76).
narrative_ontology:measurement(qura_su_t1200, quranic_gender_verses__literal_hierarchical, suppression_requirement, 1200, 0.78).
narrative_ontology:measurement(qura_su_t1400, quranic_gender_verses__literal_hierarchical, suppression_requirement, 1400, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quranic_gender_verses__literal_hierarchical, identity_coordination).
narrative_ontology:boltzmann_floor_override(quranic_gender_verses__literal_hierarchical, 0.12).
narrative_ontology:affects_constraint(quranic_gender_verses__literal_hierarchical, contextual_egalitarian_quranic_gender).
narrative_ontology:affects_constraint(quranic_gender_verses__literal_hierarchical, progressive_abrogation_quranic_gender).

% DUAL FORMULATION NOTE:
% The quranic_gender_verses kernel has three structurally distinct constraint stories. The literal_hierarchical reading (this story) instantiates verses as timeless divine ordinance with high extractiveness (0.82) and clear beneficiary/victim structure. The contextual_egalitarian reading reinterprets the same verses as historical progress requiring adaptation to modern equity principles (lower ε ≈ 0.35–0.45). The progressive_abrogation reading treats the verses as partially superseded by later Qur'anic egalitarian principles (ε ≈ 0.15–0.25). The three readings coexist as live interpretive positions within Islamic tradition but are in genuine conflict about which reading correctly understands the kernel. The literal_hierarchical reading forecloses the other two within its own authority framework (declares them heretical or culturally corrupted), while the contextual and progressive readings contest the literal reading's claim to timelessness. Each story is independently ε-invariant and carries its own beneficiary/victim declarations. The network edges represent the kernel contest structure: upstream literal reading influences (and in its own frame, rules out) the downstream readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
