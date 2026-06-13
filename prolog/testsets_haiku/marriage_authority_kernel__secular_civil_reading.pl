% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__secular_civil_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__secular_civil_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: marriage_authority_kernel__secular_civil_reading
 *   human_readable: Secular Civil Marriage Authority (Special Marriage Act 1954)
 *   domain: constitutional/legal/religious
 *
 * SUMMARY:
 *   The Special Marriage Act 1954 established a secular civil alternative to
 *   religious personal law in India. It grounds its authority in the
 *   Constitution's individual-liberty clauses (Articles 25-28), treating
 *   marriage as a civil contract governed by state law rather than religious
 *   doctrine. The Act enables inter-religious marriage, provides women with
 *   divorce and property rights independent of community approval, and treats
 *   all citizens equally regardless of faith. This constraint story captures
 *   ONE reading of the contested kernel 'marriage_authority_kernel' — the
 *   secular civil reading. Four sibling readings exist: Hindu codified (Hindu
 *   Marriage Act 1955, grounded in statutory codification of Hindu law),
 *   Muslim Shariat (personal law boards interpreting Islamic law), Christian
 *   canonical (codified Christian marriage law), and Parsi communal (Parsi
 *   Marriage and Divorce Act 1936). Each reading claims legitimacy for the
 *   authority it recognizes; the secular civil reading claims legitimacy from
 *   the Constitution's individual-rights framework. The claim/metric
 *   divergence is intentional: the secular reading is CLAIMED as rope
 *   (coordination without asymmetric extraction), yet the authored metrics
 *   show moderate suppression (0.22) and moderate theater (0.18), suggesting
 *   the secular framing performs some work to maintain authority that a
 *   purely coordinating arrangement would not require.
 *
 * KEY AGENTS:
 *   - Secular civil courts: authority codifiers and enforcement agents; institutional seat defending the secular reading as binding
 *   - Inter-religious couples: primary beneficiaries; can marry across boundaries only via the secular framework
 *   - Women seeking exit from community law: powerless beneficiaries; depend on civil courts for property/custody rights unavailable in community systems
 *   - Religious community leaders: organized payers; lose adjudicatory monopoly over marriage and divorce
 *   - Muslim personal law boards: organized payers; structurally disadvantaged relative to codified Hindu and Parsi laws, which received statutory recognition
 *   - Constitutional secularism coalition: institutional beneficiary; their interpretive authority over the Constitution depends on the secular reading persisting
 *   - Excluded fundamentalist interpretations: structurally blocked from state authority; maintain parallel systems but cannot compel adherence via law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__secular_civil_reading, 0.31).
domain_priors:suppression_score(marriage_authority_kernel__secular_civil_reading, 0.22).
domain_priors:theater_ratio(marriage_authority_kernel__secular_civil_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__secular_civil_reading, rope).
narrative_ontology:human_readable(marriage_authority_kernel__secular_civil_reading, "Secular Civil Marriage Authority (Special Marriage Act 1954)").
narrative_ontology:topic_domain(marriage_authority_kernel__secular_civil_reading, "constitutional/legal/religious").

domain_priors:requires_active_enforcement(marriage_authority_kernel__secular_civil_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__secular_civil_reading, 'ce31e44d-ba6a-465f-a088-cc31e7bac1ca').
narrative_ontology:cs_kernel_codification('ce31e44d-ba6a-465f-a088-cc31e7bac1ca', formalized).
narrative_ontology:cs_authority_grounding('ce31e44d-ba6a-465f-a088-cc31e7bac1ca', extraction).
narrative_ontology:cs_interpretation_layer_present('ce31e44d-ba6a-465f-a088-cc31e7bac1ca').
narrative_ontology:cs_reading_relation('ce31e44d-ba6a-465f-a088-cc31e7bac1ca', marriage_authority_kernel__hindu_codified_reading, influences).
narrative_ontology:cs_reading_relation('ce31e44d-ba6a-465f-a088-cc31e7bac1ca', marriage_authority_kernel__muslim_shariat_reading, forecloses).
narrative_ontology:cs_reading_relation('ce31e44d-ba6a-465f-a088-cc31e7bac1ca', marriage_authority_kernel__christian_canonical_reading, influences).
narrative_ontology:cs_reading_relation('ce31e44d-ba6a-465f-a088-cc31e7bac1ca', marriage_authority_kernel__parsi_communal_reading, influences).
narrative_ontology:cs_axiom('ce31e44d-ba6a-465f-a088-cc31e7bac1ca', foundational, individual_liberty_supersedes_community_authority).
narrative_ontology:cs_axiom_status(individual_liberty_supersedes_community_authority, holdable).
narrative_ontology:cs_axiom_grounding('ce31e44d-ba6a-465f-a088-cc31e7bac1ca', individual_liberty_supersedes_community_authority, deontological).
narrative_ontology:cs_axiom('ce31e44d-ba6a-465f-a088-cc31e7bac1ca', foundational, state_neutrality_toward_religion_requires_secular_law).
narrative_ontology:cs_axiom_status(state_neutrality_toward_religion_requires_secular_law, holdable).
narrative_ontology:cs_axiom_grounding('ce31e44d-ba6a-465f-a088-cc31e7bac1ca', state_neutrality_toward_religion_requires_secular_law, conventional).
narrative_ontology:cs_reference_frame('ce31e44d-ba6a-465f-a088-cc31e7bac1ca', constitutional_individual_liberty).
narrative_ontology:cs_drift_state('ce31e44d-ba6a-465f-a088-cc31e7bac1ca', contemporary_lgbtq_inclusion_moment, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ce31e44d-ba6a-465f-a088-cc31e7bac1ca', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, inter_religious_couples).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, women_seeking_exit_from_community_law).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, lgbtq_activists).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, constitutional_secularism_doctrine).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__secular_civil_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(marriage_authority_kernel__secular_civil_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__secular_civil_reading_tests).
:- end_tests(marriage_authority_kernel__secular_civil_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-low (0.31 at interval end) because the constraint distributes benefits widely (inter-religious couples, women seeking exit, minorities protected from majority law) and does not concentrate extraction on a small victim set. The primary cost-bearer is religious community authority — a loss of power rather than a direct economic extraction. Suppression is low (0.22) because alternatives exist and are legally recognized (community-specific codified laws remain available; individuals can choose to use them voluntarily). Theater is low-moderate (0.18) because while the secular reading does performative work (defending constitutional grounds), it is not mostly theatrical — the civil courts genuinely function as an alternative authority with real adjudicatory power. Accessibility collapse is high (0.72) because individuals locked into communities with restrictive personal laws face genuine difficulty accessing alternatives (social cost of exit is prohibitive for many, even though legal exit is available). Resistance is moderate-high (0.58) because religious communities actively resist the secular reading's claims to primacy, and conservative political movements periodically push back against expansion of civil marriage rights. The measurement series shows slight drift toward higher theater (peak at t=50 around 0.20) coinciding with political mobilization around the Uniform Civil Code debate, then a retreat (t=70, back to 0.18) as the push stalled legislatively — suggesting periodic intensification of the performative dimension when the reading faces political pressure.
 *
 * PERSPECTIVAL GAP:
 *   The secular civil courts and constitutional scholars perceive this as pure coordination: a neutral framework solving the multi-faith marriage problem. Religious community leaders perceive it as extraction masked by rights language: the secular state appropriates authority that belonged to communities. Women in restrictive systems perceive it as genuine liberation. Fundamentalist religious interpreters perceive it as imposed secularization. The engine computes per-seat classification from the structural data; this story's metrics describe the constraint as it operates institutionally (moderate extraction, low suppression), not as any one seat experiences it. The perspectival gap IS the contested kernel: which authority is legitimate — the state's constitutional claim or the community's traditional claim?
 *
 * DIRECTIONALITY LOGIC:
 *   The secular civil courts are the agenda-setter (institutional power, analytical exit, they set the rules and enforce them). Inter-religious couples and women seeking exit are genuine beneficiaries (d near 0.0 for inter-religious couples, who enjoy mobile exit; d near 0.4-0.5 for women in constrained/identity-locked exit, who depend on courts but bear the social cost of using them). Religious community leaders are payers (lose authority, organized power, constrained exit — they cannot abandon the role of community defender even if individual leaders wanted to). The secular constitutional doctrine is listed as a beneficiary (not an agent) because it is vindicated by the reading's operation; it collects no rents but its legitimacy is reinforced. Muslim personal law boards sit in a peculiar position: they are payers (lose adjudicatory authority) but also carry organized power, giving them d somewhere between 0.6-0.7 — more extractive for them than for Hindu leaders, who received statutory codification. Hindu codified establishment beneficiaries sit near d=0.2-0.3 because they gained statutory recognition while the secular reading exists, a hybrid benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (fragmented personal law, no uniform legal framework) was partly a coordination failure and partly a political-sovereignty problem. The secular reading solves the coordination failure (a framework exists) but does not solve the sovereignty problem (whose law is binding — state or community?). The reading persists because the political cost of abolishing it (via Uniform Civil Code) exceeds the benefit to any single majority, and because it genuinely benefits several constituencies. Mandatrophy is not present: the founding problem is not dead (inter-religious couples still face legal barriers, women in restrictive systems still need alternatives), and the constraint is not purely performative (civil courts do real work adjudicating marriages). The constraint is best classified as rope-with-contestation: it performs genuine coordination while being contested on legitimacy grounds by religious communities. The small theater increase during the Uniform Civil Code debate (t=35-50) is normal political contestation, not evidence of atrophied function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    secular_neutrality_vs_christian_bias,
    'Is the secular civil reading truly neutral toward all religions, or does it embed Christian (monogamous, individualist) marriage concepts that privilege some religious traditions over others?',
    'Comparative analysis of how the Special Marriage Act handles marriage dissolution, inheritance, and polygamy relative to how different religions practice these; ethnographic study of how communities experience the law as neutral vs. culturally biased.',
    'If the secular reading embeds Christian-derived concepts, it is not purely coordinating but extractive of certain religious traditions; this would lower accessibility_collapse and raise resistance for communities whose marriage concepts diverge from the Act''s template. The constraint might reclassify from rope toward tangled_rope (coordination function + asymmetric extraction from communities whose practices don''t fit the template).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(secular_neutrality_vs_christian_bias, conceptual, 'Whether secular law is culturally neutral or embedds particular religious (Christian-derived) marriage concepts.').

omega_variable(
    constitutional_secularism_interpretation_contest,
    'Does the Constitution''s Articles 25-28 (freedom of religion) require state authority over personal law, or do they permit (even mandate) community self-governance in religious matters including marriage?',
    'This is unresolvable through additional facts — it is a fundamental disagreement about constitutional interpretation between secularists (state authority over all law) and pluralists (community autonomy within bounds). It may be resolved only by constitutional amendment or sustained shifts in judicial interpretation.',
    'If the pluralist reading gains traction in jurisprudence, the secular civil reading''s legitimacy claim weakens; religious communities could assert coequal authority without contradiction to the Constitution. This would raise the constraint''s suppression (more force needed to maintain state primacy) and lower its accessibility_collapse (communities could more easily defend their own law as legitimate alternative). The constraint might reclassify toward tangled_rope or even snare if state enforcement against community law intensified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(constitutional_secularism_interpretation_contest, conceptual, 'Fundamental disagreement about what the Constitution requires: state authority or community autonomy in religious matters.').

omega_variable(
    exit_cost_measurement_ambiguity,
    'What is the true social cost to individuals of exiting community law for secular civil marriage? Is it prohibitive for most, manageable for some, or performative (claimed by communities but not enforced)?',
    'Longitudinal survey of inter-religious and inter-caste couples who chose secular civil marriage: what ostracism or loss-of-belonging did they actually experience? Panel data on whether ex-community members are genuinely excluded or informally reintegrated post-marriage.',
    'If exit costs are high and real, women''s and inter-religious beneficiaries are genuinely trapped despite legal options, raising suppression and accessibility_collapse. If exit costs are moderate or selective (enforced by some communities but not others), the constraint''s suppression is overstated. If exit costs are largely performative (threatened but not enforced), the constraint is less extractive than authored. This affects whether beneficiaries like women_seeking_exit_from_community_law should be classified as mobile or constrained/identity_locked.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_cost_measurement_ambiguity, empirical, 'Actual social cost to individuals of exiting community law — genuinely prohibitive or moderately constrained?').

omega_variable(
    kernel_reading_alternative_framing,
    'Is the secular civil reading one legitimate interpretation among several, or does it claim exclusive legitimacy and foreclose the others?',
    'Examine the Special Marriage Act''s language and implementation: does it recognize community personal laws as coequal alternatives (pluralist framing), or does it position itself as superseding them in legal hierarchy (exclusivist framing)? The answer may differ between the Act''s text and courts'' practice.',
    'The answer determines the relation between this reading and its siblings. If pluralist (coequal), the relation is coexists_with. If exclusivist, the relation is forecloses (this reading claims to override others) or influences (this reading sets baseline terms but allows opt-in to alternatives). The classification of reading_relations in cs_structure depends on this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_alternative_framing, conceptual, 'Whether the secular civil reading is one alternative among several or claims to supersede/foreclose others.').

omega_variable(
    codified_hindu_law_as_secular_or_religious,
    'Is the Hindu Marriage Act 1954 a secular law (state-codified law applying neutral principles) or a religious law (codified Hindu doctrine)?',
    'This depends on how codification is framed. If Hindu law''s codification is framed as ''the state translating Hindu doctrine into legal rules,'' it remains religious. If framed as ''the state absorbing Hindu marriage practice into secular law,'' it becomes secular. The framing choice determines whether Hindu codified law and secular civil law are coequal readings (both secular by codification) or opposed readings (Hindu law is religious, secular civil is secular).',
    'If Hindu codified law is classified as secular, the constraint family has two secular readings competing for jurisdiction (secular civil explicit, Hindu codified hybrid), and the network topology changes. If Hindu codified law is classified as religious, it is a separate reading of the kernel, and network edges must reflect this. The classification affects whether the hindu_codified_establishment should be classified as a beneficiary of the secular civil reading (they gained statutory recognition) or as a payer (they lost authority over their own law when codified).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(codified_hindu_law_as_secular_or_religious, conceptual, 'Is codified Hindu law a secular or religious reading of marriage authority?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__secular_civil_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority_kernel__secular_civil_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(marr_tr_t10, marriage_authority_kernel__secular_civil_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement(marr_tr_t20, marriage_authority_kernel__secular_civil_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement(marr_tr_t35, marriage_authority_kernel__secular_civil_reading, theater_ratio, 35, 0.18).
narrative_ontology:measurement(marr_tr_t50, marriage_authority_kernel__secular_civil_reading, theater_ratio, 50, 0.2).
narrative_ontology:measurement(marr_tr_t70, marriage_authority_kernel__secular_civil_reading, theater_ratio, 70, 0.18).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(marr_be_t10, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 10, 0.29).
narrative_ontology:measurement(marr_be_t20, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 20, 0.3).
narrative_ontology:measurement(marr_be_t35, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 35, 0.32).
narrative_ontology:measurement(marr_be_t50, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 50, 0.33).
narrative_ontology:measurement(marr_be_t70, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 70, 0.31).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(marr_su_t10, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 10, 0.21).
narrative_ontology:measurement(marr_su_t20, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 20, 0.23).
narrative_ontology:measurement(marr_su_t35, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 35, 0.24).
narrative_ontology:measurement(marr_su_t50, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 50, 0.25).
narrative_ontology:measurement(marr_su_t70, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 70, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__secular_civil_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(marriage_authority_kernel__secular_civil_reading, 0.12).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__parsi_communal_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the marriage_authority_kernel. Five constraint stories instantiate the five readings (secular civil, Hindu codified, Muslim Shariat, Christian canonical, Parsi communal). Each reading has its own ε, its own beneficiary/victim structure, and its own type classification. They are linked via network.affects_constraints to indicate they compete for jurisdiction over the same life domain (marriage authority in India). Do NOT interpret this as one constraint with five observables — interpret it as five constraints, each ε-invariant, each capturing a different answer to 'who has legitimate authority over marriage?'

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority_kernel__secular_civil_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
