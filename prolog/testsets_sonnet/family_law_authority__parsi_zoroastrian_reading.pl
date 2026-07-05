% ============================================================================
% CONSTRAINT STORY: family_law_authority__parsi_zoroastrian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__parsi_zoroastrian_reading, []).

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
 *   constraint_id: family_law_authority__parsi_zoroastrian_reading
 *   human_readable: Parsi Zoroastrian Endogamous Marriage and Priestly Authority over Ritual Validity
 *   domain: comparative_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This story instantiates the Parsi Zoroastrian reading of the
 *   family_law_authority kernel: marriage as an institution whose primary
 *   function is preserving a small, demographically endangered
 *   religious-ethnic community, governed by priestly authority over ritual
 *   validity and enforced through gendered endogamy rules. The coordination
 *   function (continuity of a shrinking community's practice and lineage) is
 *   real, but it now rides on asymmetric extraction — disproportionately from
 *   women who intermarry and from children who never chose their parents'
 *   marriage. This is a distinct constraint from the other four readings of
 *   the same kernel (Hindu dharmashastra, Muslim shariat, Christian
 *   canonical, secular contractual); each has its own ε, its own
 *   beneficiary/victim structure, and its own claimed type, linked only
 *   through the shared kernel_id, not through this file's classification.
 *
 * KEY AGENTS:
 *   - parsi_priesthood: agenda_setter (institutional/arbitrage) — administers ritual validity and membership doctrine
 *   - parsi_panchayat_trusts: beneficiary (institutional/arbitrage) — controls communal property eligibility, gains from a narrow beneficiary pool
 *   - intermarrying_parsi_women: payer (moderate/constrained) — bears asymmetric loss of rites and recognition
 *   - children_of_intermarriage: payer (powerless/trapped) — inherits exclusion with no voice
 *   - indian_secular_courts: observer (institutional/analytical) — adjudicates the doctrine's constitutional limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__parsi_zoroastrian_reading, 0.58).
domain_priors:suppression_score(family_law_authority__parsi_zoroastrian_reading, 0.62).
domain_priors:theater_ratio(family_law_authority__parsi_zoroastrian_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__parsi_zoroastrian_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__parsi_zoroastrian_reading, "Parsi Zoroastrian Endogamous Marriage and Priestly Authority over Ritual Validity").
narrative_ontology:topic_domain(family_law_authority__parsi_zoroastrian_reading, "comparative_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(family_law_authority__parsi_zoroastrian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__parsi_zoroastrian_reading, '854394f6-98cd-4aa1-9706-0b8f6aa2b870').
narrative_ontology:cs_kernel_codification('854394f6-98cd-4aa1-9706-0b8f6aa2b870', formalized).
narrative_ontology:cs_authority_grounding('854394f6-98cd-4aa1-9706-0b8f6aa2b870', lineage).
narrative_ontology:cs_interpretation_layer_present('854394f6-98cd-4aa1-9706-0b8f6aa2b870').
narrative_ontology:cs_reading_relation('854394f6-98cd-4aa1-9706-0b8f6aa2b870', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('854394f6-98cd-4aa1-9706-0b8f6aa2b870', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('854394f6-98cd-4aa1-9706-0b8f6aa2b870', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('854394f6-98cd-4aa1-9706-0b8f6aa2b870', family_law_authority__secular_contractual_reading, influences).
narrative_ontology:cs_axiom('854394f6-98cd-4aa1-9706-0b8f6aa2b870', foundational, marriage_purpose_is_community_survival).
narrative_ontology:cs_axiom_status(marriage_purpose_is_community_survival, holdable).
narrative_ontology:cs_axiom_grounding('854394f6-98cd-4aa1-9706-0b8f6aa2b870', marriage_purpose_is_community_survival, empirically_contingent).
narrative_ontology:cs_axiom('854394f6-98cd-4aa1-9706-0b8f6aa2b870', foundational, patrilineal_descent_determines_religious_membership).
narrative_ontology:cs_axiom_status(patrilineal_descent_determines_religious_membership, holdable).
narrative_ontology:cs_axiom_grounding('854394f6-98cd-4aa1-9706-0b8f6aa2b870', patrilineal_descent_determines_religious_membership, conventional).
narrative_ontology:cs_reference_frame('854394f6-98cd-4aa1-9706-0b8f6aa2b870', ancient_persian_endogamous_continuity).
narrative_ontology:cs_drift_state('854394f6-98cd-4aa1-9706-0b8f6aa2b870', post_1991_supreme_court_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('854394f6-98cd-4aa1-9706-0b8f6aa2b870', '').
narrative_ontology:cs_kernel_id(family_law_authority__parsi_zoroastrian_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, parsi_priesthood).
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, community_endogamy_advocates).
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, parsi_panchayat_trusts).
narrative_ontology:constraint_victim(family_law_authority__parsi_zoroastrian_reading, intermarrying_parsi_women).
narrative_ontology:constraint_victim(family_law_authority__parsi_zoroastrian_reading, children_of_intermarriage).
narrative_ontology:constraint_victim(family_law_authority__parsi_zoroastrian_reading, parsi_men_marrying_non_parsis_to_lesser_degree).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, parsi_men_marrying_non_parsis_to_lesser_degree).
narrative_ontology:constraint_vindicates(family_law_authority__parsi_zoroastrian_reading, community_survival_through_bloodline_purity).
narrative_ontology:constraint_vindicates(family_law_authority__parsi_zoroastrian_reading, navjote_and_fire_temple_access_requires_parsi_descent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Anjuman-appointed high priests (dasturs) and mobeds adjudicate ritual validity of marriage, navjote (initiation), and fire temple access. They administer the doctrine that community membership passes patrilineally and that intermarriage forfeits religious rights for the Parsi spouse and any children. They set the terms of who may be blessed, buried in the Tower of Silence, and counted in community rolls, and they benefit from continued authority over an ever-shrinking population that depends on them for life-cycle rites.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, parsi_priesthood, agenda_setter,
    institutional, generational, arbitrage, national).

% Bodies like the Bombay Parsi Punchayet administer housing trusts, charitable funds, and communal property reserved for those recognized as Parsi. They benefit from a narrow, policed definition of membership because it preserves the pool of beneficiaries eligible for trust housing and funds, and they litigate to keep intermarried women and their children outside that pool.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, parsi_panchayat_trusts, beneficiary,
    institutional, generational, arbitrage, national).

% Lay organizations and vocal community members who campaign for strict endogamy citing a demographically shrinking population (under 60,000 in India) and fear of cultural dissolution. They benefit symbolically and socially from the enforcement of boundaries that mark who belongs, even though many bear no direct institutional stake.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, community_endogamy_advocates, beneficiary,
    organized, generational, constrained, national).

% Parsi women who marry outside the community lose their own right to enter fire temples and to have their children recognized as Parsi, under a rule that historically did not apply symmetrically to Parsi men marrying outside. They can leave the religious community's institutions behind, but doing so means losing access to communal housing eligibility, burial rites, and recognition for their children — a cost concentrated on women specifically.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, intermarrying_parsi_women, payer,
    moderate, biographical, constrained, national).

% Children born to a Parsi mother and non-Parsi father are typically denied navjote initiation and formal community membership, regardless of their own wishes or upbringing within Parsi custom. They inherit a status determination made before their birth and have no voice in the doctrine that excludes them; courts have inconsistently allowed exceptions.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, children_of_intermarriage, payer,
    powerless, biographical, trapped, national).

% Parsi men who marry outside the community historically retain more of their own religious standing than women in the same position, and their children are more often accepted as Parsi by custom (though contested). They bear some social disapproval but far less institutional exclusion, illustrating the asymmetric gendered operation of the same rule.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, parsi_men_marrying_non_parsis_to_lesser_degree, payer,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(family_law_authority__parsi_zoroastrian_reading, parsi_men_marrying_non_parsis_to_lesser_degree, beneficiary).

% Adjudicate disputes such as Goolrokh Gupta v. Burjor Pardiwala over whether intermarried women retain fire temple access and community rights, weighing religious community autonomy against constitutional equality guarantees. Their rulings can force revision of the doctrine without the community's own consent.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, indian_secular_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__parsi_zoroastrian_reading, parsi_panchayat_trusts).
narrative_ontology:fixing_cost_class(family_law_authority__parsi_zoroastrian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, priest-administered standard for who counts as Parsi, which rites are valid, and which children may be initiated — solving a real coordination problem for an unusually small, demographically shrinking religious community trying to maintain continuity of ritual practice, language, and communal institutions across generations.
% TRANSFER_FUNCTION: Moves access to religious rites, communal trust housing, charitable funds, and burial rights away from those who intermarry (disproportionately women and their children) and concentrates continued institutional authority and beneficiary-pool control in the priesthood and panchayat trusts.
% ABSENT_VOICES: Children of intermarriage have no standing in the doctrine that excludes them before they are born. Intermarried women's own children, raised within Parsi custom, are not consulted institutionally; their exclusion is decided entirely by seats they cannot enter until an external court forces the question.
% DISAPPEARANCE_RATIONALE: If priestly authority over ritual validity and the endogamy rule disappeared overnight, panchayat trusts would need new membership criteria for housing and funds, intermarried families currently excluded would gain access to fire temples and burial rites, and the priesthood's gatekeeping function over a shrinking population would lose its exclusive basis — the community's institutional architecture is built around this boundary.
% FOUNDING_PROBLEM: A historically small refugee community (Zoroastrians fleeing persecution in Persia, settling in Gujarat and later Bombay) sought to preserve its distinct religious practice, language, and lineage against absorption into a much larger surrounding Hindu and later British colonial society, using endogamy and priestly ritual gatekeeping as continuity mechanisms.
% FOUNDING_PROBLEM_CORROBORATION: Demographers and Indian courts (in Goolrokh Gupta and related litigation) attest that the population decline the doctrine was meant to arrest has continued or worsened under the doctrine itself, suggesting the mechanism no longer serves its stated founding function; the priesthood and panchayat trusts, who administer and benefit from the doctrine, attest the founding problem remains live and requires continued strict enforcement. No demographic study originating outside the community's own advocacy organizations has found that endogamy enforcement has reversed the population decline.
narrative_ontology:disappearance_verdict(family_law_authority__parsi_zoroastrian_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__parsi_zoroastrian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__parsi_zoroastrian_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(family_law_authority__parsi_zoroastrian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__parsi_zoroastrian_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__parsi_zoroastrian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_law_authority__parsi_zoroastrian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(family_law_authority__parsi_zoroastrian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58) is moderate-high: the doctrine transfers real institutional goods (housing eligibility, burial rites, ritual recognition) away from intermarried women and their children toward continued priestly and trust authority, but a genuine coordination problem (small-community continuity) partially justifies the structure, keeping ε below the levels typical of pure extraction. Suppression (0.62) reflects that exit from the religious community's institutions is possible but costly and gendered — a woman marrying outside loses standing that a man in the same position does not, which is an enforced asymmetry rather than a symmetric coordination cost. Theater ratio is modest (0.28) and rising slowly: most enforcement activity (navjote administration, fire temple gatekeeping) still performs a real function for those who remain inside the boundary, though an increasing share is defensive boundary-maintenance rather than ritual substance, as demographic decline makes the boundary itself the primary preoccupation.
 *
 * PERSPECTIVAL GAP:
 *   From the priesthood's seat, the doctrine is coordination: a shrinking community needs firm boundaries to survive at all. From an intermarried woman's seat, the same doctrine is enforced extraction: she loses standing her brother would not lose for the identical marital choice. The engine should compute these as structurally different experiences of one constraint, not reconcile them into a single averaged verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   The priesthood and panchayat trusts sit near the beneficiary end: they administer the rule, control the beneficiary pool, and have durable institutional exit (arbitrage) even as community population shrinks. Intermarrying women and children of intermarriage sit near the target end: they bear concentrated, gendered costs and have constrained or trapped exit, since leaving the religious community's institutions forfeits recognition without the option of forcing revision from inside. Parsi men marrying outside receive a directionality override consideration but are declared as a genuinely intermediate seat (secondary beneficiary role) because doctrine and custom historically protect their own standing and often their children's status more than for women — this is a real structural asymmetry, not a modeling convenience.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing absorption of a small persecuted community into a much larger surrounding population — was genuine at the community's founding and remains partially live given continued demographic decline. But the specific mechanism (patrilineal/gendered endogamy enforcement) has not reversed the decline it was meant to arrest; if anything, demographic data cited even by community-internal sources shows continued shrinkage under decades of strict enforcement. This is the founding_problem_status: contested pattern — classifying this as pure Rope would ignore the asymmetric, gendered cost structure and the courts' repeated need to intervene; classifying it as pure Snare would ignore that many community members experience real coordination value in shared ritual continuity. Tangled Rope holds both facts without collapsing them.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    endogamy_necessity_vs_pretext,
    'Is strict gendered endogamy enforcement structurally necessary to prevent community dissolution, or has it become a pretext maintaining priestly and trust authority over a declining population regardless of demographic effect?',
    'Comparative demographic study of Zoroastrian communities with varying intermarriage-recognition policies (e.g., Iranian Zoroastrians, some diaspora communities with more permissive recognition rules) against population trend data, controlling for other factors like out-migration and fertility decline.',
    'If comparative data shows recognition policy has no measurable effect on population decline, the coordination justification weakens substantially and the structure reads closer to pure extraction (snare) than tangled rope; if it shows a measurable protective effect, the tangled_rope classification''s coordination half is better supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(endogamy_necessity_vs_pretext, empirical, 'Whether endogamy enforcement causally protects community survival or merely preserves institutional gatekeeping power.').

omega_variable(
    gendered_asymmetry_doctrinal_status,
    'Is the asymmetric treatment of intermarrying men versus women a core doctrinal requirement of Zoroastrian religious law, or a patriarchal custom later read into the tradition and now defended as though doctrinally required?',
    'Textual and historical analysis of Avestan and Pahlavi sources versus documented evolution of panchayat administrative practice in colonial and post-colonial India; comparison with more gender-symmetric practice among Iranian Zoroastrians.',
    'If the asymmetry is custom rather than doctrine, the priestly authority''s claim to ritual necessity is weaker and the extraction component becomes harder to justify as coordination cost; if doctrinally embedded, the coordination claim is stronger even though the gendered cost remains real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gendered_asymmetry_doctrinal_status, conceptual, 'Whether gendered endogamy enforcement is doctrinal necessity or administrative custom.').

omega_variable(
    kernel_reading_boundary_location,
    'Where exactly does the disagreement with sibling kernel readings live — is it in who holds interpretive authority (priesthood vs. text vs. state), or in what marriage is FOR (community survival vs. sacrament vs. contract)?',
    'Cross-reading comparison: examine whether a Parsi couple''s marriage validity claim would be assessed identically under the secular_contractual_reading''s criteria (consent, registration) even while failing the community-recognition criteria this reading applies — if validity diverges only on recognition, not on contract formation, the disagreement is located in purpose/function, not procedure.',
    'If the disagreement is purely about interpretive authority, this reading and the secular_contractual_reading could in principle be reconciled by layering (civil validity plus optional community recognition); if it is about what marriage is fundamentally FOR, the readings are more deeply incompatible and reconciliation would require one framework to yield.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary_location, conceptual, 'Locating the structural disagreement between this reading and its siblings: authority-of-interpretation versus purpose-of-institution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__parsi_zoroastrian_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t1950, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 1950, 0.15).
narrative_ontology:measurement(fami_tr_t1965, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 1965, 0.18).
narrative_ontology:measurement(fami_tr_t1980, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(fami_tr_t1995, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 1995, 0.23).
narrative_ontology:measurement(fami_tr_t2010, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 2010, 0.26).
narrative_ontology:measurement(fami_tr_t2024, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(fami_be_t1950, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 1950, 0.42).
narrative_ontology:measurement(fami_be_t1965, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 1965, 0.46).
narrative_ontology:measurement(fami_be_t1980, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 1980, 0.5).
narrative_ontology:measurement(fami_be_t1995, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 1995, 0.53).
narrative_ontology:measurement(fami_be_t2010, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 2010, 0.56).
narrative_ontology:measurement(fami_be_t2024, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t1950, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 1950, 0.4).
narrative_ontology:measurement(fami_su_t1965, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 1965, 0.45).
narrative_ontology:measurement(fami_su_t1980, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 1980, 0.5).
narrative_ontology:measurement(fami_su_t1995, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 1995, 0.55).
narrative_ontology:measurement(fami_su_t2010, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 2010, 0.59).
narrative_ontology:measurement(fami_su_t2024, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__parsi_zoroastrian_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(family_law_authority__parsi_zoroastrian_reading, 0.1).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, muslim_shariat_reading).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, christian_canonical_reading).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, secular_contractual_reading).

% DUAL FORMULATION NOTE:
% This story is one of five sibling readings of the family_law_authority kernel. Each reading is authored as an independent, ε-invariant constraint with its own beneficiary/victim structure and classification; the network edges here record kernel co-membership, not causal influence in the usual sense. The parsi_zoroastrian_reading is distinguished by its small-population survival logic and priestly ritual-validity authority, structurally different from dharmashastra's sacramental samskara framing, shariat's contractual framing, canonical law's ecclesiastical sacrament framing, and the secular_contractual_reading's individual-autonomy framing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
