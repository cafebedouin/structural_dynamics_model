% ============================================================================
% CONSTRAINT STORY: constitutional_secularism__strict_neutrality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_secularism__strict_neutrality_reading, []).

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
 *   constraint_id: constitutional_secularism__strict_neutrality_reading
 *   human_readable: Strict Equidistance Reading of Constitutional Secularism
 *   domain: constitutional_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This story instantiates the strict-neutrality reading of the
 *   constitutional secularism kernel: the state commits to formal
 *   equidistance from all religions, treating uniform non-interference as the
 *   operative meaning of neutrality. Under this reading, courts and
 *   legislatures decline to intervene in internal religious governance —
 *   personal law, clergy authority, internal doctrine — on the ground that
 *   intervening in one community's affairs while abstaining from another's
 *   would itself be a form of preferential treatment. The reading genuinely
 *   solves the inter-religious capture problem the doctrine was founded on,
 *   but its formalism about equality-between-religions produces a structural
 *   blind spot for inequality-within-religions. Sibling readings
 *   (principled_intervention_reading, reformist_reading) accept a different
 *   premise about what neutrality requires and are NOT part of this
 *   constraint — they are separate stories linked via network edges.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_secularism__strict_neutrality_reading, 0.42).
domain_priors:suppression_score(constitutional_secularism__strict_neutrality_reading, 0.38).
domain_priors:theater_ratio(constitutional_secularism__strict_neutrality_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__strict_neutrality_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_secularism__strict_neutrality_reading, "Strict Equidistance Reading of Constitutional Secularism").
narrative_ontology:topic_domain(constitutional_secularism__strict_neutrality_reading, "constitutional_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(constitutional_secularism__strict_neutrality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__strict_neutrality_reading, '9d95a05a-8ba5-48ac-92a6-a8583ef21e47').
narrative_ontology:cs_kernel_codification('9d95a05a-8ba5-48ac-92a6-a8583ef21e47', fixed_text).
narrative_ontology:cs_authority_grounding('9d95a05a-8ba5-48ac-92a6-a8583ef21e47', lineage).
narrative_ontology:cs_interpretation_layer_present('9d95a05a-8ba5-48ac-92a6-a8583ef21e47').
narrative_ontology:cs_reading_relation('9d95a05a-8ba5-48ac-92a6-a8583ef21e47', constitutional_secularism__principled_intervention_reading, coexists_with).
narrative_ontology:cs_reading_relation('9d95a05a-8ba5-48ac-92a6-a8583ef21e47', constitutional_secularism__reformist_reading, influences).
narrative_ontology:cs_axiom('9d95a05a-8ba5-48ac-92a6-a8583ef21e47', foundational, non_interference_constitutes_neutrality).
narrative_ontology:cs_axiom_status(non_interference_constitutes_neutrality, holdable).
narrative_ontology:cs_axiom_grounding('9d95a05a-8ba5-48ac-92a6-a8583ef21e47', non_interference_constitutes_neutrality, conventional).
narrative_ontology:cs_axiom('9d95a05a-8ba5-48ac-92a6-a8583ef21e47', secondary, internal_community_governance_outside_state_equality_mandate).
narrative_ontology:cs_axiom_status(internal_community_governance_outside_state_equality_mandate, holdable).
narrative_ontology:cs_axiom_grounding('9d95a05a-8ba5-48ac-92a6-a8583ef21e47', internal_community_governance_outside_state_equality_mandate, conventional).
narrative_ontology:cs_reference_frame('9d95a05a-8ba5-48ac-92a6-a8583ef21e47', founding_settlement_equidistance).
narrative_ontology:cs_drift_state('9d95a05a-8ba5-48ac-92a6-a8583ef21e47', contemporary_personal_law_litigation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9d95a05a-8ba5-48ac-92a6-a8583ef21e47', '').
narrative_ontology:cs_kernel_id(constitutional_secularism__strict_neutrality_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__strict_neutrality_reading, religious_majority_institutions).
narrative_ontology:constraint_beneficiary(constitutional_secularism__strict_neutrality_reading, orthodox_community_leadership).
narrative_ontology:constraint_beneficiary(constitutional_secularism__strict_neutrality_reading, state_judiciary_seeking_non_entanglement).
narrative_ontology:constraint_victim(constitutional_secularism__strict_neutrality_reading, intra_community_reform_movements).
narrative_ontology:constraint_victim(constitutional_secularism__strict_neutrality_reading, women_subject_to_personal_law_regimes).
narrative_ontology:constraint_victim(constitutional_secularism__strict_neutrality_reading, religious_minorities_within_minorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Applies the equidistance doctrine to decline ruling on internal religious practices, framing abstention as neutrality. Sets the operative standard through case law: courts will not preferentially favor or interfere with any religion, and will treat interference-avoidance as the default posture unless a practice crosses a narrow constitutional-morality threshold. This abstention is itself an active administrative choice, renewed case by case, not a passive default.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, state_judiciary_seeking_non_entanglement, agenda_setter,
    institutional, generational, analytical, national).

% Retain full control over internal doctrine, personal law, and community governance because the state's equidistance posture treats non-interference as principled rather than as a choice with distributive consequences. Numerical and political weight lets majority institutions shape the surrounding legal and cultural environment even while formally receiving identical treatment to smaller communities.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, religious_majority_institutions, beneficiary,
    organized, civilizational, arbitrage, national).

% Leverages the equidistance doctrine's presumption against state interference to resist internal reform pressure, citing the same constitutional non-interference principle asserted for every other faith. Their standing before courts and legislatures is amplified by the state's reluctance to be seen singling out any one community, which functionally shields the status quo they administer.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, orthodox_community_leadership, beneficiary,
    organized, generational, arbitrage, national).

% Seek to change practices within their own religious community — reforming personal law, challenging clergy authority, opening restricted rites — but find that the strict-neutrality posture treats their claims as internal religious disputes the state will not adjudicate. The same doctrine protecting minority communities from majority imposition also blocks the state from backing internal reformers against entrenched community leadership.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, intra_community_reform_movements, payer,
    moderate, biographical, constrained, national).

% Live under religion-specific personal law on marriage, divorce, inheritance, and custody. The equidistance doctrine's refusal to intervene in 'internal' religious matters leaves these regimes largely untouched by constitutional equality guarantees that would otherwise apply. Exit means leaving the religious community's jurisdiction entirely, which carries severe social and economic cost, or litigating individually against doctrine the state treats as none of its business.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, women_subject_to_personal_law_regimes, payer,
    powerless, biographical, trapped, national).

% Sub-groups and dissenting sects within an already-minority religion find that the equidistance principle recognizes only the dominant internal authority as the community's voice. Their subordination within their own faith tradition receives no state remedy because the state's neutrality is calibrated at the inter-religious level, not the intra-religious level where their actual grievance sits.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, religious_minorities_within_minorities, payer,
    powerless, biographical, trapped, national).

% Argue that formal equidistance without attention to internal power asymmetries entrenches oppression under a neutrality label. They petition courts and legislatures but are structurally outside the doctrine's own operative logic, which treats their arguments for intervention as a departure from neutrality rather than as evidence neutrality is being misapplied.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, constitutional_morality_advocates, excluded,
    moderate, generational, constrained, national).

% Study how equidistance doctrines perform across jurisdictions, comparing outcomes for internal minorities under strict-neutrality versus interventionist secularism models. Their findings feed academic and judicial debate but do not themselves alter the doctrine's operation.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_secularism__strict_neutrality_reading, diffuse).
narrative_ontology:fixing_cost_class(constitutional_secularism__strict_neutrality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents the state from being weaponized by any single religious majority against others by committing to uniform, non-preferential treatment across all religious communities — a genuine solution to the problem of state capture by a dominant faith.
% TRANSFER_FUNCTION: Moves the cost of internal religious power asymmetries (gender hierarchy, caste-like internal stratification, minority-within-minority subordination) from the state's remedial capacity onto the individuals subject to those internal hierarchies, by treating those asymmetries as outside the state's non-interference mandate.
% ABSENT_VOICES: Individual dissenters and reformers inside religious communities — women under personal law, lower-status sub-sects, apostates — are not parties to the inter-religious neutrality bargain the doctrine is built to police, and have no seat at the table where 'equidistance' is defined as the relevant equality metric.
% DISAPPEARANCE_RATIONALE: Majority and orthodox leadership seats would say the world rearranges catastrophically — state favoritism and religious conflict would return. Reform movements and internally subordinated individuals would say functionally little changes for them either way unless disappearance is replaced by an interventionist regime, since strict neutrality was already declining to help them; the disagreement is precisely about what baseline 'unchanged' means.
% FOUNDING_PROBLEM: Post-independence anxiety that the state, if aligned with any one religion, would become an instrument of majoritarian domination over religious minorities — the doctrine was built to prevent state-sponsored religious favoritism during nation-building.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians and comparative scholars outside any religious institution attest the inter-religious capture problem was real and substantially addressed by formal equidistance. Reform movements and feminist legal scholars, also outside the beneficiary set, attest that the founding problem has mutated: equidistance is now invoked to shield intra-religious hierarchy the original framers did not anticipate as the doctrine's operative use.
narrative_ontology:disappearance_verdict(constitutional_secularism__strict_neutrality_reading, contested).
narrative_ontology:founding_problem_status(constitutional_secularism__strict_neutrality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_secularism__strict_neutrality_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_secularism__strict_neutrality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_secularism__strict_neutrality_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_secularism__strict_neutrality_reading_tests).
:- end_tests(constitutional_secularism__strict_neutrality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) and suppression (0.38) sit at moderate levels because the doctrine's harm is diffuse and structural rather than a direct, visible transfer: internally subordinated individuals bear costs through the absence of a remedy rather than through active state coercion against them. Theater ratio (0.28) reflects that non-interference is a real, actively exercised judicial posture, not mere performance — courts genuinely decline jurisdiction rather than pretending to rule. Accessibility collapse (0.45) is moderate: formal legal alternatives (individual litigation, legislative reform, exit from the community) exist but are costly and often blocked by the very doctrine at issue. Resistance (0.55) is substantial, driven by organized reform movements and feminist legal advocacy that persistently challenge the equidistance framing.
 *
 * PERSPECTIVAL GAP:
 *   From the state's own institutional seat, equidistance looks like principled neutrality — a rope solving inter-religious capture. From the seat of an internally subordinated individual, the identical doctrine looks like a tangled rope: real coordination function (preventing majoritarian state capture) bundled with asymmetric extraction (foreclosing remedy for internal hierarchy) that requires active judicial enforcement (repeated abstention rulings) to persist. Both seats are looking at the same structural facts; the divergence is exactly what the engine is built to compute.
 *
 * DIRECTIONALITY LOGIC:
 *   Majority religious institutions and orthodox community leadership sit near the beneficiary end: the doctrine's abstention from internal affairs functions as protection for the internal status quo they administer, and their organized political weight lets them invoke 'neutrality' defensively whenever reform is proposed. Individuals subordinated within their own religious communities — women under personal law, dissenting sub-sects — sit near the target end: the same non-interference that protects majority institutions' autonomy also forecloses state remedy for their subordination, and their exit options are effectively trapped (leaving the community carries severe social/economic cost). The state judiciary is the agenda-setter whose repeated case-by-case choice to characterize non-interference as principled is what keeps the doctrine operative.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — state capture by a dominant faith — remains partially live in plural societies with histories of majoritarian communal violence, which is why this reading cannot be simply declared a dead mandate. But its application has drifted from policing inter-religious favoritism toward shielding intra-religious hierarchy from any state-backed reform, a function the framers of equidistance doctrines rarely intended. This is precisely the contested case mandatrophy analysis exists for: neither 'fully vindicated' nor 'fully obsolete' captures it, hence founding_problem_status is authored as contested rather than resolved either way.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    equidistance_versus_internal_hierarchy_scope,
    'Does genuine state neutrality between religions require non-interference in internal religious governance, or does neutrality only concern the state''s treatment of religions as external collective entities, leaving internal power asymmetries within a community outside the neutrality calculus entirely?',
    'Comparative constitutional analysis of jurisdictions that formally hold strict equidistance but carve explicit exceptions for internal equality claims (e.g., gender-equality override clauses), tracking whether such carve-outs are experienced by majority and minority communities as a departure from neutrality or as a clarification of what neutrality always meant.',
    'If internal hierarchy is genuinely outside the neutrality calculus, this reading is a coherent, non-extractive rope at the doctrinal level and the extraction observed here is better modeled as a separate, un-remedied injustice rather than a structural feature of the doctrine itself. If internal hierarchy is within the calculus properly understood, this reading is a tangled rope precisely because it mislabels an extraction-enabling abstention as principled neutrality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equidistance_versus_internal_hierarchy_scope, conceptual, 'Whether strict equidistance''s internal-affairs abstention is a neutral scope limit or a disguised extraction mechanism.').

omega_variable(
    sibling_reading_foreclosure_boundary,
    'Where exactly does this reading''s premise (equidistance = uniform non-interference) become logically incompatible with the principled_intervention_reading''s premise (selective intervention against internal power asymmetries is compatible with neutrality), versus merely a different policy preference within a framework that could hold both readings at different times?',
    'Doctrinal history: track whether courts operating under this reading have ever carved intervention exceptions without abandoning the equidistance label, and whether such exceptions were framed as reading-internal adjustments or as reading-external departures.',
    'If courts can and do carve intervention exceptions while still calling the result ''equidistance,'' the two readings coexist within a single evolving framework (supporting influences or coexists_with) rather than one foreclosing the other. If any intervention exception is treated by the tradition as abandoning equidistance altogether, the readings are more nearly forecloses-grade incompatible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_boundary, conceptual, 'How sharply this reading''s core premise excludes the principled_intervention_reading''s premise within a single constitutional tradition.').

omega_variable(
    internal_subordination_measurement_gap,
    'Is the extraction borne by internally subordinated individuals (women under personal law, minorities-within-minorities) adequately captured by a single ε score, or does it vary so widely across religious communities and personal-law regimes that this story averages over structurally distinct sub-constraints?',
    'Disaggregated empirical study of outcomes under each major personal-law regime operating under the equidistance umbrella, checking whether ε varies enough across regimes to warrant further decomposition per the ε-invariance principle.',
    'If ε varies widely by regime, this story should itself decompose into per-community constraint stories rather than treating ''strict neutrality'' as a single uniform extraction level across all affected communities.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internal_subordination_measurement_gap, empirical, 'Whether a single ε adequately represents extraction across all personal-law regimes covered by this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_secularism__strict_neutrality_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_secularism__strict_neutrality_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cons_tr_t10, constitutional_secularism__strict_neutrality_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement(cons_tr_t25, constitutional_secularism__strict_neutrality_reading, theater_ratio, 25, 0.2).
narrative_ontology:measurement(cons_tr_t40, constitutional_secularism__strict_neutrality_reading, theater_ratio, 40, 0.23).
narrative_ontology:measurement(cons_tr_t55, constitutional_secularism__strict_neutrality_reading, theater_ratio, 55, 0.26).
narrative_ontology:measurement(cons_tr_t70, constitutional_secularism__strict_neutrality_reading, theater_ratio, 70, 0.28).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(cons_be_t10, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(cons_be_t25, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 25, 0.36).
narrative_ontology:measurement(cons_be_t40, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 40, 0.39).
narrative_ontology:measurement(cons_be_t55, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 55, 0.41).
narrative_ontology:measurement(cons_be_t70, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 70, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(cons_su_t10, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 10, 0.31).
narrative_ontology:measurement(cons_su_t25, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 25, 0.33).
narrative_ontology:measurement(cons_su_t40, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 40, 0.35).
narrative_ontology:measurement(cons_su_t55, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 55, 0.37).
narrative_ontology:measurement(cons_su_t70, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 70, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_secularism__strict_neutrality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_secularism__strict_neutrality_reading, principled_intervention_reading).
narrative_ontology:affects_constraint(constitutional_secularism__strict_neutrality_reading, reformist_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'constitutional secularism' per the ε-invariance principle. Each reading (strict_neutrality_reading, principled_intervention_reading, reformist_reading) is a structurally distinct constraint with its own beneficiary/victim structure and its own ε, generated as a separate file. They share a kernel (the non-establishment constitutional commitment) but instantiate different operative rules for what neutrality requires, producing different classification outcomes. Network edges here connect this reading to its siblings for contamination-propagation analysis; they do not imply shared ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
