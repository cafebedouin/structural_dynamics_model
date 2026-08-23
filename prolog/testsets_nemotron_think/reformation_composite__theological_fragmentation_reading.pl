% ============================================================================
% CONSTRAINT STORY: reformation_composite__theological_fragmentation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_composite__theological_fragmentation_reading, []).

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
 *   constraint_id: reformation_composite__theological_fragmentation_reading
 *   human_readable: Confessional Boundary Formation from Competing Soteriological Commitments
 *   domain: historical/religious/political_economy
 *
 * SUMMARY:
 *   This constraint story captures the theological fragmentation reading of
 *   the Reformation: the claim that competing soteriological and
 *   ecclesiological commitments (justification by faith alone vs. sacramental
 *   mediation; Scripture alone vs. Scripture+Tradition; visible church vs.
 *   invisible church) generated structurally incompatible confessional
 *   documents, which in turn hardened into denominational boundaries. The
 *   constraint is the system of confessional subscription that makes
 *   denominational identity a condition of full communal participation.
 *   Denominational leadership and confessional institutions benefit from the
 *   boundary system; laity and dissenting theologians bear its conformity
 *   costs. The engine computes per-seat types from the structural data; the
 *   claimed_type (tangled_rope) reflects this author's judgment that the
 *   constraint coordinates genuine theological community while extracting
 *   interpretive authority and resources.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__theological_fragmentation_reading, 0.52).
domain_priors:suppression_score(reformation_composite__theological_fragmentation_reading, 0.48).
domain_priors:theater_ratio(reformation_composite__theological_fragmentation_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, resistance, 0.44).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__theological_fragmentation_reading, tangled_rope).
narrative_ontology:human_readable(reformation_composite__theological_fragmentation_reading, "Confessional Boundary Formation from Competing Soteriological Commitments").
narrative_ontology:topic_domain(reformation_composite__theological_fragmentation_reading, "historical/religious/political_economy").

domain_priors:requires_active_enforcement(reformation_composite__theological_fragmentation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__theological_fragmentation_reading, 'aba175d4-b5a1-4f46-8c25-591d7d18905b').
narrative_ontology:cs_kernel_codification('aba175d4-b5a1-4f46-8c25-591d7d18905b', fixed_text).
narrative_ontology:cs_authority_grounding('aba175d4-b5a1-4f46-8c25-591d7d18905b', lineage).
narrative_ontology:cs_interpretation_layer_present('aba175d4-b5a1-4f46-8c25-591d7d18905b').
narrative_ontology:cs_reading_relation('aba175d4-b5a1-4f46-8c25-591d7d18905b', reformation_composite__political_realignment_reading, coexists_with).
narrative_ontology:cs_reading_relation('aba175d4-b5a1-4f46-8c25-591d7d18905b', reformation_composite__technological_mediation_reading, coexists_with).
narrative_ontology:cs_axiom('aba175d4-b5a1-4f46-8c25-591d7d18905b', foundational, soteriological_commitment_determines_ecclesial_boundary).
narrative_ontology:cs_axiom_status(soteriological_commitment_determines_ecclesial_boundary, holdable).
narrative_ontology:cs_axiom_grounding('aba175d4-b5a1-4f46-8c25-591d7d18905b', soteriological_commitment_determines_ecclesial_boundary, deontological).
narrative_ontology:cs_axiom('aba175d4-b5a1-4f46-8c25-591d7d18905b', secondary, confessional_subscription_as_visibile_unity_substitute).
narrative_ontology:cs_axiom_status(confessional_subscription_as_visibile_unity_substitute, holdable).
narrative_ontology:cs_axiom_grounding('aba175d4-b5a1-4f46-8c25-591d7d18905b', confessional_subscription_as_visibile_unity_substitute, conventional).
narrative_ontology:cs_reference_frame('aba175d4-b5a1-4f46-8c25-591d7d18905b', confessional_orthodoxy_framework).
narrative_ontology:cs_drift_state('aba175d4-b5a1-4f46-8c25-591d7d18905b', post_westphalian_secularization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('aba175d4-b5a1-4f46-8c25-591d7d18905b', '').
narrative_ontology:cs_kernel_id(reformation_composite__theological_fragmentation_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, denominational_leadership).
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, confessional_institutions).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, laity_constrained_by_confessional_boundaries).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, dissenting_theologians).
narrative_ontology:constraint_vindicates(reformation_composite__theological_fragmentation_reading, sola_fide_justification).
narrative_ontology:constraint_vindicates(reformation_composite__theological_fragmentation_reading, sola_scriptura_authority).
narrative_ontology:constraint_vindicates(reformation_composite__theological_fragmentation_reading, priesthood_of_all_believers).
narrative_ontology:constraint_vindicates(reformation_composite__theological_fragmentation_reading, visible_invisible_church_distinction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control confessional subscription requirements, ordination standards, and institutional resources. Benefit from fragmentation because each confessional boundary creates a distinct jurisdiction with its own revenue base, educational system, and missionary apparatus. Cannot easily exit their own denomination without losing institutional position, but collectively maintain the boundary system that sustains their authority.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, denominational_leadership, agenda_setter,
    institutional, generational, constrained, global).

% Seminaries, publishing houses, missionary societies, and charitable networks tied to specific confessional traditions. Receive resources and legitimacy from the boundary system. Their existence depends on denominational distinctiveness; ecumenical convergence threatens their institutional rationale.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, confessional_institutions, beneficiary,
    organized, generational, constrained, global).

% Bound to a denominational tradition by baptism, family, community, and formed conscience. Changing denominations requires adopting a new confessional identity, often experienced as spiritual betrayal or existential rupture. Bear the conformity costs of confessional subscription (tithing, participation, doctrinal assent) while having no voice in confessional revision.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, laity_constrained_by_confessional_boundaries, payer,
    organized, biographical, identity_locked, regional).

% Theologians whose work challenges confessional boundaries from within (e.g., historical-critical scholars, liberation theologians, feminist theologians). Face discipline, deposition, or marginalization. Their exit options are constrained because academic theology remains confessionally segmented; leaving means losing institutional affiliation and voice.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, dissenting_theologians, excluded,
    moderate, biographical, constrained, regional).

% Regulates religious corporations, marriage law, education funding, and tax exemption. Treats denominations as legal entities regardless of theological claims. Has no theological stake but shapes the civic space in which confessional boundaries operate.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, secular_state, observer,
    institutional, generational, analytical, national).

% Institutional projects (WCC, bilateral dialogues) seeking visible unity across confessional boundaries. Produces convergence texts but lacks authority to bind denominational leadership. Exists in tension with the boundary system it studies.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, ecumenical_movement, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides theological coherence and communal identity through shared confessional commitments; solves the problem of how to maintain doctrinal integrity and corporate worship without a magisterial center.
% TRANSFER_FUNCTION: Moves interpretive authority over Scripture and tradition from the laity to confessional standards and their authorized interpreters; moves material resources (tithes, endowments, property) to denominational structures; moves conformity costs onto those whose conscience or theology strains against confessional boundaries.
% ABSENT_VOICES: Spiritual seekers who find no denominational home because their convictions cross confessional lines; theologians who would synthesize traditions (e.g., Lutheran-Catholic, Reformed-Anabaptist) but are excluded by subscription requirements; the global poor whose religious options are shaped by missionary denominationalism they did not choose.
% DISAPPEARANCE_RATIONALE: If confessional boundaries vanished overnight, the denominational system would collapse: communion tables would open, ordination standards would dissolve, seminaries would merge or close, missionary structures would consolidate, and the legal entities holding billions in assets would face existential restructuring. The social geography of Protestantism would reorganize around new poles (perhaps theological affinity networks, perhaps regional churches).
% FOUNDING_PROBLEM: After Luther's break with Rome, how to maintain doctrinal coherence and visible church unity without papal authority? The confessional documents (Augsburg Confession, Heidelberg Catechism, Westminster Standards, etc.) were the solution: fixed texts that could function as a substitute magisterium.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the confessionalization era (e.g., Heinz Schilling, Ute Lotz-Heumann) attest the problem was real and the confessional solution functionally effective for state-building. Social historians of popular religion (e.g., Keith Thomas, Carlo Ginzburg) attest that lay reception was partial and coerced. Modern ecumenical theologians (e.g., George Lindbeck, Avery Dulles) attest the founding problem persists in new form: how to confess faith together across the boundaries the confessions created.
narrative_ontology:disappearance_verdict(reformation_composite__theological_fragmentation_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_composite__theological_fragmentation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__theological_fragmentation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reformation_composite__theological_fragmentation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_composite__theological_fragmentation_reading, 0.52, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_composite__theological_fragmentation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reformation_composite__theological_fragmentation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reformation_composite__theological_fragmentation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) reflects that confessional boundaries extract interpretive monopoly and material resources while providing genuine coordination (shared worship, catechesis, discipline). Suppression (0.48) is moderate: boundaries are enforced through social pressure, exclusion from communion, and employment conditions in confessional institutions, not state violence (post-Westphalia). Theater ratio (0.31) captures that confessional documents have real theological function but increasingly serve identity-maintenance rather than doctrinal dispute. Accessibility collapse (0.58) reflects that alternatives exist (other denominations, non-denominational churches, the 'nones') but switching costs are high due to identity_lock. Resistance (0.44) reflects ongoing schism, reform movements, and ecumenical pressure.
 *
 * PERSPECTIVAL GAP:
 *   From the denominational leadership seat, the confessional boundary is a rope: it coordinates worship, mission, and discipleship. From the laity seat, it is a snare: it extracts conformity with no voice in revision. From the dissenting theologian seat, it is a tangled_rope: it coordinates the community they serve while extracting their interpretive labor. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Denominational leadership (agenda_setter, institutional power, constrained exit) sits near the beneficiary end (d ~ 0.2): they control the constraint and collect its rents. Confessional institutions (beneficiary, organized power, constrained exit) similarly benefit. Laity (payer, organized power, identity_locked exit) sit near the target end (d ~ 0.8): they bear conformity costs with structurally fused identity that makes exit existentially costly. Dissenting theologians (excluded, moderate power, constrained exit) are structurally excluded from the coordination function. Secular state and ecumenical movement (observers, analytical exit) sit at d ~ 0.5: they analyze but do not directly participate.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to maintain doctrinal coherence without papal authority) was live in 1517-1648. By 1789, Enlightenment critiques and state churches had shifted the problem. By 1962 (Vatican II), the ecumenical movement reframed it as a unity problem. Today the founding problem is contested: confessionalists say boundaries still solve it; ecumenists say boundaries are the problem. The mandate has not been resolved; it has been displaced. The confessional system persists as a piton-like structure in many denominations (theater_ratio rising) but remains a tangled_rope where confessional identity still animates communal life.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_primacy_ambiguity,
    'Is the Reformation''s causal primacy theological, political, or technological — or is the kernel itself a false unity imposing a single-causal story on a multi-causal event?',
    'Counterfactual historical modeling: remove each factor (theological dissent, princely sovereignty, print technology) and simulate whether the other two suffice to produce the observed outcome (confessional fragmentation, state churches, vernacular Bibles).',
    'If theological commitments are not primary, this reading''s claimed_type (tangled_rope) may misrepresent a constraint that is actually political (sovereignty extraction) or technological (information cascade coordination) in structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_primacy_ambiguity, conceptual, 'Whether the kernel ''Reformation'' admits a single primary causal reading or demands a constraint family with no dominant member.').

omega_variable(
    confessional_boundary_function,
    'Are confessional boundaries primarily coordination mechanisms (enabling shared worship and witness) or extraction mechanisms (securing institutional resources and interpretive monopoly)?',
    'Compare denominations that relaxed confessional subscription (mainline Protestant) with those that maintained it (confessional Lutheran, Reformed, Catholic): track retention, giving, clergy supply, and missionary vitality over 50 years.',
    'If relaxation correlates with decline, boundaries function as coordination (rope-like). If maintenance correlates with extraction (rising theater_ratio, stagnant vitality), boundaries function as extraction (snare-like).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(confessional_boundary_function, empirical, 'The coordination-extraction balance of confessional subscription in contemporary denominational life.').

omega_variable(
    identity_lock_mechanism,
    'Is the laity''s identity_locked exit option theologically grounded (conscience bound to confession) or socially constructed (community/family pressure masquerading as conscience)?',
    'Longitudinal study of denominational switchers: measure reported spiritual crisis vs. social disruption at 6 months, 2 years, 5 years post-switch. Compare with non-religious community exit (e.g., moving towns).',
    'If identity_lock is theologically grounded, the constraint''s extraction from laity is partly self-imposed (lower effective χ). If socially constructed, the constraint extracts by weaponizing community (higher effective χ).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether identity_locked exit reflects genuine theological conviction or social coercion internalized as conviction.').

omega_variable(
    sibling_reading_influence,
    'Does the theological fragmentation reading structurally influence the political realignment and technological mediation readings (e.g., by providing the legitimating vocabulary for state churches and print campaigns)?',
    'Trace conceptual genealogy: do political treatises (e.g., Bodin, Althusius) and print polemics cite confessional documents as authority? Map citation networks across the three reading corpora.',
    'If this reading influences the siblings, its constraint artifacts (confessional documents) function as upstream coordination nodes for downstream political and technological constraints — a network effect not captured by pairwise analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_influence, conceptual, 'Structural influence of theological fragmentation reading on sibling readings in the reformation_composite kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__theological_fragmentation_reading, 1517, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1517, reformation_composite__theological_fragmentation_reading, theater_ratio, 1517, 0.12).
narrative_ontology:measurement(refo_tr_t1555, reformation_composite__theological_fragmentation_reading, theater_ratio, 1555, 0.18).
narrative_ontology:measurement(refo_tr_t1648, reformation_composite__theological_fragmentation_reading, theater_ratio, 1648, 0.25).
narrative_ontology:measurement(refo_tr_t1789, reformation_composite__theological_fragmentation_reading, theater_ratio, 1789, 0.28).
narrative_ontology:measurement(refo_tr_t1910, reformation_composite__theological_fragmentation_reading, theater_ratio, 1910, 0.3).
narrative_ontology:measurement(refo_tr_t1962, reformation_composite__theological_fragmentation_reading, theater_ratio, 1962, 0.3).
narrative_ontology:measurement(refo_tr_t2024, reformation_composite__theological_fragmentation_reading, theater_ratio, 2024, 0.31).

% Extraction over time
narrative_ontology:measurement(refo_be_t1517, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1517, 0.22).
narrative_ontology:measurement(refo_be_t1555, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1555, 0.38).
narrative_ontology:measurement(refo_be_t1648, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1648, 0.47).
narrative_ontology:measurement(refo_be_t1789, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1789, 0.41).
narrative_ontology:measurement(refo_be_t1910, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1910, 0.49).
narrative_ontology:measurement(refo_be_t1962, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1962, 0.51).
narrative_ontology:measurement(refo_be_t2024, reformation_composite__theological_fragmentation_reading, base_extractiveness, 2024, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1517, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1517, 0.35).
narrative_ontology:measurement(refo_su_t1555, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1555, 0.52).
narrative_ontology:measurement(refo_su_t1648, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1648, 0.58).
narrative_ontology:measurement(refo_su_t1789, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1789, 0.45).
narrative_ontology:measurement(refo_su_t1910, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1910, 0.42).
narrative_ontology:measurement(refo_su_t1962, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1962, 0.4).
narrative_ontology:measurement(refo_su_t2024, reformation_composite__theological_fragmentation_reading, suppression_requirement, 2024, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_composite__theological_fragmentation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(reformation_composite__theological_fragmentation_reading, 0.08).
narrative_ontology:affects_constraint(reformation_composite__theological_fragmentation_reading, reformation_composite__political_realignment_reading).
narrative_ontology:affects_constraint(reformation_composite__theological_fragmentation_reading, reformation_composite__technological_mediation_reading).

% DUAL FORMULATION NOTE:
% This constraint (theological_fragmentation_reading) and its two siblings form the reformation_composite constraint family. Each reading instantiates a distinct constraint from the same kernel with different ε values, beneficiary structures, and claimed types. The theological reading claims confessional boundaries coordinate identity and extract interpretive authority; the political reading claims state churches coordinate sovereignty and extract ecclesiastical revenue; the technological reading claims print networks coordinate dissemination and extract attention capital. All three are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reformation_composite__theological_fragmentation_reading, institutional, 0.15).
constraint_indexing:directionality_override(reformation_composite__theological_fragmentation_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
