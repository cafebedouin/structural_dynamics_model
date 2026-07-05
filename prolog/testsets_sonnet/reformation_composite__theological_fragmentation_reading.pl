% ============================================================================
% CONSTRAINT STORY: reformation_composite__theological_fragmentation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: reformation_composite__theological_fragmentation_reading
 *   human_readable: Reformation as Theological Fragmentation: Confessional Doctrine as Denomination-Forming Constraint
 *   domain: historical_epistemology/religious_history/political_economy
 *
 * SUMMARY:
 *   This story instantiates the theological-fragmentation reading of the
 *   composite Reformation kernel: it treats competing soteriological
 *   commitments (justification by faith alone vs. faith-plus-works, real
 *   presence vs. memorialism, predestination vs. free will) and competing
 *   ecclesiological commitments (episcopal succession vs. congregational
 *   polity vs. presbyterian synods) as the primary generative force producing
 *   structurally incompatible denominations, with confessional documents
 *   (Augsburg Confession 1530, Council of Trent decrees 1545-1563, Book of
 *   Concord 1580, Westminster Confession 1646) as the constraint artifacts
 *   that fix and police the boundaries. This is deliberately NOT the
 *   political-sovereignty reading (rulers using religious difference to
 *   assert independence from Rome/Empire) or the print-technology reading
 *   (mass reproduction turning local dissent into continental movement) —
 *   those are separate constraints with separate ε values, linked here only
 *   as siblings in the same kernel contest.
 *
 * KEY AGENTS:
 *   - confessional_church_hierarchies: institutional beneficiary — drafts and enforces doctrinal boundaries
 *   - denominational_theologians: organized beneficiary — professional identity fused to doctrinal distinctness
 *   - territorial_church_administrators: powerful beneficiary — local implementation and discipline
 *   - religious_minorities_under_confessional_states: powerless payer — trapped, faces exile/execution
 *   - lay_believers_facing_forced_confessional_choice: powerless payer — constrained public conformity
 *   - cross_confessional_families: powerless payer — trapped in doctrinal jurisdiction disputes
 *   - ecumenical_and_secular_historians: analytical observer — assesses convergence over time
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__theological_fragmentation_reading, 0.52).
domain_priors:suppression_score(reformation_composite__theological_fragmentation_reading, 0.61).
domain_priors:theater_ratio(reformation_composite__theological_fragmentation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__theological_fragmentation_reading, tangled_rope).
narrative_ontology:human_readable(reformation_composite__theological_fragmentation_reading, "Reformation as Theological Fragmentation: Confessional Doctrine as Denomination-Forming Constraint").
narrative_ontology:topic_domain(reformation_composite__theological_fragmentation_reading, "historical_epistemology/religious_history/political_economy").

domain_priors:requires_active_enforcement(reformation_composite__theological_fragmentation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__theological_fragmentation_reading, '7a35b3ef-3490-4cc2-ae75-89feff583c37').
narrative_ontology:cs_kernel_codification('7a35b3ef-3490-4cc2-ae75-89feff583c37', formalized).
narrative_ontology:cs_authority_grounding('7a35b3ef-3490-4cc2-ae75-89feff583c37', lineage).
narrative_ontology:cs_interpretation_layer_present('7a35b3ef-3490-4cc2-ae75-89feff583c37').
narrative_ontology:cs_reading_relation('7a35b3ef-3490-4cc2-ae75-89feff583c37', reformation_composite__political_realignment_reading, coexists_with).
narrative_ontology:cs_reading_relation('7a35b3ef-3490-4cc2-ae75-89feff583c37', reformation_composite__technological_mediation_reading, influences).
narrative_ontology:cs_axiom('7a35b3ef-3490-4cc2-ae75-89feff583c37', foundational, soteriological_positions_are_mutually_exclusive_truth_claims).
narrative_ontology:cs_axiom_status(soteriological_positions_are_mutually_exclusive_truth_claims, holdable).
narrative_ontology:cs_axiom_grounding('7a35b3ef-3490-4cc2-ae75-89feff583c37', soteriological_positions_are_mutually_exclusive_truth_claims, deontological).
narrative_ontology:cs_axiom('7a35b3ef-3490-4cc2-ae75-89feff583c37', foundational, confessional_documents_fix_doctrinal_boundary_authoritatively).
narrative_ontology:cs_axiom_status(confessional_documents_fix_doctrinal_boundary_authoritatively, holdable).
narrative_ontology:cs_axiom_grounding('7a35b3ef-3490-4cc2-ae75-89feff583c37', confessional_documents_fix_doctrinal_boundary_authoritatively, conventional).
narrative_ontology:cs_axiom('7a35b3ef-3490-4cc2-ae75-89feff583c37', secondary, denominational_incompatibility_is_theologically_necessitated_not_politically_constructed).
narrative_ontology:cs_axiom_status(denominational_incompatibility_is_theologically_necessitated_not_politically_constructed, holdable).
narrative_ontology:cs_axiom_grounding('7a35b3ef-3490-4cc2-ae75-89feff583c37', denominational_incompatibility_is_theologically_necessitated_not_politically_constructed, empirically_contingent).
narrative_ontology:cs_reference_frame('7a35b3ef-3490-4cc2-ae75-89feff583c37', single_apostolic_church_doctrinal_unity).
narrative_ontology:cs_drift_state('7a35b3ef-3490-4cc2-ae75-89feff583c37', post_thirty_years_war_confessionalization, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('7a35b3ef-3490-4cc2-ae75-89feff583c37', '').
narrative_ontology:cs_kernel_id(reformation_composite__theological_fragmentation_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, confessional_church_hierarchies).
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, denominational_theologians).
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, territorial_church_administrators).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, religious_minorities_under_confessional_states).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, lay_believers_facing_forced_confessional_choice).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, cross_confessional_families).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft and enforce confessional documents (Augsburg Confession, Tridentine decrees, Westminster Confession) that define doctrinal boundaries and administer excommunication, ordination, and property control along those lines. They gain institutional continuity, tithe/benefice revenue, and political patronage from maintaining a distinct, policed confessional identity; they can shift alliances among territorial rulers to preserve institutional position.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, confessional_church_hierarchies, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(reformation_composite__theological_fragmentation_reading, confessional_church_hierarchies, beneficiary).

% Build careers, patronage networks, and intellectual authority on defending and elaborating a specific soteriological position (justification by faith alone, real presence vs. memorialism, predestination). Their professional identity and material support depend on the doctrinal boundary remaining sharp; softening the boundary threatens their institutional role.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, denominational_theologians, beneficiary,
    organized, biographical, constrained, continental).

% Local bishops, superintendents, and consistories implement confessional discipline at the parish level, controlling appointments, marriage validity, and burial rights according to doctrinal conformity. They gain local authority and resource control from confessional sorting of the population.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, territorial_church_administrators, beneficiary,
    powerful, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(reformation_composite__theological_fragmentation_reading, territorial_church_administrators, agenda_setter).

% Anabaptists, Reformed minorities in Lutheran territories, Catholics in Protestant states, and vice versa, face exclusion from office, property confiscation, exile, or execution when their doctrinal commitments do not match the locally established confession. Exit means abandoning land, kin, and livelihood for uncertain refuge elsewhere.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, religious_minorities_under_confessional_states, payer,
    powerless, biographical, trapped, regional).

% Ordinary parishioners must publicly align with whichever confession their ruler or town council adopts (cuius regio, eius religio) to retain access to sacraments, marriage, inheritance, and community standing, regardless of their private theological views. Their doctrinal 'choice' is largely imposed rather than freely reasoned.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, lay_believers_facing_forced_confessional_choice, payer,
    powerless, biographical, constrained, local).

% Families spanning confessional lines through marriage or migration face disputes over child baptism, inheritance, and burial rites, with legal and ecclesiastical structures forcing resolution toward one confession's rules, often at the cost of the marginalized spouse's practice.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, cross_confessional_families, payer,
    powerless, biographical, trapped, local).

% Study confessional formation as a historical process, comparing doctrinal texts, church records, and demographic outcomes across confessions to assess how much fragmentation tracked genuine theological incompatibility versus political and economic interests layered onto it.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, ecumenical_and_secular_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Confessional documents solve a genuine internal coordination problem for each emerging denomination: they specify what a believer must hold to be in good standing, allowing clergy training, liturgical practice, and church discipline to proceed on a shared basis rather than ad hoc local variation.
% TRANSFER_FUNCTION: The arrangement moves clerical authority, tithe and benefice income, property, and social standing away from those whose theological commitments fall outside the locally enforced confession, and concentrates administrative control and doctrinal authority in the hierarchies and theologians who define and police the boundary.
% ABSENT_VOICES: Radical Reformation groups (Anabaptists, Socinians), lay mystics, and syncretic or private believers who did not fit any established confession are largely absent from the confessional documents themselves; their objections survive mainly in polemical attacks against them and court/inquisition records rather than as parties to the settlement.
% DISAPPEARANCE_RATIONALE: If confessional boundary-enforcement vanished, clergy would lose disciplinary and property control tied to doctrinal conformity, mixed marriages and migration would no longer require confessional sorting, and denominational institutions would need to reorganize around voluntary affiliation rather than territorially enforced identity — a substantial rearrangement of church and family law across early modern Europe.
% FOUNDING_PROBLEM: Competing and, in the framers' view, genuinely incompatible answers to how a person is justified before God and how the visible church should be governed needed institutional settlement so that clergy, liturgy, and pastoral practice could function coherently rather than dissolving into unresolvable local disputation.
% FOUNDING_PROBLEM_CORROBORATION: Confessional hierarchies and theologians attest the doctrinal incompatibilities remain live and church-defining. Independent historians of religion and comparative theologians (working from outside any single confession's institutional interest) have documented substantial doctrinal convergence in later ecumenical dialogues (e.g., the Lutheran-Catholic Joint Declaration on Justification, 1999), suggesting that at least part of the original 'structural incompatibility' was a function of polemical hardening and institutional interest rather than irreducible theological content.
narrative_ontology:disappearance_verdict(reformation_composite__theological_fragmentation_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_composite__theological_fragmentation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__theological_fragmentation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reformation_composite__theological_fragmentation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_composite__theological_fragmentation_reading, 0.52, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises from 0.22 at Luther's initial protest to a peak of 0.58 around the Thirty Years' War (1618), reflecting escalating institutional and material stakes tied to confessional conformity, then eases slightly to 0.52 by Westphalia (1648) as territorial settlement reduces (without eliminating) enforcement pressure. Suppression tracks a similar but sharper arc (0.30 to 0.74 to 0.61) because confessional boundary enforcement (excommunication, exile, execution, property seizure) is the mechanism that keeps denominational lines from re-merging once doctrinal disputes might otherwise be locally negotiated. Theater ratio is comparatively low throughout (0.08-0.31) because much of the enforcement activity in this reading is genuinely functional — actual doctrinal examination, actual exclusion from actual sacraments and offices — rather than performative; it rises modestly toward 1618-1648 as confessional identity increasingly serves as a proxy marker for territorial loyalty independent of doctrinal content, which is itself a signal that the theological reading alone cannot fully account for the period's dynamics (a fact the political-realignment sibling reading addresses directly).
 *
 * PERSPECTIVAL GAP:
 *   From the confessional hierarchy's seat, the arrangement is coordination: a genuine, theologically necessary settlement that lets a church function coherently. From the trapped minority or cross-confessional family's seat, the identical boundary-enforcement machinery operates as extraction — loss of standing, property, or family relations for a doctrinal position they may hold sincerely but cannot safely practice. The engine computing tangled_rope from one set of structural facts, and something closer to snare from the payer seats' facts, is the expected divergence this reading should surface, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Confessional hierarchies, theologians, and territorial administrators are the structural beneficiaries: doctrinal boundary-drawing is the mechanism by which they obtain and retain institutional authority, career standing, and administrative control, so their derived directionality sits near the beneficiary end. Religious minorities, lay believers under cuius regio eius religio, and cross-confessional families are the structural targets: the same boundary-drawing mechanism extracts from them in the form of exile, forced conformity, property loss, or family/legal jeopardy, so their directionality sits near the full-target end, amplified by trapped or constrained exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding theological problem (irreconcilable soteriological/ecclesiological positions requiring institutional settlement) is contested rather than dead: confessional differences persist today, but twentieth-century ecumenical dialogue (e.g., the 1999 Joint Declaration on Justification) demonstrates that significant portions of the originally 'structurally incompatible' doctrinal content were reconcilable once institutional and political incentives to maintain sharp boundaries eased. This suggests the fragmentation reading correctly identifies theology as A generative force but risks overstating its independence from the political and economic interests captured in the sibling readings — the tangled_rope classification (rather than a pure mountain of theological necessity) reflects that hybrid structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_incompatibility_vs_institutional_hardening,
    'Were the soteriological and ecclesiological differences underlying the major confessions genuinely structurally incompatible at the time, or were they reconcilable positions that institutional, political, and economic interests hardened into treated-as-incompatible boundaries?',
    'Comparative analysis of the original theological texts against later ecumenical convergence documents (e.g., Joint Declaration on Justification 1999, Lutheran-Reformed Leuenberg Agreement 1973) to assess how much of the claimed incompatibility survives sustained theological re-examination outside the original institutional stakes.',
    'If largely reconcilable, this reading''s claim that the fragmentation was theologically necessitated weakens substantially, and the tangled_rope classification shifts further toward snare (extraction dominant, coordination function thinner than claimed); if genuinely incompatible, the coordination function is more robust and the tangled_rope reading is better supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_incompatibility_vs_institutional_hardening, conceptual, 'Whether the doctrinal incompatibility was real or institutionally manufactured/hardened.').

omega_variable(
    kernel_decomposition_weighting,
    'Among the three sibling readings (theological, political, technological) of the composite Reformation kernel, how should relative causal weight be assigned when they are structurally entangled in the historical record (e.g., a prince''s political interest in Reformation may have been expressed and legitimated through genuine or opportunistic theological commitment)?',
    'This is precisely the ε-invariance decomposition point: each reading is authored as its own constraint with its own ε rather than blended. Resolution is not a weighting exercise within this story but the existence of the three linked sibling stories themselves, cross-referenced via network.affects_constraints.',
    'Refusing to decompose would produce an incoherent single ε that changes depending on which observable (doctrine, sovereignty, print circulation) the analyst foregrounds — exactly the ε-invariance violation the framework exists to prevent. Decomposition (as done here) keeps each reading''s classification stable and comparable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_decomposition_weighting, conceptual, 'Documents why this constraint is one of three linked kernel readings rather than a single blended constraint.').

omega_variable(
    confessional_leadership_as_fsm_candidate,
    'Is the framing of doctrinal necessity itself partly a false-summit dynamic — do confessional hierarchies present doctrinal incompatibility as an immovable theological fact (mountain-like) precisely because they are identifiable beneficiaries of the boundary it justifies?',
    'Track whether confessional bodies that lost material stakes in boundary maintenance (e.g., after disestablishment) subsequently softened doctrinal claims previously treated as non-negotiable, holding theology constant.',
    'If doctrinal rigidity tracks material stakes rather than theological content, portions of the claimed necessity function as cover for institutional self-preservation — supporting classification nearer snare for the enforcement apparatus even while granting a genuine underlying coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(confessional_leadership_as_fsm_candidate, empirical, 'Whether claimed theological necessity partly serves as cover for institutional beneficiary interest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__theological_fragmentation_reading, 1517, 1648).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1517, reformation_composite__theological_fragmentation_reading, theater_ratio, 1517, 0.08).
narrative_ontology:measurement(refo_tr_t1530, reformation_composite__theological_fragmentation_reading, theater_ratio, 1530, 0.13).
narrative_ontology:measurement(refo_tr_t1555, reformation_composite__theological_fragmentation_reading, theater_ratio, 1555, 0.19).
narrative_ontology:measurement(refo_tr_t1572, reformation_composite__theological_fragmentation_reading, theater_ratio, 1572, 0.22).
narrative_ontology:measurement(refo_tr_t1618, reformation_composite__theological_fragmentation_reading, theater_ratio, 1618, 0.31).
narrative_ontology:measurement(refo_tr_t1648, reformation_composite__theological_fragmentation_reading, theater_ratio, 1648, 0.28).

% Extraction over time
narrative_ontology:measurement(refo_be_t1517, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1517, 0.22).
narrative_ontology:measurement(refo_be_t1530, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1530, 0.34).
narrative_ontology:measurement(refo_be_t1555, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1555, 0.44).
narrative_ontology:measurement(refo_be_t1572, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1572, 0.49).
narrative_ontology:measurement(refo_be_t1618, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1618, 0.58).
narrative_ontology:measurement(refo_be_t1648, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1648, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1517, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1517, 0.3).
narrative_ontology:measurement(refo_su_t1530, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1530, 0.42).
narrative_ontology:measurement(refo_su_t1555, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1555, 0.55).
narrative_ontology:measurement(refo_su_t1572, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1572, 0.63).
narrative_ontology:measurement(refo_su_t1618, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1618, 0.74).
narrative_ontology:measurement(refo_su_t1648, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1648, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_composite__theological_fragmentation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(reformation_composite__theological_fragmentation_reading, 0.1).
narrative_ontology:affects_constraint(reformation_composite__theological_fragmentation_reading, reformation_composite__political_realignment_reading).
narrative_ontology:affects_constraint(reformation_composite__theological_fragmentation_reading, reformation_composite__technological_mediation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the reformation_composite kernel, decomposed per the ε-invariance principle rather than blended into a single observer-dependent constraint. The theological_fragmentation_reading takes doctrinal pluralism and confessional documents as primary observables (ε=0.52, tangled_rope). The political_realignment_reading takes sovereignty assertion as primary observable and would carry a different beneficiary set (territorial rulers, emerging state bureaucracies) and likely a different ε. The technological_mediation_reading takes print circulation as primary observable and would carry yet another beneficiary set (printers, literate lay readership) and a distinct ε. All three are linked via affects_constraints; none averages or supersedes the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
