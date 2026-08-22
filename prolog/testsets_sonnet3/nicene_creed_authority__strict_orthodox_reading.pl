% ============================================================================
% CONSTRAINT STORY: nicene_creed_authority__strict_orthodox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_creed_authority__strict_orthodox_reading, []).

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
 *   constraint_id: nicene_creed_authority__strict_orthodox_reading
 *   human_readable: Nicene Creed as Binding Metaphysical Ontology (Strict Orthodox Reading)
 *   domain: religious/doctrinal/ecclesiological
 *
 * SUMMARY:
 *   This story instantiates the strict orthodox reading of the Nicene Creed
 *   kernel: the creed as a fixed metaphysical proposition binding all
 *   believers to one ontological account of the Trinity and the person of
 *   Christ, such that deviation constitutes heresy warranting formal sanction
 *   (excommunication, historically also civil penalty). This reading is
 *   distinct from the symbolic/confessional reading (authority from community
 *   discernment) and the liturgical habituation reading (identity marker
 *   independent of cognitive assent) — those are separate constraints with
 *   their own epsilon values, not alternative measurements of this one. Under
 *   the strict orthodox reading, the creed's coordination function
 *   (preventing doctrinal fragmentation) is real but is fused with an
 *   enforcement apparatus that concentrates interpretive authority in
 *   hierarchical clergy and imposes severe costs on heterodox communities and
 *   lay dissenters, which is why this reading computes as tangled rope rather
 *   than mountain or pure rope.
 *
 * KEY AGENTS:
 *   - hierarchical_clergy: primary beneficiary and agenda-setter (institutional/arbitrage) — collects interpretive authority and institutional legitimacy
 *   - heterodox_communities: primary victim (powerless/trapped) — bears excommunication and historically civil sanction
 *   - lay_interpreters: secondary victim (powerless/constrained) — bears social and sacramental exclusion for private doctrinal deviation
 *   - excommunicated_theologians: acute victim (moderate/trapped) — loses vocation and standing through conciliar condemnation
 *   - ecumenical_councils: rule-making and adjudicating body — sets doctrine and tries dissent within the same institution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_creed_authority__strict_orthodox_reading, 0.68).
domain_priors:suppression_score(nicene_creed_authority__strict_orthodox_reading, 0.79).
domain_priors:theater_ratio(nicene_creed_authority__strict_orthodox_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_creed_authority__strict_orthodox_reading, tangled_rope).
narrative_ontology:human_readable(nicene_creed_authority__strict_orthodox_reading, "Nicene Creed as Binding Metaphysical Ontology (Strict Orthodox Reading)").
narrative_ontology:topic_domain(nicene_creed_authority__strict_orthodox_reading, "religious/doctrinal/ecclesiological").

domain_priors:requires_active_enforcement(nicene_creed_authority__strict_orthodox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_creed_authority__strict_orthodox_reading, '2e85c214-7d25-4354-a11e-20939e744ecf').
narrative_ontology:cs_kernel_codification('2e85c214-7d25-4354-a11e-20939e744ecf', fixed_text).
narrative_ontology:cs_authority_grounding('2e85c214-7d25-4354-a11e-20939e744ecf', lineage).
narrative_ontology:cs_interpretation_layer_present('2e85c214-7d25-4354-a11e-20939e744ecf').
narrative_ontology:cs_reading_relation('2e85c214-7d25-4354-a11e-20939e744ecf', nicene_creed_authority__symbolic_confessional_reading, forecloses).
narrative_ontology:cs_reading_relation('2e85c214-7d25-4354-a11e-20939e744ecf', nicene_creed_authority__liturgical_habituation_reading, influences).
narrative_ontology:cs_axiom('2e85c214-7d25-4354-a11e-20939e744ecf', foundational, creedal_propositions_are_literally_true_metaphysical_claims).
narrative_ontology:cs_axiom_status(creedal_propositions_are_literally_true_metaphysical_claims, holdable).
narrative_ontology:cs_axiom_grounding('2e85c214-7d25-4354-a11e-20939e744ecf', creedal_propositions_are_literally_true_metaphysical_claims, deontological).
narrative_ontology:cs_axiom('2e85c214-7d25-4354-a11e-20939e744ecf', foundational, deviation_from_precise_ontological_formulation_constitutes_damnable_heresy).
narrative_ontology:cs_axiom_status(deviation_from_precise_ontological_formulation_constitutes_damnable_heresy, holdable).
narrative_ontology:cs_axiom_grounding('2e85c214-7d25-4354-a11e-20939e744ecf', deviation_from_precise_ontological_formulation_constitutes_damnable_heresy, conventional).
narrative_ontology:cs_reference_frame('2e85c214-7d25-4354-a11e-20939e744ecf', conciliar_ontological_definition).
narrative_ontology:cs_drift_state('2e85c214-7d25-4354-a11e-20939e744ecf', post_reformation_pluralism, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2e85c214-7d25-4354-a11e-20939e744ecf', '').
narrative_ontology:cs_kernel_id(nicene_creed_authority__strict_orthodox_reading, nicene_creed_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_creed_authority__strict_orthodox_reading, hierarchical_clergy).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__strict_orthodox_reading, creedal_orthodoxy_institutions).
narrative_ontology:constraint_victim(nicene_creed_authority__strict_orthodox_reading, heterodox_communities).
narrative_ontology:constraint_victim(nicene_creed_authority__strict_orthodox_reading, lay_interpreters).
narrative_ontology:constraint_victim(nicene_creed_authority__strict_orthodox_reading, excommunicated_theologians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bishops and councils define, interpret, and enforce the creed's precise metaphysical content (homoousios, hypostatic union, Trinitarian relations). They convene councils, issue anathemas, and control ordination and communion, which gives them the power to declare deviation heretical and to exclude dissenters from sacramental and institutional life. Their authority and institutional legitimacy are constituted by successfully policing the boundary the creed draws.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, hierarchical_clergy, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(nicene_creed_authority__strict_orthodox_reading, hierarchical_clergy, beneficiary).

% Seminaries, magisterial bodies, and denominational structures whose institutional identity, curricula, and claim to continuity with the apostolic church depend on the creed functioning as fixed, binding metaphysical truth rather than contingent formula. Their doctrinal authority over believers is downstream of the creed's status as non-negotiable.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, creedal_orthodoxy_institutions, beneficiary,
    institutional, civilizational, arbitrage, global).

% Communities holding alternative Christological or Trinitarian formulations (e.g., historically, Arian, Monophysite, or Nestorian-adjacent groups) are declared heretical, excommunicated, and in historical periods subjected to civil sanction, exile, or violence enforced through alliance with state power. Their theological alternatives are not merely disagreed with but structurally delegitimized as damnable error, foreclosing coexistence within the institution.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, heterodox_communities, payer,
    powerless, generational, trapped, regional).

% Ordinary believers who read scripture or reason toward metaphysical conclusions differing from the creed's precise formulations face catechetical correction, denial of sacraments, or social exclusion from their faith community. Exit means leaving the only community structure through which they access worship, marriage rites, burial, and social belonging — a high-cost, identity-disrupting option, not a costless alternative.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, lay_interpreters, payer,
    powerless, biographical, constrained, local).

% Trained clergy and scholars (e.g., historical figures condemned at ecumenical councils) who articulate rival ontological accounts of Christ's nature or the Trinity are formally condemned, stripped of office, and in many historical cases exiled. Their intellectual and vocational investment is destroyed by a single conciliar verdict they had no power to appeal outside the same hierarchy that benefits from uniformity.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, excommunicated_theologians, payer,
    moderate, biographical, trapped, regional).

% The conciliar mechanism (Nicaea, Constantinople, Chalcedon) that produces and ratifies the creed's precise wording, adjudicates disputes, and issues anathemas. Functions as both the rule-making body and the tribunal, with no external body to review its verdicts within the tradition it constitutes.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, ecumenical_councils, agenda_setter,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(nicene_creed_authority__strict_orthodox_reading, ecumenical_councils, observer).

% Roman and later Byzantine imperial authority frequently enforced conciliar verdicts with civil sanction, but is excluded from the theological framing of the constraint as a purely doctrinal matter — its coercive contribution to heresy suppression is structurally present but not named within the creed's own self-understanding.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, state_power_historical, excluded,
    powerful, generational, arbitrage, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nicene_creed_authority__strict_orthodox_reading, hierarchical_clergy).
narrative_ontology:fixing_cost_class(nicene_creed_authority__strict_orthodox_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, shared confession of the nature of God and Christ so that geographically and culturally dispersed Christian communities can recognize one another as holding the same faith, share sacraments, and resist genuine doctrinal fragmentation that would dissolve ecclesial unity.
% TRANSFER_FUNCTION: Moves interpretive authority and the power to define communion membership away from individual believers and local theological innovation toward the hierarchical clergy and conciliar institutions; moves social, sacramental, and in historical periods civil standing away from those judged heterodox toward those certified orthodox.
% ABSENT_VOICES: Heterodox theologians whose Christological or Trinitarian formulations were condemned rarely had a forum to argue their case on equal footing with the councils that judged them; the councils were composed overwhelmingly of bishops already aligned with the emerging orthodox consensus, and rival positions are known to us largely through the polemical accounts of their opponents.
% DISAPPEARANCE_RATIONALE: If the strict-orthodox binding reading dissolved overnight, ecclesial bodies would lose their mechanism for excluding dissenting Christological positions from communion; excommunicated lineages could be reintegrated, heresy trials would lose their doctrinal warrant, and denominational identity built on creedal fidelity would require re-grounding elsewhere (scripture alone, tradition broadly construed, or communal discernment) — a substantial institutional rearrangement, not a null change.
% FOUNDING_PROBLEM: Fourth-century Christian communities held genuinely incompatible accounts of Christ's relationship to God the Father (Arian subordinationism vs. homoousian co-equality), threatening to fracture the church into mutually excommunicating factions and undermining a coherent public presentation of the faith under imperial patronage.
% FOUNDING_PROBLEM_CORROBORATION: Clergy and magisterial bodies attest the metaphysical dispute remains live and doctrinally consequential today. Historians of early Christianity and comparative theologians outside the beneficiary hierarchy attest that the fourth-century political and ecclesiastical unification problem the creed was built to solve is substantially resolved, and that continued strict-binding enforcement in the present functions primarily to maintain institutional boundary and authority rather than to answer an active schism.
narrative_ontology:disappearance_verdict(nicene_creed_authority__strict_orthodox_reading, world_rearranges).
narrative_ontology:founding_problem_status(nicene_creed_authority__strict_orthodox_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_creed_authority__strict_orthodox_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nicene_creed_authority__strict_orthodox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_creed_authority__strict_orthodox_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_creed_authority__strict_orthodox_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nicene_creed_authority__strict_orthodox_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nicene_creed_authority__strict_orthodox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored above 0.56 as specified by the reading's structural delta: the creed under this reading does not merely coordinate belief but converts metaphysical precision into a gatekeeping mechanism controlling sacramental access, ordination, and communal belonging. Suppression is high (0.79 at interval end) because enforcement historically fused doctrinal and civil sanction and required continuous conciliar and inquisitorial machinery to hold; it dips at points of relative imperial withdrawal (post-Constantinian fragmentation) and rises again with Reformation-era confessionalization and counter-reformation doctrinal policing, hence the non-monotonic suppression_requirement series on the shared grid. Theater ratio is comparatively low and rising slowly — the mechanism was substantively enforced, not merely ceremonial, though later centuries show creeping performative recitation alongside declining capacity for actual heresy trials.
 *
 * PERSPECTIVAL GAP:
 *   From the hierarchical clergy and conciliar seats, the creed is the coordination solution to a real fourth-century crisis of doctrinal fragmentation, and enforcement is the necessary cost of preserving unity. From the heterodox communities, lay interpreters, and excommunicated theologians, the identical structure operates as forced ontological conformity backed by exclusion and, historically, violence — the same textual commitment, read from a trapped/powerless seat, computes as extraction rather than coordination. The engine's per-seat computation is expected to diverge sharply here; that divergence is the intended measurement, not an inconsistency to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Hierarchical clergy and creedal institutions sit near the beneficiary end: they set the ontology's precise boundaries, adjudicate deviation, and derive institutional legitimacy and authority from doing so, with arbitrage-grade exit (they can revise interpretation via further councils without losing standing). Heterodox communities and excommunicated theologians sit near the full-target end: trapped exit options, no appeal outside the same hierarchy that condemns them, and severe consequences (exclusion, exile, historically execution) for holding a differing ontology. Lay interpreters sit closer to the target end than a symmetric position because their exit costs (loss of sacramental and community access) are high relative to their power to contest doctrine, even though enforcement against ordinary laypeople was typically less severe than against theologians and organized heterodox communities.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — averting fourth-century ecclesial fracture over Christ's nature — is substantially resolved in the sense that the specific fourth/fifth-century schisms it targeted are settled historical facts, not live theological contests for most of the tradition's heirs. Yet the strict-binding enforcement apparatus persists in denominations that treat any deviation from precise creedal wording as heresy today, long after the specific historical crisis passed. Classifying this as tangled_rope rather than snare preserves the genuine original coordination function (it did prevent a real fragmentation) while registering that the enforcement machinery has outlived the acute crisis and now operates chiefly to maintain institutional boundary and hierarchical authority — exactly the seat-divergence and founding-problem-status mismatch (status=contested, verdict=world_rearranges) the six-questions battery is designed to surface, rather than mislabeling either the original council or present-day enforcement as pure extraction from the outset.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the Nicene Creed''s authority best modeled as a binding metaphysical ontology (this reading), a historically contingent confessional witness (symbolic_confessional_reading), or a liturgical identity marker independent of cognitive assent (liturgical_habituation_reading)?',
    'No empirical resolution mechanism exists; the choice of reading is itself a theological and ecclesiological commitment held differently by different denominational traditions and individual believers. Historical evidence about how councils understood their own authority (as defining truth vs. as pastoral boundary-setting) partially informs but does not settle which reading is correct for present-day communities.',
    'Under the strict orthodox reading, deviation is heresy warranting sanction and the constraint computes as substantially extractive tangled rope. Under the symbolic_confessional_reading, the same creed computes with near-zero extraction as a rope or mountain-adjacent coordination device. Under the liturgical_habituation_reading, cognitive assent to metaphysical content is not even the operative mechanism, changing what counts as deviation at all. These are not three measurements of one constraint but three distinct constraints; this file deliberately holds only the strict orthodox reading fixed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Which of three structurally distinct readings of creedal authority is operative for a given community or era.').

omega_variable(
    coercion_vs_persuasion_historical_variance,
    'To what extent did historical heresy enforcement under the strict orthodox reading depend on state coercive power (exile, execution) versus purely ecclesial sanction (excommunication, loss of communion)?',
    'Historical record of specific enforcement episodes (post-Nicene Arian controversies, Chalcedonian schism enforcement, medieval inquisitorial proceedings) disaggregated by whether civil authority was invoked, and comparison across periods and regions where church and state were more or less fused.',
    'If enforcement was substantially dependent on state coercion rather than ecclesial sanction alone, the constraint''s suppression score in periods of church-state separation should be revised downward, and the classification in modern voluntary-association contexts (where excommunication carries no civil penalty) would shift toward a less coercive profile than in the Byzantine or medieval periods captured by this story''s high suppression_requirement measurements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_vs_persuasion_historical_variance, empirical, 'Whether measured suppression reflects intrinsic doctrinal enforcement or borrowed state coercive capacity.').

omega_variable(
    founding_problem_persistence_ambiguity,
    'Is the fourth-century Trinitarian/Christological crisis the creed was built to resolve genuinely dead, or does it persist in modified form (e.g., ongoing disputes over the filioque clause, or contemporary heterodox Christologies in newer movements)?',
    'Survey of contemporary denominational splits and heresy charges to determine whether they invoke substantially the same ontological questions the original councils addressed, or address genuinely new theological questions using inherited creedal machinery.',
    'If the founding problem is substantially dead, present-day strict enforcement functions primarily as institutional boundary maintenance (supporting a piton-adjacent reading of contemporary enforcement specifically); if it persists in modified form, present-day enforcement retains more of its original coordination character.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_persistence_ambiguity, empirical, 'Whether the fourth-century crisis motivating the creed remains live in any substantial form today.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_creed_authority__strict_orthodox_reading, 0, 1700).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nice_tr_t0, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(nice_tr_t300, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 300, 0.18).
narrative_ontology:measurement(nice_tr_t700, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 700, 0.22).
narrative_ontology:measurement(nice_tr_t1100, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 1100, 0.24).
narrative_ontology:measurement(nice_tr_t1500, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 1500, 0.26).
narrative_ontology:measurement(nice_tr_t1700, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 1700, 0.28).

% Extraction over time
narrative_ontology:measurement(nice_be_t0, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(nice_be_t300, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 300, 0.7).
narrative_ontology:measurement(nice_be_t700, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 700, 0.66).
narrative_ontology:measurement(nice_be_t1100, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 1100, 0.6).
narrative_ontology:measurement(nice_be_t1500, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 1500, 0.63).
narrative_ontology:measurement(nice_be_t1700, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 1700, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(nice_su_t0, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(nice_su_t300, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 300, 0.85).
narrative_ontology:measurement(nice_su_t700, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 700, 0.75).
narrative_ontology:measurement(nice_su_t1100, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 1100, 0.6).
narrative_ontology:measurement(nice_su_t1500, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 1500, 0.55).
narrative_ontology:measurement(nice_su_t1700, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 1700, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_creed_authority__strict_orthodox_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(nicene_creed_authority__strict_orthodox_reading, 0.08).
narrative_ontology:affects_constraint(nicene_creed_authority__strict_orthodox_reading, symbolic_confessional_reading).
narrative_ontology:affects_constraint(nicene_creed_authority__strict_orthodox_reading, liturgical_habituation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings decomposing the natural-language concept 'the authority of the Nicene Creed.' strict_orthodox_reading (this file) treats the creed as binding metaphysical ontology with heresy sanctions (ε=0.68, tangled_rope). symbolic_confessional_reading treats it as historically contingent communal witness (expected low ε, rope or mountain-adjacent). liturgical_habituation_reading treats it as a performative identity marker decoupled from cognitive assent (expected low-to-moderate ε, distinct beneficiary/victim structure centered on liturgical belonging rather than doctrinal assent). Each carries its own stable epsilon per the ε-invariance principle; they are linked here rather than merged because measuring 'the creed's authority' three different ways produces three different extraction profiles, which is the signal that these are three constraints, not one constraint under three observables.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
