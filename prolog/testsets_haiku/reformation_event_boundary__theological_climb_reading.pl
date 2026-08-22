% ============================================================================
% CONSTRAINT STORY: reformation_event_boundary__theological_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_event_boundary__theological_climb_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: reformation_event_boundary__theological_climb_reading
 *   human_readable: Reformation as Theological Doctrinal Breakthrough (Climb Reading)
 *   domain: religious/epistemological/commitment-system
 *
 * SUMMARY:
 *   The theological_climb_reading of the Reformation construes the event
 *   primarily as a doctrinal innovation: Luther's recovery of justification
 *   by faith alone, grounded in scripture study, as a genuine theological
 *   breakthrough requiring institutional separation from a Church that had
 *   institutionalized a false doctrine (faith plus works). In this reading,
 *   the reformers are intellectual discoverers correcting inherited error;
 *   the Catholic Church becomes the victim of a doctrinal challenge to its
 *   interpretive monopoly; believers are beneficiaries of freed-up
 *   understanding. The constraint models the standing arrangement
 *   (pre-Reformation Church doctrine and authority structure) as the referent
 *   under contest, assessed by this reading's own lights (that the Church's
 *   teaching was doctrinally false). The periodization is tight: 1517 (95
 *   Theses) to 1555 (Peace of Augsburg establishing territorial settlement).
 *   This reading coexists with political_swap_reading (rulers used theology
 *   to seize power) and composite_overdetermination_reading (no single causal
 *   driver); the three readings are structurally distinct interpretations of
 *   the same historical event, not complementary perspectives on a unified
 *   phenomenon.
 *
 * KEY AGENTS:
 *   - Luther and the theological circle (moderate power, constrained exit) — formulate the reformed doctrine
 *   - Reformed believers (organized, mobile) — adopt and spread the new interpretation
 *   - Catholic Church institution (institutional power, trapped exit) — bears the cost of doctrinal refutation and authority loss
 *   - Secular rulers (powerful, arbitrage exit) — observe the doctrinal contest and calibrate political response
 *   - Scholastic defenders (powerful, trapped) — excluded from the reformed interpretive frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_event_boundary__theological_climb_reading, 0.38).
domain_priors:suppression_score(reformation_event_boundary__theological_climb_reading, 0.12).
domain_priors:theater_ratio(reformation_event_boundary__theological_climb_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, accessibility_collapse, 0.22).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__theological_climb_reading, rope).
narrative_ontology:human_readable(reformation_event_boundary__theological_climb_reading, "Reformation as Theological Doctrinal Breakthrough (Climb Reading)").
narrative_ontology:topic_domain(reformation_event_boundary__theological_climb_reading, "religious/epistemological/commitment-system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__theological_climb_reading, 'b7036a31-ef62-45e6-872b-a1fb9f47e0e6').
narrative_ontology:cs_kernel_codification('b7036a31-ef62-45e6-872b-a1fb9f47e0e6', fixed_text).
narrative_ontology:cs_authority_grounding('b7036a31-ef62-45e6-872b-a1fb9f47e0e6', lineage).
narrative_ontology:cs_interpretation_layer_present('b7036a31-ef62-45e6-872b-a1fb9f47e0e6').
narrative_ontology:cs_reading_relation('b7036a31-ef62-45e6-872b-a1fb9f47e0e6', reformation_event_boundary__political_swap_reading, coexists_with).
narrative_ontology:cs_reading_relation('b7036a31-ef62-45e6-872b-a1fb9f47e0e6', reformation_event_boundary__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('b7036a31-ef62-45e6-872b-a1fb9f47e0e6', foundational, scripture_doctrinally_supreme).
narrative_ontology:cs_axiom_status(scripture_doctrinally_supreme, holdable).
narrative_ontology:cs_axiom_grounding('b7036a31-ef62-45e6-872b-a1fb9f47e0e6', scripture_doctrinally_supreme, deontological).
narrative_ontology:cs_axiom('b7036a31-ef62-45e6-872b-a1fb9f47e0e6', foundational, justification_by_faith_alone_scriptural).
narrative_ontology:cs_axiom_status(justification_by_faith_alone_scriptural, holdable).
narrative_ontology:cs_axiom_grounding('b7036a31-ef62-45e6-872b-a1fb9f47e0e6', justification_by_faith_alone_scriptural, empirically_contingent).
narrative_ontology:cs_axiom('b7036a31-ef62-45e6-872b-a1fb9f47e0e6', secondary, doctrinal_truth_requires_institutional_separation).
narrative_ontology:cs_axiom_status(doctrinal_truth_requires_institutional_separation, holdable).
narrative_ontology:cs_axiom_grounding('b7036a31-ef62-45e6-872b-a1fb9f47e0e6', doctrinal_truth_requires_institutional_separation, deontological).
narrative_ontology:cs_reference_frame('b7036a31-ef62-45e6-872b-a1fb9f47e0e6', pre_reformation_church_doctrinal_monopoly).
narrative_ontology:cs_drift_state('b7036a31-ef62-45e6-872b-a1fb9f47e0e6', post_council_of_trent_1563, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('b7036a31-ef62-45e6-872b-a1fb9f47e0e6', '').
narrative_ontology:cs_kernel_id(reformation_event_boundary__theological_climb_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__theological_climb_reading, reformed_believers).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__theological_climb_reading, scriptural_interpretation_tradition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(reformation_event_boundary__theological_climb_reading, catholic_church_institutional).
narrative_ontology:constraint_vindicates(reformation_event_boundary__theological_climb_reading, justification_by_faith_doctrine).
narrative_ontology:constraint_vindicates(reformation_event_boundary__theological_climb_reading, biblical_supremacy_over_tradition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Luther and collaborators articulate the doctrine of justification by faith alone through textual study and theological disputation. They initiate the reinterpretation of scripture that becomes the movement's core claim. Their exit option is constrained by institutional affiliation (Augustinian order, Wittenberg university), but the theological claim itself faces no barrier to formulation. They frame their work as recovery, not innovation.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, luther_theological_circle, agenda_setter,
    moderate, biographical, constrained, regional).

% Believers adopt the reformed understanding of justification, interpreting their faith experience as freed from the burden of mechanical penance and intercessory works. They experience the theological shift as liberation from false doctrine. Their mobility increases because reformed communities offer an alternative institutional frame for worship and belief.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, reformed_believers, beneficiary,
    organized, generational, mobile, continental).

% The Church's theological monopoly on scriptural interpretation is contested and fractured. The institution must expend resources defending its doctrinal framework (Council of Trent, 1545-1563) against the reformed reading. The constraint extracts from the Church's authority position: its claim to represent true doctrine is now subject to contradiction from a coherent, textually grounded alternative. Institutional integrity is damaged by the necessity of refutation.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, catholic_church_institutional, payer,
    institutional, civilizational, trapped, universal).

% Princes and local authorities observe the theological contest; some adopt reformed doctrine for political advantage (asset seizure, reduced papal influence), others defend Catholicism for legitimacy reasons. From this reading's perspective, rulers are spectators to the doctrinal dispute; from the political_swap_reading, they are primary agents. Their seat divergence is structural.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, secular_rulers, observer,
    powerful, generational, arbitrage, regional).

% Scholastic theologians defending the synthesis of Aristotelian philosophy and Church doctrine are excluded from the reformed conversation: their interpretive framework is treated as fundamentally compromised. They would argue for the coherence of their tradition; their exclusion is structural to the reformed reading (which asserts their framework was a corruption, not a development).
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, scholastic_theology_defenders, excluded,
    powerful, biographical, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_event_boundary__theological_climb_reading, reformed_believers).
narrative_ontology:fixing_cost_class(reformation_event_boundary__theological_climb_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The reformed reading coordinates believers around a shared understanding of justification doctrine derived directly from scripture. The coordination problem solved: how to interpret Christian teaching in the face of competing institutional claims? The reformed answer is textual primacy and the doctrine of faith alone.
% TRANSFER_FUNCTION: The arrangement transfers interpretive authority from the institutional Church (which claimed monopoly on truth mediated by tradition and hierarchy) to the scripture-reading community (which claims direct textual access to truth). Believers transfer allegiance from Church-mediated salvation theology to faith-centered soteriology.
% ABSENT_VOICES: Jewish interpretive traditions, Islamic theological counterparts, and radical reform movements (Anabaptists) that would push beyond Luther's own position are excluded from this reading's frame. The Reformation is here bounded as a Christian intramural doctrinal dispute; voices questioning the scriptural foundation itself, or offering wholly alternative soteriologies, do not appear in the theological_climb_reading as primary participants.
% DISAPPEARANCE_RATIONALE: If the theological innovation had not occurred—if Luther's rediscovery of justification-by-faith doctrine had not taken hold—the Church's institutional monopoly on interpretation would likely have persisted longer, and the Reformation as a doctrinal event would not have occurred. However, the political_swap_reading asserts that secular pressures would have produced institutional fracture regardless; the theological reading treats the doctrinal breakthrough as the necessary condition for the specific institutional separation that followed. The verdict is contested because the causal architecture is disputed across readings.
% FOUNDING_PROBLEM: Christian believers faced conflicting interpretations of salvation doctrine: the Church taught justification through faith plus works (particularly sacraments, penance, indulgences); scripture read carefully appeared to teach justification by faith alone. The founding problem is how to resolve this textual-doctrinal contradiction.
% FOUNDING_PROBLEM_CORROBORATION: Reformed theologians and believers attested the doctrinal problem as live. The Council of Trent (1545-1563) responded by formally defending the Church's position, thereby conceding that the problem was a live doctrinal challenge, not a solved question. Modern historical theology outside the benefiting parties—secular historians, comparative religionists—attests that the textual problem (whether scripture supports works-plus-faith or faith-alone) is historically genuine and remains contested in contemporary scholarship. The founding problem is corroborated by the Church's own defensive response and by historians' independent analysis.
narrative_ontology:disappearance_verdict(reformation_event_boundary__theological_climb_reading, contested).
narrative_ontology:founding_problem_status(reformation_event_boundary__theological_climb_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_event_boundary__theological_climb_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reformation_event_boundary__theological_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_event_boundary__theological_climb_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_event_boundary__theological_climb_reading_tests).
:- end_tests(reformation_event_boundary__theological_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38 at interval end) is moderate because the constraint extracts from the Church's authority position (its claim to monopoly interpretation is contradicted) but does not physically coerce or economically dispossess Church members; it operates through doctrinal persuasion and institutional competition. Suppression is very low (0.12) because the reformed reading faces no structural barriers to expression—it spreads through textual argument, preaching, and theological disputation, not through coercion or censorship (the political reading involves such suppression; this reading does not). Theater ratio remains low (0.08) because the theological project is authentically about doctrine; performance is minimal in the interval 1517-1555. Accessibility collapse is low (0.22) because alternatives persist: scholastic defenders continue their work, Catholic doctrine is defended at Trent, and the interpretive contest remains open. Resistance is high (0.71) because the Church, Council, and scholastic tradition mount sustained intellectual and institutional defense. The measurement series show extractiveness and theater rising modestly and plateauing (the institutional settlement at 1555 and Trent closure), while suppression remains low—this profile is consistent with a genuine doctrinal contest, not an extraction mechanism requiring coercion.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (Catholic Church) and the beneficiary seats (reformed believers, scripture-centered tradition) compute entirely different constraint types from the SAME structural data. The Church experiences this as a loss of authority and must expend resources defending doctrine; the reformers experience it as a liberation into truth. The political_swap_reading would compute both as targets of a political extraction machine (rulers capturing assets). The engine computes per-seat types from power + exit + directionality; seat divergence here is maximal because the same institutional transformation (papal authority collapse, territorial Christendom fragmentation) is read as theological victory by reformers, institutional loss by the Church, and political advantage by rulers. The claim/metric independence rule is essential here: this reading is CLAIMED as rope (genuine coordination around scriptural interpretation) while the extractiveness metrics are modest—the divergence is not a defect, it is the measurement of how the theological reading differs structurally from the political reading (which would show higher extractiveness concentrated on the Church as a target of asset seizure).
 *
 * DIRECTIONALITY LOGIC:
 *   Luther's circle and the reformed believers are structural beneficiaries of the constraint: they gain interpretive authority, doctrinal coherence (from their perspective), and institutional space. Their d values are low (beneficiary end). The Catholic Church is the structural target: it loses its interpretive monopoly, must defend its doctrine, and bears the reputational cost of being positioned as doctrinally incorrect. Its d value is high (target end). Rulers are near-symmetric observers (d near 0.5) in this reading: they see a theological dispute unfold; the political_swap_reading would reposition them as primary actors (d shifting dramatically higher as they become targets of an extraction mechanism). The directionality divergence between readings is the central structural fact: the same historical event produces radically different seat positions depending on whether theology or politics is the causal driver.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids the mandatrophy trap by maintaining that the founding problem (scriptural interpretation of justification doctrine) is still live—the reformed doctrine remains contested in contemporary theology and scholarship, and believers continue to organize around the reformed teaching. The founding_problem_status: live assertion is key. If the status were 'dead,' the constraint would face a mandatrophy verdict: a doctrine that solved a problem that no longer exists, persisting through institutional inertia. But the theological_climb_reading posits that the doctrinal problem is not solved—the Church and reformed traditions continue to dispute justification theology—so the constraint (the reformed reading of scripture) remains functionally live, not theatrically maintained.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_interpretation_ambiguity,
    'Does scripture unambiguously support justification by faith alone, or is the reformed reading one defensible interpretation among others?',
    'Systematic exegesis of Pauline epistles and James using contemporary linguistic and historical-critical methods; comparison with Jewish interpretive parallels and Islamic theological sources; formal scholastic disputation between reformed and Catholic interpreters under conditions where neither side controls the adjudicating authority.',
    'If scripture clearly supports faith-alone, the reform is theological discovery; if the text is ambiguous, the reform is an interpretive innovation that legitimacy-depends on persuasion rather than textual truth. Either way, the extractiveness to the Church (loss of monopoly) is similar, but the classification as genuine coordination vs. power shift would shift.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(textual_interpretation_ambiguity, conceptual, 'Whether the reformed reading recovers objective scriptural meaning or instantiates a new, defensible-but-not-unique interpretation.').

omega_variable(
    causal_primacy_contest,
    'Was the institutional separation of the Reformation primarily caused by the theological innovation, or would political realignment have fractured Christendom even without Luther''s doctrinal breakthrough?',
    'Counterfactual historical analysis: comparison with other cases of theological innovation that did not produce institutional fracture (e.g., Abelard, Aquinas); analysis of secular rulers'' political incentives in 1517 independent of theological disputes; examination of whether the specific timing and geography of institutional separation correlates with doctrine or with political opportunity.',
    'If theology was necessary, the theological_climb_reading is structurally sound and extractiveness to the Church is a direct result of doctrinal correction. If political forces were sufficient, the political_swap_reading becomes primary and the theological reading becomes post-hoc narrative; extractiveness shifts from doctrine to political asset capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_primacy_contest, empirical, 'Whether theological innovation or political realignment was the structural causal driver of institutional separation.').

omega_variable(
    reformation_reading_foreclosure,
    'Does the theological_climb_reading logically foreclose the political_swap_reading, or do they coexist as distinct but compatible interpretations?',
    'Formal logical analysis of the axioms: does ''theology is the primary causal driver'' (theological reading) directly contradict ''politics is the primary causal driver'' (political reading)? Or are both claims compatible with a single historical record if weighted differently?',
    'If foreclosure obtains (one reading rules the other out), the two readings cannot coexist in any unified framework; if coexistence obtains, both readings remain live and the Reformation is genuinely overdetermined (supporting the composite_overdetermination_reading). The CS structure relation choice (forecloses vs. coexists_with) depends on this resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reformation_reading_foreclosure, conceptual, 'Logical relationship between the theological_climb and political_swap readings: do they foreclose each other or coexist?').

omega_variable(
    beneficiary_identity_in_doctrine,
    'Are the reformers'' and believers'' positions genuinely beneficiary (gaining access to truth, freed from false doctrine) or are they payers in a different extraction regime (paying the social cost of institutional fracture and religious war)?',
    'Post-Reformation trajectories: do reformed believers experience improved spiritual condition and reduced institutional coercion, or do they face new enforcement burdens (Protestant state churches, sectarian violence, loss of universal Christendom)? Do independent sources outside the reformed tradition attest benefit or cost?',
    'If beneficiaries genuinely benefit, the constraint is rope-like (coordination around truth). If reformed believers pay a net cost in social disruption and new coercion, the constraint shifts toward snare dynamics (extraction via doctrinal disruption). This affects the classification of the constraint from the believer seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identity_in_doctrine, empirical, 'Whether reformed believers are net beneficiaries of doctrinal innovation or net payers in institutional fragmentation.').

omega_variable(
    composite_reading_incompatibility,
    'Is the composite_overdetermination_reading logically incompatible with the theological_climb_reading, or does the composite reading merely add other causal factors without negating theology''s causal role?',
    'Logical analysis: does composite claim ''all four factors (theology, institutional collapse, politics, denominational emergence) were irreducibly overdetermined'' directly contradict theological claim ''theology was primary/necessary''? Or does composite merely weaken the theological claim from ''primary'' to ''one among several''?',
    'If incompatible, theological_climb forecloses composite. If compatible, both remain live—the theological reading asserts theology was necessary; the composite reading asserts it was insufficient without the others. This is the relation between these two readings in the cs_structure.reading_relations array.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(composite_reading_incompatibility, conceptual, 'Logical incompatibility between theological_climb_reading and composite_overdetermination_reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_event_boundary__theological_climb_reading, 0, 38).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t0, reformation_event_boundary__theological_climb_reading, theater_ratio, 0, 0.02).
narrative_ontology:measurement_basis(refo_tr_t0, observed).
narrative_ontology:measurement(refo_tr_t6, reformation_event_boundary__theological_climb_reading, theater_ratio, 6, 0.04).
narrative_ontology:measurement_basis(refo_tr_t6, observed).
narrative_ontology:measurement(refo_tr_t12, reformation_event_boundary__theological_climb_reading, theater_ratio, 12, 0.06).
narrative_ontology:measurement_basis(refo_tr_t12, observed).
narrative_ontology:measurement(refo_tr_t19, reformation_event_boundary__theological_climb_reading, theater_ratio, 19, 0.08).
narrative_ontology:measurement_basis(refo_tr_t19, observed).
narrative_ontology:measurement(refo_tr_t26, reformation_event_boundary__theological_climb_reading, theater_ratio, 26, 0.08).
narrative_ontology:measurement_basis(refo_tr_t26, observed).
narrative_ontology:measurement(refo_tr_t38, reformation_event_boundary__theological_climb_reading, theater_ratio, 38, 0.08).
narrative_ontology:measurement_basis(refo_tr_t38, observed).

% Extraction over time
narrative_ontology:measurement(refo_be_t0, reformation_event_boundary__theological_climb_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(refo_be_t0, observed).
narrative_ontology:measurement(refo_be_t6, reformation_event_boundary__theological_climb_reading, base_extractiveness, 6, 0.22).
narrative_ontology:measurement_basis(refo_be_t6, observed).
narrative_ontology:measurement(refo_be_t12, reformation_event_boundary__theological_climb_reading, base_extractiveness, 12, 0.28).
narrative_ontology:measurement_basis(refo_be_t12, observed).
narrative_ontology:measurement(refo_be_t19, reformation_event_boundary__theological_climb_reading, base_extractiveness, 19, 0.35).
narrative_ontology:measurement_basis(refo_be_t19, observed).
narrative_ontology:measurement(refo_be_t26, reformation_event_boundary__theological_climb_reading, base_extractiveness, 26, 0.38).
narrative_ontology:measurement_basis(refo_be_t26, observed).
narrative_ontology:measurement(refo_be_t38, reformation_event_boundary__theological_climb_reading, base_extractiveness, 38, 0.38).
narrative_ontology:measurement_basis(refo_be_t38, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(reformation_event_boundary__theological_climb_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_event_boundary__theological_climb_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(reformation_event_boundary__theological_climb_reading, 0.12).
narrative_ontology:affects_constraint(reformation_event_boundary__theological_climb_reading, reformation_event_boundary__political_swap_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__theological_climb_reading, reformation_event_boundary__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% The reformation_event_boundary kernel splits into three structurally distinct constraint stories: theological_climb_reading (this file) treats theology as primary causal driver and reads the Church as victim of doctrinal correction; political_swap_reading treats political realignment as primary and reads the Church as target of asset seizure; composite_overdetermination_reading asserts no single causal hierarchy. Each story has its own ε, beneficiary/victim structure, and timeline. They are linked as sibling readings of a single contested kernel, not as complementary perspectives on a unified constraint. The readings coexist as live positions in historical and theological scholarship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reformation_event_boundary__theological_climb_reading, institutional, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
