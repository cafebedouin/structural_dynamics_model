% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_obligation__messianic_suspension
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_obligation__messianic_suspension, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: temple_sacrifice_obligation__messianic_suspension
 *   human_readable: Temple Sacrifice Obligation Under Messianic Suspension
 *   domain: religious/halakhic
 *
 * SUMMARY:
 *   The Jewish tradition holds a Torah obligation to offer sacrifices in the
 *   Temple. After the Temple's destruction in 70 CE, this obligation became
 *   architecturally impossible to fulfill. The messianic_suspension reading
 *   interprets the obligation as genuinely suspended — neither fulfilled nor
 *   violated, but deferred to an eschatological future restoration. This is
 *   one of three structurally distinct readings of the same kernel
 *   (temple_sacrifice_obligation). Under this reading, study of sacrifice law
 *   is neither compliance nor preparation but maintenance of legal knowledge
 *   in a state of deferral. The constraint has near-zero extractiveness: no
 *   current obligation is imposed, no victim set is created, and authority is
 *   structured to defer rather than adjudicate. However, beneficiaries exist
 *   (the scholarly community and rabbinic authority structure), which
 *   triggers False Summit Mountain evaluation.
 *
 * KEY AGENTS:
 *   - jewish_diaspora_community: Identity-locked beneficiary (exempted from impossible obligation) — cannot exit religious law without abandoning self-concept
 *   - rabbinic_authority_structure: Agenda-setter (interpreting and deferring judgment) — institutional power to maintain suspension indefinitely
 *   - scholarly_community: Beneficiary and organized actor (derives legitimacy and career from sacrifice-law study) — professionally vested in study-obligation persistence
 *   - messianic_restorers: Excluded hypothetical agent (future authority that would resolve suspension) — present only as a name in the structure, not as a current actor
 *   - karaite_dissenters: Observer-position challengers (historically and contemporarily) — attest to the reading's contestation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__messianic_suspension, 0.08).
domain_priors:suppression_score(temple_sacrifice_obligation__messianic_suspension, 0.12).
domain_priors:theater_ratio(temple_sacrifice_obligation__messianic_suspension, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, extractiveness, 0.08).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__messianic_suspension, mountain).
narrative_ontology:human_readable(temple_sacrifice_obligation__messianic_suspension, "Temple Sacrifice Obligation Under Messianic Suspension").
narrative_ontology:topic_domain(temple_sacrifice_obligation__messianic_suspension, "religious/halakhic").

domain_priors:emerges_naturally(temple_sacrifice_obligation__messianic_suspension).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__messianic_suspension, '8b43c6ae-3125-4999-b5ca-f68ee90c3fcd').
narrative_ontology:cs_kernel_codification('8b43c6ae-3125-4999-b5ca-f68ee90c3fcd', fixed_text).
narrative_ontology:cs_authority_grounding('8b43c6ae-3125-4999-b5ca-f68ee90c3fcd', lineage).
narrative_ontology:cs_interpretation_layer_present('8b43c6ae-3125-4999-b5ca-f68ee90c3fcd').
narrative_ontology:cs_reading_relation('8b43c6ae-3125-4999-b5ca-f68ee90c3fcd', temple_sacrifice_obligation__study_as_occupation, coexists_with).
narrative_ontology:cs_reading_relation('8b43c6ae-3125-4999-b5ca-f68ee90c3fcd', temple_sacrifice_obligation__study_as_archiving, coexists_with).
narrative_ontology:cs_axiom('8b43c6ae-3125-4999-b5ca-f68ee90c3fcd', foundational, obligation_deferrable_by_external_cause).
narrative_ontology:cs_axiom_status(obligation_deferrable_by_external_cause, holdable).
narrative_ontology:cs_axiom_grounding('8b43c6ae-3125-4999-b5ca-f68ee90c3fcd', obligation_deferrable_by_external_cause, deontological).
narrative_ontology:cs_axiom('8b43c6ae-3125-4999-b5ca-f68ee90c3fcd', foundational, messianic_restoration_is_ultimate_arbiter).
narrative_ontology:cs_axiom_status(messianic_restoration_is_ultimate_arbiter, holdable).
narrative_ontology:cs_axiom_grounding('8b43c6ae-3125-4999-b5ca-f68ee90c3fcd', messianic_restoration_is_ultimate_arbiter, theological).
narrative_ontology:cs_reference_frame('8b43c6ae-3125-4999-b5ca-f68ee90c3fcd', temple_destroyed_obligation_suspended).
narrative_ontology:cs_drift_state('8b43c6ae-3125-4999-b5ca-f68ee90c3fcd', contemporary_2026, gap(stable, minor, true)).
narrative_ontology:cs_created_at('8b43c6ae-3125-4999-b5ca-f68ee90c3fcd', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__messianic_suspension, temple_sacrifice_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__messianic_suspension, scholarly_community).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__messianic_suspension, rabbinic_authority_structure).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__messianic_suspension, jewish_diaspora_community).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__messianic_suspension, messianic_deferral_doctrine).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__messianic_suspension, obligation_suspension_via_external_cause).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Distributed Jewish communities maintain religious identity and law observance across 2000 years without Temple or sacrifice. The messianic suspension framework permits this continuity: they are exempt from an obligation they cannot fulfill (geographically and architecturally impossible), avoiding either permanent violation or the claim that Jewish law is obsolete. Their identity is inseparable from Torah observance; exit would mean abandoning a foundational self-concept.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, jewish_diaspora_community, beneficiary,
    moderate, civilizational, identity_locked, global).

% Interprets and adjudicates halakhic status of suspended obligations. The messianic suspension reading grants them authority to defer resolution indefinitely: no obligation can be pronounced fulfilled or violated because the judgment belongs to messianic redemption, not to current rabbinic reasoning. They maintain the obligation's legal standing through study and textual transmission rather than through execution.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, rabbinic_authority_structure, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Gains professional and intellectual legitimacy from intensive study of sacrifice law and Temple procedure. The messianic suspension framework makes this study necessary and perpetual: knowledge must be preserved against the uncertain but anticipated future restoration. Scholarly careers, commentarial traditions, and institutional support rest on the ongoing study obligation.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, scholarly_community, beneficiary,
    organized, generational, mobile, global).

% A hypothetical future agent or event that would restore the Temple and resume sacrifice. They are not present in the constraint's current operation but are named as the ultimate authority that resolves the suspension. Their absence is structural to the suspension itself.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, messianic_restorers, excluded,
    powerless, civilizational, analytical, universal).

% Historical and contemporary challengers who reject the rabbinic reading and argue that obligation without execution is incoherent. They sit outside the mainstream reading but attest to the contested boundary between the three readings.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, karaite_dissenters, observer,
    moderate, civilizational, constrained, regional).

% Academic and philosophical analysts of religious law who examine whether the suspension is a coherent legal doctrine or a theodicy covering obligation-dissolution. They produce evidence for omega resolution without adjudicating the reading themselves.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, theological_interpreters, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_obligation__messianic_suspension, diffuse).
narrative_ontology:fixing_cost_class(temple_sacrifice_obligation__messianic_suspension, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Jewish diaspora religious identity across territorial and architectural impossibility: permits lawful non-performance of an ordinance that cannot be performed, avoiding both permanent violation and the claim that divine law is obsolete or context-dependent.
% TRANSFER_FUNCTION: No direct transfer. The constraint moves authority over obligation-judgment from individual compliance observation to deferred messianic adjudication. Scholarly labor is reorganized around preservation rather than execution.
% ABSENT_VOICES: The messianic restorer themselves — a hypothetical future agent whose voice would settle the obligation's status. Also absent: Jews who might have argued that obligation without execution is logically void and therefore Jewish law must be reformed; the suspension framework pre-empts that voice by denying that obligation-without-execution is incoherent.
% DISAPPEARANCE_RATIONALE: If the messianic suspension framework vanished and obligation were pronounced genuinely defunct, Jewish religious law would undergo foundational revision: either the obligation is reinterpreted (study_as_occupation reading) or obligation doctrine itself becomes historically contingent rather than timelessly binding. If the suspension vanished and obligation were pronounced currently binding despite architectural impossibility, diaspora Jewish communities would face a permanent violation state. Either outcome reorganizes religious authority and community self-understanding.
% FOUNDING_PROBLEM: After the destruction of the Second Temple (70 CE), Jews maintained diaspora communities unable to perform Temple sacrifice. The obligation to offer sacrifice was biblically mandated but architecturally impossible. The founding problem was maintaining coherent religious law and identity across this rupture.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic sources from the Talmud forward (Gemara, Rosh, Maimonides, and subsequent codes) all attest the problem remains live: the Temple is not rebuilt, the obligation stands textually, and the framework for deferring judgment persists. The problem's liveness is corroborated by the continuous scholarly tradition itself — if the problem were resolved, the tradition would have concluded and ceased producing new commentary. Karaite dissidents and modern academic theologians attest the founding problem as unresolved and contested.
narrative_ontology:disappearance_verdict(temple_sacrifice_obligation__messianic_suspension, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_obligation__messianic_suspension, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__messianic_suspension, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(temple_sacrifice_obligation__messianic_suspension, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_obligation__messianic_suspension_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, ExtMetricName, E),
    domain_priors:suppression_score(temple_sacrifice_obligation__messianic_suspension, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(temple_sacrifice_obligation__messianic_suspension),
    narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(temple_sacrifice_obligation__messianic_suspension_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.08 at interval end) because no current obligation is imposed and no systematic transfer occurs. The identity-locked exit of the diaspora community means they have no practical choice but to accept the framework; this feeds low effective extraction (the extraction formula modulates for trapped/identity-locked targets, but here the target bears no active cost). Suppression is minimal (0.12) because the constraint operates by deferring judgment, not by coercing compliance or preventing resistance. Theater ratio (0.45) reflects the increasing proportion of scholarly activity dedicated to ritual-legal precision and textual exhaustiveness, relative to the practical binding-authority of the studies — the activity becomes more elaborate and performative over the interval as the practical distance from restoration increases. Accessibility_collapse is very high (0.92) because the constraint rests on a claim that appears to natural law: the Temple is destroyed, restoration is eschatological, therefore the obligation is suspended. This collapse is not enforced suppression but rather the logical structure of the deferral itself. Resistance is minimal (0.05) because diaspora communities have no practical resistance to an exemption, and dissenters (Karaites, reformers) remain marginal to the mainstream tradition.
 *
 * PERSPECTIVAL GAP:
 *   All seats should perceive similar constraint structure because no one bears active cost and judgment is deferred. The engine should compute similar types across seats. Divergence would appear only if a dissenting reading (study_as_occupation or study_as_archiving) replaces this one — each reading is a separate constraint story. Within the messianic_suspension reading, the gap should be minimal.
 *
 * DIRECTIONALITY LOGIC:
 *   The diaspora community is structurally beneficiary (exempted from an impossible obligation, protected from permanent violation status). The scholarly community is beneficiary (derives institutional legitimacy and career from perpetual study). The rabbinic authority structure is both agenda-setter and partial beneficiary (monopolizes interpretation, defers resolution, maintains institutional authority). No seat is a victim: no one is forced into a transfer against their interest. This is why extractiveness is low. However, the question whether this is a genuine deferral or a dissolution-with-cover-story is unresolved (omega_1), which means the beneficiary structure could reclassify if the reading is reframed.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy is present in this reading. The founding obligation (to offer sacrifice) remains textually binding and the framework explicitly defers its status rather than declaring it obsolete. The suspension is a live doctrinal commitment, not an atrophied institutional function. The founding problem (Temple absent, obligation present) is still live and the framework addresses it coherently within its own terms. However, omega_1 flags the possibility that suspension is functionally dissolved and the mandatrophy is hidden — that is, the obligation's mandate has outlived its function (resumed practice would require rabbinical re-institution, not automatic resumption), but the framework denies it. This is a concealed-mandatrophy reading, not a present one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suspension_vs_dissolution_boundary,
    'Is the messianic suspension a genuine deferral of obligation-judgment, or is it functionally equivalent to obligation-dissolution with a theological cover story?',
    'Test via counterfactual: if messianic restoration occurred and Temple sacrifice resumed, would the obligation resume as binding, or would the 1900+ years of non-performance constitute a break in the law''s continuity such that resumption would require fresh rabbinical institution? If resumption would be automatic and binding, suspension is genuine deferral; if rabbinical re-institution would be required, the suspension functionally dissolved the obligation and cover is the narrative.',
    'If dissolution-cover, the constraint reclassifies from mountain (natural law suspended by external cause) to tangled_rope or snare (authority structure benefiting from narrative maintenance). The beneficiary structure changes from community-protection to authority-control.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suspension_vs_dissolution_boundary, conceptual, 'Whether suspension is genuine deferral or functional dissolution with theological framing.').

omega_variable(
    false_summit_beneficiary_scrutiny,
    'Does the scholarly community and rabbinic authority structure benefit materially from maintaining the suspension indefinitely, such that their structural interest in deferral exceeds the community''s interest in exemption?',
    'Comparative institutional analysis: measure scholarly career advancement and institutional prestige dependent on sacrifice-law study; measure rabbinic authority''s monopoly on obligation-interpretation; compare to the counterfactual — if obligation were pronounced defunct or study_as_occupation reading prevailed, how would institutions reorganize? Also: interview or textual analysis of dissenters (Karaites, modern reformers) who argue the suspension benefits authorities more than community.',
    'Evidence of concentrated institutional benefit would support FSM classification (mountain that appears natural but benefits identifiable agents). The messianic suspension would reclassify to tangled_rope (coordination function + asymmetric extraction). Current omega is high-confidence that the problem exists; resolution mechanism is medium-confidence empirically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_beneficiary_scrutiny, empirical, 'Whether institutional beneficiaries extract rent from indefinite suspension maintenance.').

omega_variable(
    reading_committer_frame_dependence,
    'The messianic_suspension reading is ONE OF THREE structurally distinct readings of the temple_sacrifice_obligation kernel. Does choosing this reading depend on the reader''s theological or institutional position (committer frame), rather than on constraint-structure alone?',
    'Textual history and community survey: trace which institutional positions (rabbinic mainstream, Karaite, Kaplan Reconstructionist, Orthodox literalist, secular academic, Israeli Haredi rebuilders) hold which reading. If reading choice correlates with institutional position rather than with independent structural analysis, the readings are committer-frame dependent. Map the dependency graph (Karaites -> study_as_archiving; Haredi -> study_as_occupation; mainstream diaspora -> messianic_suspension; secular -> dissolution-skeptics).',
    'If committer-frame dependent, the three readings may be incommensurable rather than genuinely alternative -- each reading the kernel from its own authority-grounding standpoint. The engine''s per-seat computation would then show radical divergence (each seat perceives a different constraint structure). This is not an error; it is evidence that the kernel is genuinely contested and no frame-independent reading exists.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_committer_frame_dependence, conceptual, 'Whether reading choice depends on the reader''s institutional position rather than on constraint-independent structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__messianic_suspension, 70, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t70, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 70, 0.35).
narrative_ontology:measurement_basis(temp_tr_t70, observed).
narrative_ontology:measurement(temp_tr_t500, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 500, 0.38).
narrative_ontology:measurement_basis(temp_tr_t500, observed).
narrative_ontology:measurement(temp_tr_t1000, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 1000, 0.4).
narrative_ontology:measurement_basis(temp_tr_t1000, observed).
narrative_ontology:measurement(temp_tr_t1500, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 1500, 0.42).
narrative_ontology:measurement_basis(temp_tr_t1500, observed).
narrative_ontology:measurement(temp_tr_t1900, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 1900, 0.44).
narrative_ontology:measurement_basis(temp_tr_t1900, observed).
narrative_ontology:measurement(temp_tr_t2026, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 2026, 0.45).
narrative_ontology:measurement_basis(temp_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(temp_be_t70, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 70, 0.05).
narrative_ontology:measurement_basis(temp_be_t70, observed).
narrative_ontology:measurement(temp_be_t500, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 500, 0.06).
narrative_ontology:measurement_basis(temp_be_t500, observed).
narrative_ontology:measurement(temp_be_t1000, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 1000, 0.07).
narrative_ontology:measurement_basis(temp_be_t1000, observed).
narrative_ontology:measurement(temp_be_t1500, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 1500, 0.08).
narrative_ontology:measurement_basis(temp_be_t1500, observed).
narrative_ontology:measurement(temp_be_t1900, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 1900, 0.08).
narrative_ontology:measurement_basis(temp_be_t1900, observed).
narrative_ontology:measurement(temp_be_t2026, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 2026, 0.08).
narrative_ontology:measurement_basis(temp_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t70, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 70, 0.1).
narrative_ontology:measurement_basis(temp_su_t70, observed).
narrative_ontology:measurement(temp_su_t500, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 500, 0.11).
narrative_ontology:measurement_basis(temp_su_t500, observed).
narrative_ontology:measurement(temp_su_t1000, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 1000, 0.11).
narrative_ontology:measurement_basis(temp_su_t1000, observed).
narrative_ontology:measurement(temp_su_t1500, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 1500, 0.12).
narrative_ontology:measurement_basis(temp_su_t1500, observed).
narrative_ontology:measurement(temp_su_t1900, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 1900, 0.12).
narrative_ontology:measurement_basis(temp_su_t1900, observed).
narrative_ontology:measurement(temp_su_t2026, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 2026, 0.12).
narrative_ontology:measurement_basis(temp_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_obligation__messianic_suspension, identity_coordination).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_obligation__messianic_suspension, 0.06).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__messianic_suspension, temple_sacrifice_obligation__study_as_occupation).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__messianic_suspension, temple_sacrifice_obligation__study_as_archiving).

% DUAL FORMULATION NOTE:
% The temple_sacrifice_obligation kernel admits three structurally distinct readings, each with different ε and beneficiary structure. The messianic_suspension reading (this constraint) defers judgment to eschatological restoration; study_as_occupation reads study as legitimate occupation of the obligation; study_as_archiving reads study as knowledge-preservation without obligation-fulfillment. These are not alternative measurements of one constraint — they are three separate constraints linked by shared kernel. Per the ε-invariance principle (DP-001), each reading instantiates a different constraint with a different structural ε. The three files form a constraint family linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
