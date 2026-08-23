% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__performance_only_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__performance_only_reading, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: sacrifice_obligation_kernel__performance_only_reading
 *   human_readable: Sacrificial Command as Standing Statute — Performance-Only Reading (Post-Temple Obligation Gap)
 *   domain: religious law / halakhic authority / commitment system dynamics
 *
 * SUMMARY:
 *   Under the performance reading, the Torah's sacrificial commandments
 *   remain fully binding statutes: physical offering is the sole mode of
 *   discharge, and study equips, commemorates, and preserves but does not
 *   stand in the place of bringing the offering. With the Temple destroyed in
 *   70 CE, the entire covenant population has lived roughly nineteen
 *   centuries under commandments it cannot carry out. The arrangement
 *   persists through codification (the sacrificial orders legislated 'for the
 *   day it is restored'), pedagogy, thrice-daily restoration petitions,
 *   commemorative reenactment, and concrete preparation work; no human party
 *   receives the undischarged debt, which the tradition locates in the
 *   covenantal ledger itself. KEY AGENTS (by structural relationship): see
 *   key_agents. This file fixes epsilon to THIS reading only; sibling
 *   readings are separate constraint files linked through the network block,
 *   and nothing here averages across them. The claim/metric gap is
 *   deliberate: the constraint is CLAIMED as piton (inertial persistence, no
 *   capturer, mostly maintenance and commemoration) while the metrics
 *   independently describe heavily burdensome, continuously enforced
 *   operation — the engine measures the divergence.
 *
 * KEY AGENTS:
 *   - halakhic_authority_apex: agenda-setting administrator (institutional/identity_locked) — rules the obligation standing, teaches study as preparation, and is itself bound by the same commandments
 *   - torah_observant_jews: primary bearer population (moderate/constrained) — commanded, unable to perform, carries the open debt in liturgy and conscience
 *   - kohen_lineages: double-positioned bearer (moderate/constrained) — preparation duties without office, prospective restored status
 *   - secular_jewish_identifiers: latent bearer (moderate/mobile) — counted among the commanded by the tradition's own terms, largely unengaged
 *   - rabbinic_pedagogic_establishment: incidental collector (organized/mobile) — curriculum centrality and professional niche, without receiving the debt
 *   - temple_preparatory_institutes: incidental collector (organized/constrained) — mission warrant and funding drawn from the obligation's persistence
 *   - temple_mount_custodial_regime: external veto holder (powerful/constrained) — controls the ground any performance would require, absent from the doctrinal conversation
 *   - karaite_traditionalists and reform_liturgical_committees: excluded dissenters (organized) — contest bindingness and restoration from outside the frame
 *   - comparative_ritual_scholarship: analytical observer (analytical/analytical) — records the arrangement's continuity from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__performance_only_reading, 0.85).
domain_priors:suppression_score(sacrifice_obligation_kernel__performance_only_reading, 0.7).
domain_priors:theater_ratio(sacrifice_obligation_kernel__performance_only_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__performance_only_reading, piton).
narrative_ontology:human_readable(sacrifice_obligation_kernel__performance_only_reading, "Sacrificial Command as Standing Statute — Performance-Only Reading (Post-Temple Obligation Gap)").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__performance_only_reading, "religious law / halakhic authority / commitment system dynamics").

domain_priors:requires_active_enforcement(sacrifice_obligation_kernel__performance_only_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__performance_only_reading, 'dc3f1a8e-9f28-4146-9c52-e2c6fd3f0f49').
narrative_ontology:cs_kernel_codification('dc3f1a8e-9f28-4146-9c52-e2c6fd3f0f49', fixed_text).
narrative_ontology:cs_authority_grounding('dc3f1a8e-9f28-4146-9c52-e2c6fd3f0f49', lineage).
narrative_ontology:cs_interpretation_layer_present('dc3f1a8e-9f28-4146-9c52-e2c6fd3f0f49').
narrative_ontology:cs_reading_relation('dc3f1a8e-9f28-4146-9c52-e2c6fd3f0f49', sacrifice_obligation_kernel__study_as_exercise_reading, coexists_with).
narrative_ontology:cs_reading_relation('dc3f1a8e-9f28-4146-9c52-e2c6fd3f0f49', sacrifice_obligation_kernel__messianic_suspension_reading, coexists_with).
narrative_ontology:cs_reading_relation('dc3f1a8e-9f28-4146-9c52-e2c6fd3f0f49', sacrifice_obligation_kernel__symbolic_archive_reading, forecloses).
narrative_ontology:cs_axiom('dc3f1a8e-9f28-4146-9c52-e2c6fd3f0f49', foundational, sacrifice_performance_required_for_discharge).
narrative_ontology:cs_axiom_status(sacrifice_performance_required_for_discharge, holdable).
narrative_ontology:cs_axiom_grounding('dc3f1a8e-9f28-4146-9c52-e2c6fd3f0f49', sacrifice_performance_required_for_discharge, theological).
narrative_ontology:cs_axiom('dc3f1a8e-9f28-4146-9c52-e2c6fd3f0f49', foundational, study_preparatory_not_substitutive).
narrative_ontology:cs_axiom_status(study_preparatory_not_substitutive, holdable).
narrative_ontology:cs_axiom_grounding('dc3f1a8e-9f28-4146-9c52-e2c6fd3f0f49', study_preparatory_not_substitutive, conventional).
narrative_ontology:cs_reference_frame('dc3f1a8e-9f28-4146-9c52-e2c6fd3f0f49', statutory_binding_command_awaiting_restored_performance).
narrative_ontology:cs_drift_state('dc3f1a8e-9f28-4146-9c52-e2c6fd3f0f49', contemporary_nineteen_centuries_post_destruction, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('dc3f1a8e-9f28-4146-9c52-e2c6fd3f0f49', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__performance_only_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__performance_only_reading, rabbinic_pedagogic_establishment).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__performance_only_reading, temple_preparatory_institutes).
narrative_ontology:constraint_victim(sacrifice_obligation_kernel__performance_only_reading, torah_observant_jews).
narrative_ontology:constraint_victim(sacrifice_obligation_kernel__performance_only_reading, kohen_lineages).
narrative_ontology:constraint_victim(sacrifice_obligation_kernel__performance_only_reading, secular_jewish_identifiers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__performance_only_reading, kohen_lineages).
narrative_ontology:constraint_victim(sacrifice_obligation_kernel__performance_only_reading, halakhic_authority_apex).
narrative_ontology:constraint_victim(sacrifice_obligation_kernel__performance_only_reading, rabbinic_pedagogic_establishment).
narrative_ontology:constraint_victim(sacrifice_obligation_kernel__performance_only_reading, temple_preparatory_institutes).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__performance_only_reading, obligation_binding_independent_of_capability).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__performance_only_reading, study_as_preparation_doctrine).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__performance_only_reading, temple_service_restoration_expectancy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Codifies and adjudicates the sacrificial commandments as standing law: the great codes enumerate the offerings, the Mishneh Torah's service volumes specify procedure for the day restoration comes, and responsa rule on whether any substitute counts. Teaches that study equips and preserves but does not occupy the place of bringing the offering, and rules rival formulations out of bounds. Every member of this bench is himself bound by the same commandments and grants no exemption to himself; abandoning the framework would mean ceasing to be its authority, so departure is not a live option from inside.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, halakhic_authority_apex, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_kernel__performance_only_reading, halakhic_authority_apex, payer).

% Yeshiva networks, publishers, and lecture circuits devote whole tractates, curricula, and commentary series to the sacrificial orders. The commandments' persistence keeps these materials centrally placed in the syllabus and gives specialists a durable professional niche. Teachers recite the restoration petitions themselves; a school could in principle reweight its curriculum and some smaller ones have drifted toward other emphases, so mobility exists but carries reputational cost inside the observant world.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, rabbinic_pedagogic_establishment, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_kernel__performance_only_reading, rabbinic_pedagogic_establishment, payer).

% Jerusalem-centered organizations train priests-in-waiting, fabricate vessels and garments to specification, and raise funds on the premise that performance may again become possible. The standing commandment is the warrant for their mission; fulfilled performance would complete their work rather than abolish it. Staff and donors are themselves bound worshippers, and exit is bounded by mission identity and committed resources.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, temple_preparatory_institutes, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_kernel__performance_only_reading, temple_preparatory_institutes, payer).

% Recite the sacrificial passages and restoration petitions in daily and festival liturgy, fund study of the relevant tractates, and live under commandments they currently cannot carry out. Leaving the covenant community altogether is possible but severs family, community, and self-understanding at once; remaining means carrying an obligation whose discharge date is unknown.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, torah_observant_jews, payer,
    moderate, biographical, constrained, global).

% Families of priestly descent maintain purity precautions, genealogical records, and readiness expectations on the strength of a future service, while today having no forum in which descent confers function. Restoration would restore standing and perquisites; until then the lineage carries duties of preparation without office.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, kohen_lineages, payer,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_kernel__performance_only_reading, kohen_lineages, beneficiary).

% Counted by the tradition's own terms among those commanded, though most neither study the sacrificial orders nor petition for restoration, encountering the material mainly at the Passover table or in heritage education. Disaffiliation and assimilation are available at moderate cost, and many exercise them partially, keeping identity while ignoring the demand.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, secular_jewish_identifiers, payer,
    moderate, biographical, mobile, global).

% Descendants of the medieval movement that rejected the oral interpretive layer read scripture without the rabbinic edifice and dispute the whole apparatus by which the performance reading is sustained. They stand outside the conversation that defines the obligation and would contest its terms at the root if admitted.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, karaite_traditionalists, excluded,
    organized, generational, constrained, regional).

% Nineteenth-century liturgical reformers struck the restoration-of-sacrifice petitions from the prayer book and repudiated the expectation of renewed offerings. Their successor bodies hold the sacrificial commandments historically superseded, argue this openly in their own institutions, and have no seat in the halakhic process where the performance reading is maintained.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, reform_liturgical_committees, excluded,
    organized, biographical, mobile, continental).

% The governing arrangements over Jerusalem's Temple Mount — custodianship, access rules, policing — determine whether any performance could physically occur. Administrators deliberate in diplomatic and security forums, not halakhic ones; they would object to any attempt at renewed offering and currently prevent it, yet they never enter the legal conversation that declares the obligation standing.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, temple_mount_custodial_regime, excluded,
    powerful, generational, constrained, regional).

% Academic historians and anthropologists of religion document the sacrificial systems of the ancient Near East, the destruction of 70 CE, and the subsequent literary afterlife of sacrificial law. They take no position on bindingness, publish outside the community, and serve as the record-keeping seat from which the arrangement's continuity can be seen whole.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, comparative_ritual_scholarship, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_kernel__performance_only_reading, diffuse).
narrative_ontology:fixing_cost_class(sacrifice_obligation_kernel__performance_only_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps a dispersed covenant community aligned around an intact command-system across a long performance gap: preserves exact procedural knowledge of the offerings, synchronizes liturgy and calendar around anticipated restoration, and maintains public affirmation that the commandments remain in force so the covenant reads as unbroken despite the destroyed center.
% TRANSFER_FUNCTION: Draws study-time, liturgical attention, commemorative practice, and preparation labor from every covenant-bound Jew toward maintaining an undischarged divine claim; no human seat receives the debt itself. Incidentally, curricular centrality and mission funding flow to teaching networks and preparation institutes.
% ABSENT_VOICES: Karaite traditionalists, who reject the interpretive apparatus sustaining the reading; Reform liturgical authorities, who repudiated the restoration expectation a century and a half ago; and the Temple Mount custodial powers, whose consent any actual performance would require and who sit wholly outside the halakhic conversation declaring the obligation standing. Each would object at a different joint — bindingness, restoration, or site access — and none has a seat.
% DISAPPEARANCE_RATIONALE: If the performance reading dissolved overnight — if the commandments ceased to bind as statutes awaiting offering — the restoration petitions anchoring the thrice-daily liturgy would lose their object, the sacrificial orders would collapse as a living curriculum, priestly lineages would lose their deferred office, preparation institutes would lose their warrant, and the messianic hope-structure built around resumed service would require wholesale rewriting. The liturgical and educational architecture of observant Judaism would rearrange around the loss.
% FOUNDING_PROBLEM: After 70 CE the covenant community possessed binding commandments whose appointed mode of discharge no longer existed. This reading's answer — hold the obligation intact, prepare, await restored capacity — was consolidated across the rabbinic period and the codifications to keep the command-system unbroken across the gap.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the burden-bearing public itself has recited restoration petitions three times daily for centuries, practice attesting the problem as live; medieval Karaite polemics attest the reading and its stakes from opposition; and academic historiography of halakha documents the continuity of the performance-framed obligation from the post-destruction academy through the medieval codes. No reliance is placed on the teaching establishment's own testimony.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__performance_only_reading, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__performance_only_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__performance_only_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_obligation_kernel__performance_only_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__performance_only_reading, 0.85, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__performance_only_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_obligation_kernel__performance_only_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_obligation_kernel__performance_only_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85 at interval end) because a binding command with zero discharge channels has operated on the whole covenant population for nineteen centuries — the referent is the standing arrangement assessed by this reading's own lights, in which the obligation is real, total, and currently unmeetable. Suppression (0.70) is a raw structural property, unscaled by power or scope in the engine's computation: it is carried by hermeneutic closure (rival readings ruled invalid), communal sanction, and the identity cost of exit, not by physical coercion. Theater ratio (0.60) reflects the volume of commemorative simulation — elaborate Day of Atonement service recitations, sacrificial passages in the daily liturgy, Passover table memorials — against genuinely preparatory work (priestly training, vessel fabrication); under this reading the preparatory fraction is real but minority. Accessibility collapse is low-moderate (0.40) because alternatives remain visible and arguable: sibling readings are live elsewhere, and disaffiliation is possible at known cost. Resistance (0.35) captures organized dissent at the margins — the medieval rejection of the interpretive layer, the nineteenth-century liturgical excisions — plus mass passive non-engagement, with little organized resistance inside the frame itself, where this reading was long the unmarked default. Requires_active_enforcement is true: without continuous teaching, liturgical rehearsal, and boundary-policing, the arrangement demonstrably decays toward heritage-only framings within generations. The enforcement series oscillates rather than trending monotonically: hardening after schisms (the eighth-to-tenth-century consolidation against scripturalist dissent), loosening in the emancipation era, re-hardening with nineteenth-century neo-traditionalist reaction and twentieth-century religious nationalism — periodic heresy shocks re-consolidate the frame, a mild intermittent-reinforcement effect noted as a secondary maintenance mechanism. All three series run on one shared ten-point grid; every tracked metric is authored at every examined time point, and endpoint values equal the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats compute a heavy, uncollected-for burden: commanded, unable to perform, with the compensation flowing to no one. The pedagogic and institute seats compute light, incidental collection — standing, students, mission funding — without ever touching the debt itself. The anomalous seat is the agenda-setter: unlike captured regulators, the halakhic apex co-bears the very obligation it administers and cannot exempt itself, so its computed position mixes administrator relief with bearer load. The excluded custodial seat holds the physical veto over fulfillment while holding no doctrinal voice — power without standing in the conversation that defines the demand. The diffuse bearer population has weak coalition incentives: the burden is experienced as devotion rather than injury, which is precisely why no fixing coalition forms despite enormous aggregate load.
 *
 * DIRECTIONALITY LOGIC:
 *   Victim declarations drive the bearer seats toward the full-target end: observant Jews, priestly lineages, and latently the unengaged majority all face d near 1, with constrained (not trapped, not arbitrage-grade) exit keeping them short of full lock except where identity fusion binds. The two declared beneficiaries collect incidentally — curriculum centrality, professional niche, mission warrant — placing them low on d, but the extraction's receipt is nonetheless diffuse: re-reading every stakeholder situation, no seat accrues the undischarged debt itself, which the tradition assigns to the covenantal ledger; receipt-of-gain and beneficiary-role therefore come apart in this story, and gain_flow is authored as the affirmative checked claim 'diffuse'. Two overrides correct role-based derivations that would misread this arrangement: the institutional seat is overridden to d=0.70 because an administrator role normally derives beneficiary-ward while this administrator co-bears the command (identity_locked, no self-exemption); the organized band is overridden to d=0.40 because its members mix incidental collection with personal obligation, a blend the bare role labels cannot express. The override's coarseness is acknowledged: excluded dissenters sharing the organized atom inherit a near-neutral value they did not earn — they sit outside the arrangement's flows entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — relating faithfully to commandments whose discharge channel no longer exists — is live by this reading's own design: the arrangement's function is precisely to hold the obligation intact across the gap, so no mandatrophy resolution is declared and the founding_problem_status is authored 'live'. The classification guards against two symmetric errors. Reading the absence of a capturer as absence of burden would miss that the load is real, universal, and unpaid — diffuse, not absent; reading nineteen centuries of persistence as natural fixity would miss that the arrangement is continuously maintained by pedagogy, liturgy, and boundary-policing and would decay without them. The status-live x world-rearranges pairing predicts no zombie flag from the mismatch consumer, correctly: the arrangement's persistence tracks a problem its holders insist is unresolved, not a corpse administered for show. The theater ratio's rise is the honest symptom of gradual ossification inside a live mandate — commemorative volume growing faster than preparatory substance — which is the drift vector to watch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_indexicality,
    'This story measures one reading (performance_only) of the sacrifice_obligation_kernel; how much of the measured burden is indexical to this reading rather than to the kernel itself?',
    'Cross-reading corpus comparison: compile the three sibling stories and diff epsilon, victim sets, and persistence profiles under identical engine settings.',
    'If a sibling reading displaces this one communally, the measured burden migrates to that sibling''s file and this constraint''s effective load converges on the sibling''s profile (near-zero for the archive reading, transformed for the study-as-exercise reading).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_indexicality, conceptual, 'Reading-indexed measurement over a shared kernel: epsilon here belongs to this instantiation only.').

omega_variable(
    decree_vs_social_maintenance,
    'Is the obligation''s persistence a fact that would hold without any human maintenance (divine decree, mountain-like) or a construct kept in place by continuous transmission, pedagogy, and boundary-policing?',
    'Counterfactual transmission test: track whether communities that cease teaching the sacrificial orders and drop the liturgical material retain the performance reading after two generations (observed drift toward heritage-only framings in assimilated populations suggests maintenance-dependence).',
    'A decree-fact finding would support mountain-side certification (an emerges-naturally analogue); confirmed maintenance-dependence validates the piton persistence mechanics authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decree_vs_social_maintenance, conceptual, 'Natural-fixity versus maintained-construct ambiguity in the obligation''s persistence.').

omega_variable(
    restoration_plausibility_gap,
    'Is the performance gap permanent, or contingent on geopolitical and practical change (site access, cult-site reconstruction, priestly readiness)?',
    'Scenario tracking of Temple Mount custody regimes and restoration movements'' capability milestones (vessel fabrication, red-heifer status, trained personnel).',
    'Closing contingency converts the open debt into an actionable one and drives extractiveness upward along an accumulation trajectory; perceived permanence ossifies the arrangement toward pure inertia.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restoration_plausibility_gap, empirical, 'Permanence versus contingency of the command-capacity gap.').

omega_variable(
    obligation_scope_boundary,
    'Whom does the binding reach — every Jew by covenant membership (the classical doctrine) or only those who accept the community''s normative framework?',
    'Doctrinal analysis of obligation-versus-acceptance in the codes, combined with survey data on how unengaged populations acknowledge or experience the demand.',
    'Wide scope multiplies the bearer population enormously while diluting per-capita felt burden; narrow scope concentrates the load on the observant (higher per-seat burden, far smaller victim set). Classification of individual seats shifts accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(obligation_scope_boundary, conceptual, 'Boundary of the victim set under the performance reading.').

omega_variable(
    preparatory_vs_discharge_boundary,
    'Within this reading, does study carry any discharge value while performance is impossible (the Talmudic ''as if he offered it'' line), or is its value strictly instrumental preparation?',
    'Close reading of the limiting conditions the tradition places on that passage and of codified practice: does any authority treat study as satisfying an owed offering?',
    'Any accepted discharge value drops measured extractiveness substantially and drifts this constraint toward the study_as_exercise sibling''s profile — the sharpest intra-reading lever on epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preparatory_vs_discharge_boundary, conceptual, 'Where the preparatory/not-substitutive line actually sits inside this reading.').

omega_variable(
    impossibility_attribution,
    'How much of the impossibility is internal to the command''s terms versus externally imposed by site custody and great-power politics?',
    'Counterfactual site-access analysis: if the platform were open tomorrow, could compliant performance actually resume (purification status, altar specifications, priestly readiness)?',
    'External attribution shifts fixing cost onto outside parties and reframes persistence as hostage to diplomacy; internal attribution keeps responsibility inside the interpretive system and preserves the prohibitive fixing-cost cell.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(impossibility_attribution, empirical, 'Internal versus external sourcing of the performance gap.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__performance_only_reading, 0, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement_basis(sacr_tr_t0, observed).
narrative_ontology:measurement(sacr_tr_t200, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 200, 0.36).
narrative_ontology:measurement_basis(sacr_tr_t200, observed).
narrative_ontology:measurement(sacr_tr_t500, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 500, 0.42).
narrative_ontology:measurement_basis(sacr_tr_t500, observed).
narrative_ontology:measurement(sacr_tr_t800, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 800, 0.48).
narrative_ontology:measurement_basis(sacr_tr_t800, observed).
narrative_ontology:measurement(sacr_tr_t1100, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 1100, 0.52).
narrative_ontology:measurement_basis(sacr_tr_t1100, observed).
narrative_ontology:measurement(sacr_tr_t1400, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 1400, 0.55).
narrative_ontology:measurement_basis(sacr_tr_t1400, observed).
narrative_ontology:measurement(sacr_tr_t1650, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 1650, 0.57).
narrative_ontology:measurement_basis(sacr_tr_t1650, observed).
narrative_ontology:measurement(sacr_tr_t1800, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 1800, 0.58).
narrative_ontology:measurement_basis(sacr_tr_t1800, observed).
narrative_ontology:measurement(sacr_tr_t1900, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 1900, 0.59).
narrative_ontology:measurement_basis(sacr_tr_t1900, observed).
narrative_ontology:measurement(sacr_tr_t1950, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 1950, 0.6).
narrative_ontology:measurement_basis(sacr_tr_t1950, observed).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 0, 0.8).
narrative_ontology:measurement_basis(sacr_be_t0, observed).
narrative_ontology:measurement(sacr_be_t200, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 200, 0.72).
narrative_ontology:measurement_basis(sacr_be_t200, observed).
narrative_ontology:measurement(sacr_be_t500, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 500, 0.66).
narrative_ontology:measurement_basis(sacr_be_t500, observed).
narrative_ontology:measurement(sacr_be_t800, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 800, 0.63).
narrative_ontology:measurement_basis(sacr_be_t800, observed).
narrative_ontology:measurement(sacr_be_t1100, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 1100, 0.62).
narrative_ontology:measurement_basis(sacr_be_t1100, observed).
narrative_ontology:measurement(sacr_be_t1400, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 1400, 0.64).
narrative_ontology:measurement_basis(sacr_be_t1400, observed).
narrative_ontology:measurement(sacr_be_t1650, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 1650, 0.68).
narrative_ontology:measurement_basis(sacr_be_t1650, observed).
narrative_ontology:measurement(sacr_be_t1800, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 1800, 0.73).
narrative_ontology:measurement_basis(sacr_be_t1800, observed).
narrative_ontology:measurement(sacr_be_t1900, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 1900, 0.79).
narrative_ontology:measurement_basis(sacr_be_t1900, observed).
narrative_ontology:measurement(sacr_be_t1950, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 1950, 0.85).
narrative_ontology:measurement_basis(sacr_be_t1950, observed).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement_basis(sacr_su_t0, observed).
narrative_ontology:measurement(sacr_su_t200, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 200, 0.64).
narrative_ontology:measurement_basis(sacr_su_t200, observed).
narrative_ontology:measurement(sacr_su_t500, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 500, 0.58).
narrative_ontology:measurement_basis(sacr_su_t500, observed).
narrative_ontology:measurement(sacr_su_t800, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 800, 0.72).
narrative_ontology:measurement_basis(sacr_su_t800, observed).
narrative_ontology:measurement(sacr_su_t1100, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 1100, 0.67).
narrative_ontology:measurement_basis(sacr_su_t1100, observed).
narrative_ontology:measurement(sacr_su_t1400, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 1400, 0.58).
narrative_ontology:measurement_basis(sacr_su_t1400, observed).
narrative_ontology:measurement(sacr_su_t1650, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 1650, 0.5).
narrative_ontology:measurement_basis(sacr_su_t1650, observed).
narrative_ontology:measurement(sacr_su_t1800, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 1800, 0.56).
narrative_ontology:measurement_basis(sacr_su_t1800, observed).
narrative_ontology:measurement(sacr_su_t1900, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 1900, 0.63).
narrative_ontology:measurement_basis(sacr_su_t1900, observed).
narrative_ontology:measurement(sacr_su_t1950, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 1950, 0.7).
narrative_ontology:measurement_basis(sacr_su_t1950, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__performance_only_reading, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__performance_only_reading, sacrifice_obligation_kernel__study_as_exercise_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__performance_only_reading, sacrifice_obligation_kernel__messianic_suspension_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__performance_only_reading, sacrifice_obligation_kernel__symbolic_archive_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'the sacrifice obligation after the destruction' covers four structurally distinct claims that share one kernel but differ irreducibly in epsilon, victim set, and persistence mechanics. This file is the performance_only instance (epsilon ~0.85: binding, undischarged, maintained); the study_as_exercise instance transvalues the obligation into intellectual occupation (epsilon drops sharply, victim set thins); the messianic_suspension instance defers the debt to a future court (burden becomes expectancy); the symbolic_archive instance disclaims bindingness entirely (epsilon near zero, heritage function only). Edges run peer-to-peer across the sibling set because the readings compete for the same population rather than stacking upstream/downstream.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sacrifice_obligation_kernel__performance_only_reading, institutional, 0.7).
constraint_indexing:directionality_override(sacrifice_obligation_kernel__performance_only_reading, organized, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
