% ============================================================================
% CONSTRAINT STORY: biblical_divine_nature__unitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_divine_nature__unitarian_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: biblical_divine_nature__unitarian_reading
 *   human_readable: Unitarian Reading of Divine Nature (Father Alone Is God)
 *   domain: theology/religious_authority
 *
 * SUMMARY:
 *   This story instantiates the unitarian reading of the contested kernel
 *   biblical_divine_nature: God is numerically singular and identical with
 *   the Father alone; the Son and Spirit are subordinate, derivative, or
 *   created, not co-equal persons sharing one essence. Historically this
 *   reading (in various forms — dynamic monarchianism, Arian and semi-Arian
 *   positions, Socinianism, later Unitarianism) has repeatedly organized real
 *   communities of belief and worship, and has repeatedly been the target of
 *   conciliar condemnation, imperial legal sanction, and social exclusion
 *   once trinitarian orthodoxy achieved state backing (especially
 *   post-Nicaea, 325 CE). The coordination function — resolving high
 *   Christology with strict monotheism without essence-sharing metaphysics —
 *   is genuine; the extraction runs through the credal and institutional
 *   apparatus that must actively suppress the reading to maintain its own
 *   doctrinal monopoly and disciplinary authority.
 *
 * KEY AGENTS:
 *   - unitarian_congregations: primary beneficiary of doctrinal coherence and flat ecclesiology
 *   - trinitarian_institutional_hierarchy: primary payer of legitimacy erosion and primary enforcer of suppression
 *   - unitarian_dissenters_under_state_churches: primary victims bearing direct legal and social costs
 *   - credal_orthodoxy_apparatus: institutional actor structurally trapped into opposition since its function is defined by policing this exact boundary
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_divine_nature__unitarian_reading, 0.42).
domain_priors:suppression_score(biblical_divine_nature__unitarian_reading, 0.58).
domain_priors:theater_ratio(biblical_divine_nature__unitarian_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_divine_nature__unitarian_reading, tangled_rope).
narrative_ontology:human_readable(biblical_divine_nature__unitarian_reading, "Unitarian Reading of Divine Nature (Father Alone Is God)").
narrative_ontology:topic_domain(biblical_divine_nature__unitarian_reading, "theology/religious_authority").

domain_priors:requires_active_enforcement(biblical_divine_nature__unitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_divine_nature__unitarian_reading, 'e1fc6d13-1311-49fa-9626-0620e44a1537').
narrative_ontology:cs_kernel_codification('e1fc6d13-1311-49fa-9626-0620e44a1537', fixed_text).
narrative_ontology:cs_authority_grounding('e1fc6d13-1311-49fa-9626-0620e44a1537', distributed).
narrative_ontology:cs_reading_relation('e1fc6d13-1311-49fa-9626-0620e44a1537', biblical_divine_nature__trinitarian_reading, forecloses).
narrative_ontology:cs_reading_relation('e1fc6d13-1311-49fa-9626-0620e44a1537', biblical_divine_nature__modalist_reading, coexists_with).
narrative_ontology:cs_axiom('e1fc6d13-1311-49fa-9626-0620e44a1537', foundational, father_alone_is_numerically_god).
narrative_ontology:cs_axiom_status(father_alone_is_numerically_god, holdable).
narrative_ontology:cs_axiom_grounding('e1fc6d13-1311-49fa-9626-0620e44a1537', father_alone_is_numerically_god, conventional).
narrative_ontology:cs_axiom('e1fc6d13-1311-49fa-9626-0620e44a1537', secondary, son_and_spirit_ontologically_subordinate).
narrative_ontology:cs_axiom_status(son_and_spirit_ontologically_subordinate, holdable).
narrative_ontology:cs_axiom_grounding('e1fc6d13-1311-49fa-9626-0620e44a1537', son_and_spirit_ontologically_subordinate, conventional).
narrative_ontology:cs_reference_frame('e1fc6d13-1311-49fa-9626-0620e44a1537', apostolic_scriptural_monotheism).
narrative_ontology:cs_drift_state('e1fc6d13-1311-49fa-9626-0620e44a1537', post_nicene_establishment, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('e1fc6d13-1311-49fa-9626-0620e44a1537', '').
narrative_ontology:cs_kernel_id(biblical_divine_nature__unitarian_reading, biblical_divine_nature).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_divine_nature__unitarian_reading, unitarian_congregations).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__unitarian_reading, biblicist_lay_readers).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__unitarian_reading, anti_creedal_reform_movements).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, trinitarian_institutional_hierarchy).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, credal_orthodoxy_apparatus).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, unitarian_dissenters_under_state_churches).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, biblicist_lay_readers).
narrative_ontology:constraint_vindicates(biblical_divine_nature__unitarian_reading, strict_monotheism_doctrine).
narrative_ontology:constraint_vindicates(biblical_divine_nature__unitarian_reading, scriptural_sufficiency_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organize worship and doctrine around the Father as the sole numerically singular God, reading the Son as subordinate, begotten, or created rather than co-equal. Gain a flatter ecclesiology with no need to defend a metaphysically difficult essence-unity claim, but historically face exclusion, deregistration, or violence from established church-state structures that treat the position as heresy.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, unitarian_congregations, beneficiary,
    moderate, generational, constrained, regional).

% Approach scripture directly without a professional theological class mediating the doctrine of God through creedal formulas. Benefit from interpretive simplicity but pay socially — ostracism from trinitarian communities, loss of standing in institutions that gatekeep membership on credal grounds.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, biblicist_lay_readers, beneficiary,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(biblical_divine_nature__unitarian_reading, biblicist_lay_readers, payer).

% Use the unitarian reading as a lever against consolidated ecclesiastical authority generally — the doctrinal claim doubles as an institutional critique. Mobile in the sense that reform movements can relocate, publish, or found new communities when suppressed in one polity.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, anti_creedal_reform_movements, beneficiary,
    organized, civilizational, mobile, continental).

% Historically administers councils, creeds, and clergy structures built on the co-equal Trinity; the unitarian reading directly threatens the doctrinal basis of that hierarchy's authority to define orthodoxy. Loses legitimacy and disciplinary reach wherever unitarian readings gain ground, but retains vast institutional resources to suppress or marginalize the rival reading — it is both a victim of the reading's success and the primary enforcer against it.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, trinitarian_institutional_hierarchy, payer,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(biblical_divine_nature__unitarian_reading, trinitarian_institutional_hierarchy, agenda_setter).

% The apparatus of councils, confessional statements, and inquisitorial or disciplinary machinery exists specifically to police the boundary the unitarian reading crosses. Its entire functional justification is threatened by the reading's spread; it is structurally trapped into opposing it since conceding would dissolve its reason for existing.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, credal_orthodoxy_apparatus, payer,
    institutional, civilizational, trapped, global).

% Individuals and small congregations holding the unitarian reading under polities where trinitarian orthodoxy is legally established bear direct costs — execution, exile, imprisonment, or civil disability historically; social and legal exclusion in later periods. Exit means renouncing the belief, emigrating, or practicing covertly.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, unitarian_dissenters_under_state_churches, payer,
    powerless, biographical, trapped, national).

% Examine the textual and historical evidence for subordinationist versus co-equal readings of the divine nature in the earliest Christian sources without institutional stake in either outcome, though most operate inside institutions with a stake.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, biblical_scholars_analytical, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_divine_nature__unitarian_reading, diffuse).
narrative_ontology:fixing_cost_class(biblical_divine_nature__unitarian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent, textually grounded account of monotheism that resolves the apparent tension between calling Jesus divine/exalted and maintaining that God is numerically one — coordinating belief and worship around a single referent for 'God' without requiring the metaphysical apparatus of essence-sharing among distinct persons.
% TRANSFER_FUNCTION: Moves doctrinal authority away from councils, creeds, and the clerical class that administers them, and toward direct scriptural interpretation by congregations and individual readers; where enforced by state-church power, it moves civil and social costs (exile, legal disability, exclusion) onto those who hold the reading.
% ABSENT_VOICES: Early subordinationist and Jewish-Christian communities whose readings predate and inform this position are largely absent from the historical record that survives, since the record was substantially shaped and preserved by the trinitarian institutions that prevailed; their own testimony on how the doctrine functioned for them is mostly lost or transmitted only through hostile heresiological sources.
% DISAPPEARANCE_RATIONALE: If the unitarian reading vanished as a live position, ecumenical councils and credal formulas would lose their primary doctrinal antagonist, denominational splits tracing to anti-trinitarian movements (Socinians, Unitarians, some Restorationist and Jehovah's Witness communities) would lose their founding rationale, and centuries of legal and theological suppression apparatus built specifically to police this boundary would become historically inert rather than actively deployed.
% FOUNDING_PROBLEM: How to affirm the exalted, divine-sounding status the New Testament ascribes to Jesus while preserving the numerical singularity of God asserted in Jewish monotheism and in texts like the Shema — without introducing what this reading regards as an incoherent or scripturally unwarranted claim that three persons are each fully God.
% FOUNDING_PROBLEM_CORROBORATION: Unitarian and subordinationist communities themselves attest the problem remains live — they hold it is the trinitarian resolution, not the underlying tension, that is theologically unstable. Historians of early Christian doctrine outside any confessional stake (e.g. scholars of Second Temple Judaism and pre-Nicene Christology) corroborate that the tension between strict monotheism and high Christology was a genuine, unresolved problem in the first three centuries, independent of which later resolution one favors; they do not corroborate that the unitarian resolution is the correct one, only that the problem it addresses was real.
narrative_ontology:disappearance_verdict(biblical_divine_nature__unitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_divine_nature__unitarian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_divine_nature__unitarian_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(biblical_divine_nature__unitarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_divine_nature__unitarian_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_divine_nature__unitarian_reading_tests).
:- end_tests(biblical_divine_nature__unitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects that the reading itself does not extract from anyone by its content alone — the extraction runs through the suppression apparatus built against it, not through anything the reading's adherents collect. Suppression (0.58) is authored high because historically (post-325) holding this position carried severe legal and social costs in trinitarian-established polities, though the constraint's own suppression of others is minimal — asymmetry matters here: the reading is suppressed far more than it suppresses. Theater ratio (0.28) is moderate: council condemnations and heresiological literature contain real theological argument but also substantial performative denunciation aimed at consolidating institutional authority. Accessibility collapse (0.4) is moderate-low: alternative readings remained live and practiced underground or in dissenting communities throughout the interval — the reading was never fully extinguished. Resistance (0.72) is high: this reading met sustained active opposition from an entrenched institutional apparatus for most of its history.
 *
 * PERSPECTIVAL GAP:
 *   From the unitarian congregation's seat, the arrangement is a rope — pure coordination around scriptural monotheism with no coercive apparatus of its own. From the trinitarian hierarchy's seat, the same doctrinal claim is an existential threat requiring active, enforced suppression; the hierarchy experiences it as something that must be fought rather than merely disagreed with. The engine's per-seat computation should register this asymmetry: the reading's own structure is low-suppression, but the response it triggers from the institutional victim seat generates the enforcement machinery that makes the overall constraint tangled rather than a clean rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Unitarian congregations, lay biblicist readers, and anti-creedal reformers are declared beneficiaries: they get doctrinal coherence, interpretive autonomy, and (for reform movements) a lever against consolidated clerical power — d sits near the beneficiary end for them. The trinitarian hierarchy and credal apparatus are declared victims of the reading's success: their institutional authority and disciplinary monopoly are directly threatened, and their own historical response was to invest heavily in enforcement — d sits near the target end for these institutional seats, an unusual case where high power (institutional) coincides with victim status because the threat is to legitimacy, not material resources. Individual unitarian dissenters under state churches are also victims, but for the ordinary reason: trapped exit, direct legal and social cost, powerless power atom — d sits at the extreme target end for them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling high Christology with strict monotheism — remains genuinely contested rather than resolved on either side, which is why founding_problem_status is 'contested' rather than 'dead': declaring it dead would understate the degree to which serious biblical scholarship still treats the tension as live. This prevents the classification from collapsing into either 'this is settled orthodoxy defending truth' or 'this is pure legacy suppression with no remaining function' — the coordination function (a coherent monotheism) is real and ongoing for its adherents, while the extraction (suppression cost borne by dissenters under establishment power) is also real and historically documented, which is exactly the tangled_rope signature: genuine coordination AND asymmetric extraction through the same structural conflict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unitarian_reading_historical_priority_ambiguity,
    'Is the unitarian reading the earliest, most textually grounded Christian position that trinitarian orthodoxy departed from — or is it itself a later simplification/rationalization responding to Hellenistic philosophical pressure, no more original than the trinitarian synthesis?',
    'Comparative philological and historical analysis of pre-Nicene Christian sources (Apostolic Fathers, early apologists) independent of confessional commitment, cross-checked against Second Temple Jewish monotheism scholarship.',
    'If the unitarian reading has genuine historical priority, its claim to represent ''original'' Christian doctrine strengthens and the trinitarian apparatus''s suppression looks more like innovation defending itself against a prior consensus. If it is a later rationalizing move, the framing of trinitarian orthodoxy as the extractive innovation weakens considerably.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(unitarian_reading_historical_priority_ambiguity, empirical, 'Historical priority ambiguity between unitarian and trinitarian readings in earliest Christianity.').

omega_variable(
    committer_kernel_sibling_readings,
    'This constraint is one reading of the kernel biblical_divine_nature. The sibling readings (trinitarian_reading, modalist_reading) each instantiate a different beneficiary/victim structure and different ε. What would change if a party adopted a sibling reading instead?',
    'Compare the three linked constraint files: trinitarian_reading places institutional hierarchy and credal orthodoxy on the beneficiary side (they administer the essence-unity doctrine) and unitarian/subordinationist dissenters on the victim side — an almost exact inversion of this story''s beneficiary/victim assignment. modalist_reading produces yet a third structure since it denies real personal distinction rather than denying co-equality, drawing fire from both trinitarians and unitarians simultaneously.',
    'The disagreement between readings is located specifically at the question of numerical identity versus essence-sharing versus modal sequencing for the referent ''God'' — this is the structural element the three readings differ on, not a difference of degree or emphasis but a difference of what kind of claim is being made about divine unity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_sibling_readings, conceptual, 'Committer structure: this is one reading of the biblical_divine_nature kernel; sibling readings invert the beneficiary/victim structure.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of unitarian dissenters primarily structural (legal disability, exile, execution under state-church establishment) or partly internalized (communities that self-censor or drift toward orthodoxy after generations under threat, independent of active enforcement)?',
    'Trace communities that persisted after legal establishment weakened (e.g. post-Toleration-Act Unitarian congregations in England) — if doctrinal caution persists after the legal threat is removed, that indicates an internalized component.',
    'If substantially internalized, the effective suppression borne by descendant communities is higher than the structural/legal record alone suggests, since the caution outlives the removed legal mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression among unitarian dissenting communities across the interval.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_divine_nature__unitarian_reading, 0, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_divine_nature__unitarian_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(bibl_tr_t0, observed).
narrative_ontology:measurement(bibl_tr_t325, biblical_divine_nature__unitarian_reading, theater_ratio, 325, 0.35).
narrative_ontology:measurement_basis(bibl_tr_t325, observed).
narrative_ontology:measurement(bibl_tr_t600, biblical_divine_nature__unitarian_reading, theater_ratio, 600, 0.3).
narrative_ontology:measurement_basis(bibl_tr_t600, observed).
narrative_ontology:measurement(bibl_tr_t1100, biblical_divine_nature__unitarian_reading, theater_ratio, 1100, 0.25).
narrative_ontology:measurement_basis(bibl_tr_t1100, observed).
narrative_ontology:measurement(bibl_tr_t1600, biblical_divine_nature__unitarian_reading, theater_ratio, 1600, 0.3).
narrative_ontology:measurement_basis(bibl_tr_t1600, observed).
narrative_ontology:measurement(bibl_tr_t1900, biblical_divine_nature__unitarian_reading, theater_ratio, 1900, 0.28).
narrative_ontology:measurement_basis(bibl_tr_t1900, observed).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_divine_nature__unitarian_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement_basis(bibl_be_t0, observed).
narrative_ontology:measurement(bibl_be_t325, biblical_divine_nature__unitarian_reading, base_extractiveness, 325, 0.55).
narrative_ontology:measurement_basis(bibl_be_t325, observed).
narrative_ontology:measurement(bibl_be_t600, biblical_divine_nature__unitarian_reading, base_extractiveness, 600, 0.5).
narrative_ontology:measurement_basis(bibl_be_t600, observed).
narrative_ontology:measurement(bibl_be_t1100, biblical_divine_nature__unitarian_reading, base_extractiveness, 1100, 0.45).
narrative_ontology:measurement_basis(bibl_be_t1100, observed).
narrative_ontology:measurement(bibl_be_t1600, biblical_divine_nature__unitarian_reading, base_extractiveness, 1600, 0.5).
narrative_ontology:measurement_basis(bibl_be_t1600, observed).
narrative_ontology:measurement(bibl_be_t1900, biblical_divine_nature__unitarian_reading, base_extractiveness, 1900, 0.42).
narrative_ontology:measurement_basis(bibl_be_t1900, observed).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_divine_nature__unitarian_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(bibl_su_t0, observed).
narrative_ontology:measurement(bibl_su_t325, biblical_divine_nature__unitarian_reading, suppression_requirement, 325, 0.75).
narrative_ontology:measurement_basis(bibl_su_t325, observed).
narrative_ontology:measurement(bibl_su_t600, biblical_divine_nature__unitarian_reading, suppression_requirement, 600, 0.65).
narrative_ontology:measurement_basis(bibl_su_t600, observed).
narrative_ontology:measurement(bibl_su_t1100, biblical_divine_nature__unitarian_reading, suppression_requirement, 1100, 0.55).
narrative_ontology:measurement_basis(bibl_su_t1100, observed).
narrative_ontology:measurement(bibl_su_t1600, biblical_divine_nature__unitarian_reading, suppression_requirement, 1600, 0.6).
narrative_ontology:measurement_basis(bibl_su_t1600, observed).
narrative_ontology:measurement(bibl_su_t1900, biblical_divine_nature__unitarian_reading, suppression_requirement, 1900, 0.58).
narrative_ontology:measurement_basis(bibl_su_t1900, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_divine_nature__unitarian_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(biblical_divine_nature__unitarian_reading, 0.08).
narrative_ontology:affects_constraint(biblical_divine_nature__unitarian_reading, trinitarian_reading).
narrative_ontology:affects_constraint(biblical_divine_nature__unitarian_reading, modalist_reading).

% DUAL FORMULATION NOTE:
% biblical_divine_nature kernel family (3 stories): unitarian_reading (this file, tangled_rope — genuine monotheistic coordination function, asymmetric extraction through suppression apparatus targeting dissenters), trinitarian_reading (co-equal hypostases sharing one ousia; expected higher institutional authority and inverted beneficiary/victim structure), modalist_reading (sequential modes of one person; expected to draw opposition from both other readings simultaneously, likely lower persistence and smaller surviving adherent base). All three share the founding problem (reconciling high Christology with strict monotheism) but resolve it via structurally incompatible claims about the referent 'God' — numerical identity (unitarian) versus essence-unity across distinct persons (trinitarian) versus sequential single-personhood (modalist).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
