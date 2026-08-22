% ============================================================================
% CONSTRAINT STORY: john_1_1_logos__subordinationist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_john_1_1_logos__subordinationist, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: john_1_1_logos__subordinationist
 *   human_readable: Subordinationist Reading of the Johannine Logos (John 1:1)
 *   domain: theology/biblical_hermeneutics/christology
 *
 * SUMMARY:
 *   This story authors ONE reading of the contested Johannine Logos kernel:
 *   the subordinationist reading, under which the Logos of John 1:1 is a
 *   created or generated being — first and highest of creation, a subordinate
 *   divine agent — but not co-eternal or consubstantial with the Father. This
 *   reading was historically prominent (proto-forms in Arius, later
 *   formalized in various successor traditions) and remains live in some
 *   modern communions (e.g. certain unitarian and Jehovah's Witness-adjacent
 *   traditions read John 1:1c with an anarthrous predicate as 'a god' rather
 *   than 'God'). The ε authored here concerns the standing arrangement under
 *   this reading's own lights: an institutional structure in which
 *   subordinationist clergy administer congregations on this premise,
 *   sustained historically through periods of active political and
 *   ecclesiastical suppression (Nicene and post-Nicene condemnation) and
 *   periods of relative toleration. This is NOT the orthodox trinitarian
 *   reading (a separate constraint, high full-divinity claim, different
 *   victim set) and NOT the non-incarnational monotheist reading (which
 *   denies a distinct hypostasis altogether). Per the ε-invariance principle,
 *   each reading is its own constraint with its own stable ε; this file
 *   addresses only the subordinationist claim.
 *
 * KEY AGENTS:
 *   - subordinationist_clergy: agenda_setter (organized/constrained) — administers doctrine, bears ongoing marginalization cost
 *   - unitarian_leaning_congregations: beneficiary (moderate/mobile) — gains doctrinal coherence without co-equality apparatus
 *   - arian_successor_traditions: beneficiary/payer (moderate/constrained) — vindicated in reading but historically excluded
 *   - high_church_sacramental_authorities: payer (institutional/trapped) — sacramental exclusivity premise undercut
 *   - trinitarian_creedal_institutions: payer (institutional/trapped) — conciliar founding legitimacy directly challenged
 *   - biblical_textual_scholars: observer (analytical) — philological arbiter cited by all readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(john_1_1_logos__subordinationist, 0.42).
domain_priors:suppression_score(john_1_1_logos__subordinationist, 0.55).
domain_priors:theater_ratio(john_1_1_logos__subordinationist, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, extractiveness, 0.42).
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(john_1_1_logos__subordinationist, tangled_rope).
narrative_ontology:human_readable(john_1_1_logos__subordinationist, "Subordinationist Reading of the Johannine Logos (John 1:1)").
narrative_ontology:topic_domain(john_1_1_logos__subordinationist, "theology/biblical_hermeneutics/christology").

domain_priors:requires_active_enforcement(john_1_1_logos__subordinationist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(john_1_1_logos__subordinationist, '4b5dac85-1803-4847-b618-c32c42b500f5').
narrative_ontology:cs_kernel_codification('4b5dac85-1803-4847-b618-c32c42b500f5', fixed_text).
narrative_ontology:cs_authority_grounding('4b5dac85-1803-4847-b618-c32c42b500f5', lineage).
narrative_ontology:cs_interpretation_layer_present('4b5dac85-1803-4847-b618-c32c42b500f5').
narrative_ontology:cs_reading_relation('4b5dac85-1803-4847-b618-c32c42b500f5', john_1_1_logos__orthodox_christological, forecloses).
narrative_ontology:cs_reading_relation('4b5dac85-1803-4847-b618-c32c42b500f5', john_1_1_logos__non_incarnational_monotheist, coexists_with).
narrative_ontology:cs_axiom('4b5dac85-1803-4847-b618-c32c42b500f5', foundational, logos_is_created_first_being).
narrative_ontology:cs_axiom_status(logos_is_created_first_being, holdable).
narrative_ontology:cs_axiom_grounding('4b5dac85-1803-4847-b618-c32c42b500f5', logos_is_created_first_being, conventional).
narrative_ontology:cs_axiom('4b5dac85-1803-4847-b618-c32c42b500f5', foundational, monotheism_requires_ontological_subordination_of_mediator).
narrative_ontology:cs_axiom_status(monotheism_requires_ontological_subordination_of_mediator, holdable).
narrative_ontology:cs_axiom_grounding('4b5dac85-1803-4847-b618-c32c42b500f5', monotheism_requires_ontological_subordination_of_mediator, deontological).
narrative_ontology:cs_reference_frame('4b5dac85-1803-4847-b618-c32c42b500f5', pre_nicene_subordinationist_consensus).
narrative_ontology:cs_drift_state('4b5dac85-1803-4847-b618-c32c42b500f5', post_nicene_conciliar_settlement, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('4b5dac85-1803-4847-b618-c32c42b500f5', '').
narrative_ontology:cs_kernel_id(john_1_1_logos__subordinationist, john_1_1_logos).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(john_1_1_logos__subordinationist, subordinationist_clergy).
narrative_ontology:constraint_beneficiary(john_1_1_logos__subordinationist, unitarian_leaning_congregations).
narrative_ontology:constraint_beneficiary(john_1_1_logos__subordinationist, arian_successor_traditions).
narrative_ontology:constraint_victim(john_1_1_logos__subordinationist, high_church_sacramental_authorities).
narrative_ontology:constraint_victim(john_1_1_logos__subordinationist, trinitarian_creedal_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(john_1_1_logos__subordinationist, arian_successor_traditions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Teach and administer congregations on the premise that the Logos is the first and highest created being, subordinate to the Father in nature and origin. They set catechetical content, ordination standards, and liturgical practice around this reading, and their institutional standing depends on the reading holding within their communion.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, subordinationist_clergy, agenda_setter,
    organized, generational, constrained, regional).

% Receive a christology that resolves perceived logical tensions in strict monotheism without abandoning a distinct, exalted mediating figure. Worship practice is simplified: veneration of the Logos as chief agent, without the doctrinal apparatus (hypostatic union, consubstantiality) that full divinity claims require. Free to affiliate or disaffiliate from broader communions without losing coherence.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, unitarian_leaning_congregations, beneficiary,
    moderate, biographical, mobile, regional).

% Trace doctrinal lineage to historical subordinationist movements condemned by ecumenical councils. Benefit from a reading that vindicates their tradition's core claim, but pay a continuing cost in exclusion from mainstream ecumenical recognition, historical labeling as heretical, and periodic suppression by state-aligned trinitarian churches.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, arian_successor_traditions, beneficiary,
    moderate, civilizational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(john_1_1_logos__subordinationist, arian_successor_traditions, payer).

% Their sacramental exclusivity — the claim that only their ordained hierarchy can validly mediate grace incarnate in a fully divine Christ — depends on the Logos being consubstantial with the Father. A subordinationist reading erodes the ontological premise that makes the incarnation identical to God's own self-gift, undercutting the exclusivity claim that funds their institutional authority, property, and control over sacramental access. They cannot simply exit the dispute; their authority structure is built on the contested premise.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, high_church_sacramental_authorities, payer,
    institutional, civilizational, trapped, global).

% Ecumenical councils (Nicaea, Constantinople) built creedal identity and canon law on rejecting subordinationism. Their historical legitimacy as guardians of orthodoxy is directly threatened by a reading that revives the position their founding councils anathematized; they must actively police boundaries to prevent doctrinal drift.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, trinitarian_creedal_institutions, payer,
    institutional, civilizational, trapped, global).

% Examine the Greek grammar of John 1:1c ("kai theos en ho logos" — anarthrous predicate nominative) and its Second Temple Jewish intertextual background (Wisdom literature, Philo's Logos doctrine, memra traditions) without institutional stake in which reading prevails. Their philological work is cited by all three readings in the kernel contest.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, biblical_textual_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(john_1_1_logos__subordinationist, diffuse).
narrative_ontology:fixing_cost_class(john_1_1_logos__subordinationist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent framework for communities committed to strict monotheism to retain a distinct, exalted, pre-existent mediating figure without requiring the metaphysical apparatus of co-equal divine persons — resolving the tension between confessing 'the Logos was with God' and 'the Logos was god' without positing two co-eternal beings.
% TRANSFER_FUNCTION: Moves doctrinal authority and sacramental legitimacy away from institutions whose exclusive mediating role depends on full ontological identity between Christ and the Father, and toward traditions that can accommodate a subordinate but exalted created Logos — while also moving historical legitimacy and continuity claims toward movements descended from condemned subordinationist positions (Arian, Adoptionist, and related streams).
% ABSENT_VOICES: Second-Temple Jewish exegetes who would contest that either Christian reading correctly represents the memra/Wisdom background are not party to the intra-Christian dispute. Eastern Orthodox and Oriental Orthodox voices holding strong consubstantiality positions are present as payers but their theological objections are treated by this reading as historically contingent conciliar politics rather than settled truth.
% DISAPPEARANCE_RATIONALE: If the subordinationist reading vanished as a live theological option, unitarian and Arian-successor communions would lose their primary Johannine textual anchor and would need to relocate their christology in other texts or abandon the distinctive claim; conversely, if it became dominant, high-church sacramental exclusivity claims resting on full consubstantiality would lose their strongest scriptural proof-text, materially affecting claims to unique sacramental authority.
% FOUNDING_PROBLEM: Early Christian communities needed to explain John's prologue — which calls the Logos both 'with God' and 'god' — in a way consistent with Jewish monotheism, without positing two equally ultimate divine beings, prior to the fourth-century conciliar settlement.
% FOUNDING_PROBLEM_CORROBORATION: Historians of early Christianity (outside both the subordinationist and trinitarian communions) attest that subordinationist christologies were widespread and arguably majority positions among some regional churches before Nicaea (325 CE) settled the question politically as much as theologically; classicists and Second Temple scholars corroborate that the underlying monotheism-preservation problem was real and unresolved in the first two centuries. Trinitarian institutions themselves do not corroborate that the problem remains live post-Nicaea — they hold it as settled and treat continued advocacy as heterodoxy.
narrative_ontology:disappearance_verdict(john_1_1_logos__subordinationist, world_rearranges).
narrative_ontology:founding_problem_status(john_1_1_logos__subordinationist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(john_1_1_logos__subordinationist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(john_1_1_logos__subordinationist, 'none', 1).
narrative_ontology:epsilon_provenance(john_1_1_logos__subordinationist, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(john_1_1_logos__subordinationist_tests).
:- end_tests(john_1_1_logos__subordinationist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.42) reflects that the subordinationist reading itself does not primarily function as an extraction mechanism against its own adherents — it is a genuine coordination solution to a real exegetical and monotheism-preservation problem — but it does impose a structural cost on institutions whose authority rests on the full-divinity premise it denies, and it has historically required active suppression to survive (or to be suppressed). Suppression (0.55) is authored higher than a pure rope because the historical record shows real coercive machinery on both sides: subordinationist bishops exiled after Nicaea, later communions holding this view facing legal and social sanction in Christendom. The temporal grid shows suppression_requirement peaking sharply at t=325 (Nicaea) reflecting the moment of maximal conciliar coercion, then gradually relaxing as Christendom's political-religious fusion loosened, with a modest uptick by t=1700 reflecting renewed doctrinal policing during the Reformation and Counter-Reformation periods. Theater_ratio rises modestly over time (0.15→0.30) as much of the ongoing institutional conflict shifts from substantive theological argument to performative creedal recitation and boundary-marking liturgy in communions on both sides.
 *
 * PERSPECTIVAL GAP:
 *   From the subordinationist clergy's seat, this is coordination: a coherent, monotheism-preserving reading of scripture that solves a real exegetical tension. From the high-church sacramental authorities' seat, the same textual claim is experienced as an attack on the ontological premise that funds their exclusive mediating authority — a tangled rope, where the coordination function (a coherent christology) coexists with asymmetric extraction (undermining institutions that depend on the rejected premise remaining rejected).
 *
 * DIRECTIONALITY LOGIC:
 *   Subordinationist clergy and unitarian-leaning congregations sit toward the beneficiary end: the reading subsidizes their doctrinal coherence and institutional self-justification. Arian successor traditions are dual-positioned — vindicated doctrinally (beneficiary) but historically constrained by exclusion from ecumenical recognition and periodic legal suppression (payer), hence exit_options constrained rather than mobile. High-church sacramental authorities and trinitarian creedal institutions are targets: their authority structure is built on the premise this reading denies, and they cannot exit the dispute because their institutional identity and continuity claims are constituted by rejecting exactly this position — hence exit_options trapped despite institutional power.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling John's prologue with monotheism prior to a settled conciliar doctrine — is authored as contested rather than flatly dead, because the philological ambiguity in the Greek text (anarthrous theos) has never been definitively resolved by textual scholarship independent of theological commitment; the historical record shows the problem was live for at least two centuries before Nicaea imposed a political-ecclesiastical settlement. Declaring the founding problem 'dead' would mislabel this as a piton (an empty inertial structure); declaring it fully 'live' with unanimous corroboration would ignore that trinitarian institutions themselves regard the matter as long settled. The 'contested' status with corroboration from historians outside both benefiting communions is the honest position — it prevents both an inflated coordination story (as if the subordinationist reading were simply correct exegesis) and a dismissive extraction-only story (as if it were purely a heretical power grab), instead marking it as a genuine unresolved hermeneutical dispute with real institutional stakes on both sides.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    anarthrous_predicate_grammar,
    'Does the anarthrous construction ''kai theos en ho logos'' in John 1:1c grammatically support ''the Word was a god'' (subordinationist-compatible) or is the qualitative-predicate reading (''the Word was divine in nature'') decisive against a subordinationist gradation, independent of later theological commitment?',
    'Comparative corpus analysis of anarthrous predicate nominatives preceding the copula in Koine Greek across contemporaneous non-biblical texts, controlling for Colwell''s Rule and its known exceptions, conducted by scholars without denominational stake in the outcome.',
    'If the qualitative reading is grammatically decisive, the subordinationist reading loses its primary textual anchor in John 1:1 itself (though it could still be argued from other passages); if genuinely ambiguous, the reading''s philological legitimacy is intact and the dispute remains theological rather than grammatical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anarthrous_predicate_grammar, empirical, 'Whether Greek grammar alone settles or merely permits the subordinationist reading of John 1:1c.').

omega_variable(
    council_as_theology_or_politics,
    'Was the Nicene condemnation of subordinationist christologies a theological discovery of already-true doctrine, or a politically contingent settlement shaped by imperial patronage (Constantine''s convening and enforcement role) that could have gone otherwise?',
    'Historical analysis of the voting composition, imperial pressure, and regional church politics at Nicaea and subsequent councils (Constantinople 381), weighted against the internal theological arguments actually preserved in conciliar records.',
    'If predominantly political, the subsequent suppression of subordinationism is better modeled as extraction of doctrinal conformity for imperial/ecclesiastical unity rather than settlement of a genuine theological question — raising the effective suppression figure and supporting a stronger tangled_rope reading. If predominantly theological, the coordination function (settling a genuine ambiguity) is more substantial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(council_as_theology_or_politics, conceptual, 'Whether conciliar rejection of subordinationism reflects genuine theological resolution or political imposition.').

omega_variable(
    kernel_framing_alternative,
    'Is the correct unit of analysis ''three competing readings of one Johannine text'' (as framed here) or should the kernel instead be framed as ''the broader pre-Nicene christological spectrum,'' of which subordinationism, adoptionism, modalism, and proto-orthodoxy were all live points, with John 1:1 as only one contested proof-text among several (John 10:30, Philippians 2:6-11, Colossians 1:15-20)?',
    'Compare classification outcomes under the narrower single-text kernel framing versus a broader multi-text christological-spectrum kernel framing; assess whether victim sets and beneficiary sets differ materially.',
    'The broader framing might reveal that sacramental-authority victimhood is driven more by the cumulative weight of multiple contested texts than by John 1:1 alone, which would suggest this story''s ε for high-church authorities as payers is somewhat overstated if isolated to this one verse.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_alternative, conceptual, 'Whether the kernel should be scoped to John 1:1 alone or the wider pre-Nicene christological text corpus.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(john_1_1_logos__subordinationist, 0, 1700).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(john_tr_t0, john_1_1_logos__subordinationist, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(john_tr_t0, observed).
narrative_ontology:measurement(john_tr_t325, john_1_1_logos__subordinationist, theater_ratio, 325, 0.2).
narrative_ontology:measurement_basis(john_tr_t325, observed).
narrative_ontology:measurement(john_tr_t600, john_1_1_logos__subordinationist, theater_ratio, 600, 0.25).
narrative_ontology:measurement_basis(john_tr_t600, observed).
narrative_ontology:measurement(john_tr_t1000, john_1_1_logos__subordinationist, theater_ratio, 1000, 0.28).
narrative_ontology:measurement_basis(john_tr_t1000, observed).
narrative_ontology:measurement(john_tr_t1500, john_1_1_logos__subordinationist, theater_ratio, 1500, 0.3).
narrative_ontology:measurement_basis(john_tr_t1500, observed).
narrative_ontology:measurement(john_tr_t1700, john_1_1_logos__subordinationist, theater_ratio, 1700, 0.3).
narrative_ontology:measurement_basis(john_tr_t1700, observed).

% Extraction over time
narrative_ontology:measurement(john_be_t0, john_1_1_logos__subordinationist, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(john_be_t0, observed).
narrative_ontology:measurement(john_be_t325, john_1_1_logos__subordinationist, base_extractiveness, 325, 0.55).
narrative_ontology:measurement_basis(john_be_t325, observed).
narrative_ontology:measurement(john_be_t600, john_1_1_logos__subordinationist, base_extractiveness, 600, 0.5).
narrative_ontology:measurement_basis(john_be_t600, observed).
narrative_ontology:measurement(john_be_t1000, john_1_1_logos__subordinationist, base_extractiveness, 1000, 0.4).
narrative_ontology:measurement_basis(john_be_t1000, observed).
narrative_ontology:measurement(john_be_t1500, john_1_1_logos__subordinationist, base_extractiveness, 1500, 0.38).
narrative_ontology:measurement_basis(john_be_t1500, observed).
narrative_ontology:measurement(john_be_t1700, john_1_1_logos__subordinationist, base_extractiveness, 1700, 0.42).
narrative_ontology:measurement_basis(john_be_t1700, observed).

% Suppression requirement over time
narrative_ontology:measurement(john_su_t0, john_1_1_logos__subordinationist, suppression_requirement, 0, 0.2).
narrative_ontology:measurement_basis(john_su_t0, observed).
narrative_ontology:measurement(john_su_t325, john_1_1_logos__subordinationist, suppression_requirement, 325, 0.85).
narrative_ontology:measurement_basis(john_su_t325, observed).
narrative_ontology:measurement(john_su_t600, john_1_1_logos__subordinationist, suppression_requirement, 600, 0.75).
narrative_ontology:measurement_basis(john_su_t600, observed).
narrative_ontology:measurement(john_su_t1000, john_1_1_logos__subordinationist, suppression_requirement, 1000, 0.6).
narrative_ontology:measurement_basis(john_su_t1000, observed).
narrative_ontology:measurement(john_su_t1500, john_1_1_logos__subordinationist, suppression_requirement, 1500, 0.5).
narrative_ontology:measurement_basis(john_su_t1500, observed).
narrative_ontology:measurement(john_su_t1700, john_1_1_logos__subordinationist, suppression_requirement, 1700, 0.55).
narrative_ontology:measurement_basis(john_su_t1700, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(john_1_1_logos__subordinationist, identity_coordination).
narrative_ontology:affects_constraint(john_1_1_logos__subordinationist, john_1_1_logos__orthodox_christological).
narrative_ontology:affects_constraint(john_1_1_logos__subordinationist, john_1_1_logos__non_incarnational_monotheist).

% DUAL FORMULATION NOTE:
% This constraint, john_1_1_logos__orthodox_christological, and john_1_1_logos__non_incarnational_monotheist form a three-member constraint family, each instantiating a distinct reading of the same contested kernel (john_1_1_logos). Per the ε-invariance principle, each carries its own ε, beneficiary/victim structure, and claimed type rather than averaging across readings. The orthodox_christological reading is the historically dominant/upstream reading (backed by conciliar authority since 325 CE) that this subordinationist reading stands in tension with; the non_incarnational_monotheist reading represents a structurally distant third position denying personal hypostasis altogether. All three should be read together to see the full kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
