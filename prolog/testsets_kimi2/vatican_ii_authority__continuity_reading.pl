% ============================================================================
% CONSTRAINT STORY: vatican_ii_authority__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_authority__continuity_reading, []).

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
 *   constraint_id: vatican_ii_authority__continuity_reading
 *   human_readable: Vatican II Hermeneutic of Continuity (Continuity Reading)
 *   domain: theological/ecclesiological
 *
 * SUMMARY:
 *   The Second Vatican Council (1962-1965) introduced extensive liturgical,
 *   pastoral, and disciplinary reforms. The continuity reading construes all
 *   sixteen conciliar documents as organic doctrinal development faithful to
 *   an unchanging deposit of faith. Operating as an enforced hermeneutical
 *   framework within the Catholic Church, the constraint requires that
 *   apparent novelties be read as continuous with prior teaching. It is
 *   actively maintained by the Magisterium, benefits progressive reformers
 *   and post-conciliar theologians, and extracts cognitive compliance and
 *   institutional marginalization from traditionalists who experience the
 *   reforms as rupture.
 *
 * KEY AGENTS:
 *   - progressive_reformers: Primary beneficiaries (moderate/identity_locked) â gain institutional legitimacy for reforms by framing them as continuity.
 *   - magisterial_authority: Agenda setter (institutional/analytical) â enforces the continuity reading through teaching office and discipline.
 *   - traditionalist_clergy_laity: Primary payers (organized/constrained) â bear the cost of reconciling apparent novelties with continuity claims.
 *   - post_conciliar_theologians: Secondary beneficiaries (moderate/constrained) â careers depend on constructing continuity narratives.
 *   - rupture_reading_advocates: Excluded voices (moderate/trapped) â structurally barred from mainstream teaching and publishing.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__continuity_reading, 0.48).
domain_priors:suppression_score(vatican_ii_authority__continuity_reading, 0.65).
domain_priors:theater_ratio(vatican_ii_authority__continuity_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__continuity_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_authority__continuity_reading, "Vatican II Hermeneutic of Continuity (Continuity Reading)").
narrative_ontology:topic_domain(vatican_ii_authority__continuity_reading, "theological/ecclesiological").

domain_priors:requires_active_enforcement(vatican_ii_authority__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__continuity_reading, '758e23e8-130d-4e78-9c9f-fac957507552').
narrative_ontology:cs_kernel_codification('758e23e8-130d-4e78-9c9f-fac957507552', fixed_text).
narrative_ontology:cs_authority_grounding('758e23e8-130d-4e78-9c9f-fac957507552', lineage).
narrative_ontology:cs_interpretation_layer_present('758e23e8-130d-4e78-9c9f-fac957507552').
narrative_ontology:cs_reading_relation('758e23e8-130d-4e78-9c9f-fac957507552', vatican_ii_authority__rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('758e23e8-130d-4e78-9c9f-fac957507552', vatican_ii_authority__composite_overdetermination_reading, forecloses).
narrative_ontology:cs_axiom('758e23e8-130d-4e78-9c9f-fac957507552', foundational, deposit_of_faith_unchanging).
narrative_ontology:cs_axiom_status(deposit_of_faith_unchanging, holdable).
narrative_ontology:cs_axiom_grounding('758e23e8-130d-4e78-9c9f-fac957507552', deposit_of_faith_unchanging, theological).
narrative_ontology:cs_axiom('758e23e8-130d-4e78-9c9f-fac957507552', foundational, conciliar_continuity_hermeneutic).
narrative_ontology:cs_axiom_status(conciliar_continuity_hermeneutic, holdable).
narrative_ontology:cs_axiom_grounding('758e23e8-130d-4e78-9c9f-fac957507552', conciliar_continuity_hermeneutic, theological).
narrative_ontology:cs_reference_frame('758e23e8-130d-4e78-9c9f-fac957507552', apostolic_tradition_continuity).
narrative_ontology:cs_drift_state('758e23e8-130d-4e78-9c9f-fac957507552', post_conciliar_contestation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('758e23e8-130d-4e78-9c9f-fac957507552', '').
narrative_ontology:cs_kernel_id(vatican_ii_authority__continuity_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, progressive_reformers).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, magisterial_authority).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, post_conciliar_theologians).
narrative_ontology:constraint_victim(vatican_ii_authority__continuity_reading, traditionalist_clergy_laity).
narrative_ontology:constraint_vindicates(vatican_ii_authority__continuity_reading, conciliar_infallibility).
narrative_ontology:constraint_vindicates(vatican_ii_authority__continuity_reading, doctrinal_development_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for liturgical and pastoral reforms following Vatican II. Depend on the continuity hermeneutic to legitimate changes as organic development rather than rupture. Cannot openly claim doctrinal novelty without losing institutional standing; their reform agenda is only viable within the continuity frame, binding their identity to the Church's traditional self-presentation.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, progressive_reformers, beneficiary,
    moderate, generational, identity_locked, global).

% The papacy and curial offices that declare the authoritative interpretation of Vatican II. Enforces the continuity reading through encyclicals, liturgical norms, appointment controls, and disciplinary measures against rupture claims. Collects institutional legitimacy, obedience, and the power to define what counts as authentic development.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, magisterial_authority, agenda_setter,
    institutional, civilizational, analytical, global).

% Catholics who experience post-conciliar reforms as departures from pre-conciliar tradition. Required by the continuity hermeneutic to accept reforms as non-novel. Bear the cognitive, spiritual, and social cost of reconciling apparent contradictions. Exit options include schism (SSPX), sedevacantism, or internal marginalization â all high-cost and identity-disrupting.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, traditionalist_clergy_laity, payer,
    organized, generational, constrained, global).

% Academic theologians whose careers, appointments, and publishing access depend on constructing continuity narratives between pre- and post-conciliar teaching. Their institutional survival requires demonstrating that apparent novelties were always latent in tradition.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, post_conciliar_theologians, beneficiary,
    moderate, biographical, constrained, global).

% Theologians and clergy who argue that Vatican II contradicts prior magisterial teaching. Excluded from teaching offices, mainstream Catholic publishing, and seminary appointments. Their exclusion is structural: the continuity constraint requires their silence for its stability, and their presence would expose the coordination as contested.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, rupture_reading_advocates, excluded,
    moderate, biographical, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_authority__continuity_reading, magisterial_authority).
narrative_ontology:fixing_cost_class(vatican_ii_authority__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves institutional unity after a council that introduced extensive liturgical, pastoral, and disciplinary reforms, by providing a hermeneutical framework that reads all changes as organic development rather than rupture, thereby preventing schism and maintaining a shared narrative across factions.
% TRANSFER_FUNCTION: Moves interpretive authority to the Magisterium and progressive theologians who can declare what counts as legitimate development; moves cognitive compliance cost and institutional marginalization to traditionalists who must reconcile apparent novelties with continuity claims.
% ABSENT_VOICES: Rupture-reading theologians and traditionalist communities who experience the reforms as doctrinal breaks are structurally excluded from Magisterial teaching offices and mainstream Catholic academic discourse; their objections are classified as disobedience or schism rather than legitimate theological dissent.
% DISAPPEARANCE_RATIONALE: If the continuity hermeneutic vanished overnight, the institutional Church would face a crisis of legitimacy for post-conciliar reforms; progressive reforms would lose their traditional anchoring, the Magisterium's authority to enforce interpretation would be destabilized, and the rupture reading would become the primary available interpretive option, likely accelerating schism or forcing a different reconciliation mechanism.
% FOUNDING_PROBLEM: The post-Vatican II Church needed to account for extensive liturgical, pastoral, and disciplinary changes without admitting doctrinal contradiction with prior teaching, in order to maintain claims to unchanging truth and prevent institutional fragmentation.
% FOUNDING_PROBLEM_CORROBORATION: Progressive reformers and the Magisterium attest the problem is still live, citing ongoing traditionalist resistance. Traditionalist communities outside the benefiting parties (SSPX, independent chapels) attest the problem was manufactured â the reforms were unnecessary and the continuity hermeneutic is a post-hoc rationalization. Independent church historians corroborate that the council's own periti debated the continuity problem during the council, confirming the founding tension was genuine.
narrative_ontology:disappearance_verdict(vatican_ii_authority__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_authority__continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_authority__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_authority__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_authority__continuity_reading, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_authority__continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_authority__continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_authority__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48) reflects the asymmetric cost of the continuity requirement: traditionalists must perform cognitive and spiritual labor to reconcile apparent contradictions, while progressives receive reform legitimacy. Suppression (0.65) is high because the constraint's stability depends on actively excluding rupture readings from magisterial and academic discourse. Theater ratio (0.25) is moderate-low: the coordination function (preserving institutional unity) is genuine, but a growing share of magisterial speech performs continuity rather than demonstrating it. Accessibility collapse (0.70) is high because, within the Catholic epistemic frame, the rupture reading is rendered institutionally inaccessible once the continuity hermeneutic is accepted. Resistance (0.50) reflects sustained traditionalist opposition (SSPX, sedevacantist communities, internal dissent). Measurements share one time grid (0-60) to prevent misaligned temporal sampling.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (progressive reformers, magisterial authority) experience the constraint as a necessary rope preserving unity and truth. The payer seat (traditionalists) experiences the same structure as coercive extraction â they are told that their theological perception of rupture is itself disobedience. The engine computes this divergence from the structural data: same constraint, opposite directionalities, producing different per-seat classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   The Magisterium and progressive reformers sit near the beneficiary end (low d): the constraint subsidizes their authority and reform agenda. Traditionalists sit near the target end (high d): they bear the compliance cost. The excluded rupture-reading advocates sit at maximum effective extraction â their silencing is the enforcement event that makes the coordination appear seamless. Identity-locked exit for reformers and constrained exit for traditionalists modulate the derived directionality accordingly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â maintaining unity after a council that introduced visible changes â remains live. This prevents piton classification: the Magisterium actively benefits from the continuity reading and invests real enforcement in maintaining it (theater is present but subordinate). It prevents snare classification because there is a genuine coordination function: without some hermeneutic of continuity, institutional fragmentation or schism would be more probable. Tangled rope captures both the real coordination and the asymmetric extraction from traditionalists who pay for the unity that others enjoy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    continuity_as_construction,
    'Is the continuity hermeneutic a genuine recovery of traditional hermeneutics, or a novel construction retrofitted to legitimize reforms?',
    'Comparative theological analysis of pre-conciliar and post-conciliar magisterial hermeneutics; examination of whether the ''hermeneutic of continuity'' framework appears in pre-conciliar manuals.',
    'If the hermeneutic is a novel construction, the constraint''s claimed lineage grounding is weakened and its extractiveness increases â the coordination story becomes cover for retrospective legitimation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_as_construction, conceptual, 'Whether continuity is recovered tradition or novel construction').

omega_variable(
    enforcement_mechanism_ambiguity,
    'Is the suppression of rupture readings accomplished through explicit institutional penalty or through epistemic closure that defines rupture as non-Catholic?',
    'Documented cases of theologians disciplined for rupture readings versus sociological analysis of hiring, publishing, and appointment barriers in Catholic theology.',
    'If suppression is primarily epistemic closure, the structural suppression metric understates the constraint''s effective force â the exclusion is internalized rather than penalized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_ambiguity, empirical, 'Structural penalty versus epistemic closure as suppression mechanism').

omega_variable(
    suppression_internalized,
    'For traditionalists who exit the institutional Church, does the continuity hermeneutic persist as an internalized constraint?',
    'Narrative analysis of post-exit traditionalist communities: do they still organize their theological speech around proving/disproving continuity?',
    'If internalized, effective suppression exceeds the structural measure â the target carries the constraint after formal exit, amplifying extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalized, conceptual, 'Whether suppression persists after formal exit from the institution').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__continuity_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vat2_cont_tr_t0, vatican_ii_authority__continuity_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(vat2_cont_tr_t10, vatican_ii_authority__continuity_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(vat2_cont_tr_t20, vatican_ii_authority__continuity_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(vat2_cont_tr_t30, vatican_ii_authority__continuity_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement(vat2_cont_tr_t40, vatican_ii_authority__continuity_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement(vat2_cont_tr_t50, vatican_ii_authority__continuity_reading, theater_ratio, 50, 0.28).
narrative_ontology:measurement(vat2_cont_tr_t60, vatican_ii_authority__continuity_reading, theater_ratio, 60, 0.25).

% Extraction over time
narrative_ontology:measurement(vat2_cont_be_t0, vatican_ii_authority__continuity_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(vat2_cont_be_t10, vatican_ii_authority__continuity_reading, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(vat2_cont_be_t20, vatican_ii_authority__continuity_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(vat2_cont_be_t30, vatican_ii_authority__continuity_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement(vat2_cont_be_t40, vatican_ii_authority__continuity_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement(vat2_cont_be_t50, vatican_ii_authority__continuity_reading, base_extractiveness, 50, 0.52).
narrative_ontology:measurement(vat2_cont_be_t60, vatican_ii_authority__continuity_reading, base_extractiveness, 60, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(vat2_cont_su_t0, vatican_ii_authority__continuity_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(vat2_cont_su_t10, vatican_ii_authority__continuity_reading, suppression_requirement, 10, 0.2).
narrative_ontology:measurement(vat2_cont_su_t20, vatican_ii_authority__continuity_reading, suppression_requirement, 20, 0.3).
narrative_ontology:measurement(vat2_cont_su_t30, vatican_ii_authority__continuity_reading, suppression_requirement, 30, 0.45).
narrative_ontology:measurement(vat2_cont_su_t40, vatican_ii_authority__continuity_reading, suppression_requirement, 40, 0.55).
narrative_ontology:measurement(vat2_cont_su_t50, vatican_ii_authority__continuity_reading, suppression_requirement, 50, 0.62).
narrative_ontology:measurement(vat2_cont_su_t60, vatican_ii_authority__continuity_reading, suppression_requirement, 60, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_authority__continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_authority__continuity_reading, rupture_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__continuity_reading, composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% The kernel 'Vatican II authority' decomposes into three structurally distinct constraints per the Îµ-invariance principle: continuity_reading (coordinating extraction via enforced hermeneutic), rupture_reading (pure extraction from progressive institutional capture), and composite_overdetermination_reading (ambiguity as irreducible structural feature). Each carries a distinct Îµ, stakeholder geometry, and classification. This story is the continuity reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
