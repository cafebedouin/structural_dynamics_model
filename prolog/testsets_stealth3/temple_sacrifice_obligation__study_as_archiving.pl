% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_obligation__study_as_archiving
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_obligation__study_as_archiving, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: temple_sacrifice_obligation__study_as_archiving
 *   human_readable: Post-Temple Sacrificial Command Maintained as Outstanding Debt with Study-as-Archival-Preservation
 *   domain: religious/halakhic-authority/commitment-systems
 *
 * SUMMARY:
 *   Since the destruction of the Second Temple (70 CE), rabbinic Judaism has
 *   maintained the sacrificial commandments as fully binding while
 *   performance has been impossible. This story instantiates the
 *   study_as_archiving reading of that kernel: study of the sacrificial
 *   orders preserves operational knowledge for a future restoration but does
 *   not count toward fulfillment — the command stands, and the entire
 *   post-Temple period is, on this reading, one long unfulfilled obligation
 *   managed rather than discharged. The standing arrangement under contest —
 *   binding status actively maintained plus the archiving regime — is the
 *   epsilon referent, assessed by this reading's own lights; the endorsed
 *   alternative (a restored service) is not the referent. The claimed type
 *   and the metrics are authored independently: the claim states what this
 *   reading holds structurally true; the metrics describe the arrangement's
 *   observed operation, and the engine computes each seat's classification
 *   from the structural data. KEY AGENTS (by structural relationship): -
 *   rabbinic_authority_structure: Agenda-setting custodian
 *   (institutional/identity_locked) — maintains binding status, administers
 *   the archive, collects standing authority - exilic_observant_community:
 *   Primary bearer of the standing unmet obligation (organized/constrained) —
 *   funds, studies, recites; cannot perform - future_restored_community:
 *   Terminal recipient (powerless/trapped) — inherits the archived capability
 *   at restoration - yeshiva_kodashim_curriculum: Institutional beneficiary
 *   (institutional/identity_locked) — mission and enrollment ride on the
 *   preservation mandate - unfulfilled_divine_command: Declared injured
 *   party, non-agent — the command itself stands unfulfilled -
 *   suspensionist_denominations: Excluded rival seat
 *   (institutional/arbitrage) — holds the obligation inactive; outside the
 *   adjudication table - samaritan_community: Excluded performer seat
 *   (powerless/trapped) — performs actual sacrifice on Gerizim, falsifying
 *   'unperformable' from outside - comparative_religion_analysts: Analytical
 *   observer — sees the full structure, takes no seat
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__study_as_archiving, 0.55).
domain_priors:suppression_score(temple_sacrifice_obligation__study_as_archiving, 0.58).
domain_priors:theater_ratio(temple_sacrifice_obligation__study_as_archiving, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, extractiveness, 0.55).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__study_as_archiving, tangled_rope).
narrative_ontology:human_readable(temple_sacrifice_obligation__study_as_archiving, "Post-Temple Sacrificial Command Maintained as Outstanding Debt with Study-as-Archival-Preservation").
narrative_ontology:topic_domain(temple_sacrifice_obligation__study_as_archiving, "religious/halakhic-authority/commitment-systems").

domain_priors:requires_active_enforcement(temple_sacrifice_obligation__study_as_archiving).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__study_as_archiving, 'cfb6e366-2000-426a-97ac-57ce0c74b4ff').
narrative_ontology:cs_kernel_codification('cfb6e366-2000-426a-97ac-57ce0c74b4ff', fixed_text).
narrative_ontology:cs_authority_grounding('cfb6e366-2000-426a-97ac-57ce0c74b4ff', lineage).
narrative_ontology:cs_interpretation_layer_present('cfb6e366-2000-426a-97ac-57ce0c74b4ff').
narrative_ontology:cs_reading_relation('cfb6e366-2000-426a-97ac-57ce0c74b4ff', temple_sacrifice_obligation__study_as_occupation, forecloses).
narrative_ontology:cs_reading_relation('cfb6e366-2000-426a-97ac-57ce0c74b4ff', temple_sacrifice_obligation__messianic_suspension, forecloses).
narrative_ontology:cs_axiom('cfb6e366-2000-426a-97ac-57ce0c74b4ff', foundational, sacrificial_command_remains_outstanding).
narrative_ontology:cs_axiom_status(sacrificial_command_remains_outstanding, holdable).
narrative_ontology:cs_axiom_grounding('cfb6e366-2000-426a-97ac-57ce0c74b4ff', sacrificial_command_remains_outstanding, theological).
narrative_ontology:cs_axiom('cfb6e366-2000-426a-97ac-57ce0c74b4ff', foundational, study_preserves_without_substituting).
narrative_ontology:cs_axiom_status(study_preserves_without_substituting, holdable).
narrative_ontology:cs_axiom_grounding('cfb6e366-2000-426a-97ac-57ce0c74b4ff', study_preserves_without_substituting, conventional).
narrative_ontology:cs_axiom('cfb6e366-2000-426a-97ac-57ce0c74b4ff', secondary, restoration_requires_transmitted_operational_knowledge).
narrative_ontology:cs_axiom_status(restoration_requires_transmitted_operational_knowledge, holdable).
narrative_ontology:cs_axiom_grounding('cfb6e366-2000-426a-97ac-57ce0c74b4ff', restoration_requires_transmitted_operational_knowledge, instrumental).
narrative_ontology:cs_reference_frame('cfb6e366-2000-426a-97ac-57ce0c74b4ff', standing_temple_service_baseline).
narrative_ontology:cs_drift_state('cfb6e366-2000-426a-97ac-57ce0c74b4ff', contemporary_post_temple_exile, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('cfb6e366-2000-426a-97ac-57ce0c74b4ff', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__study_as_archiving, temple_sacrifice_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_archiving, rabbinic_authority_structure).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_archiving, yeshiva_kodashim_curriculum).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_archiving, future_restored_community).
narrative_ontology:constraint_victim(temple_sacrifice_obligation__study_as_archiving, exilic_observant_community).
narrative_ontology:constraint_victim(temple_sacrifice_obligation__study_as_archiving, unfulfilled_divine_command).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_archiving, eternal_binding_status_doctrine).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_archiving, restoration_capability_preservation_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues rulings, curricula, and liturgical forms that keep the sacrificial command on the books as fully binding despite two millennia without a Temple. Runs the ordination-and-transmission chain that certifies teachers of the sacrificial orders. Its standing as interpreter depends on the command remaining unresolved: release the obligation and the custodial warrant narrows; restore the service and the interpretive monopoly ends. Leaving this position would mean dismantling the institution's own reason for being.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, rabbinic_authority_structure, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_obligation__study_as_archiving, rabbinic_authority_structure, beneficiary).

% Keeps the command in daily life: morning recitation of the sacrificial passages, mourning liturgy, festival services recounting offerings that do not occur. Funds schools whose largest Talmudic tractates concern sacrifices that cannot be brought. Every member lives inside a command they are unable to perform and are taught remains wholly in force. Departing the community resolves the impossibility at the cost of family, belonging, and worldview.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, exilic_observant_community, payer,
    organized, generational, constrained, global).

% The descendants who would rebuild the service. They owe nothing now and bear no present cost; everything the archive preserves reaches them as working capability. Their shape is fixed by what the present generation transmits: they cannot opt out of inheriting, and they cannot receive anything the present generation fails to preserve.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, future_restored_community, beneficiary,
    powerless, civilizational, trapped, global).

% Advanced study tracks devote years to the sacrificial orders — species, dissection, measurements, disqualification rules — taught as living law rather than history. Enrollment, faculty lineages, and donor identity attach to this material. If the command were declared suspended or discharged, the courses would survive, but their framing as urgent operational preparation would not.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, yeshiva_kodashim_curriculum, beneficiary,
    institutional, generational, identity_locked, global).

% The command to offer regular sacrifices at the sanctuary stands, on this reading, unfulfilled every day since 70 CE. It issues nothing, collects nothing, and bears its non-fulfillment silently. It appears here because the arrangement's declared injury is precisely this standing non-fulfillment; it is listed for completeness and marked as a non-agent entity.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, unfulfilled_divine_command, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(temple_sacrifice_obligation__study_as_archiving, unfulfilled_divine_command).

% Movements inside and alongside the tradition that teach the sacrificial obligation is inoperative for the duration of exile — neither discharged nor breached. Several excised sacrificial-restoration language from their liturgy generations ago. They regard the perpetual-debt framing as a self-imposed weight; they sit outside the rabbinic venues where binding status is adjudicated, and their objection registers nowhere in those proceedings.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, suspensionist_denominations, excluded,
    institutional, generational, arbitrage, global).

% A few hundred people on Mount Gerizim who bring an actual Passover sacrifice every spring under their own site-doctrine. Their continuing performance shows the command form is executable somewhere, cutting against any claim that performance is impossible rather than site-specific. They are not seated in the rabbinic adjudication that declares the Jerusalem obligation unperformable.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, samaritan_community, excluded,
    powerless, generational, trapped, regional).

% Scholars of ritual, canon, and institutional memory who study the arrangement as a case of capability-preservation across institutional rupture, comparing it with vestigial law maintained elsewhere. They take testimony from every seat and endorse none; they observe that the archive's users and its administrators are different populations with different stakes.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, comparative_religion_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_obligation__study_as_archiving, rabbinic_authority_structure).
narrative_ontology:fixing_cost_class(temple_sacrifice_obligation__study_as_archiving, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves complete operational knowledge of a complex sacrificial system across an open-ended interruption — species, ages, disqualifications, handlers, sequence — transmitted continuously so that restoration could begin without reconstruction from scratch. Secondarily, it keeps a dispersed population aligned on the claim that the interruption is temporary.
% TRANSFER_FUNCTION: Moves study-hours, communal funds, liturgical attention, and institutional leadership from the present exilic community into two destinations: the custodial authority and interpretive standing held now by the rabbinic structure, and the archived operational capability held in trust for the future restored community.
% ABSENT_VOICES: Suspensionist denominations would argue the obligation is inactive and the debt framing manufactured; Samaritan performers would argue performance is possible and the impossibility claim is site-politics; animal-welfare advocates and secular Israelis would question preparing to resume slaughter at scale. All are structurally outside the halakhic venues where binding status is reaffirmed; the unanimity found there reflects the seating chart, not settled consent.
% DISAPPEARANCE_RATIONALE: If the binding-status doctrine and its archiving apparatus vanished overnight, daily liturgy would lose its sacrificial core, the sacrificial-orders curriculum would lose its warrant and shrink to history seminars, custodial authority would lose its longest-standing lever, and the community's self-understanding as awaiting restoration would reorganize around whatever replaced it. Nothing physical rearranges; the normative and institutional world does.
% FOUNDING_PROBLEM: When the Second Temple fell in 70 CE, a command system centered on sacrifices lost its site, its personnel pipeline, and its instruments at a stroke. The arrangement was built to answer one question: how does a community keep an unperformable command binding and its execution-knowledge intact until circumstances permit performance?
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set by the paying community's own sustained conduct: households fund and staff the archive voluntarily across two millennia, behavioral attestation that they regard the preservation problem as real. The continuous textual record — Mishnah, both Talmuds, geonic responsum, medieval codes — shows preservation activity predating every current institution and persisting through regimes with no stake in today's beneficiaries. Partially disputed: suspensionist bodies attest that the binding-status premise is contested, while conceding the narrower inference that if the service resumes, transmitted knowledge is its precondition.
narrative_ontology:disappearance_verdict(temple_sacrifice_obligation__study_as_archiving, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_obligation__study_as_archiving, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__study_as_archiving, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(temple_sacrifice_obligation__study_as_archiving, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_obligation__study_as_archiving, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_obligation__study_as_archiving_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(temple_sacrifice_obligation__study_as_archiving, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(temple_sacrifice_obligation__study_as_archiving_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (epsilon = 0.55): the community carries a perpetual normative debt plus a large resource commitment to study with no present practical yield, offset by real goods — continuity, identity, credible restoration-readiness. Suppression (0.58) is doctrinal-internalized first — believers carry the enforcement inside their own convictions, the debt feeling self-evident — with social-structural reinforcement at the boundaries (policing of suspensionist and repudiating exits); this mix motivates the culpability omega. Suppression is authored as a raw structural property and is not scaled by the engine; only extractiveness is scaled. Theater ratio (0.30): the technical archive is functional, but a growing share of sacrificial engagement is commemorative recitation whose comprehension has thinned. Accessibility collapse is low (0.35): rival readings remain visibly live — the arrangement does not dissolve its alternatives, it out-argues them inside its own venues. Resistance (0.45): sustained dissent from suspensionists, repudiating denominations, and secular exit paths. All three series share one six-point grid spanning 70 CE to 2025 CE; the mild oscillation in extractiveness tracks external rupture-and-consolidation cycles (Karaite schism, expulsions, emancipation, catastrophe and sovereignty) rather than an engineered intermittency mechanism, and base properties are measured at interval end (t=1955), on the post-sovereignty plateau.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. To the agenda-setting custodians the arrangement is sacred stewardship: they administer a trust and collect standing authority for doing so; their identity_locked exit is fusion of the institutional self with the custodial function — if the binding-status frame broke, the institution would not relocate, it would dissolve. To the paying community the same structure presents as a command they honor and cannot perform — cost borne without culpability under the incapacity doctrine, but borne daily. The yeshiva apparatus experiences the arrangement as vocational ground. The future community experiences it only as inheritance. Same-level institutional actors diverge on exit: the rabbinic seat is identity_locked while suspensionist denominations of comparable standing took the arbitrage exit generations ago — the difference is not power but whether the institution's self-narrative survives releasing the kernel. The engine computes these divergent per-seat classifications from the structural data; the authored claim adjudicates none of them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map cleanly: the exilic community is declared victim with constrained exit — high directionality toward the target end, amplified effective extraction. Rabbinic authority and the yeshiva apparatus are declared beneficiaries with identity_locked exits — low directionality, damped extraction; note that identity-lock here stabilizes their beneficiary position rather than pushing them toward the target end. The future restored community is declared beneficiary but carries a trapped exit marker; qualitatively its directionality sits nearest the beneficiary end — its entrapment is constitutive (it is the restoration's constituency), not cost-bearing — and if the structural derivation reads trapped-exit as target-leaning, that is a known limitation rather than an override-worthy correction, since the seat is powerless and captures nothing. The unfulfilled divine command is declared victim per the kernel's own delta but flagged non-agent: it contributes no directional arithmetic by design — that the arrangement's declared injury is a non-actor's is itself diagnostically interesting. Spatial scope is global (dispersed diaspora), which scales verification difficulty and therefore effective extraction modestly upward.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — keep an unperformable command binding and its knowledge intact pending restoration — is live: the interruption persists. Mandatrophy is therefore not resolved. The classification guards against both misreadings: stripped of the declared beneficiaries, the arrangement collapses toward a snare (a perpetual debt collected forever); stripped of the declared victims and the enforcement flag, it flatters into a pure rope (costless remembrance). Tangled rope holds both facts at once: real coordination (the archive works — restoration could plausibly resume on transmitted knowledge) and real asymmetry (the present community pays; custodians and heirs collect). The forward risk is degradation toward inertial persistence: if restoration possibility fades or technical preservation stops accumulating while funding and recitation continue, theater_ratio rises and the mandate outlives its function while the apparatus persists — the restoration_viability omega watches exactly this trajectory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_status_disagreement,
    'Within the shared kernel, which account of the post-Temple period is correct — outstanding debt (this reading), legitimate occupation via study, or heavenly suspension?',
    'No empirical test exists; resolution would arrive through intra-traditional adjudication (a consensual decisor or durable communal shift) or conceptual settlement of the halakhic categories of fulfillment, incapacity exemption, and lapsed obligation.',
    'If occupation prevails, the victim set empties — the outstanding remainder disappears and extraction falls toward the coordination-cost floor; if suspension prevails, the enforcement object vanishes entirely and the arrangement loses its reason for enforcement machinery. This file''s epsilon is valid only under the archiving answer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_status_disagreement, conceptual, 'Which sibling account of the obligation''s status is true determines this constraint''s victim set and epsilon.').

omega_variable(
    epsilon_reading_indexicality,
    'How much of the measured epsilon is contributed by THIS reading''s insistence that the debt remains outstanding, as against the standing arrangement itself?',
    'Family comparison: compile the sibling stories over the identical referent (same beneficiary/victim surface, same interval) and isolate the epsilon component attributable to reading-index rather than arrangement.',
    'A high reading-contribution means the corpus is partly measuring interpretive traditions, not arrangements — the classification belongs to the reading, and cross-reading comparison must be made at the referent level rather than the epsilon level.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epsilon_reading_indexicality, conceptual, 'Epsilon is a property of the reading over a fixed referent; this omega marks the indexical component explicitly.').

omega_variable(
    restoration_viability_drift,
    'Is restoration a live prospect that the archive actually serves, or has the archiving regime begun drifting toward self-justifying maintenance?',
    'Track technical-preservation outputs over time — identifiable procedures, qualified experts, replicable vessel and rite specifications — against purely commemorative activity; a falling technical share under stable funding indicates drift.',
    'If drift confirms, theater_ratio rises and the arrangement trends toward degraded inertia — maintained by performance and habit, fixable by no one at acceptable cost; if restoration stays live, the arrangement retains its transitional character.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restoration_viability_drift, empirical, 'Degradation risk for an arrangement whose justification is a restoration that has not arrived.').

omega_variable(
    culpability_vs_incapacity,
    'Is the post-Temple non-compliance culpable — a breach owed atonement — or incapacitated: exempt because performance is impossible?',
    'Liturgical-textual analysis: whether confession and fast-day liturgies treat the absence of sacrifice as sin requiring confession, or as circumstance lamented without fault; supplemented by responsa on how obligated individuals experience the standing command.',
    'Culpability framing loads a guilt-cost onto the paying seat that the structural measure underweights — effective extraction on that seat rises sharply; incapacity framing keeps the debt real but emotionally unloaded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(culpability_vs_incapacity, empirical, 'Whether the standing unmet obligation extracts guilt or only incompleteness.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__study_as_archiving, 0, 1955).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temple_sacrifice_archiving_tr_t0, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 0, 0.1).
narrative_ontology:measurement(temple_sacrifice_archiving_tr_t400, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 400, 0.2).
narrative_ontology:measurement(temple_sacrifice_archiving_tr_t900, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 900, 0.24).
narrative_ontology:measurement(temple_sacrifice_archiving_tr_t1400, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 1400, 0.28).
narrative_ontology:measurement(temple_sacrifice_archiving_tr_t1900, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 1900, 0.32).
narrative_ontology:measurement(temple_sacrifice_archiving_tr_t1955, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 1955, 0.3).

% Extraction over time
narrative_ontology:measurement(temple_sacrifice_archiving_be_t0, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(temple_sacrifice_archiving_be_t400, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 400, 0.55).
narrative_ontology:measurement(temple_sacrifice_archiving_be_t900, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 900, 0.52).
narrative_ontology:measurement(temple_sacrifice_archiving_be_t1400, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 1400, 0.54).
narrative_ontology:measurement(temple_sacrifice_archiving_be_t1900, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 1900, 0.57).
narrative_ontology:measurement(temple_sacrifice_archiving_be_t1955, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 1955, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(temple_sacrifice_archiving_su_t0, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(temple_sacrifice_archiving_su_t400, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 400, 0.22).
narrative_ontology:measurement(temple_sacrifice_archiving_su_t900, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 900, 0.38).
narrative_ontology:measurement(temple_sacrifice_archiving_su_t1400, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 1400, 0.3).
narrative_ontology:measurement(temple_sacrifice_archiving_su_t1900, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 1900, 0.5).
narrative_ontology:measurement(temple_sacrifice_archiving_su_t1955, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 1955, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_obligation__study_as_archiving, identity_coordination).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_archiving, temple_sacrifice_obligation__study_as_occupation).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_archiving, temple_sacrifice_obligation__messianic_suspension).

% DUAL FORMULATION NOTE:
% The colloquial label 'the sacrifice obligation after the destruction' covers three structurally distinct arrangements distinguished by what the unperformable period counts as against the standing command: outstanding debt (this file), compliance-credit via study (study_as_occupation), or heavenly suspension (messianic_suspension). Each is a separate constraint with its own epsilon, beneficiary set, and victim set; one story spanning them would violate epsilon-invariance because the victim set itself differs across readings. Family links run through network.affects_constraints in both directions; the shared ancestor is the fixed biblical text, with the archiving reading downstream of the eternality doctrine and upstream of concrete restoration-preparation activity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
