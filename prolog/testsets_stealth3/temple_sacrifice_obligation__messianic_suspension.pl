% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_obligation__messianic_suspension
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
 *   constraint_id: temple_sacrifice_obligation__messianic_suspension
 *   human_readable: Messianic Suspension of the Temple Sacrifice Obligation
 *   domain: religious/halakhic/commitment_systems
 *
 * SUMMARY:
 *   After the destruction of the Second Temple (70 CE), the halakhic
 *   framework faced commandments — the sacrificial order, pilgrimage
 *   festivals, purity rites — that could no longer be performed but could
 *   not, within the framework, be abrogated. This story instantiates the
 *   messianic_suspension reading of that situation: the obligation is held in
 *   a precise legal status — in force, unfulfilled, unviolated — pending a
 *   restoration event (the messianic era and rebuilt Temple) that no human
 *   agent can unilaterally effect. The standing arrangement under contest is
 *   the regime that maintains this status: codification of the suspended
 *   commandments, curricula that keep the sacrificial laws in living study,
 *   liturgical remembrance of the service, and custodial jurisdiction that
 *   defers all adjudication of resumption to the future event. No compliance
 *   is possible, no violation accrues, no sanction exists; the arrangement's
 *   costs are a small maintenance burden and its gains are legal coherence,
 *   covenantal continuity, and custodial standing. The claim and the metrics
 *   are independent authored facts: the constraint is claimed as rope —
 *   genuine coordination of the unperformable-commandment problem at
 *   near-zero coercive overhead — and the metrics describe near-floor
 *   operation; the engine computes per-seat types from the structural data,
 *   and any divergence is the measurement, not an error.
 *
 * KEY AGENTS:
 *   - halakhic_custodial_authority: Agenda-setter (institutional / identity_locked) — codifies and transmits the suspended status; cannot abrogate, cannot fulfill, cannot date the resumption; its jurisdiction over the dormant law exists only while the obligation stays binding
 *   - torah_observant_community: Primary beneficiary, secondary payer (organized / identity_locked) — receives legal coherence and covenantal continuity; bears the maintenance burden of study, liturgical recitation, and remembrance; holds the restoration horizon across generations
 *   - abrogationist_movements: Excluded (organized / mobile) — hold the commandments void; operate outside halakhic adjudication; their exclusion marks the framework's boundary
 *   - comparative_religion_scholars: Analytical observer (analytical / analytical) — documents the regime's persistence, maintenance costs, and standing accrual from outside the framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__messianic_suspension, 0.08).
domain_priors:suppression_score(temple_sacrifice_obligation__messianic_suspension, 0.07).
domain_priors:theater_ratio(temple_sacrifice_obligation__messianic_suspension, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, extractiveness, 0.08).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 0.07).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__messianic_suspension, rope).
narrative_ontology:human_readable(temple_sacrifice_obligation__messianic_suspension, "Messianic Suspension of the Temple Sacrifice Obligation").
narrative_ontology:topic_domain(temple_sacrifice_obligation__messianic_suspension, "religious/halakhic/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__messianic_suspension, '5e2961e4-32a2-4c05-8904-4ca187f60b68').
narrative_ontology:cs_kernel_codification('5e2961e4-32a2-4c05-8904-4ca187f60b68', fixed_text).
narrative_ontology:cs_authority_grounding('5e2961e4-32a2-4c05-8904-4ca187f60b68', lineage).
narrative_ontology:cs_interpretation_layer_present('5e2961e4-32a2-4c05-8904-4ca187f60b68').
narrative_ontology:cs_reading_relation('5e2961e4-32a2-4c05-8904-4ca187f60b68', temple_sacrifice_obligation__study_as_occupation, coexists_with).
narrative_ontology:cs_reading_relation('5e2961e4-32a2-4c05-8904-4ca187f60b68', temple_sacrifice_obligation__study_as_archiving, influences).
narrative_ontology:cs_axiom('5e2961e4-32a2-4c05-8904-4ca187f60b68', foundational, unperformable_command_remains_in_force).
narrative_ontology:cs_axiom_status(unperformable_command_remains_in_force, holdable).
narrative_ontology:cs_axiom_grounding('5e2961e4-32a2-4c05-8904-4ca187f60b68', unperformable_command_remains_in_force, deontological).
narrative_ontology:cs_axiom('5e2961e4-32a2-4c05-8904-4ca187f60b68', foundational, restoration_event_sole_adjudicator).
narrative_ontology:cs_axiom_status(restoration_event_sole_adjudicator, holdable).
narrative_ontology:cs_axiom_grounding('5e2961e4-32a2-4c05-8904-4ca187f60b68', restoration_event_sole_adjudicator, theological).
narrative_ontology:cs_axiom('5e2961e4-32a2-4c05-8904-4ca187f60b68', secondary, study_maintains_without_discharging).
narrative_ontology:cs_axiom_status(study_maintains_without_discharging, holdable).
narrative_ontology:cs_axiom_grounding('5e2961e4-32a2-4c05-8904-4ca187f60b68', study_maintains_without_discharging, deontological).
narrative_ontology:cs_reference_frame('5e2961e4-32a2-4c05-8904-4ca187f60b68', sinaitic_obligation_pending_restoration).
narrative_ontology:cs_drift_state('5e2961e4-32a2-4c05-8904-4ca187f60b68', contemporary, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('5e2961e4-32a2-4c05-8904-4ca187f60b68', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__messianic_suspension, temple_sacrifice_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__messianic_suspension, halakhic_custodial_authority).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__messianic_suspension, torah_observant_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(temple_sacrifice_obligation__messianic_suspension, torah_observant_community).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__messianic_suspension, divine_command_bindingness).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__messianic_suspension, messianic_restoration_certainty).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__messianic_suspension, halakhic_continuity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Codifies and transmits the ruling that the sacrificial commandments remain binding but cannot be performed: fixes the study curriculum, maintains the liturgical remembrance of the service, and answers practical questions from within the suspended status. It cannot void the commandments (they are divine), cannot perform them (the Temple is gone), and cannot set a date for resumption (the restoration is not its act). Its standing — jurisdiction over a large body of law that is taught, liturgized, and adjudicated in principle — exists only while the commandments remain binding; a ruling that they lapsed would dissolve that jurisdiction. Exit would mean abandoning the framework its own authority rests on.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, halakhic_custodial_authority, agenda_setter,
    institutional, generational, identity_locked, global).

% Lives inside the suspended status: recites the sacrificial orders in the daily liturgy, studies the laws in yeshiva curricula, mourns the Temple, and holds the restoration horizon across generations. It receives the arrangement's coherence — every commandment binds, none is void, none is currently violated — and bears its small costs: study time, liturgical attention, the discipline of remembering a service it does not perform. Leaving would mean leaving the covenantal framework that constitutes its identity, not merely this practice.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, torah_observant_community, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_obligation__messianic_suspension, torah_observant_community, payer).

% Hold that the sacrificial commandments are simply no longer binding and that maintaining their study and liturgy is nostalgia for a superseded cult. Organized movements with their own institutions and publications, they operate entirely outside halakhic adjudication: the rabbinic framework does not adjudicate their claim, and their exclusion marks where the framework's boundary sits.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, abrogationist_movements, excluded,
    organized, biographical, mobile, global).

% Study the arrangement from outside the framework: how a legal system has held unperformable commandments in force for two millennia, what the maintenance costs, where standing accrues, and how the deferral is reproduced in curriculum and liturgy. They hold no position on the restoration and no stake in the horizon.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_obligation__messianic_suspension, diffuse).
narrative_ontology:fixing_cost_class(temple_sacrifice_obligation__messianic_suspension, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the unperformable-commandment problem: holds commandments that cannot be executed in force without generating either mass violation or legal incoherence, coordinates the community on a single resolution of the obligation's status rather than ad hoc per-agent resolutions, and maintains the legal-epistemic base (working knowledge of the sacrificial laws) that resumption would require.
% TRANSFER_FUNCTION: Moves very little: a maintenance burden — study time, liturgical recitation of the sacrificial orders, remembrance practices — flows from the observant community into the tradition's collective continuity, and the custodial authority receives standing from stewarding what the community maintains. No money, no labor service, no sanctions.
% ABSENT_VOICES: Abrogationist movements (Reform, and in an earlier key the Karaite rejection of the rabbinic framework) would object that the obligation is simply void and the maintenance is nostalgia; they are outside halakhic adjudication, and the regime's coherence depends in part on their voice not being in the room. Holders of the sibling readings are inside the conversation and contest the deferral's terms, not its existence.
% DISAPPEARANCE_RATIONALE: If the suspension regime vanished overnight, every authority would have to re-resolve the kernel immediately: abrogation would unravel the bindingness premise on which the entire legal system rests; demanding performance would make the system uninhabitable; the study curricula, the sacrificial-order liturgy, and the messianic horizon are all organized around the suspended status. Communities would rearrange around whichever resolution each adopted, and the framework's continuity would fracture along that line.
% FOUNDING_PROBLEM: The destruction of the Second Temple left Torah commandments — sacrifices, pilgrimage rites, purity law — that could no longer be performed but could not, within a framework committed to divine command, be abrogated. The founding problem: how to hold unperformable commandments in a legal system whose premise is that commandments bind.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the destruction of 70 CE is externally attested (Josephus, Roman administrative records), and the earliest rabbinic responses to the unperformable-commandments problem predate the custodial class's later institutional form. Within the framework, the liturgy's own lament for the lost service attests the problem from the community's seat rather than the authority's.
narrative_ontology:disappearance_verdict(temple_sacrifice_obligation__messianic_suspension, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_obligation__messianic_suspension, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__messianic_suspension, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(temple_sacrifice_obligation__messianic_suspension, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_obligation__messianic_suspension, 0.08, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_obligation__messianic_suspension_tests).
:- end_tests(temple_sacrifice_obligation__messianic_suspension_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is 0.08 — at the identity_coordination floor — because the regime imposes no compliance, no sanction, and no material transfer: the entire burden is maintenance (study time, liturgical recitation), and its compensating value (coherence, continuity) accrues to the same community that bears it. Suppression is 0.07: the regime enforces nothing because there is nothing to enforce; its only boundary work is doctrinal (excluding abrogationist readings from adjudication), which is structural rather than coercive. Theater is 0.15: per this reading the maintenance is functional — knowledge-in-waiting that would be deployable on restoration — with a modest commemorative accretion in the liturgy. Accessibility_collapse is 0.60: once the framework's premises are accepted, abrogation collapses entirely (a divine command cannot be voided) and the sibling readings remain the only live alternatives, but those siblings persist as legitimate halakhic positions. Resistance is 0.12: within the framework the suspension doctrine is consensus; the abrogationists exited rather than resisted. The three measurement series share one grid (centuries 0-20 from the destruction, every metric authored at every point); trajectories are near-flat by design — this regime's signature is the absence of drift: no enforcement ratchet (nothing to ratchet), no rent accumulation (nothing to accumulate), only a slow commemorative accretion in theater.
 *
 * PERSPECTIVAL GAP:
 *   From the custodial seat the regime is faithful stewardship: a divine command held intact across an impossible interval, with study as the only permitted form of engagement. From the community seat it is coherent continuity at trivial cost — the commandments bind, none are void, none are violated. From the excluded abrogationist seat the same regime is institutionalized nostalgia: a two-millennium deferral that preserves jurisdictional standing by refusing to resolve what the destruction already resolved. From the analytical seat it is a deferral machine of unusual stability — event-indexed, enforcement-free, and load-bearing for the framework's entire premise that commandments bind. The engine computes these per-seat classifications from the structural data; the divergence between the custodial seat's near-beneficiary reading and the excluded seat's extraction-flavored reading is the measurement, not something to reconcile. Inter-institutionally, the custodial authority (institutional power, identity-locked) and the abrogationist movements (organized power, mobile exit) hold comparable social standing but opposite structural relationships to the regime: what is custodianship inside the framework is nostalgia outside it, and the difference is exit, not power.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations map to real structure: the custodial authority collects standing (jurisdiction over the suspended law exists only under this regime — abrogation voids it, the occupation reading transforms it), and the community collects coherence and continuity. Both sit near the beneficiary end of d; the community's secondary payer role (it bears the maintenance burden) nudges it toward symmetric, but the burden is small and self-endorsed, so the derivation from the beneficiary declarations plus exit options is adequate and no override is authored. No victim set exists: nothing is taken from anyone that they could otherwise retain — the Temple's absence, not the regime, is what deprives the community of the service; the regime manages that deprivation, it does not create it. Suppression is an unscaled structural fact (0.07) and is not amplified by scope; extraction is scaled by the engine from directionality and scope — with both beneficiary seats near the beneficiary end and global scope, effective extraction stays near or below the coordination floor for every seat, which is the rope signature.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how to hold unperformable commandments in a system whose premise is that commandments bind — is live: the Temple is absent, the commandments remain unperformable, and the deferral has no humanly settable terminal date. The regime has therefore not outlived its function within the framework. The classification prevents two mislabels: calling the regime a snare would require a victim set, and none exists — no one is deprived of anything by the regime itself; calling it a mountain would require that the regime persist without maintenance, and it would not — the status it tracks is fixed, but the knowledge-in-waiting decays without curricula and liturgy, so the regime is maintained coordination, not natural law (the distinction between the fixed status and the maintained regime is carried by the status_naturality_ambiguity omega rather than by inflating the claim). The live risk is slow drift toward piton: if the maintenance became purely commemorative (theater rising past roughly 0.5) while the restoration expectation thinned, the regime would persist by inertia and performance. The maintenance_functionality_drift omega tracks exactly this vector, and the flat theater series (0.08 to 0.15 over twenty centuries) shows the drift is real but slow.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    status_naturality_ambiguity,
    'Is the suspended status of the sacrificial obligation a structural feature of the normative universe as the reading holds it (divine command plus Temple absence — no agent can fulfill, abrogate, or date it), or a constructed custodial arrangement whose fixity is the tradition''s own framing?',
    'Comparative doctrinal analysis: whether the framework treats the status as agent-insensitive (no authority could alter it) or as a maintainable institutional position; external observation of whether custodial gains track the status itself or the maintenance apparatus built around it.',
    'If constructed, the regime is low-grade coordination maintained partly for custodial standing and the receipt concentration becomes material to classification; if natural, no agent owns the constraint and the rope reading is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(status_naturality_ambiguity, conceptual, 'Whether the suspension''s fixity is a structural feature of the framework or a constructed custodial arrangement.').

omega_variable(
    restoration_event_indexation,
    'Can human action affect the timing of the restoration (the ''hastened end'' tradition), or is the suspension purely event-indexed beyond human control?',
    'Doctrinal analysis of the hastened-end sources and how contemporary authorities weight human agency in restoration timing; observation of the standing and growth of restoration-preparation activity within the framework.',
    'If restoration is hastenable, the regime is partially a preparation regime — maintenance drifts toward preparation, extraction and theater rise, and the reading converges toward the archiving sibling''s structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restoration_event_indexation, conceptual, 'Whether the deferral''s terminal event is beyond human influence or partially responsive to human action.').

omega_variable(
    maintenance_functionality_drift,
    'Is the knowledge maintenance still functional — the law could resume and the knowledge would be deployable — or has it drifted toward commemorative performance?',
    'Curriculum and liturgical content analysis: the proportion of study directed at operative resumption versus commemoration, and a deployability test of whether the maintained knowledge would actually support a resumed service.',
    'Rising commemoration would push theater_ratio past the piton threshold and reclassify the regime toward piton despite the reading''s own lights; stable functionality keeps the rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maintenance_functionality_drift, empirical, 'Functional versus commemorative character of the knowledge maintenance.').

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading (messianic_suspension) of the temple_sacrifice_obligation kernel; what would the sibling readings change structurally, and where exactly is the disagreement located?',
    'Framework-internal adjudication (a restored practice or an acknowledged codified ruling on the obligation''s present status) or external comparative analysis of which reading the maintenance apparatus actually presupposes.',
    'The disagreement is located in whether the suspended obligation carries present normative force: the occupation sibling would give study obligative force, creating a real compliance surface with genuine extraction; the archiving sibling would thin the live-status claim, removing the liturgical and legal entailments and shrinking the regime toward pure archival practice. Adopting either sibling changes the beneficiary structure and the epsilon this story authors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: sibling readings of the same kernel would instantiate constraints with different victim/benefit structures and different epsilon.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__messianic_suspension, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 0, 0.08).
narrative_ontology:measurement(temp_tr_t4, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 4, 0.1).
narrative_ontology:measurement(temp_tr_t8, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 8, 0.12).
narrative_ontology:measurement(temp_tr_t12, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 12, 0.13).
narrative_ontology:measurement(temp_tr_t16, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 16, 0.14).
narrative_ontology:measurement(temp_tr_t20, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 20, 0.15).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(temp_be_t4, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 4, 0.09).
narrative_ontology:measurement(temp_be_t8, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 8, 0.09).
narrative_ontology:measurement(temp_be_t12, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 12, 0.08).
narrative_ontology:measurement(temp_be_t16, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 16, 0.08).
narrative_ontology:measurement(temp_be_t20, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 20, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(temp_su_t4, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 4, 0.08).
narrative_ontology:measurement(temp_su_t8, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 8, 0.08).
narrative_ontology:measurement(temp_su_t12, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 12, 0.07).
narrative_ontology:measurement(temp_su_t16, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 16, 0.07).
narrative_ontology:measurement(temp_su_t20, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 20, 0.07).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_obligation__messianic_suspension, identity_coordination).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__messianic_suspension, temple_sacrifice_obligation__study_as_occupation).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__messianic_suspension, temple_sacrifice_obligation__study_as_archiving).

% DUAL FORMULATION NOTE:
% The colloquial label 'the sacrificial obligations after the destruction' decomposes, per the epsilon-invariance principle, into at least three structurally distinct claims: (1) messianic_suspension — the obligation is in force but suspended pending an event no agent controls (this story: near-zero epsilon, no victim set, study as maintenance of knowledge-in-waiting); (2) study_as_occupation — study legitimately occupies the obligation, creating a present compliance surface with real burden and higher epsilon; (3) study_as_archiving — study preserves knowledge for restoration without the strong live-status claim, a thin regime. The suspension regime's maintenance apparatus (curricula, liturgy, custodial jurisdiction) is the upstream infrastructure both siblings presuppose; this story links both as downstream dependents, and the epsilon values differ because the readings disagree about whether the suspended obligation carries present normative force — not because one constraint is measured two ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
