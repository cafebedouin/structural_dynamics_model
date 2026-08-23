% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__performance_only, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: temple_sacrifice_commitment__performance_only
 *   human_readable: Temple Sacrifice Commitment — Performance-Only Reading (Standing Study-Based Maintenance Arrangement)
 *   domain: religious_law/commitment_system_theory
 *
 * SUMMARY:
 *   Since the destruction of the Second Temple, the sacrificial commandments
 *   persist in the halakhic corpus as fully articulated law that no one can
 *   presently perform. The standing arrangement under contest — and the
 *   referent of every authored value in this file — is the study-based regime
 *   that keeps that corpus in daily circulation: curricula, printed editions,
 *   daily liturgical recitation of the offering order, completion
 *   celebrations, and a small restoration-preparation sector. This story is
 *   ONE READING of the contested kernel temple_sacrifice_commitment: the
 *   performance_only reading, which holds that the commitment requires
 *   material instantiation and that study without performance is archival
 *   preservation of a defunct practice, not occupation of the commitment.
 *   Assessed by this reading's own lights, the standing arrangement is a
 *   low-extraction coordination mechanism: it preserves executable technical
 *   knowledge and trained personnel against an indeterminate suspension, at
 *   modest and largely willing cost, with no current victim set. The sibling
 *   readings (study_as_exercise, hybrid_preparatory, symbolic_transformation)
 *   are separate constraints in separate files; they are not folded into this
 *   classification. KEY AGENTS (by structural relationship): -
 *   torah_scholars_kodashim_specialists: Participant-beneficiary bearing the
 *   deepest opportunity cost (moderate / identity_locked) — decades of
 *   expertise invested in material they hold cannot discharge the obligation
 *   it references - talmudic_academies_publishers: Agenda-setter and receipt
 *   seat (organized / arbitrage) — administers curricula, editions, and
 *   completion cycles; collects the funding flows - observant_laity:
 *   Symmetric participant (organized / constrained) — recites, donates,
 *   receives continuity; diffuse individual leverage -
 *   restoration_movement_activists: Literalist beneficiary constituency
 *   (moderate / mobile) — takes material instantiation seriously enough to
 *   prepare for it - animal_welfare_and_pluralist_dissenters: Excluded critic
 *   (moderate / constrained) — would object to restoration-without-evolution;
 *   outside the deliberative table - commitment_system_analyst: Analytical
 *   observer (analytical / analytical) — sees the full four-reading structure
 *
 * KEY AGENTS:
 *   - - torah_scholars_kodashim_specialists: Participant-beneficiary bearing the deepest opportunity cost (moderate / identity_locked)
 *   - - talmudic_academies_publishers: Agenda-setter and receipt seat (organized / arbitrage) — administers and collects
 *   - - observant_laity: Symmetric participant (organized / constrained) — pays modestly, receives continuity
 *   - - restoration_movement_activists: Literalist beneficiary constituency (moderate / mobile) — prepares for actual performance
 *   - - animal_welfare_and_pluralist_dissenters: Excluded critic (moderate / constrained) — objects from outside the deliberative table
 *   - - commitment_system_analyst: Analytical observer (analytical / analytical) — sees the full kernel structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__performance_only, 0.21).
domain_priors:suppression_score(temple_sacrifice_commitment__performance_only, 0.12).
domain_priors:theater_ratio(temple_sacrifice_commitment__performance_only, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, extractiveness, 0.21).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, accessibility_collapse, 0.18).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__performance_only, rope).
narrative_ontology:human_readable(temple_sacrifice_commitment__performance_only, "Temple Sacrifice Commitment — Performance-Only Reading (Standing Study-Based Maintenance Arrangement)").
narrative_ontology:topic_domain(temple_sacrifice_commitment__performance_only, "religious_law/commitment_system_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__performance_only, '405208d6-8bad-4c17-86f6-80f0d4699db3').
narrative_ontology:cs_kernel_codification('405208d6-8bad-4c17-86f6-80f0d4699db3', fixed_text).
narrative_ontology:cs_authority_grounding('405208d6-8bad-4c17-86f6-80f0d4699db3', lineage).
narrative_ontology:cs_interpretation_layer_present('405208d6-8bad-4c17-86f6-80f0d4699db3').
narrative_ontology:cs_reading_relation('405208d6-8bad-4c17-86f6-80f0d4699db3', temple_sacrifice_commitment__study_as_exercise, forecloses).
narrative_ontology:cs_reading_relation('405208d6-8bad-4c17-86f6-80f0d4699db3', temple_sacrifice_commitment__hybrid_preparatory, forecloses).
narrative_ontology:cs_reading_relation('405208d6-8bad-4c17-86f6-80f0d4699db3', temple_sacrifice_commitment__symbolic_transformation, forecloses).
narrative_ontology:cs_axiom('405208d6-8bad-4c17-86f6-80f0d4699db3', foundational, material_instantiation_required_for_occupancy).
narrative_ontology:cs_axiom_status(material_instantiation_required_for_occupancy, holdable).
narrative_ontology:cs_axiom_grounding('405208d6-8bad-4c17-86f6-80f0d4699db3', material_instantiation_required_for_occupancy, deontological).
narrative_ontology:cs_axiom('405208d6-8bad-4c17-86f6-80f0d4699db3', foundational, study_without_performance_is_archival_not_occupying).
narrative_ontology:cs_axiom_status(study_without_performance_is_archival_not_occupying, holdable).
narrative_ontology:cs_axiom_grounding('405208d6-8bad-4c17-86f6-80f0d4699db3', study_without_performance_is_archival_not_occupying, conventional).
narrative_ontology:cs_axiom('405208d6-8bad-4c17-86f6-80f0d4699db3', secondary, restoration_readiness_depends_on_corpus_integrity).
narrative_ontology:cs_axiom_status(restoration_readiness_depends_on_corpus_integrity, holdable).
narrative_ontology:cs_axiom_grounding('405208d6-8bad-4c17-86f6-80f0d4699db3', restoration_readiness_depends_on_corpus_integrity, instrumental).
narrative_ontology:cs_reference_frame('405208d6-8bad-4c17-86f6-80f0d4699db3', binding_command_awaiting_material_conditions).
narrative_ontology:cs_drift_state('405208d6-8bad-4c17-86f6-80f0d4699db3', contemporary_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('405208d6-8bad-4c17-86f6-80f0d4699db3', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__performance_only, torah_scholars_kodashim_specialists).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__performance_only, talmudic_academies_publishers).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__performance_only, observant_laity).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__performance_only, restoration_movement_activists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__performance_only, observant_laity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Spend decades mastering the orders of the Mishnah and Talmud dealing with offerings — their measurements, disqualifications, and Temple procedure. Communities grant them stipends and standing, and their judgments shape how the material is taught. Leaving the specialty would mean surrendering hard-won expertise and the social identity built around it; most have never considered it.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, torah_scholars_kodashim_specialists, beneficiary,
    moderate, generational, identity_locked, global).

% Set curriculum calendars, commission commentaries, print the editions, and organize the completion celebrations that keep sacrificial material in daily circulation. Tuition, donations, and state study subsidies flow toward the institutions that teach it. Faculty and presses could be redeployed to other subjects within a few years if priorities changed.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, talmudic_academies_publishers, agenda_setter,
    organized, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_commitment__performance_only, talmudic_academies_publishers, beneficiary).

% Recite the passage describing the daily offering each morning, donate to the schools that teach the subject, and mark life-cycle events around study milestones. They receive continuity of inherited practice and the sense that the ancient service remains accounted for. Individual families hold little leverage over curriculum decisions and sustain the system through pooled giving.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, observant_laity, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_commitment__performance_only, observant_laity, payer).

% Run Jerusalem research institutes that reconstruct vessels, garments, and priestly procedures, train candidates of priestly descent, and publicize the possibility of renewing the service. Funding and volunteers come from diaspora supporters. Members joined the cause voluntarily and could withdraw without communal penalty, though few do.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, restoration_movement_activists, beneficiary,
    moderate, generational, mobile, regional).

% Argue that any renewed slaughter program without revised welfare standards would harm animals and alienate members whose conscience recoils from it. They publish critiques and petition outside the religious courts; their submissions rarely reach the deliberations that decide what renewal would look like. Some are themselves tradition-observant and feel the exclusion doubly.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, animal_welfare_and_pluralist_dissenters, excluded,
    moderate, biographical, constrained, global).

% Studies the arrangement from outside participation: how a commandment suspended for nineteen centuries is kept administratively alive, what each competing account of its status implies, and where the accounts diverge. Bears none of its costs and collects none of its flows.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, commitment_system_analyst, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_commitment__performance_only, talmudic_academies_publishers).
narrative_ontology:fixing_cost_class(temple_sacrifice_commitment__performance_only, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the executable technical form of the sacrificial system — texts, measurements, procedures, trained personnel — across an indeterminate suspension, and coordinates dispersed communities on a single authoritative version of the practice so that material performance remains possible if conditions ever arrive.
% TRANSFER_FUNCTION: Moves time, tuition, and philanthropy from observant households and students into academies, publications, and a small restoration-preparation sector; moves scholarly labor into corpus maintenance; moves no animals or material offerings at present.
% ABSENT_VOICES: Animal-welfare ethicists and pluralist critics would object that restoration planning proceeds without consent structures or welfare revision; non-Orthodox movements would contest the restoration premise itself; both sit outside the halakhic decision table that determines what any renewal would look like. Their absence is load-bearing: unanimity about the arrangement's benignity partly reflects who was never asked.
% DISAPPEARANCE_RATIONALE: If the study regime vanished overnight, corpus transmission would fragment within a generation or two, specialist procedure-knowledge would lapse beyond recovery, restoration would become practically impossible rather than merely deferred, and the daily liturgy would lose an anchor woven into standard practice. Communal arrangements demonstrably depend on the regime's continuation.
% FOUNDING_PROBLEM: Keep the executable form of a divinely commanded practice intact through the period after its central site was destroyed in 70 CE — preserving the commitment in suspension until material instantiation becomes possible again.
% FOUNDING_PROBLEM_CORROBORATION: That the founding problem existed is corroborated from outside the beneficiary set: Josephus's account of the destruction, Roman fiscal records (the fiscus Judaicus replacing temple dues), and the archaeological record of the destroyed site. That the problem REMAINS live in practical terms is attested by no source outside the benefiting parties — the liveness claim rests wholly on intra-traditional authority and liturgical self-description, and contemporary historians treat the restoration expectation as theological rather than operational. That asymmetry is itself signal, stated here plainly.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__performance_only, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__performance_only, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(temple_sacrifice_commitment__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__performance_only, 0.21, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__performance_only_tests).
:- end_tests(temple_sacrifice_commitment__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low (0.21 at interval end) because the arrangement takes no material goods from anyone: costs are scholarly time, tuition, and philanthropic flows into institutions participants regard as the point of their giving, and the willingness of the paying seats keeps effective extraction near the coordination-cost band. Suppression is low (0.12) because nothing bars exit — leaving the community or the specialty is socially expensive but not blocked, and the arrangement does not depend on enforcement to persist (requires_active_enforcement: false — intrinsic valuation of study carries it). Theater_ratio (0.41) is the most reading-sensitive figure: under the performance_only lights, the recitative placeholder layer (daily korbanot recitation, completion celebrations framed as covenantal acts, segulah-framed study) maintains the appearance of occupancy without occupying anything, and that layer has grown faster than the archival-preparatory layer across the interval — the series runs on one shared time grid (70, 400, 1000, 1500, 1800, 1948, 1967, 2026) with both tracked metrics authored at every point. Accessibility_collapse is low (0.18): three sibling readings plus secular exit remain fully live alternatives, and the kernel contest itself is the proof that alternatives have not collapsed. Resistance is moderate-high (0.55): the mainstream equivalence doctrine (study counted as offering) directly resists this reading's core claim, and ethical dissent resists the restoration trajectory this reading keeps open.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the specialist seat the arrangement feels like vocation, near costless — identity lock converts opportunity cost into chosen devotion, damping experienced extraction below what the ledger shows. From the academy seat it is a renewable program: arbitrage-grade exit means the same corpus is a portfolio line, redirectable within years. From the excluded ethicist seat the identical arrangement reads as a latent-violence pipeline — preserved procedure awaiting a trigger, with no welfare revision in the loop. Same-level lateral differentiation appears within the scholarly class itself: locked specialists versus students who have not yet fused identity with the material face very different exit prices at nominally equal standing. The engine derives these divergences from the structural data; the authored claim adjudicates nothing.
 *
 * DIRECTIONALITY LOGIC:
 *   Academies sit nearest the beneficiary pole: they administer the arrangement and collect its flows, with arbitrage exit damping any residual target-position. Specialists are beneficiaries pulled toward the center by identity lock — locked agents sit nearer the target end than their benefit flows alone would suggest, because exit pricing is what the derivation weighs. Laity sit near symmetric: modest outflows (donations, liturgical time), genuine inflows (continuity, meaning). Activists are near-beneficiary: purpose and funding flow in, exit is mobile. The excluded dissenters bear no extraction at all — the derivation may push an absent-party seat toward the target end, but no directionality override is authored because overrides key on the power atom, not the named agent, and an atom-level override would distort the moderate-power scholars and activists who genuinely occupy differentiated positions. The coarse-grain cost of leaving the dissenters to derivation is documented here rather than papered over.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification discipline matters here because the arrangement sits at the seam of two mislabeling traps. Read from a secular-analytic seat, a nineteen-century-old practice maintained by recitation and completion parties looks like a piton — defunct-function theater sustained by inertia. Read from inside the tradition it looks like costless devotion. Authoring the metrics independently of the claim keeps both errors out: the theater series (0.20 rising to 0.41) records the growing recitative layer honestly, while the low extraction and real archival-preparatory function block the piton verdict for now. The R5 mismatch consumer finds status contested paired with verdict world_rearranges — no zombie flag, because the parties genuinely dispute whether the founding problem is live rather than unanimously pretending it is. The forward risk is tracked, not assumed away: if restoration expectation institutionally dies while the recitative layer keeps growing, theater crosses the threshold at which the same structure recomputes as inertial maintenance, and the omega sunset_vs_steady_state is the tripwire.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which reading of the temple-sacrifice-commitment kernel correctly characterizes the standing arrangement — does study occupy, maintain-in-suspension, transform, or merely archive the commitment?',
    'Comparative structural evaluation across the four sibling constraint stories; decisive data would come from any restoration episode revealing whether the preserved procedure actually executes and whether participating communities accept material performance as the commitment''s occupation.',
    'Sibling readings assign different epsilon values and different victim sets to the same observable practice; resolving the contest would reassign classification across the entire constraint family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'This story instantiates one reading (performance_only) of the contested kernel; the standing arrangement''s status depends on which reading governs.').

omega_variable(
    restoration_victim_latency,
    'If material performance were renewed without ethical evolution, would a victim set emerge — slaughtered animals under pre-modern welfare assumptions, and conscience-bound refusers exposed to communal sanction?',
    'Inspection of restoration-planning output (welfare provisions in operational manuals, vessel and slaughter-site protocols) and modeling of sanction exposure for refusers under current communal enforcement norms.',
    'If yes, the constraint family acquires a latent extraction member that activates at restoration; the current no-victim profile would be an artifact of the suspension period only.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restoration_victim_latency, empirical, 'No current victim set exists; potential future victims under unconditional restoration are documented here rather than asserted as present victims.').

omega_variable(
    sunset_vs_steady_state,
    'Is the study arrangement transitional (terminating upon restoration, carrying a liturgically declared but operationally vague terminus) or steady-state (persisting regardless of restoration prospects), and does it drift toward theatrical self-maintenance if restoration hope institutionally fades?',
    'Track whether preparatory investment (training, vessel reconstruction, curriculum hours) correlates with restoration-proximity beliefs or continues invariantly; monitor theater-ratio trend under weakening eschatological expectation.',
    'A transitional reading supports scaffold-like dynamics with a declared terminus; a steady-state reading supports the rope claim; faded hope with climbing theater would indicate piton drift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_vs_steady_state, conceptual, 'Whether the arrangement''s justification is the transition itself or an enduring coordination function independent of restoration timing.').

omega_variable(
    specialist_identity_lock_mechanism,
    'How much of the kodashim specialists'' inability to leave the specialty is structural (stipend dependence, scarcity of alternative standing within the community) versus internalized (self-concept fused with mastery of the sacrificial corpus)?',
    'Post-exit trajectories of scholars who transferred to other tractates, entered secular academia, or left observant communities: does the felt compulsion persist after the structural ties are cut?',
    'If predominantly internalized, effective suppression exceeds the structural measure — the lock travels with the scholar after exit; if structural, stipend and status diversification would dissolve it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(specialist_identity_lock_mechanism, empirical, 'Structural versus internalized components of the identity lock on specialist scholars.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__performance_only, 70, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t70, temple_sacrifice_commitment__performance_only, theater_ratio, 70, 0.2).
narrative_ontology:measurement(temp_tr_t400, temple_sacrifice_commitment__performance_only, theater_ratio, 400, 0.22).
narrative_ontology:measurement(temp_tr_t1000, temple_sacrifice_commitment__performance_only, theater_ratio, 1000, 0.26).
narrative_ontology:measurement(temp_tr_t1500, temple_sacrifice_commitment__performance_only, theater_ratio, 1500, 0.31).
narrative_ontology:measurement(temp_tr_t1800, temple_sacrifice_commitment__performance_only, theater_ratio, 1800, 0.34).
narrative_ontology:measurement(temp_tr_t1948, temple_sacrifice_commitment__performance_only, theater_ratio, 1948, 0.36).
narrative_ontology:measurement(temp_tr_t1967, temple_sacrifice_commitment__performance_only, theater_ratio, 1967, 0.38).
narrative_ontology:measurement(temp_tr_t2026, temple_sacrifice_commitment__performance_only, theater_ratio, 2026, 0.41).

% Extraction over time
narrative_ontology:measurement(temp_be_t70, temple_sacrifice_commitment__performance_only, base_extractiveness, 70, 0.12).
narrative_ontology:measurement(temp_be_t400, temple_sacrifice_commitment__performance_only, base_extractiveness, 400, 0.13).
narrative_ontology:measurement(temp_be_t1000, temple_sacrifice_commitment__performance_only, base_extractiveness, 1000, 0.13).
narrative_ontology:measurement(temp_be_t1500, temple_sacrifice_commitment__performance_only, base_extractiveness, 1500, 0.15).
narrative_ontology:measurement(temp_be_t1800, temple_sacrifice_commitment__performance_only, base_extractiveness, 1800, 0.16).
narrative_ontology:measurement(temp_be_t1948, temple_sacrifice_commitment__performance_only, base_extractiveness, 1948, 0.17).
narrative_ontology:measurement(temp_be_t1967, temple_sacrifice_commitment__performance_only, base_extractiveness, 1967, 0.19).
narrative_ontology:measurement(temp_be_t2026, temple_sacrifice_commitment__performance_only, base_extractiveness, 2026, 0.21).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(temple_sacrifice_commitment__performance_only, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__performance_only, information_standard).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment__study_as_exercise).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment__hybrid_preparatory).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment__symbolic_transformation).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'the sacrificial commitment' conflates four structurally distinct claims about what occupies the commitment during suspension. Each reading is authored as its own story with its own stable epsilon over the same standing arrangement. This member carries the materialist-maximalist epsilon; the study_as_exercise member assigns near-zero extraction to the same practices, the symbolic_transformation member re-describes them as instantiation itself, and hybrid_preparatory splits the difference as suspended maintenance. Upstream/downstream: the performance_only and hybrid_preparatory members jointly supply the restoration-premise that the symbolic and study-as-exercise members deny; contamination propagates along the affects_constraints edges when any member's purity shifts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
