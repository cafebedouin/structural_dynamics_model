% ============================================================================
% CONSTRAINT STORY: competence_exercise_validity__real_catastrophe_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_validity__real_catastrophe_only, []).

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
 *   constraint_id: competence_exercise_validity__real_catastrophe_only
 *   human_readable: Real-Catastrophe-Only Doctrine of Competence Validation
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   In high-hazard sectors — nuclear operations, commercial aviation,
 *   emergency medicine, firefighting — an influential doctrine holds that
 *   only surviving a real catastrophe genuinely exercises and proves
 *   operational competence; simulation, however sophisticated, is dismissed
 *   as an insufficient substitute. The doctrine shapes training budgets (lean
 *   drill calendars), promotion rules (event participation weighted over
 *   validated simulator performance), staffing mandates (seniors-with-scars
 *   on every critical roster), and the public meaning of a clean safety
 *   record. Its structural consequence, under this reading's own operation:
 *   retained competence stays untested between real events; clean records
 *   lose probative value (indistinguishable from luck or redundancy depth);
 *   and the decay signals that simulator telemetry would surface are never
 *   collected — the mask is the dismissal itself. The claim/metric gap is
 *   deliberate and load-bearing: the constraint is CLAIMED as tangled_rope
 *   because the transfer gap it honors is real, while the authored metrics
 *   describe substantially extractive, actively enforced operation. The
 *   engine measures the divergence; the claim is not reconciled to the
 *   metrics. Family note: per the epsilon-invariance principle this is one of
 *   three linked stories decomposing the colloquial label 'competence must be
 *   exercised'; each reading gets its own epsilon, beneficiaries, and type,
 *   and each file links its siblings via network.affects_constraints. KEY
 *   AGENTS (by structural relationship): - scar_tissue_senior_operators:
 *   Primary beneficiary (powerful/arbitrage) — collects the authority and
 *   compensation premium the doctrine sustains -
 *   incident_investigation_specialists: Secondary beneficiary
 *   (organized/arbitrage) — pipeline scales with the real-event stream the
 *   doctrine consecrates - junior_and_midcareer_operators: Primary target
 *   (moderate/identity_locked) — advancement gated on surviving real events;
 *   bears front-line exposure - downstream_affected_publics: Diffuse target
 *   (powerless/trapped) — carries unpriced tail risk during untested windows
 *   - executive_adopters: Agenda-setter (institutional/mobile) — ratifies the
 *   doctrine, banks the deferral, rotates out before failures land -
 *   simulation_training_industry: Excluded challenger (organized/arbitrage) —
 *   holds the counter-evidence, locked out of policy rooms -
 *   sector_regulators: Analytical observer (institutional/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_validity__real_catastrophe_only, 0.66).
domain_priors:suppression_score(competence_exercise_validity__real_catastrophe_only, 0.62).
domain_priors:theater_ratio(competence_exercise_validity__real_catastrophe_only, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, extractiveness, 0.66).
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_validity__real_catastrophe_only, tangled_rope).
narrative_ontology:human_readable(competence_exercise_validity__real_catastrophe_only, "Real-Catastrophe-Only Doctrine of Competence Validation").
narrative_ontology:topic_domain(competence_exercise_validity__real_catastrophe_only, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_exercise_validity__real_catastrophe_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_validity__real_catastrophe_only, '9d1191ce-cc92-4aad-a7a9-9ada5887812d').
narrative_ontology:cs_kernel_codification('9d1191ce-cc92-4aad-a7a9-9ada5887812d', distributed).
narrative_ontology:cs_authority_grounding('9d1191ce-cc92-4aad-a7a9-9ada5887812d', practice).
narrative_ontology:cs_interpretation_layer_present('9d1191ce-cc92-4aad-a7a9-9ada5887812d').
narrative_ontology:cs_reading_relation('9d1191ce-cc92-4aad-a7a9-9ada5887812d', competence_exercise_validity__simulation_as_proxy, forecloses).
narrative_ontology:cs_reading_relation('9d1191ce-cc92-4aad-a7a9-9ada5887812d', competence_exercise_validity__continuous_refresh_hybrid, forecloses).
narrative_ontology:cs_axiom('9d1191ce-cc92-4aad-a7a9-9ada5887812d', foundational, only_real_catastrophe_validates_competence).
narrative_ontology:cs_axiom_status(only_real_catastrophe_validates_competence, holdable).
narrative_ontology:cs_axiom_grounding('9d1191ce-cc92-4aad-a7a9-9ada5887812d', only_real_catastrophe_validates_competence, empirically_contingent).
narrative_ontology:cs_axiom('9d1191ce-cc92-4aad-a7a9-9ada5887812d', secondary, simulated_stress_underrepresents_true_conditions).
narrative_ontology:cs_axiom_status(simulated_stress_underrepresents_true_conditions, holdable).
narrative_ontology:cs_axiom_grounding('9d1191ce-cc92-4aad-a7a9-9ada5887812d', simulated_stress_underrepresents_true_conditions, empirically_contingent).
narrative_ontology:cs_reference_frame('9d1191ce-cc92-4aad-a7a9-9ada5887812d', catastrophe_sole_valid_exercise).
narrative_ontology:cs_drift_state('9d1191ce-cc92-4aad-a7a9-9ada5887812d', high_fidelity_simulation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9d1191ce-cc92-4aad-a7a9-9ada5887812d', '').
narrative_ontology:cs_kernel_id(competence_exercise_validity__real_catastrophe_only, competence_exercise_validity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_validity__real_catastrophe_only, scar_tissue_senior_operators).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__real_catastrophe_only, incident_investigation_specialists).
narrative_ontology:constraint_victim(competence_exercise_validity__real_catastrophe_only, junior_and_midcareer_operators).
narrative_ontology:constraint_victim(competence_exercise_validity__real_catastrophe_only, downstream_affected_publics).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__real_catastrophe_only, executive_adopters).
narrative_ontology:constraint_vindicates(competence_exercise_validity__real_catastrophe_only, simulation_transfer_limitation_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Senior operators, engineers, and commanders whose careers include direct participation in a major incident — a scram beyond the simulator envelope, a recovered in-flight emergency, a mass-casualty night. Their event histories anchor incident reviews, justify mandated senior staffing on shift rosters, and command speaking and consulting premiums. Leaving an employer is easy: the event portfolio travels with them and is bid for across the sector.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, scar_tissue_senior_operators, beneficiary,
    powerful, biographical, arbitrage, continental).

% Accident-board investigators, forensic engineers, and expert witnesses whose caseload scales with the frequency of real events. Each catastrophe generates investigation panels, reconstruction contracts, publications, and testimony fees. They did not author the doctrine, but their professional pipeline depends on the event stream the doctrine treats as the indispensable teacher, and several serve as its most quotable public voices.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, incident_investigation_specialists, beneficiary,
    organized, biographical, arbitrage, continental).

% Licensed trainees, first officers, residents, and shift candidates working under staffing and promotion rules that weight real-event participation. Without a qualifying real event, accumulated simulator hours are discounted in credential reviews and advancement stalls until chance supplies an incident. Most entered the vocation as a calling and experience departure as self-erasure; meanwhile they hold the front-line console when an unexercised failure mode finally presents.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, junior_and_midcareer_operators, payer,
    moderate, biographical, identity_locked, national).

% Residents under flight paths, beside plant fences, and within hospital catchments. They carry the tail risk of crews whose readiness has gone unexercised under real conditions during long quiet intervals, and they hold no seat in training-budget or promotion-rule decisions. Relocating away from the exposure is costly and rarely contemplated.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, downstream_affected_publics, payer,
    powerless, generational, trapped, regional).

% Plant directors, chief medical officers, and fleet chiefs who ratify the training budgets, roster rules, and promotion criteria that encode the doctrine. Invoking it lets them run lean drill calendars and defer validation spending, which shows up as present-year margin. Typical tenure means most will have rotated to another post before any deferred failure arrives on their watch.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, executive_adopters, agenda_setter,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_validity__real_catastrophe_only, executive_adopters, beneficiary).

% Simulator manufacturers, training academies, and human-factors research groups holding decades of transfer-validity evidence. Doctrine-adopting organizations keep them off training-policy committees and discount their product category in procurement; they sell to adjacent sectors and publish into venues the adopting community does not read.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, simulation_training_industry, excluded,
    organized, generational, arbitrage, global).

% Inspectors and rule-setters who audit staffing ratios, review post-event reports, and judge safety cases. They neither collect nor pay under the doctrine; their posture oscillates between accepting veteran-staffing mandates as compensating measures and asking whether competency that has never been exercised satisfies safety-case demonstration requirements.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, sector_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_validity__real_catastrophe_only, scar_tissue_senior_operators).
narrative_ontology:fixing_cost_class(competence_exercise_validity__real_catastrophe_only, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates scarce real-event experience into a recognized senior class and keeps organizations from reading simulator performance as proof of readiness for conditions simulators underrepresent — genuine physiological stress, degraded information environments, cascading multi-system failure.
% TRANSFER_FUNCTION: Moves authority, staffing mandates, and compensation premiums to operators with real-event histories; moves drill budgets away from simulation toward post-event investigation and veteran staffing; moves unpriced catastrophic risk to junior operators and downstream publics during the long windows in which retained competence goes unexercised; moves career legitimacy exclusively through survival of real events.
% ABSENT_VOICES: Simulation researchers, training-vendor engineers, and human-factors scientists hold the transfer-validity evidence and sit outside the policy rooms of doctrine-adopting organizations. Downstream publics would object to the unpriced tail risk if the untested-window implication were spelled out for them. Junior operators object internally but lack standing against the veteran gatekeepers who write the promotion criteria.
% DISAPPEARANCE_RATIONALE: Drill calendars would refill and promotion ladders would begin crediting validated simulation performance; veteran staffing mandates and event-history compensation premiums would unwind; the sector's feedback loop — training telemetry as an early-warning channel for skill decay — would reopen. Training economics, roster composition, and public-risk pricing all reorganize around whatever validation standard replaces the doctrine.
% FOUNDING_PROBLEM: Historical incidents in which crews and control rooms performed flawlessly in rehearsal yet failed on real-event features the simulators omitted, leading sectors to conclude that simulation breeds dangerous overconfidence and that only surviving genuine catastrophe teaches the missing competencies.
% FOUNDING_PROBLEM_CORROBORATION: Human-factors transfer-of-training literature and recurring accident-board findings ('performed to standard in simulation, degraded under real conditions') attest the founding problem from outside the benefiting parties — the veterans who collect event-history premiums are not the source of the corroborating record. No corroborating source outside the doctrine's defenders attests the further inferential step that simulation is therefore worthless; that step is the doctrine's own and is precisely where the sibling readings break from it.
narrative_ontology:disappearance_verdict(competence_exercise_validity__real_catastrophe_only, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_validity__real_catastrophe_only, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_validity__real_catastrophe_only, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_exercise_validity__real_catastrophe_only, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_validity__real_catastrophe_only, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_validity__real_catastrophe_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_validity__real_catastrophe_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_exercise_validity__real_catastrophe_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is substantial (0.66 at interval end) though largely non-monetary: the toll is taken in career access (advancement gated on event survival), deferred validation cost (decades of untested readiness), and risk transfer onto juniors and publics. It is not higher because the doctrine delivers something real — respect for a genuine transfer gap — which is the coordination half of the tangled-rope verdict. Suppression (0.62) is epistemic and budgetary, not physical: alternatives are delegitimated, procurement-discounted, and kept off committees rather than outlawed. Suppression is authored as a raw structural property and is not scaled by anything in the engine's arithmetic; only extractiveness is scaled, by directionality and scope. Theater ratio (0.38) reflects ritual thickening as real-event frequency declines: anniversary war-story ceremonies, token tabletop exercises scheduled and then discounted by the same doctrine that schedules them — performance increasingly substituting for the validating function the doctrine says only reality performs. Accessibility collapse (0.52) is honestly middling: the alternative framework (simulation-as-validation) is fully articulated and operating in adjacent sectors; within adopting organizations it is visible but institutionally subordinated, so collapse is local rather than general. Resistance (0.58): sustained professional pushback from the human-factors and training communities, intermittent regulatory inquiry, periodic post-accident challenges to veteran staffing orthodoxy. Measurement series run on one shared six-point grid (t=0,6,12,18,24,30) with every tracked metric authored at every point; all three trajectories rise for the same underlying reason — simulator fidelity grew across the interval, raising both the opportunity cost of dismissal (extractiveness) and the effort required to keep dismissing (suppression_requirement), while declining real-event frequency thinned the doctrine's functional content and thickened its ritual layer (theater_ratio). Coalition note: downstream publics are diffuse and generational, so their coalition potential is weak despite maximal per-capita stakes; the viable reform coalition is junior operators plus the excluded training industry, and its formation is the observable that would move resistance upward.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and the engine owns that computation. From the veteran seat the doctrine is hard-won epistemic discipline — the thing that stops an organization from confusing a flawless dress rehearsal with readiness; from the junior seat it is a ladder whose top rung is supplied only by catastrophe, experienced as both career blockade and vocational betrayal; from the public seat it is invisible except as background risk priced nowhere. Identity-lock dynamics on the junior seat: the binding mechanism is relational-professional fusion — membership in the cadre that earns scar credibility constitutes the operator's self-concept, so exit reads as self-erasure rather than job change; if the sector switched to validated-simulation credentialing, that frame would break, exit options would widen toward constrained/mobile, and the junior seat's directionality would drop materially. Same-level divergence: executives and regulators hold comparable institutional standing yet opposite relationships to the constraint — the executive's mobile exit converts the doctrine's deferred costs into someone else's tenure problem, while the regulator's analytical seat prices the deferral without bearing it.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. scar_tissue_senior_operators: declared beneficiary with arbitrage-grade exit — nearest the full-beneficiary end; the doctrine subsidizes them by making their asset class artificially scarce. incident_investigation_specialists: declared beneficiary whose revenue tracks the event stream the doctrine consecrates — low d, though they collect from occurrence, not from administration. junior_and_midcareer_operators: declared victim with identity_locked exit — pushed toward the full-target end; the lock means the extraction follows them into any attempt to leave. downstream_affected_publics: declared victim, powerless and trapped at regional scope — maximal target-end placement, with scope amplification since verification of their protection is hardest exactly where they live. executive_adopters: no array declaration; structurally split — short-run beneficiary through deferred costs, long-run exposed but shielded by mobile exit, so the derivation places them nearer symmetric with the mobility damping felt extraction. simulation_training_industry: excluded rather than coordinated — the doctrine's suppression object, feeding the suppression term rather than any seat's chi. sector_regulators: observer seat, no collection, no payment.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification disciplines two symmetrical mislabels. Read flat, the doctrine looks like pure obstruction — old operators defending turf against better tools (snare-flavored), which erases the documented transfer gap and the real incidents that founded the doctrine's credibility. Read generously, it looks like pure epistemic humility — a rope of proper caution — which erases the gatekeeping rents, the gated junior pipeline, and the risk transfer onto unconsulted publics. Tangled rope keeps both legs on the table and makes the empirical questions (gap magnitude, survivorship bias, record interpretability) the arbiters between them. Mandatrophy is NOT resolved and is not flagged: the founding problem (sim-real transfer gap) is corroborated as live by sources outside the benefiting parties, so the doctrine's mandate has not outlived its referent — what is contested is only the strong-form inference drawn from it. The receipt surface sharpens the picture: gains accrue demonstrably to the scar-tissue senior seat (gain_flow names it), and fixing is prohibitive for whoever could fix it — repudiating the doctrine means overriding the authority structure that writes promotion criteria, staffing mandates, and the sector's storytelling, at a cost no incumbent leadership pays absent a forcing disaster.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_sibling_delta,
    'This constraint instantiates the real_catastrophe_only reading of kernel competence_exercise_validity; what changes structurally if either sibling reading (simulation_as_proxy, continuous_refresh_hybrid) is adopted instead?',
    'Cross-file comparison of the three linked family stories: beneficiary sets, epsilon, and enforcement structure diverge by reading. Adoption of simulation_as_proxy dissolves the veteran authority premium and the investigator pipeline logic; adoption of continuous_refresh_hybrid preserves a reduced premium under a mandatory drill cadence with bounded deferral.',
    'Reading choice determines whether the extraction structure exists at all: under simulation_as_proxy the gatekeeping rents evaporate and the constraint collapses toward a thin humility norm; under the hybrid roughly half the authored extraction persists while the unbounded-deferral mechanism is replaced by a cadence obligation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_sibling_delta, conceptual, 'Committer-frame indexicality: classification is relative to which reading of the shared kernel is instantiated.').

omega_variable(
    transfer_gap_magnitude,
    'How large is the genuine simulation-to-reality transfer gap for rare-event competencies — stress response, ambiguous-cue recognition, cascade management?',
    'Transfer-of-training meta-analyses plus paired post-incident reconstructions comparing sim-drilled crews against event-experienced crews on matched failure modes.',
    'A small gap removes the doctrine''s coordination leg and pushes the computed classification toward snare (pure gatekeeping rent defended by a phantom epistemic claim); a large gap stabilizes the tangled-rope verdict and raises the justified floor of veteran authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transfer_gap_magnitude, empirical, 'Size of the real phenomenon the doctrine''s exclusivity claim leans on.').

omega_variable(
    survivorship_bias_in_scar_authority,
    'Does veteran event-experience actually predict team readiness, or is scar-tissue authority a survivorship artifact — the fatally wrong are unavailable to testify?',
    'Blinded cohort assessment of decision quality among event-experienced versus sim-trained operators on matched scenario sets, controlling for selection into event exposure.',
    'If artifact, the beneficiary seat collects premium without supplying the coordination good and the snare-side reading strengthens sharply; if predictive, veteran authority is a genuine if expensive coordination input and part of the authored extraction is the price of the service.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(survivorship_bias_in_scar_authority, empirical, 'Whether the doctrine''s privileged epistemic class carries real signal.').

omega_variable(
    clean_record_interpretability,
    'Under this doctrine, can a clean safety record ever be distinguished from luck or from redundancy depth, or is the record permanently uninterpretable — and does dismissing simulation telemetry guarantee that skill-decay signals are never collected?',
    'Probabilistic safety-case accounting that decomposes historical clean-record stretches into exposure-time base rates, redundancy contributions, and demonstrated recovery events; audit of whether decay-sensitive telemetry exists anywhere in the training chain.',
    'If uninterpretable, the doctrine damages the sector''s principal feedback channel and the extraction extends beyond careers into epistemic infrastructure (the ''record equals luck'' and ''simulation masks decay'' deltas are confirmed); if interpretable, those delta claims overstate and the doctrine''s cost falls.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(clean_record_interpretability, conceptual, 'Whether the doctrine leaves the organization any usable read on its own readiness.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_validity__real_catastrophe_only, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cev_real_cat_only_tr_t0, competence_exercise_validity__real_catastrophe_only, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cev_real_cat_only_tr_t6, competence_exercise_validity__real_catastrophe_only, theater_ratio, 6, 0.28).
narrative_ontology:measurement(cev_real_cat_only_tr_t12, competence_exercise_validity__real_catastrophe_only, theater_ratio, 12, 0.31).
narrative_ontology:measurement(cev_real_cat_only_tr_t18, competence_exercise_validity__real_catastrophe_only, theater_ratio, 18, 0.34).
narrative_ontology:measurement(cev_real_cat_only_tr_t24, competence_exercise_validity__real_catastrophe_only, theater_ratio, 24, 0.36).
narrative_ontology:measurement(cev_real_cat_only_tr_t30, competence_exercise_validity__real_catastrophe_only, theater_ratio, 30, 0.38).

% Extraction over time
narrative_ontology:measurement(cev_real_cat_only_be_t0, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(cev_real_cat_only_be_t6, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 6, 0.53).
narrative_ontology:measurement(cev_real_cat_only_be_t12, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 12, 0.57).
narrative_ontology:measurement(cev_real_cat_only_be_t18, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 18, 0.61).
narrative_ontology:measurement(cev_real_cat_only_be_t24, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 24, 0.64).
narrative_ontology:measurement(cev_real_cat_only_be_t30, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 30, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(cev_real_cat_only_su_t0, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(cev_real_cat_only_su_t6, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 6, 0.54).
narrative_ontology:measurement(cev_real_cat_only_su_t12, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 12, 0.58).
narrative_ontology:measurement(cev_real_cat_only_su_t18, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 18, 0.62).
narrative_ontology:measurement(cev_real_cat_only_su_t24, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 24, 0.65).
narrative_ontology:measurement(cev_real_cat_only_su_t30, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_validity__real_catastrophe_only, identity_coordination).
narrative_ontology:affects_constraint(competence_exercise_validity__real_catastrophe_only, simulation_as_proxy).
narrative_ontology:affects_constraint(competence_exercise_validity__real_catastrophe_only, continuous_refresh_hybrid).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language claim 'competence must be exercised to be retained' decomposes under the epsilon-invariance principle into three structurally distinct readings of the kernel competence_exercise_validity. This file instantiates real_catastrophe_only (epsilon 0.66, tangled_rope: genuine transfer-gap respect plus gatekeeping rents). simulation_as_proxy (drills count as valid exercise) authors a much lower epsilon over the same referent — its adoption dissolves the veteran authority premium entirely. continuous_refresh_hybrid (simulation necessary but not sufficient, continuous cadence required) sits between: partial premium, bounded deferral. The upstream member of the family is the empirical transfer-gap literature embedded in all three; this reading is downstream and most contested because its exclusivity claim outruns the evidence base it cites. All three files link one another via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
