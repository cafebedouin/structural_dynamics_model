% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_inevitability__path_dependency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_inevitability__path_dependency_reading, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: qwerty_persistence_inevitability__path_dependency_reading
 *   human_readable: QWERTY Layout Persistence (Path-Dependency Reading)
 *   domain: technology_history/political_economy
 *
 * SUMMARY:
 *   The QWERTY keyboard layout, adopted by Remington in the 1870s and
 *   universalized through successive generations of typewriters and then
 *   computers, persists as the dominant input mapping despite long-standing
 *   claims that alternative layouts offer superior efficiency. This story
 *   instantiates the PATH-DEPENDENCY READING of the contested kernel
 *   qwerty_persistence_inevitability: the layout persists through historical
 *   accident plus network-effect switching costs, with no strategic
 *   beneficiaries and no administered enforcement — manufacturers build
 *   QWERTY because users demand it, users learn QWERTY because everyone else
 *   knows it. On this reading the efficiency loss, if real, is a diffuse
 *   externality collected by no one. The epsilon referent is the standing
 *   QWERTY arrangement assessed by this reading's own lights: low extraction,
 *   near-zero suppression, no maintenance theater. The sibling reading
 *   (strategic_lock_in_reading) is a separate constraint story with its own
 *   epsilon, beneficiaries, and victims; per the epsilon-invariance principle
 *   the two are linked through network.affects_constraints, not merged here.
 *   KEY AGENTS (by structural relationship): - qwerty_trained_typists:
 *   bearers of the diffuse efficiency cost (powerless/constrained) — no
 *   proceeds from their position flow to anyone -
 *   keyboard_hardware_manufacturers: passive recipients of a standardization
 *   dividend (institutional/mobile) — respond to demand, administer nothing -
 *   large_office_employers: organizational carriers of training-path
 *   dependence (organized/constrained) - alternative_layout_advocates: voice
 *   without a venue (moderate/constrained) - national_standards_bodies:
 *   after-the-fact ratifiers holding the only analytical seat
 *   (institutional/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_inevitability__path_dependency_reading, 0.1).
domain_priors:suppression_score(qwerty_persistence_inevitability__path_dependency_reading, 0.06).
domain_priors:theater_ratio(qwerty_persistence_inevitability__path_dependency_reading, 0.06).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, extractiveness, 0.1).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 0.06).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 0.06).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_inevitability__path_dependency_reading, mountain).
narrative_ontology:human_readable(qwerty_persistence_inevitability__path_dependency_reading, "QWERTY Layout Persistence (Path-Dependency Reading)").
narrative_ontology:topic_domain(qwerty_persistence_inevitability__path_dependency_reading, "technology_history/political_economy").

domain_priors:emerges_naturally(qwerty_persistence_inevitability__path_dependency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_inevitability__path_dependency_reading, '15f46eb3-85e7-413f-aedb-608f3f4890ff').
narrative_ontology:cs_kernel_codification('15f46eb3-85e7-413f-aedb-608f3f4890ff', formalized).
narrative_ontology:cs_authority_grounding('15f46eb3-85e7-413f-aedb-608f3f4890ff', self_enforcing).
narrative_ontology:cs_reading_relation('15f46eb3-85e7-413f-aedb-608f3f4890ff', qwerty_persistence_inevitability__strategic_lock_in_reading, coexists_with).
narrative_ontology:cs_axiom('15f46eb3-85e7-413f-aedb-608f3f4890ff', foundational, persistence_requires_no_strategic_maintenance).
narrative_ontology:cs_axiom_status(persistence_requires_no_strategic_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('15f46eb3-85e7-413f-aedb-608f3f4890ff', persistence_requires_no_strategic_maintenance, empirically_contingent).
narrative_ontology:cs_axiom('15f46eb3-85e7-413f-aedb-608f3f4890ff', foundational, network_effects_suffice_for_lockin).
narrative_ontology:cs_axiom_status(network_effects_suffice_for_lockin, holdable).
narrative_ontology:cs_axiom_grounding('15f46eb3-85e7-413f-aedb-608f3f4890ff', network_effects_suffice_for_lockin, empirically_contingent).
narrative_ontology:cs_reference_frame('15f46eb3-85e7-413f-aedb-608f3f4890ff', accidental_origin_network_equilibrium).
narrative_ontology:cs_drift_state('15f46eb3-85e7-413f-aedb-608f3f4890ff', contemporary_input_method_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('15f46eb3-85e7-413f-aedb-608f3f4890ff', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_inevitability__path_dependency_reading, qwerty_persistence_inevitability).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__path_dependency_reading, keyboard_hardware_manufacturers).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__path_dependency_reading, qwerty_trained_typists).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__path_dependency_reading, large_office_employers).
narrative_ontology:constraint_vindicates(qwerty_persistence_inevitability__path_dependency_reading, path_dependency_theory).
narrative_ontology:constraint_vindicates(qwerty_persistence_inevitability__path_dependency_reading, network_externality_lockin_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Learned the layout as children or new hires because everyone around them already used it. Switching individually means weeks of retraining for zero personal payoff while every shared keyboard, coworker, and form stays on the old layout. They bear whatever efficiency cost the layout carries, spread so thinly across billions of users that no one experiences it as a line item.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, qwerty_trained_typists, payer,
    powerless, biographical, constrained, global).

% Build keyboards and laptops to the QWERTY legend because that is what buyers already know how to type on. They chose no part of the layout's history and operate no machinery that keeps it in place; producing an alternative layout is technically trivial and commercially unrewarding. The common standard spares them variant SKUs and support burden — a dividend received passively, lost only if the whole installed base moved together.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, keyboard_hardware_manufacturers, beneficiary,
    institutional, generational, mobile, global).

% Buy QWERTY equipment and train staff on it because the labor market arrives pre-trained. Retraining a workforce onto another layout would cost months of productivity against savings no accounting line can see. Their purchasing repeats the incumbent choice every quarter without anyone deciding anything.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, large_office_employers, payer,
    organized, biographical, constrained, global).

% Promote alternative layouts, publish comparisons, and distribute alternative keymaps and labeled keyboards. Their proposals are heard, occasionally studied, and adopted by scattered enthusiasts; there is no body with authority over keyboard legends to petition, so their objection registers nowhere that binds anyone.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, alternative_layout_advocates, excluded,
    moderate, biographical, constrained, global).

% Codified the layout after the fact as a national and international standard, recording what the market had already settled. They could in principle recommend a different layout; no manufacturer or user community would follow, so the codification tracks practice rather than steering it.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, national_standards_bodies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence_inevitability__path_dependency_reading, diffuse).
narrative_ontology:fixing_cost_class(qwerty_persistence_inevitability__path_dependency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single shared input mapping: one learned skill connects every typist to every keyboard, and one hardware convention lets every manufacturer build for every user. The coordination problem solved is universal interoperability of text-entry skill and equipment.
% TRANSFER_FUNCTION: Moves essentially nothing on an ongoing basis — no fee, service, or attention stream flows from any seat to any other. What it fixes in place is a sunk skill investment: each new typist's learning effort is committed to the incumbent layout, and a diffuse compatibility assurance flows to all hardware and software makers equally.
% ABSENT_VOICES: Alternative-layout advocates object but hold no seat anywhere — under this reading there is no decision forum to be excluded from; the only 'conversation' is the market, where their proposals appear as niche products. Ergonomics researchers who document strain and efficiency questions publish into the same void. Their absence is structural: the arrangement has no agenda to petition.
% DISAPPEARANCE_RATIONALE: If QWERTY vanished overnight, every physical keyboard legend, operating-system keymap, typing curriculum, and several hundred million people's muscle memory would be orphaned simultaneously; text entry would degrade worldwide until a successor standard propagated through the same adoption dynamics. The world depends on the arrangement even though no one administers it.
% FOUNDING_PROBLEM: Mechanical typebar jamming in 1860s-70s typewriters: striking adjacent keys in rapid succession made the typebars collide. The Sholes/Remington layout was arranged around the mechanics of the early machines.
% FOUNDING_PROBLEM_CORROBORATION: History-of-technology scholarship and Sholes' own patents and correspondence document the jamming problem independently of any manufacturer's commercial interest; under this reading there is no benefiting party whose testimony would be suspect, and the dead-status finding rests on the trivial observation that electronic key switches cannot jam.
narrative_ontology:disappearance_verdict(qwerty_persistence_inevitability__path_dependency_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_inevitability__path_dependency_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_inevitability__path_dependency_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qwerty_persistence_inevitability__path_dependency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_inevitability__path_dependency_reading, 0.1, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_inevitability__path_dependency_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, ExtMetricName, E),
    domain_priors:suppression_score(qwerty_persistence_inevitability__path_dependency_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(qwerty_persistence_inevitability__path_dependency_reading),
    narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(qwerty_persistence_inevitability__path_dependency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored low (0.10) because this reading locates no collector: the efficiency cost, if real, accrues to no seat. Suppression is 0.06 — there is no enforcement machinery to suppress anything; only the passive arithmetic of switching costs, which is a structural property of adoption dynamics, not applied force. Theater is 0.06: nobody performs the layout; it simply persists, and the small residual reflects post-hoc rationalizations accumulating in marketing and pedagogy. Accessibility_collapse is 0.78 — once network effects are understood, alternatives collapse for the median user, though individuals demonstrably do defect (Colemak programmers, Dvorak enclaves), placing this below natural-law completeness (~0.85+). Resistance is 0.15: advocacy exists, alternative products ship, nothing binds. The mountain claim follows the reading's own frame: given the initial condition, persistence is the automatic equilibrium of adoption dynamics — a structural feature of the system rather than a current human choice; emerges_naturally:true encodes emergence-from-network-dynamics, not physical law. One honest tension is recorded rather than reconciled: the receipt surface (fixing_cost prohibitive + gain_flow diffuse) is the signature the apparatus associates with inertial vestiges, yet this reading claims mountain because there is no administrator, no performed atrophied function, and no concentrated beneficiary. Claim and metrics are authored independently; the engine measures the divergence. The measurement series run on one shared time grid (seven points, both tracked metrics at every point) so temporal analysis samples a complete matrix; suppression_requirement is deliberately not tracked because no enforcement capacity ever existed to ratchet or decay — the static picture lives in the scalar.
 *
 * PERSPECTIVAL GAP:
 *   Within this reading, seat divergence is muted — nearly every participant experiences the arrangement as background fact. The sharpest internal asymmetry: typists bear switching costs individually with no collective-action vehicle, while manufacturers receive a passive compatibility dividend without administering anything. The major perspectival divide is BETWEEN readings, not within seats: the strategic_lock_in sibling would compute sharp divergence (agenda-setting manufacturers versus trapped typists); this reading computes near-uniform symmetry clustered around d=0.5. The engine classifies whichever structure the file declares — that contrast across the two files is the measurement the family exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries or victims are declared in base_properties because this reading asserts there are none in the extraction sense — declaring them would fabricate the very strategic structure the reading denies. Because the derivation chain has no beneficiary/victim data to read, directionality overrides encode the mild residual asymmetry directly: typists slightly target-side (0.58 — they carry the learning-cost externality), employers marginally so (0.52), manufacturers slightly beneficiary-side (0.42 — the standardization dividend lands on them without their seeking it). All values cluster near symmetric, so effective extraction should compute near zero for every seat, consistent with the mountain claim. Suppression is authored as a raw structural property (0.06) and is not scaled by power or scope; only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — mechanical typebar jam avoidance — died with the electromechanical keyboard, yet the arrangement persists. The R5 mismatch (founding_problem_status dead + disappearance_verdict world_rearranges) will flag a vestige, and the flag is correct that this is a vestige. But the receipt surface shows no capturer and the stakeholder set shows no agenda_setter: this is inertia without an administrator. That triad — dead function, no capturer, no administrator — is why this reading claims mountain rather than piton or snare: a piton requires someone theatrically maintaining the remnant, and a snare requires someone collecting from it; this reading asserts neither exists. If the sibling reading is right, the same mismatch resolves instead as capture, and the classification migrates accordingly. The mandatrophy machinery thus functions here as the hinge between the two readings of the kernel.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_contest_strategic_agency,
    'Is QWERTY persistence accident-driven path dependency (this reading) or manufacturer-engineered lock-in (sibling reading strategic_lock_in_reading)?',
    'Historical-archive research: business records of typewriter and keyboard manufacturers, any surviving training-partnership contracts, cartel correspondence, or standardization agreements; the absence of such records across a century of an intensely documented industry would support this reading.',
    'If strategic maintenance is found, this story''s structural data is wrong: beneficiaries and victims must be declared, epsilon rises sharply, and the classification migrates from mountain toward the sibling''s extractive profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_strategic_agency, empirical, 'Whether the persistence mechanism involves strategic agency at all — the locus of the kernel contest.').

omega_variable(
    inefficiency_premise_fable_of_keys,
    'Is QWERTY actually less efficient than the alternatives (Dvorak, Colemak), or is the presumed efficiency differential itself a myth (the Liebowitz-Margolis ''fable of the keys'' critique)?',
    'Controlled longitudinal studies of typing speed, error rate, and musculoskeletal outcomes across layouts, matched for training hours and learner aptitude.',
    'If the differential is negligible, the diffuse externality shrinks toward zero, epsilon falls further, and the arrangement looks closer to a genuine coordination standard than a costly lock-in; the mountain claim survives but the residual-cost story collapses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inefficiency_premise_fable_of_keys, empirical, 'Whether the arrangement imposes any real efficiency cost at all.').

omega_variable(
    necessity_vs_contingency_of_lockin,
    'Given the initial condition, is QWERTY-style persistence a necessary equilibrium of adoption dynamics (structural inevitability), or a contingent outcome that deliberate coordination could have overturned at identifiable junctures?',
    'Formal modeling of switching-cost thresholds against observed transition episodes (national layout variants such as AZERTY and QWERTZ, the Dvorak navy-training episode, modern programmable-keyboard adoption), plus counterfactual analysis of coordinated-transition proposals.',
    'If transitions were feasible at identifiable junctures, the arrangement is constructed rather than natural, emerges_naturally fails, and the mountain certification chain breaks — the persistence becomes an unremedied coordination failure rather than an inevitability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(necessity_vs_contingency_of_lockin, conceptual, 'Whether the persistence is structurally inevitable or historically contingent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_inevitability__path_dependency_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t0, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 0, 0.02).
narrative_ontology:measurement_basis(qwer_tr_t0, observed).
narrative_ontology:measurement(qwer_tr_t25, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 25, 0.02).
narrative_ontology:measurement_basis(qwer_tr_t25, observed).
narrative_ontology:measurement(qwer_tr_t50, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 50, 0.03).
narrative_ontology:measurement_basis(qwer_tr_t50, observed).
narrative_ontology:measurement(qwer_tr_t75, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 75, 0.04).
narrative_ontology:measurement_basis(qwer_tr_t75, observed).
narrative_ontology:measurement(qwer_tr_t100, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 100, 0.04).
narrative_ontology:measurement_basis(qwer_tr_t100, observed).
narrative_ontology:measurement(qwer_tr_t125, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 125, 0.05).
narrative_ontology:measurement_basis(qwer_tr_t125, observed).
narrative_ontology:measurement(qwer_tr_t150, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 150, 0.06).
narrative_ontology:measurement_basis(qwer_tr_t150, observed).

% Extraction over time
narrative_ontology:measurement(qwer_be_t0, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 0, 0.04).
narrative_ontology:measurement_basis(qwer_be_t0, observed).
narrative_ontology:measurement(qwer_be_t25, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 25, 0.05).
narrative_ontology:measurement_basis(qwer_be_t25, observed).
narrative_ontology:measurement(qwer_be_t50, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 50, 0.07).
narrative_ontology:measurement_basis(qwer_be_t50, observed).
narrative_ontology:measurement(qwer_be_t75, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 75, 0.08).
narrative_ontology:measurement_basis(qwer_be_t75, observed).
narrative_ontology:measurement(qwer_be_t100, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 100, 0.09).
narrative_ontology:measurement_basis(qwer_be_t100, observed).
narrative_ontology:measurement(qwer_be_t125, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 125, 0.1).
narrative_ontology:measurement_basis(qwer_be_t125, observed).
narrative_ontology:measurement(qwer_be_t150, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 150, 0.1).
narrative_ontology:measurement_basis(qwer_be_t150, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(qwerty_persistence_inevitability__path_dependency_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_inevitability__path_dependency_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence_inevitability__path_dependency_reading, strategic_lock_in_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'QWERTY persistence' decomposes, per the epsilon-invariance principle, into two structurally distinct claims sharing one label: this story (path_dependency_reading — accident plus network effects, no strategic agency, low epsilon, claimed mountain) and strategic_lock_in_reading (engineered lock-in with manufacturer beneficiaries and typist victims, substantially higher epsilon). This reading is the baseline account against which any strategic-agency claim must show added explanatory work; the sibling story links back. Neither file averages over the other — each carries its own epsilon, its own beneficiary/victim structure, and its own classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(qwerty_persistence_inevitability__path_dependency_reading, powerless, 0.58).
constraint_indexing:directionality_override(qwerty_persistence_inevitability__path_dependency_reading, organized, 0.52).
constraint_indexing:directionality_override(qwerty_persistence_inevitability__path_dependency_reading, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
