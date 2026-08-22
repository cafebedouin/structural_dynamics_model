% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__nuclear_taboo_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__nuclear_taboo_reading, []).

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
 *   constraint_id: total_war_possibility_space__nuclear_taboo_reading
 *   human_readable: Nuclear Taboo on Total War — Normative Prohibition Reading
 *   domain: international/strategic/institutional
 *
 * SUMMARY:
 *   This constraint story instantiates the NUCLEAR TABOO READING of the
 *   contested kernel: total war possibility space. Under this reading,
 *   nuclear war was removed from the realm of the thinkable-and-acceptable
 *   not by material capability constraint (which remained constant post-1945)
 *   but by constructed normative prohibition. The taboo operates through
 *   institutional affirmation: nuclear-armed states publicly pledge that
 *   nuclear use is categorically illegitimate, treaty regimes codify this
 *   pledge, and non-proliferation enforcement punishes defection. The
 *   constraint persists because institutional actors continuously perform the
 *   norm. This reading diverges sharply from the
 *   deterrence_equilibrium_reading (which attributes war prevention to mutual
 *   vulnerability rather than taboo) and from the space_contraction_reading
 *   (which claims nuclear weapons made war cognitively unthinkable rather
 *   than merely normatively prohibited). The three readings share the kernel
 *   (nuclear weapons exist; total war became improbable) but attribute causal
 *   authority to different mechanisms.
 *
 * KEY AGENTS:
 *   - Nuclear weapon states: institutional agenda-setters maintaining the taboo through treaty adherence, diplomatic signaling, and rhetorical reaffirmation
 *   - Norm entrepreneurship institutions: scholarly communities, international organizations, and humanitarian networks that interpret and transmit the taboo
 *   - Civilian populations: existential beneficiaries whose survival depends on the taboo's enforcement
 *   - Non-nuclear states: strategic beneficiaries participating in non-proliferation regime
 *   - Strategic rationalist counternarrative: excluded analytical voice arguing deterrence, not taboo, prevents war
 *   - Potential norm defectors: systematically absent from official discourse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__nuclear_taboo_reading, 0.28).
domain_priors:suppression_score(total_war_possibility_space__nuclear_taboo_reading, 0.19).
domain_priors:theater_ratio(total_war_possibility_space__nuclear_taboo_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 0.19).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, accessibility_collapse, 0.81).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, resistance, 0.34).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__nuclear_taboo_reading, rope).
narrative_ontology:human_readable(total_war_possibility_space__nuclear_taboo_reading, "Nuclear Taboo on Total War — Normative Prohibition Reading").
narrative_ontology:topic_domain(total_war_possibility_space__nuclear_taboo_reading, "international/strategic/institutional").

domain_priors:requires_active_enforcement(total_war_possibility_space__nuclear_taboo_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__nuclear_taboo_reading, 'f791a272-9d5d-4fdd-82c3-9aee404ac4a2').
narrative_ontology:cs_kernel_codification('f791a272-9d5d-4fdd-82c3-9aee404ac4a2', distributed).
narrative_ontology:cs_authority_grounding('f791a272-9d5d-4fdd-82c3-9aee404ac4a2', distributed).
narrative_ontology:cs_reading_relation('f791a272-9d5d-4fdd-82c3-9aee404ac4a2', total_war_possibility_space__deterrence_equilibrium_reading, coexists_with).
narrative_ontology:cs_reading_relation('f791a272-9d5d-4fdd-82c3-9aee404ac4a2', total_war_possibility_space__space_contraction_reading, influences).
narrative_ontology:cs_axiom('f791a272-9d5d-4fdd-82c3-9aee404ac4a2', foundational, nuclear_use_categorically_illegitimate).
narrative_ontology:cs_axiom_status(nuclear_use_categorically_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('f791a272-9d5d-4fdd-82c3-9aee404ac4a2', nuclear_use_categorically_illegitimate, deontological).
narrative_ontology:cs_axiom('f791a272-9d5d-4fdd-82c3-9aee404ac4a2', foundational, norm_performance_constitutes_constraint).
narrative_ontology:cs_axiom_status(norm_performance_constitutes_constraint, holdable).
narrative_ontology:cs_axiom_grounding('f791a272-9d5d-4fdd-82c3-9aee404ac4a2', norm_performance_constitutes_constraint, instrumental).
narrative_ontology:cs_reference_frame('f791a272-9d5d-4fdd-82c3-9aee404ac4a2', comprehensive_nuclear_prohibition_via_taboo).
narrative_ontology:cs_drift_state('f791a272-9d5d-4fdd-82c3-9aee404ac4a2', contemporary_post_cold_war_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f791a272-9d5d-4fdd-82c3-9aee404ac4a2', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__nuclear_taboo_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, civilian_populations).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, international_order_beneficiaries).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, norm_entrepreneurship_institutions).
narrative_ontology:constraint_vindicates(total_war_possibility_space__nuclear_taboo_reading, norm_entrepreneurship_doctrine).
narrative_ontology:constraint_vindicates(total_war_possibility_space__nuclear_taboo_reading, rhetorical_power_theory).
narrative_ontology:constraint_vindicates(total_war_possibility_space__nuclear_taboo_reading, constructivist_institutionalism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Collectively maintain the norm against nuclear war use through diplomatic signaling, treaty adherence, and rhetorical reinforcement. The constraint's persistence depends on their continuous affirmation that nuclear use is categorically prohibited regardless of strategic advantage. They possess material capability to break the taboo but maintain it as a self-binding commitment — the constraint exists because they actively say it is real and enforce it through diplomatic isolation, sanctions, and alliance restructuring against violators.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, nuclear_weapon_states, agenda_setter,
    institutional, generational, constrained, global).

% International organizations, think tanks, humanitarian NGOs, and academic communities that articulate, transmit, and reinforce the nuclear taboo through scholarship, advocacy, and institutional practice. They benefit from the norm's existence as it validates their expertise and authority as interpretive guardians of international law and strategic wisdom.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, norm_entrepreneurship_institutions, beneficiary,
    institutional, generational, mobile, global).

% The primary material beneficiary: the taboo's enforcement directly prevents their annihilation in nuclear exchange. Their benefit is existential — the constraint keeps them alive. However, they possess no enforcement mechanism themselves; the taboo persists because institutional and state actors maintain it, not because civilians participate in its administration.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, civilian_populations, beneficiary,
    powerless, immediate, trapped, global).

% Benefit from the taboo's suppression of nuclear war as a strategic option — they avoid confrontation with existential weapons they cannot field. They reinforce the taboo through non-proliferation treaty participation and rhetorical alignment with nuclear powers' stated norm commitment, though their enforcement capacity is limited to diplomatic voice and adherence to collective arrangements.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_states, beneficiary,
    organized, generational, constrained, global).

% A school of thought arguing that nuclear war is deterred by material rationality (mutual vulnerability) not by normative taboo — that the constraint operates regardless of whether institutional actors affirm the norm, and that the norm is window-dressing on deterrent mechanics. This reading is present in strategic studies but systematically excluded from official policy discourse and treaty negotiation forums where the taboo's normative foundation is treated as foundational.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, strategic_rationalist_counternarrative, excluded,
    analytical, generational, analytical, global).

% Actors (rogue states, terrorist networks, or militarily desperate great powers) who would rationally use nuclear weapons if the taboo vanished or were credibly broken. They are excluded from the norm-maintenance conversation by definition — their inclusion would delegitimize the taboo by making the constraint seem conditional rather than categorical. Their silence is the constraint's evidence that the taboo persists.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, potential_norm_defectors, excluded,
    institutional, immediate, trapped, regional).

% Examines whether the taboo is causally efficacious (material facts about nuclear danger) or performative (institutional actors saying it is real and thereby making it real). Observes the feedback: the more credibly the taboo is performed, the less it is tested, and the less it is tested, the more credible it becomes.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, analytical_observer, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_possibility_space__nuclear_taboo_reading, diffuse).
narrative_ontology:fixing_cost_class(total_war_possibility_space__nuclear_taboo_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of mutual nuclear annihilation by establishing a norm that unites all nuclear-capable actors (and their allies) around the claim that nuclear use is categorically illegitimate regardless of military advantage. Reduces the computational complexity of nuclear strategy from 'would this be rational?' to 'is this allowed?' — coordinates behavior through categorical prohibition rather than equilibrium calculation.
% TRANSFER_FUNCTION: Transfers existential safety from nuclear-armed states to the global population. The transfer is not economic but existential: the constraint prevents the transfer of risk FROM nuclear powers TO civilians. Non-nuclear states gain strategic safety by participating in the regime; nuclear states gain legitimacy and predictability by affirming the norm.
% ABSENT_VOICES: Actors who would benefit from nuclear war (conquered territories seeking revenge, collapsing regimes choosing apocalypse, ideological movements seeking total civilizational reset) are absent by construction — the taboo's enforcement includes their systematic exclusion from diplomatic voice. Their potential case FOR nuclear use is never heard because the constraint's maintenance requires treating such voices as illegitimate.
% DISAPPEARANCE_RATIONALE: If the taboo vanished overnight — if nuclear-armed states stopped affirming it and began treating nuclear use as strategically thinkable — nuclear conflict would become materially possible within months. Strategic calculations would shift from 'never' to 'under what conditions?' Proliferation would accelerate as non-nuclear states sought deterrents. International institutions would reorganize around nuclear coexistence rather than non-proliferation. The distribution of power would shift away from conventional deterrence toward hedge strategies.
% FOUNDING_PROBLEM: The material fact of nuclear weapons created an existential coordination problem: mutual capability to annihilate meant that rational strategic choice could lead to species-level extinction. Early Cold War strategists (Schelling, Kahn, McNamara) framed this as a technical problem solvable through deterrence equilibrium. The taboo reading treats it as a coordination problem solvable through norm construction: if all actors can be brought to affirm that nuclear use is simply illegitimate — not strategically disfavored, not irrational, but categorically prohibited — then the constraint becomes self-enforcing through reputation and alliance rather than fragile equilibrium.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (material existential risk from nuclear weapons) is corroborated by weapons physicists and strategic analysts independent of the taboo's beneficiaries. Multiple sources external to norm-entrepreneurship institutions (military strategists, defense ministries, proliferation scholars) attest that nuclear weapons remain materially capable of extinction-level damage. The corroboration is strong for the problem; the contested part is whether the taboo (as opposed to deterrence) solves it.
narrative_ontology:disappearance_verdict(total_war_possibility_space__nuclear_taboo_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__nuclear_taboo_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__nuclear_taboo_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(total_war_possibility_space__nuclear_taboo_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__nuclear_taboo_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__nuclear_taboo_reading_tests).
:- end_tests(total_war_possibility_space__nuclear_taboo_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is LOW (0.28 at interval end) because the taboo does not concentrate gains in a small group — the beneficiary set is diffuse (civilian populations, international order participants). Suppression is LOW (0.19) because the taboo works through affirmation and commitment rather than coercive exclusion; resistance is MODERATE (0.34) because strategic rationalists and rising powers periodically contest the norm's causal efficacy. Theater is MODERATE-HIGH (0.42) because maintaining the taboo requires continuous performative reaffirmation — treaty signings, pledges, institutional ceremonies — that constitute a growing fraction of the constraint's maintenance activity as time passes. The measurement series show extractiveness and theater both rising over the 75-year interval: as memory of nuclear war's existential threat fades and as the constraint becomes harder to test (no nuclear use occurs, making the taboo's causal role empirically ambiguous), more institutional energy goes into performing the norm rather than defending it materially. The constraint approaches piton territory (high theater, low extractiveness, persistent inertia) without crossing the threshold because the existential benefit to civilians keeps renewed institutional commitment flowing.
 *
 * PERSPECTIVAL GAP:
 *   The nuclear-armed states and norm entrepreneurs experience this constraint as genuine coordination (they built and maintain a system that prevents mutual annihilation); potential defectors (if they had a voice) would experience it as suppression of their strategic options. Non-nuclear states experience it as beneficial constraint (they are protected from nuclear war without maintaining the enforcement machinery). The divergence is structural: the agenda-setters (nuclear powers) frame it as enlightened self-restraint; the beneficiaries without enforcement voice (civilians, non-nuclear states) experience it as protection; the excluded (strategic rationalists, would-be defectors) experience it as delegitimation of rational calculation. The engine derives these divergent types from the authored power and exit data; the narrative explains why the same constraint structure appears differently from different seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear weapon states sit at d ≈ 0.3 (near beneficiary): they perform the taboo but also benefit from its existence (it delegitimizes their adversaries' use options while preserving their own deterrent). Norm entrepreneurs sit at d ≈ 0.15 (strong beneficiaries): they derive authority and institutional legitimacy from guardianship of the taboo but bear no suppression cost. Civilian populations sit at d ≈ 0.1 (extreme beneficiaries in material terms but unable to enforce): they receive existential protection without participating in constraint maintenance, so the directionality is heavily weighted toward benefit. Non-nuclear states sit at d ≈ 0.2 (moderate beneficiaries): they gain protection but at the cost of strategic inferiority and constrained exit options (they cannot credibly pursue nuclear capability without massive diplomatic cost). Strategic rationalists sit outside the constraint as excluded voices with no formal role. The absence of victims (empty victims[] array) is intentional: this reading treats the taboo as genuinely beneficial to its targets, unlike snares that extract from identifiable victims. The constraint's persistence depends on no one being hurt enough to break it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (existential risk from nuclear weapons) remains live, but the measurement of theater_ratio rising from 0.18 to 0.42 over 75 years suggests the constraint is drifting toward performance-preservation: as the existential threat becomes historically distant and as nuclear use never occurs (making the causal role of the taboo ambiguous), maintaining the constraint requires more ceremony and fewer material incentives. A Tangled Rope reading would classify this as extractive performance masking erosion of genuine coordination function. The Rope reading (current claim) holds because the taboo's beneficiaries (especially civilians) remain enormous and the constraint solves a genuine coordination problem (preventing mutual annihilation). But the rising theater ratio triggers mandatrophy investigation: is the increasing fraction of maintenance energy spent on affirmation rather than deterrence a sign that the founding problem is being narratively stretched to justify institutional positions that have become self-serving? The answer from this reading's framework is: the problem remains live (nuclear weapons still exist, mutual annihilation remains possible), and therefore the institutional performance is justified maintenance of a necessary norm.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    taboo_vs_deterrence_causal_attribution,
    'Is the observed absence of nuclear war use causally attributable to the normative taboo on nuclear weapons, or to deterrent effects of mutual vulnerability that would operate regardless of whether the taboo is affirmed?',
    'Counterfactual analysis: cases where deterrent capability existed but normative prohibition was weak (e.g., proliferator states with weak treaty adherence) vs. cases where taboo was strong but deterrent capability was asymmetric (e.g., nuclear power facing non-nuclear opponent). If taboo-weak cases show higher nuclear-use incidence despite equivalent deterrent capability, taboo is causally efficacious; if use rates are uncorrelated with taboo strength, deterrence is the primary mechanism.',
    'If taboo is causally inert, the constraint reclassifies from Rope (genuine coordination problem solving) to Piton (performance of coordination without function). If taboo is efficacious, the Rope classification holds and theater_ratio rise is maintenance cost, not dysfunction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(taboo_vs_deterrence_causal_attribution, empirical, 'Whether the taboo is causally efficacious or epiphenomenal to deterrence.').

omega_variable(
    norm_entrepreneur_dependency_risk,
    'How much of the taboo''s persistence depends on continuous institutional affirmation by norm entrepreneurs (scholars, international organizations, diplomatic communities), and how much is self-sustaining through state interest in mutual non-use?',
    'Examine periods of institutional weakening (post-Cold War institutional retrenchment, budget cuts to non-proliferation agencies) and measure whether taboo commitment by nuclear states remained constant, weakened proportionally, or was compensated by other institutional support. If institutional withdrawal produced proportional weakening of state commitments, norm entrepreneurs are dependency point; if state commitment held despite institutional decline, taboo is self-reinforcing.',
    'High institutional dependency means the constraint is vulnerable to institutional collapse and norm entrepreneur exit — a Rope sustained only while its narrators are vocal. Low dependency means the constraint has matured into genuinely self-enforcing coordination. Dependency also affects theater_ratio interpretation: rising theater under institutional weakness suggests theatrical maintenance of degraded function; rising theater amid stable institutional commitment suggests expanding performance as the constraint stabilizes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(norm_entrepreneur_dependency_risk, empirical, 'Whether taboo persistence depends on institutional maintenance or state interest.').

omega_variable(
    non_nuclear_power_constraint_structure_divergence,
    'Does the constraint operate identically for non-nuclear states and nuclear-armed states, or do non-nuclear powers face a different structural constraint (prohibition on pursuing nuclear weapons to contest the taboo)?',
    'Compare taboo strength and enforcement intensity against nuclear-armed states that breach the norm (low enforcement, diplomatic accommodation) vs. against non-nuclear states pursuing capability (high enforcement, sanctions, military intervention). If enforcement intensity differs systematically, non-nuclear states face a two-constraint structure: the taboo on use plus a constraint on pursuit; nuclear states face only the use taboo. This would decompose into separate constraint stories.',
    'If true, this is a false-unity error: the nuclear taboo is actually two constraints with different beneficiary/victim structures. This reading describes the USE taboo only; pursuit prohibition is a separate constraint. The decomposition would yield three constraint stories, not one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(non_nuclear_power_constraint_structure_divergence, conceptual, 'Whether the taboo operates uniformly or creates different constraints for nuclear vs. non-nuclear states.').

omega_variable(
    rising_theater_sustainability,
    'At what theater_ratio threshold does norm performance become self-undermining (audiences lose credibility in the affirmed prohibition, norm becomes obviously performative rather than existential)?',
    'Track public opinion and elite credibility surveys alongside institutional theater_ratio increase. If public belief in the taboo''s binding nature declines as ceremony increases, the threshold is being approached. If belief remains high despite theatrical increase, performance can sustain itself indefinitely.',
    'A rising theater trajectory that crosses the credibility threshold would reclassify the constraint from Rope to Piton (performance without function, persisting only through inertia). Current theater_ratio of 0.42 may be sustainable indefinitely or may be approaching the tipping point; the trajectory is the signal.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rising_theater_sustainability, empirical, 'Whether rising theater sustains or degrades taboo credibility.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__nuclear_taboo_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t0, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(tota_tr_t0, projected).
narrative_ontology:measurement(tota_tr_t10, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement_basis(tota_tr_t10, observed).
narrative_ontology:measurement(tota_tr_t20, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement_basis(tota_tr_t20, observed).
narrative_ontology:measurement(tota_tr_t35, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 35, 0.4).
narrative_ontology:measurement_basis(tota_tr_t35, observed).
narrative_ontology:measurement(tota_tr_t50, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement_basis(tota_tr_t50, observed).
narrative_ontology:measurement(tota_tr_t65, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 65, 0.42).
narrative_ontology:measurement_basis(tota_tr_t65, observed).
narrative_ontology:measurement(tota_tr_t75, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 75, 0.42).
narrative_ontology:measurement_basis(tota_tr_t75, projected).

% Extraction over time
narrative_ontology:measurement(tota_be_t0, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement_basis(tota_be_t0, projected).
narrative_ontology:measurement(tota_be_t10, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 10, 0.15).
narrative_ontology:measurement_basis(tota_be_t10, observed).
narrative_ontology:measurement(tota_be_t20, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 20, 0.22).
narrative_ontology:measurement_basis(tota_be_t20, observed).
narrative_ontology:measurement(tota_be_t35, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 35, 0.26).
narrative_ontology:measurement_basis(tota_be_t35, observed).
narrative_ontology:measurement(tota_be_t50, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 50, 0.27).
narrative_ontology:measurement_basis(tota_be_t50, observed).
narrative_ontology:measurement(tota_be_t65, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 65, 0.28).
narrative_ontology:measurement_basis(tota_be_t65, observed).
narrative_ontology:measurement(tota_be_t75, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 75, 0.28).
narrative_ontology:measurement_basis(tota_be_t75, projected).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t0, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement_basis(tota_su_t0, projected).
narrative_ontology:measurement(tota_su_t10, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 10, 0.12).
narrative_ontology:measurement_basis(tota_su_t10, observed).
narrative_ontology:measurement(tota_su_t20, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 20, 0.15).
narrative_ontology:measurement_basis(tota_su_t20, observed).
narrative_ontology:measurement(tota_su_t35, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 35, 0.17).
narrative_ontology:measurement_basis(tota_su_t35, observed).
narrative_ontology:measurement(tota_su_t50, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 50, 0.18).
narrative_ontology:measurement_basis(tota_su_t50, observed).
narrative_ontology:measurement(tota_su_t65, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 65, 0.19).
narrative_ontology:measurement_basis(tota_su_t65, observed).
narrative_ontology:measurement(tota_su_t75, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 75, 0.19).
narrative_ontology:measurement_basis(tota_su_t75, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__nuclear_taboo_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_possibility_space__nuclear_taboo_reading, 0.12).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, total_war_possibility_space__deterrence_equilibrium_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, total_war_possibility_space__space_contraction_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, non_proliferation_regime_enforcement).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, nuclear_hedging_constraint).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel total_war_possibility_space. The kernel contest involves three readings with different causal attributions and strategic implications. The nuclear_taboo_reading instantiated here treats the constraint as genuine coordination maintained through performed norm; the deterrence_equilibrium_reading treats it as equilibrium independent of norm performance; the space_contraction_reading treats it as cognitive constraint precluding the option entirely. All three are valid readings of the same kernel (nuclear weapons exist; total war became improbable). Their coexistence in scholarly and policy discourse is part of the kernel's operation — no single framework adjudicates which is true.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
