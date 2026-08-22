% ============================================================================
% CONSTRAINT STORY: total_war_winnability_post1945__structural_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_winnability_post1945__structural_contraction_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: total_war_winnability_post1945__structural_contraction_reading
 *   human_readable: Post-1945 Structural Unreachability of Peer Total War (Structural Contraction Reading)
 *   domain: international_relations/strategic_studies/commitment_systems
 *
 * SUMMARY:
 *   This file instantiates the structural_contraction_reading of the kernel
 *   total_war_winnability_post1945: after 1945, thermonuclear yield outran
 *   what industrial societies could absorb, and total war between peer states
 *   left the reachable space. The constraint claimed here is not a treaty,
 *   doctrine, or norm — it is a physical closure: any engagement conducted at
 *   total-war scale escalates past war-termination to mutual annihilation, so
 *   the option is deleted rather than renounced. The ε referent is the
 *   standing arrangement under contest — the post-1945 condition of
 *   unreachable total war — assessed by this reading's own lights, in which
 *   the closure is self-holding physics requiring no enforcement,
 *   administration, or belief. No beneficiaries and no victims are declared:
 *   no party collects from the constraint's operation, and the only
 *   conceivable casualty class is counterfactual (populations of exchanges
 *   that never occurred). The constraint therefore has no parties, and the
 *   stakeholders surface is omitted under the genuine-mountain exemption. The
 *   claimed type (mountain) and the authored metrics are independent facts:
 *   the metrics report near-zero extraction and suppression, low theater,
 *   near-complete accessibility collapse, and low but nonzero resistance from
 *   perennial counterforce programs.
 *
 * KEY AGENTS:
 *   - No agent seats: the constraint binds all nuclear-capable adversaries symmetrically through yield physics rather than enforcement — there is no administrator, no collector, and no target seat for the directionality machinery to differentiate.
 *   - Hypothetical casualty populations of counterfactual exchanges: the only conceivable victim class, and they are counterfactual — no realized party bears a cost flow, which is why no victims are declared.
 *   - Analytical observer: the seat from which the full structure is visible — a side effect of weapons development operating as a constraint on all indices at once, with zero degrees of freedom and no maintenance dependency.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_winnability_post1945__structural_contraction_reading, 0.05).
domain_priors:suppression_score(total_war_winnability_post1945__structural_contraction_reading, 0.03).
domain_priors:theater_ratio(total_war_winnability_post1945__structural_contraction_reading, 0.09).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 0.09).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__structural_contraction_reading, mountain).
narrative_ontology:human_readable(total_war_winnability_post1945__structural_contraction_reading, "Post-1945 Structural Unreachability of Peer Total War (Structural Contraction Reading)").
narrative_ontology:topic_domain(total_war_winnability_post1945__structural_contraction_reading, "international_relations/strategic_studies/commitment_systems").

domain_priors:emerges_naturally(total_war_winnability_post1945__structural_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__structural_contraction_reading, 'a9c19109-d9bb-4470-8f63-91c70b83024f').
narrative_ontology:cs_kernel_codification('a9c19109-d9bb-4470-8f63-91c70b83024f', distributed).
narrative_ontology:cs_authority_grounding('a9c19109-d9bb-4470-8f63-91c70b83024f', distributed).
narrative_ontology:cs_reading_relation('a9c19109-d9bb-4470-8f63-91c70b83024f', total_war_winnability_post1945__normative_reading_drop, forecloses).
narrative_ontology:cs_reading_relation('a9c19109-d9bb-4470-8f63-91c70b83024f', total_war_winnability_post1945__strategic_culture_drift, forecloses).
narrative_ontology:cs_axiom('a9c19109-d9bb-4470-8f63-91c70b83024f', foundational, total_war_structurally_unreachable).
narrative_ontology:cs_axiom_status(total_war_structurally_unreachable, holdable).
narrative_ontology:cs_axiom_grounding('a9c19109-d9bb-4470-8f63-91c70b83024f', total_war_structurally_unreachable, empirically_contingent).
narrative_ontology:cs_axiom('a9c19109-d9bb-4470-8f63-91c70b83024f', foundational, restraint_carried_by_physics_not_social_layers).
narrative_ontology:cs_axiom_status(restraint_carried_by_physics_not_social_layers, holdable).
narrative_ontology:cs_axiom_grounding('a9c19109-d9bb-4470-8f63-91c70b83024f', restraint_carried_by_physics_not_social_layers, empirically_contingent).
narrative_ontology:cs_reference_frame('a9c19109-d9bb-4470-8f63-91c70b83024f', mutual_unsurvivability_baseline).
narrative_ontology:cs_drift_state('a9c19109-d9bb-4470-8f63-91c70b83024f', contemporary_counterforce_era, gap(axiom_overriding, minor, true)).
narrative_ontology:cs_created_at('a9c19109-d9bb-4470-8f63-91c70b83024f', '2026-08-05T12:00:00Z').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__structural_contraction_reading, total_war_winnability_post1945).

% --- Structural relationships ---
narrative_ontology:constraint_vindicates(total_war_winnability_post1945__structural_contraction_reading, mutual_vulnerability_stability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None by construction: no one built this constraint to solve a coordination problem. It emerged as a side effect of weapon yield outrunning what industrial belligerent societies could absorb. Stated without evaluation: the arrangement deletes the maximal war option from the reachable set of peer adversaries; its stabilizing effect on great-power relations is a consequence of that deletion, not a function anyone designed.
% TRANSFER_FUNCTION: Nothing moves through it. No money, work, attention, or status flows from any party to any other; the constraint operates by deletion of an option, not by transfer. The absence of a transfer surface is the structural signature separating it from coordination and extraction arrangements, and it is why no receipt surface is authored.
% ABSENT_VOICES: There is no conversation from which a voice could be absent — the constraint has no forum, membership, or agenda. The only conceivable objectors are counterfactual: the populations of exchanges that never occurred cannot speak, and the strategic professions whose doctrine presupposed the decisive campaign lost their object rather than their seat. No dissenting constituency is silenced by enforcement because no enforcement exists.
% DISAPPEARANCE_RATIONALE: Under this reading the postwar strategic order is arranged around unreachability: force posture, alliance architecture, crisis bargaining, and war-termination planning all presuppose that peer total war cannot be waged and won. Lift the physical fact — a credible defense against retaliation, or yields falling below absorption thresholds — and those arrangements lose their premise and reorganize around restored winnability. Whether the world would in fact rearrange, or whether the accumulated legal and cultural layers would carry restraint independently of physics, is exactly what the sibling readings dispute; the verdict is therefore recorded as contested rather than asserted.
% FOUNDING_PROBLEM: Recurrent great-power total war: the 1815-1945 sequence in which peer industrial states waged wars of national mobilization and decisive victory, culminating in the Second World War. Thermonuclear weapons were built to win that kind of war; the closure of the category was an unplanned structural consequence of yield, not a designed remedy.
% FOUNDING_PROBLEM_CORROBORATION: No benefiting party exists to self-attest, so corroboration is unconstrained by beneficiary interest. The eight-decade non-recurrence of peer total war is attested by the historical record itself and by strategic studies scholarship across competing schools — including scholars who reject this reading's mechanism (normative-legal and strategic-culture schools) yet concede the empirical non-recurrence their own readings must then explain. External attestation is strong on the problem's non-recurrence and contested on the reason; no source inside any benefiting set vouches for the genealogy because no benefiting set exists.
narrative_ontology:disappearance_verdict(total_war_winnability_post1945__structural_contraction_reading, contested).
narrative_ontology:founding_problem_status(total_war_winnability_post1945__structural_contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_winnability_post1945__structural_contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_winnability_post1945__structural_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_winnability_post1945__structural_contraction_reading, 0.05, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_winnability_post1945__structural_contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(total_war_winnability_post1945__structural_contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(total_war_winnability_post1945__structural_contraction_reading),
    narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(total_war_winnability_post1945__structural_contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.05 because nothing transfers: the constraint deletes an option rather than moving resources, and the foregone option-value accrues to no receiver. Suppression is 0.03 because no enforcement machinery exists — the constraint is self-holding (retaliatory capability is a physical fact, not a patrol). Theater_ratio is 0.09 because the visible activity around nuclear weapons (civil defense drills, parity displays, signaling exercises) is society's response to the constraint, not its maintenance; under this reading none of it holds the closure up, so the performative share of the constraint's own operation is near-nil. Accessibility_collapse is 0.88: once the yield-versus-absorption arithmetic is understood, the alternative — waging total war anyway — collapses for any agent that weights survival, leaving no workable rival strategy. Resistance is 0.12: counterforce modernization, damage-limitation studies, and missile defense constitute continuous attempts to restore winnability, and they have so far failed against the survivable retaliatory leg, but they are real resistance and are scored as such rather than rounded to zero. The measurement series share one nine-point grid (1945-2025 at decade steps) so every tracked metric is authored at every examined time point; suppression_requirement is deliberately not tracked because the enforcement picture is static — there is no enforcement to build up or decay.
 *
 * PERSPECTIVAL GAP:
 *   The per-seat divergence machinery has no purchase here: with no beneficiaries, payers, or administrators, there are no seats whose computed types could diverge. Every index faces the same zero-degree-of-freedom structure. The only perspectival variation the structure admits runs along the preference axis — an agent that weights survival experiences the closure as absolute, while an agent with anomalous risk preferences faces a softened constraint — and that variation is routed to the preference_indexed_binding omega rather than to stakeholder differentiation.
 *
 * DIRECTIONALITY LOGIC:
 *   No directionality gradient is authored because none exists in the structure: no beneficiary or victim declarations are made, so the derivation chain has no positional data and every index defaults to the same relationship to the constraint. Declaring beneficiaries would misdescribe physics as an arrangement (and would trigger false-summit evaluation on a constraint this reading holds to be genuine); declaring victims would convert counterfactual populations into real cost-bearers. The honest structural datum is the absence of a gradient — effective extraction tracks base epsilon for every index, unscaled by any directional asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two mislabelings. First, mistaking the closure for a maintained normative regime (rope or snare) would credit institutions — treaties, doctrines, taboos — with what yield physics does, manufacturing a coordination story where there is only a physical fact; the near-zero suppression and theater scores are the descriptive signature that separates this from enforced arrangements. Second, mistaking it for a piton would read the surrounding performance (civil defense theater, arms-parade spectacle) as vestigial maintenance of a dead function; but the performance is response, not support — the function never depended on it. No mandate was ever issued, so no mandate has outlived its function: mandatrophy_resolved is not declared because there was never a mandate to resolve. The founding problem (recurrent peer total war) was closed as an unplanned side effect of weapons built to win such a war, which is why the genealogy reads as accident rather than design.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint is one reading of the kernel total_war_winnability_post1945 (reading: structural_contraction_reading; siblings: normative_reading_drop, strategic_culture_drift). Which causal carrier does the evidence actually support — physical unreachability, normative illegitimacy, or strategic-cultural drift?',
    'Cross-reading comparison at points where the candidate carriers dissociate: sub-nuclear threshold conflicts where norms are tested but physics does not bind, crises in which elites entertained options the discourse account says were undiscussable, and counterforce programs aimed at restoring reachability.',
    'If restraint tracks the physical fact wherever the carriers dissociate, this reading''s mountain classification stands; if restraint tracks law or discourse while physics is held constant, the sibling readings gain and this file''s claim weakens toward a maintained, constructed constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Kernel membership: one of three readings of total_war_winnability_post1945; the dispute is located at the causal carrier of post-1945 great-power restraint.').

omega_variable(
    self_holding_vs_maintained_survivability,
    'Is the contraction self-holding physics, or does it depend on a maintained second-strike survivability complex — hardened basing, sea-based legs, alert postures — that is a constructed arrangement requiring continuous investment?',
    'Counterforce capability assessment: if some plausible force package could disarm a major power''s retaliatory leg, the constraint is maintained and contestable; if fusion yield versus urban-industrial absorption guarantees unacceptable retaliation under every projected counterforce envelope, it is self-holding.',
    'Self-holding supports emerges_naturally and the mountain claim; maintained survivability converts the constraint into a defended arrangement with identifiable maintainers, opening the false-summit question of who benefits from presenting a maintained deterrent as physics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_holding_vs_maintained_survivability, empirical, 'Whether the constraint holds itself or is held by a maintained second-strike survivability complex.').

omega_variable(
    preference_indexed_binding,
    'Does the constraint bind absolutely, or only agents that weight survival — is unreachability a fact of physics or a fact of rational choice under certain annihilation?',
    'Examine crisis behavior under anomalous risk preferences, delegation pathologies, and isolation effects at the decision point; credible total-war initiation under extreme preference profiles would show the constraint is preference-indexed rather than absolute.',
    'An absolute reading sustains zero degrees of freedom for all indices and the pure mountain profile; a preference-indexed reading reintroduces degrees of freedom for non-standard agents and softens accessibility_collapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preference_indexed_binding, conceptual, 'Whether unreachability is preference-independent physics or a rational-choice equilibrium.').

omega_variable(
    counterforce_restoration_trajectory,
    'Are precision strike, ballistic missile defense, hypersonic delivery, and AI-enabled command and control progressively restoring winnability — converting a stable mountain into a degrading constraint?',
    'Track damage-limitation feasibility across technology generations in exchanged-strike modeling; watch for a crossing point at which a major power''s retaliatory leg becomes credibly disarmable.',
    'A confirmed restoration trajectory dates the end of the mountain phase and predicts transition toward contested management of a returning option; refutation extends the mountain''s tenure indefinitely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterforce_restoration_trajectory, empirical, 'Technological drift toward restored winnability.').

omega_variable(
    hypothetical_victim_status,
    'The only conceivable victims are the populations of counterfactual exchanges — does a constraint whose victim set is purely hypothetical differ categorically in harm structure from constraints with realized victims?',
    'Conceptual separation of harm classes: foregone-option costs borne by living agents versus counterfactual-casualty costs borne by no one; determine which class this constraint generates and whether the apparatus should weight latent victim sets that activate only on failure.',
    'If counterfactual victims count, the constraint carries a latent victim structure relevant to any reclassification under restored winnability; if not, the constraint is victimless in every state of the world, reinforcing the no-parties reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hypothetical_victim_status, conceptual, 'Status of the hypothetical victim set in the harm structure.').

omega_variable(
    bipolarity_confound,
    'Is the non-recurrence of peer total war attributable to nuclear physics specifically, or confounded by the bipolar configuration, economic integration, and the exhaustion of 1945?',
    'Comparative analysis across configurations: multipolar nuclear moments such as the Sino-Soviet split and contemporary tripolarity, and near-peer non-nuclear dyads; if total war stays absent where bipolarity fails but physics holds, the physical attribution strengthens.',
    'A confirmed confound weakens the specifically-nuclear mountain claim and redistributes explanatory weight toward political-structural factors outside this kernel; refutation isolates yield physics as the operative variable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bipolarity_confound, empirical, 'Bipolarity and economic confounds in attributing the long peace to nuclear physics.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__structural_contraction_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tww_scr_tr_t1945, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(tww_scr_tr_t1955, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 1955, 0.15).
narrative_ontology:measurement(tww_scr_tr_t1965, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 1965, 0.17).
narrative_ontology:measurement(tww_scr_tr_t1975, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 1975, 0.11).
narrative_ontology:measurement(tww_scr_tr_t1985, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 1985, 0.14).
narrative_ontology:measurement(tww_scr_tr_t1995, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 1995, 0.07).
narrative_ontology:measurement(tww_scr_tr_t2005, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 2005, 0.06).
narrative_ontology:measurement(tww_scr_tr_t2015, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 2015, 0.07).
narrative_ontology:measurement(tww_scr_tr_t2025, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 2025, 0.09).

% Extraction over time
narrative_ontology:measurement(tww_scr_be_t1945, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 1945, 0.03).
narrative_ontology:measurement(tww_scr_be_t1955, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 1955, 0.03).
narrative_ontology:measurement(tww_scr_be_t1965, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 1965, 0.04).
narrative_ontology:measurement(tww_scr_be_t1975, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 1975, 0.03).
narrative_ontology:measurement(tww_scr_be_t1985, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 1985, 0.04).
narrative_ontology:measurement(tww_scr_be_t1995, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 1995, 0.03).
narrative_ontology:measurement(tww_scr_be_t2005, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 2005, 0.03).
narrative_ontology:measurement(tww_scr_be_t2015, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 2015, 0.04).
narrative_ontology:measurement(tww_scr_be_t2025, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 2025, 0.05).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(total_war_winnability_post1945__structural_contraction_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(total_war_winnability_post1945__structural_contraction_reading, normative_reading_drop).
narrative_ontology:affects_constraint(total_war_winnability_post1945__structural_contraction_reading, strategic_culture_drift).

% DUAL FORMULATION NOTE:
% The colloquial label 'the nuclear peace' or 'the long peace' conflates three structurally distinct claims about why peer total war has not recurred since 1945: that it cannot be waged (this file — physical closure, near-zero epsilon, no parties), that it is legally condemned (normative_reading_drop — a maintained legal regime with its own enforcement and beneficiary structure), and that it is no longer discussed (strategic_culture_drift — a discursive practice with its own carriers). Each claim warrants its own epsilon, its own beneficiary/victim structure, and its own classification; this file holds the physical-unreachability claim alone. The physical fact is upstream: it sets the environment within which the normative and cultural layers operate, so this story links to both siblings and their stories should link back. The siblings assert continued reachability; this reading's core premise negates that assertion, which is why the reading_relations are forecloses even though all three remain live positions held by different parties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
