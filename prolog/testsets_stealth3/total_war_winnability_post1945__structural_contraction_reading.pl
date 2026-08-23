% ============================================================================
% CONSTRAINT STORY: total_war_winnability_post1945__structural_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
 *   constraint_id: total_war_winnability_post1945__structural_contraction_reading
 *   human_readable: Post-1945 Structural Unreachability of Total War (Structural Contraction Reading)
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This file instantiates the structural_contraction_reading of the kernel
 *   total_war_winnability_post1945: the claim that nuclear weapons physically
 *   removed total war between nuclear-armed peers from the reachable space of
 *   international politics -- not that states learned to forbear
 *   (normative_reading_drop), nor that elites stopped talking about it
 *   (strategic_culture_drift), but that the option itself ceased to exist.
 *   The epsilon referent is the standing arrangement under contest: the
 *   post-1945 condition in which general war between nuclear-armed great
 *   powers cannot be fought to a decision, assessed by this reading's own
 *   lights. The constraint has no parties in the extraction sense: no
 *   administrator (physics is not administered), no beneficiary seat
 *   collecting rents, no payer bearing imposed costs. The only candidate
 *   victim class -- populations of a counterfactual nuclear exchange -- is
 *   hypothetical and therefore not seated. Accordingly no beneficiaries or
 *   victims are declared, no stakeholders are authored (genuine-mountain
 *   exemption: a constraint with no parties has no stakeholder surface), and
 *   the receipt surface is left unauthored. KEY AGENTS (by structural
 *   relationship): none -- the enumeration legitimately terminates at zero,
 *   and that zero is the structural claim. Claim and metrics are independent:
 *   the mountain claim is what this reading believes structurally true; the
 *   near-zero metrics are what it believes descriptively true.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_winnability_post1945__structural_contraction_reading, 0.03).
domain_priors:suppression_score(total_war_winnability_post1945__structural_contraction_reading, 0.02).
domain_priors:theater_ratio(total_war_winnability_post1945__structural_contraction_reading, 0.03).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, extractiveness, 0.03).
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 0.03).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, accessibility_collapse, 0.93).
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__structural_contraction_reading, mountain).
narrative_ontology:human_readable(total_war_winnability_post1945__structural_contraction_reading, "Post-1945 Structural Unreachability of Total War (Structural Contraction Reading)").
narrative_ontology:topic_domain(total_war_winnability_post1945__structural_contraction_reading, "international_relations/strategic_studies").

domain_priors:emerges_naturally(total_war_winnability_post1945__structural_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__structural_contraction_reading, 'e75e7ed8-00a1-43c3-80bf-6d6ec434c19f').
narrative_ontology:cs_kernel_codification('e75e7ed8-00a1-43c3-80bf-6d6ec434c19f', distributed).
narrative_ontology:cs_authority_grounding('e75e7ed8-00a1-43c3-80bf-6d6ec434c19f', expertise).
narrative_ontology:cs_interpretation_layer_present('e75e7ed8-00a1-43c3-80bf-6d6ec434c19f').
narrative_ontology:cs_reading_relation('e75e7ed8-00a1-43c3-80bf-6d6ec434c19f', total_war_winnability_post1945__normative_reading_drop, forecloses).
narrative_ontology:cs_reading_relation('e75e7ed8-00a1-43c3-80bf-6d6ec434c19f', total_war_winnability_post1945__strategic_culture_drift, forecloses).
narrative_ontology:cs_axiom('e75e7ed8-00a1-43c3-80bf-6d6ec434c19f', foundational, general_war_physically_unreachable).
narrative_ontology:cs_axiom_status(general_war_physically_unreachable, holdable).
narrative_ontology:cs_axiom_grounding('e75e7ed8-00a1-43c3-80bf-6d6ec434c19f', general_war_physically_unreachable, empirically_contingent).
narrative_ontology:cs_axiom('e75e7ed8-00a1-43c3-80bf-6d6ec434c19f', secondary, foreclosure_independent_of_belief_and_enforcement).
narrative_ontology:cs_axiom_status(foreclosure_independent_of_belief_and_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('e75e7ed8-00a1-43c3-80bf-6d6ec434c19f', foreclosure_independent_of_belief_and_enforcement, empirically_contingent).
narrative_ontology:cs_reference_frame('e75e7ed8-00a1-43c3-80bf-6d6ec434c19f', thermonuclear_foreclosure_baseline).
narrative_ontology:cs_drift_state('e75e7ed8-00a1-43c3-80bf-6d6ec434c19f', contemporary_second_nuclear_age, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('e75e7ed8-00a1-43c3-80bf-6d6ec434c19f', '').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__structural_contraction_reading, total_war_winnability_post1945).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves no active coordination problem: nothing is negotiated, enforced, or maintained. Its effect is subtractive -- it deletes the terminal rung of the escalation ladder from every actor's option set simultaneously, which makes certain coordination problems (restraint agreements at the top of the ladder) unnecessary rather than solved. Great-power rivalry below the threshold requires no agreement because the terminal option is physically unavailable to both sides at once.
% TRANSFER_FUNCTION: Transfers nothing. No money, labor, attention, or status moves through the constraint; it is a pure deletion of an option, symmetric across all actors with the physical capacity to trigger it.
% ABSENT_VOICES: There are no excluded negotiating parties because there is no negotiation to be excluded from. The nearest analogues: populations of non-nuclear states living inside the constraint's shadow, whose security is shaped by great-power restraint they never agreed to and have no channel to appeal; and the dead of peripheral wars fought under the nuclear ceiling, who cannot object that the great powers' own protection licensed violence at the periphery. Neither seat has a forum to enter, because no forum administering this constraint exists.
% DISAPPEARANCE_RATIONALE: If the foreclosure lifted overnight -- if general war between nuclear-armed peers became winnable -- every great-power force posture, alliance guarantee, and war plan built since the late 1950s would be obsolete within a planning cycle. Extended-deterrence commitments would lose their logic or invert; states would rebuild war-winning doctrines, mobilization plans, and civil defense at scale; the entire architecture of crisis stability (hotlines, arms control, threshold management) would be renegotiated around a restored option. The world's arrangements demonstrably depend on the constraint.
% FOUNDING_PROBLEM: No one built this constraint to solve anything: it is the unintended structural residue of the opposite project -- the decades-long pursuit of decisive, winnable war, which culminated in weapons whose use forecloses the victory they were built to deliver. The founding problem it answers, retroactively, is the classical great-power problem of how to fight a peer to a decision; the constraint is that problem becoming unsolvable rather than solved.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside any benefiting party (there are none): the documentary record of both Cold War blocs abandoning war-termination and victory objectives against peer nuclear powers -- planning documents shifting from 'winning' to 'deterring' across the 1950s and 1960s -- and the strategic-studies literature from Brodie (1946) onward, written by analysts with no stake in the constraint's persistence. No credible external source attests that decisive victory over a nuclear-armed peer remains a live planning objective; the minority counterforce-revival literature argues for damage limitation at the margins, not for restoration of the founding problem.
narrative_ontology:disappearance_verdict(total_war_winnability_post1945__structural_contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_winnability_post1945__structural_contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_winnability_post1945__structural_contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_winnability_post1945__structural_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_winnability_post1945__structural_contraction_reading, 0.03, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.03: the constraint transfers nothing, so there is nothing to extract; the residual registers the diffuse opportunity cost the closure imposes on war-making institutions whose classical purpose it voids -- a cost borne widely and collected by no one. Suppression 0.02: physics coerces no one; it forecloses. There is no enforcement apparatus and none is needed; the residual registers only that the closure is experienced as compulsory rather than chosen. Theater_ratio 0.03: nothing about the constraint is performed -- no committee meets to maintain it, no ritual sustains it; the small residual covers rhetoric that contests the constraint (damage-limitation programs, winnability discourse) rather than activity maintaining it, and the mild 1985 uptick reflects the SDI-era contestation wave, not maintenance. Accessibility_collapse 0.93: once the physics of assured retaliatory destruction is understood, the alternative -- fighting a peer to a decision -- collapses almost completely; the residual 0.07 is the persistent margin claimed for counterforce and defense technologies. Resistance 0.08: no actor mobilizes against the constraint as such; the faint measured resistance is investment in restoring winnability at the margins, which has never approached viability against full-scale arsenals. The measurement series run on one shared nine-point grid (1945-2026) so every tracked metric is authored at every examined time point; suppression_requirement is deliberately not tracked because the constraint has no enforcement capacity whose rise or fall could be traced -- the static zero is carried by the scalar.
 *
 * PERSPECTIVAL GAP:
 *   Within this story the seats converge: with no positional asymmetry -- no one pays, no one collects -- every computable seat lands on the same classification, which is what a mountain should do. The real perspectival gap runs BETWEEN stories, not within this one: from the normative_reading_drop seat, the same observed absence looks like a successfully enforced norm and would compute with enforcement overhead and violation costs; from the strategic_culture_drift seat, it looks like a discursive horizon and would compute with identity-lock structure. This file authors only its own seat's structure; the engine's cross-story comparison is where that divergence becomes measurable.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiary or victim declarations are authored, so the derivation chain has no structural data from which to compute asymmetric directionality -- and that absence is itself the structural claim of this reading: there is no seat toward which the constraint directs net cost or benefit. Effective extraction is therefore the base epsilon (approximately 0.03) essentially unscaled everywhere: no trapped targets to amplify, no subsidized beneficiaries to damp. The hypothetical victim class (populations in a counterfactual exchange) is deliberately not declared: hypothetical persons bear no realized extraction, and seating them would fabricate a directional structure the reading denies. Suppression, as a raw structural property, is likewise unscaled and near zero: nothing is suppressed because nothing needs to be.
 *
 * MANDATROPHY ANALYSIS:
 *   The genealogy interview produces the classic false-piton signature, and the classification exists to block that misread: founding_problem_status is dead (decisive victory over a peer is no longer a live objective anywhere) while disappearance_verdict is world_rearranges (every great-power arrangement depends on the constraint) -- the pattern that, in an administered institution, flags a zombie mandate kept alive by inertia and theater. What disqualifies the piton reading here is the absence of the piton's machinery: no agenda_setter could remove the constraint (no one administers physics), no theater sustains it (theater_ratio 0.03), and no cost-asymmetry exists because there is no administrator bearing costs. The constraint persists not because its function atrophied behind a preserved form but because its 'function' was never a function -- it is the shape of the physical world. Mandatrophy vocabulary applies to built things; this reading claims the constraint is not built. The mismatch flag (dead x world_rearranges) should fire and then clear on cross-check against the computed piton/theater path; that clearing is itself the measurement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_adjudication,
    'Which of the three readings of kernel total_war_winnability_post1945 correctly identifies the binding mechanism of the post-1945 absence of total war: physical foreclosure (this file), normative prohibition (normative_reading_drop), or strategic-cultural discursive shift (strategic_culture_drift)?',
    'Adversarial case analysis at the points where the readings'' predictions diverge: crises in which norms were broken or absent but physics intact (revisionist or non-signatory dyads), documentary evidence of leaders who believed general war winnable and acted on it, and natural experiments where normative enforcement weakened without the absence of total war changing.',
    'If a normative or cultural mechanism is correct, this file misclassifies: the absence would be violable and enforceable (rope-like structure with enforcement overhead) and this story should be retired in favor of the sibling files; if the structural mechanism is correct, the sibling readings reduce to epiphenomena riding on physics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_adjudication, empirical, 'Adjudication among the three sibling readings of the total-war-winnability kernel.').

omega_variable(
    arsenal_existence_contingency,
    'Is the foreclosure a permanent feature of the international system, or is it contingent on the continued existence of large thermonuclear arsenals with credible delivery?',
    'Track arsenal trajectories, arms-control collapse and rebuild cycles, and any credible movement toward abolition; the constraint''s persistence is indexed to the artifact base that carries it.',
    'If arsenals lapse, the constraint lapses with them: the classification is a conditional mountain scoped to the nuclear era rather than a timeless natural law, and the emerges_naturally claim would need re-scoping to ''given the artifact, the impossibility follows from physics''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(arsenal_existence_contingency, empirical, 'Whether the physical foreclosure survives the artifacts that produce it.').

omega_variable(
    dyad_coverage_boundary,
    'Does the foreclosure bind all state pairs, or only dyads containing at least one nuclear-armed great power with second-strike capability?',
    'Enumerate the population of total wars actually fought since 1945 (Iran-Iraq, Korea 1950-53, the Indo-Pakistani wars, etc.) and test for each whether the mechanism of assured retaliatory destruction was present between the belligerents.',
    'If only nuclear dyads are bound, the constraint''s spatial scope contracts from global to the network of nuclear-armed pairs; the constraint is selective rather than universal, and total war between non-nuclear states remains inside the reachable space.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dyad_coverage_boundary, empirical, 'The population of state dyads over which the foreclosure actually operates.').

omega_variable(
    countermeasure_winnability_restoration,
    'Can defensive and damage-limitation technology (layered missile defense, hardened and dispersed forces, conventional prompt strike, AI-enabled targeting) partially restore winnability, converting foreclosure into friction?',
    'Defense-dominance and crisis-stability modeling against realistic full-scale offense volumes, combined with historical ballistic-missile-defense effectiveness data under saturation conditions.',
    'Partial restoration would downgrade the mountain toward a contested boundary: actors investing to re-expand the reachable space introduce an enforcement-and-contestation structure (and with it, potential extraction) that a pure foreclosure lacks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(countermeasure_winnability_restoration, empirical, 'Whether technology can reopen the space this reading declares closed.').

omega_variable(
    foreclosure_relation_framing,
    'Are the sibling readings genuinely foreclosed by this reading''s bivalent possibility premise, or do the three readings function as compatible multi-cause explanations whose relations should be coexists_with?',
    'Fix the framework semantics: if a commitment framework may conjoin causal accounts (norms bind AND physics binds), the relations downgrade to coexists_with; if a framework must take a stance on whether total war remains physically possible, the foreclosure relations stand.',
    'Changes cs_structure.reading_relations from forecloses to coexists_with for both siblings, altering the engine-computed foreclosure and axiom-overriding paths for the whole kernel family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreclosure_relation_framing, conceptual, 'Framing under-determination in the typed relations among sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__structural_contraction_reading, 1945, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 1945, 0.01).
narrative_ontology:measurement(tota_tr_t1955, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 1955, 0.02).
narrative_ontology:measurement(tota_tr_t1965, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 1965, 0.02).
narrative_ontology:measurement(tota_tr_t1975, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 1975, 0.02).
narrative_ontology:measurement(tota_tr_t1985, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 1985, 0.03).
narrative_ontology:measurement(tota_tr_t1995, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 1995, 0.02).
narrative_ontology:measurement(tota_tr_t2005, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 2005, 0.02).
narrative_ontology:measurement(tota_tr_t2015, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 2015, 0.03).
narrative_ontology:measurement(tota_tr_t2026, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 2026, 0.03).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 1945, 0.03).
narrative_ontology:measurement(tota_be_t1955, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 1955, 0.03).
narrative_ontology:measurement(tota_be_t1965, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 1965, 0.03).
narrative_ontology:measurement(tota_be_t1975, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 1975, 0.03).
narrative_ontology:measurement(tota_be_t1985, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 1985, 0.03).
narrative_ontology:measurement(tota_be_t1995, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 1995, 0.03).
narrative_ontology:measurement(tota_be_t2005, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 2005, 0.03).
narrative_ontology:measurement(tota_be_t2015, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 2015, 0.03).
narrative_ontology:measurement(tota_be_t2026, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 2026, 0.03).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(total_war_winnability_post1945__structural_contraction_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(total_war_winnability_post1945__structural_contraction_reading, total_war_winnability_post1945__normative_reading_drop).
narrative_ontology:affects_constraint(total_war_winnability_post1945__structural_contraction_reading, total_war_winnability_post1945__strategic_culture_drift).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the nuclear peace' / 'why no World War III' conflates three structurally distinct claims and is decomposed per the epsilon-invariance principle into three linked stories sharing the kernel total_war_winnability_post1945. This file (structural_contraction_reading) is the upstream member: its claim rests on physics and operational analysis with the highest empirical confidence, and both downstream readings cite the destructive physics as background while locating the binding mechanism elsewhere (law for normative_reading_drop, discourse for strategic_culture_drift). Epsilon differs sharply across the family: this reading authors approximately 0.03 (nothing transfers, nothing is extracted); the normative reading would carry meaningful enforcement and violation-cost structure (law must be administered and breaches punished); the strategic-culture reading would carry identity-lock and discourse-maintenance structure. Each member gets its own beneficiaries, victims, and classification; measuring one observable (do norms condemn total war) versus another (is total war physically executable) changes which member of the family you are reading, not the value of epsilon within any member.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
