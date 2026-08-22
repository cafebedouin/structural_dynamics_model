% ============================================================================
% CONSTRAINT STORY: nuclear_impossibility_kernel__structural_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nuclear_impossibility_kernel__structural_contraction_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: nuclear_impossibility_kernel__structural_contraction_reading
 *   human_readable: Nuclear Mutual Annihilation Impossibility (Structural Contraction Reading)
 *   domain: strategic_studies/international_relations/nuclear_deterrence
 *
 * SUMMARY:
 *   Nuclear weapons created a physical impossibility: once both superpowers
 *   possessed survivable second-strike capabilities, rational victory became
 *   unreachable because mutual annihilation is guaranteed. This is a reading
 *   of the contested kernel around nuclear deterrence's founding paradox. The
 *   structural_contraction_reading claims that war between nuclear-armed peer
 *   states exits the reachable set of rational outcomes entirely — not
 *   because the cost-benefit is unfavorable, but because the outcome is
 *   structurally impossible. War requires a winning state; mutual
 *   annihilation has no winning state. Therefore, war (defined as an
 *   organized political instrument with achievable aims) becomes literally
 *   impossible between peers with secure second-strike forces. This reading
 *   differs from siblings: the credibility_paradox_reading emphasizes the
 *   logical contradiction in the deterrent threat (credible threat requires
 *   willingness to execute; execution is mutual suicide; therefore the threat
 *   is logically incredible), and the rational_dropout_reading argues that
 *   victory remains structurally possible but costs exceed any benefit (a
 *   different calculus than impossibility). The structural_contraction
 *   reading makes a stronger claim: the action is not irrational, it is
 *   unreachable.
 *
 * KEY AGENTS:
 *   - nuclear_weapons_states — institutional actors holding arsenals; trapped by verification and breakout risk; benefit from impossibility foreclosing their own aggressive options but cannot escape unilaterally
 *   - civilian_populations — powerless beneficiaries protected from thermonuclear annihilation; depend on deterrent credibility which requires willingness-to-use that is simultaneously impossible
 *   - military_planners — institutional actors required to operationalize an impossibility; must maintain deterrent readiness while knowing use guarantees mutual destruction
 *   - non_nuclear_states — beneficiaries of impossibility at global scale but exposed to substitution mechanisms (proxy wars, regional conflicts)
 *   - theoretical analysis community — observers and articulate framers of the impossibility; their recognition and publication do not change the underlying physics but inform policy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_impossibility_kernel__structural_contraction_reading, 0.15).
domain_priors:suppression_score(nuclear_impossibility_kernel__structural_contraction_reading, 0.22).
domain_priors:theater_ratio(nuclear_impossibility_kernel__structural_contraction_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__structural_contraction_reading, mountain).
narrative_ontology:human_readable(nuclear_impossibility_kernel__structural_contraction_reading, "Nuclear Mutual Annihilation Impossibility (Structural Contraction Reading)").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__structural_contraction_reading, "strategic_studies/international_relations/nuclear_deterrence").

domain_priors:emerges_naturally(nuclear_impossibility_kernel__structural_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__structural_contraction_reading, '6e377fcf-9c5e-45aa-af8c-aa02970d3d9b').
narrative_ontology:cs_kernel_codification('6e377fcf-9c5e-45aa-af8c-aa02970d3d9b', fixed_text).
narrative_ontology:cs_authority_grounding('6e377fcf-9c5e-45aa-af8c-aa02970d3d9b', expertise).
narrative_ontology:cs_interpretation_layer_present('6e377fcf-9c5e-45aa-af8c-aa02970d3d9b').
narrative_ontology:cs_reading_relation('6e377fcf-9c5e-45aa-af8c-aa02970d3d9b', nuclear_impossibility_kernel__credibility_paradox_reading, influences).
narrative_ontology:cs_reading_relation('6e377fcf-9c5e-45aa-af8c-aa02970d3d9b', nuclear_impossibility_kernel__rational_dropout_reading, forecloses).
narrative_ontology:cs_axiom('6e377fcf-9c5e-45aa-af8c-aa02970d3d9b', foundational, mutual_annihilation_structurally_inevitable).
narrative_ontology:cs_axiom_status(mutual_annihilation_structurally_inevitable, holdable).
narrative_ontology:cs_axiom_grounding('6e377fcf-9c5e-45aa-af8c-aa02970d3d9b', mutual_annihilation_structurally_inevitable, empirically_contingent).
narrative_ontology:cs_axiom('6e377fcf-9c5e-45aa-af8c-aa02970d3d9b', foundational, victory_undefined_under_mutual_destruction).
narrative_ontology:cs_axiom_status(victory_undefined_under_mutual_destruction, holdable).
narrative_ontology:cs_axiom_grounding('6e377fcf-9c5e-45aa-af8c-aa02970d3d9b', victory_undefined_under_mutual_destruction, deontological).
narrative_ontology:cs_reference_frame('6e377fcf-9c5e-45aa-af8c-aa02970d3d9b', mutual_assured_destruction_stability).
narrative_ontology:cs_drift_state('6e377fcf-9c5e-45aa-af8c-aa02970d3d9b', contemporary_proliferation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('6e377fcf-9c5e-45aa-af8c-aa02970d3d9b', '').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__structural_contraction_reading, humanity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_weapons_states).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__structural_contraction_reading, non_nuclear_states).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__structural_contraction_reading, civilian_populations).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__structural_contraction_reading, military_planners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess arsenals capable of mutual annihilation. The structural impossibility of victory constrains their strategic options and forecloses certain aggressive paths, which indirectly benefits them by preventing escalation spirals that would consume them. They cannot exit the constraint through disarmament (verification, breakout risk, strategic vulnerability) even when it would benefit them individually.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_weapons_states, beneficiary,
    institutional, civilizational, trapped, global).

% Exist within the shadow of the impossibility. Nuclear states' strategic paralysis at the global level creates space for regional wars, proxy conflicts, and non-nuclear coercion — which they experience as threats. They benefit from the impossibility foreclosing global thermonuclear escalation but bear costs from the substitution mechanisms (proxy wars, local conflicts).
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, non_nuclear_states, beneficiary,
    organized, generational, mobile, global).

% Are the primary targets of nuclear weapons. The impossibility of victory means nuclear weapons have become strategically unusable at the scale of rational calculation, leaving their lives protected (relative to a world where nuclear war occurs) but held hostage to the continuing credibility of deterrence. They cannot exit; they depend on the constraint holding.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, civilian_populations, beneficiary,
    powerless, biographical, trapped, global).

% Must operate under the impossibility. Strategic doctrine assumes victory is impossible for all parties; war termination becomes the organizing problem instead of victory. They must maintain deterrent credibility while knowing the threat is unusable, which creates doctrinal strain and continuous adaptation costs (declaratory vs. operational policy, escalation management).
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, military_planners, payer,
    organized, biographical, constrained, national).

% Recognize and articulate the structural impossibility. They observe that the mathematics of mutual annihilation is not a convention or choice but a consequence of biophysics and weapons effects. Their analysis feeds policy formation but does not change the underlying physical constraint.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, theoretical_physicists_and_strategists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The impossibility establishes a shared interest in not fighting: both sides recognize that thermonuclear exchange eliminates all benefit. This is not coordination in the sense of voluntary agreement, but coordination in the sense of converging interests — neither side can rationally pursue victory through nuclear means because victory is physically impossible.
% TRANSFER_FUNCTION: The constraint transfers strategic optionality from nuclear weapons states to other forms of competition (proxy wars, economic pressure, intelligence operations, subconversion). Nuclear weapons remain in arsenals as deterrents but cannot be used for territorial conquest or forced regime change without mutual annihilation.
% ABSENT_VOICES: A rational actor that believes nuclear annihilation is survivable and that victory is worth the cost would object to this reading as overstating the impossibility. Such an actor is absent from contemporary strategic discourse (no credible state or movement makes this claim publicly), but the absence itself is informative — the reading's core claim has achieved near-universal acceptance among decision-makers.
% DISAPPEARANCE_RATIONALE: If the impossibility vanished (weapons lost their annihilatory character, or a technical defense were invented), military calculation would reopen: nuclear states could rationally contemplate nuclear war as a path to objectives. The entire architecture of deterrence, arms control, non-proliferation norms, and strategic doctrine would reorganize around a different calculus. The disappearance would be among the largest structural shifts in modern international relations.
% FOUNDING_PROBLEM: Once both superpowers possessed arsenals large enough and survivable enough (second-strike capability) to annihilate each other regardless of first-strike advantage, a paradox emerged: rational actors should prefer not to fight, but the deterrent threat required seeming willing to fight. Mutual assured destruction (MAD) was the formalization of the impossibility — the recognition that victory was no longer attainable.
% FOUNDING_PROBLEM_CORROBORATION: The impossibility is recognized across the strategic community: from RAND Corporation analysts and academic strategists (who have studied and published on the subject) to military planners (whose war games operationalize it) to arms control negotiators (whose treaties rest on the assumption that strategic stability requires accepting it). Adversarial states (US and USSR during Cold War, now US and Russia, US and China) acknowledge it in declaratory policy and operational restraint, not through explicit agreement but through convergent behavior. Independent verification: no credible strategic analysis challenges the core physics or mathematics of the impossibility; disagreements are about its implications, not its existence.
narrative_ontology:disappearance_verdict(nuclear_impossibility_kernel__structural_contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(nuclear_impossibility_kernel__structural_contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nuclear_impossibility_kernel__structural_contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nuclear_impossibility_kernel__structural_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nuclear_impossibility_kernel__structural_contraction_reading, 0.15, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_impossibility_kernel__structural_contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(nuclear_impossibility_kernel__structural_contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(nuclear_impossibility_kernel__structural_contraction_reading),
    narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(nuclear_impossibility_kernel__structural_contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as mountain because the impossibility emerges from physics and mathematical properties of the strategic game, not from institutional choice or convention. Extractiveness is low (0.15 at interval end) because the constraint produces no net transfer from one actor to another — mutual annihilation is not extractive in the classical sense (no one collects the outcome). The beneficiary declaration is unusual for a mountain: 'humanity' benefits from a natural law that forecloses organized violence at the largest scale. Suppression is low (0.22) because the impossibility is not maintained through coercion but through the structure of weapons effects and strategic geometry — actors need not be forced to accept that mutual annihilation is irrational; they recognize it. Accessibility_collapse is very high (0.92) because once the impossibility is understood, there is no real alternative path to nuclear victory — the reachable set of rational strategies contracts sharply. Resistance is near-zero (0.03) because there is no credible resistance to a physical impossibility; the only resistance is theoretical (actors who deny mutual destruction is guaranteed, or who claim superiority in a post-nuclear environment) but this resistance is marginalized in contemporary doctrine. Theater_ratio is very low (0.08) because the constraint's enforcement is the constraint itself, not theatrical maintenance — states comply because the impossibility is real, not because they are performing compliance. The measurement series shows extractiveness and suppression rising slightly from 1945 (when the impossibility was emerging) through the 1975-1991 period (Cold War peak), then stabilizing as the impossibility became settled doctrine. The rise reflects the period during which the constraint was being articulated and operationalized in doctrine; the plateau reflects the constraint achieving consensus recognition.
 *
 * PERSPECTIVAL GAP:
 *   This mountain should compute identically across all seats because the impossibility is physics-grounded: from the nuclear weapons state's perspective, from the civilian population's perspective, from the strategic theorist's perspective, the outcome is the same — war between peers with secure second-strike is impossible. There should be minimal divergence in the per-seat classifications. Divergence would signal that the constraint is not as natural or universal as claimed — a sign for the false_summit_mountain omega to investigate whether institutional actors maintain the impossibility-frame to preserve institutional interests.
 *
 * DIRECTIONALITY LOGIC:
 *   This constraint has no traditional directionality because it has no enforcer and no target. Unlike tangled_rope (where an agenda-setter benefits and a payer bears costs) or snare (where there is coercive extraction), the impossibility is a mutual constraint on all parties: no one can escape it, all are constrained equally by the physics. Beneficiaries are declared as 'humanity' because the constraint protects humans from thermonuclear annihilation. This is unusual but coherent: a mountain can have beneficiaries (those protected by it) without having victims (those harmed by its imposition). The lack of directionality-variability across seats is the signal that this is indeed a mountain, not a weaker category attempting to hide behind the natural-law framing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (rational path to victory was open until both superpowers achieved secure second-strike capabilities; then it closed) is live — the impossibility persists because the material conditions that created it (large survivable arsenals) persist. The constraint is not zombie (persisting after its problem is solved) but rather structural (it will persist as long as mutual annihilation remains the outcome of all-out exchange). The reading explicitly rejects mandatrophy: the founding problem has not been solved, and the constraint has not outlived its function. If weapons were dismantled or defense systems made first-strike decisive, the impossibility would vanish — but neither has occurred, and the founding problem remains.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_technical_contingency,
    'Is the impossibility of nuclear victory a law-like feature of physics that would survive fundamental changes in weapons technology (e.g., missile defense breakthrough, nuclear hardening, weapons miniaturization), or is it contingent on the current state of arsenals and delivery systems?',
    'Technical analysis of credible defense mechanisms (strategic missile defense, hardened deep-earth bunkers, dispersed small-yield warheads) to determine whether mutual annihilation could be escaped through engineering rather than negotiation. Historical precedent: every major strategic paradigm shift (from horse cavalry to tanks, from battleships to aircraft, from air superiority to distributed networks) eventually found technical counters; whether nuclear annihilation has a technical escape hatch is unresolved.',
    'If a technical counter emerges and is credible, the impossibility becomes a temporary technological state rather than a natural law — the constraint would degrade from mountain to a weaker category. If no plausible technical escape exists, the impossibility is validated as a fundamental feature of the strategic landscape.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_technical_contingency, empirical, 'Whether nuclear annihilation impossibility is law-like or technology-contingent.').

omega_variable(
    reading_boundary_contraction_vs_rational_dropout,
    'Does this reading (structural contraction: no rational path to victory EXISTS) logically foreclose the rational_dropout_reading (victory structurally possible but costs exceed any benefit), or do they describe the same phenomenon from different analytical frames?',
    'Formal analysis of the decision-theoretic structure: if victory is defined as a state-space outcome and costs include annihilation, then ''no rational path'' and ''victory possible but irrational to pursue'' may be mathematically equivalent statements about the same game. Alternatively, if ''possible'' means ''not ruled out by physics'' and ''rational'' means ''maximizes expected utility'', the readings are semantically distinct even if empirically indistinguishable in current arsenals.',
    'If the readings are equivalent framings, the engine should detect them as redundant competitors in the same kernel. If they are distinct (contraction forecloses rational dropout), this reading forecloses its sibling. The boundary is sharp in formal logic but blurred in strategic doctrine.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_boundary_contraction_vs_rational_dropout, conceptual, 'Whether structural contraction logically forecloses rational dropout or they describe the same constraint differently.').

omega_variable(
    beneficiary_asymmetry_mountain_false_summit,
    'Does humanity benefit from the impossibility of nuclear victory, or is the beneficiary claim an instance of false-summit reasoning (the constraint is treated as natural law but actually benefits identifiable actors: deterrence theorists, defense contractors, state security establishments)?',
    'Examine whether the constraint persists because it is natural law (would hold regardless of actor interests) or because it is maintained by benefiting institutional actors (if actors abandoned arms control and verification, the constraint would degrade faster than physics alone would suggest). Test case: the commitment cost to maintain verification and arms control treaties despite cost — if institutions abandon them, the measurement trajectory should steepen.',
    'If false-summit: the engine would reclassify the constraint via the false_summit_mountain signature, flagging institutional capture of the natural-law framing. If genuine mountain: the beneficiary claim is legitimate (humanity benefits from a natural law that constrains rational violence).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_asymmetry_mountain_false_summit, empirical, 'Whether the impossibility is natural law or benefits-dependent institutional persistence.').

omega_variable(
    proxy_war_substitution_scope,
    'The structural impossibility forecloses direct nuclear war between powers. Does it also foreclose conventional war, or only nuclear war, leaving conventional war and proxy conflict as rational substitutes within the impossibility''s constraints?',
    'Historical and forward-looking analysis of whether the impossibility has changed the rate, duration, or scale of non-nuclear conflict between nuclear-armed states. The presence of proxy wars (Korea, Vietnam, Afghanistan) and near-peer conventional conflicts (India-Pakistan) during the nuclear age suggests substitution is occurring — the question is whether the impossibility constrains these substitutes or merely redirects competition.',
    'If conventional war remains rational and unconstrained by the nuclear impossibility, the constraint applies only at the thermonuclear threshold, not to the full range of inter-state conflict. This affects whether the constraint''s beneficiaries (civilian populations protected from annihilation) also experience harms through substitution mechanisms.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(proxy_war_substitution_scope, empirical, 'Whether the impossibility constrains all forms of inter-nuclear-state conflict or only nuclear war itself.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__structural_contraction_reading, 1945, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nucl_tr_t1945, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1945, 0.02).
narrative_ontology:measurement_basis(nucl_tr_t1945, projected).
narrative_ontology:measurement(nucl_tr_t1962, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1962, 0.05).
narrative_ontology:measurement_basis(nucl_tr_t1962, observed).
narrative_ontology:measurement(nucl_tr_t1975, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1975, 0.07).
narrative_ontology:measurement_basis(nucl_tr_t1975, observed).
narrative_ontology:measurement(nucl_tr_t1991, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1991, 0.09).
narrative_ontology:measurement_basis(nucl_tr_t1991, observed).
narrative_ontology:measurement(nucl_tr_t2010, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 2010, 0.08).
narrative_ontology:measurement_basis(nucl_tr_t2010, observed).
narrative_ontology:measurement(nucl_tr_t2026, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 2026, 0.08).
narrative_ontology:measurement_basis(nucl_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(nucl_be_t1945, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1945, 0.05).
narrative_ontology:measurement_basis(nucl_be_t1945, projected).
narrative_ontology:measurement(nucl_be_t1962, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1962, 0.12).
narrative_ontology:measurement_basis(nucl_be_t1962, observed).
narrative_ontology:measurement(nucl_be_t1975, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1975, 0.14).
narrative_ontology:measurement_basis(nucl_be_t1975, observed).
narrative_ontology:measurement(nucl_be_t1991, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1991, 0.18).
narrative_ontology:measurement_basis(nucl_be_t1991, observed).
narrative_ontology:measurement(nucl_be_t2010, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 2010, 0.16).
narrative_ontology:measurement_basis(nucl_be_t2010, observed).
narrative_ontology:measurement(nucl_be_t2026, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 2026, 0.15).
narrative_ontology:measurement_basis(nucl_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(nucl_su_t1945, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 1945, 0.08).
narrative_ontology:measurement_basis(nucl_su_t1945, projected).
narrative_ontology:measurement(nucl_su_t1962, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 1962, 0.18).
narrative_ontology:measurement_basis(nucl_su_t1962, observed).
narrative_ontology:measurement(nucl_su_t1975, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 1975, 0.22).
narrative_ontology:measurement_basis(nucl_su_t1975, observed).
narrative_ontology:measurement(nucl_su_t1991, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 1991, 0.25).
narrative_ontology:measurement_basis(nucl_su_t1991, observed).
narrative_ontology:measurement(nucl_su_t2010, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 2010, 0.23).
narrative_ontology:measurement_basis(nucl_su_t2010, observed).
narrative_ontology:measurement(nucl_su_t2026, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 2026, 0.22).
narrative_ontology:measurement_basis(nucl_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_impossibility_kernel__structural_contraction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(nuclear_impossibility_kernel__structural_contraction_reading, 0.12).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_impossibility_kernel__credibility_paradox_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_impossibility_kernel__rational_dropout_reading).

% DUAL FORMULATION NOTE:
% The nuclear_impossibility_kernel contains three readings: structural_contraction (this story), credibility_paradox, and rational_dropout. Each reading instantiates a different analytical frame on the same underlying strategic game. The structural_contraction_reading claims that war exits the reachable set entirely; the credibility_paradox_reading claims the deterrent threat is logically impossible; the rational_dropout_reading claims victory is possible but irrational. These are distinct constraints with different ε values and different beneficiary/victim structures, all grounded in the same kernel: mutual thermonuclear annihilation. All three stories must be linked via network.affects_constraints to indicate their family relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
