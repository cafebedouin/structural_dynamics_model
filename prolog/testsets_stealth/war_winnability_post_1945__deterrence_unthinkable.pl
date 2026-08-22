% ============================================================================
% CONSTRAINT STORY: war_winnability_post_1945__deterrence_unthinkable
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_winnability_post_1945__deterrence_unthinkable, []).

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
 *   constraint_id: war_winnability_post_1945__deterrence_unthinkable
 *   human_readable: Post-1945 Categorical Unwinnability Doctrine (Deterrence-Unthinkable Reading)
 *   domain: strategic_studies/nuclear_deterrence/international_relations
 *
 * SUMMARY:
 *   This story instantiates the deterrence-unthinkable reading of the
 *   post-1945 war-winnability kernel: the standing arrangement under contest
 *   is the mutual-vulnerability order in which great-power total war is
 *   treated as categorically unwinnable and victory planning is foreclosed as
 *   incoherent. Epsilon's referent is that standing arrangement, assessed by
 *   this reading's own lights — the arrangement is substantially
 *   physics-grounded, so extraction is real but bounded: what is extracted is
 *   not money but mission coherence, operational autonomy, and the legitimacy
 *   of an entire school of military craft. The claim/metric gap is
 *   deliberate: the arrangement is CLAIMED here as tangled_rope (genuine
 *   catastrophe-avoidance coordination carrying asymmetric extraction), while
 *   the metrics describe its actual mixed operation. The engine computes
 *   per-seat classifications from the structural data; the authored claim
 *   does not adjudicate them.
 *
 * KEY AGENTS:
 *   - civilian_populations_nuclear_powers: primary beneficiary (organized/trapped) — protected from total war, unable to exit the umbrella
 *   - national_political_leaderships: agenda-setter and beneficiary (institutional/constrained) — administers doctrine, collects stability and legitimacy, surrendered war as policy instrument
 *   - deterrence_theory_epistemic_community: secondary beneficiary (organized/identity_locked) — paradigm custodian collecting authority and careers from the foreclosure
 *   - military_establishments_nuclear_powers: primary target (institutional/trapped) — absorbs mission incoherence, cannot resign
 *   - strategic_offensive_planners: target (moderate/identity_locked) — craft declared incoherent, work continues, identity fused with the forbidden art
 *   - countervailing_strategists: excluded dissenter (organized/mobile) — holds the sibling reading, periodically contests the closure
 *   - extended_deterrence_allies: secondary beneficiary-payer (institutional/constrained) — protection received, autonomy surrendered
 *   - cold_war_archival_historians: analytical observer — sees declaratory doctrine against operational archives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__deterrence_unthinkable, 0.58).
domain_priors:suppression_score(war_winnability_post_1945__deterrence_unthinkable, 0.66).
domain_priors:theater_ratio(war_winnability_post_1945__deterrence_unthinkable, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, extractiveness, 0.58).
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, accessibility_collapse, 0.74).
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__deterrence_unthinkable, tangled_rope).
narrative_ontology:human_readable(war_winnability_post_1945__deterrence_unthinkable, "Post-1945 Categorical Unwinnability Doctrine (Deterrence-Unthinkable Reading)").
narrative_ontology:topic_domain(war_winnability_post_1945__deterrence_unthinkable, "strategic_studies/nuclear_deterrence/international_relations").

domain_priors:requires_active_enforcement(war_winnability_post_1945__deterrence_unthinkable).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__deterrence_unthinkable, '88458219-3034-450b-a4a1-c8b051d58cbc').
narrative_ontology:cs_kernel_codification('88458219-3034-450b-a4a1-c8b051d58cbc', distributed).
narrative_ontology:cs_authority_grounding('88458219-3034-450b-a4a1-c8b051d58cbc', expertise).
narrative_ontology:cs_interpretation_layer_present('88458219-3034-450b-a4a1-c8b051d58cbc').
narrative_ontology:cs_reading_relation('88458219-3034-450b-a4a1-c8b051d58cbc', war_winnability_post_1945__countervailing_thinkable, forecloses).
narrative_ontology:cs_reading_relation('88458219-3034-450b-a4a1-c8b051d58cbc', war_winnability_post_1945__rhetorical_contraction, coexists_with).
narrative_ontology:cs_axiom('88458219-3034-450b-a4a1-c8b051d58cbc', foundational, great_power_total_war_categorically_unwinnable).
narrative_ontology:cs_axiom_status(great_power_total_war_categorically_unwinnable, holdable).
narrative_ontology:cs_axiom_grounding('88458219-3034-450b-a4a1-c8b051d58cbc', great_power_total_war_categorically_unwinnable, empirically_contingent).
narrative_ontology:cs_axiom('88458219-3034-450b-a4a1-c8b051d58cbc', foundational, nuclear_escalation_inherently_uncontrollable).
narrative_ontology:cs_axiom_status(nuclear_escalation_inherently_uncontrollable, holdable).
narrative_ontology:cs_axiom_grounding('88458219-3034-450b-a4a1-c8b051d58cbc', nuclear_escalation_inherently_uncontrollable, empirically_contingent).
narrative_ontology:cs_axiom('88458219-3034-450b-a4a1-c8b051d58cbc', secondary, deterrent_posture_over_warfighting_capacity).
narrative_ontology:cs_axiom_status(deterrent_posture_over_warfighting_capacity, holdable).
narrative_ontology:cs_axiom_grounding('88458219-3034-450b-a4a1-c8b051d58cbc', deterrent_posture_over_warfighting_capacity, instrumental).
narrative_ontology:cs_reference_frame('88458219-3034-450b-a4a1-c8b051d58cbc', categorical_unwinnability_baseline).
narrative_ontology:cs_drift_state('88458219-3034-450b-a4a1-c8b051d58cbc', contemporary_counterforce_revival, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('88458219-3034-450b-a4a1-c8b051d58cbc', '').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__deterrence_unthinkable, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__deterrence_unthinkable, civilian_populations_nuclear_powers).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__deterrence_unthinkable, national_political_leaderships).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__deterrence_unthinkable, deterrence_theory_epistemic_community).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__deterrence_unthinkable, extended_deterrence_allies).
narrative_ontology:constraint_victim(war_winnability_post_1945__deterrence_unthinkable, military_establishments_nuclear_powers).
narrative_ontology:constraint_victim(war_winnability_post_1945__deterrence_unthinkable, strategic_offensive_planners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(war_winnability_post_1945__deterrence_unthinkable, extended_deterrence_allies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live under the mutual-vulnerability umbrella: their cities are hostage to enemy arsenals, and in exchange the probability of great-power total war drops to near zero. They neither administer the arrangement nor chose it; their protection is a byproduct of two adversaries holding each other's societies at risk. Exit is meaningless — there is no moving out from under intercontinental range.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, civilian_populations_nuclear_powers, beneficiary,
    organized, generational, trapped, continental).

% Authorize declaratory doctrine, command the forces, and take electoral and historical credit for the absence of great-power war. They also surrendered the option of war as an instrument of policy: the doctrine tells them no victory is available at any price they could pay, and crisis decisions compress into choices among losses. Shifting doctrine is possible but domestically costly and alliance-destabilizing.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, national_political_leaderships, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(war_winnability_post_1945__deterrence_unthinkable, national_political_leaderships, beneficiary).

% Civilian strategists, think-tank analysts, and academic theorists who supplied the intellectual machinery of the unwinnability thesis — systems analysis, game-theoretic bargaining models, escalation-ladder logic. The doctrine's persistence sustains their institutions, curricula, consultancies, and authority to define what counts as serious strategy. Their professional identity is fused with the paradigm; conceding the countervailing critique would dissolve a life's framework.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, deterrence_theory_epistemic_community, beneficiary,
    organized, generational, identity_locked, global).

% Maintain, secure, and train on arsenals whose official doctrine declares their use incapable of achieving victory. They absorb the mission incoherence: organizations built to fight and win wars must instead organize around averted wars, subordinating operational judgment to civilian deterrence logic. Resignation is unavailable to an institution; their budget flows from the same doctrine that hollows their traditional mission.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, military_establishments_nuclear_powers, payer,
    institutional, generational, trapped, global).

% Targeteers, SIOP staff, and war-game designers whose craft — damage limitation, counterforce sequencing, war termination — official doctrine labels incoherent even as the work continues in classified channels. Many are privately committed to the warfighting view the declaratory layer forbids. Career advancement requires professing the doctrine; their technical identity binds them to the work the doctrine disparages.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, strategic_offensive_planners, payer,
    moderate, biographical, identity_locked, national).

% Warfighting-school analysts and counterforce advocates who argue limited victory remains achievable and that the categorical-unwinnability claim is analytically lazy. They publish at the margins of the dominant conversation, rotate through think tanks and occasional government posts, and periodically capture doctrine (late-1970s countervailing strategy, missile-defense advocacy) before the orthodox frame reasserts itself.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, countervailing_strategists, excluded,
    organized, generational, mobile, national).

% Non-weapon states sheltered under a patron's arsenal: they receive protection without bearing the direct mission-incoherence cost, but they surrender autonomous defense options and accept that their security rests on a patron's willingness to court annihilation on their behalf. Acquiring independent arsenals would break treaty commitments and alliance trust, so exit is constrained.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, extended_deterrence_allies, beneficiary,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(war_winnability_post_1945__deterrence_unthinkable, extended_deterrence_allies, payer).

% Retrospective analysts with access to declassified war plans, crisis transcripts, and leadership deliberations on both sides. They can compare declaratory doctrine against operational reality across the whole interval and attest to where the foreclosure was real, where it was professed, and where planning quietly continued.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, cold_war_archival_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_winnability_post_1945__deterrence_unthinkable, deterrence_theory_epistemic_community).
narrative_ontology:fixing_cost_class(war_winnability_post_1945__deterrence_unthinkable, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of inadvertent or deliberate great-power annihilation: mutual vulnerability gives both adversaries a shared stake in crisis restraint, communication, and reciprocal predictability, and lets alliances coordinate protection without each member solving its own security from scratch.
% TRANSFER_FUNCTION: Moves operational autonomy and definitional authority over strategy from military establishments to civilian deterrence theorists and political leaderships; moves the existential risk of total war off the negotiating table and onto civilian populations as hostages; moves budget share toward survivable delivery systems and away from warfighting capacity.
% ABSENT_VOICES: Countervailing strategists and warfighting planners object from the margins — present in the profession but excluded from the declaratory conversation that defines legitimacy. The publics of nuclear powers, who bear the hostage risk, are absent entirely: no seat represents their consent to being held at risk, and targeting decisions were never submitted to them.
% DISAPPEARANCE_RATIONALE: Alert postures, extended-deterrence guarantees, arms-control architecture, crisis communication channels, and civil-military relations all presuppose the foreclosure of victory planning. Remove it overnight and force postures immediately compete over warfighting options, allies hedge toward independent arsenals, proliferation cascades begin, and every crisis reopens the question the doctrine had closed.
% FOUNDING_PROBLEM: After 1945, and acutely after thermonuclear weapons, total war between industrial great powers became civilizational suicide while military institutions remained organized, staffed, and budgeted to fight and win exactly such wars. The founding problem was reconciling standing warfighting establishments with a weapon that made their central mission self-defeating.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: Soviet military theory (Sokolovskii's Military Strategy) independently conceded mutual devastation; retired senior commanders on both sides — most prominently the former commander of Strategic Air Command — attested after leaving office that no coherent path to victory existed in the plans they had overseen; declassified crisis deliberations show leaders on both sides privately treating war as unusable regardless of public posture. The countervailing school contests the doctrine's adequacy, not the existence of the founding problem.
narrative_ontology:disappearance_verdict(war_winnability_post_1945__deterrence_unthinkable, world_rearranges).
narrative_ontology:founding_problem_status(war_winnability_post_1945__deterrence_unthinkable, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_winnability_post_1945__deterrence_unthinkable, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(war_winnability_post_1945__deterrence_unthinkable, 'none', 1).
narrative_ontology:epsilon_provenance(war_winnability_post_1945__deterrence_unthinkable, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_winnability_post_1945__deterrence_unthinkable_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_winnability_post_1945__deterrence_unthinkable, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_winnability_post_1945__deterrence_unthinkable_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate-high (0.58 at interval end) because the arrangement's costs fall on identifiable seats — mission coherence, planning autonomy, professional legitimacy — while its justification rests on a physical substrate no one disputes. Suppression (0.66) is a raw structural property, unscaled by power or scope: the foreclosure is enforced through civilian control regimes, budget politics, professional sanction against warfighting advocacy, and the declaratory taboo, not through participant preference. Theater (0.52) is elevated because the declaratory layer (assured-destruction rhetoric, arms-control ceremony) has persistently diverged from operational practice (counterforce targeting, damage-limitation studies) — a gap the rhetorical_contraction sibling reading takes as its whole subject. Accessibility collapse (0.74) is high but short of natural-law levels because the countervailing school demonstrates the alternatives never fully died. Resistance (0.60) is sustained and recurring. The measurement series run on one shared grid; the trajectories oscillate rather than drift monotonically — orthodoxy consolidates, a warfighting challenge rises, a synthesis relaxes suppression, accumulation resumes — a doctrinal pendulum driven by alternating threat assessments and technological change (accuracy revolutions, missile defense), not by intermittent reinforcement. Base properties are measured at interval end, in the current re-hardening phase of the cycle.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats compute differently from identical structural data. From the military establishment's position the arrangement confiscates its reason for being while conscripting it as the arrangement's hardware; from the civilian population's position the same structure is the only thing standing between them and annihilation; from the epistemic community's position it is a hard-won rational achievement; from the planner's desk it is a doctrine that calls his life's work incoherent while signing his paycheck. The engine computes this divergence per seat; nothing in the authored claim resolves it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for populations, leaderships, allies, and the epistemic community; victim declarations drive high directionality for the military establishments and the planners. Exit modulation sharpens the split: trapped militaries sit nearer the full-target end than their budget size would suggest, and identity_locked planners and theorists carry amplified d in opposite directions — the planner locked into a condemned craft, the theorist locked into the condemning paradigm. Populations are trapped but subsidized (d near zero despite immobility); leaderships derive partial d because the doctrine both stabilizes their rule and strips their instrument set.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid classification is what keeps this story honest in both directions. Read as pure rope, the arrangement's extraction disappears — mission incoherence, career foreclosure, and the suppression of an entire strategic school vanish into 'the price of peace.' Read as pure snare, the genuine catastrophe-avoidance function disappears — and with it the explanation for why no great-power total war has occurred since 1945. The founding problem (reconciling standing warfighting establishments with suicidal weapons) remains contested-live: the problem of nuclear catastrophe persists, but whether THIS doctrine solves it, or merely manages it while warfighting capacity accumulates underneath, is exactly what the sibling readings dispute. The mismatch consumer should watch founding_problem_status=contested against disappearance_verdict=world_rearranges: the arrangement is load-bearing even under dispute, which is the signature of a live tangled rope rather than a resolved mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'Which reading of the war_winnability_post_1945 kernel correctly characterizes the standing arrangement — full operational foreclosure (this reading), conditional winnability (countervailing_thinkable), or rhetorical-only contraction (rhetorical_contraction)?',
    'Systematic comparison of declassified war plans and employment doctrine against declaratory policy across the interval; behavioral evidence from crises (whether leaders treated limited options as real); capability audits of counterforce leg versus arsenal survivability.',
    'If rhetorical_contraction is correct, this story''s victim structure relocates — the extraction becomes hypocrisy cost borne by publics rather than mission incoherence borne by militaries — and the classification trends toward piton-flavored declaratory theater. If countervailing_thinkable is correct, the victim class largely dissolves and the arrangement reads as contested rope. If this reading is correct, the current classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame omega: this constraint is one reading of the winnability kernel; sibling readings would restructure beneficiaries and victims.').

omega_variable(
    physics_vs_doctrine_boundary,
    'How much of the categorical foreclosure is irreducible physics (thermonuclear destructiveness, fallout, escalation dynamics) versus constructed doctrine layered above it?',
    'Independent technical assessment of counterforce lethality against hardened, dispersed, decoyed arsenals under realistic assumptions; escalation-model sensitivity analysis on limited-exchange scenarios.',
    'If physics alone does the foreclosing, the arrangement approaches pure coordination and extraction from militaries is unjustified constraint-riding. If doctrine forecloses beyond what physics requires, the excess is extractive overlay and the classification trends toward the snare boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physics_vs_doctrine_boundary, empirical, 'Whether the unwinnability claim tracks physical necessity or doctrinal construction.').

omega_variable(
    military_net_position_ambiguity,
    'Do military establishments suffer net extraction at all, given that deterrence doctrine justifies their budgets, sizes, and political insulation?',
    'Organizational analysis of civil-military friction, planner attrition, institutional doctrine disputes, and whether warfighting-capacity programs were funded or starved relative to service preferences.',
    'If the net position is positive (deterrence pays better than warfighting ever did), the victims list shrinks to the identity-locked planner stratum and the classification trends toward rope; if negative, the tangled-rope reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(military_net_position_ambiguity, empirical, 'Whether the declared victim class is net-extracted or net-subsidized.').

omega_variable(
    declaratory_operational_divergence_direction,
    'Is the persistent gap between declaratory unwinnability and operational counterforce planning converging (doctrine catching up to practice) or diverging (practice accumulating beneath a frozen doctrine)?',
    'Time-series comparison of declared policy texts against procurement and targeting-data releases at successive intervals.',
    'Convergence supports this reading''s operational-contraction claim stabilizing; divergence feeds the rhetorical_contraction sibling and predicts eventual doctrinal rupture or quiet abandonment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(declaratory_operational_divergence_direction, empirical, 'Direction of the declaratory-practice gap that separates this reading from its siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__deterrence_unthinkable, 1946, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wwp1945_det_unthink_tr_t1946, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 1946, 0.15).
narrative_ontology:measurement(wwp1945_det_unthink_tr_t1954, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 1954, 0.26).
narrative_ontology:measurement(wwp1945_det_unthink_tr_t1962, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 1962, 0.34).
narrative_ontology:measurement(wwp1945_det_unthink_tr_t1974, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 1974, 0.48).
narrative_ontology:measurement(wwp1945_det_unthink_tr_t1983, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 1983, 0.41).
narrative_ontology:measurement(wwp1945_det_unthink_tr_t1991, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 1991, 0.44).
narrative_ontology:measurement(wwp1945_det_unthink_tr_t2003, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 2003, 0.47).
narrative_ontology:measurement(wwp1945_det_unthink_tr_t2025, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 2025, 0.52).

% Extraction over time
narrative_ontology:measurement(wwp1945_det_unthink_be_t1946, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 1946, 0.22).
narrative_ontology:measurement(wwp1945_det_unthink_be_t1954, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 1954, 0.38).
narrative_ontology:measurement(wwp1945_det_unthink_be_t1962, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 1962, 0.5).
narrative_ontology:measurement(wwp1945_det_unthink_be_t1974, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 1974, 0.61).
narrative_ontology:measurement(wwp1945_det_unthink_be_t1983, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 1983, 0.57).
narrative_ontology:measurement(wwp1945_det_unthink_be_t1991, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 1991, 0.49).
narrative_ontology:measurement(wwp1945_det_unthink_be_t2003, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 2003, 0.53).
narrative_ontology:measurement(wwp1945_det_unthink_be_t2025, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(wwp1945_det_unthink_su_t1946, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 1946, 0.2).
narrative_ontology:measurement(wwp1945_det_unthink_su_t1954, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 1954, 0.4).
narrative_ontology:measurement(wwp1945_det_unthink_su_t1962, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 1962, 0.55).
narrative_ontology:measurement(wwp1945_det_unthink_su_t1974, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 1974, 0.68).
narrative_ontology:measurement(wwp1945_det_unthink_su_t1983, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 1983, 0.62).
narrative_ontology:measurement(wwp1945_det_unthink_su_t1991, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 1991, 0.5).
narrative_ontology:measurement(wwp1945_det_unthink_su_t2003, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 2003, 0.55).
narrative_ontology:measurement(wwp1945_det_unthink_su_t2025, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 2025, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_winnability_post_1945__deterrence_unthinkable, enforcement_mechanism).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, war_winnability_post_1945__countervailing_thinkable).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, war_winnability_post_1945__rhetorical_contraction).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial claim 'nuclear weapons ended winnable great-power war' decomposes into three structurally distinct readings of one kernel. This story (deterrence_unthinkable) carries the strongest reading — full operational foreclosure — with epsilon assessed on the standing mutual-vulnerability arrangement as this reading sees it. The countervailing_thinkable sibling carries lower epsilon on military-establishment extraction (its premise restores mission coherence) and different victim structure. The rhetorical_contraction sibling carries high theater_ratio by construction (its subject IS the declaratory-practice gap) and relocates extraction to publics. The upstream physical fact (thermonuclear destructiveness) is cited by all three as warrant; the family members differ on how far that warrant extends past physics into doctrine and speech. Each member links the others via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
