% ============================================================================
% CONSTRAINT STORY: second_amendment_text__originalist_civic_virtue_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__originalist_civic_virtue_reading, []).

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
 *   constraint_id: second_amendment_text__originalist_civic_virtue_reading
 *   human_readable: Second Amendment — Originalist Civic Virtue Reading (Citizen-Soldier Capacity)
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This file instantiates ONE reading of the contested second_amendment_text
 *   kernel: the originalist civic virtue reading, under which the
 *   founding-era well regulated Militia denoted the universal armed citizenry
 *   and the constitutional guarantee protects citizen-soldier capacity as a
 *   civic republican institution — the arrangement by which a free polity
 *   keeps military capability distributed among the people rather than
 *   monopolized by a standing force. Per the epsilon-invariance discipline,
 *   the inter-reading contest is not described inside this constraint; the
 *   sibling readings are separate files with their own epsilon, beneficiary
 *   structures, and classifications. The epsilon referent is the standing
 *   arrangement under contest — the constitutional guarantee as this reading
 *   instantiates it — assessed by this reading's own lights: a protective
 *   coordination arrangement in which the citizenry qua political community
 *   holds the protected capacity and no seat bears extraction. The claim and
 *   the metrics are independent authored facts: the reading is CLAIMED as
 *   rope (a genuine coordination arrangement with net-benefiting
 *   participants), while the metrics describe the arrangement's actual
 *   two-century operation — including a long enforcement dormancy, a
 *   late-century drift toward heritage performance, and a sharp
 *   twenty-first-century revival of judicial enforcement.
 *
 * KEY AGENTS:
 *   - - citizenry_as_political_community: Primary beneficiary (organized/constrained) — holds the protected citizen-soldier capacity as a body politic
 *   - - individual_arms_holders: Secondary beneficiary (moderate/mobile) — ordinary lawful possessors whose keeping and bearing sustains the wider capacity
 *   - - militia_tradition_adherents: Secondary beneficiary (organized/identity_locked) — organized militia and heritage-marksmanship participants fused with the citizen-soldier role
 *   - - federal_judiciary: Agenda setter (institutional/constrained) — administers the arrangement by fixing the text's operative meaning
 *   - - state_governments: Dual-position beneficiary (institutional/constrained) — operates the surviving militia institutions and accepts regulatory limits as part of the federal design
 *   - - gun_policy_reform_advocates: Excluded voice (organized/mobile) — politically potent but structurally voiceless inside the originalist-civic evidentiary frame
 *   - - constitutional_historians: Analytical observer (analytical/analytical) — attests the founding arrangement's actual shape, including its exclusions and decay
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__originalist_civic_virtue_reading, 0.12).
domain_priors:suppression_score(second_amendment_text__originalist_civic_virtue_reading, 0.58).
domain_priors:theater_ratio(second_amendment_text__originalist_civic_virtue_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__originalist_civic_virtue_reading, rope).
narrative_ontology:human_readable(second_amendment_text__originalist_civic_virtue_reading, "Second Amendment — Originalist Civic Virtue Reading (Citizen-Soldier Capacity)").
narrative_ontology:topic_domain(second_amendment_text__originalist_civic_virtue_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_text__originalist_civic_virtue_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__originalist_civic_virtue_reading, '02c507a5-7e02-4abc-9936-bd1d6ff105d4').
narrative_ontology:cs_kernel_codification('02c507a5-7e02-4abc-9936-bd1d6ff105d4', fixed_text).
narrative_ontology:cs_authority_grounding('02c507a5-7e02-4abc-9936-bd1d6ff105d4', lineage).
narrative_ontology:cs_interpretation_layer_present('02c507a5-7e02-4abc-9936-bd1d6ff105d4').
narrative_ontology:cs_reading_relation('02c507a5-7e02-4abc-9936-bd1d6ff105d4', second_amendment_text__collective_security_reading, coexists_with).
narrative_ontology:cs_reading_relation('02c507a5-7e02-4abc-9936-bd1d6ff105d4', second_amendment_text__individual_right_reading, coexists_with).
narrative_ontology:cs_axiom('02c507a5-7e02-4abc-9936-bd1d6ff105d4', foundational, militia_denotes_universal_armed_citizenry).
narrative_ontology:cs_axiom_status(militia_denotes_universal_armed_citizenry, holdable).
narrative_ontology:cs_axiom_grounding('02c507a5-7e02-4abc-9936-bd1d6ff105d4', militia_denotes_universal_armed_citizenry, empirically_contingent).
narrative_ontology:cs_axiom('02c507a5-7e02-4abc-9936-bd1d6ff105d4', foundational, right_protects_civic_capacity_over_private_use).
narrative_ontology:cs_axiom_status(right_protects_civic_capacity_over_private_use, holdable).
narrative_ontology:cs_axiom_grounding('02c507a5-7e02-4abc-9936-bd1d6ff105d4', right_protects_civic_capacity_over_private_use, deontological).
narrative_ontology:cs_axiom('02c507a5-7e02-4abc-9936-bd1d6ff105d4', secondary, standing_armies_endanger_republican_liberty).
narrative_ontology:cs_axiom_status(standing_armies_endanger_republican_liberty, holdable).
narrative_ontology:cs_axiom_grounding('02c507a5-7e02-4abc-9936-bd1d6ff105d4', standing_armies_endanger_republican_liberty, empirically_contingent).
narrative_ontology:cs_reference_frame('02c507a5-7e02-4abc-9936-bd1d6ff105d4', founding_era_universal_militia_order).
narrative_ontology:cs_drift_state('02c507a5-7e02-4abc-9936-bd1d6ff105d4', contemporary_post_bruen_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('02c507a5-7e02-4abc-9936-bd1d6ff105d4', '').
narrative_ontology:cs_kernel_id(second_amendment_text__originalist_civic_virtue_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__originalist_civic_virtue_reading, citizenry_as_political_community).
narrative_ontology:constraint_beneficiary(second_amendment_text__originalist_civic_virtue_reading, individual_arms_holders).
narrative_ontology:constraint_beneficiary(second_amendment_text__originalist_civic_virtue_reading, militia_tradition_adherents).
narrative_ontology:constraint_beneficiary(second_amendment_text__originalist_civic_virtue_reading, state_governments).
narrative_ontology:constraint_vindicates(second_amendment_text__originalist_civic_virtue_reading, civic_republican_antistanding_army_principle).
narrative_ontology:constraint_vindicates(second_amendment_text__originalist_civic_virtue_reading, distributed_force_check_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_text__originalist_civic_virtue_reading, citizen_soldier_civic_virtue_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The body politic as constituted by the founding settlement: a people who retain their own arms and training so that collective defense rests on mustered citizens rather than a professional standing force. What flows to this seat is the preserved capacity itself — lawful possession, the tradition of muster and marksmanship, and the constitutional bar on federal disarmament. The corresponding expectation is readiness: the capacity is held in trust for common defense, not merely for private ends. Exit is nominal — renouncing citizenship or emigrating abandons the seat entirely, and the seat is constituted by the very polity the arrangement presupposes.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, citizenry_as_political_community, beneficiary,
    organized, generational, constrained, national).

% Ordinary adults who lawfully possess firearms. Under this reading their possession matters insofar as it sustains the citizen-soldier capacity of the polity; the protected activity is the keeping and bearing that keeps the wider capacity real. Most hold no formal militia affiliation. They can sell, gift, or surrender arms, move between jurisdictions, or simply stop participating, which makes their attachment to the arrangement voluntary and revisable.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, individual_arms_holders, beneficiary,
    moderate, biographical, mobile, national).

% Members of organized volunteer militias, state defense forces, and heritage-marksmanship programs who treat the citizen-soldier role as a lived identity — drilling, mustering, and teaching the tradition across generations. The arrangement does not merely permit their activity; it names what they understand themselves to be. Leaving would mean resigning an identity, not just an activity, and participation persists across legal setbacks, organizational scandals, and long stretches of public indifference.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, militia_tradition_adherents, beneficiary,
    organized, biographical, identity_locked, regional).

% The federal courts, ultimately the Supreme Court, administer the arrangement by fixing the text's operative meaning case by case: which regulations survive, which historical analogues count, how far the protected capacity extends. They collect no revenue from the arrangement; their stake is doctrinal — precedent, institutional authority, and the interpretive method they have publicly committed to. Their freedom of maneuver is bounded by the text, prior precedent, and appointment politics.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% The states operate the militia tradition's surviving institutions — organized guards, state defense forces, marksmanship programs — and draw on citizen-soldier capacity in emergencies. The same arrangement bounds them: their police-power experimentation in firearms regulation is reviewable against the protected capacity. They accept the bound as part of the federal design while periodically testing its perimeter in litigation.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, state_governments, beneficiary,
    institutional, generational, constrained, national).

% Organizations and movements pressing for expanded firearms regulation. Within this reading's adjudicative frame they have no seat: the originalist-civic method weighs founding-era evidence, not contemporary harm statistics, so their core submissions fall outside what the administering institutions will count. They remain politically potent — ballot initiatives, legislation, amicus participation — but the frame itself gives their evidence no purchase.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, gun_policy_reform_advocates, excluded,
    organized, biographical, mobile, national).

% Academic historians of the founding era who study militia statutes, muster records, and revolutionary political thought. They attest to what the founding arrangement actually was — including its exclusions and its rapid institutional decay — without collecting anything from its operation. Their findings are cited by every faction and owned by none.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, constitutional_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_text__originalist_civic_virtue_reading, diffuse).
narrative_ontology:fixing_cost_class(second_amendment_text__originalist_civic_virtue_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the polity's defense around a universal armed citizenry: distributes military preparedness across the population so that collective security does not depend on a standing army, and maintains a structural dispersion of force as a feature of the constitutional design.
% TRANSFER_FUNCTION: Moves armed capacity and its attached obligations between the center and the populace: it withdraws from government the option of monopolizing force through disarmament, and it secures in distributed popular hands the readiness the defense design presupposes. Nothing monetary moves; what moves is capability-retention on one side and official forbearance on the other.
% ABSENT_VOICES: Communities that bear the costs of widespread armed capacity — urban residents, gun-violence survivors, communities living under heavy armed policing — have no seat in this reading's frame: the originalist-civic method admits founding-era materials, not contemporary harm data. Historically, the founding universal militia itself excluded the enslaved, most women, and often the propertyless; their descendants' objection that the universality premise was never true sits outside the frame's evidentiary gate. Both groups would object from outside the adjudicative conversation this reading licenses.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, firearms regulation would reorganize around police-power defaults with no constitutional floor; the statutory framework enrolling the unorganized militia would lose its anchor; the division of force-decisions between center and populace would drift toward fully professional institutions; and a substantial doctrinal and litigative apparatus would dissolve. The rearrangement is real but slower than a market shock — constitutional orders unwind over years, not weeks.
% FOUNDING_PROBLEM: The founding generation had fought a war against a professional army and inherited English suspicion of standing forces. Their solution was defense without standing armies: a citizenry that kept its own arms, mustered regularly, and could be called forth — with the new federal government barred from disarming the body it might someday face. The arrangement was built to keep military capacity distributed so that republican government would not rest on a monopoly of force.
% FOUNDING_PROBLEM_CORROBORATION: Founding-era historians corroborate the genealogy from outside any beneficiary seat: state militia acts, ratification-debate records, and the political literature of the 1780s uniformly attest the anti-standing-army problem and the militia remedy. Whether the problem remains live is attested differently by seat — civil-military affairs scholars note the professional force is now permanent and civilian-controlled, which transforms rather than settles the original worry; no seat outside the arrangement's beneficiaries attests that the founding problem persists in its original form.
narrative_ontology:disappearance_verdict(second_amendment_text__originalist_civic_virtue_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_text__originalist_civic_virtue_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__originalist_civic_virtue_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_text__originalist_civic_virtue_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_text__originalist_civic_virtue_reading, 0.12, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__originalist_civic_virtue_reading_tests).
:- end_tests(second_amendment_text__originalist_civic_virtue_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low (0.12 at interval end) because by this reading's own lights the guarantee takes nothing from anyone — it withdraws an option (disarmament) from government and secures capacity in distributed hands; the gentle rise across the series tracks the growing regulatory opportunity cost as enforcement strengthens, which the reading counts as the design working rather than rent. Suppression (0.58) is raw, unscaled enforcement intensity and follows a pronounced U-shape: high in the early republic when militia acts compelled enrollment, arming, and muster attendance; decaying to dormancy by the mid-twentieth century as compulsory musters lapsed and the guarantee went largely unenforced; then reviving sharply after 2008 as the courts rebuilt an active enforcement machinery that now strikes legislation against historical-analogue tests. Theater_ratio traces the function's atrophy and partial recovery: low when muster was a lived civic practice, climbing as musters were abolished state by state and the arrangement operated increasingly as heritage performance (peaking at 0.52 circa 1980, when the civic function was nearly all symbol), then easing to 0.45 as renewed doctrinal activity restored a functional share alongside the heritage residue. Accessibility_collapse is low-moderate (0.35) because the chief alternative — the professional standing force — did not collapse but won: the arrangement persists alongside, not instead of, the institution it was designed to make unnecessary. Resistance is high (0.70): the arrangement faces sustained political, legislative, and litigious contestation, which is exactly what a defended construct rather than a natural law shows. Identity-lock dynamics concentrate in militia_tradition_adherents: the fusion is ideological and institutional — self-concept constituted through the citizen-soldier role, reinforced by drill culture and heritage transmission — such that exit would mean resigning an identity, not an activity; if that frame broke, these seats would reclassify behaviorally as sport shooters and collectors, and the arrangement's most committed constituency would evaporate. The suppression mechanism is structural (judicial enforcement against legislative alternatives), not internalized; no participant is cognitively bound to compliance.
 *
 * PERSPECTIVAL GAP:
 *   There is no payer seat by design — the reading declares no victim set — so the operative divergence is not beneficiary-versus-target but administered-frame-versus-outside. From the federal_judiciary seat the arrangement is a doctrinal object it stewards under methodological commitments; from the militia_tradition_adherents seat it is constitutive of who they are, and its persistence is experienced as existential rather than instrumental; from the individual_arms_holders seat it is a background permission they could relinquish without cost; from the gun_policy_reform_advocates seat the injury is the frame itself — an evidentiary gate that admits founding-era materials and excludes the contemporary harm data their claims rest on. Same-nominal-level divergence also appears among the beneficiary seats: ordinary holders (mobile) and tradition adherents (identity_locked) hold the same protected capacity but radically different exit positions, which the engine computes from the exit atoms rather than from any authored label.
 *
 * DIRECTIONALITY LOGIC:
 *   All four declared beneficiary groups derive low directionality (near the subsidized end): the citizenry holds the capacity the guarantee secures; individual holders and tradition adherents exercise it; state governments draw on it for emergency defense while accepting its regulatory limits as designed federalism. The federal judiciary sits outside both arrays and takes the power-atom fallback near symmetric — apt for an administrator that collects no revenue and bears no extraction. Gun_policy_reform_advocates are structurally near the target end (the arrangement forecloses their policy aims) but receive no directionality override: overrides key on power atoms, and their atom (organized) is shared with beneficiary seats, so an override would mis-hit the citizenry and the adherents; the imprecision is routed instead to the kernel_reading_indexicality omega, where the sibling-reading comparison can price it properly. Suppression is authored as a raw structural property and is deliberately not tuned to any scaling — only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — securing a free state without a standing army by keeping military capacity distributed — has been transformed rather than solved or dissolved: the professional force the arrangement was built to obviate became permanent, and the militia as a mustering institution largely died. The arrangement persists anyway, now carrying a mix of live doctrinal function, heritage performance, and individual-capacity residue. The founding_problem_status is authored as contested rather than dead, and disappearance_verdict as world_rearranges, so no dead-plus-rearranges mismatch is asserted here; the liveness question that would drive a mandatrophy finding is carried openly by the civic_function_liveness omega instead of being resolved by fiat. Mandatrophy_resolved is therefore not declared. The theater_ratio series documents the drift honestly — including the circa-1980 crossing above 0.5 — while the end-state value sits just below the proxy-substitution line, reflecting the partial functional recovery under renewed judicial enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story is one reading (originalist_civic_virtue_reading) of the second_amendment_text kernel; how would the classification shift if the same referent were instantiated under a sibling reading?',
    'Compile the sibling stories (collective_security_reading, individual_right_reading) and compare per-seat classifications, beneficiary sets, and epsilon over the identical referent.',
    'The collective_security_reading would seat state governments as regulating administrators and likely raise measured extraction; the individual_right_reading would relocate the beneficiary to individual holders, detach the protection from civic function, and change the coordination type. The family comparison, not this file alone, is the measurement the kernel contest demands.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame routing: reading-indexed classification over a shared constitutional kernel.').

omega_variable(
    universality_of_founding_militia,
    'Was the founding-era militia actually universal, or did state militia acts exclude the enslaved, most women, and often the propertyless — narrowing the citizenry-qua-political-community beneficiary this reading declares?',
    'Archival reconstruction of state militia statutes and muster rolls, 1776-1815: who was enrolled, armed, fined for non-attendance, or exempted.',
    'If enrollment was substantially exclusionary, the beneficiary set narrows to a fraction of the polity, the coordination story acquires an excluded-class dimension, and effective extraction rises for excluded-lineage seats — potentially pulling the arrangement away from a pure coordination profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universality_of_founding_militia, empirical, 'Universality premise underlying the reading''s beneficiary declaration.').

omega_variable(
    civic_function_liveness,
    'Is the citizen-soldier function this reading protects live, or vestigial — maintained by heritage performance while actual defense rests on professional forces?',
    'Compare functional indicators (state defense force activations, organized training throughput, reserve integration) against symbolic activity (commemorative musters, heritage events) across the interval.',
    'If the function is dead, the arrangement drifts toward inertial maintenance — theater_ratio sustained above 0.5, the rope claim fails, and the reading survives chiefly as justification for the individual-capacity residue other readings protect directly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civic_function_liveness, empirical, 'Liveness of the civic function the reading claims to protect.').

omega_variable(
    sibling_relation_topology,
    'Are any sibling readings of the kernel logically exclusive within a single framework (foreclosure), or do all three genuinely coexist as live positions held by different parties?',
    'Doctrinal analysis: determine whether any adjudicative framework has treated the civic-capacity premise and the independence premise, or the regulatory-primacy premise, as jointly untenable; survey whether single jurists or scholars hold positions spanning readings.',
    'A forecloses edge would restructure the kernel''s contest topology and route engine-computed foreclosure between axioms; pure coexistence leaves the contest to indefinite political and interpretive competition with no logical resolution available.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_relation_topology, conceptual, 'Assessment that sibling relations are coexistence rather than foreclosure, flagged for verification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__originalist_civic_virtue_reading, 1791, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1791, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 1791, 0.15).
narrative_ontology:measurement_basis(seco_tr_t1791, observed).
narrative_ontology:measurement(seco_tr_t1820, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 1820, 0.22).
narrative_ontology:measurement_basis(seco_tr_t1820, observed).
narrative_ontology:measurement(seco_tr_t1860, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 1860, 0.3).
narrative_ontology:measurement_basis(seco_tr_t1860, observed).
narrative_ontology:measurement(seco_tr_t1900, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 1900, 0.42).
narrative_ontology:measurement_basis(seco_tr_t1900, observed).
narrative_ontology:measurement(seco_tr_t1940, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 1940, 0.48).
narrative_ontology:measurement_basis(seco_tr_t1940, observed).
narrative_ontology:measurement(seco_tr_t1980, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 1980, 0.52).
narrative_ontology:measurement_basis(seco_tr_t1980, observed).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 2008, 0.5).
narrative_ontology:measurement_basis(seco_tr_t2008, observed).
narrative_ontology:measurement(seco_tr_t2026, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 2026, 0.45).
narrative_ontology:measurement_basis(seco_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 1791, 0.06).
narrative_ontology:measurement_basis(seco_be_t1791, observed).
narrative_ontology:measurement(seco_be_t1820, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 1820, 0.07).
narrative_ontology:measurement_basis(seco_be_t1820, observed).
narrative_ontology:measurement(seco_be_t1860, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 1860, 0.08).
narrative_ontology:measurement_basis(seco_be_t1860, observed).
narrative_ontology:measurement(seco_be_t1900, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 1900, 0.09).
narrative_ontology:measurement_basis(seco_be_t1900, observed).
narrative_ontology:measurement(seco_be_t1940, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 1940, 0.1).
narrative_ontology:measurement_basis(seco_be_t1940, observed).
narrative_ontology:measurement(seco_be_t1980, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 1980, 0.1).
narrative_ontology:measurement_basis(seco_be_t1980, observed).
narrative_ontology:measurement(seco_be_t2008, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 2008, 0.11).
narrative_ontology:measurement_basis(seco_be_t2008, observed).
narrative_ontology:measurement(seco_be_t2026, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 2026, 0.12).
narrative_ontology:measurement_basis(seco_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1791, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 1791, 0.5).
narrative_ontology:measurement_basis(seco_su_t1791, observed).
narrative_ontology:measurement(seco_su_t1820, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 1820, 0.48).
narrative_ontology:measurement_basis(seco_su_t1820, observed).
narrative_ontology:measurement(seco_su_t1860, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 1860, 0.4).
narrative_ontology:measurement_basis(seco_su_t1860, observed).
narrative_ontology:measurement(seco_su_t1900, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 1900, 0.22).
narrative_ontology:measurement_basis(seco_su_t1900, observed).
narrative_ontology:measurement(seco_su_t1940, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 1940, 0.15).
narrative_ontology:measurement_basis(seco_su_t1940, observed).
narrative_ontology:measurement(seco_su_t1980, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 1980, 0.18).
narrative_ontology:measurement_basis(seco_su_t1980, observed).
narrative_ontology:measurement(seco_su_t2008, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 2008, 0.42).
narrative_ontology:measurement_basis(seco_su_t2008, observed).
narrative_ontology:measurement(seco_su_t2026, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 2026, 0.58).
narrative_ontology:measurement_basis(seco_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__originalist_civic_virtue_reading, resource_allocation).
narrative_ontology:affects_constraint(second_amendment_text__originalist_civic_virtue_reading, second_amendment_text__collective_security_reading).
narrative_ontology:affects_constraint(second_amendment_text__originalist_civic_virtue_reading, second_amendment_text__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_text__originalist_civic_virtue_reading, article_i_militia_clauses).

% DUAL FORMULATION NOTE:
% Colloquial references to the Second Amendment conflate three structurally distinct constraints — the readings of the second_amendment_text kernel. Each reading receives its own epsilon, beneficiary structure, and classification; this file instantiates the originalist_civic_virtue_reading (beneficiary: citizenry qua political community; no victim set; protection tied to the civic republican citizen-soldier function). The sibling files are linked via affects_constraints per the constraint-family rule. Article I militia clauses are included as a structural neighbor: Congress's powers to organize, arm, and discipline the militia operate on the same referent this reading protects, and the allocation between federal organization and popular capacity is part of this arrangement's operating logic.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
