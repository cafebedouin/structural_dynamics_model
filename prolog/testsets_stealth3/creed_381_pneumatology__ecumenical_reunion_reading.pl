% ============================================================================
% CONSTRAINT STORY: creed_381_pneumatology__ecumenical_reunion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_creed_381_pneumatology__ecumenical_reunion_reading, []).

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
 *   constraint_id: creed_381_pneumatology__ecumenical_reunion_reading
 *   human_readable: Bilateral Pneumatological Recognition Framework (Ecumenical Reunion Reading)
 *   domain: historical theology / ecclesiastical authority
 *
 * SUMMARY:
 *   This story instantiates one reading of the 381 creed's pneumatology
 *   clause: an arrangement in which both the Filioque and the mono-procession
 *   confession stand as legitimate regional expressions within a single
 *   communion, with recognition flowing bilaterally rather than by unilateral
 *   imposition. The framework is deliberately provisional - its justification
 *   is the transition toward resolved communion, not a steady state - and it
 *   enforces nothing: it proposes, and waits for both sides to receive. KEY
 *   AGENTS (by structural relationship): joint_bilateral_commissions:
 *   agenda-setter (institutional/constrained) - administers the framework,
 *   proposes but cannot impose; ecumenical_theologians: primary beneficiary
 *   (organized/mobile) - the dialogue apparatus collects standing and
 *   resources; eastern_orthodox_churches and roman_catholic_church: principal
 *   beneficiaries (institutional/constrained) - restored communion traded
 *   against surrendered exclusivity claims;
 *   confessional_hardliners_both_traditions: excluded opposition
 *   (organized/identity_locked) - the framework suspends their preferred
 *   outcome by design; lay_faithful_unreceived: excluded constituency
 *   (powerless/trapped) - reception never secured;
 *   academic_historians_of_doctrine: analytical observer - sees the full
 *   structure, adjudicates nothing.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(creed_381_pneumatology__ecumenical_reunion_reading, 0.3).
domain_priors:suppression_score(creed_381_pneumatology__ecumenical_reunion_reading, 0.18).
domain_priors:theater_ratio(creed_381_pneumatology__ecumenical_reunion_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(creed_381_pneumatology__ecumenical_reunion_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(creed_381_pneumatology__ecumenical_reunion_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(creed_381_pneumatology__ecumenical_reunion_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(creed_381_pneumatology__ecumenical_reunion_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(creed_381_pneumatology__ecumenical_reunion_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(creed_381_pneumatology__ecumenical_reunion_reading, scaffold).
narrative_ontology:human_readable(creed_381_pneumatology__ecumenical_reunion_reading, "Bilateral Pneumatological Recognition Framework (Ecumenical Reunion Reading)").
narrative_ontology:topic_domain(creed_381_pneumatology__ecumenical_reunion_reading, "historical theology / ecclesiastical authority").

narrative_ontology:has_sunset_clause(creed_381_pneumatology__ecumenical_reunion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(creed_381_pneumatology__ecumenical_reunion_reading, '3055d7f1-a971-4f27-bd95-5dba2a83504c').
narrative_ontology:cs_kernel_codification('3055d7f1-a971-4f27-bd95-5dba2a83504c', fixed_text).
narrative_ontology:cs_authority_grounding('3055d7f1-a971-4f27-bd95-5dba2a83504c', distributed).
narrative_ontology:cs_reading_relation('3055d7f1-a971-4f27-bd95-5dba2a83504c', creed_381_pneumatology__filioque_reading, coexists_with).
narrative_ontology:cs_reading_relation('3055d7f1-a971-4f27-bd95-5dba2a83504c', creed_381_pneumatology__monoprocession_reading, coexists_with).
narrative_ontology:cs_axiom('3055d7f1-a971-4f27-bd95-5dba2a83504c', foundational, bilateral_recognition_supersedes_unilateral_imposition).
narrative_ontology:cs_axiom_status(bilateral_recognition_supersedes_unilateral_imposition, holdable).
narrative_ontology:cs_axiom_grounding('3055d7f1-a971-4f27-bd95-5dba2a83504c', bilateral_recognition_supersedes_unilateral_imposition, conventional).
narrative_ontology:cs_axiom('3055d7f1-a971-4f27-bd95-5dba2a83504c', foundational, procession_formulas_admit_complementary_legitimacy).
narrative_ontology:cs_axiom_status(procession_formulas_admit_complementary_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('3055d7f1-a971-4f27-bd95-5dba2a83504c', procession_formulas_admit_complementary_legitimacy, theological).
narrative_ontology:cs_reference_frame('3055d7f1-a971-4f27-bd95-5dba2a83504c', bilateral_recognition_framework).
narrative_ontology:cs_drift_state('3055d7f1-a971-4f27-bd95-5dba2a83504c', contemporary_reception_gap, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3055d7f1-a971-4f27-bd95-5dba2a83504c', '').
narrative_ontology:cs_kernel_id(creed_381_pneumatology__ecumenical_reunion_reading, creed_381_pneumatology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(creed_381_pneumatology__ecumenical_reunion_reading, eastern_orthodox_churches).
narrative_ontology:constraint_beneficiary(creed_381_pneumatology__ecumenical_reunion_reading, roman_catholic_church).
narrative_ontology:constraint_beneficiary(creed_381_pneumatology__ecumenical_reunion_reading, ecumenical_theologians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mixed panels of bishops and theologians delegated by both communions. They draft agreed statements on the procession question, convene consultations, and propose mutual-recognition steps to their parent authorities. They can recommend but not enact; their texts bind no one until received by both sides. Their mandate is periodically renewed, which keeps the work provisional by design.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__ecumenical_reunion_reading, joint_bilateral_commissions, agenda_setter,
    institutional, generational, constrained, global).

% Professionals whose vocation, employment, and publication record center on the dialogue itself. Agreed statements, consultations, and joint appointments flow through this class, and their standing rises with each round of talks. Exit into ordinary academic or pastoral posts remains open, though it forfeits the dialogue's distinctive platform.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__ecumenical_reunion_reading, ecumenical_theologians, beneficiary,
    organized, biographical, mobile, global).

% Communion of self-governing churches whose liturgical confession preserves the creed's original procession clause. Under the arrangement they would recognize the Western formulation as a legitimate regional expression rather than a corruption to be removed, gaining restored communion at the price of forgoing the demand for textual restoration. Remaining outside the arrangement - the pre-dialogue status quo - remains available and is periodically urged on them by their own hardliners.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__ecumenical_reunion_reading, eastern_orthodox_churches, beneficiary,
    institutional, civilizational, constrained, global).

% Communion whose liturgical confession includes the added clause and whose teaching office has historically treated it as clarified doctrine. Under the arrangement it would stop presenting the addition as universally normative and receive the Eastern formulation as equally legitimate, gaining restored communion at the price of narrowing a claim it has asserted for a millennium. Its alternative is continuing to assert the universal claim and accepting continued separation.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__ecumenical_reunion_reading, roman_catholic_church, beneficiary,
    institutional, civilizational, constrained, global).

% Constituencies on each side for whom the single formulation is inseparable from the faith itself: some hold the added clause as dogmatically settled and non-negotiable, others hold the untouched text as inviolable. The framework offers them neither victory nor exit - their preferred outcome, universal acceptance of one formula, is precisely what the arrangement suspends. Their objections circulate inside both communions but cannot be accommodated without collapsing the framework.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__ecumenical_reunion_reading, confessional_hardliners_both_traditions, excluded,
    organized, generational, identity_locked, global).

% Ordinary members of both communions, who pray, marry, and mourn across the existing division. The recognition texts are drafted and signed far above them; their assent is presumed rather than solicited, and few encounter the documents at all. If the arrangement ever reaches their parishes it will arrive as a settled decision from hierarchies they did not choose.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__ecumenical_reunion_reading, lay_faithful_unreceived, excluded,
    powerless, biographical, trapped, global).

% Scholars in universities and seminaries, mostly outside the two communions' authority structures, who document when and how the clause entered Western usage and what the council of 381 actually transmitted. Both sides cite their work; they adjudicate nothing and collect nothing.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__ecumenical_reunion_reading, academic_historians_of_doctrine, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(creed_381_pneumatology__ecumenical_reunion_reading, ecumenical_theologians).
narrative_ontology:fixing_cost_class(creed_381_pneumatology__ecumenical_reunion_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Holds a single communion together across regions confessing different pneumatological formulas, so that doctrinal diversity over the Spirit's procession does not fracture eucharistic unity while the underlying question remains under joint study.
% TRANSFER_FUNCTION: Moves recognition, not goods: each communion grants legitimacy to the other's formulation, and each surrenders its claim to universal normativity. Authority over doctrinal definition shifts from unilateral act to bilateral consensus procedure.
% ABSENT_VOICES: Confessional hardliners on both sides would object that the framework relativizes dogma or rewards an unauthorized amendment; their voice circulates but the framework structurally cannot grant their demand. The lay faithful of both communions have never been solicited for assent; reception is presumed, not given.
% DISAPPEARANCE_RATIONALE: If the bilateral-recognition framework vanished overnight, both communions would revert to competing exclusive claims - the West reasserting the universal normativity of its formulation, the East renewing the demand for textual restoration - and the machinery of rapprochement would collapse back into the pre-dialogue standoff, with each side's liturgical confession again functioning as a barrier to communion.
% FOUNDING_PROBLEM: A creed amended unilaterally in one region became, over centuries, a formal barrier between two communions: the West confessed a clause the East regarded as an unauthorized interpolation, and neither side could enter full communion without either accepting the other's text or abandoning its own.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: academic ecclesiastical historians (largely unconfessional) document the insertion chronology and the persistence of the division, and third-party church bodies - Oriental Orthodox, Anglican, old-calendarist Orthodox - treat the East-West rupture over the clause as a live fact rather than a solved problem. No party outside the dialogue apparatus attests that the founding problem is dead.
narrative_ontology:disappearance_verdict(creed_381_pneumatology__ecumenical_reunion_reading, world_rearranges).
narrative_ontology:founding_problem_status(creed_381_pneumatology__ecumenical_reunion_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(creed_381_pneumatology__ecumenical_reunion_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(creed_381_pneumatology__ecumenical_reunion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(creed_381_pneumatology__ecumenical_reunion_reading, 0.3, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(creed_381_pneumatology__ecumenical_reunion_reading_tests).
:- end_tests(creed_381_pneumatology__ecumenical_reunion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored at 0.30 (low-moderate): the arrangement takes little from anyone - its costs are restraint costs (each communion forgoes total victory) plus the resources absorbed by the professional dialogue class - and no seat bears extraction as such. Suppression is low (0.18): there is no coercive enforcement, only marginal social pressure on internal dissenters. Theater_ratio (0.38) is the most consequential metric: the temporal series shows statement-production steadily detaching from reception - documents signed, ratified nowhere, unknown to the faithful - a Goodhart drift in which the proxy (agreed text) substitutes for the function (restored communion). Accessibility_collapse (0.42) is moderate: the alternatives - pre-dialogue standoff, renewed unilateral assertion - remain live and are periodically urged by hardliners, so understanding the framework does not close the option space. Resistance (0.55) is substantial and comes from inside both communions. The measurement series run on one shared six-point grid; both trajectories are monotonic, so no cyclical machinery is invoked. Claim and metrics are independent facts: the scaffold claim records what the arrangement is structured to be; the metrics record what it is actually doing.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the commission seat the framework is a fragile achievement requiring patience and renewal; from the two church seats it is an affordable concession priced against centuries of division; from the hardliner seats it is betrayal or relativization - the suspension of the very thing they hold to be the faith; from the lay seats it is invisible. The engine derives these divergent per-seat classifications from the power and exit data (institutional/constrained setters, mobile professional beneficiaries, identity_locked excluded opponents, trapped laity); the authored claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   All declared beneficiaries sit near the beneficiary end of directionality: the churches gain communion prospects at modest cost, and the theologian class gains vocationally. No victims are declared because, under this reading's own lights, no seat bears extraction through the structure - the consensus model is the reading's substantive claim, and the structural data reflect it. The grievance-bearing seats (hardliners, unreceived laity) are authored as excluded, not as payers: they bear foregone outcomes and unsecured reception, not a transfer running through the framework. Effective extraction is therefore damped across nearly every seat, with the modest residue concentrated where restraint costs and professional capture actually land; global scope raises verification difficulty slightly, which the engine scales.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold classification matters because it blocks two symmetrical misreadings. Read as a rope, the framework looks like a permanent coordination good and its non-reception becomes mere friction; read as a snare, its modest costs look like extraction and the dialogue becomes a con. The truth is transitional: the arrangement is justified by the division it exists to end, and its health is measured by whether the sunset fires. The founding problem is still live (corroborated externally), so no mandatrophy declaration is authored and the R5 mismatch consumer finds status=live paired with world_rearranges - no zombie flag. The danger trajectory is visible in the data nonetheless: rising theater_ratio with an unspecified sunset condition (see omega sunset_condition_specification) is the classic scaffold-to-piton path, in which the dialogue outlives its question and maintains itself performatively.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the kernel creed_381_pneumatology (reading: ecumenical_reunion_reading). What structural changes would adoption of a sibling reading produce?',
    'Institutional adoption of the filioque_reading would restore magisterial clarification authority and dissolve regional pluralism into a single-formulation regime; adoption of the monoprocession_reading would make textual restoration a precondition of communion. Either event retires this story''s referent and activates the sibling files'' classifications.',
    'This story''s epsilon, beneficiary set, and type are valid only under this reading. Under either sibling, the consensus model disappears, a new victim set appears (whichever communion must yield its formulation), and the classification recomputes from the sibling story, not from this one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: this story is one of three readings of the 381 pneumatology kernel; sibling adoptions replace the arrangement this story classifies.').

omega_variable(
    complementarity_vs_contradiction,
    'Are the two procession formulas genuinely complementary expressions of one doctrine, or materially contradictory accounts of the Spirit''s origin?',
    'Sustained bilateral theological convergence issuing in a joint doctrinal declaration, or a future ecumenical council competent to rule on the question.',
    'If the formulas are materially contradictory, the arrangement coordinates a concealed contradiction: epsilon rises sharply, the consensus model functions as cover, and the scaffold reads as an extraction-bearing hybrid. If genuinely complementary, the low-moderate epsilon stands and the framework is what it claims to be.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complementarity_vs_contradiction, conceptual, 'Whether the pluralism the framework permits is real complementarity or managed contradiction.').

omega_variable(
    reception_gap_durability,
    'Will the recognition texts achieve reception by synods, clergy, and laity, or remain elite-level signatures?',
    'Track ratification acts, liturgical-text revisions, and catechetical uptake across both communions over successive quinquennia.',
    'Genuine reception fires the sunset condition and lets the scaffold dissolve into ordinary communion. Durable non-reception drives theater_ratio past 0.5 and drifts the arrangement toward inertial self-perpetuation regardless of doctrinal progress.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reception_gap_durability, empirical, 'Whether the framework''s decisions reach the bodies they govern or accumulate as unsigned paper.').

omega_variable(
    sunset_condition_specification,
    'What event constitutes this arrangement''s sunset - full sacramental communion, definitive doctrinal convergence, or something else - and which authority is competent to declare it reached?',
    'An explicit joint declaration naming the triggering condition and the declaring authority; absent that, the question stays open by construction.',
    'Without a specified trigger, ''provisional'' becomes permanent: the scaffold''s transitional justification erodes and the arrangement persists by inertia even after its founding problem is solved, completing the drift from scaffold toward piton.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sunset_condition_specification, conceptual, 'The sunset clause exists in intent but is unspecified in content; the gap is observable now.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(creed_381_pneumatology__ecumenical_reunion_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cree_tr_t0, creed_381_pneumatology__ecumenical_reunion_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cree_tr_t6, creed_381_pneumatology__ecumenical_reunion_reading, theater_ratio, 6, 0.19).
narrative_ontology:measurement(cree_tr_t12, creed_381_pneumatology__ecumenical_reunion_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(cree_tr_t18, creed_381_pneumatology__ecumenical_reunion_reading, theater_ratio, 18, 0.28).
narrative_ontology:measurement(cree_tr_t24, creed_381_pneumatology__ecumenical_reunion_reading, theater_ratio, 24, 0.33).
narrative_ontology:measurement(cree_tr_t30, creed_381_pneumatology__ecumenical_reunion_reading, theater_ratio, 30, 0.38).

% Extraction over time
narrative_ontology:measurement(cree_be_t0, creed_381_pneumatology__ecumenical_reunion_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(cree_be_t6, creed_381_pneumatology__ecumenical_reunion_reading, base_extractiveness, 6, 0.24).
narrative_ontology:measurement(cree_be_t12, creed_381_pneumatology__ecumenical_reunion_reading, base_extractiveness, 12, 0.26).
narrative_ontology:measurement(cree_be_t18, creed_381_pneumatology__ecumenical_reunion_reading, base_extractiveness, 18, 0.27).
narrative_ontology:measurement(cree_be_t24, creed_381_pneumatology__ecumenical_reunion_reading, base_extractiveness, 24, 0.29).
narrative_ontology:measurement(cree_be_t30, creed_381_pneumatology__ecumenical_reunion_reading, base_extractiveness, 30, 0.3).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(creed_381_pneumatology__ecumenical_reunion_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(creed_381_pneumatology__ecumenical_reunion_reading, identity_coordination).
narrative_ontology:affects_constraint(creed_381_pneumatology__ecumenical_reunion_reading, creed_381_pneumatology__filioque_reading).
narrative_ontology:affects_constraint(creed_381_pneumatology__ecumenical_reunion_reading, creed_381_pneumatology__monoprocession_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the Filioque question' decomposes into three structurally distinct constraints, one per reading of the kernel creed_381_pneumatology, per the epsilon-invariance principle. This file authors epsilon approximately 0.30 for the bilateral-recognition arrangement (consensus model, no victim set, transitional justification). The sibling files author epsilon for their own referents: the filioque_reading file for the magisterial-clarification regime (extractive toward Eastern partners), the monoprocession_reading file for the inviolability regime (extractive toward Western liturgical practice). Each story carries one stable epsilon over its own standing arrangement; the links here route contamination analysis across the family, since adoption of any one reading dissolves the other two's referents.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
