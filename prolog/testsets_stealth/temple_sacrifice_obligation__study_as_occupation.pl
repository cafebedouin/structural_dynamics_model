% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_obligation__study_as_occupation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_obligation__study_as_occupation, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: temple_sacrifice_obligation__study_as_occupation
 *   human_readable: Study-as-Occupation of the Sacrificial Obligation (Post-Temple Halakhic Reading)
 *   domain: religious/legal/commitment-system
 *
 * SUMMARY:
 *   After the Temple's destruction, a covenant whose central duties were
 *   sacrificial performances faced their standing impossibility. The
 *   study_as_occupation reading answers that study of the sacrificial laws
 *   constitutes legitimate occupation of the obligation during absence: the
 *   duty stays live, and its executable form is textual engagement rather
 *   than altar service. This file authors that reading only, as a clean
 *   epsilon-invariant constraint; the sibling readings (study_as_archiving,
 *   messianic_suspension) are separate constraints with their own epsilon,
 *   beneficiary structure, and classification. The epsilon referent is the
 *   standing arrangement under contest — study-as-occupation as actually
 *   practiced across the diaspora — assessed by this reading's own lights,
 *   never the restored-performance condition it anticipates. KEY AGENTS (by
 *   structural relationship): - rabbinic_leadership: agenda-setter
 *   (institutional / identity_locked) — administers the substitution, sets
 *   curriculum and rulings - torah_scholars: performer-beneficiary (organized
 *   / identity_locked) — performs the study that constitutes the occupation;
 *   pays the labor - observant_lay_communities: beneficiary-payer (moderate /
 *   constrained) — funds and liturgically participates; receives
 *   discharge-assurance - hereditary_priestly_families: latent-functionary
 *   beneficiary (moderate / constrained) — office suspended, absorbed into
 *   the study track - temple_restoration_activists: excluded challenger
 *   (moderate / constrained) — contests substitution's passivity from outside
 *   deliberative forums - academic_historians_of_judaism: analytical observer
 *   (analytical / analytical) — sees the full two-millennium arc
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__study_as_occupation, 0.15).
domain_priors:suppression_score(temple_sacrifice_obligation__study_as_occupation, 0.08).
domain_priors:theater_ratio(temple_sacrifice_obligation__study_as_occupation, 0.14).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, extractiveness, 0.15).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, theater_ratio, 0.14).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, accessibility_collapse, 0.28).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__study_as_occupation, rope).
narrative_ontology:human_readable(temple_sacrifice_obligation__study_as_occupation, "Study-as-Occupation of the Sacrificial Obligation (Post-Temple Halakhic Reading)").
narrative_ontology:topic_domain(temple_sacrifice_obligation__study_as_occupation, "religious/legal/commitment-system").

narrative_ontology:has_sunset_clause(temple_sacrifice_obligation__study_as_occupation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__study_as_occupation, 'ff4103a4-585b-4a49-b282-ebc2ffbcbed1').
narrative_ontology:cs_kernel_codification('ff4103a4-585b-4a49-b282-ebc2ffbcbed1', fixed_text).
narrative_ontology:cs_authority_grounding('ff4103a4-585b-4a49-b282-ebc2ffbcbed1', lineage).
narrative_ontology:cs_interpretation_layer_present('ff4103a4-585b-4a49-b282-ebc2ffbcbed1').
narrative_ontology:cs_reading_relation('ff4103a4-585b-4a49-b282-ebc2ffbcbed1', temple_sacrifice_obligation__study_as_archiving, coexists_with).
narrative_ontology:cs_reading_relation('ff4103a4-585b-4a49-b282-ebc2ffbcbed1', temple_sacrifice_obligation__messianic_suspension, forecloses).
narrative_ontology:cs_axiom('ff4103a4-585b-4a49-b282-ebc2ffbcbed1', foundational, study_discharges_sacrificial_obligation).
narrative_ontology:cs_axiom_status(study_discharges_sacrificial_obligation, holdable).
narrative_ontology:cs_axiom_grounding('ff4103a4-585b-4a49-b282-ebc2ffbcbed1', study_discharges_sacrificial_obligation, theological).
narrative_ontology:cs_axiom('ff4103a4-585b-4a49-b282-ebc2ffbcbed1', secondary, obligation_persists_despite_impossibility).
narrative_ontology:cs_axiom_status(obligation_persists_despite_impossibility, holdable).
narrative_ontology:cs_axiom_grounding('ff4103a4-585b-4a49-b282-ebc2ffbcbed1', obligation_persists_despite_impossibility, deontological).
narrative_ontology:cs_reference_frame('ff4103a4-585b-4a49-b282-ebc2ffbcbed1', live_obligation_occupied_by_study).
narrative_ontology:cs_drift_state('ff4103a4-585b-4a49-b282-ebc2ffbcbed1', contemporary_post_emancipation, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ff4103a4-585b-4a49-b282-ebc2ffbcbed1', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__study_as_occupation, temple_sacrifice_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_occupation, rabbinic_leadership).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_occupation, torah_scholars).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_occupation, observant_lay_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_occupation, hereditary_priestly_families).
narrative_ontology:constraint_victim(temple_sacrifice_obligation__study_as_occupation, torah_scholars).
narrative_ontology:constraint_victim(temple_sacrifice_obligation__study_as_occupation, observant_lay_communities).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_occupation, study_substitution_principle).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_occupation, obligation_impossibility_non_lapse).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_occupation, halakhic_adaptability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Poskim, roshei yeshiva, and the transmitters of the masorah who administer the substitution: they fix the curriculum of sacrificial-law study, rule on whether liturgical recitation of the sacrificial order counts toward engagement, and answer new questions of intent and sufficiency. Their authority is exercised through the interpretive chain itself; stepping outside it would mean repudiating the transmission they embody, so departure is not a live option from where they stand. They carry custodial burden — defending coherence, absorbing challenge — and the recognition they receive circulates back into the function.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, rabbinic_leadership, agenda_setter,
    institutional, generational, identity_locked, global).

% Students and scholars in yeshiva and kollel who perform the daily study that constitutes the occupation. Many spend years on the orders of Kodshim; they receive discharge-assurance, communal support, and a vocation, and they pay decades of labor and forgone alternatives. Leaving the study world would unravel livelihood, marriage prospects, and communal identity at once, so exit is nominal rather than real.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, torah_scholars, beneficiary,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_obligation__study_as_occupation, torah_scholars, payer).

% Households who fund the institutions and recite the korbanot passages in the daily liturgy. They receive the assurance that the covenant's duties remain dischargeable in their condition, and they pay in donations, tuition, and liturgical time. Leaving observance altogether is possible but carries family, social, and self-conception costs, so most remain inside on the arrangement's terms.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, observant_lay_communities, beneficiary,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_obligation__study_as_occupation, observant_lay_communities, payer).

% Descendants of the priestly line whose Temple service is impossible in the current condition. Inside the arrangement they take up the study track — priestly families are prominently represented among Kodshim specialists — and receive ordinary member benefits; their hereditary office itself remains latent, neither abolished nor exercisable.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, hereditary_priestly_families, beneficiary,
    moderate, generational, constrained, global).

% Groups that prepare vessels, garments, and priestly training for renewed service, and argue that substitution should sharpen rather than replace the drive toward restoration. They are marginal to mainstream halakhic deliberation — rarely seated in the forums where the substitution's terms are set — yet they are bound by the same covenantal framework they criticize and cannot exit it without leaving Orthodoxy.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, temple_restoration_activists, excluded,
    moderate, generational, constrained, regional).

% Scholars of ancient and medieval Judaism who trace how the substitution doctrine crystallized after 70 CE and how it traveled through geonic, rishonic, and acharonic literature. They take no part in discharge and bear none of its costs; they observe the arrangement's full arc from outside the covenantal framework.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, academic_historians_of_judaism, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_obligation__study_as_occupation, diffuse).
narrative_ontology:fixing_cost_class(temple_sacrifice_obligation__study_as_occupation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains continuous covenantal engagement with the sacrificial commandments for a community lacking its performance-site: gives the obligation a daily executable form (structured study, liturgical recitation of the sacrificial order), keeps the legal corpus and its transmission chain alive, and holds the full commandment-set together as a lived system rather than a partially lapsed one.
% TRANSFER_FUNCTION: Moves time, labor, and funding from lay households and students into the study institutions; moves interpretive authority-recognition toward the rabbinic-transmission chain; returns, within the framework's own accounting, discharge-assurance to the obligated.
% ABSENT_VOICES: Temple-restoration activists would object that substitution entrenches passivity toward restoration; they sit outside mainstream halakhic deliberation. Descendants of the obligated population who have exited religious observance entirely voice no objection and bear no costs — their absence makes the arrangement's unanimity look more complete than the population's.
% DISAPPEARANCE_RATIONALE: Overnight removal leaves the obligation standing with no executable form: communities would face a normative void (suspension-style dormancy) or an accumulating unmet deficit (archiving-style), yeshiva curricula and the korbanot liturgy would lose their organizing rationale, and the covenantal self-understanding of the obligated population would require wholesale renegotiation.
% FOUNDING_PROBLEM: After the Temple's destruction, a covenant whose central duties were sacrificial performances faced their standing impossibility: how do the obligated remain in faithful discharge when performance is unavailable?
% FOUNDING_PROBLEM_CORROBORATION: The problem's factual core — the Temple's continued absence — is attested independently of any beneficiary: the archaeological record of the mount, secular historiography of 70 CE, and the plain non-existence of the institution. Holders of the sibling readings attest the problem is live while disputing this reading's solution; no party claims performance is currently possible.
narrative_ontology:disappearance_verdict(temple_sacrifice_obligation__study_as_occupation, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_obligation__study_as_occupation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__study_as_occupation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(temple_sacrifice_obligation__study_as_occupation, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_obligation__study_as_occupation, 0.15, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_obligation__study_as_occupation_tests).
:- end_tests(temple_sacrifice_obligation__study_as_occupation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claimed type is rope: the arrangement solves a real collective-action problem — keeping a performance-impossible duty set executable and coherent — with participants as net beneficiaries and no identifiable victim set, matching the expected structural delta for this reading. Metrics are authored independently as descriptive facts: extractiveness 0.15 (study labor and funding flow inward, but the surplus above coordination cost is thin and recirculates as cultural capital); suppression 0.08 (adherence is voluntary in the contemporary enforcement vacuum; no machinery compels study); theater_ratio 0.14 (some liturgical recitation proceeds rote, but the core function — engaged study — is real); accessibility_collapse 0.28 (sibling readings remain live alternatives; understanding this arrangement does not close them off); resistance 0.18 (contestation takes the form of rival readings and activist marginality, not organized resistance to study itself). The temporal series runs on one shared seven-point grid spanning roughly 200 CE to the present, with every tracked metric authored at every point. suppression_requirement is included deliberately: this story's enforcement history is a genuine decay curve — corporate-communal discipline of the geonic and kehillah eras eroding through emancipation to near-zero voluntarism — and the arrangement's persistence under that decay is the analytic point. theater_ratio rises gently (rote liturgical recitation growing relative to deep study) but stays far below the substitution threshold. No cyclical dynamics: extraction is flat-low and suppression decays monotonically.
 *
 * PERSPECTIVAL GAP:
 *   The seats should classify differently from identical structure. From the leadership seat the arrangement is custodial stewardship of a live duty — administration, not enjoyment. From the scholar seat study and fulfillment are the same act; extraction is nearly invisible behind vocation. From the lay seat it is inherited continuity that costs donations and liturgical minutes and returns assurance. From the excluded activist seat the same structure reads as institutionalized quietism that converts restoration-pressure into curriculum. The engine computes these divergences from power, exit, and directional data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: rabbinic_leadership, torah_scholars, observant_lay_communities. No victim group is declared because none is identifiable — costs are diffuse participation costs borne by the same seats that receive the benefit. Left to raw derivation, all three beneficiary declarations would seat their holders near the full-beneficiary end, which misdescribes each: leadership absorbs custodial burden and enforcement-decay costs (override institutional -> 0.22); scholars supply the labor the arrangement runs on, decades of it (override organized -> 0.42, near-symmetric with a slight benefit tilt); lay communities fund the institutions (override moderate -> 0.30). The excluded activists inherit the moderate override but sit outside the arrangement's operative set; their exclusion is maintained by agenda control, not by the substitution itself.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy: the founding problem — standing impossibility of performance — is live, and the arrangement's function is precisely to occupy a condition that has not resolved. The mandatrophy risk for this family sits elsewhere: if restoration ever occurred and study-retention persisted as pure inertia, the arrangement would continue without function. That contingency is carried by the restoration_terminus_behavior omega, and theater_ratio is tracked temporally to catch recitation-without-study drift before it hardens.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint instantiates the study_as_occupation reading of the temple_sacrifice_obligation kernel; would the sibling readings (study_as_archiving, messianic_suspension) change the structural classification?',
    'Author the sibling stories and compare computed types across the family; locate whether the disagreement turns on discharge-status (conceptual) or on observable practice differences (empirical).',
    'If archiving prevailed, study would lose its discharge function and the arrangement would lose its coordination payoff, drifting toward dormancy; if suspension prevailed, the obligation''s present status flips from occupied to dormant and the beneficiary structure dissolves.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer-frame omega: classification is reading-indexed over a shared kernel.').

omega_variable(
    restoration_terminus_behavior,
    'If the Temple were restored, does the substitution sunset cleanly into resumed performance, or does study retain independent obligating force alongside sacrifice?',
    'The event itself plus subsequent halakhic rulings; pre-event, analysis of whether the tradition treats study-equivalence as a conditional expedient tied to absence or as a permanent value.',
    'A clean sunset confirms the conditional-expedient structure carried by has_sunset_clause; retained dual-track force would convert the arrangement into a permanent parallel obligation, changing its long-run classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restoration_terminus_behavior, conceptual, 'Behavior of the declared terminus upon its triggering condition.').

omega_variable(
    enforcement_decay_stability,
    'Does the arrangement''s persistence under near-zero enforcement confirm intrinsically low extraction, or would latent coercive capacity re-emerge if communal discipline strengthened?',
    'Compare sub-communities with stronger disciplinary structures against voluntarist ones: if adherence burdens diverge with enforcement intensity, part of the current low reading is enforcement-dependent rather than structural.',
    'If measured burdens rise with enforcement capacity, the low epsilon reflects enforcement decay rather than structural benignity, and classification would track enforcement cycles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_decay_stability, empirical, 'Whether low suppression is structural or an artifact of enforcement collapse.').

omega_variable(
    priestly_latency_cost,
    'Does occupying the obligation through study impose an unacknowledged diffuse cost on hereditary priestly families whose service-office remains permanently latent, constituting a hidden victim set?',
    'Examine whether priestly-lineage scholarship patterns show compensatory absorption (kohanim disproportionately leading sacrificial-law study) or persistent role-deprivation grievance; compare communities that emphasize restoration against those that do not.',
    'If a coherent deprivation pattern exists, the no-victim-set claim fails and the arrangement tilts toward a hybrid coordination-plus-deferral structure; if absorption is genuine, the pure-coordination reading stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(priestly_latency_cost, conceptual, 'Tests the no-victim-set claim against the latent functionary class.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__study_as_occupation, 0, 1800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tso_study_occ_tr_t0, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(tso_study_occ_tr_t0, observed).
narrative_ontology:measurement(tso_study_occ_tr_t300, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 300, 0.09).
narrative_ontology:measurement_basis(tso_study_occ_tr_t300, observed).
narrative_ontology:measurement(tso_study_occ_tr_t600, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 600, 0.11).
narrative_ontology:measurement_basis(tso_study_occ_tr_t600, observed).
narrative_ontology:measurement(tso_study_occ_tr_t900, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 900, 0.13).
narrative_ontology:measurement_basis(tso_study_occ_tr_t900, observed).
narrative_ontology:measurement(tso_study_occ_tr_t1200, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 1200, 0.15).
narrative_ontology:measurement_basis(tso_study_occ_tr_t1200, observed).
narrative_ontology:measurement(tso_study_occ_tr_t1500, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 1500, 0.17).
narrative_ontology:measurement_basis(tso_study_occ_tr_t1500, observed).
narrative_ontology:measurement(tso_study_occ_tr_t1800, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 1800, 0.18).
narrative_ontology:measurement_basis(tso_study_occ_tr_t1800, observed).

% Extraction over time
narrative_ontology:measurement(tso_study_occ_be_t0, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 0, 0.14).
narrative_ontology:measurement_basis(tso_study_occ_be_t0, observed).
narrative_ontology:measurement(tso_study_occ_be_t300, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 300, 0.13).
narrative_ontology:measurement_basis(tso_study_occ_be_t300, observed).
narrative_ontology:measurement(tso_study_occ_be_t600, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 600, 0.15).
narrative_ontology:measurement_basis(tso_study_occ_be_t600, observed).
narrative_ontology:measurement(tso_study_occ_be_t900, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 900, 0.17).
narrative_ontology:measurement_basis(tso_study_occ_be_t900, observed).
narrative_ontology:measurement(tso_study_occ_be_t1200, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 1200, 0.16).
narrative_ontology:measurement_basis(tso_study_occ_be_t1200, observed).
narrative_ontology:measurement(tso_study_occ_be_t1500, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 1500, 0.14).
narrative_ontology:measurement_basis(tso_study_occ_be_t1500, observed).
narrative_ontology:measurement(tso_study_occ_be_t1800, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 1800, 0.15).
narrative_ontology:measurement_basis(tso_study_occ_be_t1800, observed).

% Suppression requirement over time
narrative_ontology:measurement(tso_study_occ_su_t0, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(tso_study_occ_su_t0, observed).
narrative_ontology:measurement(tso_study_occ_su_t300, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 300, 0.48).
narrative_ontology:measurement_basis(tso_study_occ_su_t300, observed).
narrative_ontology:measurement(tso_study_occ_su_t600, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 600, 0.44).
narrative_ontology:measurement_basis(tso_study_occ_su_t600, observed).
narrative_ontology:measurement(tso_study_occ_su_t900, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 900, 0.38).
narrative_ontology:measurement_basis(tso_study_occ_su_t900, observed).
narrative_ontology:measurement(tso_study_occ_su_t1200, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 1200, 0.26).
narrative_ontology:measurement_basis(tso_study_occ_su_t1200, observed).
narrative_ontology:measurement(tso_study_occ_su_t1500, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 1500, 0.14).
narrative_ontology:measurement_basis(tso_study_occ_su_t1500, observed).
narrative_ontology:measurement(tso_study_occ_su_t1800, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 1800, 0.08).
narrative_ontology:measurement_basis(tso_study_occ_su_t1800, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_obligation__study_as_occupation, identity_coordination).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_occupation, temple_sacrifice_obligation__study_as_archiving).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_occupation, temple_sacrifice_obligation__messianic_suspension).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'what happens to sacrificial obligations without a Temple' decomposes into three structurally distinct readings of one kernel, each with its own epsilon and beneficiary structure. This story (study_as_occupation) is the low-extraction member: study discharges, so the arrangement has a live coordination payoff and no victim set. study_as_archiving splits study's function from discharge (preservation only), and messianic_suspension removes present discharge entirely. The upstream member (highest textual consolidation, this reading) influences the downstream members because both siblings must account for the same Talmudic warrants this reading cites; each file links the others via network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(temple_sacrifice_obligation__study_as_occupation, institutional, 0.22).
constraint_indexing:directionality_override(temple_sacrifice_obligation__study_as_occupation, organized, 0.42).
constraint_indexing:directionality_override(temple_sacrifice_obligation__study_as_occupation, moderate, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
