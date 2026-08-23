% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__boundary_maintenance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__boundary_maintenance_reading, []).

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
 *   constraint_id: catastrophe_memory_kernel__boundary_maintenance_reading
 *   human_readable: Shared Mourning Practice as Group-Boundary Enforcement
 *   domain: religious/collective_memory/social
 *
 * SUMMARY:
 *   A catastrophe-survivor community maintains an obligatory calendar of
 *   shared mourning: annual fasts, public lamentation recitals,
 *   house-to-house condolence rounds, and memory-education for children.
 *   Attendance is public and witnessed; households that miss observances
 *   receive visits; members who date outside the community are counseled,
 *   pressured, and if they proceed, progressively estranged. The
 *   boundary_maintenance_reading holds that this apparatus's operative
 *   function is marking and policing who belongs: the rites solve a real
 *   coordination problem (pooled grief-labor, scheduled memory transmission,
 *   mutual-aid visibility) while simultaneously extracting conformity from
 *   members and excluding outsiders from full relation. This story is ONE
 *   READING of the catastrophe_memory_kernel; the symbol_continuity,
 *   survival_competence, and trauma_encoding readings are separate
 *   constraints with their own epsilon, victim sets, and classifications,
 *   linked through network.affects_constraints. Claim and metrics are
 *   independent authored facts: the claimed type is what I believe
 *   structurally true of this reading's constraint; the metrics describe its
 *   observed operation without being tuned to any predicted engine verdict.
 *
 * KEY AGENTS:
 *   - communal_leadership: agenda_setter (institutional/identity_locked) — administers the mourning calendar, collects deference and authority
 *   - core_observant_families: primary beneficiary (organized/constrained) — collect status, marriage-network closure, and mutual aid
 *   - rank_and_file_members: dual-positioned beneficiary-payer (moderate/constrained) — receive belonging and aid, bear conformity costs
 *   - doubting_members: primary target (powerless/identity_locked) — perform compulsory participation, bear the autonomy costs
 *   - outgroup_intimate_partners: secondary target (moderate/mobile) — excluded from full relation by the boundary rules
 *   - secularized_descendants: excluded voice (moderate/mobile) — the boundary's output, unrepresented in the communal conversation
 *   - ritual_scholars: analytical observer (institutional/analytical) — document the structure without stake in its persistence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__boundary_maintenance_reading, 0.58).
domain_priors:suppression_score(catastrophe_memory_kernel__boundary_maintenance_reading, 0.6).
domain_priors:theater_ratio(catastrophe_memory_kernel__boundary_maintenance_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__boundary_maintenance_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__boundary_maintenance_reading, "Shared Mourning Practice as Group-Boundary Enforcement").
narrative_ontology:topic_domain(catastrophe_memory_kernel__boundary_maintenance_reading, "religious/collective_memory/social").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__boundary_maintenance_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__boundary_maintenance_reading, '6a8986d0-9841-4551-84ba-56182cf5df21').
narrative_ontology:cs_kernel_codification('6a8986d0-9841-4551-84ba-56182cf5df21', fixed_text).
narrative_ontology:cs_authority_grounding('6a8986d0-9841-4551-84ba-56182cf5df21', lineage).
narrative_ontology:cs_interpretation_layer_present('6a8986d0-9841-4551-84ba-56182cf5df21').
narrative_ontology:cs_reading_relation('6a8986d0-9841-4551-84ba-56182cf5df21', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('6a8986d0-9841-4551-84ba-56182cf5df21', catastrophe_memory_kernel__survival_competence_reading, influences).
narrative_ontology:cs_reading_relation('6a8986d0-9841-4551-84ba-56182cf5df21', catastrophe_memory_kernel__trauma_encoding_reading, influences).
narrative_ontology:cs_axiom('6a8986d0-9841-4551-84ba-56182cf5df21', foundational, mourning_participation_constitutes_membership).
narrative_ontology:cs_axiom_status(mourning_participation_constitutes_membership, holdable).
narrative_ontology:cs_axiom_grounding('6a8986d0-9841-4551-84ba-56182cf5df21', mourning_participation_constitutes_membership, conventional).
narrative_ontology:cs_axiom('6a8986d0-9841-4551-84ba-56182cf5df21', foundational, boundary_integrity_precedes_individual_practice_autonomy).
narrative_ontology:cs_axiom_status(boundary_integrity_precedes_individual_practice_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('6a8986d0-9841-4551-84ba-56182cf5df21', boundary_integrity_precedes_individual_practice_autonomy, deontological).
narrative_ontology:cs_reference_frame('6a8986d0-9841-4551-84ba-56182cf5df21', enforced_mourning_boundary).
narrative_ontology:cs_drift_state('6a8986d0-9841-4551-84ba-56182cf5df21', contemporary_secularizing_diaspora, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('6a8986d0-9841-4551-84ba-56182cf5df21', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__boundary_maintenance_reading, communal_leadership).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__boundary_maintenance_reading, core_observant_families).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__boundary_maintenance_reading, doubting_members).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__boundary_maintenance_reading, outgroup_intimate_partners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__boundary_maintenance_reading, rank_and_file_members).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__boundary_maintenance_reading, rank_and_file_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rabbis and lay councils set the mourning calendar, fix the liturgy, and decide which life events require communal rites. They keep attendance rolls, visit households that missed observances, and counsel couples considering partners outside the community. Their standing, livelihood, and moral authority rest on administering these obligations; stepping away would mean surrendering the role that organizes their entire public identity. Deference, honor, and institutional resources flow to them as the administrators of the calendar.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, communal_leadership, agenda_setter,
    institutional, generational, identity_locked, global).

% Long-established member families whose marriages, business ties, and children's schooling all run through the community. The mourning calendar structures their year and their status: visible scrupulousness in observance earns honor seats and matchmaking priority. Leaving would forfeit the dense web of mutual aid — loans, childcare, emergency support — that only full members access.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, core_observant_families, beneficiary,
    organized, generational, constrained, global).

% Ordinary adult members who attend the annual fasts and memorial gatherings, contribute to bereavement meals, and enroll children in memory-education. They receive belonging, disaster mutual aid, and a ready-made answer to the question of who they are. They also submit to attendance expectations, accept that dating outside the community ends in estrangement, and absorb the calendar's demands even in years when the grief being commemorated is not personally theirs.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, rank_and_file_members, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__boundary_maintenance_reading, rank_and_file_members, payer).

% Members raised inside the community who privately doubt the theology or resent the calendar but continue to attend and perform the rites. Speaking the doubt aloud costs friendships, marriage prospects, and sometimes livelihood; most manage the discrepancy silently, performing grief on schedule that they do not feel. Their sense of self was built inside the membership, so walking away reads to them less like a choice than an amputation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, doubting_members, payer,
    powerless, biographical, identity_locked, global).

% Spouses, fiancés, and close friends from outside the community who fall in love with, or befriend, members. The boundary rules cap what the relationship may become: full inclusion requires adopting the mourning obligations wholesale, refusal pressures the member to choose, and the partner experiences the rites as a wall that marks them permanently other. They retain lives, careers, and relationships outside the community, so the wall costs them a particular future rather than their whole world.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, outgroup_intimate_partners, payer,
    moderate, biographical, mobile, global).

% Adult children and grandchildren who drifted from observance and now mark catastrophes privately or through civic memorials. They are the boundary's output: their departure is cited in sermons as the cost of laxity, yet no seat in the communal conversation represents them. They would object that the enforcement itself drove them out and that their private mourning counts as fidelity too.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, secularized_descendants, excluded,
    moderate, generational, mobile, global).

% Academic observers of ritual and collective memory who study the community's mourning calendar comparatively. They document attendance patterns, sanction cases, and intermarriage rates, publish analyses neither the leadership nor the members commissioned, and hold no stake in whether the rites persist.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, ritual_scholars, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_kernel__boundary_maintenance_reading, communal_leadership).
narrative_ontology:fixing_cost_class(catastrophe_memory_kernel__boundary_maintenance_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Synchronizes the community's grief onto a shared calendar: bereavement labor is pooled, catastrophe memory is transmitted to children on schedule, and membership is made publicly visible and mutually witnessed at recurring intervals.
% TRANSFER_FUNCTION: Moves conformity and attention from individual members to the collective boundary — attendance, performed grief, restricted intimate choice — and moves deference, honor, and marriage-network closure inward toward compliant members and the leadership that administers the obligations.
% ABSENT_VOICES: Secularized descendants and outgroup intimate partners are outside the conversation entirely; internally, doubting members are present in body but cannot voice dissent without paying for it, so the consensus the rites display is curated rather than free.
% DISAPPEARANCE_RATIONALE: If the enforced mourning calendar vanished overnight, the community's year would lose its spine: memory transmission would fragment into household habit, mutual aid would thin as the membership marker blurred, marriage would open outward within a generation or two, and leadership authority — which administers the obligations — would lose its administrative object. Something like the community might persist as an ethnicity, but the bounded, self-reproducing group this arrangement produces would not survive intact.
% FOUNDING_PROBLEM: After catastrophic destruction and dispersal, survivors faced collective extinction by attrition: scattered households, assimilating children, no shared occasion on which the dead were mourned or the danger rehearsed. The arrangement was built so that remembering would be obligatory, public, and recurring — binding the scattered into one grieving, vigilant body.
% FOUNDING_PROBLEM_CORROBORATION: Historians outside the community corroborate the founding catastrophe and the post-catastrophe institution-building that followed; sociologists of religion corroborate that the boundary-enforcement function now operates largely independently of any live existential threat. No party outside the benefiting leadership attests that the original mortal danger remains live — the corroboration record supports 'problem transformed, arrangement intensified,' which is why the status is contested rather than live.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__boundary_maintenance_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__boundary_maintenance_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__boundary_maintenance_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_kernel__boundary_maintenance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__boundary_maintenance_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__boundary_maintenance_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_kernel__boundary_maintenance_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_kernel__boundary_maintenance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.58): the calendar returns real goods (belonging, mutual aid, scheduled memory transmission) while imposing conformity costs, restricting intimate choice, and sanctioning dissent — asymmetric, but not cover for pure extraction. Suppression (0.60) is enforcement through attendance monitoring, marriage gatekeeping, and social sanction rather than force; it is a raw structural property and is deliberately NOT scaled by power or scope in my authoring — the engine owns that arithmetic. Theater (0.25) is low-moderate: the rites do genuine mnemonic and solidarity work, though performance-for-witness grows as lived catastrophe recedes. Accessibility collapse (0.45): alternatives (private mourning, civic commemoration, exit) remain visible and legible but are costly, so they are suppressed rather than erased. Resistance (0.50): assimilation drift, intermittent open dissent, and secular exit meet the enforcement continuously. The measurement series runs on one shared grid (all three metrics at t=0,10,20,30,40,50,60) so no metric row borrows another's end-state values. The trajectories are monotonic intensification, not cyclical: as the founding catastrophe passed from living memory, enforcement machinery matured (rising suppression series) and extraction crept upward while the coordination goods stayed roughly constant — the classic pattern of a coordination structure accreting boundary-policing overhead.
 *
 * PERSPECTIVAL GAP:
 *   From the leadership seat the arrangement is sacred duty faithfully administered: the same data the engine reads as enforcement reads locally as pastoral care. From the doubting member's seat the identical structure is compulsory performance with a price on honesty. From the outgroup partner's seat it is a wall that converts love into a conversion demand. From the scholar's seat it is a textbook boundary-maintenance system. The engine computes these per-seat classifications from the structural data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Communal_leadership sits near the beneficiary end (collects deference, authority, and livelihood from administering the obligations) but its identity-lock damps exit-driven arbitrage. Core_observant_families sit low-d: status, marriage-network closure, and mutual aid flow to them. Rank_and_file_members derive mid-range: declared beneficiary with a substantial payer secondary role — belonging received, conformity paid. Doubting_members sit near the full-target end: they bear the transfer (compelled participation, foreclosed honesty) with identity-locked exit amplifying their effective extraction. Outgroup_intimate_partners bear real exclusion costs but their mobility outside the community damps effective extraction relative to trapped insiders. Secularized_descendants are outside the arrangement's benefit flow entirely — their position is the boundary's product, not a seat within it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — collective extinction by attrition after catastrophe — is contested rather than dead: leadership attests perpetual vigilance; the corroborating record (historians, sociologists of religion) attests the problem has been transformed while enforcement intensified. Because status is contested rather than dead, the mismatch consumer does not fire the zombie flag outright, but the rising suppression series alongside a receding threat is precisely the accumulation pattern that precedes mandate-outlived-function. Classifying this as a hybrid coordination-extraction structure prevents both misreadings: reading it as pure extraction erases the solidarity and memory goods that measurably flow to compliant members; reading it as pure coordination erases the autonomy costs borne by doubting members and the exclusion borne by outsiders. If the founding problem is ultimately judged dead while the enforcement series keeps rising, the arrangement trends toward inertia maintained by administrators who could change it but for whom the cost of fixing exceeds what they bear.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint instantiates the boundary_maintenance_reading of catastrophe_memory_kernel; what structural differences would the sibling readings (symbol_continuity, survival_competence, trauma_encoding) introduce if instantiated instead?',
    'Generate the three sibling stories and compare epsilon, victim sets, and computed types across the four readings of the shared kernel.',
    'A symbol-continuity instantiation would shrink the victim set (continuity goods reach nearly all members) and lower epsilon; a survival-competence instantiation would reframe conformity costs as training costs and shift victims toward those excluded from competence transmission; a trauma-encoding instantiation would add victims among members re-injured by compulsory remembrance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer-frame indexicality: one kernel, four readings, four structurally distinct constraints.').

omega_variable(
    boundary_function_vs_leadership_interest,
    'Is the boundary-enforcement effect intrinsic to shared mourning practice, or an overlay imposed by leadership whose authority and livelihood depend on administering the obligations?',
    'Compare communities with identical mourning calendars but different governance (lay-led, rabbinic, congregational-democratic); measure whether boundary strictness tracks governance type at fixed ritual content.',
    'If strictness tracks governance rather than ritual content, part of the measured extraction is leadership rent and the constraint drifts toward pure extraction; if invariant across governance forms, the boundary function is constitutive of the practice itself and the hybrid reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_function_vs_leadership_interest, empirical, 'Whether enforcement intensity is a property of the ritual or of its administrators.').

omega_variable(
    suppression_internalization_split,
    'Is the measured suppression structural (attendance monitoring, marriage gatekeeping, social sanction) or internalized (members who have fused identity with membership and no longer experience the obligation as external)?',
    'Post-exit trajectory study: interview leavers at 1, 5, and 10 years after exit; if calendar-compliance and guilt persist after all sanctions have ceased, a large share of the suppression is internalized.',
    'If internalized, effective suppression exceeds the structural measure and survives removal of the enforcement machinery, changing any transition or reform scenario.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_split, empirical, 'Structural versus internalized suppression mechanism in compulsory mourning observance.').

omega_variable(
    consent_under_identity_lock,
    'Can participation extracted from identity-locked members count as consensual coordination, or does identity lock convert the coordination into compelled transfer?',
    'Pair conceptual analysis with preference data from members offered genuine cost-free exit (relocation supported, exogamous marriage accepted without estrangement): observe whether participation rates persist once the exit penalty is removed.',
    'If participation collapses when penalties vanish, the coordination component is smaller than claimed and effective extraction rises; if it persists, the solidarity goods are real and the hybrid coordination-plus-extraction reading strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consent_under_identity_lock, conceptual, 'Whether identity lock invalidates the consent basis of the coordination function.').

omega_variable(
    theater_drift_endpoint,
    'As the founding catastrophe recedes from living memory, is the rising theater_ratio decay toward inertial performance (rites maintained theatrically after their function atrophies) or stabilization at a healthy-coordination steady state?',
    'Extend the measurement series across coming decades; watch whether theater_ratio crosses 0.5 while extractiveness plateaus (inertial drift) or plateaus below it.',
    'Inertial drift would eventually justify redesign or retirement of the enforced calendar; steady-state stabilization would confirm the boundary function now reproduces itself independent of living memory of the catastrophe.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_drift_endpoint, empirical, 'Lifecycle endpoint of the mourning-practice regime as living memory fades.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__boundary_maintenance_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 30, 0.19).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 50, 0.24).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 60, 0.25).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 10, 0.46).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 30, 0.53).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 50, 0.57).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 60, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 10, 0.51).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 20, 0.54).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 30, 0.56).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 50, 0.59).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 60, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__boundary_maintenance_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel__trauma_encoding_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the community's mourning rituals' decomposes into four structurally distinct claims (boundary enforcement, symbolic continuity, survival-competence transmission, trauma-encoded warning), per the epsilon-invariance principle — measuring the regime by membership-policing outcomes versus continuity outcomes versus competence-transmission outcomes yields materially different epsilon values, so they are separate stories sharing one kernel. This boundary_maintenance_reading is upstream of the other three in one specific sense: enforced boundaries determine the membership pool through which continuity, competence, and trauma signals circulate, so this reading's enforcement intensity changes the operating environment of its siblings without logically ruling any of them out.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
