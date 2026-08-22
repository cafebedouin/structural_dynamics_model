% ============================================================================
% CONSTRAINT STORY: preparedness_transmission__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_transmission__competence_reading, []).

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
 *   constraint_id: preparedness_transmission__competence_reading
 *   human_readable: Drill-and-Inspection Regime as Live Competence Transmission (Competence Reading)
 *   domain: institutional/civil-defense
 *
 * SUMMARY:
 *   A national civil defense system mandates recurring multi-agency drills
 *   and periodic inspections of plans, equipment, and personnel readiness.
 *   Under the competence reading authored here, these performances are live
 *   exercised knowledge: each generation of responders re-validates
 *   capability by practicing it, inspectors accumulate the pattern
 *   recognition to spot novel failure signatures, and participants improvise
 *   successfully when scenarios deviate from script. The regime's costs are
 *   real but convert into the readiness its participants and publics consume.
 *   This file is one member of the preparedness_transmission constraint
 *   family; the sibling readings are separate constraint files linked through
 *   network.affects_constraints, and this file's epsilon and stakeholder
 *   valences are valid only under this reading. KEY AGENTS (by structural
 *   relationship): - national_civil_defense_authority: Agenda-setter
 *   (institutional/arbitrage) — writes and funds the mandate, can restructure
 *   the regime - regional_inspection_corps: Primary beneficiary with
 *   enforcement discretion (organized/mobile) — collects career standing and
 *   certification authority - county_emergency_management_agencies: Dual
 *   payer/beneficiary (moderate/constrained) — bears drill costs, draws
 *   readiness returns - small_rural_response_agencies: Primary target
 *   (powerless/trapped) — bears disproportionate compliance burden with
 *   thinnest capacity - hazard_zone_residents: Diffuse beneficiary
 *   (moderate/constrained) — consumes readiness indirectly, funds it through
 *   taxes - mutual_aid_partner_networks: Secondary beneficiary
 *   (organized/mobile) — gains cross-boundary interoperability -
 *   academic_disaster_researchers: Analytical observer — measures whether
 *   practice transfers to performance
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__competence_reading, 0.2).
domain_priors:suppression_score(preparedness_transmission__competence_reading, 0.35).
domain_priors:theater_ratio(preparedness_transmission__competence_reading, 0.24).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, theater_ratio, 0.24).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__competence_reading, rope).
narrative_ontology:human_readable(preparedness_transmission__competence_reading, "Drill-and-Inspection Regime as Live Competence Transmission (Competence Reading)").
narrative_ontology:topic_domain(preparedness_transmission__competence_reading, "institutional/civil-defense").

domain_priors:requires_active_enforcement(preparedness_transmission__competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__competence_reading, '829e9be3-9a2d-459f-aeb6-1515215ded91').
narrative_ontology:cs_kernel_codification('829e9be3-9a2d-459f-aeb6-1515215ded91', formalized).
narrative_ontology:cs_authority_grounding('829e9be3-9a2d-459f-aeb6-1515215ded91', practice).
narrative_ontology:cs_interpretation_layer_present('829e9be3-9a2d-459f-aeb6-1515215ded91').
narrative_ontology:cs_reading_relation('829e9be3-9a2d-459f-aeb6-1515215ded91', preparedness_transmission__husk_reading, forecloses).
narrative_ontology:cs_reading_relation('829e9be3-9a2d-459f-aeb6-1515215ded91', preparedness_transmission__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('829e9be3-9a2d-459f-aeb6-1515215ded91', foundational, competence_requires_live_practice).
narrative_ontology:cs_axiom_status(competence_requires_live_practice, holdable).
narrative_ontology:cs_axiom_grounding('829e9be3-9a2d-459f-aeb6-1515215ded91', competence_requires_live_practice, empirically_contingent).
narrative_ontology:cs_axiom('829e9be3-9a2d-459f-aeb6-1515215ded91', secondary, findings_must_close_the_learning_loop).
narrative_ontology:cs_axiom_status(findings_must_close_the_learning_loop, holdable).
narrative_ontology:cs_axiom_grounding('829e9be3-9a2d-459f-aeb6-1515215ded91', findings_must_close_the_learning_loop, instrumental).
narrative_ontology:cs_reference_frame('829e9be3-9a2d-459f-aeb6-1515215ded91', practice_validated_capability).
narrative_ontology:cs_drift_state('829e9be3-9a2d-459f-aeb6-1515215ded91', contemporary_after_action_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('829e9be3-9a2d-459f-aeb6-1515215ded91', '').
narrative_ontology:cs_kernel_id(preparedness_transmission__competence_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, regional_inspection_corps).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, hazard_zone_residents).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, mutual_aid_partner_networks).
narrative_ontology:constraint_victim(preparedness_transmission__competence_reading, small_rural_response_agencies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, county_emergency_management_agencies).
narrative_ontology:constraint_victim(preparedness_transmission__competence_reading, county_emergency_management_agencies).
narrative_ontology:constraint_vindicates(preparedness_transmission__competence_reading, deliberate_practice_retention_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes the national drill-frequency mandates, inspection standards, and certification criteria for civil defense and disaster-response organizations. Funds much of the exercise calendar through grants conditioned on compliance. Can rewrite the regime, but depends on the inspection corps and participating agencies to make it real on the ground.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, national_civil_defense_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Career inspectors and exercise evaluators who travel between jurisdictions verifying plans, equipment, and drill performance. Their professional standing, promotion paths, and later consulting prospects are built on the inspection calendar. Many are veterans of major incidents whose judgment shapes which findings matter and which scenarios get designed next cycle.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, regional_inspection_corps, beneficiary,
    organized, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__competence_reading, regional_inspection_corps, agenda_setter).

% Run the local drill calendar, host exercises, and absorb inspection visits while carrying day-to-day response duties. Drill hours come out of the same staff time and budgets as actual service delivery, but the same rehearsals are what make multi-agency response work when an incident lands. Leaving is not an option: the statutory duties and the hazards stay where they are.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, county_emergency_management_agencies, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__competence_reading, county_emergency_management_agencies, beneficiary).

% Volunteer-heavy fire, EMS, and rescue services with a handful of paid staff. Every mandated drill hour displaces fundraising, backfill training, or call coverage, and inspection preparation falls on the same three people who run everything else. Waivers exist on paper but are slow and stigmatizing; not participating means losing grant eligibility and mutual-aid standing, and the hazard exposure cannot be relocated.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, small_rural_response_agencies, payer,
    powerless, immediate, trapped, local).

% Live in flood plains, wildfire interfaces, seismic zones, or downwind of industrial sites. They fund the exercise system through taxes and occasionally have roads closed for full-scale drills. What they receive is mostly invisible: response organizations that arrive already knowing each other's procedures, equipment that works, and plans that have been physically rehearsed rather than filed.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, hazard_zone_residents, beneficiary,
    moderate, biographical, constrained, local).

% Neighboring jurisdictions and specialized teams — urban search and rescue, hazmat, medical surge — that deploy across jurisdictional boundaries during major events. Standardized drills are how they learn each other's radio protocols, command structures, and equipment quirks before the incident rather than during it.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, mutual_aid_partner_networks, beneficiary,
    organized, generational, mobile, regional).

% Study whether exercise participation predicts incident performance, using after-action data, survey panels, and controlled scenario-injection experiments. Publish findings that feed back into mandate design; hold no operational role in the regime they study.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, academic_disaster_researchers, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_transmission__competence_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_transmission__competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes rehearsed interoperability among dispersed response organizations before incidents: shared procedures, common vocabulary, tested equipment, and personal familiarity across agency lines, renewed often enough that skills and relationships do not decay between uses.
% TRANSFER_FUNCTION: Moves staff hours, budget, and attention out of routine service delivery into rehearsal and verification, converting them into a shared readiness stock held across agencies; moves certification authority and career standing to the inspection corps; moves grant funds from the national authority to compliant local agencies.
% ABSENT_VOICES: Rural volunteer agencies have the weakest seat at mandate-design tables dominated by metro agencies and the national authority; residents affected by past incidents rarely sit on the after-action panels that write future scenarios; front-line volunteers' tacit knowledge enters only through formal channels. Their objections — drill fatigue, unrealistic scenarios, unfunded mandates — surface mainly in surveys, not in the rooms where frequencies and standards are set.
% DISAPPEARANCE_RATIONALE: Response organizations would drift apart procedurally within a few years — radio protocols, command habits, and personal familiarity decay without rehearsal — and the first major cross-boundary incident after lapse would expose the gaps, as comparable historical lapses have. Grant conditioning, certification, and the inspection economy would unwind with it.
% FOUNDING_PROBLEM: Mid-century civil defense reviews and successive disaster inquiries found that paper plans failed under stress: organizations that had never operated together could not coordinate in the first hours, equipment sat unmaintained, and skills decayed between rare real events. The drill-and-inspection regime was built to convert written plans into practiced capability and to detect decay before an incident did.
% FOUNDING_PROBLEM_CORROBORATION: Independent disaster inquiries and after-action commissions — seated outside the benefiting parties — repeatedly attribute poor incident outcomes to unrehearsed coordination and credit exercised ones; peer-reviewed studies of exercise-to-performance transfer corroborate the founding problem from academic seats. No serious seat disputes that unpracticed coordination fails under stress.
narrative_ontology:disappearance_verdict(preparedness_transmission__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_transmission__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_transmission__competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_transmission__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_transmission__competence_reading, 0.2, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_transmission__competence_reading_tests).
:- end_tests(preparedness_transmission__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low (0.20) because the regime's costs — drill hours, inspection preparation, exercise budgets — convert directly into the readiness the participants themselves consume; the residual is administrative overhead plus the disproportionate burden falling on thin-staffed rural agencies. Suppression (0.35) reflects statutory compulsion and grant conditioning rather than closed alternatives: agencies can shape scenario design, seek waivers, and vary exercise formats, but cannot exit the mandate while holding response duties. Suppression is authored as a raw structural property and is left unscaled — only extractiveness is scaled by directionality and scope in the engine's computation. Theater ratio (0.24) is low but nonzero: photo-op exercises and checkbox inspections exist inside an otherwise functional calendar, and the series shows slow creep (0.18 to 0.24) worth monitoring. Accessibility collapse (0.45): alternatives to formal drills exist — real-incident experience, apprenticeship, simulation platforms — and partially substitute, but none reproduces multi-agency rehearsal at scale. Resistance (0.30): grumbling, waiver-seeking, and occasional non-participation by rural agencies, without organized opposition. The suppression_requirement series is omitted deliberately: enforcement capacity is stable across the interval, so the static scalar carries the picture. All temporal series share one grid (t=0 to 24, step 4) so no metric row is sampled against another's gaps.
 *
 * PERSPECTIVAL GAP:
 *   From the inspection corps' seat the regime is a vocation: each visit exercises judgment that matters, and the corps' own competence grows with exposure to varied jurisdictions. From the rural agency's seat the same visit is an unfunded mandate consuming its scarcest resource. Metro county agencies sit near the midpoint — they pay heavily and draw heavily on the interoperability the drills build. Residents experience the regime almost entirely through its outputs, or their absence. The engine computes these divergent per-seat classifications from the structural data; the authored coordination-dominant claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (inspection corps, residents, mutual-aid networks) derive low directionality — the regime subsidizes them. The declared victim group (rural agencies) derives high directionality — they bear extraction with the least return, and their trapped exit position pushes them toward the full-target end. County agencies are authored as dual-positioned (payer with secondary beneficiary) precisely so the engine sees both flows rather than a single valence; no power-atom override is used because overrides apply at power-atom granularity and would collide with residents sharing the moderate atom. The national authority sits near the beneficiary end through arbitrage-grade exit — it writes the rules it administers and can restructure rather than endure them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem remains live — disasters recur and capability decays without practice — and its status is corroborated from outside the benefiting parties by independent after-action commissions and academic transfer studies. The live-status pairing with a world-rearranges disappearance verdict yields no obsolescence flag. The classification discipline cuts both ways: it prevents the regime's genuine function from being misread as pure ritual (the error a hollowed-out reading of the same performances would make), while the theater_ratio series and the rural-burden omega mark exactly where decay would show first. If theater crossed roughly 0.5 alongside failing transfer measures, the same structural data would support reclassification pressure toward inertial-performance territory. Mandatrophy here is a monitoring posture, not a verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the preparedness_transmission kernel (competence_reading). Would adopting the husk_reading or hybrid_reading instead change the structural classification, and where exactly is the disagreement located?',
    'Reading choice is settled by the transfer evidence: longitudinal drill-to-performance studies and scenario-injection tests of improvisation under novel signatures. Adoption of husk_reading would re-author the same performances with high theater_ratio, inverted beneficiary structure, and an inertia-dominated type; adoption of hybrid_reading would split this file into stratified sub-constraints with divergent epsilon per domain.',
    'Epsilon (0.20), the beneficiary/victim valences, and the coordination-dominant claim are all valid only under this reading; the sibling files carry the same performances under different structural commitments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this file is the competence reading of the preparedness_transmission kernel; rivals are separate constraints, and the disagreement lives in whether drill participation produces transferable operational competence.').

omega_variable(
    drill_to_performance_transfer,
    'Does drill participation causally improve incident performance — novel-signature recognition by inspectors, improvised coordination by participants under scenario variation — or does measured readiness reflect selection effects and self-report?',
    'Blinded scenario-injection evaluations with unannounced variations, and matched-jurisdiction comparisons of cumulative drill hours against realized incident outcomes.',
    'Confirmation stabilizes the low-theater, low-extraction profile authored here; failure would push theater_ratio sharply upward and shift classification pressure toward the rival husk reading''s structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drill_to_performance_transfer, empirical, 'Whether the regime''s performances transmit live capability or merely sustain the appearance of it.').

omega_variable(
    rural_burden_asymmetry_tolerance,
    'Is the disproportionate compliance burden on small rural agencies a stable coordination cost, or a growing inequity that will erode participation and mutual-aid cohesion?',
    'Track rural drill-hour displacement of service delivery, waiver uptake rates, and rural agency attrition from mutual-aid rosters across successive mandate cycles.',
    'If the burden grows faster than rural capacity, effective extraction on the rural seat rises and the coordination-dominant classification trends toward a hybrid coordination/extraction structure with the rural seat as systematic payer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rural_burden_asymmetry_tolerance, empirical, 'Whether the regime''s main internal asymmetry is bounded or accumulating.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__competence_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_transmission__competence_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(prep_tr_t0, observed).
narrative_ontology:measurement(prep_tr_t4, preparedness_transmission__competence_reading, theater_ratio, 4, 0.19).
narrative_ontology:measurement_basis(prep_tr_t4, observed).
narrative_ontology:measurement(prep_tr_t8, preparedness_transmission__competence_reading, theater_ratio, 8, 0.21).
narrative_ontology:measurement_basis(prep_tr_t8, observed).
narrative_ontology:measurement(prep_tr_t12, preparedness_transmission__competence_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement_basis(prep_tr_t12, observed).
narrative_ontology:measurement(prep_tr_t16, preparedness_transmission__competence_reading, theater_ratio, 16, 0.23).
narrative_ontology:measurement_basis(prep_tr_t16, observed).
narrative_ontology:measurement(prep_tr_t20, preparedness_transmission__competence_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement_basis(prep_tr_t20, observed).
narrative_ontology:measurement(prep_tr_t24, preparedness_transmission__competence_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement_basis(prep_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_transmission__competence_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(prep_be_t0, observed).
narrative_ontology:measurement(prep_be_t4, preparedness_transmission__competence_reading, base_extractiveness, 4, 0.16).
narrative_ontology:measurement_basis(prep_be_t4, observed).
narrative_ontology:measurement(prep_be_t8, preparedness_transmission__competence_reading, base_extractiveness, 8, 0.17).
narrative_ontology:measurement_basis(prep_be_t8, observed).
narrative_ontology:measurement(prep_be_t12, preparedness_transmission__competence_reading, base_extractiveness, 12, 0.18).
narrative_ontology:measurement_basis(prep_be_t12, observed).
narrative_ontology:measurement(prep_be_t16, preparedness_transmission__competence_reading, base_extractiveness, 16, 0.19).
narrative_ontology:measurement_basis(prep_be_t16, observed).
narrative_ontology:measurement(prep_be_t20, preparedness_transmission__competence_reading, base_extractiveness, 20, 0.19).
narrative_ontology:measurement_basis(prep_be_t20, observed).
narrative_ontology:measurement(prep_be_t24, preparedness_transmission__competence_reading, base_extractiveness, 24, 0.2).
narrative_ontology:measurement_basis(prep_be_t24, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(preparedness_transmission__competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission__competence_reading, identity_coordination).
narrative_ontology:affects_constraint(preparedness_transmission__competence_reading, preparedness_transmission__husk_reading).
narrative_ontology:affects_constraint(preparedness_transmission__competence_reading, preparedness_transmission__hybrid_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'drills keep us ready' conflates three structurally distinct claims about the same performance regime: that practice transmits live competence (this file, low epsilon, coordination-dominant), that identical performances persist as memorial husks after operational knowledge has hollowed out (separate file, high theater, inertia-dominated), and that transmission is stratified across domains with engineering competence live while civilian coordination knowledge decays (separate file, split victim/beneficiary structure). Per the epsilon-invariance principle these are authored as separate constraints with separate epsilon values and separate stakeholder valences. This file is the upstream optimistic baseline; the rival readings are downstream contestations that take this regime's audited operation as their object, so this file links both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
