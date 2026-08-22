% ============================================================================
% CONSTRAINT STORY: preparedness_retention__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_retention__husk_reading, []).

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
 *   constraint_id: preparedness_retention__husk_reading
 *   human_readable: Ceremonial Drill-and-Inspection Regime (Memorial Performance Reading)
 *   domain: governance/institutional_memory/disaster_preparedness
 *
 * SUMMARY:
 *   In a low-lying, dike-protected country, a statutory regime of recurring
 *   multi-agency exercises and preparedness inspections consumes a large
 *   share of the crisis-management budget. Assessed by this story's reading,
 *   the regime operates as memorial performance: scenarios are written months
 *   in advance and briefed to participants, exercises are staged to conclude
 *   successfully, inspections verify documentation rather than skill, and the
 *   assurance produced flows upward through reporting chains while live
 *   response capacity stagnates. The arrangement was built after catastrophic
 *   near-miss evacuations to prove diligence and synchronize agencies that
 *   had never operated together; three decades on, the synchronization shell
 *   persists and the assurance function has detached from capability.
 *   Resource allocation follows visibility: budget flows to what can be
 *   scheduled, filmed, and audited, and tacit skill retention — which fails
 *   publicly and cannot be certified in advance — loses every allocation
 *   round. The epsilon referent throughout is the standing
 *   drill-and-inspection arrangement itself, as this reading assesses it. KEY
 *   AGENTS (by structural relationship): - safety_region_exercise_planners:
 *   agenda-setter (institutional/constrained) — scripts, stages, and
 *   certifies the exercise cycle - municipal_executive_boards: primary
 *   beneficiary (institutional/mobile) — converts completed drills into
 *   legitimacy and liability cover - exercise_vendor_consultancies: secondary
 *   beneficiary (organized/arbitrage) — sells scenario packages and
 *   compliance documentation; the seat budgets land in -
 *   flood_plain_residents: primary target (powerless/trapped) — bears the
 *   deferred response-capacity risk behind the dikes -
 *   frontline_response_workers: secondary target (organized/constrained) —
 *   pays training hours into ceremony and inherits the gap during events -
 *   drill_efficacy_researchers: excluded voice (moderate/analytical) — holds
 *   the transfer-of-training evidence, holds no seat -
 *   parliamentary_inquiry_committees: analytical observer
 *   (institutional/analytical) — documents the gap after each near-miss,
 *   changes nothing structural
 *
 * KEY AGENTS:
 *   - safety_region_exercise_planners: agenda-setter (institutional/constrained) — designs the annual exercise calendar, writes success-scripted scenarios, compiles inspection dossiers; careers are bound to the calendar they administer
 *   - municipal_executive_boards: primary beneficiary (institutional/mobile) — collect public assurance, budget defense, and documented diligence from the exercise cycle without running it; rotate out to other portfolios
 *   - exercise_vendor_consultancies: secondary beneficiary (organized/arbitrage) — consultancy firms whose revenue scales with exercise volume and formality; multi-jurisdictional, exit is arbitrage across compliance markets
 *   - flood_plain_residents: primary target (powerless/trapped) — fund the system through taxes, are its intended protectees, have no seat in exercise design, cannot relocate away from flood exposure
 *   - frontline_response_workers: secondary target (organized/constrained) — firefighters, ambulance crews, police, dike crews spending hundreds of duty hours yearly in pre-scripted exercises; unions voice dissatisfaction cautiously
 *   - drill_efficacy_researchers: excluded voice (moderate/analytical) — disaster sociologists and training scientists with published transfer findings, outside certification committees and funding decisions
 *   - parliamentary_inquiry_committees: analytical observer (institutional/analytical) — convene after near-misses and floods, interview all seats, publish the gap, control neither budgets nor standards
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_retention__husk_reading, 0.72).
domain_priors:suppression_score(preparedness_retention__husk_reading, 0.58).
domain_priors:theater_ratio(preparedness_retention__husk_reading, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, theater_ratio, 0.78).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_retention__husk_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_retention__husk_reading, "Ceremonial Drill-and-Inspection Regime (Memorial Performance Reading)").
narrative_ontology:topic_domain(preparedness_retention__husk_reading, "governance/institutional_memory/disaster_preparedness").

domain_priors:requires_active_enforcement(preparedness_retention__husk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_retention__husk_reading, 'c0ee7461-938b-43aa-8e8a-58dcca3684a1').
narrative_ontology:cs_kernel_codification('c0ee7461-938b-43aa-8e8a-58dcca3684a1', formalized).
narrative_ontology:cs_authority_grounding('c0ee7461-938b-43aa-8e8a-58dcca3684a1', extraction).
narrative_ontology:cs_interpretation_layer_present('c0ee7461-938b-43aa-8e8a-58dcca3684a1').
narrative_ontology:cs_reading_relation('c0ee7461-938b-43aa-8e8a-58dcca3684a1', preparedness_retention__competence_reading, forecloses).
narrative_ontology:cs_reading_relation('c0ee7461-938b-43aa-8e8a-58dcca3684a1', preparedness_retention__hybrid_reading, forecloses).
narrative_ontology:cs_axiom('c0ee7461-938b-43aa-8e8a-58dcca3684a1', foundational, ceremony_displaces_live_competence).
narrative_ontology:cs_axiom_status(ceremony_displaces_live_competence, holdable).
narrative_ontology:cs_axiom_grounding('c0ee7461-938b-43aa-8e8a-58dcca3684a1', ceremony_displaces_live_competence, empirically_contingent).
narrative_ontology:cs_axiom('c0ee7461-938b-43aa-8e8a-58dcca3684a1', secondary, auditability_selects_for_performative_training).
narrative_ontology:cs_axiom_status(auditability_selects_for_performative_training, holdable).
narrative_ontology:cs_axiom_grounding('c0ee7461-938b-43aa-8e8a-58dcca3684a1', auditability_selects_for_performative_training, instrumental).
narrative_ontology:cs_reference_frame('c0ee7461-938b-43aa-8e8a-58dcca3684a1', ceremonial_readiness_doctrine).
narrative_ontology:cs_drift_state('c0ee7461-938b-43aa-8e8a-58dcca3684a1', contemporary_post_inquiry_era, gap(repudiation_pressure, minor, true)).
narrative_ontology:cs_created_at('c0ee7461-938b-43aa-8e8a-58dcca3684a1', '').
narrative_ontology:cs_kernel_id(preparedness_retention__husk_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__husk_reading, municipal_executive_boards).
narrative_ontology:constraint_beneficiary(preparedness_retention__husk_reading, exercise_vendor_consultancies).
narrative_ontology:constraint_victim(preparedness_retention__husk_reading, flood_plain_residents).
narrative_ontology:constraint_victim(preparedness_retention__husk_reading, frontline_response_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and run the annual multi-agency exercise calendar for a safety region: they write scenario scripts months in advance, brief participating services on the intended sequence, stage the exercise, and compile the after-action dossier that inspections require. Their professional standing rests on delivering polished, auditable exercises; proposing unscripted formats that might fail publicly would put the careers built on the current calendar at risk. Leaving the field means abandoning accumulated seniority in a small professional circle.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, safety_region_exercise_planners, agenda_setter,
    institutional, biographical, constrained, national).

% Elected and appointed board members of municipalities and joint public authorities. Each completed drill and passed inspection converts into a public assurance statement, a line in the budget defense, and documented diligence that limits personal exposure if a disaster later goes wrong. They do not run the exercises themselves; they collect the legitimacy the exercise cycle produces and can rotate to unrelated portfolios when political winds shift.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, municipal_executive_boards, beneficiary,
    institutional, biographical, mobile, regional).

% Consulting firms and training companies that sell scenario packages, facilitation, compliance documentation templates, and evaluation reports to regions and ministries. Revenue scales with the volume and formality of the exercise cycle rather than with measured improvement in response outcomes; they operate across multiple countries and can shift sales effort to whichever jurisdiction expands its compliance market next.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, exercise_vendor_consultancies, beneficiary,
    organized, biographical, arbitrage, global).

% People living below sea level behind the dikes, in the zones a national-scale flood scenario would inundate. They fund the preparedness system through taxes and are its ultimate intended protectees, yet they have no seat in exercise design and no way to verify that drill hours translate into rescue capacity. Moving away from flood-prone housing is blocked by property values, family ties, and work, so they carry the deferred risk if capacity is thinner than the paperwork suggests.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, flood_plain_residents, payer,
    powerless, generational, trapped, regional).

% Firefighters, ambulance crews, police officers, and dike crew members who spend hundreds of duty hours each year in pre-scripted exercises and inspection rehearsals. Many privately report that the scenarios are choreographed to succeed and teach little that transfers to chaotic conditions, but their unions voice dissatisfaction cautiously because open criticism reads as attacking public safety. Exiting means leaving a vocation, not just an employer.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, frontline_response_workers, payer,
    organized, biographical, constrained, regional).

% Academic specialists in disaster sociology and training science who study whether exercise participation improves real-world performance. Their published findings on poor skill transfer from scripted drills circulate in journals and conferences, but they hold no seat in certification committees, funding decisions, or inspection design; the compliance conversation proceeds without them.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, drill_efficacy_researchers, excluded,
    moderate, generational, analytical, continental).

% Ad hoc parliamentary commissions and the national court of audit, convened after near-misses and floods. They interview the other seats, commission analyses, and publish findings on the distance between exercised and actual capability. They can recommend restructuring the exercise economy but control neither its budgets nor its certification standards.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, parliamentary_inquiry_committees, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_retention__husk_reading, exercise_vendor_consultancies).
narrative_ontology:fixing_cost_class(preparedness_retention__husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the multi-agency synchronization problem: a national-scale flood requires dozens of municipalities, water boards, emergency services, and ministries to act on shared schedules, shared terminology, and pre-established contact paths. The annual exercise cycle builds and refreshes that common interface, and the inspection cycle produces a common evidentiary record through which oversight chains monitor the field.
% TRANSFER_FUNCTION: Moves preparedness budgets, staff duty hours, and public attention away from capability-building activity and toward documented, visible compliance performance. Assurance moves upward — from municipalities through ministries to the public — while deferred risk moves downward, landing on residents and frontline workers who absorb whatever gap remains when a real event arrives.
% ABSENT_VOICES: Residents of the deepest polder zones have no seat in exercise design; drill-efficacy researchers sit outside every certification committee; volunteer and citizen response networks that actually improvise in floods are never consulted. Their shared objection — that scripted success demonstrates nothing about unscripted failure — never enters the compliance record, because the record is compiled by the offices the objection targets.
% DISAPPEARANCE_RATIONALE: If the exercise-and-inspection cycle vanished overnight, the assurance chain would break immediately: boards would lose their diligence evidence, ministries would lose their oversight record, vendors would lose their revenue base, and planners would lose their professional object. Agencies would have to justify preparedness spending through demonstrated capability instead of documented activity, budgets would migrate toward unscripted training and equipment, and the inter-agency contact web would decay within a few years unless deliberately rebuilt on another footing.
% FOUNDING_PROBLEM: After the near-breach flood crises that forced the evacuation of roughly a quarter million people, the state faced a twin problem: a public that had just watched mass evacuation demanded demonstrable proof that the response apparatus worked, and the many agencies that would fight a real flood had never once operated together. Recurring drills and inspections were built to solve both — to synchronize the agencies and to make diligence visible and auditable.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: parliamentary inquiry committees and the national court of audit attest that the original inter-agency synchronization gaps have narrowed while the assurance function has detached from demonstrated capability; drill-efficacy researchers corroborate from published transfer-of-training studies that exercise volume does not track live performance. The only sources attesting that current drill volume evidences live readiness are the offices and vendors whose budgets and legitimacy the exercise economy sustains — no independent corroboration for that claim exists.
narrative_ontology:disappearance_verdict(preparedness_retention__husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_retention__husk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_retention__husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_retention__husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_retention__husk_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_retention__husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_retention__husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_retention__husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because the regime's resource draw — budgets, duty hours, planning capacity, public attention — is decoupled from measured improvement in response outcomes; the transfer from expenditure to capability is the exact quantity this reading finds near-absent. Suppression (0.58) is a raw structural property, unscaled by power or scope: participation is statutorily mandated, audit criteria reward documented participation, and liability law rewards demonstrable diligence, so unscripted formats that might fail publicly are priced out without ever being banned. Theater ratio (0.78) is the signature figure for this reading: most exercise hours are choreographed to succeed, participants are briefed on the intended sequence, and inspection activity verifies paper trails. Accessibility collapse sits at 0.50 — live-exercise and apprenticeship models remain fully conceivable and are demonstrated in adjacent professions, but are institutionally foreclosed inside the compliance economy, so alternatives are half-visible rather than unthinkable. Resistance (0.55) is real but fragmented: inquiry reports, researcher publications, and union caution recur after every event, yet no durable coalition forms because openly attacking drills reads as attacking public safety, and blame asymmetry punishes visible dissent before any event occurs. The temporal series run on one shared six-point grid so every tracked metric is authored at every examined time point; all three rise together over the interval as the enforcement ratchet tightened — each post-incident inquiry recommended 'more realistic exercises,' and each recommendation was implemented as more scheduling, more documentation, and more inspection, which raised the suppression requirement and the ceremony share simultaneously. On why not a purely inertial reading despite the high theater figure: theatricality here is a symptom, not the load-bearing fact — the arrangement is actively maintained because maintenance pays (vendor revenue, board legitimacy, planner careers), which places it with the captured-coordination family rather than the abandoned-husk family.
 *
 * PERSPECTIVAL GAP:
 *   From the planner, board, and vendor seats the regime is functioning professionalism: auditable diligence, deliverable contracts, defensible budgets, careers built on reliable delivery. From the resident and responder seats the same documents read as cover: assurance flowing upward while the capacity underneath it thins, and training hours spent rehearsing a success that was scripted in advance. The identical after-action dossier is proof upstairs and evidence of emptiness downstairs. The resident seat additionally carries a coalition problem: individually powerless and geographically bound, residents could in principle form a constituency around verified capability — the historical water-board tradition shows such civic oversight is possible — but the blame asymmetry that punishes pre-event dissent fragments exactly the coalition that could demand it. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Municipal executive boards and exercise vendor consultancies are declared beneficiaries: boards collect legitimacy and liability cover without operating the cycle, and vendors collect fees proportional to ceremony volume — both derive low directionality, with vendors pushed nearest the beneficiary end by arbitrage-grade exit across jurisdictions. Flood-plain residents are declared victims with trapped exit and no seat: they sit near the full-target end, and their powerlessness means the engine should weight their extraction heavily rather than assume coalition correction. Frontline response workers are victims with organized power but constrained exit — they pay the opportunity cost of training hours and inherit the capability gap, though their professional stake in the system's existence keeps them slightly inside the full-target position. The planner seat administers rather than collects; its constrained exit (career identity fused with the calendar it runs) pulls its derived directionality targetward despite its agenda-setting role. Researchers and inquiry committees are analytical and excluded respectively — they shape nothing and receive nothing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — proving diligence to a public that had just watched a quarter-million people evacuate, and synchronizing agencies that had never operated jointly — was real, and the exercise cycle genuinely solved the synchronization half: the inter-agency contact web, shared terminology, and common schedules it built are used in every real response. That surviving sliver of coordination is why the classification here is not pure extraction, and labeling the arrangement as such would erase the interface agencies genuinely rely on. The inverse error is equally available: reading the ceremony's sincerity as proof of coordination would launder memorial performance as readiness. Holding both facts — thin live coordination, dominant ceremonial extraction — is what the hybrid coordination/extraction category exists to do. The genealogy interview records the founding problem's status as contested: beneficiaries attest the trust problem is permanently live because threats evolve; inquiry committees and researchers attest the assurance function has detached from the capability it was built to evidence. Because the status is contested rather than dead, the zombie flag does not fire — but if the transfer-of-training omega resolves toward zero conversion, expect the computed classification to slide toward pure extraction with high theatrical maintenance, and the founding-problem mismatch to become decidable rather than disputed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates the husk_reading of kernel preparedness_retention; would instantiating competence_reading or hybrid_reading instead change the structural classification of the same drill-and-inspection arrangement?',
    'Transfer-of-training evidence decides between readings: if drill participation predicts live incident performance across regions, competence_reading governs; if performance tracks only the depth of specialized institutions, hybrid_reading; if no drill metric predicts performance anywhere, husk_reading stands.',
    'Under competence_reading the same regime would classify as coordination with negligible extraction and the victim sets dissolve; under hybrid_reading extraction concentrates on the societal tier while specialized-tier actors leave the victim set; beneficiary and victim declarations would be rewritten accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Kernel-membership omega: which reading of preparedness_retention this story instantiates and what sibling adoption would change structurally.').

omega_variable(
    ceremony_competence_measurability,
    'Can the ceremony-to-competence ratio be measured independently of the drill scores and inspection results it renders suspect?',
    'Linkage datasets pairing regional drill-completion metrics with blind-coded performance in real incidents and no-notice exercises; jurisdictions that experimented with unscripted formats serve as comparison cases.',
    'A near-zero correlation confirms the husk profile and pushes effective classification toward pure extraction sustained by enforcement; a partial correlation splits the arrangement into a competent core and a ceremonial shell as separate linked stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ceremony_competence_measurability, empirical, 'Whether the ratio underlying this reading is observable without circular reliance on the regime''s own metrics.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the force keeping unscripted, failure-tolerant training formats out of the exercise cycle structural (funding formulas, liability exposure, audit criteria) or internalized (practitioners sincerely equate staged success with duty)?',
    'Post-reform trajectory: if removing audit penalties for failed experimental exercises does not revive unscripted formats, a large internalized share is indicated.',
    'If internalized, effective suppression exceeds the structural measure and persists after any administrative fix; repair would require professional-culture change rather than rule changes alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized mechanism sustaining the exclusion of non-performative training formats.').

omega_variable(
    d5_scale_masking,
    'Does routine incident performance mask the rehearsal-performance gap until events approach national-flood scale?',
    'Stress-test modeling and comparative analysis of regions struck by mid-scale events: locate the severity threshold beyond which improvised absorption fails and rehearsed-only capacity collapses.',
    'If masking holds, harm to the resident and responder seats concentrates in rare high-severity events, raising the stakes-weighted cost of the gap; if the gap shows at small scale, degradation is broader and continuous.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(d5_scale_masking, empirical, 'Severity threshold at which the gap between rehearsed and live capacity becomes outcome-decisive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__husk_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_retention__husk_reading, theater_ratio, 0, 0.62).
narrative_ontology:measurement(prep_tr_t6, preparedness_retention__husk_reading, theater_ratio, 6, 0.66).
narrative_ontology:measurement(prep_tr_t12, preparedness_retention__husk_reading, theater_ratio, 12, 0.7).
narrative_ontology:measurement(prep_tr_t18, preparedness_retention__husk_reading, theater_ratio, 18, 0.73).
narrative_ontology:measurement(prep_tr_t24, preparedness_retention__husk_reading, theater_ratio, 24, 0.76).
narrative_ontology:measurement(prep_tr_t30, preparedness_retention__husk_reading, theater_ratio, 30, 0.78).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_retention__husk_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(prep_be_t6, preparedness_retention__husk_reading, base_extractiveness, 6, 0.6).
narrative_ontology:measurement(prep_be_t12, preparedness_retention__husk_reading, base_extractiveness, 12, 0.64).
narrative_ontology:measurement(prep_be_t18, preparedness_retention__husk_reading, base_extractiveness, 18, 0.68).
narrative_ontology:measurement(prep_be_t24, preparedness_retention__husk_reading, base_extractiveness, 24, 0.7).
narrative_ontology:measurement(prep_be_t30, preparedness_retention__husk_reading, base_extractiveness, 30, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_retention__husk_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(prep_su_t6, preparedness_retention__husk_reading, suppression_requirement, 6, 0.47).
narrative_ontology:measurement(prep_su_t12, preparedness_retention__husk_reading, suppression_requirement, 12, 0.51).
narrative_ontology:measurement(prep_su_t18, preparedness_retention__husk_reading, suppression_requirement, 18, 0.54).
narrative_ontology:measurement(prep_su_t24, preparedness_retention__husk_reading, suppression_requirement, 24, 0.56).
narrative_ontology:measurement(prep_su_t30, preparedness_retention__husk_reading, suppression_requirement, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_retention__husk_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_retention__husk_reading, preparedness_retention__competence_reading).
narrative_ontology:affects_constraint(preparedness_retention__husk_reading, preparedness_retention__hybrid_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'preparedness' decomposes into three readings of kernel preparedness_retention, partitioning a single empirical question: do drill hours convert to live competence (never / always / specialized-tier-only)? The readings are mutually exclusive within any single framework, so this maximal-denial reading carries foreclosure edges to both siblings; each sibling file carries its own epsilon, beneficiaries, and victims, and all three link via affects_constraints. Upstream-downstream structure: whichever reading accumulates confirming transfer evidence becomes the citation backbone the others must answer to.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
