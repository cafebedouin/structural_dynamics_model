% ============================================================================
% CONSTRAINT STORY: preparedness_transmission__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_transmission__hybrid_reading, []).

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
 *   constraint_id: preparedness_transmission__hybrid_reading
 *   human_readable: Stratified Preparedness Transmission (Hybrid Reading): Physical Layer Competent, Civilian Coordination Channel Decayed
 *   domain: disaster risk management / institutional memory / civil defense
 *
 * SUMMARY:
 *   A national preparedness-transmission apparatus — mandated drills,
 *   standardized exercises, compliance documentation, grant conditionality —
 *   was built to move survival-critical coordination knowledge across
 *   generations. This story instantiates the HYBRID READING of that contested
 *   kernel: transmission is stratified. The physical-infrastructure channel
 *   works — engineering competence re-validates itself through daily paid
 *   practice, licensure, and apprenticeship, and infrastructure performs as
 *   designed under stress. The civilian coordination channel has decayed:
 *   drills continue at growing cost, but they transmit script adherence, not
 *   judgment; residents retain almost nothing transferable; mutual-aid
 *   carriers of the old knowledge were absorbed nominally and defunded
 *   actually; when events arrive, evacuation and coordination fail in exactly
 *   the ways the evaluated exercises said were covered. The capability break
 *   (the manifest's D5 delta) sits in the coordination layer, not the
 *   physical layer. EPSILON REFERENT: the standing arrangement under contest
 *   — the transmission apparatus itself (its mandates, exercises, doctrine,
 *   and funding conditionality) — assessed by this reading's own lights;
 *   neither the fully-working system the competence_reading would affirm nor
 *   the wholly-hollow shell the husk_reading would indict. Claimed type and
 *   metrics are authored independently: the arrangement is CLAIMED
 *   tangled_rope (real residual coordination function on both channels plus
 *   asymmetric extraction with active enforcement), while the metrics
 *   describe the stratified reality — moderate-high extraction concentrated
 *   in the decayed civilian channel, majority-theatrical civilian-layer
 *   activity, alternatives crowded out rather than forbidden. Family: this
 *   story links its two sibling readings via network edges; each sibling is a
 *   separate constraint with its own epsilon.
 *
 * KEY AGENTS:
 *   - - emergency_management_agencies: Agenda setter (institutional/constrained) — administers doctrine, audits compliance, captures the accumulating budget share
 *   - - exercise_industry_vendors: Beneficiary (organized/arbitrage) — sells compliance machinery, exits any single market freely
 *   - - elected_officials: Beneficiary (powerful/arbitrage) — harvests visible preparedness, diffuses accountability across terms
 *   - - engineering_professions: Legitimate-function beneficiary (organized/mobile) — the working half; competence transmits through practice, not ritual
 *   - - civilian_residents: Primary payer (powerless/constrained) — surrender time and receive false assurance; improvise when events test the channel
 *   - - neighborhood_mutual_aid_networks: Payer (moderate/constrained) — displaced carriers of civilian coordination, marginalized between events
 *   - - small_municipalities: Payer (moderate/constrained) — bear unfunded mandate burden with handful-sized staffs
 *   - - mobility_limited_residents: Excluded voice (powerless/trapped) — assumed away by scenario design; appear in records as fatalities
 *   - - disaster_research_community: Analytical observer (analytical/analytical) — documents the stratified decay that the apparatus's own metrics cannot see
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__hybrid_reading, 0.61).
domain_priors:suppression_score(preparedness_transmission__hybrid_reading, 0.45).
domain_priors:theater_ratio(preparedness_transmission__hybrid_reading, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, theater_ratio, 0.64).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_transmission__hybrid_reading, "Stratified Preparedness Transmission (Hybrid Reading): Physical Layer Competent, Civilian Coordination Channel Decayed").
narrative_ontology:topic_domain(preparedness_transmission__hybrid_reading, "disaster risk management / institutional memory / civil defense").

domain_priors:requires_active_enforcement(preparedness_transmission__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__hybrid_reading, 'a68b9151-ee64-4dcc-a49f-21668bad872e').
narrative_ontology:cs_kernel_codification('a68b9151-ee64-4dcc-a49f-21668bad872e', formalized).
narrative_ontology:cs_authority_grounding('a68b9151-ee64-4dcc-a49f-21668bad872e', extraction).
narrative_ontology:cs_interpretation_layer_present('a68b9151-ee64-4dcc-a49f-21668bad872e').
narrative_ontology:cs_reading_relation('a68b9151-ee64-4dcc-a49f-21668bad872e', preparedness_transmission__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('a68b9151-ee64-4dcc-a49f-21668bad872e', preparedness_transmission__husk_reading, coexists_with).
narrative_ontology:cs_axiom('a68b9151-ee64-4dcc-a49f-21668bad872e', foundational, layer_specific_transmission_dynamics).
narrative_ontology:cs_axiom_status(layer_specific_transmission_dynamics, holdable).
narrative_ontology:cs_axiom_grounding('a68b9151-ee64-4dcc-a49f-21668bad872e', layer_specific_transmission_dynamics, empirically_contingent).
narrative_ontology:cs_axiom('a68b9151-ee64-4dcc-a49f-21668bad872e', secondary, practiced_competence_persists_unpracticed_decays).
narrative_ontology:cs_axiom_status(practiced_competence_persists_unpracticed_decays, holdable).
narrative_ontology:cs_axiom_grounding('a68b9151-ee64-4dcc-a49f-21668bad872e', practiced_competence_persists_unpracticed_decays, empirically_contingent).
narrative_ontology:cs_reference_frame('a68b9151-ee64-4dcc-a49f-21668bad872e', dual_channel_transmission_baseline).
narrative_ontology:cs_drift_state('a68b9151-ee64-4dcc-a49f-21668bad872e', contemporary_all_hazards_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a68b9151-ee64-4dcc-a49f-21668bad872e', '').
narrative_ontology:cs_kernel_id(preparedness_transmission__hybrid_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_transmission__hybrid_reading, emergency_management_agencies).
narrative_ontology:constraint_beneficiary(preparedness_transmission__hybrid_reading, exercise_industry_vendors).
narrative_ontology:constraint_beneficiary(preparedness_transmission__hybrid_reading, elected_officials).
narrative_ontology:constraint_beneficiary(preparedness_transmission__hybrid_reading, engineering_professions).
narrative_ontology:constraint_victim(preparedness_transmission__hybrid_reading, civilian_residents).
narrative_ontology:constraint_victim(preparedness_transmission__hybrid_reading, neighborhood_mutual_aid_networks).
narrative_ontology:constraint_victim(preparedness_transmission__hybrid_reading, small_municipalities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(preparedness_transmission__hybrid_reading, mobility_limited_residents).
narrative_ontology:constraint_vindicates(preparedness_transmission__hybrid_reading, professional_practice_preserves_technical_competence).
narrative_ontology:constraint_vindicates(preparedness_transmission__hybrid_reading, compliance_metrics_substitute_for_capability_under_scarcity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the national preparedness doctrine: set exercise standards, audit local compliance, and distribute preparedness grants conditioned on completed exercises. Career ladders run through exercise program management. When real events expose coordination gaps, the standard response is expanded documentation and revised templates rather than resumed civilian training; budgets and headcount grow with each mandate cycle regardless.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, emergency_management_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Sell scenario design, simulation software, evaluator certification, and compliance documentation to agencies and municipalities. Revenue scales with mandate complexity — every new reporting requirement creates billable engagements. Clients span government, hospitals, and campuses, so a shock in one procurement stream can be offset in another.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, exercise_industry_vendors, beneficiary,
    organized, biographical, arbitrage, national).

% Appear at large visible exercises, open new emergency operations centers, and issue reassurance statements during hazard season. The visibility converts directly into campaign material. Accountability for coordination failures typically arrives years later, after term limits or reassignment, diffused across many former officeholders.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, elected_officials, beneficiary,
    powerful, immediate, arbitrage, national).

% Design, inspect, and maintain the physical layer: levees, standby generators, seismic retrofits, water systems. Competence transmits through daily practice on live projects, licensure examinations, and supervised apprenticeship — each cohort re-validates capability by doing the work under experienced eyes. Maintenance capital survives austerity cycles better than social programming, keeping the practice loop funded.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, engineering_professions, beneficiary,
    organized, generational, mobile, global).

% Attend the drills scheduled by employers and schools, which rehearse building-specific scripts rather than generalizable judgment. Most cannot name their assembly point a week later, do not know which neighbors need help evacuating, and have never practiced a decision under time pressure. Between events, messaging asks them for awareness rather than skill; during events, they improvise with whatever social ties remain.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, civilian_residents, payer,
    powerless, biographical, constrained, local).

% Descended from block wardens, congregation networks, and volunteer auxiliaries that once carried civilian coordination. Professional agencies absorbed their nominal functions while grant eligibility rules favored credentialed providers, leaving them marginal between events. They reassemble spontaneously when floods or fires hit — the knowledge persists in pockets — but lack funding, rosters, or official recognition that would sustain continuity.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, neighborhood_mutual_aid_networks, payer,
    moderate, biographical, constrained, local).

% Must file compliance documentation, host evaluated exercises, and adopt doctrine templates with administrative staffs of a handful. Compliance paperwork consumes the liaison time that once built working relationships with schools, congregations, and employers. Declining the grants is possible but leaves the municipality exposed in an audit and politically vulnerable.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, small_municipalities, payer,
    moderate, generational, constrained, regional).

% Absent from exercise design: scenarios assume private vehicles, stair descent, and able bodies. Registries of people needing evacuation assistance are outdated or nonexistent. They would object that the drills rehearse an evacuation that does not include them; they enter the record chiefly as fatality counts in after-action reports.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, mobility_limited_residents, excluded,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__hybrid_reading, mobility_limited_residents, payer).

% Studies drill efficacy, evacuation behavior, and organizational memory across events and countries. Repeatedly documents the split finding: inspection-and-remediation regimes for physical assets show high closure rates, while civilian drill evaluations show script compliance without knowledge retention. Findings circulate through journals and commission testimony but rarely alter mandate design.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, disaster_research_community, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_transmission__hybrid_reading, emergency_management_agencies).
narrative_ontology:fixing_cost_class(preparedness_transmission__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves real coordination problems on two channels: the physical channel maintains inspection-and-remediation schedules for critical infrastructure and keeps interagency command language interoperable; the civilian channel nominally organizes mass rehearsal of evacuation and shelter behavior so populations act coherently without ad hoc instruction during events.
% TRANSFER_FUNCTION: Moves preparedness grant money downward from national programs to states and municipalities, and onward to certified exercise vendors; moves resident and municipal staff time into drill participation and compliance documentation; moves assurance upward — officials acquire demonstrable readiness performances, institutions acquire liability cover, residents acquire confidence that coordination is handled.
% ABSENT_VOICES: Mobility-limited residents, renters, shift workers, and undocumented residents are absent from scenario design and after-action panels — they surface as casualty lines in reports rather than seated voices. Veteran organizers of the displaced mutual-aid networks are likewise outside the room: grant eligibility favors credentialed providers, so the people who once ran civilian coordination are not consulted about its decay.
% DISAPPEARANCE_RATIONALE: Grant flows, compliance scaffolding, and the interagency command framework would lapse, and the surrounding exercise economy — vendors, evaluators, certifications — would dissolve within a budget cycle. Crucially, the two channels part company upon disappearance: bridge inspections and generator maintenance would continue essentially unchanged, because that channel runs on professional practice rather than this apparatus, while civilian coordination would fall back to improvisation and whatever mutual-aid remnants reassemble. The asymmetric aftermath is itself the strongest evidence for the stratified structure this reading asserts.
% FOUNDING_PROBLEM: Mid-century civil defense confronted a population that had lost village-scale survival folk knowledge: how to shelter, evacuate, and coordinate neighbors under sudden attack or disaster without waiting for instructions. The arrangement was built to transmit that coordination knowledge to civilians at scale, running parallel to professional training for responders and engineers.
% FOUNDING_PROBLEM_CORROBORATION: No corroborator from outside the benefiting parties attests that the founding problem remains live in its original form. External attestors of the transformation exist and disagree among themselves: independent and legislative after-action commissions attest that coordination-knowledge transmission has decayed while hazards grew; disaster-sociology studies attest drill participation without retention; agency testimony attests the problem is solved by professionalization and now lives in responder training alone. The disagreement among external witnesses is itself what marks the status as contested rather than settled.
narrative_ontology:disappearance_verdict(preparedness_transmission__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_transmission__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_transmission__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_transmission__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_transmission__hybrid_reading, 0.61, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_transmission__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_transmission__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_transmission__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored 0.61: high on the civilian channel, where participation time, municipal staff effort, and grant-funded attention are extracted and returned as compliance artifacts rather than capability; low on the physical channel, where the same apparatus family delivers inspected, maintained, performing infrastructure. Suppression 0.45 is structural and unscaled: participation is compelled softly (employer/school mandates, grant conditionality that punishes noncompliant municipalities), not prohibited alternatives — mutual aid is not banned, it is starved. Theater ratio 0.64: the majority of civilian-channel activity is evaluated performance — scenarios written to pass, evaluators certifying adherence — while the physical channel's inspection activity is overwhelmingly functional, blending to a majority-theatrical profile. Accessibility collapse 0.40 is deliberately below snare range: alternatives (self-organized drills, community response networks, household planning) remain legally available and demonstrably resurgent when events force them; the apparatus crowds out rather than forecloses. Resistance 0.45: post-event criticism, academic findings, and journalistic investigation recur after each visible failure, then attenuate between events; no sustained opposition movement targets the apparatus itself. MEASUREMENTS: one shared grid (T=0..60 at intervals of 12) across all three tracked metrics; the extractiveness and theater series rise monotonically with the shift from genuine mid-century mass training to the modern compliance-vendor economy (Goodhart drift — the evaluated proxy replaced the transmitted substance). The suppression_requirement series is deliberately NON-monotonic because the enforcement MACHINERY changed form twice — direct civil-defense mandates (high), post-détente relaxation (declining), then post-2001 fiscal-conditionality ratcheting via grant eligibility (rising again, then settling) — intensity roughly conserved while the lever moved from statute to budget; this is a real enforcement-capacity history, which is why the series is authored rather than left to the static scalar. No cyclical oscillation is modeled: decay is monotone, punctuated by grant-regime steps, not by tension-relapse cycles.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently by construction. From the agenda_setter seat, the apparatus is a functioning professional system: dashboards green, exercises complete, audits passed — the competence_reading looks true from inside. From the civilian payer seats, the same apparatus delivers attendance sheets and false assurance: the husk_reading looks true from the kitchen table. From the engineering seats, the physical channel is simply work that gets done and checked — neither pole describes their experience. From the analytical seat, both poles are locally accurate and globally wrong: the truth is stratified. Per-seat classification divergence is the expected output of this structural asymmetry; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: agencies (d near the subsidized end — they receive the mandate growth), vendors (fee streams keyed to mandate complexity), and officials (visibility rents) all sit near the beneficiary pole. Engineering_professions are also beneficiaries, and correctly so — the constraint genuinely subsidizes sustained infrastructure practice; their low d anchors the working half of the stratification rather than contaminating the extraction picture, because what flows to them is payment for delivered function. Payees drive high directionality: residents (time in, false assurance out, casualty risk retained), mutual-aid networks (function absorbed, capacity unfunded), small municipalities (compliance labor extracted under grant duress). Receipt concentration: the extraction's gains demonstrably accrue to the agency seat — grant funds pool there, convert into staffing and mandate growth, and only then pass outward to vendors as transactions; hence gain_flow names emergency_management_agencies rather than 'diffuse'. Fixing cost is authored independently: reviving civilian transmission would require rebuilding trusted neighborhood institutions, recurring realistic resident-inclusive exercises, and registry maintenance for the populations plans ignore — decades of politically invisible work against a benefit that arrives at the next disaster, which is why fixing_cost reads prohibitive despite periodic post-event recommendation surges.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mandate — transmit civilian coordination knowledge at mass scale — has atrophied into a compliance-documentation economy: the apparatus persists by performing transmission rather than doing it, which is mandate-atrophy localized to the civilian channel. The hybrid framing is what prevents both misclassifications: read through the competence pole alone, the apparatus looks like a healthy rope (and the decay stays invisible behind green compliance metrics); read through the husk pole alone, it looks like pure ritual (and the genuinely working physical channel gets condemned with the dead one). The tangled_rope claim preserves both facts — residual real coordination function plus asymmetric extraction under active enforcement — and locates the rot precisely. R5 interaction: founding_problem_status is contested (external witnesses disagree about whether the original problem died with the attack scenarios or mutated into the all-hazards gap) and disappearance_verdict is world_rearranges (grant flows and interoperability genuinely depend on the apparatus), so the dead-mandate-plus-persistent-world signature does NOT fire automatically — the mandatrophy here is channel-specific and must be read from the stratification itself, not from the status-times-verdict mismatch alone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decay_locus_ambiguity,
    'Does capability decay split along the physical/civilian layer boundary as this hybrid reading asserts, or along some other axis such as jurisdictional wealth, hazard type, or urban/rural divide?',
    'Cross-jurisdiction comparison pairing physical-inspection closure rates with civilian evacuation outcomes for comparable hazard events; if decay tracks jurisdiction capacity rather than layer, the hybrid reading mislocates the capability break.',
    'If the break follows jurisdiction wealth rather than layer, this reading collapses toward the husk pole for poor jurisdictions and the competence pole for wealthy ones, shifting beneficiary/victim structure and the distribution of effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decay_locus_ambiguity, conceptual, 'Whether the stratified-decay boundary is the layer boundary this reading draws.').

omega_variable(
    false_assurance_suppression,
    'Does the apparatus''s visible preparedness performance actively suppress organic civilian coordination (false assurance crowding out self-organization), or does it merely fail to build capability?',
    'Compare spontaneous mutual-aid formation rates and household preparedness behavior in jurisdictions with heavy official exercise regimes versus light ones, controlling for hazard exposure and demographics.',
    'If false assurance is operative, civilian-layer suppression exceeds the authored scalar and the coordination channel is net-negative rather than merely neglected; if inert, decay reflects abandonment rather than crowding-out.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(false_assurance_suppression, empirical, 'Crowding-out versus neglect as the decay mechanism on the civilian channel.').

omega_variable(
    evaluation_metric_validity,
    'Do standardized exercise evaluations measure transferable coordination capability or script adherence?',
    'Correlate exercise evaluation scores with independently assessed response outcomes in subsequent real events for the same jurisdictions.',
    'If scores track outcomes, the authored theater ratio overstates ritual content; if uncorrelated, theater is understated relative to true incapacity and the extraction picture worsens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(evaluation_metric_validity, empirical, 'Validity of the compliance metrics the transmission apparatus runs on.').

omega_variable(
    recoverability_pathway,
    'Can the existing mandate apparatus revive civilian coordination transmission, or does recovery require new social formation outside it?',
    'Track jurisdictions that redirected exercise budgets toward sustained community organizing and recurring realistic resident-inclusive drills; observe whether measurable coordination capability returns within a decade.',
    'If recovery is achievable inside the apparatus, the arrangement admits reform while keeping its tangled structure; if not, the decayed channel warrants replacement by a declared transitional arrangement with an explicit sunset, changing the appropriate remedy class.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recoverability_pathway, conceptual, 'Reform-versus-replace question for the decayed civilian channel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__hybrid_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_transmission__hybrid_reading, theater_ratio, 0, 0.26).
narrative_ontology:measurement_basis(prep_tr_t0, observed).
narrative_ontology:measurement(prep_tr_t12, preparedness_transmission__hybrid_reading, theater_ratio, 12, 0.37).
narrative_ontology:measurement_basis(prep_tr_t12, observed).
narrative_ontology:measurement(prep_tr_t24, preparedness_transmission__hybrid_reading, theater_ratio, 24, 0.46).
narrative_ontology:measurement_basis(prep_tr_t24, observed).
narrative_ontology:measurement(prep_tr_t36, preparedness_transmission__hybrid_reading, theater_ratio, 36, 0.54).
narrative_ontology:measurement_basis(prep_tr_t36, observed).
narrative_ontology:measurement(prep_tr_t48, preparedness_transmission__hybrid_reading, theater_ratio, 48, 0.6).
narrative_ontology:measurement_basis(prep_tr_t48, observed).
narrative_ontology:measurement(prep_tr_t60, preparedness_transmission__hybrid_reading, theater_ratio, 60, 0.64).
narrative_ontology:measurement_basis(prep_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_transmission__hybrid_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement_basis(prep_be_t0, observed).
narrative_ontology:measurement(prep_be_t12, preparedness_transmission__hybrid_reading, base_extractiveness, 12, 0.41).
narrative_ontology:measurement_basis(prep_be_t12, observed).
narrative_ontology:measurement(prep_be_t24, preparedness_transmission__hybrid_reading, base_extractiveness, 24, 0.47).
narrative_ontology:measurement_basis(prep_be_t24, observed).
narrative_ontology:measurement(prep_be_t36, preparedness_transmission__hybrid_reading, base_extractiveness, 36, 0.52).
narrative_ontology:measurement_basis(prep_be_t36, observed).
narrative_ontology:measurement(prep_be_t48, preparedness_transmission__hybrid_reading, base_extractiveness, 48, 0.57).
narrative_ontology:measurement_basis(prep_be_t48, observed).
narrative_ontology:measurement(prep_be_t60, preparedness_transmission__hybrid_reading, base_extractiveness, 60, 0.61).
narrative_ontology:measurement_basis(prep_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_transmission__hybrid_reading, suppression_requirement, 0, 0.56).
narrative_ontology:measurement_basis(prep_su_t0, observed).
narrative_ontology:measurement(prep_su_t12, preparedness_transmission__hybrid_reading, suppression_requirement, 12, 0.49).
narrative_ontology:measurement_basis(prep_su_t12, observed).
narrative_ontology:measurement(prep_su_t24, preparedness_transmission__hybrid_reading, suppression_requirement, 24, 0.44).
narrative_ontology:measurement_basis(prep_su_t24, observed).
narrative_ontology:measurement(prep_su_t36, preparedness_transmission__hybrid_reading, suppression_requirement, 36, 0.52).
narrative_ontology:measurement_basis(prep_su_t36, observed).
narrative_ontology:measurement(prep_su_t48, preparedness_transmission__hybrid_reading, suppression_requirement, 48, 0.49).
narrative_ontology:measurement_basis(prep_su_t48, observed).
narrative_ontology:measurement(prep_su_t60, preparedness_transmission__hybrid_reading, suppression_requirement, 60, 0.45).
narrative_ontology:measurement_basis(prep_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission__hybrid_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_transmission__hybrid_reading, preparedness_transmission__competence_reading).
narrative_ontology:affects_constraint(preparedness_transmission__hybrid_reading, preparedness_transmission__husk_reading).

% DUAL FORMULATION NOTE:
% Constraint family for the contested kernel 'preparedness transmission'. One colloquial label ('does preparedness transmission still work?') decomposes into three structurally distinct claims, each a separate file with its own epsilon over the SAME referent (the standing drills-inspections-doctrine apparatus): competence_reading (low epsilon — each generation re-validates capability through practice; authoritative seat: agencies and professional bodies), husk_reading (high epsilon — memorial ritual over hollowed operational memory; authoritative seat: critical sociology and investigative journalism), and this hybrid_reading (intermediate epsilon 0.61 — stratified: the physical channel transmits through paid daily practice while the civilian channel decayed into evaluated compliance). The siblings supply the poles the hybrid mediates between: competence_reading carries the upstream evidence for the working layer (inspection closure rates, licensure pass-through), husk_reading carries the civilian-layer evidence (retention studies, after-action improvisation findings). No reading hedges across the others; the hybrid cites both poles as evidence for its stratification verdict.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
