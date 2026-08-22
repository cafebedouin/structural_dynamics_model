% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission__hybrid_embedded_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_transmission__hybrid_embedded_reading, []).

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
 *   constraint_id: catastrophe_memory_transmission__hybrid_embedded_reading
 *   human_readable: Catastrophe-Memory Ritual Fidelity — Hybrid Embedded Reading
 *   domain: religious_studies/collective_memory/ritual_studies
 *
 * SUMMARY:
 *   Communities that have survived recurrent catastrophe — seismic
 *   coastlines, floodplains, famine-prone valleys — maintain memorial and
 *   preparatory rites whose inherited form is treated here as the carrier of
 *   survival competence itself: evacuation timing rehearsed as procession,
 *   safe-siting limits recited as taboo, resource caching drilled as festival
 *   labor, mutual-aid choreography embedded in mourning sequence. The
 *   hybrid_embedded_reading holds that symbolic form and operational function
 *   are co-constitutive: alter the form and the function degrades, yet the
 *   function exists only insofar as the form is enacted. Ritual fidelity is
 *   therefore not decoration around a transferable lesson; it is the
 *   transmission channel for knowledge that has never been propositional. The
 *   claim/metric split is deliberate: the constraint is CLAIMED as rope
 *   (coordination through shared practice, participants as net beneficiaries)
 *   while the metrics are authored from the arrangement's observed operation,
 *   including its decay-and-revival cycle; the engine computes per-seat
 *   classifications from the structural data. A mountain substrate is noted
 *   but not claimed: the embodied character of the knowledge behaves like a
 *   physical limit on substitution, while the fidelity norm itself is a
 *   constructed, actively maintained practice.
 *
 * KEY AGENTS:
 *   - practicing_community_members: participant body ([organized]/[identity_locked]) — enacts the rites, holds the competence, bears rehearsal and behavioral costs while receiving the transmitted know-how
 *   - ritual_officiants_elders: transmission authority ([organized]/[identity_locked]) — teaches, corrects drift, leads revivals; collects deference and office continuity
 *   - descendant_generations: intended heirs ([powerless]/[trapped]) — absent from every deliberation that alters the form they will depend on
 *   - practice_modification_advocates: internal reformers ([moderate]/[constrained]) — propose vernacular, shortened, or documented variants; absorb correction and social cooling
 *   - disaster_ethnographers: analytical observers ([analytical]/[analytical]) — compare practiced, altered, and lapsed communities against recorded hazard outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__hybrid_embedded_reading, 0.26).
domain_priors:suppression_score(catastrophe_memory_transmission__hybrid_embedded_reading, 0.32).
domain_priors:theater_ratio(catastrophe_memory_transmission__hybrid_embedded_reading, 0.24).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, extractiveness, 0.26).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 0.24).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__hybrid_embedded_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__hybrid_embedded_reading, "Catastrophe-Memory Ritual Fidelity — Hybrid Embedded Reading").
narrative_ontology:topic_domain(catastrophe_memory_transmission__hybrid_embedded_reading, "religious_studies/collective_memory/ritual_studies").

domain_priors:requires_active_enforcement(catastrophe_memory_transmission__hybrid_embedded_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__hybrid_embedded_reading, '9d317424-796e-4a77-b1c5-117e5e1f7635').
narrative_ontology:cs_kernel_codification('9d317424-796e-4a77-b1c5-117e5e1f7635', implicit).
narrative_ontology:cs_authority_grounding('9d317424-796e-4a77-b1c5-117e5e1f7635', practice).
narrative_ontology:cs_interpretation_layer_present('9d317424-796e-4a77-b1c5-117e5e1f7635').
narrative_ontology:cs_reading_relation('9d317424-796e-4a77-b1c5-117e5e1f7635', catastrophe_memory_transmission__operational_competence_reading, influences).
narrative_ontology:cs_reading_relation('9d317424-796e-4a77-b1c5-117e5e1f7635', catastrophe_memory_transmission__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('9d317424-796e-4a77-b1c5-117e5e1f7635', foundational, form_function_inseparability).
narrative_ontology:cs_axiom_status(form_function_inseparability, holdable).
narrative_ontology:cs_axiom_grounding('9d317424-796e-4a77-b1c5-117e5e1f7635', form_function_inseparability, empirically_contingent).
narrative_ontology:cs_axiom('9d317424-796e-4a77-b1c5-117e5e1f7635', secondary, nonpropositional_transmission_necessity).
narrative_ontology:cs_axiom_status(nonpropositional_transmission_necessity, holdable).
narrative_ontology:cs_axiom_grounding('9d317424-796e-4a77-b1c5-117e5e1f7635', nonpropositional_transmission_necessity, empirically_contingent).
narrative_ontology:cs_reference_frame('9d317424-796e-4a77-b1c5-117e5e1f7635', embodied_form_function_unity).
narrative_ontology:cs_drift_state('9d317424-796e-4a77-b1c5-117e5e1f7635', contemporary, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('9d317424-796e-4a77-b1c5-117e5e1f7635', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__hybrid_embedded_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__hybrid_embedded_reading, practicing_community_members).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__hybrid_embedded_reading, descendant_generations).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__hybrid_embedded_reading, ritual_officiants_elders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__hybrid_embedded_reading, practicing_community_members).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__hybrid_embedded_reading, practice_modification_advocates).
narrative_ontology:constraint_vindicates(catastrophe_memory_transmission__hybrid_embedded_reading, embodied_knowledge_thesis).
narrative_ontology:constraint_vindicates(catastrophe_memory_transmission__hybrid_embedded_reading, ritual_fidelity_efficacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enact the memorial and preparatory rites on the communal calendar: fasts, processions along the old escape routes, siting limits recited as taboo at gatherings, seasonal caches restocked by rote. The rehearsal keeps evacuation timing, terrain reading, and mutual-aid choreography in their bodies. They give rehearsal hours and behavioral latitude and hold the competence the practice stores. Stepping away means losing both the community's recognition and the know-how itself, since nothing outside the practice carries it in usable form.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, practicing_community_members, beneficiary,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_transmission__hybrid_embedded_reading, practicing_community_members, payer).

% Teach the sequences, correct drifted performances, decide which variations are tolerable and which break transmission, and lead the periodic revivals that follow near-miss events or milestone anniversaries. Deference and continuity of office flow to them as stewards of fidelity. Their authority exists only inside the practice; abandoning it would dissolve their standing along with the transmission they oversee.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, ritual_officiants_elders, agenda_setter,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_transmission__hybrid_embedded_reading, ritual_officiants_elders, beneficiary).

% The unborn and youngest members for whom the practice is chiefly maintained. Every decision to abbreviate, translate, or retire a rite is taken before they can speak, yet they inherit whichever competence, or gap, those decisions leave behind. They cannot opt out of the hazard environment the practice prepares for, nor attend the assemblies where its form is debated.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, descendant_generations, excluded,
    powerless, generational, trapped, regional).

% Members who propose shortening rites, performing them in the vernacular, or replacing segments with documented drills and printed instructions. Proposals draw correction from the officiants and coolness from kin; a few have withdrawn from attendance after repeated rebuffs. They remain exposed to the same hazards and bound to the same calendar obligations while their proposals stay pending.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, practice_modification_advocates, payer,
    moderate, biographical, constrained, regional).

% Compare communities that kept, altered, or lapsed their memorial rites against recorded hazard outcomes, and document what seasoned performers can do that written accounts cannot reproduce. They publish outside the community, hold no office in it, and owe it no deference.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, disaster_ethnographers, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_transmission__hybrid_embedded_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_transmission__hybrid_embedded_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves intergenerational transmission of non-propositional survival knowledge: a fixed communal calendar compels rehearsal at intervals no individual would choose voluntarily, synchronizes mutual-aid choreography across the whole body, embeds threat cues and siting limits in place-bound ceremony, and makes maintenance of the knowledge self-enforcing through obligation rather than voluntary study.
% TRANSFER_FUNCTION: Moves rehearsal time and behavioral latitude from present members into a preserved competence stock held in common across generations; moves deference and office continuity to the officiants who steward fidelity. Nothing flows out of the community; the movement is internal and cumulative.
% ABSENT_VOICES: Descendant generations, the arrangement's principal intended beneficiaries, are absent from every assembly that debates altering, abbreviating, or retiring the rites; the dead whose catastrophe experience the form encodes have no representative; modification advocates speak but carry no agenda weight against officiant correction. Apparent unanimity around fidelity partly reflects who is alive and seated.
% DISAPPEARANCE_RATIONALE: Without the fidelity norm, performance becomes optional and abbreviates within a decade; embodied cues, siting taboos, and aid choreography thin over one to two generations; settlement creeps back into forbidden zones and seasonal caches lapse. The next recurrence finds a community holding memories of ceremonies but not the competence they carried — settlement patterns, preparedness habits, and the ceremonial calendar all rearrange around the loss.
% FOUNDING_PROBLEM: Recurrent catastrophe killed or ruined the community at intervals longer than any individual's working memory; each event's warning signs, safe sites, and survival procedures were bought at lethal cost and would vanish with the cohort that paid for them unless fixed in a form the untested could be made to repeat.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: geological and archival hazard reconstructions establish recurrence intervals exceeding generational memory, and comparative ethnography with disaster epidemiology attests differential outcomes between communities that maintained, altered, or lapsed their transmission practice. The community's own testimony of the founding events is treated as participating, not corroborating.
narrative_ontology:disappearance_verdict(catastrophe_memory_transmission__hybrid_embedded_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_transmission__hybrid_embedded_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__hybrid_embedded_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_transmission__hybrid_embedded_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_transmission__hybrid_embedded_reading, 0.26, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_transmission__hybrid_embedded_reading_tests).
:- end_tests(catastrophe_memory_transmission__hybrid_embedded_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-to-moderate (0.26 at interval end) because the arrangement's costs — rehearsal hours, behavioral restraint, foreclosed simplification — are borne overwhelmingly by participants who are simultaneously its beneficiaries, and no seat converts the flow into private gain; officiant deference is real but small beside the distributed competence stock. Suppression (0.32) reflects correction of drifted performance and pressure on modification advocates rather than any coercive apparatus; it is authored as a raw structural property and is deliberately left unscaled here — the engine owns the directionality and scope arithmetic. Theater (0.24) is structurally capped by the reading's own thesis: performance IS the functional substrate, so pure theater would be self-refuting; what theater exists accumulates during long calm interludes when enactment continues after living memory of the founding events has faded. Accessibility collapse (0.62) sits between rope and mountain territory: under this reading, written manuals and standalone drills degrade the content, but video and simulation partially substitute, so alternatives narrow without vanishing. Resistance (0.28) is episodic reform pressure, repeatedly absorbed. The measurement series run on one shared nine-point grid and display one full decay-revival cycle: after a near-miss event or milestone anniversary, enforcement tightens and theater drops as enactment reconnects to salient function; across decades of calm, enforcement relaxes, performance abbreviates, and theater and cost-per-competence rise together until the next revival resets them. The oscillation is not an extraction mechanism — it is the transmission system's maintenance rhythm, though officiants occasionally extend revivals past functional need, which the capture omega tracks.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. From the officiants' position the arrangement is a sacred trust they steward and the source of their standing; from the practitioners' position it is obligation shot through with periodically renewed meaning; from the modification advocates' position it is conservatism that punishes improvement while hazards evolve; from the descendant generations' position — a seat that cannot speak — fidelity is the difference between inheriting competence and inheriting exposure. Same community, same rite, four different lived structures. The engine computes this divergence from power, exit, and role data; the authored rope claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations drive the derivation: practicing_community_members are declared beneficiaries with a payer secondary — net recipients of competence who also supply the rehearsal effort — placing them near the beneficiary end with a modest upward pull. ritual_officiants_elders administer the practice and collect deference, sitting nearest the beneficiary pole. descendant_generations are the deepest beneficiaries (the entire arrangement transmits for them) but hold no seat and no exit; their directionality approaches the full-beneficiary pole while their influence on the arrangement is nil, which is precisely the absent-voice asymmetry the six questions record. practice_modification_advocates bear sanction and discount the fidelity benefit, placing them highest on the target axis among community seats, though short of victimhood — no one's welfare is systematically transferred away, which is why no victims are declared. Gains are authored as diffuse: the extracted effort converts into a distributed competence stock rather than accruing to any named seat; officiant deference was checked against the receipt surface and judged incidental to the main flow. Regional scope modestly amplifies effective extraction through verification difficulty, per the engine's scaling.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — hazard recurrence outlasting institutional memory — remains live wherever return intervals exceed a generation, so the mandate has not outlived its function and mandatrophy is not resolved. The arrangement carries its own anti-atrophy machinery: the revival cycle re-couples performance to function before drift completes, which is why theater peaks are bounded rather than secular. The failure mode to watch is lengthening return intervals: if no near-miss arrives within the tolerance of living memory, revivals lose their trigger, theater climbs without reset, and the arrangement slides toward inertial persistence — a trajectory this story's temporal series is designed to detect. Classifying as rope also prevents the opposite error: reading the fidelity demand as pure extraction would miss that its costs are the coordination itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'This story instantiates the hybrid_embedded_reading of the catastrophe_memory_transmission kernel: is ritual form truly inseparable from the survival competence it transmits, or could the operational_competence_reading''s stripped-down drills or the symbol_continuity_reading''s form-only preservation each capture what fidelity captures?',
    'Comparative longitudinal ethnography of communities that altered form versus maintained fidelity, tracking measured survival-relevant behavior (evacuation latency, siting compliance, cache maintenance) across generations.',
    'If form proves separable, this constraint decomposes into a transmission channel plus a removable stylistic shell and the fidelity norm loses its necessity defense; if inseparable, the fidelity requirement is load-bearing and both sibling readings under-describe the mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Which reading of the catastrophe-memory kernel correctly locates the transmission mechanism.').

omega_variable(
    embodied_substrate_reality,
    'Is the non-propositional transmission claim anchored in real cognitive mechanisms (motor rehearsal, situational cueing, place-based memory) or is it retrospective valorization of customary practice?',
    'Controlled comparison of hazard-recognition and response performance between long-term ritual participants, counterparts trained from manuals and drills, and untrained residents.',
    'Real mechanisms give the arrangement a physical anchor approaching a natural limit and raise effective accessibility collapse; absent mechanisms, the fidelity norm is conventional and revisable at much lower cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(embodied_substrate_reality, empirical, 'Whether the mountain-like substrate attributed to embodied knowledge is physically real.').

omega_variable(
    substitution_channel_viability,
    'How much of the encoded competence can modern channels — video documentation, scheduled drills, printed siting rules — actually carry, and how much decays without enacted communal form?',
    'Field trials introducing documentation-and-drill programs alongside or in place of the rites, with multi-year retention and response testing against ritual-trained cohorts.',
    'High viability lowers accessibility collapse and makes the fidelity norm one option among substitutes; low viability confirms the collapse score and the scarcity of exits from the practice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(substitution_channel_viability, empirical, 'Degree to which propositional and recorded substitutes can replace enacted transmission.').

omega_variable(
    officiant_capture_risk,
    'Do officiant lineages convert custody of fidelity into durable status and office rents sufficient to constitute a capturing seat?',
    'Track deference, office succession, and material advantage across successive revival cycles; test whether enforcement intensity correlates with officiant-lineage benefit rather than with community competence indicators.',
    'Confirmed capture would add an extracting seat and push the arrangement toward a hybrid coordination-extraction profile; continued absence keeps gains diffuse among participants and heirs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(officiant_capture_risk, empirical, 'Whether stewardship of fidelity is drifting toward private status rent.').

omega_variable(
    internalized_adherence_balance,
    'Is member adherence to fidelity carried mainly by internalized formation (habituated embodiment, early training) or by external correction and sanction?',
    'Observe adherence trajectories in cohorts formed before versus after enforcement relaxed, and in members residing beyond the reach of communal correction.',
    'Internalized dominance means the measured suppression understates the binding force and exit is costlier than it appears; external dominance means relaxation would rapidly erode fidelity and the suppression scalar is the operative number.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_adherence_balance, empirical, 'Split between internalized and externally enforced adherence mechanisms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__hybrid_embedded_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(catmem_hybrid_embedded_tr_t0, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 0, 0.13).
narrative_ontology:measurement(catmem_hybrid_embedded_tr_t7, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 7, 0.16).
narrative_ontology:measurement(catmem_hybrid_embedded_tr_t15, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(catmem_hybrid_embedded_tr_t22, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 22, 0.29).
narrative_ontology:measurement(catmem_hybrid_embedded_tr_t30, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 30, 0.34).
narrative_ontology:measurement(catmem_hybrid_embedded_tr_t37, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 37, 0.26).
narrative_ontology:measurement(catmem_hybrid_embedded_tr_t45, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 45, 0.15).
narrative_ontology:measurement(catmem_hybrid_embedded_tr_t52, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 52, 0.19).
narrative_ontology:measurement(catmem_hybrid_embedded_tr_t60, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 60, 0.24).

% Extraction over time
narrative_ontology:measurement(catmem_hybrid_embedded_be_t0, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 0, 0.21).
narrative_ontology:measurement(catmem_hybrid_embedded_be_t7, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 7, 0.23).
narrative_ontology:measurement(catmem_hybrid_embedded_be_t15, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 15, 0.27).
narrative_ontology:measurement(catmem_hybrid_embedded_be_t22, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 22, 0.31).
narrative_ontology:measurement(catmem_hybrid_embedded_be_t30, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 30, 0.33).
narrative_ontology:measurement(catmem_hybrid_embedded_be_t37, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 37, 0.28).
narrative_ontology:measurement(catmem_hybrid_embedded_be_t45, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 45, 0.22).
narrative_ontology:measurement(catmem_hybrid_embedded_be_t52, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 52, 0.24).
narrative_ontology:measurement(catmem_hybrid_embedded_be_t60, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 60, 0.26).

% Suppression requirement over time
narrative_ontology:measurement(catmem_hybrid_embedded_su_t0, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 0, 0.44).
narrative_ontology:measurement(catmem_hybrid_embedded_su_t7, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 7, 0.39).
narrative_ontology:measurement(catmem_hybrid_embedded_su_t15, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 15, 0.33).
narrative_ontology:measurement(catmem_hybrid_embedded_su_t22, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 22, 0.29).
narrative_ontology:measurement(catmem_hybrid_embedded_su_t30, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 30, 0.27).
narrative_ontology:measurement(catmem_hybrid_embedded_su_t37, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 37, 0.41).
narrative_ontology:measurement(catmem_hybrid_embedded_su_t45, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 45, 0.38).
narrative_ontology:measurement(catmem_hybrid_embedded_su_t52, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 52, 0.35).
narrative_ontology:measurement(catmem_hybrid_embedded_su_t60, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 60, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission__hybrid_embedded_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__hybrid_embedded_reading, catastrophe_memory_transmission__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__hybrid_embedded_reading, catastrophe_memory_transmission__operational_competence_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'catastrophe memory rituals' conflates three structurally distinct claims, decomposed per the epsilon-invariance principle into a constraint family: symbol_continuity_reading (form as intrinsic identity and mourning good), operational_competence_reading (ritual as rehearsable skill package), and this file (form and competence co-constitutive, fidelity the transmission channel). Each carries its own epsilon, beneficiary structure, and classification. This reading sits downstream of the other two in argument structure: it asserts what each sibling separately denies or omits — that neither form alone nor extracted drills suffice — and cites both as the positions its inseparability thesis answers. Coordination type is authored as identity_coordination rather than information_standard despite the information-carrier framing, because under this reading the standard is not separable from the practicing body that carries it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
