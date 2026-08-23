% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__survival_competence_reading, []).

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
 *   constraint_id: catastrophe_memory_kernel__survival_competence_reading
 *   human_readable: Mourning-Practice Regime as Persecution-Survival Training
 *   domain: religious/collective_memory
 *
 * SUMMARY:
 *   A persecuted minority's mourning-practice regime — fixed fast days,
 *   lamentation cycles, commemorative rehearsals of destruction and escape,
 *   and the mutual-aid obligations attached to them — is read here under one
 *   specific claim: that the regime encodes and transmits adaptive capacity
 *   for surviving persecution. On this reading the calendar is a training
 *   system: it rehearses warning-interpretation, flight logistics,
 *   portable-asset discipline, and mutual-aid activation often enough, and
 *   binds them tightly enough to obligation, that the competence survives the
 *   generations-long gaps between catastrophe episodes. The claim and the
 *   metrics are independent authored facts: the story CLAIMS a hybrid
 *   coordination/extraction structure (a genuine survival-training function
 *   operating through the same machinery that imposes real
 *   boundary-maintenance costs on identifiable members), while the metrics
 *   describe moderate extraction with a pronounced cyclical dynamic. Per the
 *   epsilon-invariance principle this file decomposes one colloquial label
 *   ('catastrophe-memory ritual') and authors only this reading; the three
 *   sibling readings are separate constraints linked through the network
 *   block. KEY AGENTS (by structural relationship): -
 *   ritual_authority_council: Agenda-setting administrator
 *   ([institutional]/[identity_locked]) — curates the canon, enforces
 *   observance, accrues interpretive authority -
 *   persecution_exposed_community: Primary collective beneficiary
 *   ([organized]/[constrained]) — holds the pooled survival capacities -
 *   diaspora_survivor_households: Household-scale beneficiaries
 *   ([moderate]/[constrained]) — draw on competence in crisis, carry daily
 *   distinctiveness friction - assimilation_pressured_members: Primary paying
 *   seat ([moderate]/[identity_locked]) — bear the standing charge on
 *   integration - successor_generations: Paying seat with conditional benefit
 *   ([powerless]/[trapped]) — inherit the full obligation load before any
 *   current threat - secularized_descendants: Excluded voice
 *   ([moderate]/[mobile]) — would contest the regime's premises, kept outside
 *   its speech rules - comparative_ritual_scholars: Analytical observer
 *   ([analytical]/[analytical]) — compares rehearsal regimes against
 *   documented outcomes
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__survival_competence_reading, 0.52).
domain_priors:suppression_score(catastrophe_memory_kernel__survival_competence_reading, 0.58).
domain_priors:theater_ratio(catastrophe_memory_kernel__survival_competence_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__survival_competence_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__survival_competence_reading, "Mourning-Practice Regime as Persecution-Survival Training").
narrative_ontology:topic_domain(catastrophe_memory_kernel__survival_competence_reading, "religious/collective_memory").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__survival_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__survival_competence_reading, 'ab747b68-485f-435d-9bc6-b84c3510b401').
narrative_ontology:cs_kernel_codification('ab747b68-485f-435d-9bc6-b84c3510b401', fixed_text).
narrative_ontology:cs_authority_grounding('ab747b68-485f-435d-9bc6-b84c3510b401', lineage).
narrative_ontology:cs_interpretation_layer_present('ab747b68-485f-435d-9bc6-b84c3510b401').
narrative_ontology:cs_reading_relation('ab747b68-485f-435d-9bc6-b84c3510b401', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('ab747b68-485f-435d-9bc6-b84c3510b401', catastrophe_memory_kernel__trauma_encoding_reading, coexists_with).
narrative_ontology:cs_reading_relation('ab747b68-485f-435d-9bc6-b84c3510b401', catastrophe_memory_kernel__boundary_maintenance_reading, influences).
narrative_ontology:cs_axiom('ab747b68-485f-435d-9bc6-b84c3510b401', foundational, ritual_transmits_operational_survival_competence).
narrative_ontology:cs_axiom_status(ritual_transmits_operational_survival_competence, holdable).
narrative_ontology:cs_axiom_grounding('ab747b68-485f-435d-9bc6-b84c3510b401', ritual_transmits_operational_survival_competence, empirically_contingent).
narrative_ontology:cs_axiom('ab747b68-485f-435d-9bc6-b84c3510b401', secondary, boundary_costs_instrumentally_justified).
narrative_ontology:cs_axiom_status(boundary_costs_instrumentally_justified, holdable).
narrative_ontology:cs_axiom_grounding('ab747b68-485f-435d-9bc6-b84c3510b401', boundary_costs_instrumentally_justified, instrumental).
narrative_ontology:cs_reference_frame('ab747b68-485f-435d-9bc6-b84c3510b401', persecution_preparedness_canon).
narrative_ontology:cs_drift_state('ab747b68-485f-435d-9bc6-b84c3510b401', contemporary_long_peace_decades, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ab747b68-485f-435d-9bc6-b84c3510b401', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__survival_competence_reading, persecution_exposed_community).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__survival_competence_reading, diaspora_survivor_households).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__survival_competence_reading, assimilation_pressured_members).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__survival_competence_reading, successor_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__survival_competence_reading, successor_generations).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel__survival_competence_reading, persecution_recurrence_premise).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Curates the calendar of fast days, lamentation recitals, and commemorative rehearsals; decides which catastrophes enter the canon and how they are narrated; disciplines lapses in observance through communal standing, marriage brokerage, and occasional formal ban. Its members' vocation, standing, and self-understanding are bound up with custodianship of the memory; stepping outside the role means relinquishing the office and the learned station attached to it. In crisis decades the council coordinates mutual-aid activation; in quiet decades it defends the calendar against simplification.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, ritual_authority_council, agenda_setter,
    institutional, generational, identity_locked, global).

% The gathered body of households that observes the calendar together. It holds the pooled capacities the regime maintains: warning-interpretation habits, escape routes rehearsed in narrative, mutual-aid funds triggered by ritual occasions, and the trust networks that move people and goods quickly when episodes strike. Leaving the observant body means forfeiting those networks; staying means carrying the calendar's demands.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, persecution_exposed_community, beneficiary,
    organized, generational, constrained, regional).

% Individual families scattered across host societies who keep the observance at household scale: fasting, retelling, teaching children the response patterns, keeping documents and portable holdings ready. They draw on the pooled competence most directly when episodes reach them, and they bear the daily friction of distinctiveness — diet, sabbath, schooling choices, marriage expectations.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, diaspora_survivor_households, beneficiary,
    moderate, generational, constrained, regional).

% Members for whom the surrounding society offers language, profession, marriage, and civic belonging on assimilable terms. For them the calendar functions as a standing charge on crossing over: every step toward integration runs against family expectation, communal sanction, and the sense of betraying the mourned dead. Some pay the charge and stay; some pay it in estrangement after leaving; few leave without paying.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, assimilation_pressured_members, payer,
    moderate, biographical, identity_locked, regional).

% Children and grandchildren raised inside the rehearsal before any current danger has touched them. They inherit the full obligation load — fasts, vigilance narratives, distinctiveness upkeep — chosen by predecessors, along with a conditional inheritance: the response patterns that would protect them if episodes return. They have no seat in deciding what they are trained for.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, successor_generations, payer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__survival_competence_reading, successor_generations, beneficiary).

% Descendants who have left observance and largely left the conversation. They would contest the calendar's threat premises, its demand on their time and identity, and its account of what loyalty to the dead requires. The regime's definition of who may speak about the catastrophe leaves little room for their testimony inside the room.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, secularized_descendants, excluded,
    moderate, biographical, mobile, regional).

% Researchers of collective memory and persecution response across traditions. They compare rehearsal regimes against documented crisis outcomes, trace which practices carry operational content versus commemorative habit, and publish outside any community's sanction structure.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, comparative_ritual_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_kernel__survival_competence_reading, ritual_authority_council).
narrative_ontology:fixing_cost_class(catastrophe_memory_kernel__survival_competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves and transmits, across generations longer than any individual memory and across ruptures of language, literacy, and geography, a body of non-obvious survival knowledge: which warnings are credible, what to carry, where to scatter, how to reconstitute credit and care networks after destruction. It also keeps mutual-aid capacity warm between rare episodes when that capacity has no day-to-day payoff — a free-rider problem that only an obligation structure can solve.
% TRANSFER_FUNCTION: Moves observance labor — fasting, lamentation, vigilance rehearsal, distinctiveness upkeep — from all members, disproportionately the young and the integration-inclined, into a collective preparedness stock administered by the ritual authority; moves interpretive control and sanction power to that authority; and moves a guarantee of remembrance, symbolically, to the mourned dead.
% ABSENT_VOICES: Secularized descendants and would-be liturgical reformers inside the community, and integration-minded members who experience the calendar as a standing charge — they are outside the conversation because the regime defines who may speak about the catastrophe and on what terms. Host-society perspectives on what the boundary costs do to integration are absent by construction.
% DISAPPEARANCE_RATIONALE: Within roughly two generations the preparedness stock would decay: warning-interpretation habits lapse without rehearsal, mutual-aid funds lose their activation triggers, portable-holding norms dissolve into ordinary asset strategies. Assimilation-pressured members and successors would feel relief immediately; the first subsequent catastrophe episode would find the community relearning at full cost what the calendar had been keeping cheap.
% FOUNDING_PROBLEM: Repeated catastrophe episodes — expulsions, massacres, forced conversions — destroyed communities whose members survived individually but failed collectively: each generation relearned warning-signs, flight logistics, and mutual-aid mobilization from scratch, at catastrophic cost, because nothing carried the knowledge across the gap between episodes.
% FOUNDING_PROBLEM_CORROBORATION: Historians of persecution cycles, working outside any benefiting party, document recurrence patterns across host societies and centuries; contemporary incident data corroborates that the threat is episodic rather than closed. Assimilated descendants attest the opposite from their seat — that the problem is finished and the calendar taxes the living for the dead's wars. No single external source settles liveness; the attestation is genuinely split, which is why the status is contested rather than live or dead.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__survival_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__survival_competence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__survival_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_kernel__survival_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__survival_competence_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__survival_competence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_kernel__survival_competence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_kernel__survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52 at interval end) because the regime's transfers — observance labor, distinctiveness upkeep, integration foregone — are real but are answered, in crisis phases, by delivered survival value; the referent for epsilon is the standing arrangement across the whole cycle, not its best or worst decade. Suppression (0.58) is the enforcement effort that holds integration-inclined members inside: communal sanction, marriage and credit network dependence, ancestor-obligation framing. It is authored as a raw structural property and is deliberately NOT scaled by power or scope — only extractiveness is scaled, by directionality and spatial scope, in the engine's computation. Theater (0.25) is low-to-moderate: much of the repertoire remains operational, but quiet decades accumulate commemorative performance whose survival content is asserted rather than exercised. CYCLICAL PATTERN: all three series run on one shared nine-point grid (t=0..120, 15-year steps) and oscillate with a roughly 60-year period driven by external threat episodes interacting with internal enforcement capacity: crisis strikes, the canon ratchets (new commemorations are added, enforcement hardens, extraction and suppression peak), long peace follows, observance loosens, theater accumulates, until the next episode re-legitimates the regime. The oscillation is partly an extraction mechanism in itself — intermittent reinforcement: peacetime relaxation lowers resistance to the next ratchet, and each crisis permanently adds canonical catastrophes that are never removed. The base_properties scalars are the t=120 readings: mid-relaxation, one ratchet back.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. From the council seat the arrangement is custodianship its members personally sacrifice for; from the integration-inclined member seat it is a standing charge on a life available next door; from the successor-generation seat it is an inherited training with a conditional payoff. The same nominal seat also computes differently across cycle phase: a cohort that lived an episode and a cohort raised in the long peace hold opposite experiences of identical obligations — the measurement series is how the engine sees that temporal seat variable. Coalition potential: the integration-inclined are individually weak but have repeatedly formed reform and secession coalitions (the resistance series tracks these); each coalition's partial success relaxes enforcement until the next crisis reverses it.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows: the observing community and its households sit near the beneficiary end (pooled competence flows to them in crisis; their exit — leaving the observant body — forfeits the networks, so they are constrained rather than mobile); integration-pressured members sit near the target end (they pay the boundary charge with identity-locked exit: leaving means family rupture and self-betrayal, not merely relocation); successor generations are declared victims but hold a call option on the competence, moderating their target position. Two overrides correct derivations the structural data alone gets wrong: (1) the council derives as a near-full beneficiary because it administers and collects standing, but its members also bear enforcement labor, ascetic obligations, and crisis-era personal risk — d is overridden from roughly 0.1 to 0.28; (2) successor generations derive as near-full targets from the victim declaration alone, but their conditional inheritance pulls d down to 0.62. Receipt surface: the arrangement's discretionary surplus — control of the canon, sanction power, communal standing — demonstrably accrues to the council seat even though the material protection diffuses across households, so gain_flow names the council. Fixing is prohibitive because transmission is irreversible once interrupted: competence that stops being rehearsed cannot be rebuilt on demand, and the option value against threat recurrence prices removal above its benefit for whoever could order it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — persecution recurrence — is intermittently live, and the arrangement is built never to sunset: every crisis adds canonical commemorations and none are ever removed, so the regime's cost basis is calibrated to the worst historical maximum and billed in the quietest decades. Reading the structure without the coordination half yields a pure-extraction verdict that erases the documented crisis-phase vindication of the training function; reading it without the extraction half yields a pure-coordination verdict that erases the identifiable members who pay integration costs in every phase, including peaceful ones. The hybrid claim keeps both halves visible, and the threat_environment_liveness omega governs the drift question: if the threat closes permanently, the regime persists by ratchet inertia with successors as net payers and the profile degrades toward maintained performance; if the threat is episodic, the ratchet is the price of the option and the hybrid stands. The classification therefore prevents mandatrophy mislabeling in both directions — neither declaring the mandate dead during a peace the historical record does not support, nor declaring it live on the regime's own testimony alone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_decomposition,
    'This story instantiates only the survival_competence_reading of the catastrophe_memory_kernel; do the sibling readings (symbol_continuity_reading, trauma_encoding_reading, boundary_maintenance_reading) describe structurally distinct constraints with materially different epsilon and beneficiary/victim sets, such that the colloquial unity of ''catastrophe-memory ritual'' is a label artifact?',
    'Generate and classify the three sibling stories; compare computed types, epsilon, and seat structures across the four-member family.',
    'If a sibling''s structure dominates (e.g., boundary_maintenance widens the victim set and raises epsilon), this story''s epsilon is misattributed to the wrong constraint; the family comparison locates the disagreement structurally instead of rhetorically.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_decomposition, conceptual, 'Kernel-label decomposition: one kernel, four readings, four constraints.').

omega_variable(
    operational_repertoire_decay,
    'Does the rehearsed repertoire still contain operationally current survival content (credible-warning taxonomies, flight logistics, portable-asset norms, mutual-aid activation protocols), or has peacetime drift converted it into commemorative performance whose survival value is asserted rather than exercised?',
    'Content analysis of liturgy and practice against documented crisis behavior: compare survival outcomes and response latency across communities with dense versus thin rehearsal regimes in recorded catastrophe episodes.',
    'If the repertoire is largely theatrical, this reading collapses toward inertial persistence and epsilon falls; if operational content is demonstrably transmitted, the coordination half of the hybrid stands and the moderate-epsilon reading is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_repertoire_decay, empirical, 'Whether the ritual repertoire carries live operational content or decaying performance.').

omega_variable(
    threat_environment_liveness,
    'Is the persecution-threat environment that the training regime presupposes live, episodically recurrent, or historically closed for this community''s host societies?',
    'Longitudinal incident data across host societies over the interval, combined with the regime''s own crisis-vindication record: did rehearsed patterns activate in documented episodes?',
    'If closed, the arrangement persists by ratchet inertia with successor generations as net payers and the profile degrades toward maintained performance; if episodic, the ratchet-and-relax cycle is the price of a real option and the hybrid classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threat_environment_liveness, empirical, 'Liveness of the founding threat environment across the interval.').

omega_variable(
    internalized_suppression_share,
    'Is the suppression holding assimilation-pressured members inside the observance regime primarily structural (communal sanction, marriage and credit network dependence, geographic clustering) or internalized (ancestor-guilt, betrayal-fear, fused self-concept)?',
    'Post-exit trajectory of leavers: if the obligation load persists after structural barriers are removed (relocation, network independence), the internalized share is substantial.',
    'If largely internalized, effective suppression exceeds the structural measure and exits are more locked than the declared exit_options suggest; if largely structural, enforcement relaxation would release defection quickly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_share, empirical, 'Structural versus internalized composition of the regime''s hold on members.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__survival_competence_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(cata_tr_t15, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 30, 0.26).
narrative_ontology:measurement(cata_tr_t45, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 45, 0.34).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 60, 0.16).
narrative_ontology:measurement(cata_tr_t75, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 75, 0.14).
narrative_ontology:measurement(cata_tr_t90, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 90, 0.22).
narrative_ontology:measurement(cata_tr_t105, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 105, 0.24).
narrative_ontology:measurement(cata_tr_t120, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 120, 0.25).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(cata_be_t15, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 30, 0.44).
narrative_ontology:measurement(cata_be_t45, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 45, 0.38).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 60, 0.62).
narrative_ontology:measurement(cata_be_t75, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 75, 0.66).
narrative_ontology:measurement(cata_be_t90, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 90, 0.56).
narrative_ontology:measurement(cata_be_t105, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 105, 0.6).
narrative_ontology:measurement(cata_be_t120, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 120, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(cata_su_t15, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement(cata_su_t45, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 45, 0.4).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 60, 0.72).
narrative_ontology:measurement(cata_su_t75, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 75, 0.78).
narrative_ontology:measurement(cata_su_t90, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 90, 0.64).
narrative_ontology:measurement(cata_su_t105, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 105, 0.7).
narrative_ontology:measurement(cata_su_t120, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 120, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__survival_competence_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel__trauma_encoding_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel__boundary_maintenance_reading).

% DUAL FORMULATION NOTE:
% Decomposition of the colloquial label 'catastrophe-memory ritual' into four structurally distinct constraints per the epsilon-invariance principle: this file authors the survival_competence_reading only. The sibling stories (symbol_continuity, trauma_encoding, boundary_maintenance) carry their own epsilon, beneficiary/victim sets, and claimed types. Ordering within the family runs from the better-evidenced transmission claims toward the more contested boundary-justification claims: this reading influences the boundary_maintenance_reading because its empirical fortunes supply, or withdraw, the cost-justification the boundary reading relies on.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_kernel__survival_competence_reading, institutional, 0.28).
constraint_indexing:directionality_override(catastrophe_memory_kernel__survival_competence_reading, powerless, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
