% ============================================================================
% CONSTRAINT STORY: state_commitment_installation_mechanism__hybrid_cascade_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_commitment_installation_mechanism__hybrid_cascade_reading, []).

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
 *   constraint_id: state_commitment_installation_mechanism__hybrid_cascade_reading
 *   human_readable: Apex-Initiated Commitment Cascade with Mandatory Fringe Validation
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   Agrarian and early-modern states rarely could install a new normative
 *   commitment — an official cult, a legal code, a doctrinal orthodoxy, a
 *   standardized administrative category — by decree alone, and could rarely
 *   afford to wait generations for it to emerge from below. The arrangement
 *   this story describes is the two-phase machinery that resolves the
 *   impasse: an apex coalition promulgates the commitment and builds the
 *   transmission apparatus (offices, academies, ceremonies, reporting
 *   chains); the commitment then descends through administrative layers to
 *   peripheral elites whose endorsement and interpretive adaptation give it
 *   local credibility; partial resistance is not crushed wholesale but
 *   metabolized — recoded as permissible 'local interpretation' — until the
 *   commitment stabilizes as shared fact. This file instantiates ONE reading
 *   of the contested kernel state_commitment_installation_mechanism: the
 *   hybrid_cascade_reading, whose structural delta is state initiation,
 *   fringe legitimation, two-phase adoption, and absorption of partial
 *   resistance. The sibling readings — endogenous_climb (legitimacy climbs
 *   from fringes through demonstrated superiority) and exogenous_imposition
 *   (transformation-mandate authority installs legitimacy directly) — are
 *   separate constraints with their own epsilon, beneficiary/victim
 *   structure, and classification, linked through
 *   network.affects_constraints. Per the epsilon referent rule for
 *   kernel-reading stories, epsilon here is authored for the standing
 *   arrangement under contest — the operating cascade machinery as documented
 *   across installation episodes — assessed by this reading's own lights,
 *   never for any endorsed alternative. The colloquial label 'how new state
 *   commitments gain legitimacy' conflates three structurally distinct
 *   mechanisms with materially different extraction profiles; the
 *   decomposition follows the epsilon-invariance principle.
 *
 * KEY AGENTS:
 *   - apex_ruling_coalition: Agenda setter (institutional/arbitrage) — promulgates commitments, owns the transmission machinery, captures the stability dividend
 *   - mid_level_administrators: Transmission-layer beneficiaries (organized/constrained) — carry the cascade downward, gain office security, bear translation costs
 *   - peripheral_fringe_elites: Conscripted validators (moderate/constrained) — supply the endorsement and interpretive adaptation the apex cannot manufacture; bear the appropriation
 *   - heterodox_dissenters: Absorbed resisters (powerless/trapped) — their objections are processed as raw material for local interpretation, not answered
 *   - ordinary_subject_populations: Diffuse beneficiary-payers (powerless/constrained) — receive coordinated norms and legible order, pay compliance costs, choose nothing
 *   - comparative_historians: Analytical observer (analytical/analytical) — compare installation episodes across polities; attest the pattern from outside every beneficiary set
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_commitment_installation_mechanism__hybrid_cascade_reading, 0.62).
domain_priors:suppression_score(state_commitment_installation_mechanism__hybrid_cascade_reading, 0.45).
domain_priors:theater_ratio(state_commitment_installation_mechanism__hybrid_cascade_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_commitment_installation_mechanism__hybrid_cascade_reading, tangled_rope).
narrative_ontology:human_readable(state_commitment_installation_mechanism__hybrid_cascade_reading, "Apex-Initiated Commitment Cascade with Mandatory Fringe Validation").
narrative_ontology:topic_domain(state_commitment_installation_mechanism__hybrid_cascade_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(state_commitment_installation_mechanism__hybrid_cascade_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_commitment_installation_mechanism__hybrid_cascade_reading, 'ce46b532-cb5c-4767-9b9e-2d15bc1190fa').
narrative_ontology:cs_kernel_codification('ce46b532-cb5c-4767-9b9e-2d15bc1190fa', distributed).
narrative_ontology:cs_authority_grounding('ce46b532-cb5c-4767-9b9e-2d15bc1190fa', expertise).
narrative_ontology:cs_interpretation_layer_present('ce46b532-cb5c-4767-9b9e-2d15bc1190fa').
narrative_ontology:cs_reading_relation('ce46b532-cb5c-4767-9b9e-2d15bc1190fa', state_commitment_installation_mechanism__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('ce46b532-cb5c-4767-9b9e-2d15bc1190fa', state_commitment_installation_mechanism__exogenous_imposition_reading, coexists_with).
narrative_ontology:cs_axiom('ce46b532-cb5c-4767-9b9e-2d15bc1190fa', foundational, apex_initiation_constitutive).
narrative_ontology:cs_axiom_status(apex_initiation_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('ce46b532-cb5c-4767-9b9e-2d15bc1190fa', apex_initiation_constitutive, empirically_contingent).
narrative_ontology:cs_axiom('ce46b532-cb5c-4767-9b9e-2d15bc1190fa', foundational, fringe_validation_necessary_for_stabilization).
narrative_ontology:cs_axiom_status(fringe_validation_necessary_for_stabilization, holdable).
narrative_ontology:cs_axiom_grounding('ce46b532-cb5c-4767-9b9e-2d15bc1190fa', fringe_validation_necessary_for_stabilization, empirically_contingent).
narrative_ontology:cs_axiom('ce46b532-cb5c-4767-9b9e-2d15bc1190fa', secondary, resistance_absorbed_as_interpretation).
narrative_ontology:cs_axiom_status(resistance_absorbed_as_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('ce46b532-cb5c-4767-9b9e-2d15bc1190fa', resistance_absorbed_as_interpretation, empirically_contingent).
narrative_ontology:cs_reference_frame('ce46b532-cb5c-4767-9b9e-2d15bc1190fa', two_phase_apex_fringe_stabilization).
narrative_ontology:cs_drift_state('ce46b532-cb5c-4767-9b9e-2d15bc1190fa', contemporary_comparative_turn, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ce46b532-cb5c-4767-9b9e-2d15bc1190fa', '').
narrative_ontology:cs_kernel_id(state_commitment_installation_mechanism__hybrid_cascade_reading, state_commitment_installation_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__hybrid_cascade_reading, apex_ruling_coalition).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__hybrid_cascade_reading, mid_level_administrators).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__hybrid_cascade_reading, ordinary_subject_populations).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__hybrid_cascade_reading, peripheral_fringe_elites).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__hybrid_cascade_reading, heterodox_dissenters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__hybrid_cascade_reading, peripheral_fringe_elites).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__hybrid_cascade_reading, mid_level_administrators).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__hybrid_cascade_reading, ordinary_subject_populations).
narrative_ontology:constraint_vindicates(state_commitment_installation_mechanism__hybrid_cascade_reading, local_consensus_necessity_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Promulgates the new commitment — an edict, state cult, code, or official doctrine — and builds the apparatus that carries it: appoints transmission officers, endows academies, schedules reaffirmation ceremonies, and reviews reports of uptake from every jurisdiction. Cannot compel sincere belief and knows it; depends on peripheral figures volunteering endorsement for the commitment to hold outside the administrative core. When a commitment stalls, can rework it, rebrand it, or quietly drop it and begin another; the apparatus outlives any single campaign and serves whoever holds the apex next.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, apex_ruling_coalition, agenda_setter,
    institutional, generational, arbitrage, continental).

% Governors, prefects, diocesan bureaucracies, provincial secretariats: carry the commitment from promulgation to locality, translate apex doctrine into administrable practice, and report uptake upward. Advancement rides on successful reports, so transmission doubles as advocacy; shortfalls invite investigation from above. Leaving office forfeits position and protection; remaining binds them to whatever the apex installs next.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, mid_level_administrators, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(state_commitment_installation_mechanism__hybrid_cascade_reading, mid_level_administrators, payer).

% Village headmen, provincial literati, rural clergy, lineage elders — the validators whose endorsement tells local populations the new commitment is compatible with local life. Their adaptations reconcile the commitment with custom, and those adaptations are collected upward as evidence the campaign succeeded. Cooperation brings recognition, exemptions, and standing; refusal brings inquiry, pressure on their followers, and eventually replacement with someone more accommodating. Over time their authority fuses with the role: they become known as the ones who speak for the commitment locally, and stepping out of that role means stepping down from local standing altogether.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, peripheral_fringe_elites, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(state_commitment_installation_mechanism__hybrid_cascade_reading, peripheral_fringe_elites, beneficiary).

% Teachers, prophets, sect leaders, and customary-law defenders whose own commitments conflict with the installed one. Open opposition meets discipline; quieter persistence meets the softer instruments — co-optation offers, official reinterpretations that recode their teaching as a variant of the installed commitment, selective enforcement that isolates them from potential followers. Their objections are never answered as claims; they are processed as material for local interpretation.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, heterodox_dissenters, payer,
    powerless, biographical, trapped, regional).

% Farmers, townspeople, parishioners: live under the commitment once it lands. They gain predictable rules, legible dispute settlement, shared festivals and categories; they pay redirected taxes, reassigned worship, and new standards for judging conduct. Nobody asked them whether they wanted the commitment; their eventual acceptance is recorded downstream as proof the installation worked.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, ordinary_subject_populations, beneficiary,
    powerless, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(state_commitment_installation_mechanism__hybrid_cascade_reading, ordinary_subject_populations, payer).

% Scholars who compare installation episodes across polities and centuries. Work from archives, case literature, and comparative method; owe nothing to any ruling coalition, past or present, and take testimony mostly from the dead. Their classifications circulate through curricula and policy folklore and occasionally shape how later apex coalitions design their campaigns — feedback, but weak and slow.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, comparative_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_commitment_installation_mechanism__hybrid_cascade_reading, apex_ruling_coalition).
narrative_ontology:fixing_cost_class(state_commitment_installation_mechanism__hybrid_cascade_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts an intractable simultaneous-coordination problem — a large, heterogeneous polity adopting one new normative commitment at once — into a sequenced one: the apex fixes content, administrative layers carry it downward, peripheral endorsement supplies the local credibility decree cannot manufacture, and licensed local interpretation reconciles residual conflicts with custom. Output: workable uniformity without unanimous consent.
% TRANSFER_FUNCTION: Moves directive content downward (edicts, doctrines, codes, offices, funding, ceremonial calendars) and legitimation labor upward (endorsement, interpretive adaptation, participation, reported uptake); moves dissent sideways into contained channels labeled local variation; leaves compliance costs with subject populations and the stability dividend with the apex coalition.
% ABSENT_VOICES: Heterodox dissenters and unconvinced locals appear only as objects of the cascade — their objections enter the record as resistance to be absorbed, never as positions to be answered; rival commitment entrepreneurs (competing cults, alternative legal traditions) are excluded from the transmission infrastructure outright; and the generations bound by the stabilized commitment had no seat at installation. All three groups sit outside the conversation the machinery conducts with itself.
% DISAPPEARANCE_RATIONALE: Without the two-phase machinery, apex coalitions face a binary they historically avoided: coerce belief directly (provoking revolt, hollow compliance, and chronic reconquest of the periphery) or wait for endogenous convergence beyond any dynasty's horizon. Installation timelines lengthen from decades to centuries or fail outright; religious geographies, legal integration, and administrative standardization rearrange around whichever path each polity takes; the rapid, wide-scale commitment installations that fill the historical record cease to occur as observed.
% FOUNDING_PROBLEM: Expanding states needed shared normative commitments — legitimate succession, authorized worship, standardized obligation, legible categories — across territories whose local traditions predated the state and whose elites commanded local loyalties the center did not; decree produced rebellion or empty compliance, and bottom-up convergence exceeded any ruling house's time horizon.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside every beneficiary set: comparative historical sociology and area-studies historiography (studies of Han-era Confucianization of the periphery, Christianization of the Roman provinces, Qing legal and ritual assimilation, Tokugawa and Meiji doctrinal campaigns) attest both the founding problem and the recurring two-phase pattern; missionary archives, subaltern-studies recoveries of local voices, and administrative records compiled for fiscal rather than ideological purposes independently document fringe validation episodes. No ruling coalition's own chronicles suffice as attestation, and the scholarly attestation predates and ignores modern political uses of the pattern.
narrative_ontology:disappearance_verdict(state_commitment_installation_mechanism__hybrid_cascade_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_commitment_installation_mechanism__hybrid_cascade_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_commitment_installation_mechanism__hybrid_cascade_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_commitment_installation_mechanism__hybrid_cascade_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_commitment_installation_mechanism__hybrid_cascade_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_commitment_installation_mechanism__hybrid_cascade_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_commitment_installation_mechanism__hybrid_cascade_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_commitment_installation_mechanism__hybrid_cascade_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.62 is authored for the mature machinery: the fringe's endorsement and interpretive labor are appropriated under terms the apex set, dissent is recoded rather than answered, and each new commitment restarts the appropriation — but the fringe gains real interpretive space and standing, and subject populations receive genuine coordination goods, so the profile is hybrid rather than purely extractive. Suppression 0.45 reflects the mature phase: open coercion peaks mid-installation (see the suppression_requirement series) and then decays as absorption replaces discipline; the disciplinary edge remains latent — non-validating elites are investigated and replaceable — but steady-state force is modest. Theater_ratio 0.42 rises across the interval as validation becomes ceremonial reaffirmation; the machinery itself stays functional because every new commitment re-enters it, so the ratio signals ritualization of individual cycles, not death of the mechanism. Accessibility_collapse 0.5: once the mechanism is understood, alternatives narrow — withholding validation is costly, exit means marginalization — yet heterodox enclaves, quiet noncompliance, and frontier zones persist, so alternatives are narrowed, not eliminated. Resistance 0.6: foot-dragging, selective compliance, and reinterpretation are endemic; the mechanism exists because resistance is real. The measurement series show one full installation cycle (rise, enforcement peak, decay to normalized background) on a single shared time grid; the cycle is the two-phase structure itself, not intermittent reinforcement, and at civilization timescale the machinery repeats it for each new commitment. Coalition note: the powerless seats' latent power is collective — a coordinated validation refusal by fringe elites is the one lever that bends apex terms — but coordination costs and selective co-optation have historically kept it dormant (see omega fringe_leverage_durability). Claim and metrics are independent authored facts: tangled_rope is claimed from structure (genuine coordination function plus asymmetric appropriation plus active enforcement); the metrics describe operation as observed; the engine computes per-seat types from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   The apex seat experiences the machinery as prudence: cheaper than garrisoning every province against disbelief, more reliable than hoping norms converge on their own — from that chair the two-phase structure is simply how serious states build durable order. The fringe-validator seat experiences conscription: their endorsement is solicited under implicit threat, their adaptations are harvested as evidence of success, and their objections come back to them re-labeled as local color. The administrator seat experiences career dependence — promotion rides on reported uptake, so transmission becomes self-interested advocacy. The dissenter seat experiences the softest face of the machinery: not the dungeon but the offer, the reinterpretation, the isolation. The historian seat sees all of these at once across dozens of polities, and no seat's self-description matches the cross-case pattern. The engine computes this divergence from power, exit, and role data; nothing in the authored claim adjudicates it.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to seats as follows. apex_ruling_coalition is declared beneficiary and holds the agenda: it collects the stability dividend while bearing only administration costs, and its arbitrage-grade exit (rework, rebrand, abandon, restart) places it nearest the beneficiary pole. mid_level_administrators are declared beneficiaries with constrained exit: office security and advancement flow to them, translation burdens flow from them — mildly beneficiary, damped extraction. peripheral_fringe_elites are declared victims with constrained exit: the appropriation lands on them directly and their inability to walk away amplifies their effective extraction toward the target pole; their secondary beneficiary position (recognition, exemptions) moderates but does not reverse this. heterodox_dissenters are declared victims with trapped exit: full targets. ordinary_subject_populations are declared beneficiaries with constrained exit: coordination goods received, compliance costs paid, no seat in choosing content — near symmetric with a slight beneficiary tilt. comparative_historians are analytical observers with no directional stake. Continental scope amplifies effective extraction where verification of sincere uptake is hardest — which is precisely why the machinery invests in ceremony and reporting: verifiable performance substitutes for unverifiable belief.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — installing shared commitments across a heterogeneous polity faster than endogenous convergence and more durably than decree — remains live for every large-scale polity, so no mandatrophy is declared: the machinery has not outlived its function. The classification guards against two mislabels. Reading the arrangement as pure extraction misses that the coordination problem is real and unsolved by alternatives: pure imposition provokes revolt or hollow compliance, pure emergence exceeds any ruler's horizon; the two-phase structure is a working solution, which is why it recurs across unrelated polities. Reading it as pure coordination misses the appropriation: the fringe's legitimation labor is taken under apex-set terms, dissent is metabolized rather than honored, and the stability dividend concentrates at the apex. Tangled rope holds both halves. The forward risk is piton-drift per cycle: as theater_ratio climbs within each installation cycle, validation becomes ceremonial, and if a future era's commitments stop requiring genuine fringe uptake while the ceremonies continue, the machinery would persist as performance — the theater trajectory is the early-warning series for that transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_under_determination,
    'This constraint instantiates one reading (hybrid_cascade) of the kernel state_commitment_installation_mechanism; do the sibling readings — endogenous_climb (legitimacy climbs from fringes through demonstrated superiority) or exogenous_imposition (transformation-mandate authority installs legitimacy top-down) — better capture the actual mechanism across documented installation episodes?',
    'Comparative coding of installation episodes across polities: count cases stabilizing without fringe validation (favors exogenous), cases climbing without apex initiation (favors endogenous), and cases fitting the two-phase pattern (confirms this reading); adjudicate by episode-level fit rather than anecdote.',
    'Under endogenous_climb, fringe actors become initiators rather than conscripted validators — the victim set empties and measured extraction falls toward coordination cost; under exogenous_imposition, fringe validation drops out entirely, suppression rises, and the arrangement trends toward pure extraction. This file''s beneficiary/victim structure and epsilon are valid only under the hybrid reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_under_determination, conceptual, 'Which reading of the installation kernel this story''s structure instantiates.').

omega_variable(
    absorption_genuineness,
    'When the mechanism records partial resistance as ''absorbed via local interpretation,'' is that genuine accommodation of fringe concerns or coerced capitulation relabeled as interpretation?',
    'Compare episodes where validating fringe elites retained autonomous institutions and could withdraw endorsement later, against episodes where validation was extracted under threat of replacement; track whether any absorbed ''interpretation'' ever reversed apex content.',
    'If absorption is predominantly coerced, authored suppression understates the mechanism and the arrangement trends from hybrid coordination toward pure extraction; if genuine, the current profile stands and fringe seats carry less effective extraction than their payer role suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absorption_genuineness, empirical, 'Whether interpretive absorption is accommodation or relabeled coercion.').

omega_variable(
    fringe_leverage_durability,
    'Fringe validators hold latent countervailing power — collective validation refusal destabilizes apex commitments — but does that leverage durably cap extraction, or does the absorption machinery plus selective co-optation neutralize it?',
    'Trace episodes of coordinated validation refusal and apex response: did refusal win concessions, trigger replacement of validators, or split the refusing coalition via selective exemption?',
    'Durable leverage lowers effective extraction on the fringe seats and supports the coordination half of the claimed type; neutralized leverage raises it and shifts weight toward the extraction half.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fringe_leverage_durability, empirical, 'Whether validator refusal power is a real brake or is neutralized by co-optation.').

omega_variable(
    canonical_case_generalization,
    'The authored metrics synthesize canonical installation episodes (doctrinal standardizations, legal transplants, mission campaigns); do they describe the mechanism as such, or a modal case whose parameters vary across polities?',
    'Cross-polity parameter estimation: fit the two-phase model to installation episodes with independent uptake data and report the spread of phase lengths, enforcement peaks, and stabilization thresholds.',
    'Wide spread means epsilon and the temporal profile are case-contingent and classification should be read as modal, not universal; tight spread licenses the current values as mechanism-level.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(canonical_case_generalization, conceptual, 'Case-specific versus mechanism-general status of the authored metric profile.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_commitment_installation_mechanism__hybrid_cascade_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scim_hybrid_cascade_tr_t0, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(scim_hybrid_cascade_tr_t10, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement(scim_hybrid_cascade_tr_t20, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement(scim_hybrid_cascade_tr_t30, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement(scim_hybrid_cascade_tr_t40, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement(scim_hybrid_cascade_tr_t50, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 50, 0.39).
narrative_ontology:measurement(scim_hybrid_cascade_tr_t60, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 60, 0.42).

% Extraction over time
narrative_ontology:measurement(scim_hybrid_cascade_be_t0, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(scim_hybrid_cascade_be_t10, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(scim_hybrid_cascade_be_t20, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(scim_hybrid_cascade_be_t30, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 30, 0.66).
narrative_ontology:measurement(scim_hybrid_cascade_be_t40, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(scim_hybrid_cascade_be_t50, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 50, 0.63).
narrative_ontology:measurement(scim_hybrid_cascade_be_t60, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 60, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(scim_hybrid_cascade_su_t0, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(scim_hybrid_cascade_su_t10, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(scim_hybrid_cascade_su_t20, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(scim_hybrid_cascade_su_t30, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 30, 0.64).
narrative_ontology:measurement(scim_hybrid_cascade_su_t40, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 40, 0.55).
narrative_ontology:measurement(scim_hybrid_cascade_su_t50, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 50, 0.48).
narrative_ontology:measurement(scim_hybrid_cascade_su_t60, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 60, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_commitment_installation_mechanism__hybrid_cascade_reading, identity_coordination).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__hybrid_cascade_reading, state_commitment_installation_mechanism__endogenous_climb_reading).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__hybrid_cascade_reading, state_commitment_installation_mechanism__exogenous_imposition_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'how new state commitments gain legitimacy' decomposes under the epsilon-invariance principle into three structurally distinct mechanisms — endogenous climb, exogenous imposition, hybrid cascade — each with its own epsilon, beneficiary/victim structure, and classification. This file instantiates the hybrid cascade reading; its epsilon (0.62) is authored for the two-phase arrangement as this reading assesses it, and differs from the siblings' epsilons because the location of initiation and the source of legitimation differ structurally, not because the same thing is measured differently. Family members are linked through network.affects_constraints; the hybrid reading functions as the synthesis position whose case library bears on both flanking readings without resolving the contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
