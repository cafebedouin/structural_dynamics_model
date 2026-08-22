% ============================================================================
% CONSTRAINT STORY: manifesto_revolutionary_method__council_communist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_manifesto_revolutionary_method__council_communist_reading, []).

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
 *   constraint_id: manifesto_revolutionary_method__council_communist_reading
 *   human_readable: Council Communist Reading: Federated Workplace Assembly Rule
 *   domain: political_philosophy/revolutionary_theory
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the manifesto_revolutionary_method
 *   kernel: the council-communist reading, in which the Manifesto's
 *   revolutionary method terminates in federated workplace assemblies holding
 *   power directly - replacing both the capitalist state and any
 *   professional-revolutionary party. Per the epsilon-invariance principle,
 *   the referent of epsilon is this standing arrangement as the reading
 *   itself assesses it: the council order, not the vanguard or gradualist
 *   orders its rivals would build. The arrangement coordinates production,
 *   distribution, and defense through mandated, recallable delegates
 *   answerable to continuous assembly majorities; its historical instances
 *   are short-lived but recurrent (1871, 1905, 1918-21, 1936, 1956, 1970-80),
 *   and between episodes it persists as doctrine. KEY AGENTS (by structural
 *   relationship): - autonomous_worker_collectives: Primary beneficiary
 *   (organized/constrained) - holds production power through federated
 *   assemblies and administers the federation - state_bureaucrats: Primary
 *   target (institutional/identity_locked) - loses the mediation function the
 *   arrangement deletes - party_officials: Secondary target
 *   (organized/identity_locked) - loses the representative monopoly
 *   assemblies make redundant - rural_agricultural_producers: Excluded seat
 *   (moderate/constrained) - outside the workplace franchise -
 *   household_and_informal_workers: Excluded seat (powerless/trapped) -
 *   unrecognized by workplace-based power - comparative_council_historians:
 *   Analytical observer (analytical/analytical) - sees the full structure
 *   across episodes
 *
 * KEY AGENTS:
 *   - autonomous_worker_collectives: Primary beneficiary (organized/constrained) - governs production through assemblies, administers the federation, recalls delegates
 *   - state_bureaucrats: Primary target (institutional/identity_locked) - career mediation function deleted by federation
 *   - party_officials: Secondary target (organized/identity_locked) - class-representation office eliminated by direct assembly rule
 *   - rural_agricultural_producers: Excluded seat (moderate/constrained) - food suppliers with no franchise seat
 *   - household_and_informal_workers: Excluded seat (powerless/trapped) - labor outside the workplace register
 *   - comparative_council_historians: Analytical observer (analytical/analytical) - assembles the cross-episode record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__council_communist_reading, 0.25).
domain_priors:suppression_score(manifesto_revolutionary_method__council_communist_reading, 0.5).
domain_priors:theater_ratio(manifesto_revolutionary_method__council_communist_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__council_communist_reading, tangled_rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__council_communist_reading, "Council Communist Reading: Federated Workplace Assembly Rule").
narrative_ontology:topic_domain(manifesto_revolutionary_method__council_communist_reading, "political_philosophy/revolutionary_theory").

domain_priors:requires_active_enforcement(manifesto_revolutionary_method__council_communist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__council_communist_reading, '370eb484-fa2b-4b24-8346-b356c5e63bf4').
narrative_ontology:cs_kernel_codification('370eb484-fa2b-4b24-8346-b356c5e63bf4', fixed_text).
narrative_ontology:cs_authority_grounding('370eb484-fa2b-4b24-8346-b356c5e63bf4', lineage).
narrative_ontology:cs_interpretation_layer_present('370eb484-fa2b-4b24-8346-b356c5e63bf4').
narrative_ontology:cs_reading_relation('370eb484-fa2b-4b24-8346-b356c5e63bf4', manifesto_revolutionary_method__vanguard_rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('370eb484-fa2b-4b24-8346-b356c5e63bf4', manifesto_revolutionary_method__democratic_gradualism_reading, coexists_with).
narrative_ontology:cs_axiom('370eb484-fa2b-4b24-8346-b356c5e63bf4', foundational, immediate_assembly_sovereignty).
narrative_ontology:cs_axiom_status(immediate_assembly_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('370eb484-fa2b-4b24-8346-b356c5e63bf4', immediate_assembly_sovereignty, deontological).
narrative_ontology:cs_axiom('370eb484-fa2b-4b24-8346-b356c5e63bf4', foundational, no_mediating_political_caste).
narrative_ontology:cs_axiom_status(no_mediating_political_caste, holdable).
narrative_ontology:cs_axiom_grounding('370eb484-fa2b-4b24-8346-b356c5e63bf4', no_mediating_political_caste, instrumental).
narrative_ontology:cs_reference_frame('370eb484-fa2b-4b24-8346-b356c5e63bf4', council_direct_democracy_frame).
narrative_ontology:cs_drift_state('370eb484-fa2b-4b24-8346-b356c5e63bf4', contemporary_post_1989, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('370eb484-fa2b-4b24-8346-b356c5e63bf4', '').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__council_communist_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__council_communist_reading, autonomous_worker_collectives).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__council_communist_reading, state_bureaucrats).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__council_communist_reading, party_officials).
narrative_ontology:constraint_vindicates(manifesto_revolutionary_method__council_communist_reading, self_emancipation_principle).
narrative_ontology:constraint_vindicates(manifesto_revolutionary_method__council_communist_reading, commune_state_form_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Workplace assemblies of production workers govern their shops directly, elect mandated delegates to district and industry federations for coordination tasks, and retain the right to recall them at any time. Surplus disposition, output priorities, and safety rules are decided in assembly; delegates carry instructions rather than personal authority, and the assemblies themselves set the federation's rules. Leaving the arrangement means leaving coordinated production altogether, since the federation is the economy's operating structure.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, autonomous_worker_collectives, beneficiary,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__council_communist_reading, autonomous_worker_collectives, agenda_setter).

% Career administrators of the managerial state - planning offices, statistical bureaus, licensing bodies - occupy positions whose authority consists in mediating between ministries and enterprises. Federation of workplace decisions deletes the mediation layer their careers are built on; their technical skills remain useful but their offices do not. Moving to a rival organizational form preserves their function; staying means accepting a subordinate technical role inside assemblies.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, state_bureaucrats, payer,
    institutional, generational, identity_locked, national).

% Full-time cadres of revolutionary parties derive standing from speaking and deciding on behalf of a class they organize. Assemblies that decide their own business leave no office of class representation to hold; the cadre's organizational capital - networks, discipline, doctrine - retains value only in organizations that keep representation separate from the represented. Their exit runs through the rival organizational forms that preserve such offices.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, party_officials, payer,
    organized, generational, identity_locked, continental).

% Smallholders, landless laborers, and village cooperatives produce food the federated economy depends on but hold no seat in a structure franchised by workplace. District peasant assemblies existed in several historical episodes but were subordinated or admitted on unequal terms; their leverage is withholding supply, and their standing in the arrangement is peripheral.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, rural_agricultural_producers, excluded,
    moderate, generational, constrained, regional).

% Unpaid domestic labor and casual or informal work fall outside any workplace franchise; the assembly circuit registers neither their hours nor their needs. They consume the arrangement's outputs and staff its margins, and their claim to power has no organ to land in.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, household_and_informal_workers, excluded,
    powerless, biographical, trapped, national).

% Researchers comparing council episodes across countries and decades - 1871, 1905, 1918-21, 1936, 1956, 1970-80 - assemble the record the tradition argues from. They hold no position in the arrangement and bear none of its costs; their analyses feed both the tradition's self-understanding and its critics'.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, comparative_council_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(manifesto_revolutionary_method__council_communist_reading, autonomous_worker_collectives).
narrative_ontology:fixing_cost_class(manifesto_revolutionary_method__council_communist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates production, distribution, and defense across federated workplaces through mandated, recallable delegates answerable to continuous assembly majorities - solving the scale problem of direct democracy without creating a separate administrative body standing above the producers.
% TRANSFER_FUNCTION: Moves disposal over production and surplus from private owners, state managers, and party hierarchies to the workplace collectives themselves; moves no ongoing revenue or standing office to any political caste, since delegate positions are temporary, instructed, and revocable.
% ABSENT_VOICES: Those without a workplace seat - rural smallholders and landless laborers, household and informal workers - would object that the franchise maps power onto factory geography; they are outside the assembly circuit the arrangement constitutes. Defeated owner and managerial interests object from outside the arrangement entirely.
% DISAPPEARANCE_RATIONALE: Production governance, defense organization, and surplus disposition all route through the federated assemblies; overnight disappearance returns coordination to managerial hierarchies, party mediation, or market allocation, and every seated agent's situation changes - the collectives lose their operating structure, the displaced castes regain their offices, and the unseated remain unseated under whatever replaces it.
% FOUNDING_PROBLEM: How the working class can hold power directly without the delegated institutions of prior revolutions hardening into a new ruling stratum - the Commune's lesson that representation must be impermanent, mandated, and revocable, applied against both the bourgeois state and the professional-revolutionary party.
% FOUNDING_PROBLEM_CORROBORATION: Robert Michels' iron-law-of-oligarchy research, conducted outside and against the socialist movement, independently attests the recurrence of delegation-alienation in every delegated organization; organizational sociologists across orientations treat delegate-accountability decay as an unsolved standing problem. The attestation does not rest on the benefiting parties alone.
narrative_ontology:disappearance_verdict(manifesto_revolutionary_method__council_communist_reading, world_rearranges).
narrative_ontology:founding_problem_status(manifesto_revolutionary_method__council_communist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(manifesto_revolutionary_method__council_communist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(manifesto_revolutionary_method__council_communist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(manifesto_revolutionary_method__council_communist_reading, 0.25, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(manifesto_revolutionary_method__council_communist_reading_tests).
:- end_tests(manifesto_revolutionary_method__council_communist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low (0.25) because the arrangement's design deletes the political caste that collects in rival forms: delegates are mandated, recallable, and paid a worker's wage, so little surplus routes to administrators. Suppression is authored moderate (0.5): holding council power has historically required real defensive force against restoration attempts, while internal disagreement is handled by mandate and recall rather than by an apparatus; the scalar describes the arrangement's intrinsic enforcement demand when operative, while the suppression_requirement series traces realized enforcement capacity decaying from 0.62 to 0.12 as live episodes vanished. Theater is authored 0.38 as the end-state blend: functional episodes ran 0.12-0.17, but the tradition's survival decades are increasingly commemorative. Accessibility collapse is low (0.2): mastering the council form forecloses no rival organizational form - which is precisely why the reading competes rather than settles. Resistance is high (0.8): every live episode met armed opposition from states, owners, and rival Marxist currents. CYCLICAL PATTERN: the shared-grid series shows roughly two full cycles of eruption, crushing, latency, and revival (functional peaks near t=0, 40, 50; latency troughs at t=30 and t=80-100). The oscillation is lifecycle, not intermittent reinforcement: visibility collapses under external defeat and recovers with new labor upheaval, and theater rises in latency phases because survival turns commemorative. The base_properties scalars were measured at interval end (t=100), a latency-phase point, so the end-state theater figure overstates the functional episodes' ratio.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat the arrangement is self-rule: the same structure that coordinates production leaves no one to obey. From the two payer seats the identical structure is expropriation of a life-function - the bureaucrat's office and the cadre's calling are deleted rather than taxed, and both are identity_locked, so the loss reads as existential rather than financial. The excluded seats compute neither benefit nor payment: they stand outside the franchise, a third structural position that the beneficiary/victim declaration alone does not surface. The engine computes these divergent per-seat classifications from the power, exit, and role data; the authored claim adjudicates none of them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map directly: autonomous_worker_collectives (beneficiary, constrained exit) derive d near the subsidy end; state_bureaucrats and party_officials (payers, identity_locked) derive d near the full-target end, with identity lock amplifying effective extraction beyond what mobile payers would register. No directionality_overrides are authored: the derivation chain captures every seated agent correctly, and the excluded seats' position - outside the distribution rather than inside it at some d - cannot be expressed as a per-power-atom override without smearing across unrelated seats; it is routed to the workplace_franchise_boundary omega instead.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - delegation hardening into a ruling stratum - is live wherever delegated organizations operate, so no mandatrophy resolution is declared. Two misclassification risks are guarded against. First, mistaking low internal epsilon for pure coordination: the arrangement genuinely pays, through the same structure that coordinates everyone else, at the expense of two identifiable castes, which is why the claim is tangled_rope rather than rope. Second, reading the rising theater series (0.12 to 0.38) as piton decay: theatrical survival is real, but no agenda-setting administrator profits enough to maintain the arrangement artificially, and no seat bears a fix cheaper than the arrangement's cost - it is kept alive by conviction and periodic labor upheaval, not inertia - so the piton signature fails and the constraint remains tangled_rope with a monitored theater trajectory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'Does the engine''s classification of this file describe the council-communist reading specifically, or leak across sibling readings of the same kernel?',
    'Compare computed types across the three sibling files of manifesto_revolutionary_method; divergent epsilon and victim sets confirm indexical separation, while convergent outputs suggest the kernel label rather than the structural data is driving the verdict.',
    'If readings are conflated, the vanguard reading''s high extraction contaminates this file''s verdict or vice versa; per-seat classifications must be read against this file''s declarations only.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame routing: one reading, one constraint, one epsilon.').

omega_variable(
    episode_vs_tradition_extraction,
    'Is the authored epsilon the extraction of lived council episodes or of the textual tradition that persists between them?',
    'Recompute conditioned on intervals where assemblies held actual power (1918-21, 1936, 1956, 1970-80) versus latency decades where the arrangement survived only as doctrine.',
    'Episode-conditioned epsilon trends toward 0.18-0.22; tradition-inclusive epsilon rises toward 0.3 as commemorative overhead counts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(episode_vs_tradition_extraction, empirical, 'Measurement-basis ambiguity between functioning episodes and survival-as-text.').

omega_variable(
    workplace_franchise_boundary,
    'Does the workplace-based franchise constitute structural extraction from the unseated (rural, household, informal labor) or merely a scoping limit of the arrangement?',
    'Examine council constitutions'' treatment of non-workplace labor across episodes (Austrian 1918, Hungarian 1956, Polish 1980-81): were peasant and household seats admitted, subordinated, or absent?',
    'If exclusion is load-bearing design, the victim set extends beyond the two displaced castes and epsilon rises materially; if incidental, the excluded seats remain commentary-grade.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(workplace_franchise_boundary, empirical, 'Franchise-boundary status of non-workplace labor.').

omega_variable(
    defensive_coercion_degeneration_threshold,
    'How much defensive coercion can holding council power require before the defense apparatus reproduces the standing-force structure the reading forbids?',
    'Comparative study of council defense formations (Makhnovshchina, CNT militias, 1956 Budapest workers'' councils) for rotation, mandate, and recall practices maintained under fire.',
    'Above the threshold the suppression scalar climbs and the no_mediating_political_caste axiom becomes self-undermining, pushing the reading toward the very structure it opposes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(defensive_coercion_degeneration_threshold, empirical, 'Degeneration threshold of council defense force.').

omega_variable(
    external_suppression_indexing,
    'The manifest records high external suppression of this reading by rival organizational forms; the suppression scalar authors this constraint''s own coercive force - is the asymmetry being indexed correctly?',
    'Per-seat chi computation separates target-side amplification from beneficiary-side damping; external hostility enters as resistance faced (authored 0.8), not as suppression exerted.',
    'Misindexing would double-count the hostile environment as internal coercion, inflating suppression and distorting the type verdict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(external_suppression_indexing, conceptual, 'Directional indexing of suppression-faces versus suppression-exerts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__council_communist_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mani_tr_t0, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(mani_tr_t0, observed).
narrative_ontology:measurement(mani_tr_t10, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement_basis(mani_tr_t10, observed).
narrative_ontology:measurement(mani_tr_t20, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement_basis(mani_tr_t20, observed).
narrative_ontology:measurement(mani_tr_t30, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement_basis(mani_tr_t30, observed).
narrative_ontology:measurement(mani_tr_t40, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 40, 0.14).
narrative_ontology:measurement_basis(mani_tr_t40, observed).
narrative_ontology:measurement(mani_tr_t50, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 50, 0.13).
narrative_ontology:measurement_basis(mani_tr_t50, observed).
narrative_ontology:measurement(mani_tr_t60, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 60, 0.17).
narrative_ontology:measurement_basis(mani_tr_t60, observed).
narrative_ontology:measurement(mani_tr_t70, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 70, 0.22).
narrative_ontology:measurement_basis(mani_tr_t70, observed).
narrative_ontology:measurement(mani_tr_t80, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 80, 0.31).
narrative_ontology:measurement_basis(mani_tr_t80, observed).
narrative_ontology:measurement(mani_tr_t90, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 90, 0.34).
narrative_ontology:measurement_basis(mani_tr_t90, observed).
narrative_ontology:measurement(mani_tr_t100, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 100, 0.38).
narrative_ontology:measurement_basis(mani_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(mani_be_t0, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(mani_be_t0, observed).
narrative_ontology:measurement(mani_be_t10, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 10, 0.26).
narrative_ontology:measurement_basis(mani_be_t10, observed).
narrative_ontology:measurement(mani_be_t20, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 20, 0.24).
narrative_ontology:measurement_basis(mani_be_t20, observed).
narrative_ontology:measurement(mani_be_t30, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 30, 0.28).
narrative_ontology:measurement_basis(mani_be_t30, observed).
narrative_ontology:measurement(mani_be_t40, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 40, 0.23).
narrative_ontology:measurement_basis(mani_be_t40, observed).
narrative_ontology:measurement(mani_be_t50, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 50, 0.21).
narrative_ontology:measurement_basis(mani_be_t50, observed).
narrative_ontology:measurement(mani_be_t60, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 60, 0.24).
narrative_ontology:measurement_basis(mani_be_t60, observed).
narrative_ontology:measurement(mani_be_t70, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 70, 0.26).
narrative_ontology:measurement_basis(mani_be_t70, observed).
narrative_ontology:measurement(mani_be_t80, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 80, 0.29).
narrative_ontology:measurement_basis(mani_be_t80, observed).
narrative_ontology:measurement(mani_be_t90, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 90, 0.27).
narrative_ontology:measurement_basis(mani_be_t90, observed).
narrative_ontology:measurement(mani_be_t100, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 100, 0.25).
narrative_ontology:measurement_basis(mani_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(mani_su_t0, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(mani_su_t0, observed).
narrative_ontology:measurement(mani_su_t10, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement_basis(mani_su_t10, observed).
narrative_ontology:measurement(mani_su_t20, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 20, 0.44).
narrative_ontology:measurement_basis(mani_su_t20, observed).
narrative_ontology:measurement(mani_su_t30, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 30, 0.3).
narrative_ontology:measurement_basis(mani_su_t30, observed).
narrative_ontology:measurement(mani_su_t40, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement_basis(mani_su_t40, observed).
narrative_ontology:measurement(mani_su_t50, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 50, 0.36).
narrative_ontology:measurement_basis(mani_su_t50, observed).
narrative_ontology:measurement(mani_su_t60, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 60, 0.34).
narrative_ontology:measurement_basis(mani_su_t60, observed).
narrative_ontology:measurement(mani_su_t70, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 70, 0.28).
narrative_ontology:measurement_basis(mani_su_t70, observed).
narrative_ontology:measurement(mani_su_t80, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 80, 0.18).
narrative_ontology:measurement_basis(mani_su_t80, observed).
narrative_ontology:measurement(mani_su_t90, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 90, 0.15).
narrative_ontology:measurement_basis(mani_su_t90, observed).
narrative_ontology:measurement(mani_su_t100, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 100, 0.12).
narrative_ontology:measurement_basis(mani_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manifesto_revolutionary_method__council_communist_reading, resource_allocation).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__council_communist_reading, manifesto_revolutionary_method__vanguard_rupture_reading).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__council_communist_reading, manifesto_revolutionary_method__democratic_gradualism_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the revolutionary-method kernel (epsilon-invariance): the colloquial label 'the Manifesto's revolutionary method' covers three structurally distinct claims about where class power resides at the moment of rupture. This file authors the council-communist claim (assemblies hold power immediately; epsilon low, victims are the displaced bureaucratic and party castes). The vanguard-rupture sibling authors the party-seizure claim (substantially higher epsilon, victims are the unled class itself during the transition). The democratic-gradualism sibling authors the electoral-vehicle claim (extraction profile set by existing-state friction). The upstream text is common; the downstream epsilon, victim sets, and enforcement requirements differ, so each reading is a separate story linked by network edges rather than one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
