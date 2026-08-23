% ============================================================================
% CONSTRAINT STORY: imposition_mechanism_kernel__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_mechanism_kernel__exogenous_override_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: imposition_mechanism_kernel__exogenous_override_reading
 *   human_readable: State-Coerced Norm Imposition (Violence-Monopoly Legitimacy Reading)
 *   domain: historical sociology/state formation/cultural authority
 *
 * SUMMARY:
 *   A late-centralizing state imposes a package of cultural-administrative
 *   norms — civil registration, calendar and dress reform, standardized
 *   script and schooling, conscription — on a population whose customary
 *   orders predate it, and maintains compliance through a purpose-built
 *   monitoring and punishment apparatus rather than through prior popular
 *   endorsement. Legitimacy, on this reading, flows from the state's
 *   monopolization of sanctioned force: people comply because noncompliance
 *   is punished and alternatives are criminalized, not because the norms were
 *   accepted first. ASSUMPTIONS: the story anchors the abstract reading in
 *   the canonical episode class of late-developer centralizing states, with
 *   Meiji-era Japan (1868-1912) as the primary empirical referent and the
 *   Peter-the-Great and early-Republican-Turkey reform waves as structural
 *   parallels; metric values are scholarly estimates of that episode class's
 *   trajectory, not measurements of a single archive. EPSILON REFERENT:
 *   epsilon assesses the standing arrangement under contest — the imposed
 *   norm order as actually enforced — by this reading's own lights; it does
 *   not assess the negotiated-standardization counterfactual this reading
 *   rejects. FAMILY NOTE: this file instantiates ONLY the
 *   exogenous_override_reading of imposition_mechanism_kernel; the
 *   endogenous_climb and hybrid_legitimation readings are separate constraint
 *   files with their own epsilon, beneficiaries, and metrics, linked through
 *   network.affects_constraints. KEY AGENTS (by structural relationship):
 *   central_state_apparatus: agenda-setter (institutional/arbitrage) —
 *   authors the norms, owns enforcement, collects revenue and legibility;
 *   administrative_enforcement_officials: beneficiary (organized/constrained)
 *   — staff the monitoring machinery, careers bound to it;
 *   allied_elite_factions: beneficiary (powerful/arbitrage) — positions
 *   secured by the new order; subject_populations: primary target
 *   (powerless/trapped) — bear compliance costs, punishment risk, cultural
 *   displacement; traditional_practitioner_communities: target with identity
 *   lock (moderate/identity_locked) — absorb standing enforcement costs to
 *   keep displaced practice alive; peripheral_monitoring_gap_communities:
 *   nominal target (powerless/constrained) — formally bound, de facto
 *   buffered by monitoring distance; local_customary_authorities: excluded
 *   (moderate/trapped) — displaced governors of the old norms, never
 *   consulted; comparative_historical_sociologists: analytical observer —
 *   sees the full structure across episodes.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_mechanism_kernel__exogenous_override_reading, 0.68).
domain_priors:suppression_score(imposition_mechanism_kernel__exogenous_override_reading, 0.82).
domain_priors:theater_ratio(imposition_mechanism_kernel__exogenous_override_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_mechanism_kernel__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(imposition_mechanism_kernel__exogenous_override_reading, "State-Coerced Norm Imposition (Violence-Monopoly Legitimacy Reading)").
narrative_ontology:topic_domain(imposition_mechanism_kernel__exogenous_override_reading, "historical sociology/state formation/cultural authority").

domain_priors:requires_active_enforcement(imposition_mechanism_kernel__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_mechanism_kernel__exogenous_override_reading, '716304f3-da99-4657-99af-c788c178e32f').
narrative_ontology:cs_kernel_codification('716304f3-da99-4657-99af-c788c178e32f', distributed).
narrative_ontology:cs_authority_grounding('716304f3-da99-4657-99af-c788c178e32f', distributed).
narrative_ontology:cs_reading_relation('716304f3-da99-4657-99af-c788c178e32f', imposition_mechanism_kernel__endogenous_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('716304f3-da99-4657-99af-c788c178e32f', imposition_mechanism_kernel__hybrid_legitimation_reading, influences).
narrative_ontology:cs_axiom('716304f3-da99-4657-99af-c788c178e32f', foundational, coercion_precedes_acceptance).
narrative_ontology:cs_axiom_status(coercion_precedes_acceptance, holdable).
narrative_ontology:cs_axiom_grounding('716304f3-da99-4657-99af-c788c178e32f', coercion_precedes_acceptance, empirically_contingent).
narrative_ontology:cs_axiom('716304f3-da99-4657-99af-c788c178e32f', foundational, legitimacy_from_violence_monopoly).
narrative_ontology:cs_axiom_status(legitimacy_from_violence_monopoly, holdable).
narrative_ontology:cs_axiom_grounding('716304f3-da99-4657-99af-c788c178e32f', legitimacy_from_violence_monopoly, empirically_contingent).
narrative_ontology:cs_reference_frame('716304f3-da99-4657-99af-c788c178e32f', coercive_monopoly_norm_order).
narrative_ontology:cs_drift_state('716304f3-da99-4657-99af-c788c178e32f', post_revisionist_archival_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('716304f3-da99-4657-99af-c788c178e32f', '').
narrative_ontology:cs_kernel_id(imposition_mechanism_kernel__exogenous_override_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__exogenous_override_reading, central_state_apparatus).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__exogenous_override_reading, administrative_enforcement_officials).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__exogenous_override_reading, allied_elite_factions).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__exogenous_override_reading, subject_populations).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__exogenous_override_reading, traditional_practitioner_communities).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__exogenous_override_reading, peripheral_monitoring_gap_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts and promulgates the mandated norms (civil registry, calendar, dress, script, schooling, conscription rolls), funds the police and village surveillance network that monitors them, and collects the taxes, fines, and levies that the new legibility makes collectable. It can amend or suspend any mandate by decree and bears the fiscal cost of running the enforcement machinery.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, central_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Staff the prefectural police, household registries, and inspection rounds through which the mandates are applied. Salaries, promotion, and pension depend on enforcement performance; leaving the arrangement means leaving state service altogether. They carry the day-to-day labor of monitoring and punishment that the arrangement runs on.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, administrative_enforcement_officials, beneficiary,
    organized, biographical, constrained, national).

% Landowning and former-status elites whose property rights, tax exemptions, and social precedence the new norm order secures. They supplied the political coalition behind the mandates and can redirect allegiance if the regime's coercive capacity visibly fails.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, allied_elite_factions, beneficiary,
    powerful, generational, arbitrage, national).

% Farmers, townspeople, and workers who must register, dress, educate their children, and serve under rules they were never consulted on. Noncompliance brings fines, conscription penalties, and police visits; collective petition is treated as sedition in the early decades. Relocating means abandoning land, kin networks, and livelihood, so most comply while watched and evade where unwatched.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, subject_populations, payer,
    powerless, biographical, trapped, national).

% Communities attached to the displaced religious and cultural practices. They maintain rites, dress, and teaching privately at continuing legal risk, absorbing fines and occasional prosecution as a standing cost of continuity. Adopting the mandated forms wholesale would dissolve the inherited identity that constitutes the community, and relocating out of the jurisdiction would dissolve the community itself; so they persist in place, paying the enforcement price generation after generation.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, traditional_practitioner_communities, payer,
    moderate, generational, identity_locked, regional).

% Remote villages formally bound by the identical edicts but rarely visited by inspectors. Compliance is nominal and seasonal; old practice continues semi-openly in the gaps. Their evasion space is real but shrinks each time the state extends rail lines, registries, or garrison posts into the region, and each extension converts their de facto latitude into the same enforced compliance the centers already live under.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, peripheral_monitoring_gap_communities, payer,
    powerless, biographical, constrained, regional).

% Village headmen, guild masters, and sectarian teachers who previously governed the now-displaced practices and adjudicated disputes under them. They were displaced from adjudication without being consulted, retain informal influence over whether households actually comply, and have no seat where the mandates are drafted or revised.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, local_customary_authorities, excluded,
    moderate, generational, trapped, local).

% Reconstruct adoption timing and compliance behavior from fiscal, judicial, and parish archives across multiple state-formation episodes. They test whether popular uptake preceded or followed the mandates, and what happens to compliance when enforcement capacity lapses; their findings are the main external check on the enforcing state's own chronicles.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, comparative_historical_sociologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imposition_mechanism_kernel__exogenous_override_reading, central_state_apparatus).
narrative_ontology:fixing_cost_class(imposition_mechanism_kernel__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The mandated uniformities solve a real large-territory coordination problem: a single civil registry, calendar, script, and legal form make contracts enforceable across regions, the population countable and taxable, and administration legible from the center — functions the fragmented customary orders could not deliver at scale.
% TRANSFER_FUNCTION: Moves compliance labor, cultural autonomy, and punishment discretion from subject populations to the state; converts formerly self-governed custom into state-legible activity, enabling taxation, conscription, and fine revenue to flow upward, and concentrates the means of sanctioned coercion in central hands.
% ABSENT_VOICES: Local customary authorities and dissenting sectarian leaders were never seated where the mandates were drafted; the subject populations' consent was neither sought nor recorded. The appearance of normative agreement is produced by criminalizing objection, not by assembling it — unanimity here is an artifact of who was kept out of the room.
% DISAPPEARANCE_RATIONALE: If enforcement vanished overnight, public compliance would decay within the lifespan of the generation that remembers the old practice: registries would go unmaintained, dress and calendar edicts ignored, tax and conscription rolls keyed to the uniform norms would degrade, and the state's administrative reach would contract sharply. Allied elite positions and enforcement careers would unravel with it. Rapid rearrangement is precisely this reading's core prediction — compliance tracks the monitor.
% FOUNDING_PROBLEM: Late-centralizing states confronted fragmented customary orders that resisted taxation, conscription, and uniform administration; the imposed cultural-administrative norms were built to make territory and population legible and governable from the center.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by comparative historical sociology and by administrative and fiscal archives independent of the enforcing state — enforcement expenditure and prosecution volumes that keep rising decades after the consolidation milestones the founding problem describes. The enforcing state's own chronicles attest the problem too, but sit inside the beneficiary set, so corroboration rests on the external scholarly and archival record; no disinterested source attests that the founding problem still requires the current enforcement intensity.
narrative_ontology:disappearance_verdict(imposition_mechanism_kernel__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_mechanism_kernel__exogenous_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_mechanism_kernel__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(imposition_mechanism_kernel__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_mechanism_kernel__exogenous_override_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_mechanism_kernel__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imposition_mechanism_kernel__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imposition_mechanism_kernel__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.68: the compliance burden (registration labor, mandated schooling and dress costs, conscription exposure, fines) plus the standing risk of punishment falls on populations who never consented, while the returns — revenue, legibility, administrative reach — concentrate in the state and its coalition; the burden is decoupled from any bargaining process. Suppression 0.82: persistence depends on actively excluding the alternative (private practice of the old norms), via police networks, household-responsibility surveillance units, and prosecution; this is enforcement dependence, not participant preference. Theater ratio 0.28: enforcement is predominantly functional through the interval — inspections, prosecutions, and registry maintenance are real work — with a growing ceremonial share (compliance exhibitions, loyalty rituals) late in the period; monitoring never becomes purely performative. Accessibility collapse 0.50: alternatives collapse in the public sphere but persist in the private one — partial, not near-total, closure. Resistance 0.70: recurring tax riots, sect persecutions, evasion, and quiet noncompliance run through the whole interval, which is exactly why suppression requirements stay high. The three metric series run on ONE shared eight-point grid (1868-1912) so every tracked metric is authored at every examined time point; trajectories rise steeply during the enforcement-buildout decade, then plateau as the machinery matures while extraction continues accumulating modestly. CLAIM/METRIC INDEPENDENCE: claimed_type tangled_rope is my structural judgment — the same uniformity that genuinely coordinates (legibility, standardization) also asymmetrically burdens the unconsenting — while the metrics above are authored independently as descriptive estimates; the engine computes per-seat classifications from the structural data and may diverge from the claim, and that divergence is signal, not error.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute differently from the same structure. From the central_state_apparatus seat the arrangement is order-building: it inherited fragmentation and manufactured governability, and the enforcement bill is the price of civilization. From the subject_populations and traditional_practitioner_communities seats the same structure operates as unconsented extraction backed by punishment. The enforcement-official seat sees a livelihood and a career ladder, not a moral question. Same-level lateral dynamics: subject_populations and peripheral_monitoring_gap_communities hold identical formal obligations at identical nominal power, yet live under effectively different constraints because monitoring depth differs — the center pays full compliance prices, the periphery pays nominal ones until the rail line arrives. Identity-lock dynamics: traditional_practitioner_communities are bound by relational and ideological identity fusion — the community IS the practice, so exit (assimilation or relocation) dissolves the self, not just the habit; if that identity frame broke (e.g., a generational cohort re-reads the practice as heritage rather than obligation), their exit option would loosen toward constrained and their effective burden would drop without any change in the statute book.
 *
 * DIRECTIONALITY LOGIC:
 *   central_state_apparatus sits nearest the beneficiary pole (d near 0): it authors the rules, controls the mechanism, and could exit by decree — arbitrage-grade exit damps its effective burden toward subsidy. administrative_enforcement_officials derive low d from beneficiary status, slightly offset by the real labor they contribute. allied_elite_factions likewise sit near the beneficiary end with arbitrage exit. subject_populations derive high d from victim status amplified by trapped exit — no mobility, no arbitrage, full exposure to whatever the machinery extracts. traditional_practitioner_communities sit nearest the full-target end: victim status compounded by identity_locked exit, the configuration the derivation treats as maximally exposed. peripheral_monitoring_gap_communities carry victim declarations but constrained exit — monitoring gaps give them partial evasion latitude, placing their d somewhat below the trapped center populations. The engine derives all of this from the declared beneficiaries/victims plus power and exit atoms; no overrides were needed because the exit-option differentiation does the work the overrides otherwise would.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification discipline matters doubly here because both mislabeling directions are live temptations. Reading the arrangement as pure extraction (snare) would erase the genuine coordination it delivers — the registry, the standardized script, and the enforceable contract are real goods the fragmented order could not supply, and abolishing the arrangement wholesale would destroy them along with the rents. Reading it as pure coordination (rope) would launder coercion as consent — the founding problem's solution does not explain why enforcement intensity keeps rising decades after consolidation milestones. The tangled_rope claim holds both facts: coordination function plus asymmetric extraction plus active enforcement dependence. On mandatrophy: the founding problem (fragmentation blocking central administration) was substantially solved by mid-interval, yet enforcement expenditure and prosecution volume kept climbing — the founding_problem_status is therefore authored 'contested', and the status-x-verdict combination (contested x world_rearranges) leaves the capture/zombie determination to the engine's mismatch consumer rather than asserting it in the narrative, which is the field the battery treats as most gameable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_allocation,
    'Is the exogenous_override_reading the correct instantiation of imposition_mechanism_kernel for the canonical norm waves, or do the endogenous_climb_reading or hybrid_legitimation_reading better fit the adoption record?',
    'Archival adoption-timing reconstruction across the episode class: date popular uptake of each mandated norm relative to the mandate''s promulgation and to enforcement intensity, using fiscal, judicial, and parish records independent of the enforcing state''s chronicles.',
    'If the climb reading fits, enforcement costs and suppression fall sharply and the arrangement computes nearer a voluntary coordination form with low epsilon; if the hybrid fits, intermediate values obtain; this file''s high-extraction, high-suppression profile stands only if the override reading fits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_allocation, conceptual, 'Which sibling reading of the imposition-mechanism kernel the adoption record actually supports.').

omega_variable(
    compliance_monitoring_conditionality,
    'Is compliance actually conditional on state monitoring, as this reading''s core premise asserts, or has the norm achieved durable acceptance independent of enforcement?',
    'Natural experiments: compliance levels during administrative breakdowns (war-mobilization strain, occupation, fiscal crisis) and in monitoring-gap peripheries versus monitored centers; post-relaxation rebound studies where enforcement was temporarily withdrawn.',
    'Rapid compliance decay in unmonitored conditions confirms the override structure and sustains high effective extraction indefinitely; durable compliance refutes the reading''s foundational premise and shifts classification toward the climb reading''s low-enforcement profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_monitoring_conditionality, empirical, 'Whether compliance tracks the monitor (override signature) or persists without it (acceptance signature).').

omega_variable(
    coordination_extraction_separability,
    'Is the coordination value of the uniform norms (legibility, standardization, enforceable contracts) separable from the coercive manner of their imposition?',
    'Counterfactual comparison with territories that adopted comparable uniformities through negotiated or federated standardization at similar speed; decompose the compliance burden into a standardization price and a sanction premium.',
    'If separable, a larger share of measured extraction is imposition overhead removable by procedural change without losing the coordination good; if inseparable, part of the burden is the irreducible price of the coordination itself and the tangled_rope reading is strengthened against the snare alternative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the arrangement''s coordination function and its coercive excess can be structurally pried apart.').

omega_variable(
    resistance_suppression_equilibrium,
    'Does the mid-interval decline in overt resistance indicate growing acceptance of the norms, or merely maturing suppression capacity?',
    'Private-practice continuity records (sectarian registers, household observance data), prosecution-volume series normalized per capita, and the timing of practice rebound wherever enforcement relaxed.',
    'If suppression-maintained, the suppression requirement stays high indefinitely and the arrangement remains enforcement-dependent for its entire life; if acceptance-driven, enforcement needs decay and the trajectory bends toward the climb reading''s profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resistance_suppression_equilibrium, empirical, 'Whether quiet is consent or successfully policed dissent — the equilibrium this reading''s persistence rests on.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_mechanism_kernel__exogenous_override_reading, 1868, 1912).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t1868, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 1868, 0.12).
narrative_ontology:measurement_basis(impo_tr_t1868, observed).
narrative_ontology:measurement(impo_tr_t1874, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 1874, 0.16).
narrative_ontology:measurement_basis(impo_tr_t1874, observed).
narrative_ontology:measurement(impo_tr_t1880, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 1880, 0.19).
narrative_ontology:measurement_basis(impo_tr_t1880, observed).
narrative_ontology:measurement(impo_tr_t1886, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 1886, 0.21).
narrative_ontology:measurement_basis(impo_tr_t1886, observed).
narrative_ontology:measurement(impo_tr_t1892, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 1892, 0.23).
narrative_ontology:measurement_basis(impo_tr_t1892, observed).
narrative_ontology:measurement(impo_tr_t1898, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 1898, 0.24).
narrative_ontology:measurement_basis(impo_tr_t1898, observed).
narrative_ontology:measurement(impo_tr_t1905, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 1905, 0.26).
narrative_ontology:measurement_basis(impo_tr_t1905, observed).
narrative_ontology:measurement(impo_tr_t1912, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 1912, 0.28).
narrative_ontology:measurement_basis(impo_tr_t1912, observed).

% Extraction over time
narrative_ontology:measurement(impo_be_t1868, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 1868, 0.54).
narrative_ontology:measurement_basis(impo_be_t1868, observed).
narrative_ontology:measurement(impo_be_t1874, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 1874, 0.57).
narrative_ontology:measurement_basis(impo_be_t1874, observed).
narrative_ontology:measurement(impo_be_t1880, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 1880, 0.6).
narrative_ontology:measurement_basis(impo_be_t1880, observed).
narrative_ontology:measurement(impo_be_t1886, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 1886, 0.62).
narrative_ontology:measurement_basis(impo_be_t1886, observed).
narrative_ontology:measurement(impo_be_t1892, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 1892, 0.64).
narrative_ontology:measurement_basis(impo_be_t1892, observed).
narrative_ontology:measurement(impo_be_t1898, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 1898, 0.65).
narrative_ontology:measurement_basis(impo_be_t1898, observed).
narrative_ontology:measurement(impo_be_t1905, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 1905, 0.67).
narrative_ontology:measurement_basis(impo_be_t1905, observed).
narrative_ontology:measurement(impo_be_t1912, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 1912, 0.68).
narrative_ontology:measurement_basis(impo_be_t1912, observed).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t1868, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 1868, 0.58).
narrative_ontology:measurement_basis(impo_su_t1868, observed).
narrative_ontology:measurement(impo_su_t1874, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 1874, 0.66).
narrative_ontology:measurement_basis(impo_su_t1874, observed).
narrative_ontology:measurement(impo_su_t1880, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 1880, 0.71).
narrative_ontology:measurement_basis(impo_su_t1880, observed).
narrative_ontology:measurement(impo_su_t1886, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 1886, 0.75).
narrative_ontology:measurement_basis(impo_su_t1886, observed).
narrative_ontology:measurement(impo_su_t1892, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 1892, 0.77).
narrative_ontology:measurement_basis(impo_su_t1892, observed).
narrative_ontology:measurement(impo_su_t1898, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 1898, 0.78).
narrative_ontology:measurement_basis(impo_su_t1898, observed).
narrative_ontology:measurement(impo_su_t1905, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 1905, 0.8).
narrative_ontology:measurement_basis(impo_su_t1905, observed).
narrative_ontology:measurement(impo_su_t1912, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 1912, 0.82).
narrative_ontology:measurement_basis(impo_su_t1912, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_mechanism_kernel__exogenous_override_reading, identity_coordination).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__exogenous_override_reading, imposition_mechanism_kernel__endogenous_climb_reading).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__exogenous_override_reading, imposition_mechanism_kernel__hybrid_legitimation_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of imposition_mechanism_kernel per the epsilon-invariance principle: the colloquial question 'how did the new norms gain legitimacy?' covers three structurally distinct claims and is authored as three files. This file (exogenous_override_reading) authors high epsilon (~0.68), high suppression (~0.82), and heavy enforcement dependence. The endogenous_climb_reading file authors low enforcement cost and low suppression (mandate ratified existing acceptance — nearer a rope profile). The hybrid_legitimation_reading file authors intermediate values (symbolic authority transfer plus institutional incentives). Upstream/downstream structure: the climb reading is the higher-confidence baseline in the revisionist literature, and the override reading's documented enforcement machinery is the evidence any hybrid formulation must accommodate — hence this file's edges run to both siblings. Each file carries a single stable epsilon over the same standing arrangement (the imposed norm order as enforced); the epsilons differ because the READINGS differ, not because the observable varies within any one file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
