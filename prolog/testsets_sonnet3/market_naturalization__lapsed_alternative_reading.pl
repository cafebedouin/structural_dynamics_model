% ============================================================================
% CONSTRAINT STORY: market_naturalization__lapsed_alternative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_naturalization__lapsed_alternative_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: market_naturalization__lapsed_alternative_reading
 *   human_readable: Market Dominance as Lapsed Coordination Closure (No Active Beneficiary)
 *   domain: political_economy/economic_history
 *
 * SUMMARY:
 *   This story instantiates the 'lapsed alternative' reading of the
 *   market_naturalization kernel: the dominant market arrangement in a given
 *   sector is not actively defended by any beneficiary class. Instead, its
 *   persistence is explained entirely by the atrophy of switching
 *   infrastructure and alternative-practice knowledge through decades of
 *   non-use following an originally legitimate coordination closure. Under
 *   this reading, extraction is low (coordination/switching costs only) and
 *   there is no identifiable rent-collecting agent — the arrangement behaves
 *   like eroded sediment, not a maintained wall. This is decision-relevant
 *   against the sibling readings: the beneficiary_maintained_reading would
 *   author high extractiveness and a concentrated beneficiary class actively
 *   defending the arrangement through lobbying, litigation, or exclusionary
 *   contracts; the hybrid_reading would author moderate extractiveness with a
 *   partial beneficiary class alongside genuine atrophy. Each reading is
 *   authored as its own constraint with its own stable epsilon per the
 *   epsilon-invariance principle; they are linked via
 *   network.affects_constraints, not merged into one story with a measurement
 *   parameter.
 *
 * KEY AGENTS:
 *   - incumbent_market_structure: agenda_setter (institutional/analytical) — administers a structure with no active extraction ledger
 *   - would_be_market_entrants: payer (moderate/constrained) — bear switching costs from atrophied alternatives
 *   - displaced_alternative_practitioners: payer (powerless/trapped) — lost practice infrastructure through generational disuse
 *   - economic_historians: observer (analytical/analytical) — document erosion-resistant sediment, not maintained wall
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_naturalization__lapsed_alternative_reading, 0.18).
domain_priors:suppression_score(market_naturalization__lapsed_alternative_reading, 0.22).
domain_priors:theater_ratio(market_naturalization__lapsed_alternative_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__lapsed_alternative_reading, piton).
narrative_ontology:human_readable(market_naturalization__lapsed_alternative_reading, "Market Dominance as Lapsed Coordination Closure (No Active Beneficiary)").
narrative_ontology:topic_domain(market_naturalization__lapsed_alternative_reading, "political_economy/economic_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__lapsed_alternative_reading, '0fdf62d0-c7b9-41d0-a7e4-306c2506297a').
narrative_ontology:cs_kernel_codification('0fdf62d0-c7b9-41d0-a7e4-306c2506297a', distributed).
narrative_ontology:cs_authority_grounding('0fdf62d0-c7b9-41d0-a7e4-306c2506297a', distributed).
narrative_ontology:cs_reading_relation('0fdf62d0-c7b9-41d0-a7e4-306c2506297a', market_naturalization__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_reading_relation('0fdf62d0-c7b9-41d0-a7e4-306c2506297a', market_naturalization__hybrid_reading, influences).
narrative_ontology:cs_axiom('0fdf62d0-c7b9-41d0-a7e4-306c2506297a', foundational, persistence_without_agency_is_possible).
narrative_ontology:cs_axiom_status(persistence_without_agency_is_possible, holdable).
narrative_ontology:cs_axiom_grounding('0fdf62d0-c7b9-41d0-a7e4-306c2506297a', persistence_without_agency_is_possible, empirically_contingent).
narrative_ontology:cs_axiom('0fdf62d0-c7b9-41d0-a7e4-306c2506297a', secondary, absence_of_collector_implies_no_extraction_target).
narrative_ontology:cs_axiom_status(absence_of_collector_implies_no_extraction_target, holdable).
narrative_ontology:cs_axiom_grounding('0fdf62d0-c7b9-41d0-a7e4-306c2506297a', absence_of_collector_implies_no_extraction_target, empirically_contingent).
narrative_ontology:cs_reference_frame('0fdf62d0-c7b9-41d0-a7e4-306c2506297a', coordination_closure_at_founding).
narrative_ontology:cs_drift_state('0fdf62d0-c7b9-41d0-a7e4-306c2506297a', contemporary_market_structure, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0fdf62d0-c7b9-41d0-a7e4-306c2506297a', '').
narrative_ontology:cs_kernel_id(market_naturalization__lapsed_alternative_reading, market_naturalization).

% --- Structural relationships ---
narrative_ontology:constraint_victim(market_naturalization__lapsed_alternative_reading, would_be_market_entrants).
narrative_ontology:constraint_victim(market_naturalization__lapsed_alternative_reading, displaced_alternative_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The dominant arrangement (a standard, a firm configuration, a distribution channel) continues to occupy the field not because anyone actively defends it but because the coordination costs of switching were never paid down by any organized actor after the initial closure. No one currently administers it as a going extraction concern; it persists as sunk-cost sediment. There is no ledger of rents collected because no single agent is positioned to collect them.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, incumbent_market_structure, agenda_setter,
    institutional, civilizational, analytical, national).

% Face switching costs and network effects inherited from the original closure. They cannot easily coordinate around the incumbent structure not because someone blocks them, but because the coordination infrastructure for an alternative was allowed to atrophy from disuse decades ago. Their exit is constrained by absence of maintained alternatives, not by active gatekeeping.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, would_be_market_entrants, payer,
    moderate, biographical, constrained, national).

% Practitioners of the historically viable alternative arrangement (a rival standard, a cooperative distribution form, an artisanal production mode) whose practice knowledge and infrastructure decayed through generations of non-use. They bear the cost of the closure's persistence but there is no rent-collecting party to petition or litigate against — the loss is diffuse and structural, attributable to disuse rather than to a defender.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, displaced_alternative_practitioners, payer,
    powerless, generational, trapped, regional).

% Study why the dominant arrangement persists and find no organized beneficiary actively defending it against alternatives — the persistence looks structurally like erosion-resistant sediment rather than a maintained wall. They document the atrophy of the alternative's supporting infrastructure as the primary mechanism, not enforcement.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, economic_historians, observer,
    analytical, civilizational, analytical, global).

% Argue for reviving the lapsed alternative but find no adversary to negotiate with — there is no beneficiary class holding the line, only inertia and the sunk cost of rebuilding switching infrastructure. Their proposals struggle for traction because there is no extraction narrative to mobilize opposition against; the barrier is disuse, which is a harder political target than a defender.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, policy_reform_advocates, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_naturalization__lapsed_alternative_reading, diffuse).
narrative_ontology:fixing_cost_class(market_naturalization__lapsed_alternative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The incumbent arrangement originally solved a genuine coordination problem — settling on one standard, channel, or production mode avoided the costs of parallel incompatible systems. That coordination function was real at the point of closure.
% TRANSFER_FUNCTION: At present, the arrangement moves almost nothing in a directed sense: switching costs are paid by would-be entrants and displaced practitioners to the arrangement's inertia itself, not to any collecting party. What is transferred is opportunity cost from those who might benefit from the alternative to no one in particular.
% ABSENT_VOICES: Displaced alternative practitioners and would-be entrants would argue for coordinated investment to revive the lapsed alternative, but there is no forum because there is no adversarial party — reform advocates report the absence of an opponent as itself an obstacle to organizing.
% DISAPPEARANCE_RATIONALE: If the incumbent arrangement vanished overnight, the reading's own thesis predicts the world would substantially rearrange for entrants and displaced practitioners (switching costs would vanish along with the arrangement), yet no organized actor currently depends on its continuation for extraction — so removal would matter to those bearing the cost of persistence but not to any beneficiary seat, since none exists. This is why the verdict is contested rather than settled: the sibling readings dispute whether removal is even the right counterfactual, since a beneficiary-maintained account would predict active resistance to removal that this reading does not expect to observe.
% FOUNDING_PROBLEM: The arrangement was built to solve a genuine multi-party coordination failure: incompatible standards or channels imposed real transaction costs, and settling on one arrangement — any one — was better than fragmentation.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians, examining archival records of the original standard-setting episode, attest that the coordination problem was resolved decades ago and that no contemporary organized actor invests resources in defending the arrangement against alternatives — the corroboration comes from outside any beneficiary class because this reading holds that no beneficiary class currently exists to self-report.
narrative_ontology:disappearance_verdict(market_naturalization__lapsed_alternative_reading, contested).
narrative_ontology:founding_problem_status(market_naturalization__lapsed_alternative_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_naturalization__lapsed_alternative_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(market_naturalization__lapsed_alternative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_naturalization__lapsed_alternative_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_naturalization__lapsed_alternative_reading_tests).
:- end_tests(market_naturalization__lapsed_alternative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.18 at interval end) and declining over the measured interval, consistent with the reading's thesis that whatever residual extraction existed at closure has decayed toward pure coordination cost as the arrangement settled into inertia rather than being actively rent-extracted. Suppression is likewise low and falling (0.30 to 0.22) — the barrier facing entrants and displaced practitioners is the absence of maintained alternative infrastructure, not active gatekeeping, and that absence itself softens somewhat over time as isolated revival attempts chip at it. Theater ratio is low and rises only marginally (0.10 to 0.15), reflecting that there is very little performative defense of the arrangement to observe, because under this reading there is no defender staging that performance. Accessibility collapse is authored moderately high (0.62) because switching infrastructure genuinely has decayed — the alternative is hard to access — but this is attributed to disuse, not suppression, and resistance is low (0.20) because there is no active adversary against which to resist; would-be reformers report frustration at having no opponent to organize against, which is itself evidence for this reading's core claim.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter seat (incumbent_market_structure) and the payer seats diverge structurally but not adversarially: the incumbent structure has no collecting agent behind it, so it computes closer to a mountain-adjacent inertial artifact from the analytical seat, while the payer seats (entrants, displaced practitioners) experience real costs regardless of whether anyone collects them. This is the signature the piton classification is meant to capture — extraction from many, concentrated benefit to none, persistence by inertia rather than defense. The claimed_type piton is authored deliberately distinct from the metrics: the metrics alone (low extractiveness, low suppression) might read as approaching rope or even mountain, but the presence of identifiable payers with no coordinating collector, and the absence of any active administrator with incentive to fix it, is the structural piton signature independent of the metric magnitudes.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries are declared under this reading — that is the reading's central structural claim, not an oversight. Victims (would_be_market_entrants, displaced_alternative_practitioners) are declared because real costs are borne, even though no beneficiary collects the corresponding gain; this is the piton signature (diffuse cost, no concentrated collector) rather than the snare/tangled_rope signature (concentrated collector + victim). The incumbent_market_structure stakeholder is authored as agenda_setter because it retains formal position to be changed, but its exit_options are marked analytical rather than arbitrage/institutional-beneficiary because under this reading no one occupies that seat as a maintaining agent — it is a structure without an operator, not an operator with high exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem was a genuine coordination failure (incompatible standards/channels), now authored as dead: the coordination problem the arrangement was built to solve was resolved long ago, yet the arrangement persists. This is precisely the mandatrophy pattern the R5 genealogy interview is designed to surface — but under this reading, the persistence is NOT mandatrophy of extraction (no one benefits from perpetuating a solved problem to keep collecting) but mandatrophy of inertia (no one bears the concentrated cost of fixing it either). The founding_problem_status=dead paired with disappearance_verdict=contested captures this precisely: a naive mismatch check would flag dead+would-rearrange as a capture/zombie signal, but here the rearrangement (if it occurred) would benefit diffuse victims rather than dispossess a concentrated beneficiary — the flag should route to inertial-piton refinement rather than snare/capture refinement, which is exactly what distinguishes this reading from beneficiary_maintained_reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    atrophy_vs_active_defense_ambiguity,
    'Is the observed persistence of the dominant arrangement genuinely explained by atrophy through non-use, or does careful investigation reveal a beneficiary class quietly maintaining switching costs (e.g., through patent thickets, exclusive contracts, or lobbying that is simply less visible than the beneficiary_maintained_reading assumes)?',
    'Forensic institutional history: trace whether any organized actor has invested resources (lobbying expenditure, litigation, exclusive contracting) in maintaining the arrangement''s dominance within the measured interval, versus whether the arrangement''s persistence tracks purely with the decay rate of alternative-practice infrastructure and no correlated investment pattern.',
    'If active investment is found, this story''s core premise (no beneficiary class) is falsified for the domain in question and the correct reading shifts to beneficiary_maintained_reading or hybrid_reading — the ε would rise substantially and a beneficiary array would need to be populated. If no such investment is found, this reading is corroborated and the piton classification with no beneficiary is the accurate structural read.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(atrophy_vs_active_defense_ambiguity, empirical, 'Whether the lapsed-closure account or the beneficiary-maintained account better fits the observed persistence mechanism.').

omega_variable(
    counterfactual_revival_cost_attribution,
    'If alternative infrastructure were rebuilt today, would the costs of doing so properly be attributed to natural decay (nobody''s fault, pure entropy) or to a historical failure of some past actor to maintain optionality (a decision, now obscured, that let the alternative lapse)?',
    'Historical reconstruction of the decisions (or absence of decisions) at the moment the alternative arrangement began to lose adoption — was there a specific choice point where maintaining the alternative was foreclosed by an identifiable actor''s decision, even if that actor no longer benefits?',
    'If a specific historical foreclosure decision is identifiable, the current lapsed state may be better understood as a delayed-action snare (extraction was collected at the founding moment, and the current lapsed state is simply the deferred cost) rather than a genuine piton with no historical beneficiary at all. This would not change the current-period ε but would change the genealogical/founding_problem framing substantially.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_revival_cost_attribution, conceptual, 'Whether current lapsed inertia has a hidden historical beneficiary at the founding moment, distinct from any current beneficiary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_naturalization__lapsed_alternative_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_naturalization__lapsed_alternative_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(mark_tr_t8, market_naturalization__lapsed_alternative_reading, theater_ratio, 8, 0.11).
narrative_ontology:measurement(mark_tr_t16, market_naturalization__lapsed_alternative_reading, theater_ratio, 16, 0.12).
narrative_ontology:measurement(mark_tr_t24, market_naturalization__lapsed_alternative_reading, theater_ratio, 24, 0.13).
narrative_ontology:measurement(mark_tr_t32, market_naturalization__lapsed_alternative_reading, theater_ratio, 32, 0.14).
narrative_ontology:measurement(mark_tr_t40, market_naturalization__lapsed_alternative_reading, theater_ratio, 40, 0.15).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_naturalization__lapsed_alternative_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mark_be_t8, market_naturalization__lapsed_alternative_reading, base_extractiveness, 8, 0.28).
narrative_ontology:measurement(mark_be_t16, market_naturalization__lapsed_alternative_reading, base_extractiveness, 16, 0.24).
narrative_ontology:measurement(mark_be_t24, market_naturalization__lapsed_alternative_reading, base_extractiveness, 24, 0.2).
narrative_ontology:measurement(mark_be_t32, market_naturalization__lapsed_alternative_reading, base_extractiveness, 32, 0.19).
narrative_ontology:measurement(mark_be_t40, market_naturalization__lapsed_alternative_reading, base_extractiveness, 40, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t0, market_naturalization__lapsed_alternative_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(mark_su_t8, market_naturalization__lapsed_alternative_reading, suppression_requirement, 8, 0.27).
narrative_ontology:measurement(mark_su_t16, market_naturalization__lapsed_alternative_reading, suppression_requirement, 16, 0.25).
narrative_ontology:measurement(mark_su_t24, market_naturalization__lapsed_alternative_reading, suppression_requirement, 24, 0.23).
narrative_ontology:measurement(mark_su_t32, market_naturalization__lapsed_alternative_reading, suppression_requirement, 32, 0.22).
narrative_ontology:measurement(mark_su_t40, market_naturalization__lapsed_alternative_reading, suppression_requirement, 40, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_naturalization__lapsed_alternative_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(market_naturalization__lapsed_alternative_reading, 0.12).
narrative_ontology:affects_constraint(market_naturalization__lapsed_alternative_reading, market_naturalization__beneficiary_maintained_reading).
narrative_ontology:affects_constraint(market_naturalization__lapsed_alternative_reading, market_naturalization__hybrid_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the market_naturalization kernel, decomposed per the epsilon-invariance principle because the natural-language claim 'market dominance is natural/inevitable' covers structurally distinct claims with different epsilon values. lapsed_alternative_reading authors low, declining extractiveness (0.18, coordination cost only) and no beneficiary class. beneficiary_maintained_reading (sibling) authors substantially higher extractiveness with a concentrated, actively-defending beneficiary class. hybrid_reading (sibling) authors intermediate extractiveness with a partial beneficiary class. All three share the same underlying kernel (market dominance's naturalization) but diverge on whether active maintenance exists, and are linked here via affects_constraints rather than merged into one story with an observable-selection parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
