% ============================================================================
% CONSTRAINT STORY: unclos_maritime_sovereignty__expansive_construction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_maritime_sovereignty__expansive_construction_reading, []).

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
 *   constraint_id: unclos_maritime_sovereignty__expansive_construction_reading
 *   human_readable: Expansive Construction Reading: Artificial Islands Generating Territorial Waters Through Effective Occupation
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   Since the mid-1990s, capable littoral states have dredged submerged reefs
 *   and low-tide elevations into fortified, permanently administered
 *   installations and treated the surrounding twelve nautical miles as
 *   territorial sea under their control — patrolling, licensing fisheries,
 *   and turning back foreign vessels. This story instantiates ONE reading of
 *   the unclos_maritime_sovereignty kernel: the
 *   expansive_construction_reading, under which effective occupation of
 *   constructed features generates territorial waters. The sibling readings —
 *   strict_geographic_reading (only natural high-tide formations qualify;
 *   construction alters nothing) and hybrid_effective_control_reading
 *   (natural features generate full zones; artificial features receive 500m
 *   safety zones that may mature absent challenge) — are separate constraints
 *   with their own epsilon values and victim sets, linked through
 *   network.affects_constraints. The epsilon referent throughout is the
 *   standing de facto arrangement — construction plus garrisoned
 *   administration yielding enforced maritime bands — assessed by this
 *   reading's own lights; the reading endorses the arrangement yet still
 *   records the asymmetric structure its own delta declaration names:
 *   constructors gain, neighboring claimants and freedom-of-navigation
 *   interests pay.
 *
 * KEY AGENTS:
 *   - island_constructing_states: agenda-setting constructor (institutional/constrained) — builds, garrisons, patrols, and administers; collects the generated zones
 *   - neighboring_claimant_states: primary target (organized/trapped) — loses waters adjacent to its own coasts
 *   - freedom_of_navigation_states: secondary target (institutional/mobile) — absorbs precedential and operational costs
 *   - traditional_fishing_communities: diffuse target (powerless/trapped) — expelled from generational grounds
 *   - marine_dredging_contractors: incidental beneficiary (powerful/arbitrage) — collects construction revenue
 *   - arbitral_tribunals: analytical observer (institutional/analytical) — adjudicates the kernel's meaning without enforcement reach
 *   - small_island_developing_states: excluded voice (organized/constrained) — bears precedent risk without a seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__expansive_construction_reading, 0.7).
domain_priors:suppression_score(unclos_maritime_sovereignty__expansive_construction_reading, 0.62).
domain_priors:theater_ratio(unclos_maritime_sovereignty__expansive_construction_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__expansive_construction_reading, tangled_rope).
narrative_ontology:human_readable(unclos_maritime_sovereignty__expansive_construction_reading, "Expansive Construction Reading: Artificial Islands Generating Territorial Waters Through Effective Occupation").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__expansive_construction_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__expansive_construction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__expansive_construction_reading, 'fdf5849f-a323-4bbb-b715-ce211e7d69a1').
narrative_ontology:cs_kernel_codification('fdf5849f-a323-4bbb-b715-ce211e7d69a1', fixed_text).
narrative_ontology:cs_authority_grounding('fdf5849f-a323-4bbb-b715-ce211e7d69a1', extraction).
narrative_ontology:cs_interpretation_layer_present('fdf5849f-a323-4bbb-b715-ce211e7d69a1').
narrative_ontology:cs_reading_relation('fdf5849f-a323-4bbb-b715-ce211e7d69a1', unclos_maritime_sovereignty__strict_geographic_reading, forecloses).
narrative_ontology:cs_reading_relation('fdf5849f-a323-4bbb-b715-ce211e7d69a1', unclos_maritime_sovereignty__hybrid_effective_control_reading, influences).
narrative_ontology:cs_axiom('fdf5849f-a323-4bbb-b715-ce211e7d69a1', foundational, effective_occupation_confers_maritime_title).
narrative_ontology:cs_axiom_status(effective_occupation_confers_maritime_title, holdable).
narrative_ontology:cs_axiom_grounding('fdf5849f-a323-4bbb-b715-ce211e7d69a1', effective_occupation_confers_maritime_title, conventional).
narrative_ontology:cs_axiom('fdf5849f-a323-4bbb-b715-ce211e7d69a1', secondary, labor_investment_generates_sovereign_desert).
narrative_ontology:cs_axiom_status(labor_investment_generates_sovereign_desert, holdable).
narrative_ontology:cs_axiom_grounding('fdf5849f-a323-4bbb-b715-ce211e7d69a1', labor_investment_generates_sovereign_desert, deontological).
narrative_ontology:cs_reference_frame('fdf5849f-a323-4bbb-b715-ce211e7d69a1', effective_control_title_regime).
narrative_ontology:cs_drift_state('fdf5849f-a323-4bbb-b715-ce211e7d69a1', post_arbitral_award_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('fdf5849f-a323-4bbb-b715-ce211e7d69a1', '').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__expansive_construction_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__expansive_construction_reading, island_constructing_states).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__expansive_construction_reading, marine_dredging_contractors).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__expansive_construction_reading, neighboring_claimant_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__expansive_construction_reading, freedom_of_navigation_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__expansive_construction_reading, traditional_fishing_communities).
narrative_ontology:constraint_vindicates(unclos_maritime_sovereignty__expansive_construction_reading, effective_occupation_doctrine).
narrative_ontology:constraint_vindicates(unclos_maritime_sovereignty__expansive_construction_reading, maritime_prescription_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Dredge submerged reefs and low-tide elevations into fortified multipurpose installations, then station garrisons, coast guard flotillas, and administrative personnel on them year-round. They publish baselines, register administrative districts, and patrol a declared band of water around each feature, turning back foreign vessels. Having sunk capital and national prestige into the constructions, they cannot abandon them without forfeiting the entire position, so continuation is compulsory; their alternatives are negotiation or litigation, both pursued from the strength of the occupied ground.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, island_constructing_states, agenda_setter,
    institutional, generational, constrained, regional).

% Hold overlapping historic claims to the same reefs and waters and watch constructed features mature into exclusion zones beside their own coastlines. They lose fisheries access, face encirclement of their own outposts, and see hydrocarbon blocks they had licensed fall inside someone else's administered band. Geography fixes them in place: they cannot move away from waters adjacent to their shores, so their options reduce to protest, parallel construction they can ill afford, arbitration the constructing state rejects, or escalation they dread.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, neighboring_claimant_states, payer,
    organized, generational, trapped, regional).

% Major naval and trading powers whose vessels transit the region and whose doctrine treats unimpeded passage as a global interest. Each constructed feature that generates an enforced territorial band adds a segment of water their warships must either request permission to enter — conceding the generating claim — or contest through scheduled passages. They can redeploy forces globally and route commerce around friction points, so the cost to them is strategic and precedential rather than existential.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, freedom_of_navigation_states, payer,
    institutional, generational, mobile, global).

% Village fleets from neighboring coasts have worked the shoals and lagoons for generations. Coast guard expulsions, water-cannonings, and radio warnings now meet them inside grounds their grandparents fished freely; catch declines as distant-water industrial fleets take the remainder. Their boats cannot fish elsewhere profitably and their households cannot relocate, so each enforced exclusion band lands directly on their livelihood with no compensating channel.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, traditional_fishing_communities, payer,
    powerless, biographical, trapped, regional).

% International dredging and marine construction firms receive multi-billion-dollar contracts to pump, pile, and armor the features. Revenue scales with each new construction cycle regardless of which state commissions it; when one jurisdiction's program pauses, they bid the next. They collect from the arrangement without setting its terms.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, marine_dredging_contractors, beneficiary,
    powerful, immediate, arbitrage, global).

% Convention-established tribunals hear challenges to occupation-generated claims, apply the codified definitions of island and low-tide elevation, and issue awards that the constructing state may reject. They shape the written record of the kernel's meaning but command no fleet to make their findings operative on the water.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, arbitral_tribunals, observer,
    institutional, generational, analytical, global).

% Pacific and Caribbean island states whose security rests on the fixed-baseline and natural-feature regime the convention guarantees. If constructed landmasses can generate territorial seas, the protection their geography depends on becomes purchasable by any capable power. They object through UN resolutions and alliance statements but hold no seat where the constructions and their accompanying claims are actually made.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, small_island_developing_states, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_maritime_sovereignty__expansive_construction_reading, island_constructing_states).
narrative_ontology:fixing_cost_class(unclos_maritime_sovereignty__expansive_construction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts ambiguous sovereignty over remote reefs and low-tide elevations into determinate, administrable spheres: whoever occupies, builds, and continuously administers a feature organizes the surrounding water around that administration, replacing indefinite multi-party standoff with a single identifiable controlling authority.
% TRANSFER_FUNCTION: Moves maritime space and its yields — fisheries grounds, hydrocarbon exploration blocks, navigational priority, strategic depth — from neighboring claimant states and open-access navigation to whichever state constructs and garrisons the feature, with construction contracts flowing to international dredging firms.
% ABSENT_VOICES: Traditional fishing communities are never seated where the waters they fish are allocated; nonclaimant regional states and small island developing states, whose baseline protections the precedent erodes, object through UN forums but hold no seat where the facts are made.
% DISAPPEARANCE_RATIONALE: If the occupation-generates-waters regime vanished overnight, every constructed feature would revert to a non-zone-generating status, neighboring claimants would resume parallel use of the shoals, naval passage rules would snap back to the codified convention's categories, and billions in construction value would become strategically inert — the regional order built on the occupied ground would dissolve faster than any replacement could form.
% FOUNDING_PROBLEM: After decolonization left hundreds of reefs and low-tide elevations with overlapping or vacant claims, no authority exercised on-site control: incidents went unattributed, fishermen and surveyors operated without a governing power, and rival claimants froze in indefinite standoff. Demonstrated occupation promised to convert presence into determinate title.
% FOUNDING_PROBLEM_CORROBORATION: Constructing-state white papers attest the ambiguity remains live wherever administration lapses. Arbitral jurisprudence and the codified convention text — sources outside the benefiting parties — attest the opposite: the convention denies artificial islands any territorial sea, answering the founding problem by category rather than by occupation, which makes the arrangement's persistence a departure from the codified answer rather than its continuation. No corroborating source outside the constructing states supports the reading that occupation-based generation remains necessary.
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__expansive_construction_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__expansive_construction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__expansive_construction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unclos_maritime_sovereignty__expansive_construction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_maritime_sovereignty__expansive_construction_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_maritime_sovereignty__expansive_construction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_maritime_sovereignty__expansive_construction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_maritime_sovereignty__expansive_construction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness sits at 0.70 because the arrangement transfers whole maritime zones — fisheries grounds, seabed blocks, navigational priority — from neighbors to the constructor, and the transfer grows with each completed installation. Suppression (0.62) is authored as a raw structural property, unscaled: it reflects the coercive machinery the arrangement requires — permanent garrisons, coast guard expulsion of fishing fleets, radar-enforced warning zones, a challenge-or-permit posture toward warships — and nothing else; the engine applies directionality and scope scaling to extractiveness alone. Theater_ratio (0.45) is high-moderate because a large share of the administrative-control apparatus — ceremonial annexation events, registered administrative districts, civilian resettlement programs, dedicated post offices — exists to perform occupation for legal audiences rather than to govern a resident population numbering in the hundreds; the runways, ports, and radars are functional, which keeps the ratio below piton territory. Accessibility_collapse (0.58): once construction and garrisoning complete, a challenger's alternatives collapse sharply — dislodging installations means war — but not completely, since litigation, parallel construction, and diplomatic coalitions remain partly open. Resistance (0.60) is substantial and continuous: an adverse arbitral award, standing freedom-of-navigation operations, rival claimant construction, and coalition diplomacy all press against the arrangement without displacing it. Claimed type is tangled_rope on structural grounds independent of these scores: the occupation-title rule is a genuine, ancient coordination device that resolves standoff by creating determinate authority, AND the same structure carries asymmetric transfer with named payers, held up by active enforcement — both halves are present, which is the tangled-rope definition; the metrics describe how heavily the extraction half currently operates. The measurement series run on one shared time grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from the same structure. From the constructing state's seat the arrangement is legitimate title acquisition it labored for and defends — coordination it performs, not imposition it suffers; its directionality sits near the beneficiary pole and effective extraction inverts toward subsidy. From the trapped neighbor's seat the identical structure is uncompensated dispossession by fait accompli — full-target directionality amplified by immobility. The mobile freedom-of-navigation seat experiences a third version: a precedential tax paid in operational friction, dampened by its ability to redeploy globally. The powerless fishing communities sit nearest full-target with no damping at all. The engine derives these divergences from the declared roles, power atoms, and exit options; the divergence between the constructor's self-understanding and the neighbors' experience is the perspectival gap this corpus exists to measure.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: island_constructing_states (declared in base_properties, seated as agenda_setter) derive near-zero directionality — the generated zones subsidize them; marine_dredging_contractors likewise sit near the beneficiary pole with arbitrage-grade exit. Victims: neighboring_claimant_states (trapped, organized) derive high directionality — trapped targets sit nearer the full-target end than mobile ones; traditional_fishing_communities (trapped, powerless) sit at the extreme target end; freedom_of_navigation_states are declared victims but their mobility and global scope damp their effective extraction below the trapped seats'. Regional scope on the core seats keeps verification feasible enough that enforcement, not distance, does the work. No directionality overrides are needed: the beneficiary/victim declarations plus exit options already produce the correct ordering (constructor < contractor < freedom-of-navigation states < neighbor claimants < fishing communities), so the structural derivation chain suffices.
 *
 * MANDATROPHY ANALYSIS:
 *   The occupation-title rule began life as a rope: a genuine settlement device for genuinely ambiguous sovereignty over uninhabited features, with broad buy-in because every state might someday occupy something. Mandatrophy here is partial and contested rather than resolved: the codified convention answered the founding problem by category — artificial islands generate no territorial sea — which renders the expansive practice a revival of pre-convention custom against the treaty bargain, not a response to a still-open coordination gap. Treating the arrangement as pure snare would erase the real coordination residue (determinate authority does replace standoff around each administered feature); treating it as pure rope would erase the named payers and the enforcement dependence. The tangled-rope classification holds both truths apart instead of collapsing them, and the R5 mismatch consumer can read the contested founding-problem status against the world-rearranges verdict without the genealogy narrative being consumed as a claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_membership,
    'This constraint is one reading of the unclos_maritime_sovereignty kernel; what structurally changes if the strict_geographic_reading or hybrid_effective_control_reading is instantiated instead?',
    'Comparative classification across the three sibling stories: victim sets, epsilon, and enforcement dependence are re-derived per reading; convergence or divergence in computed types locates the disagreement''s material weight.',
    'Under the strict reading the constructor''s generated zones vanish and the victim set collapses toward empty (nothing is extracted because nothing is generated); under the hybrid reading a maturation threshold splits victims into challenged and unchallenged classes. This file''s classifications hold only for the expansive reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_membership, conceptual, 'Committer structure: which kernel reading this constraint instantiates and what sibling readings would change.').

omega_variable(
    title_generation_locus,
    'Where does maritime title attach — to natural formation, to demonstrated control, or to a maturation gradient between them — and can any single framework hold more than one locus?',
    'Doctrinal analysis of whether the three readings'' criteria are logically compatible within one legal framework, tracked against state practice and judicial treatment of constructed features.',
    'If the loci are logically exclusive, the expansive reading forecloses the strict reading outright; if gradable, the readings blend and the victim sets merge rather than partition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(title_generation_locus, conceptual, 'Location of the inter-reading disagreement: the criterion for island status.').

omega_variable(
    maturation_vs_codified_bar,
    'Does prolonged unchallenged administration of a constructed feature ever override the codified bar denying artificial islands a territorial sea, or does the bar persist regardless of duration?',
    'Track whether any constructed feature''s generated zone achieves general acquiescence across claimant and nonclaimant states over a full generational horizon; acquiescence breadth is the observable.',
    'If duration defeats the bar, the arrangement hardens toward custom and its transfers become self-executing; if the bar persists, the arrangement remains permanently enforcement-dependent and never matures past contested de facto status.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(maturation_vs_codified_bar, empirical, 'Whether the expansive reading can mature into settled custom against the codified rule.').

omega_variable(
    enforcement_dependence_test,
    'Is the generated-zone regime maintained by continuous coercion, or has it acquired independent acceptance that would survive an enforcement pause?',
    'Counterfactual from enforcement lapses: observe whether fishing fleets and foreign warships re-enter the bands during garrison rotations, budget shortfalls, or crisis drawdowns, and whether re-entry triggers durable renegotiation.',
    'Survival without enforcement would push the arrangement toward rope-like consolidation; full relapse would confirm enforcement dependence and keep the tangled-rope/snare boundary live.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_dependence_test, empirical, 'Whether persistence rests on coercion or on consolidated acceptance.').

omega_variable(
    precedent_universalization_cost,
    'If the expansive reading were universalized, what happens to the fixed-baseline protections that geographically vulnerable states rely on — and does the reading generalize or remain a positional advantage of capable powers?',
    'Count constructed-feature claims admitted by other powers following the precedent; the universal adoption rate measures whether the reading scales into a general regime or stays a great-power exception.',
    'Universalization would redistribute who benefits and could restore a coordination character to the arrangement; failure to spread would confirm it as positional advantage rather than rule.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precedent_universalization_cost, preference, 'Whether the reading generalizes across states or remains concentrated advantage.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__expansive_construction_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t0, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(uncl_tr_t0, observed).
narrative_ontology:measurement(uncl_tr_t6, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 6, 0.26).
narrative_ontology:measurement_basis(uncl_tr_t6, observed).
narrative_ontology:measurement(uncl_tr_t12, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 12, 0.38).
narrative_ontology:measurement_basis(uncl_tr_t12, observed).
narrative_ontology:measurement(uncl_tr_t18, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 18, 0.47).
narrative_ontology:measurement_basis(uncl_tr_t18, observed).
narrative_ontology:measurement(uncl_tr_t24, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 24, 0.46).
narrative_ontology:measurement_basis(uncl_tr_t24, observed).
narrative_ontology:measurement(uncl_tr_t30, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 30, 0.45).
narrative_ontology:measurement_basis(uncl_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(uncl_be_t0, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(uncl_be_t0, observed).
narrative_ontology:measurement(uncl_be_t6, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 6, 0.42).
narrative_ontology:measurement_basis(uncl_be_t6, observed).
narrative_ontology:measurement(uncl_be_t12, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement_basis(uncl_be_t12, observed).
narrative_ontology:measurement(uncl_be_t18, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 18, 0.66).
narrative_ontology:measurement_basis(uncl_be_t18, observed).
narrative_ontology:measurement(uncl_be_t24, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 24, 0.69).
narrative_ontology:measurement_basis(uncl_be_t24, observed).
narrative_ontology:measurement(uncl_be_t30, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 30, 0.7).
narrative_ontology:measurement_basis(uncl_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t0, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(uncl_su_t0, observed).
narrative_ontology:measurement(uncl_su_t6, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 6, 0.36).
narrative_ontology:measurement_basis(uncl_su_t6, observed).
narrative_ontology:measurement(uncl_su_t12, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 12, 0.5).
narrative_ontology:measurement_basis(uncl_su_t12, observed).
narrative_ontology:measurement(uncl_su_t18, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 18, 0.6).
narrative_ontology:measurement_basis(uncl_su_t18, observed).
narrative_ontology:measurement(uncl_su_t24, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 24, 0.62).
narrative_ontology:measurement_basis(uncl_su_t24, observed).
narrative_ontology:measurement(uncl_su_t30, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement_basis(uncl_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__expansive_construction_reading, resource_allocation).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, unclos_maritime_sovereignty__strict_geographic_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, unclos_maritime_sovereignty__hybrid_effective_control_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'UNCLOS maritime sovereignty over artificial islands' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints sharing one kernel: this expansive reading (construction plus occupation generates territorial waters; epsilon high, victims named), the strict geographic reading (construction is legally inert; epsilon negligible, no victims), and the hybrid effective-control reading (maturation gradient; intermediate epsilon with a conditional victim set). Measuring 'the' rule with different observables yields different epsilon because they are different constraints. Family links run through affects_constraints; the strict reading is upstream (codified text, highest institutional confidence) and exerts pressure on both downstream readings, while this expansive reading pressures the hybrid middle position with each completed construction cycle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
