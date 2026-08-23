% ============================================================================
% CONSTRAINT STORY: commerce_clause_text__originalist_narrow_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_text__originalist_narrow_reading, []).

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
 *   constraint_id: commerce_clause_text__originalist_narrow_reading
 *   human_readable: Commerce Clause — Originalist Narrow Reading (Border-Crossing Trade and Interstate Instrumentalities Only)
 *   domain: constitutional law/federalism/commerce regulation
 *
 * SUMMARY:
 *   A constitutional boundary fixes how far Congress may reach into economic
 *   life: under the reading instantiated here, national authority attaches
 *   only to trade that physically crosses state lines and to the persons and
 *   things moving through interstate channels, while everything occurring
 *   wholly within a state — manufacturing, farming, mining, local sales,
 *   labor conditions — remains governed by that state's police power. The
 *   reading is maintained by judicial invalidation of federal statutes
 *   exceeding the confined scope. KEY AGENTS (by structural relationship): -
 *   state_governments: primary beneficiary (institutional/constrained) —
 *   retains intrastate police power, inherits externality bills; -
 *   supreme_court: agenda_setter (institutional/constrained) — administers
 *   the boundary through judicial review; -
 *   anti_federal_consolidation_advocates: beneficiary
 *   (organized/identity_locked) — the structural constituency for the narrow
 *   scope; - locally_dominant_industries: beneficiary (organized/mobile) —
 *   shielded from national standards; - national_multistate_businesses: payer
 *   and incidental beneficiary (powerful/arbitrage) — bears the patchwork,
 *   captures the guaranteed interstate flow; - externality_bearing_states:
 *   payer (organized/trapped) — absorbs neighbors' spillovers without remedy;
 *   - workers_in_weak_protection_states: payer (powerless/constrained) — no
 *   national labor floor; - federal_lawmakers: payer
 *   (institutional/constrained) — statutes struck, national agendas blocked;
 *   - constitutional_scholars: analytical observer — maps the interpretive
 *   contest. This file instantiates ONE reading of the contested
 *   commerce_clause_text kernel; its siblings (expansive_federal_reading,
 *   substantial_effects_limited_reading) are separate constraint files with
 *   their own epsilon values, beneficiary/victim sets, and classifications.
 *   The referent of epsilon here is the border-crossing confinement
 *   arrangement itself as this reading maintains it — never the expansive
 *   arrangement the sibling file models. Historical arc: ascendant from E.C.
 *   Knight Co. v. United States (1895) through Hammer v. Dagenhart (1918) to
 *   the late-1920s peak, repudiated by the 1937 switch, dormant mid-century,
 *   partially revived by National League of Cities (1976) and United States
 *   v. Lopez (1995), and today a live minority position with an organized
 *   intellectual movement behind it. The claimed type and the authored
 *   metrics are independent facts: the claim records what this reading's
 *   structure is; the metrics record how its operation measured across the
 *   interval.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_text__originalist_narrow_reading, 0.3).
domain_priors:suppression_score(commerce_clause_text__originalist_narrow_reading, 0.35).
domain_priors:theater_ratio(commerce_clause_text__originalist_narrow_reading, 0.26).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, theater_ratio, 0.26).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__originalist_narrow_reading, tangled_rope).
narrative_ontology:human_readable(commerce_clause_text__originalist_narrow_reading, "Commerce Clause — Originalist Narrow Reading (Border-Crossing Trade and Interstate Instrumentalities Only)").
narrative_ontology:topic_domain(commerce_clause_text__originalist_narrow_reading, "constitutional law/federalism/commerce regulation").

domain_priors:requires_active_enforcement(commerce_clause_text__originalist_narrow_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__originalist_narrow_reading, 'e99a8588-450a-41b6-9bd5-00c9db74bda7').
narrative_ontology:cs_kernel_codification('e99a8588-450a-41b6-9bd5-00c9db74bda7', fixed_text).
narrative_ontology:cs_authority_grounding('e99a8588-450a-41b6-9bd5-00c9db74bda7', lineage).
narrative_ontology:cs_interpretation_layer_present('e99a8588-450a-41b6-9bd5-00c9db74bda7').
narrative_ontology:cs_reading_relation('e99a8588-450a-41b6-9bd5-00c9db74bda7', commerce_clause_text__expansive_federal_reading, coexists_with).
narrative_ontology:cs_reading_relation('e99a8588-450a-41b6-9bd5-00c9db74bda7', commerce_clause_text__substantial_effects_limited_reading, influences).
narrative_ontology:cs_axiom('e99a8588-450a-41b6-9bd5-00c9db74bda7', foundational, fixed_ratification_semantics).
narrative_ontology:cs_axiom_status(fixed_ratification_semantics, holdable).
narrative_ontology:cs_axiom_grounding('e99a8588-450a-41b6-9bd5-00c9db74bda7', fixed_ratification_semantics, conventional).
narrative_ontology:cs_axiom('e99a8588-450a-41b6-9bd5-00c9db74bda7', foundational, state_police_power_primacy).
narrative_ontology:cs_axiom_status(state_police_power_primacy, holdable).
narrative_ontology:cs_axiom_grounding('e99a8588-450a-41b6-9bd5-00c9db74bda7', state_police_power_primacy, deontological).
narrative_ontology:cs_reference_frame('e99a8588-450a-41b6-9bd5-00c9db74bda7', ratification_era_commerce_meaning).
narrative_ontology:cs_drift_state('e99a8588-450a-41b6-9bd5-00c9db74bda7', contemporary_integrated_national_economy, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('e99a8588-450a-41b6-9bd5-00c9db74bda7', '').
narrative_ontology:cs_kernel_id(commerce_clause_text__originalist_narrow_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__originalist_narrow_reading, state_governments).
narrative_ontology:constraint_beneficiary(commerce_clause_text__originalist_narrow_reading, anti_federal_consolidation_advocates).
narrative_ontology:constraint_beneficiary(commerce_clause_text__originalist_narrow_reading, locally_dominant_industries).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, workers_in_weak_protection_states).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, externality_bearing_states).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, national_multistate_businesses).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, federal_lawmakers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(commerce_clause_text__originalist_narrow_reading, national_multistate_businesses).
narrative_ontology:constraint_vindicates(commerce_clause_text__originalist_narrow_reading, dual_federalism_doctrine).
narrative_ontology:constraint_vindicates(commerce_clause_text__originalist_narrow_reading, enumerated_powers_limitation).
narrative_ontology:constraint_vindicates(commerce_clause_text__originalist_narrow_reading, fixed_ratification_semantics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Fifty legislatures and governors retain exclusive authority to regulate manufacturing, agriculture, labor conditions, land use, health, and safety within their borders whenever the activity does not itself cross a state line. They write and enforce their own commercial codes, set their own standards, and answer only to their own electorates for intrastate economic policy. What they give up is access to national help when harms originating in a neighboring state spill across their borders, and they inherit the full administrative bill for regulating complex industries alone. Leaving the arrangement is not on the table — a state cannot opt out of the union's constitutional structure — though they can push for friendlier interpretations through litigation and interstate organizations.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, state_governments, beneficiary,
    institutional, generational, constrained, regional).

% Decides, case by case, how far Congress may reach. When a challenged statute regulates activity that neither crosses a state line nor moves through interstate channels, this body either lets the statute stand or strikes it down, and each decision redraws the practical boundary for every regulator in the country. It cannot delegate the question, cannot be overridden short of constitutional amendment, and its members are chosen by the very branches whose statutes it reviews.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, supreme_court, agenda_setter,
    institutional, generational, constrained, national).

% A durable coalition of states'-rights politicians, libertarian jurists, federalism scholars, and citizen movements for whom keeping domestic regulatory decisions close to home is a first-order political commitment rather than a side effect. They litigate, publish, draft model legislation, and supply judicial nominees committed to a narrow federal commerce scope. Their position is bound up with a broader account of what the American constitutional order is for; adopting the opposite view would amount to abandoning the project, not merely revising a policy preference.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, anti_federal_consolidation_advocates, beneficiary,
    organized, civilizational, identity_locked, national).

% Textile mills, mining operators, agricultural processors, and similar employers concentrated in particular states gain when no national standard raises their labor, safety, or environmental costs above what their home legislature requires. They organize politically within their states to keep it that way, and they can relocate across state lines if a host state's rules turn hostile — a flexibility their employees rarely share.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, locally_dominant_industries, beneficiary,
    organized, biographical, mobile, regional).

% Railroads, manufacturers, retailers, and platforms selling across the country depend on goods, capital, and people moving freely between states — a flow the constitutional structure guarantees and polices. At the same time, when their operations touch activity inside individual states, they must comply with as many distinct regulatory regimes as they operate in: fifty sets of labeling, licensing, wage, and product rules where a single national code would do. They manage the patchwork with compliance departments, forum shopping, lobbying in multiple capitals, and facility relocation; few other parties have comparable room to maneuver.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, national_multistate_businesses, payer,
    powerful, biographical, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(commerce_clause_text__originalist_narrow_reading, national_multistate_businesses, beneficiary).

% States downstream of another state's rivers or downwind of its smokestacks absorb pollution, water depletion, and disease generated by activity their neighbors classify as internal and beyond anyone else's reach. They cannot move, cannot regulate the source directly, and under this boundary they cannot call on national law to abate it; their remedies shrink to negotiation, interstate compacts requiring the source state's consent, or suits in the courts.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, externality_bearing_states, payer,
    organized, generational, trapped, regional).

% Mill hands, farm laborers, and miners working in states that choose minimal hours, wage, and safety rules have no national floor beneath them while this boundary holds; whatever protection they receive is whatever their own legislature grants. Moving to a stronger-protection state is possible for some and ruinously expensive for others — skills, family ties, and credit do not travel equally — and organizing politically inside a weak-protection state pits them against the employers who dominate it.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, workers_in_weak_protection_states, payer,
    powerless, biographical, constrained, regional).

% Members of Congress respond to national problems — monopoly, depression, child labor, environmental spillover — with statutes that this boundary sometimes simply erases, along with the constituents' expectations those statutes answered. Their remaining tools are drafting statutes that hew close to channels and instrumentalities, attaching conditions to federal spending, waiting for judicial appointments to shift, or pursuing a constitutional amendment that has succeeded only twenty-seven times in two centuries.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, federal_lawmakers, payer,
    institutional, biographical, constrained, national).

% Law professors, historians, and judges writing outside the case stream reconstruct what the ratifying generation understood 'commerce' to cover, test competing readings against founding-era evidence and modern economics, and supply the arguments every faction borrows. They cast no votes and bear none of the operating costs; their stake is argumentative, and their exit is total.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, constitutional_scholars, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_text__originalist_narrow_reading, state_governments).
narrative_ontology:fixing_cost_class(commerce_clause_text__originalist_narrow_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves a national common market while reserving domestic police power: goods, persons, and money crossing state lines move under one federally guaranteed rule of free flow, with discriminatory state barriers policed under the same text, while manufacturing, agriculture, labor, land use, health, and safety inside each state are governed by that state's own accountable institutions. It solves, once and durably, the twin founding problems of state trade warfare and feared national consolidation.
% TRANSFER_FUNCTION: Moves regulatory authority and the rents that attend it. Reserves intrastate regulatory discretion to state governments; assigns multistate firms the cost of complying with as many distinct state regimes as they touch; leaves the cost of managing cross-border externalities with whichever state hosts (or suffers) the generating activity; denies workers and consumers in weak-protection states any national floor; and removes from federal lawmakers the statutory instruments those constituencies demand.
% ABSENT_VOICES: The people bearing the arrangement's sharpest costs had no voice in fixing it: workers in weak-protection states, downwind communities, and future generations facing problems that require national solutions were absent at ratification and are represented now only obliquely, through legislators whose statutes the boundary can erase. Historically the Progressive and New Deal coalitions objected loudly from inside the system; today the objection is carried by reform movements and by adherents of the sibling readings rather than by any seated party.
% DISAPPEARANCE_RATIONALE: If the boundary vanished overnight, the entire allocation of American regulatory authority would reorganize: Congress could set uniform national standards for labor, environment, and commerce in formerly intrastate domains; state regulatory bureaucracies would shrink or convert to administering federal floors; the multistate compliance patchwork would compress into a single regime; and the organized movements built around the narrow scope would lose their object.
% FOUNDING_PROBLEM: Two problems bargained over at Philadelphia: under the Articles, states were strangling each other's trade with tariffs and discriminatory duties, and the delegates simultaneously feared that a national government with general regulatory power would swallow the states' police functions. The commerce text was cut to solve the first while reassuring the second.
% FOUNDING_PROBLEM_CORROBORATION: Both halves are attested from outside any current beneficiary set: Federalist Nos. 11 and 42 document the state-trade-barriers problem; the Anti-Federalist essays (Brutus, Federal Farmer) document the consolidation fear that produced the narrow assurances; standard histories of the Convention's commerce debates corroborate both halves; and living opponents of this reading concede the state-barriers half while disputing whether the anti-consolidation half still binds a continental economy. Establishing the genealogy requires no testimony from within the current beneficiary set.
narrative_ontology:disappearance_verdict(commerce_clause_text__originalist_narrow_reading, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_text__originalist_narrow_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_text__originalist_narrow_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(commerce_clause_text__originalist_narrow_reading, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_text__originalist_narrow_reading, 0.3, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_text__originalist_narrow_reading_tests).
:- end_tests(commerce_clause_text__originalist_narrow_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.30 for the contemporary profile: the boundary currently binds modestly, but its historical operation peaked at 0.62 (1918–1927) when it erased federal child-labor and antitrust-in-manufacturing remedies, and the series records that arc. Suppression (0.35) is a raw structural property, unscaled by power or scope: within the reading's framework the federal legislative route is foreclosed outright, leaving constitutional amendment (twenty-seven successes in two centuries) and appointment politics as the remaining exits — meaningful but slow. Theater (0.26) is low-moderate: the doctrinal machinery does real gating work whenever invoked, but the 1950s dormancy window pushed the performative-to-functional ratio above 0.5, when remaining invocations were largely ceremonial federalism rhetoric. Accessibility collapse (0.40): alternatives persist — state regulation, interstate compacts, spending-power conditioning, judicial reinterpretation through appointments — so the boundary closes fewer exits than a natural law would. Resistance (0.75) reflects nine decades of sustained assault: the court-packing confrontation, the New Deal repudiation, and continuous scholarly attack. All three tracked series run on one shared ten-point grid (every metric authored at every examined time point; misaligned grids are rejected). The trajectory is cyclical rather than monotonic: enforcement capacity rose with Lochner-era appointments, collapsed at the 1937 switch, and revived partially with the Lopez-era doctrine. The cycle tracks judicial-composition politics interacting with crisis-driven federal agendas — it is a side effect of appointment cycles, not an intermittently reinforced mechanism of gain. Base properties anchor to the interval-end (2026) profile, matching the terminal grid values.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute divergently. From the supreme_court seat the boundary reads as neutral umpiring between two levels of government; from the state_governments and advocate seats it reads as preserved self-government; from the worker, downwind-state, and lawmaker seats it reads as protection withheld — a statute passed by their representatives erased by five votes. Same-level divergence: two 'institutional' seats sit in direct opposition (state governments benefit; federal lawmakers pay) despite equal nominal standing, because the constraint-specific factor — whose regulatory agenda survives — differentiates them, not global power. The multistate-business seat is internally split: it pays the patchwork on everything intrastate-touching yet collects the guaranteed interstate flow its revenue rides on, so no single-seat experience of this boundary exists for it. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality directly: state_governments, anti_federal_consolidation_advocates, and locally_dominant_industries are declared beneficiaries (d toward the subsidized end); workers_in_weak_protection_states, externality_bearing_states, national_multistate_businesses, and federal_lawmakers are declared victims (d toward the target end). Exit modulation sharpens the picture: trapped externality-bearing states sit nearer the full-target end than arbitrage-grade multistate firms, and identity_locked advocates sit nearer the full-beneficiary end than their mobile industry allies. One override: derivation from the victim declaration alone would place national_multistate_businesses near the full-target end, but the same boundary guarantees them the friction-free interstate market their revenues ride on — their net structural position is nearer symmetric — so the powerful atom is overridden to d=0.55. Scope amplification applies modestly at national and continental footprints (verifying fifty parallel regimes is hard); the engine owns that arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this boundary as pure coordination would erase the named victims — the child-labor era proves the confinement can withhold protection catastrophically. Classifying it as pure extraction would erase the genuine dual-sovereignty bargain: the same structure that blocks federal labor law still polices discriminatory state trade barriers, a service every merchant constituency at the founding demanded. The hybrid classification holds both halves. On mandate decay: founding_problem_status is contested, not dead — the state-barrier half of the founding problem remains live (courts still strike discriminatory state measures under the same text), so the dead-status-plus-world_rearranges mismatch flag does not fire; the arrangement persists because half its founding problem persists, not because its function has rotted behind unanimous pretense. Theater behavior corroborates: the ratio peaked (0.55, 1955) during dormancy, when remaining invocations were ceremonial, and fell once the doctrine resumed real gating after Lopez — the signature of a constraint whose performance tracks enforcement intensity, not of an inertial shell maintained as empty ritual.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint instantiates only the originalist narrow reading of the commerce_clause_text kernel; which of the three declared readings (this one, expansive_federal_reading, substantial_effects_limited_reading) actually governs the federal power boundary?',
    'Judicial appointment trajectories, Supreme Court grant patterns in Commerce Clause cases, and doctrinal drift in the nexus and pretext requirements; a sustained five-vote coalition for either alternative reading resolves the contest.',
    'Switching readings relocates regulatory authority wholesale between the federal and state levels and swaps the beneficiary/victim sets: state governments and anti-consolidation advocates gain under this reading and lose under the expansive one; uniform-standard seekers and externality-bearing populations gain under the expansive one and lose here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of the contested commerce text binds; committer-frame uncertainty over the kernel''s operative reading.').

omega_variable(
    narrow_reading_revival_trajectory,
    'Will the narrow reading recover binding force (a Lopez/Morrison-line expansion) or remain a minority position?',
    'Track subsequent Supreme Court holdings on whether intrastate economic activity with aggregate national effects remains regulable, and lower-court uptake of jurisdictional-nexus limits.',
    'Revival drives effective extraction toward the 1918–1935 historical profile (blocking national protective legislation); continued minority status keeps the boundary mostly rhetorical, holding the constraint near its current mild profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(narrow_reading_revival_trajectory, empirical, 'Whether the historical high-extraction phase of this reading recurs.').

omega_variable(
    fragmentation_cost_scale,
    'How large are the compliance and fragmentation costs this boundary actually imposes on multistage firms, relative to the coordination value of the guaranteed national market they simultaneously capture?',
    'Comparative studies of multi-jurisdiction compliance overhead across federations with unified versus divided commercial codes; natural experiments wherever preemption or harmonization occurs in adjacent domains.',
    'A large net fragmentation cost strengthens the asymmetric-extraction reading of the boundary; a negligible net cost supports treating the confinement as near-pure coordination and would push classification toward the rope end.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fragmentation_cost_scale, empirical, 'Net cost position of the dual-positioned multistate-business seat.').

omega_variable(
    externality_remedy_gap,
    'How much cross-border harm — pollution, water depletion, contagion — goes unremedied because the activity generating it counts as intrastate under this boundary?',
    'Inventory interstate externality disputes resolved by compact, negotiation, or litigation versus those abandoned; compare abatement outcomes in domains where national authority later attached.',
    'A large unremedied stock identifies and quantifies the principal victim class of the confinement and raises the measured extraction borne by downwind and downstream populations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(externality_remedy_gap, empirical, 'Scale of the unremedied externality burden the boundary leaves with trapped states.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__originalist_narrow_reading, 1895, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1895, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1895, 0.2).
narrative_ontology:measurement(comm_tr_t1905, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1905, 0.22).
narrative_ontology:measurement(comm_tr_t1918, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1918, 0.26).
narrative_ontology:measurement(comm_tr_t1927, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1927, 0.3).
narrative_ontology:measurement(comm_tr_t1937, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1937, 0.35).
narrative_ontology:measurement(comm_tr_t1955, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1955, 0.55).
narrative_ontology:measurement(comm_tr_t1976, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1976, 0.4).
narrative_ontology:measurement(comm_tr_t1995, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1995, 0.3).
narrative_ontology:measurement(comm_tr_t2012, commerce_clause_text__originalist_narrow_reading, theater_ratio, 2012, 0.28).
narrative_ontology:measurement(comm_tr_t2026, commerce_clause_text__originalist_narrow_reading, theater_ratio, 2026, 0.26).

% Extraction over time
narrative_ontology:measurement(comm_be_t1895, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1895, 0.42).
narrative_ontology:measurement(comm_be_t1905, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1905, 0.5).
narrative_ontology:measurement(comm_be_t1918, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1918, 0.62).
narrative_ontology:measurement(comm_be_t1927, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1927, 0.6).
narrative_ontology:measurement(comm_be_t1937, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1937, 0.25).
narrative_ontology:measurement(comm_be_t1955, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1955, 0.18).
narrative_ontology:measurement(comm_be_t1976, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1976, 0.28).
narrative_ontology:measurement(comm_be_t1995, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1995, 0.36).
narrative_ontology:measurement(comm_be_t2012, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 2012, 0.33).
narrative_ontology:measurement(comm_be_t2026, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 2026, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1895, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 1895, 0.45).
narrative_ontology:measurement(comm_su_t1905, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 1905, 0.55).
narrative_ontology:measurement(comm_su_t1918, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 1918, 0.65).
narrative_ontology:measurement(comm_su_t1927, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 1927, 0.7).
narrative_ontology:measurement(comm_su_t1937, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 1937, 0.3).
narrative_ontology:measurement(comm_su_t1955, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 1955, 0.15).
narrative_ontology:measurement(comm_su_t1976, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 1976, 0.3).
narrative_ontology:measurement(comm_su_t1995, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 1995, 0.4).
narrative_ontology:measurement(comm_su_t2012, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 2012, 0.38).
narrative_ontology:measurement(comm_su_t2026, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 2026, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_text__originalist_narrow_reading, resource_allocation).
narrative_ontology:affects_constraint(commerce_clause_text__originalist_narrow_reading, expansive_federal_reading).
narrative_ontology:affects_constraint(commerce_clause_text__originalist_narrow_reading, substantial_effects_limited_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'the Commerce Clause.' The label conflates three structurally distinct constraints — rival readings of one fixed text — each with its own stable epsilon, its own beneficiary/victim sets, and its own classification. Upstream/downstream ordering is historical: the originalist narrow reading (this file) was the operative constraint from ratification practice through 1937; the expansive federal reading displaced it and governed the post-Wickard regulatory state; the substantial-effects limited reading emerged as a mediated position after Lopez. This reading structurally influences the limited reading (its revival supplied the jurisdictional-nexus and non-pretext machinery the limited reading now uses) and coexists with the expansive reading across judicial coalitions. Epsilon differs sharply across the family: this file's referent (border-crossing confinement) measures 0.30 at interval end with a 0.62 historical peak; the expansive reading's referent (near-plenary federal reach) carries a different victim set and a different value; the limited reading's referent sits between. No observable-dependent epsilon appears in any member file — the decomposition, not a measurement parameter, absorbs the variation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(commerce_clause_text__originalist_narrow_reading, powerful, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
