% ============================================================================
% CONSTRAINT STORY: separation_of_powers_text__formalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_separation_of_powers_text__formalist_reading, []).

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
 *   constraint_id: separation_of_powers_text__formalist_reading
 *   human_readable: Separation of Powers as Impermeable Boundary — Formalist Nondelegation Reading
 *   domain: constitutional_law/political_theory/administrative_law
 *
 * SUMMARY:
 *   This story models the standing American delegation settlement — Congress
 *   writes broad statutory mandates, expert agencies fill in binding rules,
 *   courts sustain the arrangement through deference doctrines — as assessed
 *   by the formalist reading of the separation-of-powers kernel. Per the
 *   epsilon-referent rule for kernel readings, the referent is the standing
 *   arrangement under contest, NOT the strict-boundary design this reading
 *   endorses (that alternative would trivialize epsilon to zero); the
 *   formalist authors high epsilon for the delegation settlement as the
 *   formalist sees it: a transfer of the core legislative power away from
 *   electorally accountable institutions, sustained by actively maintained
 *   judicial deference and by legislators' own interest in blame avoidance.
 *   The reading dual-lists federal_agency_bureaucracies as beneficiary and
 *   victim: agencies operationally collect the transferred authority, yet
 *   under this reading that authority was never validly theirs, so they carry
 *   a permanent legitimacy liability and stand as the designated casualties
 *   of any restoration. The claim/metric split is deliberate: claimed_type
 *   records what I judge structurally true of the settlement (a genuine
 *   coordination function wrapped around asymmetric extraction, requiring
 *   active enforcement), while the metrics record its operation as
 *   descriptively seen from this reading's seat; where the engine's per-seat
 *   computations diverge from the claim, that divergence is the datum.
 *
 * KEY AGENTS:
 *   - - federal_agency_bureaucracies: Primary beneficiary-agenda-setter (institutional/identity_locked) — wields the transferred lawmaking authority; simultaneously designated casualty under this reading
 *   - - career_legislators: Primary beneficiary (powerful/arbitrage) — collect credit and shed blame through the delegation structure
 *   - - incumbent_regulated_industries: Secondary beneficiary/payer (organized/constrained) — shape implementation detail, absorb and pass through compliance costs
 *   - - executive_office_of_the_president: Dual-positioned beneficiary/payer (institutional/constrained) — gains policy delivery, loses direct control
 *   - - national_electorate: Primary payer (powerless/trapped) — governed by rules made by officials it cannot elect or remove
 *   - - small_regulated_businesses: Primary payer (powerless/constrained) — bear regressive compliance costs without process capacity
 *   - - state_governments: Payer (organized/constrained) — displaced from preempted policy space
 *   - - united_states_federal_judiciary: Agenda-setter (institutional/generational) — enforces the settlement's deference and is the sole actor able to reverse it
 *   - - formalist_jurists_and_scholars: Analytical observer (analytical/analytical) — sees the full structure and holds the excluded alternative design
 *   - - original_ratifying_public: Excluded voice (powerless/trapped) — the absent ratifiers whose bargain this reading claims to enforce
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__formalist_reading, 0.78).
domain_priors:suppression_score(separation_of_powers_text__formalist_reading, 0.72).
domain_priors:theater_ratio(separation_of_powers_text__formalist_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__formalist_reading, tangled_rope).
narrative_ontology:human_readable(separation_of_powers_text__formalist_reading, "Separation of Powers as Impermeable Boundary — Formalist Nondelegation Reading").
narrative_ontology:topic_domain(separation_of_powers_text__formalist_reading, "constitutional_law/political_theory/administrative_law").

domain_priors:requires_active_enforcement(separation_of_powers_text__formalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__formalist_reading, '46a6058e-e24d-44e1-8f81-35687a99778d').
narrative_ontology:cs_kernel_codification('46a6058e-e24d-44e1-8f81-35687a99778d', fixed_text).
narrative_ontology:cs_authority_grounding('46a6058e-e24d-44e1-8f81-35687a99778d', lineage).
narrative_ontology:cs_interpretation_layer_present('46a6058e-e24d-44e1-8f81-35687a99778d').
narrative_ontology:cs_reading_relation('46a6058e-e24d-44e1-8f81-35687a99778d', separation_of_powers_text__functionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('46a6058e-e24d-44e1-8f81-35687a99778d', separation_of_powers_text__unitary_executive_reading, influences).
narrative_ontology:cs_axiom('46a6058e-e24d-44e1-8f81-35687a99778d', foundational, legislative_authority_nontransferable).
narrative_ontology:cs_axiom_status(legislative_authority_nontransferable, holdable).
narrative_ontology:cs_axiom_grounding('46a6058e-e24d-44e1-8f81-35687a99778d', legislative_authority_nontransferable, deontological).
narrative_ontology:cs_axiom('46a6058e-e24d-44e1-8f81-35687a99778d', secondary, accountability_chain_requires_elected_rulemaker).
narrative_ontology:cs_axiom_status(accountability_chain_requires_elected_rulemaker, holdable).
narrative_ontology:cs_axiom_grounding('46a6058e-e24d-44e1-8f81-35687a99778d', accountability_chain_requires_elected_rulemaker, instrumental).
narrative_ontology:cs_reference_frame('46a6058e-e24d-44e1-8f81-35687a99778d', ratified_impermeable_trias_politica).
narrative_ontology:cs_drift_state('46a6058e-e24d-44e1-8f81-35687a99778d', contemporary_administrative_state, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('46a6058e-e24d-44e1-8f81-35687a99778d', '').
narrative_ontology:cs_kernel_id(separation_of_powers_text__formalist_reading, separation_of_powers_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers_text__formalist_reading, federal_agency_bureaucracies).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__formalist_reading, career_legislators).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__formalist_reading, incumbent_regulated_industries).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__formalist_reading, executive_office_of_the_president).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, national_electorate).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, small_regulated_businesses).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, state_governments).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, federal_agency_bureaucracies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, incumbent_regulated_industries).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, executive_office_of_the_president).
narrative_ontology:constraint_vindicates(separation_of_powers_text__formalist_reading, strict_trias_politica).
narrative_ontology:constraint_vindicates(separation_of_powers_text__formalist_reading, legislative_vesting_clause_fidelity).
narrative_ontology:constraint_vindicates(separation_of_powers_text__formalist_reading, popular_accountability_chain).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Career civil services and appointed leadership of bodies such as EPA, SEC, and OSHA. They draft and enforce the rules that carry the force of law within mandates Congress wrote in broad strokes; rulemaking output, budgets, and headcount flow to them. Under this reading their core activity is held void — the lawmaking authority they exercise was never validly transferred to them — so they operate under a permanent legitimacy challenge, divert resources to defending their enabling statutes, and face piecemeal dispossession whenever a court narrows a delegation. Leaving the enterprise means abandoning a professional identity built around the administrative mission.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, federal_agency_bureaucracies, agenda_setter,
    institutional, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__formalist_reading, federal_agency_bureaucracies, beneficiary).

% Members of Congress across decades of service. They vote for sweeping statutory mandates, claim credit for the resulting protections, and redirect blame to agencies when particular rules prove unpopular; the hardest policy choices get made in comment periods and courtrooms rather than on recorded roll calls. Individually they can retire into lobbying or agency-adjacent work that monetizes the relationships the arrangement creates.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, career_legislators, beneficiary,
    powerful, biographical, arbitrage, national).

% Large established firms in regulated sectors. They participate heavily in notice-and-comment processes, shape implementation detail, and gain from compliance regimes whose fixed costs fall hardest on smaller rivals; they also pay substantial compliance and litigation costs of their own. Exiting the regulated market is rarely viable, so they work the process instead.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, incumbent_regulated_industries, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__formalist_reading, incumbent_regulated_industries, payer).

% The President and White House staff. Delegation lets an administration deliver policy through friendly agency leadership without winning legislation, but the same arrangement places day-to-day lawmaking in hands the White House controls only indirectly, through appointment, budget proposals, and centralized review. Their leverage is real but partial, and each administration inherits its predecessors' agency workforce and rules.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, executive_office_of_the_president, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__formalist_reading, executive_office_of_the_president, payer).

% The voting public. Rules that bind them are written day-to-day by officials they never elected and cannot remove; their recourse runs through distant elections of representatives who voted only for broad mandates. There is no exit from the constitutional order that allocates lawmaking this way.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, national_electorate, payer,
    powerless, generational, trapped, national).

% Small firms subject to federal rules. They bear compliance costs without the legal staff, comment-process capacity, or lobbying presence that larger competitors deploy; for many the realistic options are absorbing the costs, passing them to customers, or closing.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, small_regulated_businesses, payer,
    powerless, biographical, constrained, national).

% State legislatures and agencies. Federal rules preempt their policy space in areas from environmental protection to insurance, and they hold no seat in the federal rulemaking process that displaces them; their recourse is litigation, interstate coordination, and political pressure on Congress.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, state_governments, payer,
    organized, generational, constrained, regional).

% Article III courts, and especially the Supreme Court. They police the boundary between statute and implementing rule and currently tolerate broad delegation under deference doctrines, while a growing bloc of judges argues the tolerance is itself unconstitutional. They are the only actor positioned to restore strict boundaries by majority decision, and their maintenance of deference is what holds the present arrangement in place.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, united_states_federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Judges, academics, and practitioners in the nondelegation tradition. They publish, testify, and dissent against the settlement; they command no enforcement machinery of their own and depend on appointment luck and doctrinal openings. From their seat the whole structure is visible, including the alternative design the present arrangement excludes.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, formalist_jurists_and_scholars, observer,
    analytical, civilizational, analytical, national).

% The generations who ratified the constitutional text assigning lawmaking to Congress alone. They are absent — reachable only through the text, ratification records, and tradition — and the formalist reading claims to speak for the bargain they struck. No living party represents them except by interpretive argument.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, original_ratifying_public, excluded,
    powerless, civilizational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(separation_of_powers_text__formalist_reading, federal_agency_bureaucracies).
narrative_ontology:fixing_cost_class(separation_of_powers_text__formalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the scale-and-expertise problem of modern governance: a continental economy and technical regulatory domains exceed what a large, deliberatively slow, part-time-expertise legislature can specify in advance. Delegation converts general statutory mandates into administrable, updateable rules and concentrates technical judgment in dedicated bodies.
% TRANSFER_FUNCTION: Moves lawmaking authority from elected representatives to unelected agency professionals; moves compliance costs onto regulated parties, regressively onto smaller firms; moves political blame from legislators to agencies; moves policy-shaping influence toward organized incumbents with comment-process capacity.
% ABSENT_VOICES: The ratifying public whose Article I bargain assigned lawmaking to Congress alone is literally absent — dead, represented only by text and tradition; the formalist reading speaks for them by interpretive argument. Also absent: diffuse consumers and citizens who bear rule costs without comment-process participation, and state legislatures excluded from the federal rulemaking space that preempts them.
% DISAPPEARANCE_RATIONALE: If the delegation settlement vanished overnight — every delegation of legislative authority void — the administrative state would collapse: thousands of existing rules would lose their force, Congress would have to legislate specifics it has no capacity to produce quickly, regulatory output would crater, and markets would reorganize around litigation, state-level regulation, and whatever interim statutes Congress managed. The rearrangement would be among the largest in American institutional history.
% FOUNDING_PROBLEM: The New Deal capacity crisis: a national industrial economy had outgrown a nineteenth-century legislative machine. Congress could not draft, and could not keep updated, detailed national rules for securities, airwaves, labor, agriculture, and finance, so it wrote broad mandates and created expert agencies to fill them in.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: economic historians and administrative-law scholars across the methodological spectrum attest that the New Deal capacity gap was real, and even committed formalist critics concede the expertise problem exists while disputing the constitutional remedy. Contemporaneous congressional committee records and testimony corroborate the legislative-incapacity diagnosis. The settlement's own beneficiaries also attest the problem, but the cross-cutting attestation from its opponents distinguishes this genealogy from a cover story.
narrative_ontology:disappearance_verdict(separation_of_powers_text__formalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(separation_of_powers_text__formalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(separation_of_powers_text__formalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(separation_of_powers_text__formalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(separation_of_powers_text__formalist_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(separation_of_powers_text__formalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(separation_of_powers_text__formalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(separation_of_powers_text__formalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78 at interval end) because the settlement relocates the core Article I function — making binding law — to bodies with no electoral principal, and because the transfer compounds: each broadened mandate widens the space agencies fill without new consent. Suppression (0.72) is authored as a raw structural property and is deliberately NOT scaled by power or scope — the engine owns any scaling; the raw value reflects that the strict-separation alternative is actively marginalized inside official institutions by deference precedent, congressional drafting conventions, and agency self-defense, surviving mainly in scholarship and dissents. Theater ratio (0.45) is moderate and rising: oversight hearings, notice-and-comment participation, and presidential management rhetoric increasingly perform accountability that the structure does not deliver, while a real core of expert administration continues. Accessibility collapse (0.65) is substantial but not total: once the settlement's premises are granted, the functionalist design space is the only admissible one, yet the strict design remains articulable from outside. Resistance (0.55) is real — the nondelegation revival, major-questions decisions, and recurring dissent blocs — but has not overturned the settlement. The temporal series run on ONE shared seven-point grid (every tracked metric authored at every examined time point) showing a monotonic ratchet: extraction and enforcement infrastructure rose together from the New Deal consolidation through the codification of deference, plateauing as normalization set in; there is no oscillation to model, so no cyclical apparatus is invoked. Coalition potential among the payer seats (electorate, small business, states) is noted and discounted: the settlement's costs are diffuse and its beneficiaries concentrated, which is precisely the collective-action shape the arrangement exploits.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute sharply different types from identical structural data. From the national_electorate seat (payer, trapped, generational horizon) the settlement operates as enforced extraction with no exit — the closest to a pure-snare experience. From the career_legislators seat (beneficiary, arbitrage) the same structure is a blame-laundering machine they would not redesign. From the federal_agency_bureaucracies seat the structure is experienced as mission plus siege: identity-fused professionals defending their enterprise while this reading holds its foundation void — the identity-lock here is professional and institutional (career civil-service identity constituted by the administrative mission); if that frame broke, agency staff would stop defending rulemaking authority and the settlement's enforcement would lose its inner ring. From the formalist_jurists_and_scholars seat the whole structure is visible at once, including the alternative design every participant seat treats as unthinkable. The engine computes this divergence; the authored claim adjudicates nothing.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (agencies, legislators, incumbents, presidency) derive low directionality for those seats; victim declarations (electorate, small business, states) derive high directionality. The dual listing of federal_agency_bureaucracies in both arrays makes the automatic derivation ambiguous for the institutional seats — agencies collect operational authority (pull toward beneficiary) while carrying the legitimacy liability and dispossession risk this reading assigns them (pull toward target), and the judiciary and presidency hold similarly mixed positions the binary arrays cannot express. A single directionality override pins the institutional power atom to 0.40: near-symmetric with mild beneficiary tilt, matching agencies' dual position, the judiciary's enforcer stake in the settlement it maintains, and the presidency's genuine two-sided exposure. Without the override, the derivation would either strand the institutional seats on a canonical fallback or split them incoherently across the arrays.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy is declared: the founding problem — legislative incapacity at national-industrial scale — is live, and the R5 mismatch consumer reads founding_problem_status=live against disappearance_verdict=world_rearranges, yielding no zombie flag. The classification discipline prevents mislabeling in both directions: the settlement is not a piton, because its function is actively performed and its gains are demonstrably captured (named receipt seat, not diffuse); and it is not a pure snare from every seat, because the coordination function is real and even this reading's opponents concede the expertise problem it addresses. The tangled_rope claim encodes exactly that hybrid: genuine coordination delivered through a structure that simultaneously transfers unaccountable authority. What the formalist reading adds to the corpus is the strongest-available reading-indexed epsilon over the shared referent — the functionalist sibling will author lower epsilon over the same arrangement, and the delta between them is the measured disagreement of the kernel.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story is one reading (formalist_reading) of the separation_of_powers_text kernel, with epsilon, beneficiaries, and victims indexed to that reading over the shared referent of the delegation settlement. Does the formalist indexing correctly locate the settlement''s burden in the broken consent chain, or does it overweight formal validity against functional performance?',
    'Comparative per-seat computation across the three sibling stories: the functionalist sibling authors lower epsilon over the same referent (accountability-through-oversight judged adequate); the unitary-executive sibling relocates the burden to presidential-control loss. Divergence patterns across the triplet localize the disagreement.',
    'If the consent-chain location is wrong, this reading''s high epsilon is an artifact of its own axiom set rather than a structural finding, and the settlement''s classification migrates toward the functionalist profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed epsilon over a shared kernel referent; committer structure routed here per Rule 2.').

omega_variable(
    enforcement_trajectory_contestation,
    'Is the formalist constraint''s enforcement rising into a restoration phase (major-questions decisions, post-deference erosion, a durable dissent bloc on the Court), or plateauing as a rhetorical position that never commands five votes?',
    'Track Supreme Court composition and boundary-policing case outcomes over 2020-2040: count delegations narrowed or struck versus upheld, and whether deference doctrines are formally repudiated.',
    'A rising trajectory pushes the settlement from stable tangled_rope toward unraveling dynamics, raises the agencies'' directionality toward full target, and dates a type transition; a plateau confirms the current profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_trajectory_contestation, empirical, 'Whether the strict-boundary constraint is resurgent or rhetorically stalled.').

omega_variable(
    agency_dual_position_authenticity,
    'The formalist reading dual-lists agencies as beneficiary and victim — collectors of void authority and designated casualties of restoration. Is the victim designation structurally real, or rhetorical cover for the reading''s own program?',
    'Observe agency behavior under narrowing precedents: resource diversion into defending enabling statutes, mission shrinkage, hiring freezes tied to legitimacy challenges. Real casualty dynamics show measurable defensive expenditure; rhetorical designation shows none.',
    'If purely rhetorical, agencies'' directionality drops toward pure beneficiary and the settlement reads as cleaner extraction from the public alone; if real, the dual position stands and the restoration path carries identifiable institutional losses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agency_dual_position_authenticity, conceptual, 'Authenticity of the agencies'' victim designation under this reading.').

omega_variable(
    founding_problem_capacity_resolution,
    'Does the founding problem — legislative incapacity at national-industrial scale — still require delegation-grade solutions, or could modern legislative capacity (professional staff, information technology, committee specialization, delegation within Congress itself) absorb detailed rulemaking?',
    'Comparative institutional analysis: state legislatures and parliamentary systems that legislate in detail, plus counterfactual costing of converting major agency rulemakings to statutes.',
    'If legislative capacity exists, the settlement''s coordination justification weakens sharply and its profile migrates toward pure extraction; if it does not, the coordination leg of the tangled_rope claim is secured.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_capacity_resolution, empirical, 'Whether the founding problem still necessitates the delegation solution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__formalist_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sepa_tr_t0, separation_of_powers_text__formalist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(sepa_tr_t0, observed).
narrative_ontology:measurement(sepa_tr_t15, separation_of_powers_text__formalist_reading, theater_ratio, 15, 0.3).
narrative_ontology:measurement_basis(sepa_tr_t15, observed).
narrative_ontology:measurement(sepa_tr_t30, separation_of_powers_text__formalist_reading, theater_ratio, 30, 0.34).
narrative_ontology:measurement_basis(sepa_tr_t30, observed).
narrative_ontology:measurement(sepa_tr_t45, separation_of_powers_text__formalist_reading, theater_ratio, 45, 0.38).
narrative_ontology:measurement_basis(sepa_tr_t45, observed).
narrative_ontology:measurement(sepa_tr_t60, separation_of_powers_text__formalist_reading, theater_ratio, 60, 0.41).
narrative_ontology:measurement_basis(sepa_tr_t60, observed).
narrative_ontology:measurement(sepa_tr_t75, separation_of_powers_text__formalist_reading, theater_ratio, 75, 0.44).
narrative_ontology:measurement_basis(sepa_tr_t75, observed).
narrative_ontology:measurement(sepa_tr_t90, separation_of_powers_text__formalist_reading, theater_ratio, 90, 0.45).
narrative_ontology:measurement_basis(sepa_tr_t90, observed).

% Extraction over time
narrative_ontology:measurement(sepa_be_t0, separation_of_powers_text__formalist_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(sepa_be_t0, observed).
narrative_ontology:measurement(sepa_be_t15, separation_of_powers_text__formalist_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement_basis(sepa_be_t15, observed).
narrative_ontology:measurement(sepa_be_t30, separation_of_powers_text__formalist_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(sepa_be_t30, observed).
narrative_ontology:measurement(sepa_be_t45, separation_of_powers_text__formalist_reading, base_extractiveness, 45, 0.72).
narrative_ontology:measurement_basis(sepa_be_t45, observed).
narrative_ontology:measurement(sepa_be_t60, separation_of_powers_text__formalist_reading, base_extractiveness, 60, 0.75).
narrative_ontology:measurement_basis(sepa_be_t60, observed).
narrative_ontology:measurement(sepa_be_t75, separation_of_powers_text__formalist_reading, base_extractiveness, 75, 0.77).
narrative_ontology:measurement_basis(sepa_be_t75, observed).
narrative_ontology:measurement(sepa_be_t90, separation_of_powers_text__formalist_reading, base_extractiveness, 90, 0.78).
narrative_ontology:measurement_basis(sepa_be_t90, observed).

% Suppression requirement over time
narrative_ontology:measurement(sepa_su_t0, separation_of_powers_text__formalist_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(sepa_su_t0, observed).
narrative_ontology:measurement(sepa_su_t15, separation_of_powers_text__formalist_reading, suppression_requirement, 15, 0.58).
narrative_ontology:measurement_basis(sepa_su_t15, observed).
narrative_ontology:measurement(sepa_su_t30, separation_of_powers_text__formalist_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement_basis(sepa_su_t30, observed).
narrative_ontology:measurement(sepa_su_t45, separation_of_powers_text__formalist_reading, suppression_requirement, 45, 0.7).
narrative_ontology:measurement_basis(sepa_su_t45, observed).
narrative_ontology:measurement(sepa_su_t60, separation_of_powers_text__formalist_reading, suppression_requirement, 60, 0.71).
narrative_ontology:measurement_basis(sepa_su_t60, observed).
narrative_ontology:measurement(sepa_su_t75, separation_of_powers_text__formalist_reading, suppression_requirement, 75, 0.72).
narrative_ontology:measurement_basis(sepa_su_t75, observed).
narrative_ontology:measurement(sepa_su_t90, separation_of_powers_text__formalist_reading, suppression_requirement, 90, 0.72).
narrative_ontology:measurement_basis(sepa_su_t90, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(separation_of_powers_text__formalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(separation_of_powers_text__formalist_reading, separation_of_powers_text__functionalist_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__formalist_reading, separation_of_powers_text__unitary_executive_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__formalist_reading, chevron_deference_doctrine).
narrative_ontology:affects_constraint(separation_of_powers_text__formalist_reading, major_questions_doctrine).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'separation of powers' covers three structurally distinct constraint stories — formalist (this file), functionalist, and unitary-executive readings of the same ratified text. All three share one referent (the standing delegation settlement) and author different epsilon over it; they are linked as siblings rather than merged, because merging would average away the very disagreement the corpus exists to measure. Upstream of all three sits the ratified constitutional text (highest empirical confidence); this reading exerts downstream pressure on chevron_deference_doctrine (whose erosion is this reading's enforcement vector) and on major_questions_doctrine (its partial judicial proxy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(separation_of_powers_text__formalist_reading, institutional, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
