% ============================================================================
% CONSTRAINT STORY: refugee_convention_text__restrictive_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_refugee_convention_text__restrictive_sovereignty_reading, []).

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
 *   constraint_id: refugee_convention_text__restrictive_sovereignty_reading
 *   human_readable: Refugee Convention Restrictive-Sovereignty Reading (Minimum Floor, Maximum Discretion)
 *   domain: international law / migration governance / human rights
 *
 * SUMMARY:
 *   This story instantiates the restrictive-sovereignty reading of the 1951
 *   Refugee Convention kernel: the text as a minimum floor beneath which
 *   states will not go, above which maximum sovereign discretion prevails —
 *   individualized persecution proof for well-founded fear, particular social
 *   group confined to immutable characteristics with state awareness,
 *   admissibility screening and offshore processing left open. The ε referent
 *   is the standing arrangement under contest — the operative
 *   asylum-determination and border-enforcement regime maintained by
 *   restrictive-reading states — assessed by this reading's own lights: the
 *   floor is treated as real and binding, the discretion above it as
 *   legitimate, and the costs of the arrangement as distributed as the narrow
 *   criteria distribute them. Per the ε-invariance principle, the sibling
 *   readings are separate constraints with their own files, victim sets, and
 *   ε values, linked through network.affects_constraints; nothing about them
 *   is averaged into this story. The claimed type and the authored metrics
 *   are independent: the claim asserts a genuine coordination floor entangled
 *   with asymmetric extraction, and the metrics describe the arrangement's
 *   actual operation without being tuned to any predicted engine output. KEY
 *   AGENTS (by structural relationship): - destination_state_executives:
 *   agenda-setter (institutional/arbitrage) — administers the floor, captures
 *   discretion and political credit - restrictionist_political_movements:
 *   beneficiary (organized/mobile) — collects electoral rents without
 *   administering anything - excluded_asylum_claimants: primary target
 *   (powerless/trapped) — bears the narrowed criteria's costs -
 *   offshore_detained_claimants: target (powerless/trapped) — bears the
 *   screening-and-containment costs - frontline_host_states: burden-bearing
 *   payer (institutional/constrained) — absorbs displaced populations as
 *   gates narrow - border_interdicted_would_be_claimants: pre-procedural
 *   target (powerless/trapped) — removed from the conversation entirely -
 *   unhcr_supervisory_mission: supervisory observer
 *   (institutional/analytical) — documents, advises, cannot bind -
 *   national_asylum_decision_bodies: adjudicative observer
 *   (institutional/analytical) — applies the reading with residual discretion
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__restrictive_sovereignty_reading, 0.7).
domain_priors:suppression_score(refugee_convention_text__restrictive_sovereignty_reading, 0.84).
domain_priors:theater_ratio(refugee_convention_text__restrictive_sovereignty_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 0.84).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__restrictive_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(refugee_convention_text__restrictive_sovereignty_reading, "Refugee Convention Restrictive-Sovereignty Reading (Minimum Floor, Maximum Discretion)").
narrative_ontology:topic_domain(refugee_convention_text__restrictive_sovereignty_reading, "international law / migration governance / human rights").

domain_priors:requires_active_enforcement(refugee_convention_text__restrictive_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__restrictive_sovereignty_reading, '78db88e8-80f4-4049-b4bb-63646ffbcc3a').
narrative_ontology:cs_kernel_codification('78db88e8-80f4-4049-b4bb-63646ffbcc3a', fixed_text).
narrative_ontology:cs_authority_grounding('78db88e8-80f4-4049-b4bb-63646ffbcc3a', lineage).
narrative_ontology:cs_interpretation_layer_present('78db88e8-80f4-4049-b4bb-63646ffbcc3a').
narrative_ontology:cs_reading_relation('78db88e8-80f4-4049-b4bb-63646ffbcc3a', refugee_convention_text__expansive_humanitarian_reading, forecloses).
narrative_ontology:cs_reading_relation('78db88e8-80f4-4049-b4bb-63646ffbcc3a', refugee_convention_text__procedural_integrity_reading, coexists_with).
narrative_ontology:cs_axiom('78db88e8-80f4-4049-b4bb-63646ffbcc3a', foundational, minimum_floor_maximum_discretion).
narrative_ontology:cs_axiom_status(minimum_floor_maximum_discretion, holdable).
narrative_ontology:cs_axiom_grounding('78db88e8-80f4-4049-b4bb-63646ffbcc3a', minimum_floor_maximum_discretion, conventional).
narrative_ontology:cs_axiom('78db88e8-80f4-4049-b4bb-63646ffbcc3a', foundational, individualized_persecution_proof_required).
narrative_ontology:cs_axiom_status(individualized_persecution_proof_required, holdable).
narrative_ontology:cs_axiom_grounding('78db88e8-80f4-4049-b4bb-63646ffbcc3a', individualized_persecution_proof_required, empirically_contingent).
narrative_ontology:cs_axiom('78db88e8-80f4-4049-b4bb-63646ffbcc3a', secondary, psg_limited_immutable_state_aware).
narrative_ontology:cs_axiom_status(psg_limited_immutable_state_aware, holdable).
narrative_ontology:cs_axiom_grounding('78db88e8-80f4-4049-b4bb-63646ffbcc3a', psg_limited_immutable_state_aware, empirically_contingent).
narrative_ontology:cs_reference_frame('78db88e8-80f4-4049-b4bb-63646ffbcc3a', drafter_reciprocal_floor_compact).
narrative_ontology:cs_drift_state('78db88e8-80f4-4049-b4bb-63646ffbcc3a', contemporary_externalization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('78db88e8-80f4-4049-b4bb-63646ffbcc3a', '').
narrative_ontology:cs_kernel_id(refugee_convention_text__restrictive_sovereignty_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__restrictive_sovereignty_reading, destination_state_executives).
narrative_ontology:constraint_beneficiary(refugee_convention_text__restrictive_sovereignty_reading, restrictionist_political_movements).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, excluded_asylum_claimants).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, offshore_detained_claimants).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, frontline_host_states).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, border_interdicted_would_be_claimants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interior ministries and heads of government of wealthy destination states. They set the national reading through statute and administrative guidance: narrow particular-social-group lists, individualized-proof requirements, admissibility screening, safe-third-country designations, and externalization deals. Reduced protection obligations and discretionary control flow to them, along with the political credit for lowered arrivals. They fund the enforcement machinery, absorb occasional court reversals and diplomatic friction, and can reposition — tightening further, joining different legal arrangements, or outsourcing enforcement — at comparatively low cost.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, destination_state_executives, agenda_setter,
    institutional, biographical, arbitrage, national).

% Parties and movements campaigning on reduced arrivals. They convert the narrow reading into platform planks and legislative proposals and collect votes, donations, and media standing from its operation, without carrying any administrative or adjudicative burden. Their attachment is issue-portable: if the salience of arrival numbers fades they can redirect to other grievances at little loss.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, restrictionist_political_movements, beneficiary,
    organized, biographical, mobile, national).

% People fleeing harms the narrowed criteria cannot name: generalized violence in gang- or militia-controlled territory, civil-war indiscriminate bombardment, gender-based persecution without provably individualized targeting, LGBTQ+ persecution enforced by private social enforcement, clan-based targeting. Their claims are rejected, pre-screened as inadmissible, or never fit a cognizable category. The available exits are re-routing through more dangerous corridors or remaining in danger; within the determination system itself there is no way out of the category problem.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, excluded_asylum_claimants, payer,
    powerless, immediate, trapped, regional).

% People transferred under offshore-processing arrangements to third-country islands or camps, where claims undergo admissibility screening before any substantive assessment. They spend years in containment with no access to the appeal jurisdiction of the transferring state, no freedom of movement, and no pathway except resettlement quotas they do not control. Geography itself is the barrier: the arrangement is built so that leaving the processing site is impossible.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, offshore_detained_claimants, payer,
    powerless, immediate, trapped, local).

% States adjacent to major conflict zones — Jordan, Lebanon, Turkey, Kenya, Pakistan, Bangladesh and similar — that host the largest displaced populations. As destination states narrow their gates, displaced populations concentrate on these borders. They trade floor compliance for externalization funding and border-management packages, gradually relaxing their own commitments in exchange for payments, and cannot exit the arrangement because aid, trade, and security cooperation are tied to it.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, frontline_host_states, payer,
    institutional, generational, constrained, regional).

% People intercepted at sea or land borders under pushback and interdiction policies that the narrow reading's screening logic authorizes. They never lodge a claim at all: no interview, no file, no statistic. They are returned to transit countries or places of danger without the individualized assessment whose absence the narrow reading treats as unproblematic. Their exclusion from the conversation is the mechanism working as designed.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, border_interdicted_would_be_claimants, payer,
    powerless, immediate, trapped, regional).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__restrictive_sovereignty_reading, border_interdicted_would_be_claimants, excluded).

% The agency mandated to supervise the Convention. It issues interpretive guidance — the Handbook, Executive Committee conclusions — that favors broader categories and documents floor violations, including pushbacks and offshore conditions. Its pronouncements carry advisory weight only; under this reading, domestic determinations systematically discount them. It collects nothing and pays nothing; its position is supervisory and evidentiary.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, unhcr_supervisory_mission, observer,
    institutional, generational, analytical, global).

% Courts and tribunal systems that apply the reading statute by statute. They hold residual interpretive discretion and periodically widen particular-social-group definitions or find individualization satisfied where executive guidance says otherwise, prompting guidance revisions and legislative tightening in response. They neither collect nor pay under the arrangement; their divergences from executive guidance are the visible seam along which the reading is contested.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, national_asylum_decision_bodies, observer,
    institutional, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(refugee_convention_text__restrictive_sovereignty_reading, destination_state_executives).
narrative_ontology:fixing_cost_class(refugee_convention_text__restrictive_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Convention coordinates a common minimum definition of the protection duty across roughly 149 states parties: a shared trigger ('well-founded fear of being persecuted'), a shared prohibition on return, standardized travel documents, and enough definitional commonality that states can rely on each other's determinations for readmission and transfer decisions. Without a common floor, each border defines its own categories and reciprocal trust in non-return collapses.
% TRANSFER_FUNCTION: Moves interpretive authority over who deserves protection from claimants and humanitarian bodies to destination-state executives; moves protection costs away from discretionary destination states and onto claimant categories the narrowed criteria exclude and onto frontline host regions; moves enforcement labor onto carriers, transit states, and third-country processors through sanctions and paid deals.
% ABSENT_VOICES: People interdicted at borders before lodging never enter the conversation — they are at sea, in transit zones, or already returned. Claimants whose harms fit no cognizable category have no vocabulary to speak in during determination. UNHCR's interpretive voice is formally outside domestic decisions. Frontline-host populations are represented only by their governments, whose positions are purchased with externalization funding. Each of these seats would object that the floor being defended is narrower than the harm landscape it governs.
% DISAPPEARANCE_RATIONALE: If the Convention text and this reading of it vanished overnight, the floor disappears with it: no common trigger, no reciprocal non-return expectation, and a cascade of chain refoulement as each border redefines protection bilaterally. Simultaneously the entire restrictive apparatus — admissibility screening, offshore processing, safe-third-country designations, carrier sanctions — loses its legal anchor and would have to be rebuilt on naked domestic law. Protection arrangements, migration corridors, and frontline-state financing would all reorganize within months.
% FOUNDING_PROBLEM: Built after the Second World War to solve a specific catastrophe: states had repeatedly returned persecuted people to death — the Évian Conference's failure, the St. Louis, wartime refoulement of Jewish and dissident refugees — because flight from persecution carried no predictable legal duty. Drafters needed a common legal trigger strong enough to prevent return-to-death, yet bounded enough that sovereign states would ratify it.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the historical record of the Évian Conference and documented wartime refoulement; UNHCR's global forced-displacement series showing persecution-driven flight at record scale; the refugee-law scholarly literature (Hathaway, Goodwin-Gill and successors) tracing the drafting problem; and testimony of frontline host states whose populations embody the continuing problem. Destination-state executives also attest liveness, but the corroboration does not rest on them.
narrative_ontology:disappearance_verdict(refugee_convention_text__restrictive_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(refugee_convention_text__restrictive_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__restrictive_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(refugee_convention_text__restrictive_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(refugee_convention_text__restrictive_sovereignty_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(refugee_convention_text__restrictive_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(refugee_convention_text__restrictive_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(refugee_convention_text__restrictive_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.70 at interval end because the narrow reading converts a shared protection duty into a discretionary allocation: the same treaty text yields radically different protection depending on which side of the individualization and PSG lines a claimant falls, and the line has moved steadily toward exclusion since 1951. Suppression is high (0.84) because the arrangement's persistence depends on actively closing access channels — visa and carrier sanctions, interdiction, offshore geography, safe-third-country designation — rather than on participant assent; note suppression is a raw structural property and is not scaled by power or scope, unlike extractiveness. Theater is low-moderate (0.30): the machinery is overwhelmingly functional, but a growing share of activity is performative processing of claims effectively pre-judged inadmissible, and status-determination rituals in offshore sites whose outcomes are predetermined. Accessibility collapse is moderate (0.45) because alternatives persist meaningfully: courts widen categories against executive guidance, subsidiary and complementary protection regimes catch some excluded harms, and entire jurisdictions run the expansive reading — the narrow reading constrains but does not extinguish the option space. Resistance is substantial (0.58): strategic litigation (including successful challenges to interdiction), UNHCR public objection, NGO documentation campaigns, and periodic judicial widening. All three tracked metrics share one time grid (points at 0, 15, 30, 45, 60, 74 — years since the 1951 adoption, marking the Protocol era, harmonization onset, post-Cold-War securitization, externalization consolidation, and the pushback/offshore era) so no metric row is ever silently filled from another's end-state. Suppression_requirement is authored deliberately: the story's central dynamic is enforcement-capacity build-up (sanctions regimes, Dublin-type transfer systems, offshore facilities, pushback doctrine), which the scalar base_properties.suppression alone cannot trace. The series are monotonic — no cyclical dynamic is claimed.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats should compute sharply different types from identical structural data. From destination_state_executives the arrangement is a preserved sovereign prerogative wrapped around a modest reciprocal commitment — the floor is honored, discretion is lawful self-government. From excluded_asylum_claimants and offshore_detained_claimants the same structure operates as a gated survival lottery in which their categories of harm are defined out of existence. national_asylum_decision_bodies sit between: bound by statute, occasionally widening categories, experiencing the reading as unstable interpretive terrain. unhcr_supervisory_mission experiences steady erosion of supervisory purchase. The engine computes this per-seat divergence from the authored power, exit, and role data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations map directly onto structural relationships. destination_state_executives and restrictionist_political_movements are declared beneficiaries: the former captures discretion, avoided obligations, and political credit; the latter captures electoral rents — both derive low directionality (subsidy-side). The four declared victim groups — excluded_asylum_claimants, offshore_detained_claimants, frontline_host_states, border_interdicted_would_be_claimants — all derive high directionality, amplified by trapped exit options: the engine treats trapped or access-blocked targets as sitting nearer the full-target end than mobile ones, and interdicted claimants are the limiting case of exit destruction. No directionality overrides are authored: the derivation chain produces the correct d for every seat from the declarations plus exit data, and a power-atom-level override would misfire across the several institutional-power seats that hold opposite relationships to the constraint. Scope runs from local (offshore sites) through national (executives, movements, courts) to regional and global (frontline hosts, UNHCR, the regime itself), so the engine's scope scaling of effective extraction applies unevenly across seats — larger-scope verification failures (pushbacks at sea, offshore conditions) are precisely where oversight is weakest.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem remains live and the disappearance verdict is world_rearranges: status(live) x verdict(world_rearranges) aligns, so the mismatch consumer raises no capture/zombie flag — this constraint is not a dead mandate kept alive by inertia, and the R5 genealogy corroborates the founding problem from sources outside the beneficiary set. Mandatrophy discipline matters here in the other direction: the genuine coordination floor (reciprocal non-return, common trigger, reliance on each other's determinations) prevents mislabeling the whole arrangement as pure predation, while the named receipt seat and the asymmetrically shifted burden prevent romanticizing it as mere coordination. The piton signature fails on its own terms: theater_ratio is 0.30, the administrator demonstrably profits (gain_flow names a seat), and the arrangement meets active, organized resistance — none of which describes inertial theatrical maintenance. The live risk is drift, not decay: if floor-genuineness collapses (see omega floor_genuineness), the coordination component becomes cover and the type slides toward pure extraction; the temporal series is the instrument that watches for that slide.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_index_structural_delta,
    'This constraint instantiates the restrictive_sovereignty_reading of kernel refugee_convention_text. Which structural elements flip under the expansive_humanitarian_reading, and where exactly is the disagreement located?',
    'Cross-reading comparison across the sibling stories'' victim sets, admissibility structures, and offshore permissions, anchored in the Travaux Preparatoires and subsequent treaty practice; the dispute locates at three elements: the scope of ''well-founded fear'' (individualized persecution versus generalized violence and non-state persecution), the breadth of ''particular social group'' (immutable-characteristics-plus-state-awareness versus gender, LGBTQ+, clan), and the discretion ceiling (maximum discretion above the floor versus an unbendable humanitarian mandate).',
    'Victim-set membership, the per-seat chi distribution, and potentially the type boundary (tangled_rope versus snare) shift per reading. Epsilon values are indexed to readings over a fixed referent and must not be averaged across the family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_index_structural_delta, conceptual, 'Kernel-reading indexicality: the victim set and enforcement permissions are properties of the reading, not of the treaty text.').

omega_variable(
    floor_genuineness,
    'Is the Convention floor a sincere reciprocal commitment that states honor under pressure, or rhetorical cover beneath which breach is routine whenever incentives suffice?',
    'Systematic floor-violation data under stress: documented pushback incidence, non-refoulement findings in regional human-rights courts, interdiction return records, and offshore condition reports, correlated against crisis intensity and enforcement incentives. If breach scales with incentive and draws no systemic sanction from fellow states parties, the floor functions as cover.',
    'Routine unpunished breach dissolves the coordination component into cover for the extraction machinery, moving the classification toward the pure-extraction pole; a consistently honored floor under stress supports the entangled-coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(floor_genuineness, empirical, 'Whether the minimum floor is binding commitment or negotiating rhetoric.').

omega_variable(
    psg_criterion_validity,
    'Does the immutable-characteristics-plus-state-awareness particular-social-group criterion track a real difference in protection need, or does it exclude people facing equivalent lethality under a technicality?',
    'Matched-outcome comparison across jurisdictions: recognition rates and post-decision harm or mortality for claimants presenting identical underlying dangers coded under narrow versus broad PSG formulations; cohort tracking of rejected claimants whose persecutors were private actors or whose harm was generalized.',
    'If excluded classes face comparable lethality, the authored epsilon understates the arrangement''s true extraction and the victim set is understated — strengthening the case that the criterion is an exclusion device rather than a relevance filter, and supporting the expansive reading''s structural delta.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(psg_criterion_validity, empirical, 'Empirical validity of the PSG narrowing as a protection-needs filter.').

omega_variable(
    burden_shift_stability,
    'Does shifting protection costs onto frontline host states stabilize the regime as a functional division of labor, or progressively erode the floor as externally funded states relax their own compliance?',
    'Longitudinal correlation of externalization funding flows with frontline-state floor compliance indicators, secondary-movement volumes, and documented violation rates in funded states over successive agreement cycles.',
    'A destabilizing shift decays the coordination function over the interval and predicts degradation of the arrangement''s cooperative core; a stabilizing shift would strengthen the coordination component and justify a lower effective-extraction profile for the burden-sharing element.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(burden_shift_stability, empirical, 'Whether externalized burden-sharing sustains or corrodes the common floor.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__restrictive_sovereignty_reading, 0, 74).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refu_tr_t0, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(refu_tr_t15, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 15, 0.14).
narrative_ontology:measurement(refu_tr_t30, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(refu_tr_t45, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 45, 0.22).
narrative_ontology:measurement(refu_tr_t60, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 60, 0.27).
narrative_ontology:measurement(refu_tr_t74, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 74, 0.3).

% Extraction over time
narrative_ontology:measurement(refu_be_t0, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(refu_be_t15, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 15, 0.36).
narrative_ontology:measurement(refu_be_t30, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 30, 0.45).
narrative_ontology:measurement(refu_be_t45, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 45, 0.53).
narrative_ontology:measurement(refu_be_t60, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 60, 0.63).
narrative_ontology:measurement(refu_be_t74, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 74, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(refu_su_t0, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(refu_su_t15, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 15, 0.24).
narrative_ontology:measurement(refu_su_t30, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(refu_su_t45, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 45, 0.57).
narrative_ontology:measurement(refu_su_t60, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 60, 0.74).
narrative_ontology:measurement(refu_su_t74, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 74, 0.84).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__restrictive_sovereignty_reading, resource_allocation).
narrative_ontology:affects_constraint(refugee_convention_text__restrictive_sovereignty_reading, refugee_convention_text__expansive_humanitarian_reading).
narrative_ontology:affects_constraint(refugee_convention_text__restrictive_sovereignty_reading, refugee_convention_text__procedural_integrity_reading).
narrative_ontology:affects_constraint(refugee_convention_text__restrictive_sovereignty_reading, eu_common_european_asylum_system).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language label 'the Refugee Convention' decomposes into at least three structurally distinct constraints — one per reading of the fixed treaty text (kernel_id: refugee_convention_text). This story instantiates restrictive_sovereignty_reading (narrow victim set, high admissibility screening, offshore processing permitted, generalized violence and non-state persecution excluded). The sibling stories instantiate expansive_humanitarian_reading (wide victim set, generalized violence and non-state persecution protected, gender/LGBTQ+/clan PSG) and procedural_integrity_reading (flexible threshold, non-negotiable fair individualized process). Each member carries its own stable epsilon over the same standing arrangement referent; the readings disagree on victim-set membership, discretion ceiling, and process guarantees, and the disagreement is routed through omega variables and cs_structure rather than averaged into any single story's metrics. This reading sits upstream of eu_common_european_asylum_system: restrictive statutory narrowings and safe-third-country designations feed directly into CEAS design.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
