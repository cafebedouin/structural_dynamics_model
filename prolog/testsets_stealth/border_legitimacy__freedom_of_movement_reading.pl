% ============================================================================
% CONSTRAINT STORY: border_legitimacy__freedom_of_movement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_legitimacy__freedom_of_movement_reading, []).

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
 *   constraint_id: border_legitimacy__freedom_of_movement_reading
 *   human_readable: Freedom of Movement as a Human Right — the Standing Border-Restriction Regime as Presumptively Illegitimate
 *   domain: political philosophy / migration studies / international law
 *
 * SUMMARY:
 *   The standing arrangement under contest is the global border-restriction
 *   and enforcement regime: visa walls, carrier sanctions, externalized
 *   route-policing, detention and removal. This file is the
 *   freedom_of_movement_reading of the border_legitimacy kernel: it assesses
 *   that standing arrangement by the lights of the claim that freedom of
 *   movement is a human right and borders are presumptively illegitimate
 *   restrictions on it. The epsilon referent is therefore the restriction
 *   regime itself, as this reading sees it — and it is high: the regime
 *   withholds the place premium from the global poor, binds current citizens
 *   (displaced workers, welfare-dependent residents) inside the boundary that
 *   claims to protect them, and sustains itself through an enforcement
 *   apparatus that has grown in every decade of the interval. The sibling
 *   readings live in separate files (border_legitimacy__sovereignty_reading,
 *   border_legitimacy__humanitarian_obligation_reading) linked by network
 *   edges; nothing here averages over them or hedges epsilon across readings.
 *   KEY AGENTS (by structural relationship): - destination_state_governments:
 *   agenda-setter (institutional/constrained) — sets visa policy, funds
 *   enforcement, negotiates externalization; administers the regime and
 *   collects fiscal and electoral rents from it - citizenship_rent_holders:
 *   primary beneficiary (powerful/constrained) — born inside the boundary,
 *   captures the premium closure maintains without administering anything -
 *   border_enforcement_industry: secondary beneficiary (organized/mobile) —
 *   revenue scales with enforcement intensity, contracts portable across
 *   governments - would_be_migrants: primary target (powerless/trapped) —
 *   bears the full cost of exclusion; exit is the very thing being denied -
 *   citizens_of_poor_origin_states: primary target (powerless/trapped) —
 *   lifetime earnings set by the passport issued at birth -
 *   displaced_domestic_workers: citizen-side target (moderate/constrained) —
 *   the boundary that claims to protect their jobs caps their own mobility -
 *   welfare_dependent_residents: citizen-side target (powerless/trapped) —
 *   bound by portability rules, invoked as the reason for the border -
 *   transit_state_governments: dual-positioned (moderate/constrained) — paid
 *   to enforce, absorbing the detention burden - human_rights_bodies:
 *   analytical observer (institutional/analytical) — sees the full structure,
 *   holds no enforcement power
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__freedom_of_movement_reading, 0.86).
domain_priors:suppression_score(border_legitimacy__freedom_of_movement_reading, 0.88).
domain_priors:theater_ratio(border_legitimacy__freedom_of_movement_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, extractiveness, 0.86).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__freedom_of_movement_reading, snare).
narrative_ontology:human_readable(border_legitimacy__freedom_of_movement_reading, "Freedom of Movement as a Human Right — the Standing Border-Restriction Regime as Presumptively Illegitimate").
narrative_ontology:topic_domain(border_legitimacy__freedom_of_movement_reading, "political philosophy / migration studies / international law").

domain_priors:requires_active_enforcement(border_legitimacy__freedom_of_movement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__freedom_of_movement_reading, '73fa29b5-81a4-480f-b68c-af075024abac').
narrative_ontology:cs_kernel_codification('73fa29b5-81a4-480f-b68c-af075024abac', distributed).
narrative_ontology:cs_authority_grounding('73fa29b5-81a4-480f-b68c-af075024abac', lineage).
narrative_ontology:cs_interpretation_layer_present('73fa29b5-81a4-480f-b68c-af075024abac').
narrative_ontology:cs_reading_relation('73fa29b5-81a4-480f-b68c-af075024abac', border_legitimacy__sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('73fa29b5-81a4-480f-b68c-af075024abac', border_legitimacy__humanitarian_obligation_reading, influences).
narrative_ontology:cs_axiom('73fa29b5-81a4-480f-b68c-af075024abac', foundational, freedom_of_movement_is_human_right).
narrative_ontology:cs_axiom_status(freedom_of_movement_is_human_right, holdable).
narrative_ontology:cs_axiom_grounding('73fa29b5-81a4-480f-b68c-af075024abac', freedom_of_movement_is_human_right, deontological).
narrative_ontology:cs_axiom('73fa29b5-81a4-480f-b68c-af075024abac', foundational, birthplace_citizenship_morally_arbitrary).
narrative_ontology:cs_axiom_status(birthplace_citizenship_morally_arbitrary, holdable).
narrative_ontology:cs_axiom_grounding('73fa29b5-81a4-480f-b68c-af075024abac', birthplace_citizenship_morally_arbitrary, deontological).
narrative_ontology:cs_reference_frame('73fa29b5-81a4-480f-b68c-af075024abac', presumptive_movement_freedom).
narrative_ontology:cs_drift_state('73fa29b5-81a4-480f-b68c-af075024abac', contemporary_fortified_border_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('73fa29b5-81a4-480f-b68c-af075024abac', '').
narrative_ontology:cs_kernel_id(border_legitimacy__freedom_of_movement_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_reading, citizenship_rent_holders).
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_reading, border_enforcement_industry).
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_reading, destination_state_governments).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, would_be_migrants).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, citizens_of_poor_origin_states).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, displaced_domestic_workers).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, welfare_dependent_residents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_reading, transit_state_governments).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, transit_state_governments).
narrative_ontology:constraint_vindicates(border_legitimacy__freedom_of_movement_reading, territorial_exclusion_prerogative_doctrine).
narrative_ontology:constraint_vindicates(border_legitimacy__freedom_of_movement_reading, bounded_membership_welfare_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set visa policy, fund and direct border enforcement, and negotiate externalization agreements with transit states. Collect fiscal and electoral returns from restriction politics and shield domestic labor and welfare arrangements from outside claims. Their own freedom of action is bounded by treaty obligations, labor-market dependence on migrant labor, and the interstate system they administer — they cannot step out of the border regime without dissolving the membership boundary their budgets and elections are built on.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, destination_state_governments, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(border_legitimacy__freedom_of_movement_reading, destination_state_governments, beneficiary).

% People born into wealthy destination states who capture the wage and living-standard premium that closure maintains. They do not administer enforcement; they receive its protection as a birthright. Their stake is tied to their own jurisdiction — taking the premium abroad means surrendering it — so their position depends on the boundary staying where it is.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, citizenship_rent_holders, beneficiary,
    powerful, generational, constrained, national).

% Contractors and vendors that build surveillance infrastructure, run detention and removal logistics, and staff processing operations. Revenue scales with enforcement intensity, and contracts are portable across governments, so the industry follows enforcement budgets wherever they grow.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, border_enforcement_industry, beneficiary,
    organized, biographical, mobile, global).

% People who want to live and work in another state and are refused legal entry. Their alternatives are waiting in visa queues that function as denials, or irregular routes policed by the same enforcement that closed the legal path. Detention, pushback, and death in transit are the operating risks of moving without permission. They hold no vote in any polity that designs the rules that bind them.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, would_be_migrants, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(border_legitimacy__freedom_of_movement_reading, would_be_migrants, excluded).

% Populations of states whose passports carry little access. The gap between their home labor market and destination labor markets is maintained by the border regime; their lifetime earnings are set substantially by a document they were issued at birth. Legal exit exists; legal arrival almost nowhere.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, citizens_of_poor_origin_states, payer,
    powerless, generational, trapped, global).

% Current citizens of destination states whose industries contracted or relocated. They are told the border protects their jobs, but the same regime caps their own options: working abroad requires visas they rarely qualify for, and domestic relocation means abandoning local support networks. Their mobility is restricted by the same boundary said to protect them.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, displaced_domestic_workers, payer,
    moderate, biographical, constrained, national).

% Current citizens whose income depends on benefits tied to residence. Restriction is justified partly in their name — the claim that newcomers would drain the programs they rely on — while the same logic binds them: benefit portability rules and residence requirements punish them for moving, and they carry the political identity of the people the border is said to protect.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, welfare_dependent_residents, payer,
    powerless, biographical, trapped, national).

% Governments along migration routes that accept enforcement funding, equipment, and visa concessions in exchange for policing movement northward. The money and diplomatic capital flow in; the stranded populations, detention burden, and erosion of their own discretion over territory stay. They enforce a boundary drawn for others and absorb its costs locally.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, transit_state_governments, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(border_legitimacy__freedom_of_movement_reading, transit_state_governments, beneficiary).

% Regional courts, UN treaty bodies, and special rapporteurs that adjudicate movement-rights claims, document pushbacks and detention conditions, and publish findings that states are free to disregard. Their seat sees the full structure and holds no enforcement power over it.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, human_rights_bodies, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_legitimacy__freedom_of_movement_reading, citizenship_rent_holders).
narrative_ontology:fixing_cost_class(border_legitimacy__freedom_of_movement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The arrangement matches presence to membership: states use it to know who is on their territory, to sequence and verify entry, and to keep the population that pays into fiscal and welfare arrangements roughly coextensive with the population that votes. Entry administration and membership bookkeeping are real coordination problems; this regime is one solution to them.
% TRANSFER_FUNCTION: Withholds access to territory, labor markets, and welfare membership from people born outside the boundary and delivers the protected premium to those born inside; moves enforcement budgets from destination-state taxpayers to the enforcement industry; and moves enforcement labor and detention burden onto transit states through funding-and-conditions agreements.
% ABSENT_VOICES: Would-be migrants and citizens of poor origin states — the people the regime binds most tightly — hold no vote in the destination legislatures that design it and no seat in the negotiations that externalize it; transit-state populations bear the detention burden without setting it. The near-unanimity of destination-state politics behind restriction is partly an artifact of who was never in the room.
% DISAPPEARANCE_RATIONALE: If border enforcement vanished overnight, labor would move toward the productivity gap until wage differentials compressed; welfare states would restructure financing around a larger, mobile membership; remittance and diaspora flows would surge; enforcement contracts would evaporate; and destination-state politics would reorganize around adjustment and integration rather than exclusion. Almost no major economic or demographic arrangement would stay where it is.
% FOUNDING_PROBLEM: The modern passport-and-control regime was consolidated during the First World War to solve wartime problems: identifying and tracking populations, preventing enemy infiltration, and controlling labor allocation. After 1945 it was repurposed to bound the membership of the new welfare states — to know in advance who would pay in and who could draw out.
% FOUNDING_PROBLEM_CORROBORATION: Migration historians (the wartime genealogy of the passport system in the Torpey line of scholarship) attest from outside the benefiting parties that the founding problem was wartime population control and is gone; development economists (the place-premium and closed-borders-as-rent literature following Clemens' estimates) attest that the arrangement now functions as premium maintenance rather than as a solution to any live founding problem. Destination governments dispute only the characterization of what persists, not the historical record of the founding.
narrative_ontology:disappearance_verdict(border_legitimacy__freedom_of_movement_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__freedom_of_movement_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__freedom_of_movement_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(border_legitimacy__freedom_of_movement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(border_legitimacy__freedom_of_movement_reading, 0.86, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_legitimacy__freedom_of_movement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_legitimacy__freedom_of_movement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_legitimacy__freedom_of_movement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.86 at interval end) because the arrangement's principal transfer — the wage-and-living-standard premium that closure maintains for those born inside — is decoupled from any service rendered to those it excludes, and because the victim set extends inside the boundary: displaced workers whose options it caps and benefit-dependent residents whom portability rules bind, per this reading's structural delta. Suppression is authored higher still (0.88) as a raw structural property, unscaled by power or scope — only extractiveness is scaled downstream: the arrangement persists through carrier sanctions, detention, pushback, and externalized route-policing, and the interval's history is an enforcement ratchet (Schengen hardening and carrier sanctions, the post-2001 security build-up, Frontex's growth, the EU-Turkey and Italy-Libya arrangements, pandemic-era total closure), which is why suppression_requirement is traced temporally. Theater is moderate and rising (0.45): walls and removal spectacles that survive contact with the rerouting evidence are performance, but the coercive machinery behind them is real. Accessibility collapse is moderate (0.55): irregular movement persists at scale — criminalized and lethal — so alternatives are narrowed rather than erased. Resistance is high (0.65): migration itself is the standing act of resistance, joined by sanctuary jurisdictions, pushback litigation, and open-borders advocacy. All three series run on one shared eight-point grid (1990-2025) so no metric's row is ever backfilled. Coalition note: the victim class is the largest potential coalition in the story — billions of people — but legal exclusion from destination polities, cross-border coordination costs, and enforcement against organizers keep it disorganized; diaspora networks and remittance leverage are the live coalition vector, which is why resistance is high despite the trapped exit atoms.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute very differently. From the agenda-setter seat (destination_state_governments) the arrangement is a membership-matching system it built and must run; from the trapped payer seats (would_be_migrants, citizens_of_poor_origin_states) the same structure is a closed door with the price attached to the wall. From the citizen-side payer seats (displaced_domestic_workers, welfare_dependent_residents) it is a boundary that claims to protect them while capping their own mobility — the seat where this reading's structural delta lives. The sibling files make the same divergence a between-constraint fact: a sovereignty_reading story would compute the arrangement as coordination from nearly every inside seat, with the outsider seats carrying all the divergence. This file does not reconcile the seats; the engine computes per-seat classifications from the structural data, and the divergence between this file's computed types and its sibling files' is the measurement the family exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: citizenship_rent_holders (receive the premium without administering anything — d near the beneficiary end, with constrained exit because the premium is jurisdiction-bound), border_enforcement_industry (revenue scales with enforcement intensity, mobile across contracts), and destination_state_governments, who both administer and collect — hence the secondary beneficiary role. Targets: would_be_migrants and citizens_of_poor_origin_states (trapped, d near the full-target end — the regime's entire cost lands on them and their exit is the thing being denied), plus the citizen-side victims displaced_domestic_workers and welfare_dependent_residents, whose d is elevated by constrained rather than trapped exit. Transit_state_governments sit mid-structure: paid to enforce, bearing the detention burden — a genuinely dual position recorded as payer with secondary beneficiary. Human_rights_bodies hold the analytical seat and collect nothing. The base_properties beneficiary and victim declarations are the structural source; the derivation chain and any scaling belong to the engine. No directionality overrides are authored: the role, power, and exit declarations above are sufficient for the derivation, and no seat's derived position is visibly wrong.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification work this reading performs is to keep the arrangement from being mislabeled as pure coordination: a rope verdict would read the membership-matching function as the whole story and book the premium transfer as coordination cost. The snare claim from this seat says the reverse: the coordination function is real but thin, and the arrangement's thirty-five-year growth track — all three series rising together — shows extraction accumulating on top of it rather than coordination maturing. The R5 genealogy does the same work from the other side: the founding problem (wartime population control, repurposed to bound welfare membership) is attested dead by historians and by the rent-structure literature from outside the benefiting parties, while the world would rearrange drastically if the arrangement vanished — the dead-problem-plus-persistent-structure combination is the capture signature, and this file authors it honestly rather than suppressing the mismatch. The guard cuts both ways: if the welfare_state_compatibility omega resolves toward genuine incompatibility, part of the measured epsilon is coordination cost and the honest computed type moves toward a hybrid profile; if the enforcement_effectiveness omega resolves toward real gatekeeping, the security justification gains weight. This file authors the reading's verdict and the descriptive metrics without pre-empting those resolutions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is the freedom_of_movement_reading of the border_legitimacy kernel. What would change structurally if a sibling reading — sovereignty_reading or humanitarian_obligation_reading — were instantiated over the same standing arrangement?',
    'Not resolvable by data within this story: by the epsilon-invariance principle the readings are separate constraints, and the corpus resolves the question by generating the sibling files and comparing their authored epsilon, victim sets, and computed types over the shared referent.',
    'Under the sovereignty reading the same arrangement computes as low-epsilon coordination and the victim set empties; under the humanitarian reading a refugee/economic-migrant line partitions the victim set. The disagreement is located in whether movement is a presumptive human right, a state prerogative, or a conditional duty — not in any measurable property of the arrangement itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed classification of the shared border-legitimacy kernel; sibling readings are separate constraint files.').

omega_variable(
    welfare_state_compatibility,
    'Is universal freedom of movement compatible with bounded-financing welfare states, or does welfare-state financing require membership boundaries?',
    'Natural experiments: intra-EU free movement with portable benefits, studies of the 2004 EU accessions, and fiscal-incidence analyses of migrant contributions versus draws.',
    'If compatible, the welfare justification for the standing restriction fails and epsilon rises toward pure rent; if incompatible, part of the restriction is genuine coordination cost and the computed type shifts toward a hybrid coordination/extraction profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_state_compatibility, empirical, 'Whether the welfare-bounding justification is live coordination need or cover.').

omega_variable(
    enforcement_effectiveness_ambiguity,
    'Does enforcement actually reduce movement, or does it mainly reroute it — converting circular migration into settled irregularity and raising transit mortality while enforcement budgets grow?',
    'Enforcement-surge natural experiments (the US border-buildup literature), route-shift mortality data, and circularity/recidivism studies.',
    'If enforcement mostly reroutes, the theater share is understated and the arrangement''s functional content is smaller than its budget; if it genuinely gates movement, the security justification gains weight and the extraction reading weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_effectiveness_ambiguity, empirical, 'Whether the enforcement machinery performs gatekeeping or displacement.').

omega_variable(
    citizen_victim_magnitude,
    'How much do current citizens of destination states actually bear as targets of the same boundary — displaced workers capped in where they can work, welfare recipients bound by portability rules — relative to what they capture as premium holders?',
    'Emigration-elasticity and intra-EU mobility studies, benefit-portability loss estimates, and wage effects of emigration options on stayers.',
    'If citizen-side harm is substantial, the victim set genuinely includes current citizens (this reading''s structural delta) and the arrangement is not even a stable insider pact; if negligible, victims are non-citizens only and the insider/outsider line holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(citizen_victim_magnitude, empirical, 'Whether the citizen victim set is real or rhetorical.').

omega_variable(
    externalization_scope_boundary,
    'Is externalized enforcement — route-policing funded and directed by destination states but executed by transit states and non-state actors — part of this constraint''s standing arrangement, or a separate constraint with its own epsilon?',
    'Decomposition test: if externalization''s extraction profile, victim set, and enforcement structure can be stated independently of the destination-state border regime, author it as a separate story and link via network edges.',
    'If separate, this story''s epsilon covers the destination-state regime only and the measured suppression drops; if inseparable (externalization is the regime''s enforcement arm operating abroad), the current epsilon stands and the transit-state seats belong fully in the victim ledger.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(externalization_scope_boundary, conceptual, 'Whether externalized enforcement lies inside or outside the constraint''s boundary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__freedom_of_movement_reading, 1990, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1990, border_legitimacy__freedom_of_movement_reading, theater_ratio, 1990, 0.24).
narrative_ontology:measurement_basis(bord_tr_t1990, observed).
narrative_ontology:measurement(bord_tr_t1995, border_legitimacy__freedom_of_movement_reading, theater_ratio, 1995, 0.26).
narrative_ontology:measurement_basis(bord_tr_t1995, observed).
narrative_ontology:measurement(bord_tr_t2000, border_legitimacy__freedom_of_movement_reading, theater_ratio, 2000, 0.29).
narrative_ontology:measurement_basis(bord_tr_t2000, observed).
narrative_ontology:measurement(bord_tr_t2005, border_legitimacy__freedom_of_movement_reading, theater_ratio, 2005, 0.32).
narrative_ontology:measurement_basis(bord_tr_t2005, observed).
narrative_ontology:measurement(bord_tr_t2010, border_legitimacy__freedom_of_movement_reading, theater_ratio, 2010, 0.35).
narrative_ontology:measurement_basis(bord_tr_t2010, observed).
narrative_ontology:measurement(bord_tr_t2015, border_legitimacy__freedom_of_movement_reading, theater_ratio, 2015, 0.38).
narrative_ontology:measurement_basis(bord_tr_t2015, observed).
narrative_ontology:measurement(bord_tr_t2020, border_legitimacy__freedom_of_movement_reading, theater_ratio, 2020, 0.42).
narrative_ontology:measurement_basis(bord_tr_t2020, observed).
narrative_ontology:measurement(bord_tr_t2025, border_legitimacy__freedom_of_movement_reading, theater_ratio, 2025, 0.45).
narrative_ontology:measurement_basis(bord_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(bord_be_t1990, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 1990, 0.68).
narrative_ontology:measurement_basis(bord_be_t1990, observed).
narrative_ontology:measurement(bord_be_t1995, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 1995, 0.71).
narrative_ontology:measurement_basis(bord_be_t1995, observed).
narrative_ontology:measurement(bord_be_t2000, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 2000, 0.73).
narrative_ontology:measurement_basis(bord_be_t2000, observed).
narrative_ontology:measurement(bord_be_t2005, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 2005, 0.76).
narrative_ontology:measurement_basis(bord_be_t2005, observed).
narrative_ontology:measurement(bord_be_t2010, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 2010, 0.78).
narrative_ontology:measurement_basis(bord_be_t2010, observed).
narrative_ontology:measurement(bord_be_t2015, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 2015, 0.81).
narrative_ontology:measurement_basis(bord_be_t2015, observed).
narrative_ontology:measurement(bord_be_t2020, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 2020, 0.84).
narrative_ontology:measurement_basis(bord_be_t2020, observed).
narrative_ontology:measurement(bord_be_t2025, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 2025, 0.86).
narrative_ontology:measurement_basis(bord_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1990, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement_basis(bord_su_t1990, observed).
narrative_ontology:measurement(bord_su_t1995, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 1995, 0.73).
narrative_ontology:measurement_basis(bord_su_t1995, observed).
narrative_ontology:measurement(bord_su_t2000, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement_basis(bord_su_t2000, observed).
narrative_ontology:measurement(bord_su_t2005, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 2005, 0.78).
narrative_ontology:measurement_basis(bord_su_t2005, observed).
narrative_ontology:measurement(bord_su_t2010, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 2010, 0.8).
narrative_ontology:measurement_basis(bord_su_t2010, observed).
narrative_ontology:measurement(bord_su_t2015, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 2015, 0.84).
narrative_ontology:measurement_basis(bord_su_t2015, observed).
narrative_ontology:measurement(bord_su_t2020, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 2020, 0.87).
narrative_ontology:measurement_basis(bord_su_t2020, observed).
narrative_ontology:measurement(bord_su_t2025, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 2025, 0.88).
narrative_ontology:measurement_basis(bord_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_legitimacy__freedom_of_movement_reading, identity_coordination).
narrative_ontology:affects_constraint(border_legitimacy__freedom_of_movement_reading, border_legitimacy__sovereignty_reading).
narrative_ontology:affects_constraint(border_legitimacy__freedom_of_movement_reading, border_legitimacy__humanitarian_obligation_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the border_legitimacy kernel per the epsilon-invariance principle. 'Border legitimacy' is one contested kernel read three ways, and each reading is a structurally distinct constraint over the same standing arrangement (the global border-restriction and enforcement regime). This file instantiates the freedom_of_movement_reading and authors high epsilon over that referent; border_legitimacy__sovereignty_reading authors low epsilon over the same referent (the arrangement as legitimate coordination), and border_legitimacy__humanitarian_obligation_reading authors a partitioned victim set (duty to admit the persecuted, none toward economic migrants). The readings are linked because they share the referent and compete for the same institutional surface: the human-rights framework this reading articulates is the upstream source of the humanitarian reading's refugee duties (influences), while the sovereignty reading coexists as the opposing live position. Each file's epsilon differs because the readings differ — not because one arrangement is being measured two ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
