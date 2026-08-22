% ============================================================================
% CONSTRAINT STORY: fifth_republic_constitution__cohabitation_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fifth_republic_constitution__cohabitation_equilibrium_reading, []).

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
 *   constraint_id: fifth_republic_constitution__cohabitation_equilibrium_reading
 *   human_readable: Cohabitation-Equilibrium Dual Executive (Negotiated Authority Allocation)
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   The 1958 Constitution gives France two separately legitimated executive
 *   authorities: a directly elected president who appoints the prime
 *   minister, dissolves the Assembly, and commands foreign and military
 *   policy, and a government answerable to the National Assembly that
 *   conducts legislation and domestic administration. The
 *   cohabitation_equilibrium_reading holds that the text thereby requires a
 *   negotiated allocation of authority between the two: when electoral
 *   arithmetic aligns, the president leads and the government executes; when
 *   it diverges, the president keeps the reserved foreign-military domain
 *   while the opposing majority governs domestic policy through its prime
 *   minister, and the boundary itself is settled by ongoing negotiation
 *   backed by mutual deterrents (dissolution against censure). Per the
 *   epsilon-invariance principle this file authors ONLY the equilibrium
 *   reading's constraint: the standing arrangement under contest is the
 *   negotiated dual-executive allocation itself, and epsilon is assessed by
 *   this reading's own lights. The sibling readings —
 *   hyper_presidential_reading and parliamentary_constraint_reading —
 *   instantiate different arrangements with different beneficiary/victim
 *   structures and are separate stories linked through
 *   network.affects_constraints. The manifest's expected delta ('victim is
 *   policy coherence') is operationalized at actor level: voters who cannot
 *   assign responsibility and implementers who receive crossed directives are
 *   the named payers; 'beneficiary is whichever actor controls key policy
 *   domains' is operationalized as conditional beneficiary declarations on
 *   both the presidency and the Assembly majority.
 *
 * KEY AGENTS:
 *   - incumbent_president: Primary agenda-setter and conditional beneficiary (institutional/constrained) — holds appointment, dissolution, arbitration, and the reserved foreign-military domain; exposed whenever the Assembly holds an opposing majority
 *   - prime_minister_government: Co-agenda-setter and payer (institutional/constrained) — runs domestic policy on Assembly confidence; bears negotiation costs and shared blame
 *   - national_assembly_majority: Conditional beneficiary (institutional/constrained) — collects domestic policy control during divided arithmetic; pressures the presidency through censure and legislation
 *   - voters_facing_accountability_diffusion: Payer (moderate/constrained) — bears diffused responsibility for outcomes neither branch solely owns
 *   - civil_service_implementers: Payer (moderate/constrained) — receives potentially contradictory directives across the reserved-shared boundary
 *   - constitutional_council: Analytical observer (institutional/analytical) — adjudicates boundary disputes between domains on referral
 *   - constitutional_reform_advocates: Excluded (organized/trapped) — would re-settle the allocation (sixth republic, parliamentary restoration, or full presidentialism) but lack the amendment path
 *   - minority_parliamentary_parties: Excluded (moderate/constrained) — outside the bilateral dyad; pivotal only when arithmetic fragments
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.57).
domain_priors:suppression_score(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.51).
domain_priors:theater_ratio(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, extractiveness, 0.57).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 0.51).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__cohabitation_equilibrium_reading, tangled_rope).
narrative_ontology:human_readable(fifth_republic_constitution__cohabitation_equilibrium_reading, "Cohabitation-Equilibrium Dual Executive (Negotiated Authority Allocation)").
narrative_ontology:topic_domain(fifth_republic_constitution__cohabitation_equilibrium_reading, "constitutional/political").

domain_priors:requires_active_enforcement(fifth_republic_constitution__cohabitation_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__cohabitation_equilibrium_reading, '860e30b1-1c33-4806-b7e4-05b5cee6e0e9').
narrative_ontology:cs_kernel_codification('860e30b1-1c33-4806-b7e4-05b5cee6e0e9', fixed_text).
narrative_ontology:cs_authority_grounding('860e30b1-1c33-4806-b7e4-05b5cee6e0e9', practice).
narrative_ontology:cs_interpretation_layer_present('860e30b1-1c33-4806-b7e4-05b5cee6e0e9').
narrative_ontology:cs_reading_relation('860e30b1-1c33-4806-b7e4-05b5cee6e0e9', fifth_republic_constitution__hyper_presidential_reading, coexists_with).
narrative_ontology:cs_reading_relation('860e30b1-1c33-4806-b7e4-05b5cee6e0e9', fifth_republic_constitution__parliamentary_constraint_reading, coexists_with).
narrative_ontology:cs_axiom('860e30b1-1c33-4806-b7e4-05b5cee6e0e9', foundational, executive_authority_is_negotiable_between_dual_mandates).
narrative_ontology:cs_axiom_status(executive_authority_is_negotiable_between_dual_mandates, holdable).
narrative_ontology:cs_axiom_grounding('860e30b1-1c33-4806-b7e4-05b5cee6e0e9', executive_authority_is_negotiable_between_dual_mandates, conventional).
narrative_ontology:cs_axiom('860e30b1-1c33-4806-b7e4-05b5cee6e0e9', secondary, cohabitation_record_vindicates_mutual_constraint).
narrative_ontology:cs_axiom_status(cohabitation_record_vindicates_mutual_constraint, holdable).
narrative_ontology:cs_axiom_grounding('860e30b1-1c33-4806-b7e4-05b5cee6e0e9', cohabitation_record_vindicates_mutual_constraint, empirically_contingent).
narrative_ontology:cs_reference_frame('860e30b1-1c33-4806-b7e4-05b5cee6e0e9', negotiated_domain_partition_equilibrium).
narrative_ontology:cs_drift_state('860e30b1-1c33-4806-b7e4-05b5cee6e0e9', post_term_alignment_hung_parliament_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('860e30b1-1c33-4806-b7e4-05b5cee6e0e9', '').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__cohabitation_equilibrium_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, incumbent_president).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, national_assembly_majority).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, voters_facing_accountability_diffusion).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, civil_service_implementers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, prime_minister_government).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__cohabitation_equilibrium_reading, duverger_semipresidential_thesis).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__cohabitation_equilibrium_reading, domaine_reserve_convention).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Directly elected head of state serving a five-year term. Appoints the prime minister, may dissolve the National Assembly, arbitrates in emergencies, and commands the armed forces and foreign policy regardless of parliamentary arithmetic. When the Assembly majority belongs to the opposition, the president keeps the foreign and military portfolio but loses domestic initiative and must accept a prime minister from the opposing camp. Leaving the arrangement is not available to the officeholder short of resignation; the practical levers are timing dissolutions and shaping the electoral calendar.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, incumbent_president, agenda_setter,
    institutional, biographical, constrained, national).

% Appointed by the president but answerable to the National Assembly through the censure motion. Directs legislation, budget, and domestic administration. When the president's camp lacks a majority, the prime minister comes from the opposition and governs domestic policy independently while negotiating the boundary with the Elysee. Resignation is always available but ends the officeholder's tenure; staying means sharing blame for outcomes shaped jointly with the president.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, prime_minister_government, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__cohabitation_equilibrium_reading, prime_minister_government, payer).

% Elected coalition holding at least half the seats. Confirms or topples governments through censure, passes legislation and budgets, and during divided arithmetic effectively chooses the prime minister by making anyone else ungovernable. Collects domestic policy control in those periods; loses influence over foreign and military decisions, which remain with the president.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, national_assembly_majority, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__cohabitation_equilibrium_reading, national_assembly_majority, agenda_setter).

% Elect both the president and the Assembly on separate calendars. When the two mandates diverge, responsibility for economic and social outcomes is split between camps that each blame the other, and the ballot offers no clean way to reward or punish a single governing team. Their recourse is the next election cycle; they cannot opt out of the dual structure between elections.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, voters_facing_accountability_diffusion, payer,
    moderate, biographical, constrained, national).

% Senior administrators and ministry staff who execute laws, treaties, and budgets. During divided arithmetic they receive instructions from a government oriented to one camp and signals from a president oriented to the other, particularly where foreign contracts, European negotiations, and defense procurement touch domestic portfolios. Careers depend on serving successive principals of both camps.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, civil_service_implementers, payer,
    moderate, biographical, constrained, national).

% Nine-member body ruling on the constitutionality of statutes and on disputes over the respective powers. Its decisions police the boundary between presidential and governmental domains, but it acts only on referral; it neither initiates nor administers the allocation.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, constitutional_council, observer,
    institutional, generational, analytical, national).

% Scholars, parties, and commissions proposing to replace the current settlement — a Sixth Republic restoring parliamentary primacy, or conversely a fully presidential regime. Changing anything requires either a three-fifths congressional vote or a referendum the sitting president must choose to call, so the advocacy has no procedural path without presidential assent.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, constitutional_reform_advocates, excluded,
    organized, generational, trapped, national).

% Parties outside the two large camps that historically had no seat in the bilateral understanding between the Elysee and the Matignon. When no coalition reaches a majority, as after the 2024 dissolution, their votes become decisive for any government's survival, yet the negotiated division of domains still proceeds primarily between the president and whichever bloc can form a government.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, minority_parliamentary_parties, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fifth_republic_constitution__cohabitation_equilibrium_reading, incumbent_president).
narrative_ontology:fixing_cost_class(fifth_republic_constitution__cohabitation_equilibrium_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Manages dual democratic legitimacy: two branches each hold independent mandates from the electorate, and the arrangement partitions executive competence between them — foreign and military command to the president, legislation and domestic administration to the government resting on Assembly confidence — with the boundary renegotiated whenever electoral arithmetic changes, preventing both one-branch concentration and executive paralysis.
% TRANSFER_FUNCTION: Moves policy-domain control and agenda-setting authority between the presidency and the government/Assembly as electoral arithmetic shifts; during divided periods it moves domestic initiative away from the president and confers on the president a veto-and-command role over foreign and military affairs; it also moves blame for joint outcomes diffusely onto the electorate, which cannot cleanly attribute responsibility to either camp.
% ABSENT_VOICES: Minority parliamentary parties sit outside the bilateral Elysee-Matignon bargain and were irrelevant to its formation; constitutional-reform advocates (sixth-republic parliamentarians, outright presidentialists) have no procedural path without presidential assent; voters affected by responsibility diffusion are represented only through the very elections whose calendars the arrangement shapes.
% DISAPPEARANCE_RATIONALE: If the negotiated-allocation arrangement vanished overnight, the two mandates would immediately collide: either the presidency absorbs executive power wholesale (the hyper-presidential settlement its occupants have repeatedly sought) or the Assembly subordinates the presidency to governmental confidence (the parliamentary settlement its critics propose). Cabinet formation, foreign-policy command, and hung-parliament bargaining of the 2024 kind would all reorganize around whichever settlement prevailed; the current mutual-check configuration would not persist on its own.
% FOUNDING_PROBLEM: The Fourth Republic's assembly supremacy produced chronic governmental instability (roughly twenty-five governments in twelve years) while the Algerian war demanded decisive executive authority; the 1958 founders built a dual executive to stabilize executive power against parliamentary fragmentation without creating an unconstrained plebiscitary presidency.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting seats: constitutional historians' accounts of the Fourth Republic's collapse and the 1958 drafting record; Maurice Duverger's and subsequent comparative-politics literature treating divided-legitimacy management as the regime's defining problem; and the observable recurrence of the problem itself — every period of divided arithmetic (1986, 1993, 1997, and the 2024 hung parliament) reopened the allocation question without any beneficiary needing to assert it. Presidents and parliamentary majorities additionally attest the problem is live when it suits them, which is itself signal that the corroboration does not rest on them alone.
narrative_ontology:disappearance_verdict(fifth_republic_constitution__cohabitation_equilibrium_reading, world_rearranges).
narrative_ontology:founding_problem_status(fifth_republic_constitution__cohabitation_equilibrium_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__cohabitation_equilibrium_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fifth_republic_constitution__cohabitation_equilibrium_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.57, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fifth_republic_constitution__cohabitation_equilibrium_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fifth_republic_constitution__cohabitation_equilibrium_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fifth_republic_constitution__cohabitation_equilibrium_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.57 at interval end) and unstable: it spikes whenever electoral arithmetic divides the two mandates (0.63 in 1986, 0.61 in 1993) because each camp then holds a domain the other needs and prices access accordingly, and it settles lower during unified arithmetic when the allocation is uncontested. Suppression (0.51) is structural rather than personal: the arrangement holds through institutional deterrence — the president's dissolution threat against the majority's censure threat — plus the procedural difficulty of any alternative settlement; it is authored unscaled as a raw structural property, with only extractiveness subject to directionality and scope scaling in the engine. Theater rises steadily after the 2000 five-year-term and calendar-alignment reform made cohabitation unlikely (0.18 at the 1986 peak of real negotiation versus 0.45 in 2024): the negotiated-allocation machinery increasingly exists as doctrine and rhetoric invoked episodically rather than as routine operating procedure, though the 2024 hung parliament shows the machinery still engages when arithmetic forces it. Accessibility collapse is moderate (0.42): alternative settlements remain imaginable and periodically proposed but are blocked by amendment requirements and incumbent interest. Resistance (0.60) reflects six decades of presidential attempts to escape the mutual-check structure, recurring sixth-republic campaigns, and open contestation after the 2024 dissolution. The measurement series run on one shared nine-point grid (1958-2024) with every tracked metric authored at every point. The oscillation is itself part of the mechanism: each alternation of arithmetic reopens the allocation contest and lets the temporarily dominant camp price the other's access, so the cycle functions as intermittent reinforcement rather than noise; the base_properties scalars are measured at the end-state phase (post-revival, elevated).
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the presidency the arrangement looks like a guaranteed core (foreign and military command) wrapped around an exposure that opens whenever the Assembly diverges; from the prime-ministerial and Assembly-majority seats it looks like hard-won co-governance perpetually threatened by dissolution and calendar engineering. From the voter seat both look like a single fog of responsibility. The engine derives these divergences from the declared roles, power atoms, and exit options; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Both executive seats are genuinely dual-positioned: the president is an unconditional beneficiary of the reserved domain and a conditional target of Assembly control; the Assembly majority is a conditional beneficiary of domestic control and a target of dissolution and appointment leverage. Structural derivation from these mixed declarations plus constrained exits lands both seats mid-range rather than at either pole, which is exactly the reading's own thesis ('beneficiary is whichever actor controls key policy domains'). No directionality_overrides are authored: the override mechanism is keyed by power atom, and the president and prime minister share the institutional atom, so an override would conflate two structurally distinct seats; the beneficiary/victim declarations plus exit data already produce the intended mid-range placement. Voters and civil-service implementers derive near the target end: they bear diffuse costs with no compensating domain control and no exit from the constitutional order. The Constitutional Council derives as an analytical observer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — Fourth Republic governmental instability under assembly supremacy, managed in 1958 by strengthening the executive while keeping it doubly legitimated — is largely resolved as originally stated: the regime has been stable for decades. But the arrangement's operative function (managing divided legitimacy between two mandates) recurs whenever arithmetic splits, most recently in 2024, so the founding-problem status is authored contested rather than dead. That choice matters for the mismatch consumer: dead-status plus a world_rearranges verdict would flag a zombie arrangement kept alive by its beneficiaries; contested-status plus world_rearranges instead records a live function with a disputed genealogy. Keeping the coordination declarations (beneficiaries) alongside the asymmetric-cost declarations (payers) prevents mislabeling the mutual-check function as pure extraction, while the rising theater series prevents mistaking post-2000 disuse for functional health.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'This constraint is one reading of the fifth_republic_constitution kernel — the cohabitation_equilibrium_reading. Would instantiating a sibling reading (hyper_presidential_reading or parliamentary_constraint_reading) change the constraint''s beneficiary/victim structure and epsilon?',
    'Author the sibling stories separately and compare computed classifications. The disagreement is located in whether Articles 5, 8, and 20 read together establish a negotiable domain partition or a hierarchical settlement of executive authority.',
    'Under hyper_presidential_reading the president is near-full beneficiary with no mutual constraint and a different victim set; under parliamentary_constraint_reading the Assembly is the beneficiary and the presidency the target. This file''s epsilon (0.57) is valid only for the equilibrium reading''s referent — the standing negotiated-allocation arrangement assessed by this reading''s own lights.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Committer structure: one of three readings of the Fifth Republic kernel; sibling readings instantiate structurally different constraints.').

omega_variable(
    reserved_domain_scope_erosion,
    'Is the president''s reserved foreign-and-military domain a durable constitutional feature or a convention eroding under EU integration, parliamentary ratification powers, and alliance command structures?',
    'Track treaty-ratification practice, EU-law competence creep, and defense-decision participation by the prime minister and government across successive legislatures.',
    'If the reserved domain shrinks, the presidency''s unconditional benefit decays, the equilibrium tilts toward the parliamentary reading, and measured extraction redistributes toward the Assembly-majority seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reserved_domain_scope_erosion, empirical, 'Durability of the presidential reserved domain that anchors the equilibrium''s asymmetric benefit.').

omega_variable(
    accountability_diffusion_weight,
    'How much of the voter-side cost is genuine responsibility diffusion specific to the dual executive, versus ordinary multiparty blame-shifting any system produces?',
    'Comparative blame-attribution studies of cohabitation periods versus unified-government periods; survey data on perceived responsibility during 1986-88, 1993-95, and 1997-2002.',
    'If diffusion is mostly generic, the voter victim declaration overstates extraction and the arrangement sits closer to a working mutual-check mechanism; if specific to the dual structure, the victim weight stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accountability_diffusion_weight, empirical, 'Magnitude and specificity of the accountability cost borne by the electorate.').

omega_variable(
    term_alignment_atrophy_reversibility,
    'Has the 2000 five-year-term and calendar-alignment reform permanently reduced the negotiated-allocation function toward idle machinery, or does divided arithmetic (as in 2024) reliably reactivate it?',
    'Observe whether future hung-parliament or split-election episodes reproduce negotiated domain allocation without formal cohabitation, and whether proposals to de-align the electoral calendar gain traction.',
    'If irreversible, the arrangement drifts toward inertial maintenance with rising theater and eventual piton-side reclassification pressure; if reversible, the oscillation is a structural property and the equilibrium reading stays descriptively accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(term_alignment_atrophy_reversibility, empirical, 'Whether the post-2000 rise in theater is a permanent atrophy or a reversible dormancy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__cohabitation_equilibrium_reading, 1958, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fr_cohab_eq_tr_t1958, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 1958, 0.09).
narrative_ontology:measurement_basis(fr_cohab_eq_tr_t1958, observed).
narrative_ontology:measurement(fr_cohab_eq_tr_t1969, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 1969, 0.11).
narrative_ontology:measurement_basis(fr_cohab_eq_tr_t1969, observed).
narrative_ontology:measurement(fr_cohab_eq_tr_t1978, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 1978, 0.15).
narrative_ontology:measurement_basis(fr_cohab_eq_tr_t1978, observed).
narrative_ontology:measurement(fr_cohab_eq_tr_t1986, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 1986, 0.18).
narrative_ontology:measurement_basis(fr_cohab_eq_tr_t1986, observed).
narrative_ontology:measurement(fr_cohab_eq_tr_t1993, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 1993, 0.17).
narrative_ontology:measurement_basis(fr_cohab_eq_tr_t1993, observed).
narrative_ontology:measurement(fr_cohab_eq_tr_t2000, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 2000, 0.28).
narrative_ontology:measurement_basis(fr_cohab_eq_tr_t2000, observed).
narrative_ontology:measurement(fr_cohab_eq_tr_t2007, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 2007, 0.33).
narrative_ontology:measurement_basis(fr_cohab_eq_tr_t2007, observed).
narrative_ontology:measurement(fr_cohab_eq_tr_t2017, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 2017, 0.38).
narrative_ontology:measurement_basis(fr_cohab_eq_tr_t2017, observed).
narrative_ontology:measurement(fr_cohab_eq_tr_t2024, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 2024, 0.45).
narrative_ontology:measurement_basis(fr_cohab_eq_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(fr_cohab_eq_be_t1958, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 1958, 0.36).
narrative_ontology:measurement_basis(fr_cohab_eq_be_t1958, observed).
narrative_ontology:measurement(fr_cohab_eq_be_t1969, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 1969, 0.39).
narrative_ontology:measurement_basis(fr_cohab_eq_be_t1969, observed).
narrative_ontology:measurement(fr_cohab_eq_be_t1978, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 1978, 0.44).
narrative_ontology:measurement_basis(fr_cohab_eq_be_t1978, observed).
narrative_ontology:measurement(fr_cohab_eq_be_t1986, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 1986, 0.63).
narrative_ontology:measurement_basis(fr_cohab_eq_be_t1986, observed).
narrative_ontology:measurement(fr_cohab_eq_be_t1993, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 1993, 0.61).
narrative_ontology:measurement_basis(fr_cohab_eq_be_t1993, observed).
narrative_ontology:measurement(fr_cohab_eq_be_t2000, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 2000, 0.52).
narrative_ontology:measurement_basis(fr_cohab_eq_be_t2000, observed).
narrative_ontology:measurement(fr_cohab_eq_be_t2007, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 2007, 0.5).
narrative_ontology:measurement_basis(fr_cohab_eq_be_t2007, observed).
narrative_ontology:measurement(fr_cohab_eq_be_t2017, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 2017, 0.53).
narrative_ontology:measurement_basis(fr_cohab_eq_be_t2017, observed).
narrative_ontology:measurement(fr_cohab_eq_be_t2024, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 2024, 0.57).
narrative_ontology:measurement_basis(fr_cohab_eq_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(fr_cohab_eq_su_t1958, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 1958, 0.34).
narrative_ontology:measurement_basis(fr_cohab_eq_su_t1958, observed).
narrative_ontology:measurement(fr_cohab_eq_su_t1969, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 1969, 0.37).
narrative_ontology:measurement_basis(fr_cohab_eq_su_t1969, observed).
narrative_ontology:measurement(fr_cohab_eq_su_t1978, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 1978, 0.43).
narrative_ontology:measurement_basis(fr_cohab_eq_su_t1978, observed).
narrative_ontology:measurement(fr_cohab_eq_su_t1986, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 1986, 0.56).
narrative_ontology:measurement_basis(fr_cohab_eq_su_t1986, observed).
narrative_ontology:measurement(fr_cohab_eq_su_t1993, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 1993, 0.54).
narrative_ontology:measurement_basis(fr_cohab_eq_su_t1993, observed).
narrative_ontology:measurement(fr_cohab_eq_su_t2000, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 2000, 0.46).
narrative_ontology:measurement_basis(fr_cohab_eq_su_t2000, observed).
narrative_ontology:measurement(fr_cohab_eq_su_t2007, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 2007, 0.44).
narrative_ontology:measurement_basis(fr_cohab_eq_su_t2007, observed).
narrative_ontology:measurement(fr_cohab_eq_su_t2017, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 2017, 0.47).
narrative_ontology:measurement_basis(fr_cohab_eq_su_t2017, observed).
narrative_ontology:measurement(fr_cohab_eq_su_t2024, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 2024, 0.51).
narrative_ontology:measurement_basis(fr_cohab_eq_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__cohabitation_equilibrium_reading, resource_allocation).
narrative_ontology:affects_constraint(fifth_republic_constitution__cohabitation_equilibrium_reading, fifth_republic_constitution__hyper_presidential_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__cohabitation_equilibrium_reading, fifth_republic_constitution__parliamentary_constraint_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'Fifth Republic semi-presidentialism' per the epsilon-invariance principle: the label conflates three structurally distinct claims about the same 1958 text. This story authors the cohabitation_equilibrium_reading (negotiated allocation; moderate, unstable extraction; epsilon approximately 0.57). hyper_presidential_reading authors the presidential-sovereignty arrangement (near-full presidential benefit; legislature and voters as targets). parliamentary_constraint_reading authors the legislative-authorization arrangement (Assembly benefit; presidency as target). Each has its own epsilon, beneficiaries, and victims; citation pressure runs from the equilibrium reading toward both siblings because equilibrium practice supplies the factual baseline each rival reading argues against.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
