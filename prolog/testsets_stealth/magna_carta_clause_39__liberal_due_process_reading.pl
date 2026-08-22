% ============================================================================
% CONSTRAINT STORY: magna_carta_clause_39__liberal_due_process_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_clause_39__liberal_due_process_reading, []).

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
 *   constraint_id: magna_carta_clause_39__liberal_due_process_reading
 *   human_readable: Clause 39 as Universal Due-Process Right (Liberal Reading)
 *   domain: constitutional/legal-historical/political
 *
 * SUMMARY:
 *   Clause 39 of the 1215 charter — no free man to be seized, imprisoned,
 *   dispossessed, outlawed, exiled, or destroyed except by lawful judgment of
 *   his equals or the law of the land — is instantiated here as the liberal
 *   due-process reading: a universal individual entitlement binding the
 *   state, the ancestor constraint of habeas enforcement, due-process
 *   clauses, and fair-trial guarantees. The epsilon referent is the standing
 *   arrangement under contest — the due-process constraint as it operates in
 *   the constitutional order — assessed by this reading's own lights: it
 *   holds the constraint substantially extractive against unchecked executive
 *   authority, and authors epsilon accordingly. The kernel contest is routed
 *   to omega variables; this file does not hedge across readings or average
 *   their epsilon values. The measurement grid maps t=0 to 1215 and t=800 to
 *   2015. KEY AGENTS (by structural relationship): - state_executive: primary
 *   target (institutional/trapped) — bears the extraction of discretionary
 *   power - free_subjects: primary beneficiary (moderate/constrained) — hold
 *   the universal entitlement - accused_persons: protected seat where the
 *   constraint bites (powerless/trapped) — benefit and pay simultaneously -
 *   judiciary: agenda-setter and collector (institutional/identity_locked) —
 *   enforces the constraint and converts enforcement into constitutional
 *   jurisdiction - legal_profession: beneficiary (organized/constrained) —
 *   paid from the procedures the constraint requires - parliament:
 *   beneficiary and co-administrator (institutional/constrained) — writes the
 *   law of the land the constraint routes through - indigent_litigants:
 *   excluded voice (powerless/trapped) — bear the constraint's heaviest
 *   costs, absent from the conversation that defines it
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_clause_39__liberal_due_process_reading, 0.72).
domain_priors:suppression_score(magna_carta_clause_39__liberal_due_process_reading, 0.58).
domain_priors:theater_ratio(magna_carta_clause_39__liberal_due_process_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__liberal_due_process_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_clause_39__liberal_due_process_reading, "Clause 39 as Universal Due-Process Right (Liberal Reading)").
narrative_ontology:topic_domain(magna_carta_clause_39__liberal_due_process_reading, "constitutional/legal-historical/political").

domain_priors:requires_active_enforcement(magna_carta_clause_39__liberal_due_process_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__liberal_due_process_reading, '12ce1c62-8167-4350-8966-def211f4612e').
narrative_ontology:cs_kernel_codification('12ce1c62-8167-4350-8966-def211f4612e', fixed_text).
narrative_ontology:cs_authority_grounding('12ce1c62-8167-4350-8966-def211f4612e', lineage).
narrative_ontology:cs_interpretation_layer_present('12ce1c62-8167-4350-8966-def211f4612e').
narrative_ontology:cs_reading_relation('12ce1c62-8167-4350-8966-def211f4612e', magna_carta_clause_39__feudal_prerogative_reading, forecloses).
narrative_ontology:cs_reading_relation('12ce1c62-8167-4350-8966-def211f4612e', magna_carta_clause_39__originalist_limitation_reading, forecloses).
narrative_ontology:cs_axiom('12ce1c62-8167-4350-8966-def211f4612e', foundational, rights_attach_to_persons_not_statuses).
narrative_ontology:cs_axiom_status(rights_attach_to_persons_not_statuses, holdable).
narrative_ontology:cs_axiom_grounding('12ce1c62-8167-4350-8966-def211f4612e', rights_attach_to_persons_not_statuses, deontological).
narrative_ontology:cs_axiom('12ce1c62-8167-4350-8966-def211f4612e', foundational, arbitrary_power_is_per_se_illegitimate).
narrative_ontology:cs_axiom_status(arbitrary_power_is_per_se_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('12ce1c62-8167-4350-8966-def211f4612e', arbitrary_power_is_per_se_illegitimate, deontological).
narrative_ontology:cs_axiom('12ce1c62-8167-4350-8966-def211f4612e', secondary, law_of_land_requires_general_standing_law).
narrative_ontology:cs_axiom_status(law_of_land_requires_general_standing_law, holdable).
narrative_ontology:cs_axiom_grounding('12ce1c62-8167-4350-8966-def211f4612e', law_of_land_requires_general_standing_law, conventional).
narrative_ontology:cs_reference_frame('12ce1c62-8167-4350-8966-def211f4612e', universal_individual_rights_charter).
narrative_ontology:cs_drift_state('12ce1c62-8167-4350-8966-def211f4612e', contemporary_historiographic_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('12ce1c62-8167-4350-8966-def211f4612e', '').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__liberal_due_process_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__liberal_due_process_reading, free_subjects).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__liberal_due_process_reading, accused_persons).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__liberal_due_process_reading, judiciary).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__liberal_due_process_reading, legal_profession).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__liberal_due_process_reading, parliament).
narrative_ontology:constraint_victim(magna_carta_clause_39__liberal_due_process_reading, state_executive).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(magna_carta_clause_39__liberal_due_process_reading, accused_persons).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__liberal_due_process_reading, rule_of_law_doctrine).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__liberal_due_process_reading, judicial_review_authority).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__liberal_due_process_reading, constitutional_supremacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The crown and its modern successor executives hold the detention, seizure, and prosecution powers the clause binds. Every deprivation of liberty or property must route through lawful judgment or general law; the executive cannot opt out, waive the requirement, or relocate its coercive acts outside the constraint's reach. What it loses is discretion: the capacity to act first and justify later. Its historical attempts to recover that discretion — prerogative courts, emergency regimes — have each been met by the enforcement machinery the constraint built.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, state_executive, payer,
    institutional, generational, trapped, national).

% Hold a universal personal entitlement: no seizure, imprisonment, dispossession, outlawry, or exile without lawful judgment or the law of the land. They supply juries, elect the legislatures that write the law of the land, and fund the courts. Emigration is possible but costly, and in practice the entitlement has followed the common law to other jurisdictions rather than being left behind.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, free_subjects, beneficiary,
    moderate, generational, constrained, national).

% Experience the constraint from inside a prosecution: they invoke lawful-judgment protection precisely when state power is at its maximum against them individually. The same process that shields them costs them — delay, legal fees, the burden of procedure — and they bear both sides of that trade with no ability to decline it.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, accused_persons, beneficiary,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_clause_39__liberal_due_process_reading, accused_persons, payer).

% Administers the constraint: issues writs, controls habeas, defines what lawful judgment and the law of the land require, and reviews executive action against that standard. The constraint is the source of the courts' constitutional jurisdiction — judicial authority over the executive exists because this constraint routes deprivations through judges. The bench's self-understanding as guardian of ancient liberties is constituted by this role; abandoning it would dissolve the institution's own claim to authority.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, judiciary, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_clause_39__liberal_due_process_reading, judiciary, beneficiary).

% Supplies the advocacy, drafting, and argument the constraint's procedures require, and is paid from the same procedures. Procedural complexity is the profession's market; simplification would shrink it. Individual lawyers have portable skills, but the profession's collective income and status are jurisdiction-bound to the procedural system this constraint sustains.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, legal_profession, beneficiary,
    organized, biographical, constrained, national).

% Writes the law of the land that the clause channels all deprivation through. The constraint converts every sovereign act against liberty or property into a parliamentary act, which concentrates legislative supremacy in parliament. It co-administers the constraint's content while collecting the supremacy the routing confers; it could in principle amend the constraint but has never had an interest in doing so.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, parliament, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_clause_39__liberal_due_process_reading, parliament, agenda_setter).

% Would object that the constraint's protection is rationed by price: those who cannot fund advocacy receive the thinnest version of the lawful-judgment guarantee while bearing its heaviest costs in remand time and delayed hearings. They are present in the system as defendants but absent from the professional and scholarly conversation that defines what the constraint requires; legal aid exists as a partial, contested patch, not a seat.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, indigent_litigants, excluded,
    powerless, immediate, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_clause_39__liberal_due_process_reading, judiciary).
narrative_ontology:fixing_cost_class(magna_carta_clause_39__liberal_due_process_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of sovereign power: by requiring lawful judgment or general law before any deprivation of liberty, property, or legal standing, it gives every subject a stable, prospectively-known protection against arbitrary punishment and gives officials a shared standard that makes state coercion legible and contestable. Routing deprivations through courts also produces precedent, which lowers the cost of predicting state behavior for everyone.
% TRANSFER_FUNCTION: Moves decision authority over life, liberty, and property from unilateral executive will to courts applying general law; moves fees, professional income, and institutional jurisdiction to the legal system; moves procedural time and cost to litigants; and moves the content of permissible deprivation to the legislature, which writes the law of the land the constraint channels through.
% ABSENT_VOICES: Those the 1215 table never seated: unfree tenants excluded by 'free man,' women in most legal capacities, the propertyless, and colonial subjects governed by the crown while denied the rights of Englishmen — the liberal reading universalizes a bargain struck without them. Within the modern order, indigent litigants bear the constraint's heaviest costs and receive its thinnest protection yet hold no seat in the professional conversation that defines what process is due; emergency-powers advocates press their case openly rather than from exclusion.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight, detention discretion would return to the executive within the first emergency, the judiciary would lose the jurisdiction that constitutes its constitutional role, and every downstream protection — habeas enforcement, due-process clauses, fair-trial guarantees — would lose its operative ancestor and have to be rebuilt from nothing. The arrangement is load-bearing: removal does not return the world to a neutral baseline but to an unprotected one.
% FOUNDING_PROBLEM: King John's arbitrary disseisin, imprisonment, and exaction — a sovereign seizing lands, jailing rivals, and extorting money outside any lawful process, against which the barons demanded judgment-based limits in 1215.
% FOUNDING_PROBLEM_CORROBORATION: The problem's recurrence is attested by the adverse party: every documented executive attempt to bypass process — prerogative courts, Star Chamber, wartime and emergency detention regimes — re-demonstrates that arbitrary state power is a standing possibility the constraint answers. Historical corroboration outside the beneficiary set: pre-1215 royal records and chronicles document the disseisin and imprisonment the clause addressed. No party with an interest in the constraint's persistence had to assert the problem for the evidence of it to exist.
narrative_ontology:disappearance_verdict(magna_carta_clause_39__liberal_due_process_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_clause_39__liberal_due_process_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_clause_39__liberal_due_process_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(magna_carta_clause_39__liberal_due_process_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_clause_39__liberal_due_process_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_clause_39__liberal_due_process_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_clause_39__liberal_due_process_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_clause_39__liberal_due_process_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.72 at interval end) because the constraint takes real decision authority from the executive — the reading's central claim — and because a procedural-rent layer has accreted: complexity that pays the profession and the courts whether or not it improves protection. Suppression (0.58) is structural, not internalized: the executive has no exit from a constitutionally binding constraint and subjects cannot waive the entitlement; there is no cognitive-fusion mechanism to weigh on the target side. Theater (0.28) is low-to-moderate: the constraint genuinely protects, but commemorative and rhetorical layers — 'eight centuries of liberty' invoked for purposes the clause does not govern — have grown on top of function. Accessibility collapse (0.5) is partial: workable alternatives to judicially enforced due process exist (legislative rights protection without strong review, parliamentary-supremacy models) and persist in real orders. Resistance (0.35) reflects recurring executive pushback — prerogative claims, emergency detentions, attempts to limit review — against broadly normalized compliance. Claim and metrics are independent authored facts: tangled_rope is claimed from structure (a genuine coordination function plus asymmetric extraction with an identifiable capturer, held by active enforcement), while the metrics describe observed operation; the engine computes per-seat types independently and any divergence is the datum. The base_extractiveness dip across t=100 to t=300 is enforcement decay, not meaning change: the reading holds the principle constant while its force atrophied, which the theater_ratio spike over the same span records. The suppression_requirement series is authored because this story specifically tracks enforcement-capacity change: armed baronial enforcement at t=0, atrophy through the ceremonial-reissue era, Coke-era reconstruction against active royal resistance, post-1689 institutionalization, and renewed enforcement demand in the modern emergency-powers era.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the executive's seat the constraint is pure limitation — every exercise of its core powers is conditional and its exit is constitutionally foreclosed. From free_subjects the same structure is protection they did not build and cannot waive. The judiciary's seat is distinctive: it enforces the constraint and is constituted by it. The identity lock is institutional — the bench has 'become' its function as guardian of lawful process, and its self-conception as heir of ancient liberties is fused with the constraint's maintenance; if that identity frame broke and courts came to see the jurisdiction as contingent rather than constitutive, the judiciary would recompute as a rent-collecting administrator and the constraint's extraction profile would sharpen toward the capturer pattern. Accused persons sit on both sides at once: the process that shields them is the process that costs them. Indigent litigants experience a thinner constraint than the reading promises — protection rationed by price. The engine derives these divergences from power, exit, and role; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   state_executive is declared the structural victim: it bears the constraint's costs (lost discretion) with no exit, placing it near the full-target end. free_subjects, accused_persons, legal_profession, and parliament are declared beneficiaries: the entitlement protects them, the procedures pay the profession, and the routing confers supremacy on parliament — all near the beneficiary end, with accused_persons pulled toward symmetric by the procedural costs they simultaneously bear. The judiciary is both enforcer and collector: its agenda-setting role and its jurisdiction gains place it at the beneficiary end despite administering the extraction, and its identity lock holds it there. No directionality overrides are used: the beneficiary/victim declarations plus exit options produce the correct relationships, and the one genuinely dual seat (accused_persons) is handled through its secondary payer role rather than an override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — arbitrary sovereign power — is live, so mandatrophy is not resolved and the constraint is not a piton: its function has not atrophied even where its enforcement once did (the t=100 to t=300 theater spike records an enforcement recession, and the revival is in the record). The classification prevents two opposite mislabelings. A snare reading — courts and lawyers extracting rents behind a rights story — would erase the genuine, adverse-party-attested protection the constraint delivers. A rope reading — pure coordination with everyone a net beneficiary — would erase the extraction asymmetry the manifest names: discretion is taken from a specific seat, jurisdiction and income accrue to specific seats, and protection is stratified by price. The tangled-rope classification holds both facts in one structure, which is what eight centuries of this constraint's operation show.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'This constraint is one reading of kernel magna_carta_clause_39. Would instantiating the feudal_prerogative_reading or the originalist_limitation_reading instead produce a structurally different constraint?',
    'Author the two sibling stories and compare computed classifications; adjudicate the 1215 text''s meaning against feudal land-law records and the drafting and reissue history of 1217 and 1225.',
    'If the feudal reading is right, the beneficiary set collapses from all subjects to a narrow free-tenantry class and the extraction inverts (baronial privilege taken from crown and unfree majority). If the originalist reading is right, the constraint''s scope shrinks to a catalog of documented royal abuses and the modern due-process machinery loses the ancestry this reading claims for it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Which reading of the clause text this constraint instantiates, and how the siblings would change its structure.').

omega_variable(
    constructed_vs_necessary_protection,
    'Is universal due process a constructed constitutional choice specific to this legal lineage, or a near-necessary feature of any durable legitimate order?',
    'Comparative constitutional analysis: whether durable legitimate orders that never inherited Clause 39 independently converge on functionally equivalent process constraints.',
    'If near-necessary, the constraint''s persistence requires less active enforcement and its profile sits closer to stable coordination; if constructed, it remains enforcement-dependent and vulnerable to the enforcement decay the measurement record shows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constructed_vs_necessary_protection, conceptual, 'Constructed constitutional artifact versus convergent necessity of legitimate order.').

omega_variable(
    procedural_rent_vs_coordination_cost,
    'How much of the legal system''s gain from the constraint is genuine coordination cost (process requires skilled advocates and judges) and how much is rent (complexity sustained because it pays)?',
    'Compare protection quality and outcome equity in jurisdictions that simplified procedure (specialized tribunals, streamlined criminal process) against complexity-matched common-law baselines.',
    'If the gain is mostly rent, effective extraction is higher and the asymmetry sharpens toward the capturer pattern; if mostly cost, the constraint sits closer to pure coordination than its current profile suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_rent_vs_coordination_cost, empirical, 'Rent versus coordination-cost share of the legal system''s take from the constraint.').

omega_variable(
    universalism_protection_gap,
    'Does the constraint protect all subjects equally, or is protection rationed by ability to pay for process?',
    'Outcome data on representation rates, remand duration, and case outcomes stratified by income within the constraint''s jurisdiction.',
    'If protection stratifies, the liberal reading''s universalism is only partially instantiated, the excluded indigent_litigants seat hardens into a structural cost-bearing set, and effective extraction against the unprotected rises.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(universalism_protection_gap, empirical, 'Whether universal protection is delivered uniformly or rationed by price.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__liberal_due_process_reading, 0, 800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magna_carta_liberal_dp_tr_t0, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(magna_carta_liberal_dp_tr_t0, observed).
narrative_ontology:measurement(magna_carta_liberal_dp_tr_t100, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 100, 0.55).
narrative_ontology:measurement_basis(magna_carta_liberal_dp_tr_t100, observed).
narrative_ontology:measurement(magna_carta_liberal_dp_tr_t200, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 200, 0.5).
narrative_ontology:measurement_basis(magna_carta_liberal_dp_tr_t200, observed).
narrative_ontology:measurement(magna_carta_liberal_dp_tr_t300, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 300, 0.45).
narrative_ontology:measurement_basis(magna_carta_liberal_dp_tr_t300, observed).
narrative_ontology:measurement(magna_carta_liberal_dp_tr_t400, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 400, 0.25).
narrative_ontology:measurement_basis(magna_carta_liberal_dp_tr_t400, observed).
narrative_ontology:measurement(magna_carta_liberal_dp_tr_t500, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 500, 0.2).
narrative_ontology:measurement_basis(magna_carta_liberal_dp_tr_t500, observed).
narrative_ontology:measurement(magna_carta_liberal_dp_tr_t600, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 600, 0.18).
narrative_ontology:measurement_basis(magna_carta_liberal_dp_tr_t600, observed).
narrative_ontology:measurement(magna_carta_liberal_dp_tr_t700, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 700, 0.2).
narrative_ontology:measurement_basis(magna_carta_liberal_dp_tr_t700, observed).
narrative_ontology:measurement(magna_carta_liberal_dp_tr_t800, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 800, 0.28).
narrative_ontology:measurement_basis(magna_carta_liberal_dp_tr_t800, observed).

% Extraction over time
narrative_ontology:measurement(magna_carta_liberal_dp_be_t0, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement_basis(magna_carta_liberal_dp_be_t0, observed).
narrative_ontology:measurement(magna_carta_liberal_dp_be_t100, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 100, 0.35).
narrative_ontology:measurement_basis(magna_carta_liberal_dp_be_t100, observed).
narrative_ontology:measurement(magna_carta_liberal_dp_be_t200, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 200, 0.3).
narrative_ontology:measurement_basis(magna_carta_liberal_dp_be_t200, observed).
narrative_ontology:measurement(magna_carta_liberal_dp_be_t300, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 300, 0.28).
narrative_ontology:measurement_basis(magna_carta_liberal_dp_be_t300, observed).
narrative_ontology:measurement(magna_carta_liberal_dp_be_t400, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 400, 0.45).
narrative_ontology:measurement_basis(magna_carta_liberal_dp_be_t400, observed).
narrative_ontology:measurement(magna_carta_liberal_dp_be_t500, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 500, 0.55).
narrative_ontology:measurement_basis(magna_carta_liberal_dp_be_t500, observed).
narrative_ontology:measurement(magna_carta_liberal_dp_be_t600, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 600, 0.62).
narrative_ontology:measurement_basis(magna_carta_liberal_dp_be_t600, observed).
narrative_ontology:measurement(magna_carta_liberal_dp_be_t700, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 700, 0.68).
narrative_ontology:measurement_basis(magna_carta_liberal_dp_be_t700, observed).
narrative_ontology:measurement(magna_carta_liberal_dp_be_t800, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 800, 0.72).
narrative_ontology:measurement_basis(magna_carta_liberal_dp_be_t800, observed).

% Suppression requirement over time
narrative_ontology:measurement(magna_carta_liberal_dp_su_t0, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement_basis(magna_carta_liberal_dp_su_t0, observed).
narrative_ontology:measurement(magna_carta_liberal_dp_su_t100, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 100, 0.4).
narrative_ontology:measurement_basis(magna_carta_liberal_dp_su_t100, observed).
narrative_ontology:measurement(magna_carta_liberal_dp_su_t200, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 200, 0.35).
narrative_ontology:measurement_basis(magna_carta_liberal_dp_su_t200, observed).
narrative_ontology:measurement(magna_carta_liberal_dp_su_t300, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 300, 0.3).
narrative_ontology:measurement_basis(magna_carta_liberal_dp_su_t300, observed).
narrative_ontology:measurement(magna_carta_liberal_dp_su_t400, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 400, 0.6).
narrative_ontology:measurement_basis(magna_carta_liberal_dp_su_t400, observed).
narrative_ontology:measurement(magna_carta_liberal_dp_su_t500, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 500, 0.55).
narrative_ontology:measurement_basis(magna_carta_liberal_dp_su_t500, observed).
narrative_ontology:measurement(magna_carta_liberal_dp_su_t600, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 600, 0.5).
narrative_ontology:measurement_basis(magna_carta_liberal_dp_su_t600, observed).
narrative_ontology:measurement(magna_carta_liberal_dp_su_t700, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 700, 0.52).
narrative_ontology:measurement_basis(magna_carta_liberal_dp_su_t700, observed).
narrative_ontology:measurement(magna_carta_liberal_dp_su_t800, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 800, 0.58).
narrative_ontology:measurement_basis(magna_carta_liberal_dp_su_t800, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_clause_39__liberal_due_process_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, magna_carta_clause_39__feudal_prerogative_reading).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, magna_carta_clause_39__originalist_limitation_reading).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, habeas_corpus_enforcement).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, us_due_process_clauses).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, echr_fair_trial_guarantee).

% DUAL FORMULATION NOTE:
% One text, three constraints: the 1215 clause is a kernel read three ways, and this story instantiates only the liberal universal-rights reading. The feudal and originalist readings are separate constraint stories with their own epsilon, beneficiary/victim structures, and classifications, linked here for family and contamination analysis. The liberal reading's epsilon is high because the reading assesses the standing constraint as substantially extracting from executive discretion — that extraction is the reading's own account of the constraint's force; the feudal sibling would author low epsilon against a narrow baronial class, and the originalist sibling would author low epsilon against a short catalog of royal abuses.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
