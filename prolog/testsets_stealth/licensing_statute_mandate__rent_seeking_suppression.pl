% ============================================================================
% CONSTRAINT STORY: licensing_statute_mandate__rent_seeking_suppression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_licensing_statute_mandate__rent_seeking_suppression, []).

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
 *   constraint_id: licensing_statute_mandate__rent_seeking_suppression
 *   human_readable: Occupational Licensing Statutes as Incumbent Supply Restriction (Rent-Seeking Reading)
 *   domain: economic/political/labor-regulatory
 *
 * SUMMARY:
 *   Statutory credential regimes — enacted state by state, administered by
 *   occupation-specific boards — condition the right to work in hundreds of
 *   occupations on completing mandated hours, examinations, and recurring
 *   fees. This story instantiates the rent_seeking_suppression reading of the
 *   licensing_statute_mandate kernel: it treats the statutes' operative
 *   function as restriction of labor supply for the benefit of current
 *   license holders, with the consumer-protection rationale functioning as
 *   the public justification the arrangement is defended by. Per the
 *   epsilon-referent rule, extractiveness is authored over the standing
 *   arrangement (the statutes as they operate), assessed by this reading's
 *   lights — never over the deregulated alternative this reading would
 *   prefer. The claim and the metrics are independent authored facts:
 *   claimed_type states what this reading holds structurally true; the
 *   metrics state what the descriptive record (wage-premium studies,
 *   stringency-versus-harm comparisons, enforcement dockets) shows. Sibling
 *   readings — public_safety_coordination and graduated_access_filter — are
 *   separate files with their own epsilon values, beneficiary sets, and
 *   classifications; see network.dual_formulation_note. KEY AGENTS (by
 *   structural relationship): - incumbent_licensees: primary beneficiary
 *   (organized/identity_locked) — collects the wage premium and demand
 *   insulation the credential sustains - state_licensing_boards: agenda
 *   setter (institutional/constrained) — administers entry standards and
 *   prosecutes unlicensed practice; staffed and funded from within the
 *   occupation - professional_associations: beneficiary and agenda setter
 *   (organized/mobile) — drafts model legislation and finances defense of the
 *   credential's value - proprietary_trade_schools: secondary beneficiary
 *   (organized/mobile) — sells the mandated training hours -
 *   aspiring_practitioners: primary target (powerless/constrained) — bears
 *   tuition, examination, and delay costs before earning -
 *   consumers_of_licensed_services: target via price (moderate/constrained) —
 *   pays elevated prices and waits - unlicensed_service_providers: target and
 *   excluded voice (powerless/trapped) — competes without the credential,
 *   faces fines and prosecution - state_legislators: agenda setter
 *   (institutional/arbitrage) — enacts and could repeal; hears concentrated
 *   testimony - policy_research_litigators: analytical observer
 *   (moderate/analytical) — compiles the evidence base and litigates board
 *   authority
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(licensing_statute_mandate__rent_seeking_suppression, 0.78).
domain_priors:suppression_score(licensing_statute_mandate__rent_seeking_suppression, 0.8).
domain_priors:theater_ratio(licensing_statute_mandate__rent_seeking_suppression, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, extractiveness, 0.78).
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(licensing_statute_mandate__rent_seeking_suppression, snare).
narrative_ontology:human_readable(licensing_statute_mandate__rent_seeking_suppression, "Occupational Licensing Statutes as Incumbent Supply Restriction (Rent-Seeking Reading)").
narrative_ontology:topic_domain(licensing_statute_mandate__rent_seeking_suppression, "economic/political/labor-regulatory").

domain_priors:requires_active_enforcement(licensing_statute_mandate__rent_seeking_suppression).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(licensing_statute_mandate__rent_seeking_suppression, '6f5bc83b-ee4e-4b21-a063-e4cfcc2b8be5').
narrative_ontology:cs_kernel_codification('6f5bc83b-ee4e-4b21-a063-e4cfcc2b8be5', formalized).
narrative_ontology:cs_authority_grounding('6f5bc83b-ee4e-4b21-a063-e4cfcc2b8be5', extraction).
narrative_ontology:cs_interpretation_layer_present('6f5bc83b-ee4e-4b21-a063-e4cfcc2b8be5').
narrative_ontology:cs_reading_relation('6f5bc83b-ee4e-4b21-a063-e4cfcc2b8be5', licensing_statute_mandate__public_safety_coordination, influences).
narrative_ontology:cs_reading_relation('6f5bc83b-ee4e-4b21-a063-e4cfcc2b8be5', licensing_statute_mandate__graduated_access_filter, influences).
narrative_ontology:cs_axiom('6f5bc83b-ee4e-4b21-a063-e4cfcc2b8be5', foundational, licensing_functions_as_supply_restriction).
narrative_ontology:cs_axiom_status(licensing_functions_as_supply_restriction, holdable).
narrative_ontology:cs_axiom_grounding('6f5bc83b-ee4e-4b21-a063-e4cfcc2b8be5', licensing_functions_as_supply_restriction, empirically_contingent).
narrative_ontology:cs_axiom('6f5bc83b-ee4e-4b21-a063-e4cfcc2b8be5', foundational, stringency_exceeds_harm_justification).
narrative_ontology:cs_axiom_status(stringency_exceeds_harm_justification, holdable).
narrative_ontology:cs_axiom_grounding('6f5bc83b-ee4e-4b21-a063-e4cfcc2b8be5', stringency_exceeds_harm_justification, empirically_contingent).
narrative_ontology:cs_reference_frame('6f5bc83b-ee4e-4b21-a063-e4cfcc2b8be5', incumbent_rent_preservation_regime).
narrative_ontology:cs_drift_state('6f5bc83b-ee4e-4b21-a063-e4cfcc2b8be5', contemporary_deregulation_scrutiny, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('6f5bc83b-ee4e-4b21-a063-e4cfcc2b8be5', '').
narrative_ontology:cs_kernel_id(licensing_statute_mandate__rent_seeking_suppression, licensing_statute_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__rent_seeking_suppression, incumbent_licensees).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__rent_seeking_suppression, professional_associations).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__rent_seeking_suppression, proprietary_trade_schools).
narrative_ontology:constraint_victim(licensing_statute_mandate__rent_seeking_suppression, aspiring_practitioners).
narrative_ontology:constraint_victim(licensing_statute_mandate__rent_seeking_suppression, consumers_of_licensed_services).
narrative_ontology:constraint_victim(licensing_statute_mandate__rent_seeking_suppression, unlicensed_service_providers).
narrative_ontology:constraint_vindicates(licensing_statute_mandate__rent_seeking_suppression, labor_supply_restriction_price_elevation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold current licenses in regulated occupations. Collect higher wages and steadier demand than unregulated comparison occupations show. Invested years of schooling, examination fees, and waiting periods to obtain the credential. Professional self-concept and retirement security are bound to the credential's continued exclusivity; leaving the occupation would forfeit that investment.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, incumbent_licensees, beneficiary,
    organized, biographical, identity_locked, national).

% Administer the statutes: set qualifying hours and examinations, issue and revoke licenses, investigate and prosecute unlicensed practice. Board seats are filled mostly by current license holders from the occupation. Operations are funded by application fees, renewal fees, and fines. Bound by enabling statutes they did not write and rarely recommend revising.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, state_licensing_boards, agenda_setter,
    institutional, generational, constrained, national).

% Organize incumbent practitioners, draft model legislation creating and expanding credential mandates, testify in rulemaking, and finance campaigns through political arms. Dues revenue depends on the credential's value to members; if mandates lapsed, the associations could redirect toward voluntary certification and continue operating.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, professional_associations, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(licensing_statute_mandate__rent_seeking_suppression, professional_associations, agenda_setter).

% Sell the mandated classroom hours and examination preparation. Enrollment is guaranteed by the mandate itself; program size expands and contracts with changes in required hours. Tuition is frequently financed by federal student aid, with default risk carried by the students.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, proprietary_trade_schools, beneficiary,
    organized, biographical, mobile, regional).

% Seek entry into licensed occupations. Face tuition, examination fees, supervised-hour requirements, and waiting periods before earning anything; some fail board examinations after sinking years of cost and exit with debt. Can switch to unlicensed occupations but forfeit the accumulated training investment.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, aspiring_practitioners, payer,
    powerless, biographical, constrained, national).

% Buy services from license holders at prices above those in comparable unregulated markets, and sometimes wait longer for appointments. Can substitute informal providers, self-service, or travel to neighboring jurisdictions in border regions; substitution is partial and unevenly available.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, consumers_of_licensed_services, payer,
    moderate, immediate, constrained, national).

% Practice the same trades without the credential — braiders, teeth-whiteners, landscape contractors operating at the edge of scope rules. Offer lower prices, receive cease-and-desist orders, fines, and occasional prosecution. Their skills are specific to the regulated trade; switching occupations means starting over.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, unlicensed_service_providers, payer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(licensing_statute_mandate__rent_seeking_suppression, unlicensed_service_providers, excluded).

% Enact and amend the credential statutes, usually at the requesting profession's initiative. Hear concentrated, well-resourced testimony from associations and boards alongside scattered consumer complaint. Hold the votes to narrow or repeal any mandate and face organized opposition and primary challenges if they attempt it.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, state_legislators, agenda_setter,
    institutional, biographical, arbitrage, national).

% Public-interest law firms and university economists compile wage-premium and stringency studies, challenge board authority in court, and advise sunset commissions. Neither collect from the arrangement nor bear its costs; their output shifts the information available to every other seat.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, policy_research_litigators, observer,
    moderate, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(licensing_statute_mandate__rent_seeking_suppression, incumbent_licensees).
narrative_ontology:fixing_cost_class(licensing_statute_mandate__rent_seeking_suppression, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, government-backed screen separating credentialed from uncredentialed practitioners in an occupation, giving buyers a searchable signal of minimum training and giving practitioners a common admission standard, along with a disciplinary registry.
% TRANSFER_FUNCTION: Moves money from service buyers (through prices elevated above unregulated-market levels) and from entry seekers (through tuition, examination, and delay costs) to incumbent license holders, trade schools, and board operations; moves discretion over who may work from individual practitioners to boards.
% ABSENT_VOICES: Entry seekers sit inside the arrangement as fee-payers but hold no board seats and rarely testify; buyers who are priced out entirely never enter the record; future cohorts who will pay the elevated prices have no present representative. Unlicensed providers appear only as respondents in enforcement dockets, never as witnesses on standards.
% DISAPPEARANCE_RATIONALE: Overnight repeal would expand working supply in affected occupations within months as trained-but-unlicensed practitioners entered, compress prices toward unregulated levels, idle much of the mandated-training sector, and force boards to dissolve or convert to voluntary certification bodies; incumbent incomes built on restricted supply would fall, and quality variance at the low end would widen until private certification refilled the signal.
% FOUNDING_PROBLEM: Progressive-era cities faced epidemic-scale fraud and harm: patent medicines killing users, building trades producing collapsing structures, untrained practitioners selling medical and engineering services with impunity, and buyers unable to distinguish competence before purchase.
% FOUNDING_PROBLEM_CORROBORATION: Historical mortality and accident records corroborate the original harms. Contemporary corroboration from outside the incumbent beneficiary set: the licensing wage-premium economics literature, Federal Trade Commission enforcement findings against occupation boards, and state sunset-commission reports attest that in many covered occupations the harm rationale now runs well ahead of documented risk. Board and association testimony attests the opposite; the status is disputed across seats rather than settled.
narrative_ontology:disappearance_verdict(licensing_statute_mandate__rent_seeking_suppression, world_rearranges).
narrative_ontology:founding_problem_status(licensing_statute_mandate__rent_seeking_suppression, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(licensing_statute_mandate__rent_seeking_suppression, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(licensing_statute_mandate__rent_seeking_suppression, 'none', 1).
narrative_ontology:epsilon_provenance(licensing_statute_mandate__rent_seeking_suppression, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(licensing_statute_mandate__rent_seeking_suppression_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(licensing_statute_mandate__rent_seeking_suppression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(licensing_statute_mandate__rent_seeking_suppression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.78) because the measurable flows — licensing wage premiums of roughly 10-15 percent in covered occupations, prices above unregulated comparison markets, and entry costs running to years of forgone income — are large relative to any documented service the screen renders at the margin. Suppression (0.80) is authored as a raw structural property, unscaled by power or scope: practicing without a credential is a criminal offense in every jurisdiction, boards hold investigative and prosecution powers, and the mandatory version displaced the voluntary-certification alternative. Theater_ratio (0.45) reflects a mixed record: genuine screening activity persists in high-harm occupations, while a growing share of standard-setting activity — hours mandates unrelated to documented harm, recurring fee schedules, scope-of-practice expansion — defends the credential's scarcity value rather than buyer safety. Accessibility_collapse (0.52) is mid-range: understood alternatives (certification, informal provision, relocation, occupational switch) survive but each carries real cost. Resistance (0.62) is substantive: constitutional litigation against boards, sunset-commission reviews, universal-recognition statutes, and a sustained economics literature. The measurement series share one grid (1950-2020, decade steps) across all three tracked metrics; trajectories are monotonic rather than cyclical — post-war expansion of coverage drove extraction and enforcement machinery upward together, with the safety justification growing more theatrical as mandates spread into low-harm occupations. Fixing cost is authored prohibitive: the vote-holder who could repeal faces concentrated, organized opposition against diffuse, unorganized gain — the classic arithmetic that keeps the statutes on the books.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute divergent types from identical statutes. The incumbent seat — identity-locked, with decade-deep credential investment and a self-concept fused with professional standing — experiences the arrangement as earned order and computes low extraction from inside it. The aspiring practitioner experiences a wall priced in years; the consumer a markup; the unlicensed provider an enforcement docket. The board experiences administration; the legislator a low-cost favor repayable in testimony and campaign support. Nothing in the authored claim adjudicates among these — the engine derives per-seat classifications from power, exit, and declared position, and the divergence is the datum the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (incumbent licensees, associations, trade schools) derive directionality near the beneficiary pole; declared victims derive near the target pole, with exit trapping modulating distance: unlicensed providers (trapped, skill-specific) sit nearest full-target, aspiring practitioners (constrained, sunk training investment) close behind, consumers (partial substitution available) somewhat further back. Identity lock binds the incumbents: professional identity, not merely income, is constituted by the credential, so exit is unthinkable regardless of arithmetic — break the identity frame and the beneficiary seat's computed extraction collapses toward the consumer seat's view. One override is authored for the institutional power atom (d = 0.28): the derivation chain reads agenda setters with no declared beneficiary/victim position as roughly symmetric, but both institutional seats here are tilted beneficiary-ward by capture — boards are staffed and funded from within the occupation, and legislators hear concentrated testimony and receive association-backed support — so the derived symmetry understates the tilt. Suppression remains unscaled; only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification decision here guards against the characteristic mislabel in both directions. Reading the safety cover story at face value scores the arrangement as pure coordination — solving adverse selection — and misses the asymmetric flows the wage-premium literature documents. Conversely, treating every mandate as pure extraction erases the occupations where the screen demonstrably prevents harm. The R5 interview locates the dispute: founding problem contested, disappearance world-rearranging — the arrangement still does something, and the open question is how much of what it does is the founding function versus the scarcity effect. The mismatch consumer reads status=contested x verdict=world_rearranges as live-function-with-contested-balance, not zombie; the zombie flag would require status=dead, which the corroboration record does not support at class level. The temporal series matters here: rising theater_ratio alongside rising extraction is the signature of a coordination story progressively converted into scarcity maintenance, and T17-style accumulation hypotheses should be checked against the stringency-harm omega before acceptance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Which reading of the licensing_statute_mandate kernel correctly identifies the arrangement''s operative function — protective screen, class-sorting barrier, or supply-restriction device serving current license holders?',
    'Cross-occupation analysis correlating entry stringency (mandated hours, fees, scope-of-practice breadth) with documented harm rates and with incumbent-income effects; adjudication by sunset commissions applying a harm-evidence standard.',
    'Resolution reassigns the story within the kernel: a harm-tracking pattern supports the public_safety_coordination reading (lower epsilon, different victim set); a class-correlated pattern supports the graduated_access_filter reading; stringency decoupled from both harm and service cost supports this reading as instantiated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'This constraint is one reading of the licensing_statute_mandate kernel; sibling readings would reassign epsilon and the victim set over the same statutes.').

omega_variable(
    stringency_harm_decoupling,
    'Does entry stringency track documented consumer harm across occupations, or does it track incumbent income protection?',
    'Panel regression across states and occupations of mandated hours and fees against injury and fraud rates and against incumbent wage effects; the cosmetology-versus-emergency-medical-hours contrast as the canonical probe.',
    'Decoupling confirms the rent-dominant account and hardens the computed classification; tracking would shift weight to the safety reading and soften effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stringency_harm_decoupling, empirical, 'Whether credential stringency correlates with harm or with incumbent income.').

omega_variable(
    certification_substitutability,
    'Could voluntary certification and disclosure deliver the information function the statutes provide, at lower cost to entry?',
    'Outcome comparison in jurisdictions and occupations that repealed mandates or never adopted them, controlling for service-quality measures and adverse-event rates.',
    'If substitutable, the measured extraction is surplus over coordination cost and the arrangement''s justification narrows to residual high-harm occupations; if not, part of the measured extraction is the price of the screen itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(certification_substitutability, empirical, 'Substitutability of voluntary certification for mandatory licensure.').

omega_variable(
    victim_coalition_feasibility,
    'Can the diffuse targets — entry seekers, buyers, criminalized informal providers — overcome their collective-action disadvantage and mount coalition-level resistance?',
    'Track formation and durability of cross-occupation reform coalitions, litigation funding vehicles, and ballot-measure campaigns; measure legislative success rates against concentration-based predictions.',
    'A durable coalition would raise realized resistance above the authored 0.62 and destabilize the arrangement toward repeal or conversion to voluntary bodies; persistent fragmentation confirms the persistence mechanics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_coalition_feasibility, empirical, 'Feasibility of coalition power among diffuse victim seats.').

omega_variable(
    aggregation_scope_framing,
    'Is occupational licensing one constraint (the national pattern-class authored here) or thousands of distinct state-by-occupation statutes with heterogeneous epsilon?',
    'Per-statute decomposition pilot: classify a stratified sample of individual mandates and test whether the class-level profile reproduces or masks the underlying distribution.',
    'If heterogeneous, the class-level epsilon conceals subpopulations that would compute as genuine screens alongside pure scarcity devices; per-statute files would supersede this aggregate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aggregation_scope_framing, conceptual, 'Framing choice: national pattern-class versus per-statute decomposition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(licensing_statute_mandate__rent_seeking_suppression, 1950, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lice_tr_t1950, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 1950, 0.2).
narrative_ontology:measurement_basis(lice_tr_t1950, observed).
narrative_ontology:measurement(lice_tr_t1960, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 1960, 0.24).
narrative_ontology:measurement_basis(lice_tr_t1960, observed).
narrative_ontology:measurement(lice_tr_t1970, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 1970, 0.28).
narrative_ontology:measurement_basis(lice_tr_t1970, observed).
narrative_ontology:measurement(lice_tr_t1980, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 1980, 0.32).
narrative_ontology:measurement_basis(lice_tr_t1980, observed).
narrative_ontology:measurement(lice_tr_t1990, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 1990, 0.35).
narrative_ontology:measurement_basis(lice_tr_t1990, observed).
narrative_ontology:measurement(lice_tr_t2000, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 2000, 0.38).
narrative_ontology:measurement_basis(lice_tr_t2000, observed).
narrative_ontology:measurement(lice_tr_t2010, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 2010, 0.42).
narrative_ontology:measurement_basis(lice_tr_t2010, observed).
narrative_ontology:measurement(lice_tr_t2020, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 2020, 0.45).
narrative_ontology:measurement_basis(lice_tr_t2020, observed).

% Extraction over time
narrative_ontology:measurement(lice_be_t1950, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 1950, 0.55).
narrative_ontology:measurement_basis(lice_be_t1950, observed).
narrative_ontology:measurement(lice_be_t1960, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 1960, 0.58).
narrative_ontology:measurement_basis(lice_be_t1960, observed).
narrative_ontology:measurement(lice_be_t1970, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 1970, 0.62).
narrative_ontology:measurement_basis(lice_be_t1970, observed).
narrative_ontology:measurement(lice_be_t1980, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 1980, 0.66).
narrative_ontology:measurement_basis(lice_be_t1980, observed).
narrative_ontology:measurement(lice_be_t1990, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 1990, 0.7).
narrative_ontology:measurement_basis(lice_be_t1990, observed).
narrative_ontology:measurement(lice_be_t2000, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 2000, 0.73).
narrative_ontology:measurement_basis(lice_be_t2000, observed).
narrative_ontology:measurement(lice_be_t2010, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 2010, 0.76).
narrative_ontology:measurement_basis(lice_be_t2010, observed).
narrative_ontology:measurement(lice_be_t2020, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 2020, 0.78).
narrative_ontology:measurement_basis(lice_be_t2020, observed).

% Suppression requirement over time
narrative_ontology:measurement(lice_su_t1950, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 1950, 0.5).
narrative_ontology:measurement_basis(lice_su_t1950, observed).
narrative_ontology:measurement(lice_su_t1960, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 1960, 0.54).
narrative_ontology:measurement_basis(lice_su_t1960, observed).
narrative_ontology:measurement(lice_su_t1970, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 1970, 0.6).
narrative_ontology:measurement_basis(lice_su_t1970, observed).
narrative_ontology:measurement(lice_su_t1980, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 1980, 0.65).
narrative_ontology:measurement_basis(lice_su_t1980, observed).
narrative_ontology:measurement(lice_su_t1990, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement_basis(lice_su_t1990, observed).
narrative_ontology:measurement(lice_su_t2000, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 2000, 0.74).
narrative_ontology:measurement_basis(lice_su_t2000, observed).
narrative_ontology:measurement(lice_su_t2010, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 2010, 0.78).
narrative_ontology:measurement_basis(lice_su_t2010, observed).
narrative_ontology:measurement(lice_su_t2020, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 2020, 0.8).
narrative_ontology:measurement_basis(lice_su_t2020, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(licensing_statute_mandate__rent_seeking_suppression, identity_coordination).
narrative_ontology:affects_constraint(licensing_statute_mandate__rent_seeking_suppression, public_safety_coordination).
narrative_ontology:affects_constraint(licensing_statute_mandate__rent_seeking_suppression, graduated_access_filter).

% DUAL FORMULATION NOTE:
% Constraint family: one colloquial label — 'occupational licensing' — covers three structurally distinct claims, decomposed per the epsilon-invariance principle. This file authors the rent-seeking reading (high epsilon over the standing statutes; incumbents benefit, entrants and buyers bear costs). public_safety_coordination authors the same statutes as a low-epsilon competence screen; graduated_access_filter authors them as a class-sorting access barrier with a different victim geometry. Edges record coupling: the safety reading supplies the legitimacy cover this reading rides on, and the rent mechanism (scarcity premium on credentials) causally feeds the tiered-access outcome the graduated-access reading describes. Each file carries its own epsilon, beneficiaries, and claimed type; divergence among their computed classifications is expected and informative, not an error.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(licensing_statute_mandate__rent_seeking_suppression, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
