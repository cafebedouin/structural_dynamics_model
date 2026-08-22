% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__inherent_executive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_powers_allocation__inherent_executive_reading, []).

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
 *   constraint_id: war_powers_allocation__inherent_executive_reading
 *   human_readable: Inherent Executive Authority over Force Initiation (Commander-in-Chief Reading)
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   This story instantiates the inherent_executive_reading of the
 *   war_powers_allocation kernel: the Commander-in-Chief grant itself confers
 *   authority to deploy force in defense of national interests without prior
 *   congressional authorization. Under this reading, authorization becomes a
 *   courtesy rather than a requirement, the legislative check enters the
 *   victim set, little active suppression of unilateral action is needed, and
 *   appropriations operate as ratification. The sibling readings
 *   (congressional_primacy_reading, functional_accommodation_reading) are
 *   separate constraint files over the same referent and are not described or
 *   averaged here. Epsilon is reading-indexed (OQ-26/OQ-258): the referent is
 *   the standing practice of unilateral deployment, and the value is authored
 *   by this reading's own lights, which place it well below what a hostile
 *   reading would author over the identical arrangement. The structural
 *   declarations (who benefits, who pays) are position facts that hold
 *   regardless of the reading's endorsement, and the claimed_type is stated
 *   independently of the metrics per the claim/metric independence rule.
 *
 * KEY AGENTS:
 *   - the_presidency: agenda-setter and principal beneficiary (institutional/arbitrage) — holds and compounds the initiation discretion
 *   - congress_as_institution: primary payer (institutional/trapped) — Article I powers operate as post-hoc ratification
 *   - national_security_establishment: secondary beneficiary (institutional/mobile) — budgets and mission space expand with each operation
 *   - military_service_members: payer (moderate/trapped) — bear deployment exposure without voice in initiation
 *   - american_public: payer with incidental beneficiary position (moderate/constrained) — bears costs, receives purported security
 *   - federal_judiciary: observer with enabling abstention (institutional/analytical) — political-question doctrine shields the arrangement
 *   - foreign_target_populations: excluded (powerless/trapped) — bear the direct effects of operations, never consulted
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__inherent_executive_reading, 0.48).
domain_priors:suppression_score(war_powers_allocation__inherent_executive_reading, 0.35).
domain_priors:theater_ratio(war_powers_allocation__inherent_executive_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__inherent_executive_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__inherent_executive_reading, "Inherent Executive Authority over Force Initiation (Commander-in-Chief Reading)").
narrative_ontology:topic_domain(war_powers_allocation__inherent_executive_reading, "constitutional/political").

domain_priors:requires_active_enforcement(war_powers_allocation__inherent_executive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__inherent_executive_reading, '61fa7cc4-c490-482b-80f3-0c762c64436b').
narrative_ontology:cs_kernel_codification('61fa7cc4-c490-482b-80f3-0c762c64436b', fixed_text).
narrative_ontology:cs_authority_grounding('61fa7cc4-c490-482b-80f3-0c762c64436b', lineage).
narrative_ontology:cs_interpretation_layer_present('61fa7cc4-c490-482b-80f3-0c762c64436b').
narrative_ontology:cs_reading_relation('61fa7cc4-c490-482b-80f3-0c762c64436b', war_powers_allocation__congressional_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('61fa7cc4-c490-482b-80f3-0c762c64436b', war_powers_allocation__functional_accommodation_reading, influences).
narrative_ontology:cs_axiom('61fa7cc4-c490-482b-80f3-0c762c64436b', foundational, commander_in_chief_grant_confers_initiation_authority).
narrative_ontology:cs_axiom_status(commander_in_chief_grant_confers_initiation_authority, holdable).
narrative_ontology:cs_axiom_grounding('61fa7cc4-c490-482b-80f3-0c762c64436b', commander_in_chief_grant_confers_initiation_authority, conventional).
narrative_ontology:cs_axiom('61fa7cc4-c490-482b-80f3-0c762c64436b', secondary, appropriations_ratify_rather_than_condition_war_making).
narrative_ontology:cs_axiom_status(appropriations_ratify_rather_than_condition_war_making, holdable).
narrative_ontology:cs_axiom_grounding('61fa7cc4-c490-482b-80f3-0c762c64436b', appropriations_ratify_rather_than_condition_war_making, conventional).
narrative_ontology:cs_reference_frame('61fa7cc4-c490-482b-80f3-0c762c64436b', plenary_commander_in_chief_prerogative).
narrative_ontology:cs_drift_state('61fa7cc4-c490-482b-80f3-0c762c64436b', contemporary_unitary_executive_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('61fa7cc4-c490-482b-80f3-0c762c64436b', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__inherent_executive_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__inherent_executive_reading, the_presidency).
narrative_ontology:constraint_beneficiary(war_powers_allocation__inherent_executive_reading, national_security_establishment).
narrative_ontology:constraint_victim(war_powers_allocation__inherent_executive_reading, congress_as_institution).
narrative_ontology:constraint_victim(war_powers_allocation__inherent_executive_reading, military_service_members).
narrative_ontology:constraint_victim(war_powers_allocation__inherent_executive_reading, american_public).
narrative_ontology:constraint_victim(war_powers_allocation__inherent_executive_reading, foreign_target_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(war_powers_allocation__inherent_executive_reading, american_public).
narrative_ontology:constraint_beneficiary(war_powers_allocation__inherent_executive_reading, federal_judiciary).
narrative_ontology:constraint_vindicates(war_powers_allocation__inherent_executive_reading, unitary_executive_theory).
narrative_ontology:constraint_vindicates(war_powers_allocation__inherent_executive_reading, political_question_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Directs the armed forces under the Commander-in-Chief clause and decides when and where to introduce force into hostilities. Issues the controlling legal opinions through the Office of Legal Counsel, notifies Congress after operations begin, and treats each completed operation as precedent widening the next one. Gains discretion over war initiation; its institutional memory spans administrations even as electoral accountability runs in four-year cycles.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, the_presidency, agenda_setter,
    institutional, generational, arbitrage, global).

% Holds the Article I powers to declare war, raise armies, and fund operations. Its authorization votes increasingly follow operations rather than precede them, and funding votes continue campaigns already underway. Attempts to condition or terminate funding meet veto threats and the political cost of appearing to abandon troops in the field. It cannot leave the constitutional structure, and its remedies all operate inside the arrangement they seek to discipline.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, congress_as_institution, payer,
    institutional, biographical, trapped, national).

% Executive agencies, combatant commands, intelligence services, and their contractor base. Mission scope, budgets, and operational tempo expand with each unilateral deployment, and planning proceeds on generational horizons insulated from electoral turnover. Its personnel circulate among agencies, firms, and advisory boards regardless of which party holds office.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, national_security_establishment, beneficiary,
    institutional, generational, mobile, global).

% Deploy into conflicts initiated without deliberative authorization, bearing the physical, psychological, and legal exposure that follows. Bound by oath and the uniform code of justice, they cannot decline particular operations, and initiation decisions are made far above their station. Veterans' organizations give them collective voice after service, but not a vote before it.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, military_service_members, payer,
    moderate, biographical, trapped, global).

% Receives the security the arrangement purports to provide and bears its costs: casualties, taxation, veterans' care, and retaliation risk. Expresses preferences through elections spaced years apart from any given operation, and war-initiation decisions rarely appear on any ballot. Exit is not available short of emigration; voice is intermittent and diffuse.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, american_public, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(war_powers_allocation__inherent_executive_reading, american_public, beneficiary).

% Declines most war-powers challenges under political-question and ripeness doctrines, reasoning that allocation disputes between the branches are not judicially manageable. The abstention insulates the courts from politically costly rulings, and the resulting silence is cited in the legal opinions that justify each successive operation. Its position is observational in form and load-bearing in effect.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, federal_judiciary, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(war_powers_allocation__inherent_executive_reading, federal_judiciary, beneficiary).

% Populations in the states where strikes and interventions occur. They are represented in no branch's deliberations, their consent is never solicited, and they typically learn of an operation when it arrives. Flight is possible for those with resources; for the rest, the operating area is simply where they live.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, foreign_target_populations, excluded,
    powerless, biographical, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_powers_allocation__inherent_executive_reading, the_presidency).
narrative_ontology:fixing_cost_class(war_powers_allocation__inherent_executive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies command of armed force in a single actor: speed, secrecy, and single-point accountability for emergencies in which deliberative process is too slow or too leaky to defend the nation.
% TRANSFER_FUNCTION: Moves the force-initiation decision right from Congress to the president; moves the costs of unconsented conflict (casualties, treasury, retaliation risk) onto service members, taxpayers, and affected foreign populations; and converts congressional funding votes from prospective authorization into retrospective ratification.
% ABSENT_VOICES: Foreign populations in operating areas are never consulted; service members have no institutional voice in initiation; future Congresses inherit prerogatives spent by predecessors; and the public speaks only through elections spaced years away from any given operation.
% DISAPPEARANCE_RATIONALE: Every deployment beyond immediate border defense would stall pending authorization; ongoing operations would face funding deadlines treated as termination points rather than ratification rituals; the president would negotiate terms with Congress before acting, restoring the deliberative gate the arrangement bypasses; and alliance planning, OLC practice, and combatant-command posture would all reorganize around the authorization requirement.
% FOUNDING_PROBLEM: The new republic needed to repel sudden attack without waiting for a deliberative assembly, while avoiding a monarch's unilateral war-making. The Convention split the difference: command of the forces to the executive, declaration of war to the legislature.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the War Powers Resolution's own 60-day window concedes the emergency-response need, and it was drafted by the arrangement's principal opponents; Anti-Federalist acceptance of Washington's defensive actions against frontier raids attests the immediacy problem from the opposition of the day; and scholarly consensus across the spectrum accepts that immediate-defense speed is a real requirement. No one outside the executive attests that 'national interests' generally, as opposed to imminent defense, falls within the founding problem.
narrative_ontology:disappearance_verdict(war_powers_allocation__inherent_executive_reading, world_rearranges).
narrative_ontology:founding_problem_status(war_powers_allocation__inherent_executive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__inherent_executive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(war_powers_allocation__inherent_executive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(war_powers_allocation__inherent_executive_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_powers_allocation__inherent_executive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_powers_allocation__inherent_executive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_powers_allocation__inherent_executive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored at 0.48 by this reading's lights: the reading regards unilateral deployment as constitutionally proper execution of a granted power, while candidly acknowledging the accountability gap and the unconsented costs — hence moderate, not negligible, and far below the value a congressional-primacy authorship would assign to the same referent. Suppression is 0.35: formal alternatives persist on paper (authorization votes, funding conditions, litigation) but are politically inert, and the coercive residue is the veto wall plus the justiciability bar. The suppression_requirement series FALLS across the interval (0.55 to 0.35): this models the decay of effective opposition capacity, not liberalization — the early arrangement required vigorous institutional defense (Truman-era confrontation, Youngstown), while the contemporary arrangement entrenches by normalization and needs only token defense. Theater_ratio rises steadily (0.20 to 0.62) as consultation becomes notification, notification becomes ritual, and the 60-day clock is managed through reinterpretations of 'hostilities'; the command function itself remains entirely real, which is why high theater coexists with a non-piton profile. Accessibility_collapse is 0.60: once actors understand how the arrangement operates, alternatives collapse in practice (courts close, funding converts to ratification) though they survive in text. Resistance is 0.40: the War Powers Resolution, the Libya-era suits, and periodic privileged resolutions are real but episodic and weak. The series shares one time grid (t = 0, 15, 30, 45, 60, 75) across all tracked metrics; the mid-interval dip in extractiveness corresponds to the Vietnam-backlash/War-Powers-Resolution window and is an external-shock effect, not a cycle. Coalition note: the classic remedy is a Congress-plus-public coalition conditioned on funding, and its persistent failure to form (electoral cycles, troop-abandonment framing, collective-action costs) is what keeps payer resistance at 0.40 rather than higher.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical nominal power. From the presidency's position the arrangement is coordination it performs: unity of command, speed, secrecy, and an accountability it locates in elections and the purse. From the congressional seat the same structure is dispossession administered through its own voting rituals. The judiciary experiences its abstention as neutrality while functioning as an enabler whose silence is cited in every justifying memo. The public seat is internally split between the security it is told it receives and the costs it demonstrably bears. Service members and foreign populations occupy the extreme target positions with the least voice. The engine derives this divergence from the structural data; nothing in the authored claim adjudicates it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (the_presidency, national_security_establishment) drive those seats toward the beneficiary end; victim declarations (congress_as_institution, military_service_members, american_public, foreign_target_poplications) drive them toward the target end, amplified by trapped or constrained exit. The public's dual role (payer with secondary beneficiary position) lands it near-symmetric but slightly target-side, since the costs are concrete and the security diffuse. The judiciary's observer role yields a near-symmetric derivation with a mild beneficiary lean via institutional insulation. No directionality_overrides are authored: overrides key on power atoms, and the four institutional seats diverge sharply in their true directionalities, so any atom-level override would distort three seats to tune one — the role-plus-exit derivation is strictly finer-grained here.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — rapid response to sudden attack — remains live in narrowed form, but the doctrine's operative scope ('national interests' generally) exceeds it, which is why founding_problem_status is contested rather than dead. The mismatch consumer reads status x disappearance_verdict: contested-status plus world_rearranges does not fire the zombie flag, correctly, because the arrangement's core function has not atrophied — command coordination is performed constantly and theater_ratio, while high, measures consultative ritual layered on top of a living function, not performance substituting for a dead one. The classification prevents mislabeling in both directions: the arrangement is not pure extraction (the coordination core is real and even the arrangement's opponents concede the emergency need), and it is not pure coordination (the transfer of the initiation right and the conversion of funding into ratification are real and asymmetrical). Hence tangled_rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading (inherent_executive_reading) of the war_powers_allocation kernel; what structurally changes if a sibling reading is adopted instead?',
    'Author the sibling stories (congressional_primacy_reading, functional_accommodation_reading) over the same referent and compare epsilon, victim sets, and computed types; the disagreement is located in whether the declare-war clause binds as precondition or survives as formality.',
    'Under congressional_primacy_reading, epsilon rises sharply and the presidency flips from beneficiary toward constrained actor; under functional_accommodation_reading, the victim set splits by operational context. The classification of this file is valid only within this reading''s commitments.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: reading-indexed membership in the war_powers_allocation kernel family.').

omega_variable(
    appropriations_ratification_vs_coerced_continuation,
    'Is congressional funding of ongoing operations ratification (as this reading holds) or coerced continuation under sunk-cost and troop-abandonment pressure (as critics hold)?',
    'Examine the historical record of attempted defunding (Cambodia 1973, the Lebanon 1983 timeline, Somalia withdrawal sequencing) for cases where funding leverage was actually exercised against a sitting operation and what happened.',
    'If funding is genuine leverage, the arrangement requires more active suppression than measured and drifts toward enforced extraction; if funding is ratification, the transfer function operates substantially by consent and the measured suppression stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appropriations_ratification_vs_coerced_continuation, empirical, 'Whether the appropriations-as-ratification mechanism is voluntary ratification or absorbed leverage.').

omega_variable(
    national_interest_scope_boundary,
    'Where is the boundary between the founding problem''s immediate-defense core (universally conceded) and the ''national interests'' extension (contested), and does the extension swallow the core?',
    'Case-by-case analysis of the major unilateral deployments (Korea, Kosovo, Libya) against explicit imminence criteria, asking whether each is defensible under the narrow founding problem alone.',
    'A narrow boundary lowers epsilon and pulls this reading toward the functional_accommodation sibling; a boundary that dissolves under scrutiny raises epsilon and strengthens the congressional-primacy critique from within this reading''s own concessions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(national_interest_scope_boundary, conceptual, 'Scope boundary between emergency defense and interest-based initiation.').

omega_variable(
    judicial_abstention_durability,
    'Will the political-question and ripeness barriers continue to shield the arrangement from adjudication?',
    'Track the post-Libya and post-AUMF-expansion litigation line for any case in which a court reaches the merits of the allocation question rather than dismissing on justiciability grounds.',
    'If courts begin reaching merits, suppression rises sharply, the arrangement''s persistence becomes enforcement-dependent, and the classification shifts toward the extractive pole; continued abstention leaves the current profile intact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_abstention_durability, empirical, 'Durability of the judicial shield that keeps measured suppression low.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__inherent_executive_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t0, war_powers_allocation__inherent_executive_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(war__tr_t0, observed).
narrative_ontology:measurement(war__tr_t15, war_powers_allocation__inherent_executive_reading, theater_ratio, 15, 0.3).
narrative_ontology:measurement_basis(war__tr_t15, observed).
narrative_ontology:measurement(war__tr_t30, war_powers_allocation__inherent_executive_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement_basis(war__tr_t30, observed).
narrative_ontology:measurement(war__tr_t45, war_powers_allocation__inherent_executive_reading, theater_ratio, 45, 0.45).
narrative_ontology:measurement_basis(war__tr_t45, observed).
narrative_ontology:measurement(war__tr_t60, war_powers_allocation__inherent_executive_reading, theater_ratio, 60, 0.55).
narrative_ontology:measurement_basis(war__tr_t60, observed).
narrative_ontology:measurement(war__tr_t75, war_powers_allocation__inherent_executive_reading, theater_ratio, 75, 0.62).
narrative_ontology:measurement_basis(war__tr_t75, observed).

% Extraction over time
narrative_ontology:measurement(war__be_t0, war_powers_allocation__inherent_executive_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(war__be_t0, observed).
narrative_ontology:measurement(war__be_t15, war_powers_allocation__inherent_executive_reading, base_extractiveness, 15, 0.36).
narrative_ontology:measurement_basis(war__be_t15, observed).
narrative_ontology:measurement(war__be_t30, war_powers_allocation__inherent_executive_reading, base_extractiveness, 30, 0.3).
narrative_ontology:measurement_basis(war__be_t30, observed).
narrative_ontology:measurement(war__be_t45, war_powers_allocation__inherent_executive_reading, base_extractiveness, 45, 0.38).
narrative_ontology:measurement_basis(war__be_t45, observed).
narrative_ontology:measurement(war__be_t60, war_powers_allocation__inherent_executive_reading, base_extractiveness, 60, 0.44).
narrative_ontology:measurement_basis(war__be_t60, observed).
narrative_ontology:measurement(war__be_t75, war_powers_allocation__inherent_executive_reading, base_extractiveness, 75, 0.48).
narrative_ontology:measurement_basis(war__be_t75, observed).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t0, war_powers_allocation__inherent_executive_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(war__su_t0, observed).
narrative_ontology:measurement(war__su_t15, war_powers_allocation__inherent_executive_reading, suppression_requirement, 15, 0.5).
narrative_ontology:measurement_basis(war__su_t15, observed).
narrative_ontology:measurement(war__su_t30, war_powers_allocation__inherent_executive_reading, suppression_requirement, 30, 0.45).
narrative_ontology:measurement_basis(war__su_t30, observed).
narrative_ontology:measurement(war__su_t45, war_powers_allocation__inherent_executive_reading, suppression_requirement, 45, 0.4).
narrative_ontology:measurement_basis(war__su_t45, observed).
narrative_ontology:measurement(war__su_t60, war_powers_allocation__inherent_executive_reading, suppression_requirement, 60, 0.37).
narrative_ontology:measurement_basis(war__su_t60, observed).
narrative_ontology:measurement(war__su_t75, war_powers_allocation__inherent_executive_reading, suppression_requirement, 75, 0.35).
narrative_ontology:measurement_basis(war__su_t75, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__inherent_executive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, congressional_primacy_reading).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, functional_accommodation_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'war powers' decomposes into three structurally distinct allocations over one kernel (war_powers_allocation), per the epsilon-invariance family rule. This file is the inherent-executive member. The congressional-primacy member authors high epsilon over the same referent; the functional-accommodation member splits the victim set by operational context. Edges: this reading's core premise directly contradicts the primacy premise (forecloses), and each successful unilateral assertion ratchets the baseline the accommodation reading must treat as permissible (influences) without logically eliminating it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
