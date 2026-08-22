% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__inherent_executive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   human_readable: Inherent Executive War-Initiative Allocation (Commander-in-Chief Reading)
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   Under the inherent-executive reading of Article II, the president may
 *   initiate and sustain military force in defense of national interests
 *   without prior congressional authorization; authorization, where sought,
 *   confirms rather than conditions. This story instantiates that reading as
 *   a standing allocation: one office decides, the legislature records, the
 *   funding process ratifies. The claim/metric gap is deliberate — the
 *   reading is CLAIMED as tangled_rope (a genuine rapid-command coordination
 *   function fused with a one-way transfer of war-initiating judgment away
 *   from the deliberative branch), while the authored metrics describe
 *   substantially extractive, actively maintained operation whose enforcement
 *   cost falls over time as precedents accumulate. The engine measures the
 *   divergence; nothing here reconciles the claim to the metrics.
 *
 * KEY AGENTS:
 *   - - sitting_president: Primary beneficiary and agenda-setter (institutional/arbitrage) — collects war-initiation discretion and manufactures the precedent that entrenches it
 *   - - congress: Primary target (institutional/trapped) — surrenders initiating judgment; retains ratification-shaped instruments
 *   - - service_members_deployed: Material target (powerless/trapped) — bear the physical costs that make defunding unthinkable
 *   - - taxpaying_public: Diffuse target with incidental benefit (moderate/constrained)
 *   - - national_security_establishment: Secondary beneficiary (institutional/mobile) — staffs and expands the asserted power
 *   - - federal_courts: Observational seat (institutional/analytical) — abstention is load-bearing
 *   - - antiwar_legislators_and_movements: Excluded voice (organized/constrained)
 *   - - foreign_populations_in_theater: Excluded voice bearing the largest human costs (powerless/trapped)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__inherent_executive_reading, 0.68).
domain_priors:suppression_score(war_powers_allocation__inherent_executive_reading, 0.52).
domain_priors:theater_ratio(war_powers_allocation__inherent_executive_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__inherent_executive_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__inherent_executive_reading, "Inherent Executive War-Initiative Allocation (Commander-in-Chief Reading)").
narrative_ontology:topic_domain(war_powers_allocation__inherent_executive_reading, "constitutional/political").

domain_priors:requires_active_enforcement(war_powers_allocation__inherent_executive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__inherent_executive_reading, '4f167231-3eeb-4aaf-868d-e2f0d2b11e1c').
narrative_ontology:cs_kernel_codification('4f167231-3eeb-4aaf-868d-e2f0d2b11e1c', fixed_text).
narrative_ontology:cs_authority_grounding('4f167231-3eeb-4aaf-868d-e2f0d2b11e1c', extraction).
narrative_ontology:cs_interpretation_layer_present('4f167231-3eeb-4aaf-868d-e2f0d2b11e1c').
narrative_ontology:cs_reading_relation('4f167231-3eeb-4aaf-868d-e2f0d2b11e1c', war_powers_allocation__congressional_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('4f167231-3eeb-4aaf-868d-e2f0d2b11e1c', war_powers_allocation__functional_accommodation_reading, forecloses).
narrative_ontology:cs_axiom('4f167231-3eeb-4aaf-868d-e2f0d2b11e1c', foundational, command_power_independent_of_prior_authorization).
narrative_ontology:cs_axiom_status(command_power_independent_of_prior_authorization, holdable).
narrative_ontology:cs_axiom_grounding('4f167231-3eeb-4aaf-868d-e2f0d2b11e1c', command_power_independent_of_prior_authorization, conventional).
narrative_ontology:cs_axiom('4f167231-3eeb-4aaf-868d-e2f0d2b11e1c', secondary, executive_energy_required_for_national_survival).
narrative_ontology:cs_axiom_status(executive_energy_required_for_national_survival, holdable).
narrative_ontology:cs_axiom_grounding('4f167231-3eeb-4aaf-868d-e2f0d2b11e1c', executive_energy_required_for_national_survival, instrumental).
narrative_ontology:cs_reference_frame('4f167231-3eeb-4aaf-868d-e2f0d2b11e1c', inherent_plenary_command_authority).
narrative_ontology:cs_drift_state('4f167231-3eeb-4aaf-868d-e2f0d2b11e1c', contemporary_post_911_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('4f167231-3eeb-4aaf-868d-e2f0d2b11e1c', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__inherent_executive_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__inherent_executive_reading, sitting_president).
narrative_ontology:constraint_beneficiary(war_powers_allocation__inherent_executive_reading, national_security_establishment).
narrative_ontology:constraint_victim(war_powers_allocation__inherent_executive_reading, congress).
narrative_ontology:constraint_victim(war_powers_allocation__inherent_executive_reading, service_members_deployed).
narrative_ontology:constraint_victim(war_powers_allocation__inherent_executive_reading, taxpaying_public).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(war_powers_allocation__inherent_executive_reading, taxpaying_public).
narrative_ontology:constraint_vindicates(war_powers_allocation__inherent_executive_reading, inherent_commander_in_chief_doctrine).
narrative_ontology:constraint_vindicates(war_powers_allocation__inherent_executive_reading, unitary_executive_theory).
narrative_ontology:constraint_vindicates(war_powers_allocation__inherent_executive_reading, political_question_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Initiates and sustains military operations on the strength of Article II command authority, obtaining Justice Department opinions that bless each novel use before or after the fact. Files War Powers Resolution reports as informational notices rather than requests for permission, and counts on subsequent funding votes to convert completed operations into settled practice. The office accumulates rather than surrenders this discretion; narrowing it is available at any moment and never taken.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, sitting_president, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(war_powers_allocation__inherent_executive_reading, sitting_president, beneficiary).

% Holds the formal instruments — declarations, authorizations, appropriations, impeachment — but each carries a political price that rises once troops are committed: cutting funds reads publicly as abandoning soldiers in the field, and authorization votes held after operations begin ratify rather than deliberate. The chamber's war-initiating judgment operates mostly as recorded assent. Its alternatives narrow to choosing between funding finished facts and wearing the blame for their consequences.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, congress, payer,
    institutional, generational, trapped, national).

% Execute deployments ordered without prior legislative judgment, bearing casualty risk and legal exposure under military law. Cannot decline lawful orders and cannot leave the force at will. Their physical presence in a theater is what makes withholding funds politically untenable, so they stand at the center of the mechanism that converts disputed beginnings into accepted facts.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, service_members_deployed, payer,
    powerless, immediate, trapped, global).

% Finances the operations and receives whatever protection they purchase. Shapes war policy mainly through elections that rarely turn on it, and has no forum in which the decision to begin a war is made in its name before the fact. Bears the treasury costs and the grief; enjoys the security the force provides.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, taxpaying_public, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(war_powers_allocation__inherent_executive_reading, taxpaying_public, beneficiary).

% Executive agencies, departmental counsel, and the uniformed staff plan the operations, produce the legal memoranda, and run the resulting campaigns. Careers advance by demonstrating that the tools work; personnel rotate among agencies, administrations, and the private sector regardless of how any particular operation ends. Collects budgets, staffing, and mission scope that grow with each successful assertion.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, national_security_establishment, beneficiary,
    institutional, generational, mobile, global).

% Dismiss challenges to unauthorized operations under political-question, standing, and ripeness doctrines. Neither commands nor forbids anything and collects nothing. Its abstention is nonetheless load-bearing: with litigation closed, the remaining checks are political ones that the committed-troops dynamic prices out of reach.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, federal_courts, observer,
    institutional, generational, analytical, national).

% Seek binding limits, funding cutoffs, and court rulings; introduce resolutions that die under veto threats and procedural scheduling. Are heard seriously only after commitments become irreversible. Would restructure the allocation toward prior authorization if admitted as an equal participant rather than a dissenter to be managed.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, antiwar_legislators_and_movements, excluded,
    organized, biographical, constrained, national).

% Absorb the violence of operations begun without any deliberation in which they had a voice, in countries whose governments may be bypassed or toppled. Have no representation in any branch of the deploying state and are never asked. Bear the largest human costs of the arrangement while holding the least power over it.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, foreign_populations_in_theater, excluded,
    powerless, immediate, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_powers_allocation__inherent_executive_reading, sitting_president).
narrative_ontology:fixing_cost_class(war_powers_allocation__inherent_executive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides unified, rapid, and secret command over military force: a single chain of command can meet sudden attacks and fast-moving crises faster than a multimember deliberative body can convene, and can sustain coherent strategy across a professional military dispersed worldwide.
% TRANSFER_FUNCTION: Moves war-initiation discretion from the bicameral legislature to a single executive; moves the costs of war — casualties, treasury, geopolitical risk — onto service members, taxpayers, and foreign populations, while converting each completed operation into precedent that further concentrates decision rights in the presidency.
% ABSENT_VOICES: Foreign populations in deployed theaters have no seat anywhere in the allocating government. Antiwar legislators and movements are consulted only after commitments become irreversible. The founding generation's explicit fear — executive war-making as the road back to monarchy — is argued today by no institution inside the conversation.
% DISAPPEARANCE_RATIONALE: Every deployment beyond immediate self-defense would require prior statutory authorization; dozens of standing operations and forward-posture commitments would face immediate legality and funding cliffs; the presidency would lose roughly two centuries of accumulated war-initiative precedent, and the legislative branch would recover a core constitutional function it currently exercises mainly as ceremony.
% FOUNDING_PROBLEM: Give the national government energy sufficient to repel sudden attacks and manage fast-moving foreign threats — the decisive failure of the Articles of Confederation — without recreating the monarchical war-making power the Revolution was fought to escape.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set by the founding-era record itself: Federalist 69 concedes the commander-in-chief role while insisting that the power to declare war stays with Congress, and the 1973 War Powers Resolution's findings — authored by the branch this reading displaces — attest both the reality of the threat-response problem and the claim that the modern reading exceeds it. No corroboration exists outside the offices that hold the power for extending the problem from 'sudden attacks' to open-ended 'national interests.'
narrative_ontology:disappearance_verdict(war_powers_allocation__inherent_executive_reading, world_rearranges).
narrative_ontology:founding_problem_status(war_powers_allocation__inherent_executive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__inherent_executive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(war_powers_allocation__inherent_executive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(war_powers_allocation__inherent_executive_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is high (0.68 at interval end) because the arrangement transfers war-initiation judgment from a 535-member deliberative body to one office and converts each exercise into precedent; it stops short of maximal because Congress retains appropriations, oversight, and impeachment, and because elections occasionally price the power. Suppression (0.52) is moderate and structural rather than coercive: the arrangement holds less by punishing dissent than by making every alternative instrument — defunding, litigation, binding statute — politically ruinous once forces are committed. Suppression is authored as a raw structural property; only extractiveness is scaled by directionality and scope downstream. Theater (0.46) reflects the growing share of legislative war activity that is ceremonial: post-hoc authorization votes, informational consultations, resolutions that bind no one. Accessibility collapse (0.60): alternatives remain formally open (statute, amendment, litigation) but collapse in practice against veto points and the committed-forces dilemma. Resistance (0.55): the 1973 Resolution, recurring funding battles, and serial litigation show real, repeated, unsuccessful pushback. The three temporal series share one eight-point grid so every metric is authored at every examined time point. The suppression_requirement series is included because enforcement capacity itself changed over the interval — departmental legal opinion institutionalized after 1973 and court-abstention doctrine hardened, lowering the marginal cost of each new assertion. The trajectory is a ratchet with two accommodation dips (the 1973 override fight, the 1991 Gulf authorization) rather than a cycle; the dips are concessions inside the reading's frame, not reversals of it. Service members are individually powerless, but coalition potential with antiwar movements was demonstrated during Vietnam and then suppressed through discipline, prosecution, and the draft's end — the victim set's weakness is partly produced, not natural.
 *
 * PERSPECTIVAL GAP:
 *   From the presidency's seat the arrangement computes as a functioning capability it built, maintains, and uses responsibly — a coordination structure whose costs it internalizes. From Congress's seat the same structure operates as the stripping of a core constitutional function down to ceremony, with ratification duties attached. Courts occupy an observational seat that declines to rank the two accounts. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Sitting presidents and the national-security establishment sit near the beneficiary pole: the arrangement subsidizes them with discretion, budgets, and precedent, and their exit from it is arbitrage — claims can be reshaped office by office, and holders never personally bear the arrangement's costs. Congress sits near the target pole: it pays in surrendered function and ratification labor, and its exit is trapped because every instrument it holds turns against it once troops are committed. Service members and taxpayers carry the material costs with little compensating control; the public's secondary beneficiary role (security delivered) moderates but does not offset its payer position. Foreign populations in theater are pure targets with no seat at all. Courts derive a near-symmetric directionality: they neither collect nor pay.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — energy sufficient to repel sudden attacks — remains live, which keeps the arrangement from decaying into pure performance: the command apparatus genuinely functions and is continuously used. But the reading's scope has outrun the problem it was built to solve: 'national interests' is far broader than 'sudden attacks,' and the surplus is maintained by precedent accumulation rather than by any renewed founding justification. If the coordination function were narrowed to imminent defense, the residual broad-scope assertion would be theatrical maintenance of transferred power — the classic atrophy signature. Reading the genealogy this way prevents two mislabels: it blocks calling the whole arrangement pure predation (the rapid-response function is real), and it blocks calling it healthy coordination (the scope surplus serves the holder, not the problem).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Does the fixed constitutional text determine the war-powers allocation, or is the Commander-in-Chief Clause underdetermined such that each reading projects its own allocation onto it?',
    'Comparative structural analysis of the three readings against founding-era usage, early practice (the Neutrality Proclamation, the Quasi-War), and independent originalist methods; convergence across methods would indicate the text determines the allocation.',
    'If underdetermined, this constraint is one contingent projection among live siblings and its classification travels with whichever reading prevails; under congressional_primacy_reading the presidency flips from beneficiary to usurping agenda-setter and Congress exits the victim set entirely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Whether the kernel text fixes the allocation or admits rival projections.').

omega_variable(
    appropriations_ratification_status,
    'Does subsequent congressional funding of an unauthorized operation constitute constitutional ratification that entrenches the inherent-authority reading, or coerced support for troops already committed that ratifies nothing?',
    'Track episodes where Congress funded operations under explicit disclaimer language or cut funds mid-operation (Indochina 1973, Angola 1976); sustained disclaimers that fail to alter subsequent practice indicate ratification-by-appropriations is doing the entrenching work.',
    'If appropriations ratify, the arrangement self-entrenches and suppression stays low because resistance converts into ratification; if not, the enforcement mechanism is weaker than it appears and the reading depends on inertia alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appropriations_ratification_status, empirical, 'Whether the funding mechanism ratifies or merely takes the legislature hostage.').

omega_variable(
    threat_environment_naturalness,
    'Is broad unilateral war initiative a structural feature of the modern security environment — something that would persist under any allocation because threats outrun deliberation — or a constructed accumulation maintained because identifiable offices benefit from it?',
    'Compare allied democracies with parliamentary war-approval requirements (Commons votes, Bundestag mandates): if they meet comparable threat environments with prior authorization, environmental necessity fails as an explanation.',
    'If environmentally necessary, part of the measured extraction is irreducible coordination cost and the arrangement sits closer to rope; if constructed, the full asymmetry is attributable to the allocation and false-summit scrutiny applies to any naturality framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threat_environment_naturalness, empirical, 'Environmental necessity versus constructed accumulation of war initiative.').

omega_variable(
    judicial_abstention_character,
    'Is the courts'' political-question abstention a principled allocation of war powers to the political branches, or the removal of the only external check that would otherwise discipline the reading?',
    'Test counterfactual justiciability: whether standing, ripeness, and political-question doctrine could be reformulated to adjudicate authorization disputes without managing campaigns, and whether courts declined available narrower rulings in the Merflark-line of war-powers suits.',
    'If abdication, suppression of the congressional-primacy alternative is higher than the scalar suggests because the litigation exit is illusory; if principled, abstention is part of the allocation itself rather than its enforcement machinery.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_abstention_character, conceptual, 'Principled deference versus check-removal in judicial abstention.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__inherent_executive_reading, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wpa_inherent_exec_tr_t1950, war_powers_allocation__inherent_executive_reading, theater_ratio, 1950, 0.24).
narrative_ontology:measurement_basis(wpa_inherent_exec_tr_t1950, observed).
narrative_ontology:measurement(wpa_inherent_exec_tr_t1964, war_powers_allocation__inherent_executive_reading, theater_ratio, 1964, 0.31).
narrative_ontology:measurement_basis(wpa_inherent_exec_tr_t1964, observed).
narrative_ontology:measurement(wpa_inherent_exec_tr_t1973, war_powers_allocation__inherent_executive_reading, theater_ratio, 1973, 0.39).
narrative_ontology:measurement_basis(wpa_inherent_exec_tr_t1973, observed).
narrative_ontology:measurement(wpa_inherent_exec_tr_t1983, war_powers_allocation__inherent_executive_reading, theater_ratio, 1983, 0.41).
narrative_ontology:measurement_basis(wpa_inherent_exec_tr_t1983, observed).
narrative_ontology:measurement(wpa_inherent_exec_tr_t1991, war_powers_allocation__inherent_executive_reading, theater_ratio, 1991, 0.34).
narrative_ontology:measurement_basis(wpa_inherent_exec_tr_t1991, observed).
narrative_ontology:measurement(wpa_inherent_exec_tr_t2001, war_powers_allocation__inherent_executive_reading, theater_ratio, 2001, 0.43).
narrative_ontology:measurement_basis(wpa_inherent_exec_tr_t2001, observed).
narrative_ontology:measurement(wpa_inherent_exec_tr_t2011, war_powers_allocation__inherent_executive_reading, theater_ratio, 2011, 0.49).
narrative_ontology:measurement_basis(wpa_inherent_exec_tr_t2011, observed).
narrative_ontology:measurement(wpa_inherent_exec_tr_t2025, war_powers_allocation__inherent_executive_reading, theater_ratio, 2025, 0.46).
narrative_ontology:measurement_basis(wpa_inherent_exec_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(wpa_inherent_exec_be_t1950, war_powers_allocation__inherent_executive_reading, base_extractiveness, 1950, 0.44).
narrative_ontology:measurement_basis(wpa_inherent_exec_be_t1950, observed).
narrative_ontology:measurement(wpa_inherent_exec_be_t1964, war_powers_allocation__inherent_executive_reading, base_extractiveness, 1964, 0.53).
narrative_ontology:measurement_basis(wpa_inherent_exec_be_t1964, observed).
narrative_ontology:measurement(wpa_inherent_exec_be_t1973, war_powers_allocation__inherent_executive_reading, base_extractiveness, 1973, 0.57).
narrative_ontology:measurement_basis(wpa_inherent_exec_be_t1973, observed).
narrative_ontology:measurement(wpa_inherent_exec_be_t1983, war_powers_allocation__inherent_executive_reading, base_extractiveness, 1983, 0.61).
narrative_ontology:measurement_basis(wpa_inherent_exec_be_t1983, observed).
narrative_ontology:measurement(wpa_inherent_exec_be_t1991, war_powers_allocation__inherent_executive_reading, base_extractiveness, 1991, 0.59).
narrative_ontology:measurement_basis(wpa_inherent_exec_be_t1991, observed).
narrative_ontology:measurement(wpa_inherent_exec_be_t2001, war_powers_allocation__inherent_executive_reading, base_extractiveness, 2001, 0.66).
narrative_ontology:measurement_basis(wpa_inherent_exec_be_t2001, observed).
narrative_ontology:measurement(wpa_inherent_exec_be_t2011, war_powers_allocation__inherent_executive_reading, base_extractiveness, 2011, 0.71).
narrative_ontology:measurement_basis(wpa_inherent_exec_be_t2011, observed).
narrative_ontology:measurement(wpa_inherent_exec_be_t2025, war_powers_allocation__inherent_executive_reading, base_extractiveness, 2025, 0.68).
narrative_ontology:measurement_basis(wpa_inherent_exec_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(wpa_inherent_exec_su_t1950, war_powers_allocation__inherent_executive_reading, suppression_requirement, 1950, 0.38).
narrative_ontology:measurement_basis(wpa_inherent_exec_su_t1950, observed).
narrative_ontology:measurement(wpa_inherent_exec_su_t1964, war_powers_allocation__inherent_executive_reading, suppression_requirement, 1964, 0.44).
narrative_ontology:measurement_basis(wpa_inherent_exec_su_t1964, observed).
narrative_ontology:measurement(wpa_inherent_exec_su_t1973, war_powers_allocation__inherent_executive_reading, suppression_requirement, 1973, 0.56).
narrative_ontology:measurement_basis(wpa_inherent_exec_su_t1973, observed).
narrative_ontology:measurement(wpa_inherent_exec_su_t1983, war_powers_allocation__inherent_executive_reading, suppression_requirement, 1983, 0.53).
narrative_ontology:measurement_basis(wpa_inherent_exec_su_t1983, observed).
narrative_ontology:measurement(wpa_inherent_exec_su_t1991, war_powers_allocation__inherent_executive_reading, suppression_requirement, 1991, 0.47).
narrative_ontology:measurement_basis(wpa_inherent_exec_su_t1991, observed).
narrative_ontology:measurement(wpa_inherent_exec_su_t2001, war_powers_allocation__inherent_executive_reading, suppression_requirement, 2001, 0.56).
narrative_ontology:measurement_basis(wpa_inherent_exec_su_t2001, observed).
narrative_ontology:measurement(wpa_inherent_exec_su_t2011, war_powers_allocation__inherent_executive_reading, suppression_requirement, 2011, 0.58).
narrative_ontology:measurement_basis(wpa_inherent_exec_su_t2011, observed).
narrative_ontology:measurement(wpa_inherent_exec_su_t2025, war_powers_allocation__inherent_executive_reading, suppression_requirement, 2025, 0.52).
narrative_ontology:measurement_basis(wpa_inherent_exec_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__inherent_executive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, congressional_primacy_reading).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, functional_accommodation_reading).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, war_powers_resolution_1973).

% DUAL FORMULATION NOTE:
% Member of the war_powers_allocation constraint family: the colloquial label 'war powers' decomposes into three rival readings of one fixed-text kernel, each with its own epsilon, beneficiary/victim structure, and classification. This (inherent-executive) member carries the highest extraction because it strips the legislative constraint entirely; the congressional_primacy member inverts the beneficiary/victim sets; the functional_accommodation member partitions them by operational context. The fixed text is upstream of all three; this reading structurally pressures the others by converting each unauthorized operation into precedent their frameworks must absorb.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
