% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__congressional_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_powers_allocation__congressional_primacy_reading, []).

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
 *   constraint_id: war_powers_allocation__congressional_primacy_reading
 *   human_readable: War Powers Allocation — Congressional Primacy Reading (Prior Authorization as Constitutional Necessity)
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the war_powers_allocation kernel:
 *   the congressional primacy reading, which holds that military force beyond
 *   immediate defense requires explicit prior congressional authorization as
 *   a constitutional necessity. Per the kernel-reading epsilon-referent rule,
 *   epsilon is authored for the STANDING ARRANGEMENT under contest — the
 *   operative allocation in which the executive initiates force and Congress
 *   ratifies or acquiesces afterward — assessed by this reading's own lights.
 *   Measured against the ratified allocation (Article I initiation vested in
 *   Congress; Article II confined to conducting authorized war and repelling
 *   sudden attacks), the standing arrangement shows five decades of
 *   accumulated transfer: hundreds of force uses initiated without prior
 *   authorization, a declare-war clause unused since 1942, and post-hoc
 *   ratifications that convert each exercise into precedent. The claim/metric
 *   gap is deliberate and independent: the reading CLAIMS the authorization
 *   requirement is a binding constitutional necessity, while the authored
 *   METRICS describe the standing arrangement's actual operation — the
 *   divergence between the claim and the practice is precisely what this
 *   reading exists to measure. Constraint family: this file is one of three
 *   readings of the same kernel. ASSUMPTION: sibling constraint_ids are
 *   inferred from the kernel naming pattern as
 *   war_powers_allocation__inherent_executive_reading and
 *   war_powers_allocation__functional_accommodation_reading. The epsilon
 *   values differ across the family because each reading measures the same
 *   standing practice against a different baseline: under the
 *   inherent-executive reading the same practice reads as legitimate exercise
 *   (epsilon collapses toward the beneficiary pole and the victim set
 *   relocates to congressional overreach); under the functional-accommodation
 *   reading epsilon is moderate and context-indexed; under this reading
 *   epsilon is high because the measure is the ratified allocation itself.
 *
 * KEY AGENTS:
 *   - executive_branch: Primary beneficiary and agenda-setter (institutional/arbitrage) — captures the war-initiation decision and writes the doctrine under which it operates
 *   - national_security_bureaucracy: Secondary beneficiary (institutional/constrained) — budgets, force structure, and mission scope grow with executive-led campaigning
 *   - congress_as_institution: Primary target (institutional/constrained) — formal holder of the initiation power, reduced to post-hoc ratification and symbolic condition-setting
 *   - deliberative_public: Target (moderate/constrained) — denied pre-commitment voice on war; episodic electoral leverage poorly coupled to the decision
 *   - service_members: Target (moderate/constrained) — bear deployment and casualty costs of campaigns never authorized
 *   - federal_courts: Enforcement participant (institutional/arbitrage) — abstention doctrine quietly sustains the allocation as practiced
 *   - populations_of_host_nations: Excluded (powerless/trapped) — bear the gravest physical costs with no seat in any deliberation
 *   - constitutional_law_scholars: Analytical observer (analytical/analytical) — audit the gap between text and practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__congressional_primacy_reading, 0.86).
domain_priors:suppression_score(war_powers_allocation__congressional_primacy_reading, 0.78).
domain_priors:theater_ratio(war_powers_allocation__congressional_primacy_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, extractiveness, 0.86).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__congressional_primacy_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__congressional_primacy_reading, "War Powers Allocation — Congressional Primacy Reading (Prior Authorization as Constitutional Necessity)").
narrative_ontology:topic_domain(war_powers_allocation__congressional_primacy_reading, "constitutional/political").

domain_priors:requires_active_enforcement(war_powers_allocation__congressional_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__congressional_primacy_reading, '5a8f1bf1-df57-4088-8808-2cd7cbd9a9b7').
narrative_ontology:cs_kernel_codification('5a8f1bf1-df57-4088-8808-2cd7cbd9a9b7', fixed_text).
narrative_ontology:cs_authority_grounding('5a8f1bf1-df57-4088-8808-2cd7cbd9a9b7', practice).
narrative_ontology:cs_interpretation_layer_present('5a8f1bf1-df57-4088-8808-2cd7cbd9a9b7').
narrative_ontology:cs_reading_relation('5a8f1bf1-df57-4088-8808-2cd7cbd9a9b7', war_powers_allocation__inherent_executive_reading, forecloses).
narrative_ontology:cs_reading_relation('5a8f1bf1-df57-4088-8808-2cd7cbd9a9b7', war_powers_allocation__functional_accommodation_reading, forecloses).
narrative_ontology:cs_axiom('5a8f1bf1-df57-4088-8808-2cd7cbd9a9b7', foundational, exclusive_congressional_war_initiation_authority).
narrative_ontology:cs_axiom_status(exclusive_congressional_war_initiation_authority, holdable).
narrative_ontology:cs_axiom_grounding('5a8f1bf1-df57-4088-8808-2cd7cbd9a9b7', exclusive_congressional_war_initiation_authority, conventional).
narrative_ontology:cs_axiom('5a8f1bf1-df57-4088-8808-2cd7cbd9a9b7', secondary, deliberative_consent_precondition_of_war_legitimacy).
narrative_ontology:cs_axiom_status(deliberative_consent_precondition_of_war_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('5a8f1bf1-df57-4088-8808-2cd7cbd9a9b7', deliberative_consent_precondition_of_war_legitimacy, deontological).
narrative_ontology:cs_reference_frame('5a8f1bf1-df57-4088-8808-2cd7cbd9a9b7', article_i_declared_war_design).
narrative_ontology:cs_drift_state('5a8f1bf1-df57-4088-8808-2cd7cbd9a9b7', contemporary_national_security_state, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('5a8f1bf1-df57-4088-8808-2cd7cbd9a9b7', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__congressional_primacy_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__congressional_primacy_reading, executive_branch).
narrative_ontology:constraint_beneficiary(war_powers_allocation__congressional_primacy_reading, national_security_bureaucracy).
narrative_ontology:constraint_victim(war_powers_allocation__congressional_primacy_reading, congress_as_institution).
narrative_ontology:constraint_victim(war_powers_allocation__congressional_primacy_reading, deliberative_public).
narrative_ontology:constraint_victim(war_powers_allocation__congressional_primacy_reading, service_members).
narrative_ontology:constraint_vindicates(war_powers_allocation__congressional_primacy_reading, exclusive_congressional_declaration_power).
narrative_ontology:constraint_vindicates(war_powers_allocation__congressional_primacy_reading, separation_of_powers_accountability).
narrative_ontology:constraint_vindicates(war_powers_allocation__congressional_primacy_reading, republican_deliberation_before_war).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Initiates and conducts military operations worldwide. Decides when force is used, issues the legal opinions that declare its own authority sufficient, notifies Congress after operations begin or not at all, and treats statutory gates as consultative. Selects among competing legal rationales — statute, treaty, inherent constitutional power — as each operation requires. It faces no exit question: it wrote the operating doctrine and can rewrite it.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, executive_branch, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(war_powers_allocation__congressional_primacy_reading, executive_branch, beneficiary).

% Plans, staffs, and executes the operations the initiation decision produces. Departmental budgets, force structure, and mission scope expand with each executive-led campaign; career advancement runs through operational relevance. Its access runs through the White House rather than Capitol Hill, and its planning horizons assume executive freedom of action.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, national_security_bureaucracy, beneficiary,
    institutional, generational, constrained, global).

% Holds the constitutional power to declare war and controls appropriations. Members debate authorizations after deployments are underway, pass notification requirements that are honored as paperwork, and periodically attach funding conditions that are waived or signed around. Defunding forces already in the field carries electoral costs few members will pay; litigation is discouraged by the courts' own abstention. The institution's tools remain formally intact and practically blunted, and each member's electoral horizon is shorter than any enforcement campaign would require.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, congress_as_institution, payer,
    institutional, biographical, constrained, national).

% Votes for the officials who decide war but has no direct gate on the decision. Polling repeatedly shows majorities opposing specific campaigns before and during them while representatives fund them regardless; public attention concentrates episodically around escalations and dissipates between them. There is no exit from the polity whose armed forces act in its name.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, deliberative_public, payer,
    moderate, biographical, constrained, national).

% Execute the deployments the initiation decision produces, under contractual and disciplinary obligations that permit no refusal of lawful orders. They bear casualty risk and repeated extended separations in campaigns that were never voted on, and their professional institutions reward participation rather than objection.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, service_members, payer,
    moderate, biographical, constrained, global).

% Decide whether to reach war-powers disputes at all. Across five decades the doctrinal answer has been abstention — political-question dismissals, standing denials, ripeness rulings — leaving the allocation as practiced legally unexamined. Each abstention is discretionary and low-cost to the court; the docket-management choice is the quietest load-bearing element holding the current allocation in place.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, federal_courts, agenda_setter,
    institutional, generational, arbitrage, national).

% Live where the force lands. They are targeted, displaced, or left to govern the aftermath of operations decided entirely in Washington; no element of the authorization process — congressional or executive — includes a seat for them, and no channel exists through which their objection could enter the deliberation.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, populations_of_host_nations, excluded,
    powerless, biographical, trapped, regional).

% Publish the running audit of the allocation: treatises, law-review symposia, congressional testimony. The field broadly reads the constitutional text as assigning war initiation to Congress even where individual scholars defend executive practice on functional grounds. They observe with no lever on the outcome beyond persuasion.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, constitutional_law_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_powers_allocation__congressional_primacy_reading, executive_branch).
narrative_ontology:fixing_cost_class(war_powers_allocation__congressional_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates the decision to initiate and conduct military operations in a single office capable of acting at operational speed, with unified command, secrecy, and continuity — while leaving Congress statutory gates (appropriations, authorization statutes, notification requirements) that operate alongside executive initiative.
% TRANSFER_FUNCTION: Moves the war-initiation decision — and the costs attached to exercising it (casualties, expenditure, legal exposure, precedent) — from Congress and the voting public to the presidency; post-hoc appropriations and authorizations convert each completed exercise into precedent that lowers the cost of the next unilateral use.
% ABSENT_VOICES: Populations of the countries where force is employed have no seat anywhere in the process; rank-and-file service members execute commitments they had no voice in setting; the anti-war electoral majorities that polling periodically registers are represented only episodically inside Congress; and the federal courts decline the venue entirely, so no adjudicatory forum exists in which the objection could be heard.
% DISAPPEARANCE_RATIONALE: If the standing allocation vanished overnight — if prior authorization were suddenly strictly required for every use of force beyond repelling an actual attack — overseas operations would pause pending votes, the authorization calendar would become the pacing item for military planning, alliance commitments would be renegotiated around congressional timelines, and executive operational doctrine would be rewritten. Force-projection schedules, basing agreements, and the national-security budget cycle all presuppose the current allocation.
% FOUNDING_PROBLEM: The constitutional design answered the founding problem of concentrated war prerogative: the framers' anti-royalist settlement moved the power to begin war from the executive to the legislature — Federalist 69 contrasts the president's conditional war power with the British king's plenary one — reserving to the president alone the repelling of sudden attacks. The standing arrangement is the modern settlement of the collision between that design and a national-security state operating at global scale and operational tempo.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties: the Constitutional Convention records and the Pennsylvania minority's ratification dissent attest the founding fear of executive war prerogative; Hamilton's Federalist 69 concedes the president's power is 'much inferior' to the king's; the 1973 War Powers Resolution passed both chambers overwhelmingly and over a presidential veto, attesting Congress's continuing claim; and no court has ever endorsed the executive's contrary reading — the principal contrary attestations (Office of Legal Counsel opinions) issue from inside the benefiting parties themselves.
narrative_ontology:disappearance_verdict(war_powers_allocation__congressional_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(war_powers_allocation__congressional_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__congressional_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(war_powers_allocation__congressional_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(war_powers_allocation__congressional_primacy_reading, 0.86, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_powers_allocation__congressional_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_powers_allocation__congressional_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_powers_allocation__congressional_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.86 at interval end) because the transfer is cumulative and compounding: each unauthorized use is a withdrawal from the congressional account, and post-hoc ratification converts the withdrawal into precedent that lowers the cost of the next one. Suppression is high (0.78) and operates in two registered senses: the standing arrangement survives only by actively suppressing enforcement of the authorization requirement (judicial abstention, practical nullification of the War Powers Resolution's 60-day clock after the 2011 Libya memoranda, funding fait accompli once troops are committed, and rally-effect punishment of dissent), while the reading's own framework would categorically close the executive's inherent-authority exit — the omega suppression_mechanism_ambiguity tracks which mechanism dominates. Theater ratio (0.68) reflects the growing share of compliance activity that is performative: notification letters filed as operations begin, hearings that ratify rather than constrain, symbolic repeal votes on authorizations still stretched to cover new campaigns. Accessibility collapse is moderate (0.60): once the equilibrium is understood, the obvious remedies (suit, defunding, override) are mostly closed, but Congress retains formally intact tools it declines to pay for using. Resistance is moderate (0.55): the 1973 Resolution, recurring funding-condition fights, and periodic AUMF-repeal efforts are persistent, organized, and consistently unsuccessful. The measurement series are monotonic rather than cyclical by design: rally-effect spikes around each escalation ride on a secular ratchet, and the series records the ratchet; the oscillation is documented qualitatively in commentary rather than imposed on the grid. All three tracked metrics share one eight-point time grid (1973-2026) so no metric row borrows an end-state value from another.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute differently from identical structural data. From the executive seat the arrangement is a functioning allocation validated by two centuries of practice, with the flexibility the modern threat environment demands; from the congressional and public seats the same structure operates as a sustained uncompensated taking of a power the text vests elsewhere, with ratification sought only after the fact. Among four institutional seats at nominally equal power, exit options differentiate the experience: the executive and the courts hold arbitrage-grade exit (each selects among legal theories, or among justiciability outcomes, opportunistically and at low cost), while Congress and the security bureaucracy are constrained — Congress's tools are real but each carries prohibitive political cost, and the bureaucracy's access runs entirely through the seat capturing the decision. The courts' internal view of abstention is institutional humility; its external effect is enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive the derivation: executive_branch and national_security_bureaucracy sit near the beneficiary pole (the arrangement subsidizes both — decision-right capture for one, budget and mission growth for the other), with the executive nearest zero given its arbitrage-grade exit from any doctrinal constraint. congress_as_institution, deliberative_public, and service_members sit near the target pole: each bears the transfer (lost power, lost voice, borne risk) with constrained exit. federal_courts take a mid-low position through their enforcement role — abstention collects institutional quiet for the court while sustaining the arrangement. populations_of_host_nations are excluded rather than seated: they bear the gravest costs but stand outside the conversation entirely, which is recorded as absence (Q4), not as a directionality value. No directionality_overrides are authored: the override mechanism is keyed by power atom, and this story contains FOUR institutional seats with sharply different directionalities — any power-atom-level override would smear them together. The structural declarations (roles, exit options, situations) carry the differentiation instead.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing a single magistrate from plunging the nation into war — is live, not dead: the standing practice is the problem recurring, not a solution outliving its purpose. No mandatrophy is declared, and the R5 mismatch consumer correctly finds no zombie flag (status=live x verdict=world_rearranges). The classification guards both error directions: reading the standing arrangement as pure extraction would erase the genuine defense-coordination core that even this reading concedes (immediate defense, unified command, operational secrecy — the reason the founders themselves carved out sudden attacks); reading it as pure coordination would erase the asymmetric transfer the same record documents. The tangled-rope structure holds both facts: a real coordination function, an asymmetric extraction riding on it, and active enforcement required to keep the extraction in place.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_delta,
    'This constraint is one reading of the war_powers_allocation kernel (congressional_primacy_reading). What structurally changes under the sibling readings, and where exactly is the disagreement located?',
    'Compare the three stories'' victim sets and epsilon values directly: under inherent_executive_reading the victim set relocates to Congress-as-overreacher and epsilon collapses toward the beneficiary pole; under functional_accommodation_reading epsilon becomes context-indexed and the victim set thins to prolonged-campaign cases. The disagreement is located in one structural element: whether Article II grants an independent war-initiation power or merely the conduct of wars Article I authorizes.',
    'Classification is reading-relative by construction: the same standing practice computes as coordinated-and-extractive under this reading, as legitimate exercise under the inherent-executive sibling, and as contextually mixed under the accommodation sibling. Cross-reading comparison is valid only at the family level, never within a single story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_delta, conceptual, 'Committer-frame indexicality: which reading of the war-powers kernel is instantiated determines the victim set and epsilon.').

omega_variable(
    ratification_under_duress,
    'When Congress post-hoc ratifies an unauthorized use of force (funding after deployment, an authorization sought mid-campaign), does the ratification reverse the extraction (retroactive consent legitimates) or deepen it (fait accompli converts usurpation into precedent)?',
    'Longitudinal comparison of campaigns ratified ex ante, ratified post hoc, and never ratified: track whether subsequent presidential assertions of authority expand faster after post-hoc ratifications than after ex ante ones, controlling for threat environment.',
    'If ratification legitimates, measured extraction drops sharply for ratified episodes and the arrangement reads as bargaining; if ratification-under-duress compounds, each episode strengthens the ratchet and the high-extraction trajectory is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ratification_under_duress, empirical, 'Whether post-hoc congressional ratification offsets or compounds executive capture of the initiation power.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the authorization requirement''s non-enforcement structural (court-made justiciability barriers, funding lock-in once troops deploy) or internalized (members of Congress have absorbed the norm that war is the president''s business — the water''s-edge convention)?',
    'Counterfactual probe: if a justiciability barrier fell — a court accepted a War Powers Resolution enforcement suit — would Congress litigate and press the claim, or decline to sue? Track member statements and caucus behavior in any such window.',
    'If suppression is internalized, removing the structural barriers changes little and the enforcement deficit persists with the barriers gone; if structural, a single justiciability shift could reactivate the congressional tools the metrics currently score as blunted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of the authorization requirement''s enforcement.').

omega_variable(
    immediate_defense_boundary,
    'Where does ''immediate defense'' end — does repelling an imminent (not yet landed) attack fall inside the unilateral carve-out, and who certifies imminence?',
    'Conceptual analysis against the founding record (the ''repel sudden attacks'' language) plus examination of executive imminence certifications in actual operations; the certification question is empirically tractable whenever a strike is justified by imminence.',
    'A wide defense carve-out shrinks the extraction surface and moves computed extraction down; a narrow one (actual attack only) widens it. Executive self-certification of imminence collapses this reading toward the functional-accommodation sibling from the inside.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immediate_defense_boundary, conceptual, 'Boundary of the immediate-defense exception on which the entire extraction surface depends.').

omega_variable(
    congressional_enforcement_coalition_viability,
    'Can the fragmented congressional victims form a durable enforcement coalition — supermajority overrides, binding appropriations conditions, sustained litigation support — given that each member''s electoral incentive favors deferral once forces are engaged?',
    'Examine the few near-miss windows (post-Vietnam reform wave, post-Iraq funding-condition fights) for what distinguished coalitions that held from those that dissolved; test whether procedural reforms (bipartisan war-powers caucuses, automatic trigger provisions) change dissolution rates.',
    'A viable coalition would raise the resistance score materially and could shift the arrangement''s trajectory from ratchet toward renegotiation; persistent non-viability confirms the constrained-exit scoring and the high suppression reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(congressional_enforcement_coalition_viability, empirical, 'Coalition potential of the primary victim seat against individual electoral defection incentives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__congressional_primacy_reading, 1973, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war_powers_cong_primacy_tr_t1973, war_powers_allocation__congressional_primacy_reading, theater_ratio, 1973, 0.3).
narrative_ontology:measurement_basis(war_powers_cong_primacy_tr_t1973, observed).
narrative_ontology:measurement(war_powers_cong_primacy_tr_t1980, war_powers_allocation__congressional_primacy_reading, theater_ratio, 1980, 0.33).
narrative_ontology:measurement_basis(war_powers_cong_primacy_tr_t1980, observed).
narrative_ontology:measurement(war_powers_cong_primacy_tr_t1990, war_powers_allocation__congressional_primacy_reading, theater_ratio, 1990, 0.4).
narrative_ontology:measurement_basis(war_powers_cong_primacy_tr_t1990, observed).
narrative_ontology:measurement(war_powers_cong_primacy_tr_t2001, war_powers_allocation__congressional_primacy_reading, theater_ratio, 2001, 0.45).
narrative_ontology:measurement_basis(war_powers_cong_primacy_tr_t2001, observed).
narrative_ontology:measurement(war_powers_cong_primacy_tr_t2007, war_powers_allocation__congressional_primacy_reading, theater_ratio, 2007, 0.52).
narrative_ontology:measurement_basis(war_powers_cong_primacy_tr_t2007, observed).
narrative_ontology:measurement(war_powers_cong_primacy_tr_t2011, war_powers_allocation__congressional_primacy_reading, theater_ratio, 2011, 0.6).
narrative_ontology:measurement_basis(war_powers_cong_primacy_tr_t2011, observed).
narrative_ontology:measurement(war_powers_cong_primacy_tr_t2016, war_powers_allocation__congressional_primacy_reading, theater_ratio, 2016, 0.64).
narrative_ontology:measurement_basis(war_powers_cong_primacy_tr_t2016, observed).
narrative_ontology:measurement(war_powers_cong_primacy_tr_t2026, war_powers_allocation__congressional_primacy_reading, theater_ratio, 2026, 0.68).
narrative_ontology:measurement_basis(war_powers_cong_primacy_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(war_powers_cong_primacy_be_t1973, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 1973, 0.55).
narrative_ontology:measurement_basis(war_powers_cong_primacy_be_t1973, observed).
narrative_ontology:measurement(war_powers_cong_primacy_be_t1980, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 1980, 0.6).
narrative_ontology:measurement_basis(war_powers_cong_primacy_be_t1980, observed).
narrative_ontology:measurement(war_powers_cong_primacy_be_t1990, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 1990, 0.63).
narrative_ontology:measurement_basis(war_powers_cong_primacy_be_t1990, observed).
narrative_ontology:measurement(war_powers_cong_primacy_be_t2001, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 2001, 0.72).
narrative_ontology:measurement_basis(war_powers_cong_primacy_be_t2001, observed).
narrative_ontology:measurement(war_powers_cong_primacy_be_t2007, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 2007, 0.78).
narrative_ontology:measurement_basis(war_powers_cong_primacy_be_t2007, observed).
narrative_ontology:measurement(war_powers_cong_primacy_be_t2011, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 2011, 0.83).
narrative_ontology:measurement_basis(war_powers_cong_primacy_be_t2011, observed).
narrative_ontology:measurement(war_powers_cong_primacy_be_t2016, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 2016, 0.84).
narrative_ontology:measurement_basis(war_powers_cong_primacy_be_t2016, observed).
narrative_ontology:measurement(war_powers_cong_primacy_be_t2026, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 2026, 0.86).
narrative_ontology:measurement_basis(war_powers_cong_primacy_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(war_powers_cong_primacy_su_t1973, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 1973, 0.5).
narrative_ontology:measurement_basis(war_powers_cong_primacy_su_t1973, observed).
narrative_ontology:measurement(war_powers_cong_primacy_su_t1980, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement_basis(war_powers_cong_primacy_su_t1980, observed).
narrative_ontology:measurement(war_powers_cong_primacy_su_t1990, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 1990, 0.58).
narrative_ontology:measurement_basis(war_powers_cong_primacy_su_t1990, observed).
narrative_ontology:measurement(war_powers_cong_primacy_su_t2001, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 2001, 0.66).
narrative_ontology:measurement_basis(war_powers_cong_primacy_su_t2001, observed).
narrative_ontology:measurement(war_powers_cong_primacy_su_t2007, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 2007, 0.7).
narrative_ontology:measurement_basis(war_powers_cong_primacy_su_t2007, observed).
narrative_ontology:measurement(war_powers_cong_primacy_su_t2011, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 2011, 0.74).
narrative_ontology:measurement_basis(war_powers_cong_primacy_su_t2011, observed).
narrative_ontology:measurement(war_powers_cong_primacy_su_t2016, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 2016, 0.76).
narrative_ontology:measurement_basis(war_powers_cong_primacy_su_t2016, observed).
narrative_ontology:measurement(war_powers_cong_primacy_su_t2026, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 2026, 0.78).
narrative_ontology:measurement_basis(war_powers_cong_primacy_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__congressional_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(war_powers_allocation__congressional_primacy_reading, war_powers_allocation__inherent_executive_reading).
narrative_ontology:affects_constraint(war_powers_allocation__congressional_primacy_reading, war_powers_allocation__functional_accommodation_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'war powers' conflates three structurally distinct allocations of the same kernel. This file instantiates the congressional_primacy_reading (authorization as constitutional necessity; epsilon 0.86 against the standing arrangement). The sibling files instantiate the inherent_executive_reading (same practice reads as legitimate exercise; epsilon collapses toward the beneficiary pole; victim set relocates to congressional overreach) and the functional_accommodation_reading (epsilon moderate and context-indexed; victim set thins to prolonged-campaign cases). The upstream/downstream structure runs from this reading outward: the ratified-design baseline is the textual anchor both siblings argue from or against, and each congressional assertion of this reading raises the legitimacy cost the siblings must spend to hold their positions. Sibling constraint_ids are inferred from the kernel naming pattern; all three stories link one another via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
