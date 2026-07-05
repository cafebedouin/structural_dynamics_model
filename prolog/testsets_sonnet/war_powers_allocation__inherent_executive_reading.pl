% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__inherent_executive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Inherent Executive War Powers Reading
 *   domain: constitutional_law/separation_of_powers
 *
 * SUMMARY:
 *   This story instantiates the inherent-executive reading of the war-powers
 *   kernel: the commander-in-chief clause is read as granting the president
 *   self-executing authority to deploy force in defense of national interests
 *   without prior congressional authorization. Under this reading,
 *   congressional authorization is a political courtesy sought for domestic
 *   legitimacy, not a constitutional precondition, and subsequent
 *   appropriations function as de facto ratification. This is a distinct
 *   constraint from the congressional_primacy_reading (which treats
 *   authorization as constitutionally necessary) and the
 *   functional_accommodation_reading (which splits by operational context) —
 *   each reading has its own beneficiary/victim structure and its own
 *   epsilon, and should not be averaged together. Low suppression is authored
 *   deliberately: this reading does not need to actively coerce compliance
 *   because the political-question doctrine and standing barriers passively
 *   remove the judiciary as an enforcement threat, and Congress's own
 *   appropriations dependency does the remaining structural work without
 *   direct coercive action.
 *
 * KEY AGENTS:
 *   - executive_branch: primary agenda-setter and beneficiary, institutional/arbitrage
 *   - congressional_war_power: primary structural victim under this reading, institutional/constrained
 *   - populations_in_theaters_of_unauthorized_deployment: powerless/trapped, bears the sharpest immediate cost
 *   - federal_judiciary: excluded by its own doctrinal choice, institutional/analytical
 *   - constitutional_law_scholars: analytical observer documenting the drift
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__inherent_executive_reading, 0.61).
domain_priors:suppression_score(war_powers_allocation__inherent_executive_reading, 0.28).
domain_priors:theater_ratio(war_powers_allocation__inherent_executive_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__inherent_executive_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__inherent_executive_reading, "Inherent Executive War Powers Reading").
narrative_ontology:topic_domain(war_powers_allocation__inherent_executive_reading, "constitutional_law/separation_of_powers").

domain_priors:requires_active_enforcement(war_powers_allocation__inherent_executive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__inherent_executive_reading, '157ffd0d-095e-44a5-b5c4-7201163eb2b7').
narrative_ontology:cs_kernel_codification('157ffd0d-095e-44a5-b5c4-7201163eb2b7', fixed_text).
narrative_ontology:cs_authority_grounding('157ffd0d-095e-44a5-b5c4-7201163eb2b7', lineage).
narrative_ontology:cs_interpretation_layer_present('157ffd0d-095e-44a5-b5c4-7201163eb2b7').
narrative_ontology:cs_reading_relation('157ffd0d-095e-44a5-b5c4-7201163eb2b7', war_powers_allocation__congressional_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('157ffd0d-095e-44a5-b5c4-7201163eb2b7', war_powers_allocation__functional_accommodation_reading, influences).
narrative_ontology:cs_axiom('157ffd0d-095e-44a5-b5c4-7201163eb2b7', foundational, commander_in_chief_grant_is_self_executing).
narrative_ontology:cs_axiom_status(commander_in_chief_grant_is_self_executing, holdable).
narrative_ontology:cs_axiom_grounding('157ffd0d-095e-44a5-b5c4-7201163eb2b7', commander_in_chief_grant_is_self_executing, conventional).
narrative_ontology:cs_axiom('157ffd0d-095e-44a5-b5c4-7201163eb2b7', secondary, post_hoc_appropriation_constitutes_ratification).
narrative_ontology:cs_axiom_status(post_hoc_appropriation_constitutes_ratification, holdable).
narrative_ontology:cs_axiom_grounding('157ffd0d-095e-44a5-b5c4-7201163eb2b7', post_hoc_appropriation_constitutes_ratification, instrumental).
narrative_ontology:cs_reference_frame('157ffd0d-095e-44a5-b5c4-7201163eb2b7', federalist_69_limited_command_authority).
narrative_ontology:cs_drift_state('157ffd0d-095e-44a5-b5c4-7201163eb2b7', post_cold_war_unilateral_deployment_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('157ffd0d-095e-44a5-b5c4-7201163eb2b7', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__inherent_executive_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__inherent_executive_reading, executive_branch).
narrative_ontology:constraint_beneficiary(war_powers_allocation__inherent_executive_reading, standing_military_apparatus).
narrative_ontology:constraint_beneficiary(war_powers_allocation__inherent_executive_reading, national_security_bureaucracy).
narrative_ontology:constraint_victim(war_powers_allocation__inherent_executive_reading, congressional_war_power).
narrative_ontology:constraint_victim(war_powers_allocation__inherent_executive_reading, domestic_constituencies_bearing_conflict_costs).
narrative_ontology:constraint_victim(war_powers_allocation__inherent_executive_reading, populations_in_theaters_of_unauthorized_deployment).
narrative_ontology:constraint_vindicates(war_powers_allocation__inherent_executive_reading, unitary_executive_doctrine).
narrative_ontology:constraint_vindicates(war_powers_allocation__inherent_executive_reading, commander_in_chief_plenary_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Article II commander-in-chief power as inherent and self-executing authority to deploy force without prior congressional authorization when the executive judges national interests are implicated. Sets precedent through repeated unilateral action, builds legal opinions (OLC memos) that entrench the reading, and treats subsequent congressional appropriations or acquiescence as retroactive validation rather than required consent.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, executive_branch, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(war_powers_allocation__inherent_executive_reading, executive_branch, beneficiary).

% Gains operational flexibility, faster deployment timelines, and reduced legislative friction when the executive can order force commitments without waiting on authorization votes. Benefits institutionally from the doctrine's persistence regardless of any specific conflict's merits.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, standing_military_apparatus, beneficiary,
    institutional, generational, arbitrage, global).

% Career officials in defense and intelligence agencies benefit from a decision-making structure concentrated in the executive, which shortens approval chains and insulates operational choices from public legislative debate.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, national_security_bureaucracy, beneficiary,
    institutional, generational, arbitrage, global).

% Holds the constitutional textual grant (Article I, Section 8) to declare war and raise armies, but under this reading that grant is treated as a courtesy the executive may seek after the fact rather than a precondition. Congress retains the power of the purse in theory, but cutting funding mid-deployment is politically costly once troops are committed, so appropriations function as de facto ratification rather than independent check. Legal challenge is largely foreclosed by political-question doctrine and standing barriers.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, congressional_war_power, payer,
    institutional, generational, constrained, national).

% Taxpayers, military families, and voters bear the fiscal and human costs of deployments decided without the deliberative friction of authorization debate. Their only leverage is electoral, exercised long after deployment decisions and irreversible commitments have already been made.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, domestic_constituencies_bearing_conflict_costs, payer,
    organized, biographical, trapped, national).

% Civilian populations in states where force is deployed under this doctrine bear direct physical and infrastructural consequences of decisions made through an internal U.S. executive process in which they have no voice and no standing in any U.S. forum.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, populations_in_theaters_of_unauthorized_deployment, payer,
    powerless, immediate, trapped, regional).

% Would be positioned to adjudicate the constitutional boundary but largely declines via political-question doctrine and standing dismissals, leaving the inter-branch dispute unresolved by the institution best positioned to resolve it.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, federal_judiciary, excluded,
    institutional, civilizational, analytical, national).

% Document the drift from declared-war practice toward unilateral deployment, tracking War Powers Resolution non-compliance patterns and the growth of OLC opinions supporting expansive executive authority.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, constitutional_law_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_powers_allocation__inherent_executive_reading, executive_branch).
narrative_ontology:fixing_cost_class(war_powers_allocation__inherent_executive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables rapid, decisive military response to perceived threats without the delay of legislative deliberation — a genuine operational advantage in fast-moving crises where authorization votes would be too slow to matter.
% TRANSFER_FUNCTION: Moves the practical authority to initiate armed conflict from the legislature (constitutionally textual holder of the war-declaring power) to the executive, and moves the costs of that initiation from the decision-maker onto taxpayers, deployed families, and foreign civilian populations who have no vote in the decision.
% ABSENT_VOICES: Foreign civilian populations in deployment theaters have no standing anywhere in the U.S. constitutional process that produces the decision to deploy against them. Rank-and-file legislators who might resist are structurally disadvantaged by the political cost of appearing to withhold support once troops are already committed.
% DISAPPEARANCE_RATIONALE: If the inherent-executive reading were abandoned and pre-authorization became binding, deployment timelines would slow substantially, the War Powers Resolution's 60-90 day clock would become an operative constraint rather than a routinely ignored formality, and a significant category of post-1945 U.S. military interventions would not have proceeded as they did.
% FOUNDING_PROBLEM: The commander-in-chief clause was written to ensure unified, decisive command of forces already committed to war and to allow response to sudden attack before Congress could convene — an operational-command problem, not a war-initiation problem.
% FOUNDING_PROBLEM_CORROBORATION: The executive branch and its OLC attest the inherent-authority reading as continuous constitutional practice. Constitutional law scholars operating outside the executive branch, and dissenting members of Congress across party lines in war-powers debates since Vietnam, corroborate that the founding problem was narrow operational command in immediate defense, and that its extension to discretionary force projection for broader 'national interests' is a later expansion not present in the founding design — no corroboration for the expanded reading exists from a source outside the branch that benefits from it.
narrative_ontology:disappearance_verdict(war_powers_allocation__inherent_executive_reading, world_rearranges).
narrative_ontology:founding_problem_status(war_powers_allocation__inherent_executive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__inherent_executive_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(war_powers_allocation__inherent_executive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(war_powers_allocation__inherent_executive_reading, 0.61, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.61) reflects substantial but not maximal transfer: the executive's expanded authority reallocates decision power and its costs onto Congress and downstream populations, but the transfer is partially masked by post-hoc appropriations that create an appearance of shared ownership. Suppression is authored low (0.28) because this reading does not require heavy active coercion to persist — it operates through doctrinal avoidance (political question doctrine) and structural incentive (sunk-cost appropriations pressure) rather than direct suppression of dissent. Theater ratio rises over the interval (0.20 to 0.44) as the gap between the War Powers Resolution's textual reporting requirements and actual executive compliance widens — the reporting mechanism increasingly performs consultation without constraining action. Accessibility collapse is moderate (0.35): the congressional_primacy_reading and functional_accommodation_reading remain live alternative framings actively argued by scholars and some legislators, so alternatives have not fully collapsed even as practice entrenches the inherent-executive pattern.
 *
 * PERSPECTIVAL GAP:
 *   From the executive's seat, this is continuous, legitimate exercise of an inherent constitutional grant — a rope solving a genuine operational-speed problem. From Congress's seat, particularly post-Vietnam and post-War Powers Resolution, the same structure operates as a tangled rope at best: real coordination benefit in true emergencies, wrapped around routine extraction of the war-declaring power in non-emergency cases. The engine computes these as different seat-level classifications from the same structural data; this story does not adjudicate between them, it authors the inherent-executive seat's own account plus the payer seats' burdens.
 *
 * DIRECTIONALITY LOGIC:
 *   The executive branch, standing military apparatus, and national security bureaucracy are declared beneficiaries because the doctrine concentrates decision authority and operational flexibility with them, with essentially arbitrage-grade exit (they set the rules they operate under). Congressional war power is the primary institutional victim: it holds the textual grant but the reading treats that grant as advisory, and Congress's own tools (funding cutoff) carry such high political cost mid-deployment that they function as constrained rather than free exit. Populations in deployment theaters and domestic constituencies bearing conflict costs are trapped payers with no meaningful exit at all — they cannot leave the jurisdiction of a decision made without their input.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unified command for immediate defensive response before Congress can convene) is largely dead as a live justification for the doctrine's current scope — most contemporary invocations involve discretionary force projection over weeks or months, not sudden-attack response. The founding_problem_status is authored contested rather than flatly dead because the executive branch continues to invoke it sincerely for a genuine subset of cases (imminent threat response), while the doctrine's operative reach in practice has expanded far past that subset. This is exactly the kind of drift the mandatrophy classification exists to catch: a genuine, narrow coordination function (rapid response to sudden attack) has been used to justify a much broader unilateral authority (discretionary force projection for undefined 'national interests') that the founding design did not clearly contemplate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_locus,
    'This constraint is one reading (inherent_executive_reading) of the contested war_powers_allocation kernel. The sibling readings — congressional_primacy_reading and functional_accommodation_reading — locate the disagreement differently: congressional_primacy_reading holds that any force deployment beyond immediate defense requires explicit prior authorization as constitutional necessity (the disagreement is located at whether Article I''s war-declaring clause is a precondition or a formality); functional_accommodation_reading holds that the allocation is context-dependent (the disagreement is located at the threshold distinguishing ''imminent threat response'' from ''prolonged campaign''). This reading holds that Article II''s commander-in-chief grant is self-executing and inherent (the disagreement is located at whether the clause grants substantive war-initiation power or only operational-command power once force is already committed).',
    'A definitive Supreme Court ruling on standing and the political-question doctrine that reached the merits of a war-powers dispute would resolve which reading the judiciary treats as controlling; short of that, the disagreement is resolved politically case-by-case through appropriations behavior and never doctrinally.',
    'If the congressional_primacy_reading were judicially adopted, this constraint''s beneficiary structure would invert: the executive branch would become the constrained party and congressional_war_power would become the beneficiary of a restored precondition requirement. If functional_accommodation_reading prevailed, this constraint would survive only for the imminent-threat subset and would be extinguished for the discretionary-deployment subset that currently drives its extractiveness score.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Where the three kernel readings locate the core disagreement, and what judicial resolution would change.').

omega_variable(
    appropriations_as_ratification_validity,
    'Does congressional appropriation of funds for an already-deployed force constitute genuine constitutional ratification of the deployment decision, or is it merely sunk-cost-driven acquiescence that should carry no constitutional weight?',
    'Comparative analysis of appropriations votes where genuine debate and rejection were politically feasible versus votes taken under sunk-cost pressure (troops already in theater) — a pattern of near-unanimous appropriation regardless of the deployment''s contested legality would support the acquiescence reading over the ratification reading.',
    'If appropriations are acquiescence rather than ratification, the extractiveness of this reading is understated — the appearance of congressional buy-in is itself part of the extraction mechanism (a manufactured legitimacy signal), not independent validation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appropriations_as_ratification_validity, empirical, 'Whether post-hoc funding functions as real ratification or as a coerced legitimacy signal.').

omega_variable(
    natural_vs_constructed_executive_power,
    'Is the inherent commander-in-chief authority a genuine feature of the constitutional design (a fixed allocation the framers intended), or a constructed doctrine that has expanded through accumulated executive practice and judicial non-review, benefiting the executive branch and national security apparatus specifically?',
    'Originalist historical analysis of founding-era debates (Federalist 69, ratification convention records) compared against the doctrine''s actual twentieth and twenty-first century scope of application.',
    'If constructed rather than original, the doctrine''s persistence is better explained by institutional benefit-capture than by constitutional necessity, which would support reclassifying the reading''s self-presentation as closer to a false-summit dynamic than a stable constitutional settlement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_vs_constructed_executive_power, conceptual, 'Whether the inherent-executive doctrine reflects original design or accumulated institutional practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__inherent_executive_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t1945, war_powers_allocation__inherent_executive_reading, theater_ratio, 1945, 0.2).
narrative_ontology:measurement(war__tr_t1960, war_powers_allocation__inherent_executive_reading, theater_ratio, 1960, 0.25).
narrative_ontology:measurement(war__tr_t1975, war_powers_allocation__inherent_executive_reading, theater_ratio, 1975, 0.32).
narrative_ontology:measurement(war__tr_t1990, war_powers_allocation__inherent_executive_reading, theater_ratio, 1990, 0.36).
narrative_ontology:measurement(war__tr_t2005, war_powers_allocation__inherent_executive_reading, theater_ratio, 2005, 0.4).
narrative_ontology:measurement(war__tr_t2025, war_powers_allocation__inherent_executive_reading, theater_ratio, 2025, 0.44).

% Extraction over time
narrative_ontology:measurement(war__be_t1945, war_powers_allocation__inherent_executive_reading, base_extractiveness, 1945, 0.32).
narrative_ontology:measurement(war__be_t1960, war_powers_allocation__inherent_executive_reading, base_extractiveness, 1960, 0.4).
narrative_ontology:measurement(war__be_t1975, war_powers_allocation__inherent_executive_reading, base_extractiveness, 1975, 0.48).
narrative_ontology:measurement(war__be_t1990, war_powers_allocation__inherent_executive_reading, base_extractiveness, 1990, 0.52).
narrative_ontology:measurement(war__be_t2005, war_powers_allocation__inherent_executive_reading, base_extractiveness, 2005, 0.58).
narrative_ontology:measurement(war__be_t2025, war_powers_allocation__inherent_executive_reading, base_extractiveness, 2025, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t1945, war_powers_allocation__inherent_executive_reading, suppression_requirement, 1945, 0.15).
narrative_ontology:measurement(war__su_t1960, war_powers_allocation__inherent_executive_reading, suppression_requirement, 1960, 0.18).
narrative_ontology:measurement(war__su_t1975, war_powers_allocation__inherent_executive_reading, suppression_requirement, 1975, 0.22).
narrative_ontology:measurement(war__su_t1990, war_powers_allocation__inherent_executive_reading, suppression_requirement, 1990, 0.24).
narrative_ontology:measurement(war__su_t2005, war_powers_allocation__inherent_executive_reading, suppression_requirement, 2005, 0.26).
narrative_ontology:measurement(war__su_t2025, war_powers_allocation__inherent_executive_reading, suppression_requirement, 2025, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__inherent_executive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, congressional_primacy_reading).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, functional_accommodation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the war_powers_allocation kernel, decomposed per the epsilon-invariance principle because 'the constitutional war-powers allocation' does not have a single stable epsilon across readings: congressional_primacy_reading treats unauthorized deployment as constitutionally impermissible (near-zero legitimate scope for unilateral action beyond immediate defense), functional_accommodation_reading splits epsilon by operational context, and this inherent_executive_reading treats congressional authorization as non-binding courtesy (high legitimate scope for unilateral action, epsilon 0.61 here). All three share the same underlying text and history but instantiate structurally distinct constraints with different beneficiary/victim sets and different classifications. Linked bidirectionally in each sibling's network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
