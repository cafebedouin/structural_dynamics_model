% ============================================================================
% CONSTRAINT STORY: article_27_veto_power__coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_27_veto_power__coordination_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: article_27_veto_power__coordination_reading
 *   human_readable: P5 Veto as Great-Power War-Prevention Gate (Coordination Reading)
 *   domain: international_relations/institutional_design/constitutional_law
 *
 * SUMMARY:
 *   Article 27 of the UN Charter gives each of the five permanent members of
 *   the Security Council an unconditional veto over substantive resolutions.
 *   This story models the veto in its coordination function: the unanimity
 *   gate that ensures the Council's enforcement machinery can never be turned
 *   into an instrument that compels a nuclear great power into military
 *   confrontation against its will. The gate solves the collective-action
 *   problem that destroyed the League of Nations — a binding
 *   collective-security framework that great powers will not join, or will
 *   abandon, if its decisions can direct them into war. Every class of party
 *   the arrangement touches is positioned as a net beneficiary: the five hold
 *   the gate and are its protected class; the elected membership and the
 *   broader UN membership receive the stability assurance; civilian
 *   populations hold the avoided counterfactual. The story declares no victim
 *   class, and its metrics describe a low-extraction, low-suppression,
 *   substantively exercised coordination device whose modest costs are the
 *   premium of the coordination itself. KEY AGENTS (by structural
 *   relationship): - permanent_five_governments: Agenda-setting beneficiaries
 *   (institutional/constrained) — each holds the veto; no binding resolution
 *   can compel any of them into confrontation with another nuclear power;
 *   they administer the gate and are its protected class -
 *   elected_council_members: Beneficiaries (moderate/constrained) — rotate
 *   onto the Council; receive the stability assurance; cannot themselves
 *   block - general_un_membership: Beneficiaries (organized/constrained) —
 *   the broader membership that accepted the gate as the price of great-power
 *   participation - great_power_civilian_populations: Beneficiaries
 *   (powerless/trapped) — populations who would bear the costs of nuclear-age
 *   great-power war; hold no Council seat - veto_reform_coalitions: Excluded
 *   (organized/trapped) — restraint-code and amendment advocates with no
 *   procedural path to the decision point - institutional_design_scholars:
 *   Analytical observer — assesses the gate's coordination function against
 *   the historical counterfactuals
 *
 * KEY AGENTS:
 *   - permanent_five_governments: Agenda-setting beneficiaries (institutional/constrained) — hold the veto, administer the gate, protected from binding compulsion into great-power confrontation
 *   - elected_council_members: Beneficiaries (moderate/constrained) — rotating Council seats; receive the stability assurance; cannot block; closest seat to the gate's coarse cost
 *   - general_un_membership: Beneficiaries (organized/constrained) — ratified the Charter accepting the gate; receive the assurance; hold only recommendatory alternatives
 *   - great_power_civilian_populations: Beneficiaries (powerless/trapped) — bear the avoided counterfactual; no institutional seat
 *   - veto_reform_coalitions: Excluded (organized/trapped) — ACT group and French-Mexican initiative; every remedy terminates at the Article 108 ratification hold
 *   - institutional_design_scholars: Analytical observer (analytical/analytical) — evaluate the gate against League-collapse and San Francisco counterfactuals
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__coordination_reading, 0.18).
domain_priors:suppression_score(article_27_veto_power__coordination_reading, 0.15).
domain_priors:theater_ratio(article_27_veto_power__coordination_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__coordination_reading, rope).
narrative_ontology:human_readable(article_27_veto_power__coordination_reading, "P5 Veto as Great-Power War-Prevention Gate (Coordination Reading)").
narrative_ontology:topic_domain(article_27_veto_power__coordination_reading, "international_relations/institutional_design/constitutional_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__coordination_reading, 'a17b7dc7-ecb0-4160-8dba-f97714235004').
narrative_ontology:cs_kernel_codification('a17b7dc7-ecb0-4160-8dba-f97714235004', fixed_text).
narrative_ontology:cs_authority_grounding('a17b7dc7-ecb0-4160-8dba-f97714235004', lineage).
narrative_ontology:cs_interpretation_layer_present('a17b7dc7-ecb0-4160-8dba-f97714235004').
narrative_ontology:cs_reading_relation('a17b7dc7-ecb0-4160-8dba-f97714235004', article_27_veto_power__oligopoly_reading, coexists_with).
narrative_ontology:cs_reading_relation('a17b7dc7-ecb0-4160-8dba-f97714235004', article_27_veto_power__sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('a17b7dc7-ecb0-4160-8dba-f97714235004', foundational, great_power_war_prevention_requires_unanimity_gate).
narrative_ontology:cs_axiom_status(great_power_war_prevention_requires_unanimity_gate, holdable).
narrative_ontology:cs_axiom_grounding('a17b7dc7-ecb0-4160-8dba-f97714235004', great_power_war_prevention_requires_unanimity_gate, instrumental).
narrative_ontology:cs_axiom('a17b7dc7-ecb0-4160-8dba-f97714235004', foundational, no_council_compulsion_of_nuclear_states).
narrative_ontology:cs_axiom_status(no_council_compulsion_of_nuclear_states, holdable).
narrative_ontology:cs_axiom_grounding('a17b7dc7-ecb0-4160-8dba-f97714235004', no_council_compulsion_of_nuclear_states, deontological).
narrative_ontology:cs_reference_frame('a17b7dc7-ecb0-4160-8dba-f97714235004', san_francisco_unanimity_compact).
narrative_ontology:cs_drift_state('a17b7dc7-ecb0-4160-8dba-f97714235004', contemporary_multipolar_nuclear_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('a17b7dc7-ecb0-4160-8dba-f97714235004', '').
narrative_ontology:cs_kernel_id(article_27_veto_power__coordination_reading, article_27_veto_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, permanent_five_governments).
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, elected_council_members).
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, general_un_membership).
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, great_power_civilian_populations).
narrative_ontology:constraint_vindicates(article_27_veto_power__coordination_reading, great_power_unanimity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Each holds an unconditional veto over Security Council resolutions; no binding resolution can direct any of them into military confrontation with another nuclear power against its will. They drafted the gate into the Charter, administer it through the Council's rules, and hold an absolute ratification hold over any amendment under Article 108. Exit would mean withdrawal from, or open defiance of, the Council — either would cost them the legitimacy platform of the institution they anchor, so they remain inside and exercise the gate instead.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, permanent_five_governments, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(article_27_veto_power__coordination_reading, permanent_five_governments, beneficiary).

% Rotate onto the Council for two-year terms through their regional groups; their votes count toward resolutions and they share the assurance that Council enforcement cannot march a great power into war — the failure mode that in past systems consumed smaller states first. They cannot block a resolution themselves and occasionally see measures they supported fail to a permanent member's negative vote; their recourse is the General Assembly and coalition diplomacy, not exit.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, elected_council_members, beneficiary,
    moderate, biographical, constrained, regional).

% The broader membership that accepted the veto at San Francisco as the price of great-power participation. They receive the stability assurance the gate provides and conduct most of their security diplomacy through UN forums; they cannot amend the gate (Article 108 gives the five an absolute ratification hold), and their collective alternatives — General Assembly resolutions, the Uniting for Peace procedure — are recommendatory rather than binding.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, general_un_membership, beneficiary,
    organized, biographical, constrained, global).

% Civilian populations of nuclear-armed states, and populations within reach of escalation dynamics generally, who would bear the direct costs if Council majorities could direct great powers into mutual confrontation. They hold no seat in the Council, cannot veto, and cannot exit the state system whose war decisions the gate shapes; their stake lies entirely in the avoided counterfactual.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, great_power_civilian_populations, beneficiary,
    powerless, generational, trapped, global).

% Cross-regional coalitions (the ACT group, the French-Mexican restraint initiative) campaigning for restraint codes, voluntary-use declarations, and Assembly oversight of veto use. Their proposals all terminate at Article 108: no amendment takes effect without ratification by all five permanent members, so the conversation they want to join has no decision point they can reach; they persist as advocacy voices without procedural standing to change the rule.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, veto_reform_coalitions, excluded,
    organized, biographical, trapped, global).

% Study the gate's coordination function against historical counterfactuals — the League's collapse, the San Francisco negotiating record, P5 behavior when the gate was bypassed — and assess whether the veto's breadth is necessary to its war-prevention function or exceeds it.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, institutional_design_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_27_veto_power__coordination_reading, permanent_five_governments).
narrative_ontology:fixing_cost_class(article_27_veto_power__coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unanimity gate among the five nuclear great powers: no Security Council resolution can issue binding enforcement directions that compel a permanent member into military confrontation against its will, which keeps the Council from becoming an instrument of direct great-power war and keeps the great powers inside a binding collective-security framework they would otherwise not join or would abandon.
% TRANSFER_FUNCTION: Allocates unconditional negative control over Council enforcement decisions to each of the five permanent members, and distributes the resulting assurance — that no binding resolution can march a great power into war — across the whole membership; what is surrendered is the Council majority's capacity to act over a great power's objection.
% ABSENT_VOICES: Veto-reform coalitions (the ACT group, the French-Mexican restraint initiative) and member states whose preferred Council action has failed to a permanent member's negative vote would contest the gate's granularity — but Article 108 gives the five an absolute ratification hold over any amendment, so the voices that would argue the coarse-gate question have no decision forum to reach; they persist as advocacy outside the conversation that could enact their proposals.
% DISAPPEARANCE_RATIONALE: Without the unanimity gate, binding Council resolutions could direct great-power military action over a nuclear state's objection: the targeted power would either defy the Council — collapsing the enforcement framework and returning the system to pre-1945 alliance blocs — or comply into a confrontation with another nuclear power. The five would not have ratified the Charter without the gate (the San Francisco record shows they declined a League-style framework on precisely these terms), so the institution as constituted would not exist; the postwar collective-security architecture rearranges around either great-power exit or great-power war risk.
% FOUNDING_PROBLEM: The League of Nations' collapse: a collective-security framework whose decisions could in principle bind the great powers, which the great powers then ignored, exited, or defied as the 1930s crises escalated, dissolving into general war. The Charter's drafters needed a design the five great powers would ratify and remain inside — which required that no Council decision could bind any of them into war against its will.
% FOUNDING_PROBLEM_CORROBORATION: The San Francisco conference record: smaller founding delegations objected to the veto and accepted it expressly as the price of great-power participation — attestation of the gate's load-bearing role from outside the P5. Standard diplomatic history of the League's collapse, authored by no party to the current arrangement, corroborates the founding problem itself. The USSR's 1950 behavior — boycotting the Council, then returning once a gateless Council had authorized war against its client — corroborates from the record that the gate is what keeps great powers inside the framework. No corroboration exists from within the P5 that is independent of their interest, which is why the external record carries the attestation.
narrative_ontology:disappearance_verdict(article_27_veto_power__coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_27_veto_power__coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_27_veto_power__coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_27_veto_power__coordination_reading, 0.18, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_27_veto_power__coordination_reading_tests).
:- end_tests(article_27_veto_power__coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.18) because, on this reading's referent, the gate's costs — blocked Council majorities, coarse granularity — are the premium paid for the coordination function, not rents skimmed from a subject class; epsilon here derives from the collective-action failure risk that exists without the unanimity gate, and the value sits modestly above the enforcement_mechanism coordination floor, representing inherent coordination cost rather than extractive overhead. Suppression is low (0.15): the veto coerces no one and is a self-executing procedural entitlement; alternatives (majority binding, weighted voting, restraint codes) are debated openly rather than suppressed — the Article 108 amendment lock is the mechanism's own design feature, not external coercion. Theater is low (0.10): vetoes are substantively exercised with real effects; nothing about the gate is maintained performatively. Accessibility_collapse (0.60) is rope-typical: once the League's collapse and the nuclear revolution are understood, the majority-binding alternative collapses for any serious institutional designer, though restraint-code proposals remain live at the margin. Resistance (0.50) is moderate, persistent, and structurally ineffective — Uniting for Peace (1950), the ACT group, the French-Mexican initiative — because every remedy terminates at the P5 ratification hold. claimed_type is rope from the structure: a genuine collective-action problem (great-power war), minimal coercive overhead, net beneficiaries across all seats, alternatives contested but not suppressed. The metrics were authored descriptively and independently of the claim. No suppression_requirement series is authored: the gate's enforcement picture is static (a self-executing rule with no enforcement machinery to build up or erode), which the base_properties.suppression scalar already captures. The measurement series runs on one shared nine-point grid so every tracked metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   Within this reading the seats converge rather than diverge — every declared party is a net beneficiary, so per-seat effective extraction computes low across the surface. The seats still differ in what they touch of the gate's coarse cost: elected_council_members are the only seat that ever feels a blocked resolution directly (the nearest any seat comes to bearing a cost), general_un_membership feels it only through foregone collective action, great_power_civilian_populations bear nothing in the observed world and everything only in the avoided counterfactual, and permanent_five_governments bear nothing at all while holding the discretion. The perspectivals that would read this same arrangement as extraction belong to different constraints (the sibling readings of the kernel, authored in their own files); within this story's referent, every seat sits on the beneficiary side of the derivation.
 *
 * DIRECTIONALITY LOGIC:
 *   Every declared party is a beneficiary, so the derivation chain places every seat near the d = 0.0 end and damps effective extraction across the surface. permanent_five_governments sit nearest the full-beneficiary end: the gate directly subsidizes their discretion and shields each from binding compulsion. great_power_civilian_populations sit nearly as low on the catastrophic dimension — in the counterfactual without the gate they bear everything. elected_council_members and general_un_membership derive slightly higher d: they receive the stability assurance but hold none of the discretion, and they occasionally absorb the coarse gate's blocked-action cost without any compensating control right. No victim declarations exist, so no seat derives toward the target end; the residual epsilon is the coordination premium, not extraction from a subject class. No directionality overrides are needed: the structural declarations (all-beneficiary, differentiated exit) produce the correct d for every seat without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The rope classification here is the guard against one specific mislabeling: reading the veto's blocked-majority costs as pure extraction and licensing majority-override remedies that would reintroduce the great-power war risk the gate exists to suppress. The founding problem — a binding collective-security framework that great powers will join and stay inside — remains live as long as nuclear rivalry does, so no mandatrophy resolution is declared. The measurement series is the early-warning instrument: base_extractiveness drifts gently upward (0.12 to 0.18) as uses beyond the strict war-prevention core accumulate; if that drift continued toward extraction-dominant levels, the coordination reading would be losing descriptive grip and the arrangement would need re-derivation under a reading that carries a victim class. That is a classification-boundary signal, not a mandatrophy event: the gate's function has not atrophied, and no theatrical maintenance props it up.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates the coordination_reading of the article_27_veto_power kernel; the sibling readings (oligopoly_reading, sovereignty_reading) instantiate different constraints from the same Charter text — which structural element of the veto arrangement does each reading treat as the operative core, and how do their epsilon and beneficiary/victim structures differ?',
    'Cross-reading classification: compile and classify all three sibling stories, then locate the disagreement structurally — the coordination reading fixes the core as the unanimity gate over compulsion of great powers into war (low epsilon, no victim class); the oligopoly reading fixes it as entrenched discretion exceeding that necessity (high epsilon, blocked-action victim class); the sovereignty reading fixes it as the consent principle applied to great powers (epsilon indexed to the consent norm''s value).',
    'If the oligopoly reading''s scope claim is sustained, this story''s epsilon is understated and its beneficiary structure incomplete (a blocked-action victim class exists); if the coordination reading is sustained, the sibling stories overstate extraction by counting the gate''s necessary coarseness as rent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Which reading of the Article 27 kernel this story''s epsilon and beneficiary structure are indexed to.').

omega_variable(
    gate_coarseness_necessity,
    'Is the veto''s coarse granularity — its availability against any substantive resolution, not only ones that would compel the vetoing power itself into war — a structurally necessary feature of a ratifiable, functioning unanimity gate, or discretionary surplus beyond war-prevention necessity?',
    'Institutional-design and use-pattern analysis: could a narrower gate (veto confined to resolutions directing force at, or binding, the vetoing state) have been ratified at San Francisco and functioned since; compare the historical distribution of vetoes on resolutions that could plausibly compel the vetoing power into confrontation against vetoes on third-party conflicts.',
    'If the coarse gate is necessary, this story''s epsilon stands and the rope classification holds; if a large share of veto use is discretionary surplus, that surplus belongs to the oligopoly reading''s constraint and this story''s epsilon must be re-derived on the narrow-gate referent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gate_coarseness_necessity, empirical, 'Whether the veto''s full breadth is necessary to its coordination function or contains extractive surplus.').

omega_variable(
    great_power_participation_counterfactual,
    'Would the five great powers have ratified the Charter and remained inside the Council across the postwar period absent the unanimity gate?',
    'Historical analysis of the Dumbarton Oaks and San Francisco negotiating records; natural experiments in P5 behavior when the gate was bypassed (the USSR''s 1950 boycott and return after the Korean resolution); the record of gateless collective-security proposals.',
    'If the great powers would not have joined or would have exited, the gate is load-bearing coordination and the rope classification is robust; if they would have participated regardless, the war-prevention justification weakens and the residual function requires re-derivation under a sibling reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(great_power_participation_counterfactual, empirical, 'Whether the unanimity gate was the binding constraint on great-power membership in the collective-security framework.').

omega_variable(
    victim_class_absence_check,
    'Does the gate as such impose net costs on any identifiable class of states, or does every class benefit net from the avoided great-power-war counterfactual?',
    'Class-level welfare comparison: states whose Council-supported action failed to a veto versus those same states'' exposure under a gateless Council carrying great-power war risk and institutional collapse; coalition analysis of whether blocked-action states could credibly prefer amendment given the Article 108 ratification hold.',
    'If a net-victim class exists, this story''s beneficiary structure is incomplete and the classification shifts toward tangled_rope with the blocked-action states declared; if none exists, the rope classification stands with no victim declaration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_class_absence_check, empirical, 'Whether the unanimity gate leaves any net-victim class under this reading''s referent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__coordination_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_27_veto_power__coordination_reading, theater_ratio, 0, 0.04).
narrative_ontology:measurement(arti_tr_t10, article_27_veto_power__coordination_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(arti_tr_t20, article_27_veto_power__coordination_reading, theater_ratio, 20, 0.06).
narrative_ontology:measurement(arti_tr_t30, article_27_veto_power__coordination_reading, theater_ratio, 30, 0.06).
narrative_ontology:measurement(arti_tr_t40, article_27_veto_power__coordination_reading, theater_ratio, 40, 0.07).
narrative_ontology:measurement(arti_tr_t50, article_27_veto_power__coordination_reading, theater_ratio, 50, 0.08).
narrative_ontology:measurement(arti_tr_t60, article_27_veto_power__coordination_reading, theater_ratio, 60, 0.08).
narrative_ontology:measurement(arti_tr_t70, article_27_veto_power__coordination_reading, theater_ratio, 70, 0.09).
narrative_ontology:measurement(arti_tr_t80, article_27_veto_power__coordination_reading, theater_ratio, 80, 0.1).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_27_veto_power__coordination_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(arti_be_t10, article_27_veto_power__coordination_reading, base_extractiveness, 10, 0.13).
narrative_ontology:measurement(arti_be_t20, article_27_veto_power__coordination_reading, base_extractiveness, 20, 0.14).
narrative_ontology:measurement(arti_be_t30, article_27_veto_power__coordination_reading, base_extractiveness, 30, 0.15).
narrative_ontology:measurement(arti_be_t40, article_27_veto_power__coordination_reading, base_extractiveness, 40, 0.16).
narrative_ontology:measurement(arti_be_t50, article_27_veto_power__coordination_reading, base_extractiveness, 50, 0.16).
narrative_ontology:measurement(arti_be_t60, article_27_veto_power__coordination_reading, base_extractiveness, 60, 0.17).
narrative_ontology:measurement(arti_be_t70, article_27_veto_power__coordination_reading, base_extractiveness, 70, 0.18).
narrative_ontology:measurement(arti_be_t80, article_27_veto_power__coordination_reading, base_extractiveness, 80, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(article_27_veto_power__coordination_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_27_veto_power__coordination_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_27_veto_power__coordination_reading, article_27_veto_power__oligopoly_reading).
narrative_ontology:affects_constraint(article_27_veto_power__coordination_reading, article_27_veto_power__sovereignty_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the P5 veto' covers structurally distinct claims that this corpus models as separate constraints sharing the article_27_veto_power kernel. This story (coordination_reading) authors the veto as a war-prevention unanimity gate: low epsilon, all-member beneficiary structure, no victim class. The sibling oligopoly_reading authors the same Charter text as entrenched discretion extracting authority rents, with states whose Council action is blocked as the victim class (high epsilon). The sovereignty_reading authors it as the Westphalian consent principle applied to great powers, with epsilon indexed to the value of the consent norm itself. The epsilon divergence is located in the gate's scope: whether the veto's breadth beyond resolutions that would compel the vetoing power into war is necessary coarseness (this reading) or extractive surplus (oligopoly reading). Each file carries its own stable epsilon; this file links to its siblings via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
