% ============================================================================
% CONSTRAINT STORY: border_normative_status__freedom_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_normative_status__freedom_primary, []).

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
 *   constraint_id: border_normative_status__freedom_primary
 *   human_readable: Border Enforcement Read as Unjustified Restriction on a Fundamental Right (Freedom-Primary Reading)
 *   domain: political_philosophy/international_law/migration
 *
 * SUMMARY:
 *   This story instantiates the freedom_primary reading of the
 *   border_normative_status kernel: freedom of movement is treated as a
 *   fundamental human right on par with other basic liberties, and
 *   territorial exclusion is treated as presumptively impermissible absent
 *   extraordinary justification (imminent threat, genuine scarcity) that
 *   ordinary immigration control does not clear. Under this reading, border
 *   enforcement is not a legitimate exercise of collective self-determination
 *   but an unjustified rights violation maintained by state coercion. This is
 *   a DIFFERENT constraint from the sovereignty_primary reading (where
 *   exclusion is a legitimate instrument of self-determination and the victim
 *   set is largely absent) and from the qualified_sovereignty reading (where
 *   exclusion is conditionally legitimate subject to proportionality review).
 *   Each reading has its own stable ε and its own beneficiary/victim
 *   structure; they are linked as sibling constraints, not merged into one
 *   story.
 *
 * KEY AGENTS:
 *   - excluded_would_be_migrants: primary target (powerless/trapped) — bears the full cost of exclusion the reading holds illegitimate
 *   - asylum_seekers_in_transit: acute target (powerless/trapped) — most immediate cost bearer
 *   - displaced_domestic_workers: secondary target under this reading's prescribed remedy (moderate/constrained)
 *   - receiving_state_incumbent_workers: beneficiary (organized/constrained) — protected by restricted labor markets
 *   - border_enforcement_industry: institutional beneficiary (institutional/arbitrage) — budget and mandate depend on continued enforcement
 *   - citizenship_status_holders: diffuse beneficiary (organized/mobile) — holds the birthright bundle this reading calls arbitrary
 *   - receiving_states: agenda_setter (institutional/arbitrage) — sets and enforces entry policy
 *   - cosmopolitan_rights_theorists: analytical observer (analytical/analytical) — articulates the reading's normative case
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__freedom_primary, 0.71).
domain_priors:suppression_score(border_normative_status__freedom_primary, 0.78).
domain_priors:theater_ratio(border_normative_status__freedom_primary, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, extractiveness, 0.71).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__freedom_primary, snare).
narrative_ontology:human_readable(border_normative_status__freedom_primary, "Border Enforcement Read as Unjustified Restriction on a Fundamental Right (Freedom-Primary Reading)").
narrative_ontology:topic_domain(border_normative_status__freedom_primary, "political_philosophy/international_law/migration").

domain_priors:requires_active_enforcement(border_normative_status__freedom_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__freedom_primary, 'd174db84-b2f4-41df-8a6c-d99f5936d46f').
narrative_ontology:cs_kernel_codification('d174db84-b2f4-41df-8a6c-d99f5936d46f', distributed).
narrative_ontology:cs_authority_grounding('d174db84-b2f4-41df-8a6c-d99f5936d46f', distributed).
narrative_ontology:cs_reading_relation('d174db84-b2f4-41df-8a6c-d99f5936d46f', border_normative_status__sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('d174db84-b2f4-41df-8a6c-d99f5936d46f', border_normative_status__qualified_sovereignty, influences).
narrative_ontology:cs_axiom('d174db84-b2f4-41df-8a6c-d99f5936d46f', foundational, movement_as_fundamental_right).
narrative_ontology:cs_axiom_status(movement_as_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding('d174db84-b2f4-41df-8a6c-d99f5936d46f', movement_as_fundamental_right, deontological).
narrative_ontology:cs_axiom('d174db84-b2f4-41df-8a6c-d99f5936d46f', foundational, territorial_birthright_morally_arbitrary).
narrative_ontology:cs_axiom_status(territorial_birthright_morally_arbitrary, holdable).
narrative_ontology:cs_axiom_grounding('d174db84-b2f4-41df-8a6c-d99f5936d46f', territorial_birthright_morally_arbitrary, deontological).
narrative_ontology:cs_axiom('d174db84-b2f4-41df-8a6c-d99f5936d46f', secondary, exclusion_requires_extraordinary_justification).
narrative_ontology:cs_axiom_status(exclusion_requires_extraordinary_justification, holdable).
narrative_ontology:cs_axiom_grounding('d174db84-b2f4-41df-8a6c-d99f5936d46f', exclusion_requires_extraordinary_justification, deontological).
narrative_ontology:cs_reference_frame('d174db84-b2f4-41df-8a6c-d99f5936d46f', unqualified_movement_liberty).
narrative_ontology:cs_drift_state('d174db84-b2f4-41df-8a6c-d99f5936d46f', contemporary_migration_governance_era, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('d174db84-b2f4-41df-8a6c-d99f5936d46f', '').
narrative_ontology:cs_kernel_id(border_normative_status__freedom_primary, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__freedom_primary, receiving_state_incumbent_workers).
narrative_ontology:constraint_beneficiary(border_normative_status__freedom_primary, border_enforcement_industry).
narrative_ontology:constraint_beneficiary(border_normative_status__freedom_primary, citizenship_status_holders).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, excluded_would_be_migrants).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, displaced_domestic_workers).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, stateless_and_undocumented_residents).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, asylum_seekers_in_transit).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, citizenship_status_holders).
narrative_ontology:constraint_vindicates(border_normative_status__freedom_primary, freedom_of_movement_as_fundamental_right).
narrative_ontology:constraint_vindicates(border_normative_status__freedom_primary, territorial_birthright_as_morally_arbitrary).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Denied entry, visas, or legal status by territorial border controls despite (on this reading) holding an unqualified moral right to move. Bear the full weight of exclusion: blocked labor markets, family separation, and in many cases exposure to danger in the state of origin. Have no institutional channel to contest exclusion on rights grounds because domestic and international law still treat sovereignty as the default.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, excluded_would_be_migrants, payer,
    powerless, biographical, trapped, global).

% On this reading, entered the victim set because open borders (the corrective the reading demands) would expose them to wage competition and labor-market displacement they did not choose and cannot easily exit from — they are structurally implicated by the policy remedy this reading prescribes, not by the border itself.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, displaced_domestic_workers, payer,
    moderate, biographical, constrained, national).

% Physically present at or near borders, often in transit camps or detention, awaiting adjudication under a sovereignty-default legal architecture that this reading holds to be illegitimate on its face. Suffer the most acute and immediate costs of the gap between the moral claim and its institutional non-recognition.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, asylum_seekers_in_transit, payer,
    powerless, immediate, trapped, regional).

% Benefit from border-restricted labor markets that protect wages and job access in sectors exposed to migrant labor competition. Organized politically (unions, national electorates) and able to resist policy change; their gain is the flip side of the exclusion this reading calls unjustified.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, receiving_state_incumbent_workers, beneficiary,
    organized, biographical, constrained, national).

% Government agencies, private detention contractors, and surveillance-technology vendors whose budgets, staffing, and institutional mandates depend on maintaining and intensifying border enforcement. Have strong incentive to defend the sovereignty framing this reading contests, regardless of the underlying rights question.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, border_enforcement_industry, beneficiary,
    institutional, generational, arbitrage, national).

% Hold the bundle of goods (voting rights, welfare access, unrestricted internal movement, labor market priority) that citizenship status confers, which on this reading is morally arbitrary birthright privilege sustained only by border enforcement. Some bear diffuse costs if enforcement erodes services or social trust, but most retain net benefit from the status quo.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, citizenship_status_holders, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(border_normative_status__freedom_primary, citizenship_status_holders, payer).

% Set and enforce entry criteria, visa regimes, and deportation policy. On this reading, exercise a moral authority they do not actually possess without extraordinary justification (imminent threat, genuine scarcity) that ordinary immigration control does not meet. Retain full practical capacity to change policy but face no binding institutional requirement to do so.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, receiving_states, agenda_setter,
    institutional, generational, arbitrage, national).

% Advance the philosophical case (Carens, open-borders liberalism) that freedom of movement is continuous with other basic liberties and that birthplace is a morally arbitrary basis for life-chance allocation. Analyze but do not administer the constraint; their arguments are cited by advocacy movements but carry no binding institutional force.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, cosmopolitan_rights_theorists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_normative_status__freedom_primary, diffuse).
narrative_ontology:fixing_cost_class(border_normative_status__freedom_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: On this reading, there is no genuine coordination function performed by exclusion itself — the only coordination that could be legitimate is coordinating admission processes (safety screening, resource planning for reception) that do not depend on a right to exclude. Any coordination story beyond that is treated as a cover for protecting incumbent advantage.
% TRANSFER_FUNCTION: Moves life-chances, labor-market access, physical safety, and family unity from would-be migrants and asylum seekers to incumbent citizens and the institutions (enforcement agencies, contractors) that administer exclusion; moves a smaller, contested burden onto domestic workers exposed to increased labor competition under the reading's prescribed remedy.
% ABSENT_VOICES: Excluded migrants and asylum seekers are structurally absent from the domestic and international fora that set border policy — they are the population most affected and least represented in the legislatures and courts that adjudicate entry rules. Displaced domestic workers are present but frequently reduced to a rhetorical foil in the debate rather than genuinely consulted.
% DISAPPEARANCE_RATIONALE: If border enforcement disappeared overnight, global labor and residence patterns would reorganize substantially: large population movements, altered wage structures in destination labor markets, transformed political coalitions around citizenship and welfare eligibility, and a wholesale renegotiation of what states are for. This is a live structural fact, not a norm reading — the magnitude of rearrangement is why the kernel is contested at all.
% FOUNDING_PROBLEM: Historically, territorial border control was built to solve problems of taxation, conscription, security from armed incursion, and (later) management of collective self-determination and welfare-state membership boundaries.
% FOUNDING_PROBLEM_CORROBORATION: Sovereignty-primary theorists and state officials attest the founding problem (self-determination, resource stewardship) remains live. Cosmopolitan rights theorists and human rights bodies (UNHCR reporting, academic migration ethics literature) attest that whatever functional problem borders once solved, the exclusion of moral persons from an unqualified right is not corroborated by anyone outside the states and enforcement institutions that benefit from maintaining it — no independent tribunal or philosophical consensus outside the beneficiary set has ratified the sovereignty premise as itself an extraordinary justification.
narrative_ontology:disappearance_verdict(border_normative_status__freedom_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_normative_status__freedom_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__freedom_primary, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(border_normative_status__freedom_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_normative_status__freedom_primary, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_normative_status__freedom_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_normative_status__freedom_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_normative_status__freedom_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.71) and suppression (0.78) are authored high because, under this reading, exclusion transfers substantial life-chances from a population with an unqualified moral claim to move, and that transfer is sustained entirely by active coercive enforcement (detention, deportation, interdiction) rather than by any voluntarily accepted coordination. Theater ratio (0.42) reflects that a meaningful share of enforcement activity is justified rhetorically (security, orderly processing) in excess of what the reading would accept as extraordinary justification — much of the apparatus performs legitimacy it does not, on this reading, possess. Accessibility collapse (0.58) is moderate rather than near-total because legal migration channels, asylum claims, and irregular movement all persist as partial alternatives despite enforcement; resistance (0.74) is high because migrants, advocacy networks, and international rights bodies actively contest exclusion on rights grounds.
 *
 * DIRECTIONALITY LOGIC:
 *   Excluded migrants and asylum seekers sit at the full-target end: trapped exit options, powerless standing, and a moral claim this reading holds is being violated without adequate justification — directionality derives near d=1.0. Receiving-state incumbent workers and the enforcement industry sit toward the beneficiary end: they collect the protective and institutional gains that exclusion produces. Displaced domestic workers occupy an unusual middle position — they enter the victim set specifically because the remedy this reading prescribes (open movement) would expose them to labor-market costs; their directionality is driven by the reading's own policy implication, not by the border itself, and is documented as a distinguishing feature of this reading versus its siblings.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem interview shows the classic mandatrophy structure this reading diagnoses: the functions borders were built to serve (security, self-determination) may still be live in sovereignty_primary's telling, but on the freedom_primary reading those functions do not by themselves clear the bar of extraordinary justification required to override a fundamental right. Corroboration outside the beneficiary set (rights bodies, migration ethics scholarship) supports the 'dead-or-insufficient justification' status; corroboration from receiving states and enforcement institutions supports 'live.' The contested status itself is the data — the mismatch between founding_problem_status=contested and disappearance_verdict=world_rearranges signals an arrangement whose defenders and critics agree on stakes but disagree entirely on legitimacy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraordinary_justification_threshold,
    'What, precisely, counts as the ''extraordinary justification'' this reading requires before exclusion is permissible, and does any actually-existing immigration regime meet it?',
    'Comparative institutional analysis against the reading''s own stated exceptions (imminent security threat, genuine absorptive-capacity scarcity) applied to specific national border regimes, adjudicated by bodies outside both the excluding states and migrant-advocacy organizations.',
    'If no existing regime meets the threshold, virtually all current border enforcement is reclassified as unjustified extraction under this reading; if some regimes plausibly meet it, the victim set and extraction magnitude narrow substantially.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraordinary_justification_threshold, conceptual, 'Whether any real-world border policy satisfies this reading''s own justificatory standard.').

omega_variable(
    domestic_worker_displacement_magnitude,
    'How large is the actual wage and employment displacement effect on domestic workers from significantly liberalized movement, versus the scale claimed by opponents of open borders?',
    'Empirical labor economics literature on migration''s wage effects (e.g., natural experiments from past liberalization episodes), assessed independently of both open-borders advocates and restrictionist political actors.',
    'A small measured effect would weaken this reading''s own acknowledgment of the domestic-worker victim class as a serious limiting condition; a large effect would sharpen the tension this reading carries internally between its rights claim and its distributive consequences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_worker_displacement_magnitude, empirical, 'Empirical scale of the labor-market cost this reading''s own remedy imposes on domestic workers.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the choice among freedom_primary, sovereignty_primary, and qualified_sovereignty itself resolvable by evidence, or is it an irreducibly normative/political commitment that no amount of empirical work about migration effects can settle?',
    'None fully available — this is a question about the type of question the kernel poses. Partial evidence: whether international law and state practice show convergence toward one reading over time (e.g., expanding non-refoulement obligations would suggest drift toward qualified_sovereignty).',
    'If irreducibly normative, this story''s classification of the constraint as extractive/snare-like is itself a reading-dependent verdict rather than a discovered fact, and the sibling stories must each be read as internally coherent rather than as competing empirical hypotheses about the same object.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether the kernel contest is empirically adjudicable or a bare normative disagreement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__freedom_primary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_normative_status__freedom_primary, theater_ratio, 0, 0.22).
narrative_ontology:measurement(bord_tr_t8, border_normative_status__freedom_primary, theater_ratio, 8, 0.27).
narrative_ontology:measurement(bord_tr_t16, border_normative_status__freedom_primary, theater_ratio, 16, 0.32).
narrative_ontology:measurement(bord_tr_t24, border_normative_status__freedom_primary, theater_ratio, 24, 0.36).
narrative_ontology:measurement(bord_tr_t32, border_normative_status__freedom_primary, theater_ratio, 32, 0.39).
narrative_ontology:measurement(bord_tr_t40, border_normative_status__freedom_primary, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_normative_status__freedom_primary, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(bord_be_t8, border_normative_status__freedom_primary, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(bord_be_t16, border_normative_status__freedom_primary, base_extractiveness, 16, 0.64).
narrative_ontology:measurement(bord_be_t24, border_normative_status__freedom_primary, base_extractiveness, 24, 0.67).
narrative_ontology:measurement(bord_be_t32, border_normative_status__freedom_primary, base_extractiveness, 32, 0.69).
narrative_ontology:measurement(bord_be_t40, border_normative_status__freedom_primary, base_extractiveness, 40, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_normative_status__freedom_primary, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(bord_su_t8, border_normative_status__freedom_primary, suppression_requirement, 8, 0.65).
narrative_ontology:measurement(bord_su_t16, border_normative_status__freedom_primary, suppression_requirement, 16, 0.7).
narrative_ontology:measurement(bord_su_t24, border_normative_status__freedom_primary, suppression_requirement, 24, 0.73).
narrative_ontology:measurement(bord_su_t32, border_normative_status__freedom_primary, suppression_requirement, 32, 0.76).
narrative_ontology:measurement(bord_su_t40, border_normative_status__freedom_primary, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_normative_status__freedom_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(border_normative_status__freedom_primary, border_normative_status__sovereignty_primary).
narrative_ontology:affects_constraint(border_normative_status__freedom_primary, border_normative_status__qualified_sovereignty).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the border_normative_status kernel. sovereignty_primary treats territorial exclusion as a legitimate instrument of collective self-determination (minimal or no migrant victim set; the constraint reads closer to rope/tangled_rope). qualified_sovereignty treats exclusion as conditionally legitimate subject to proportionality and rights-consistency review (a narrower, more contested victim set, closer to tangled_rope). freedom_primary (this story) treats exclusion as presumptively illegitimate absent extraordinary justification, producing the widest victim set (excluded migrants, asylum seekers) plus a reading-specific victim class (displaced domestic workers) generated by the reading's own prescribed remedy. All three share the same underlying institutional facts about border enforcement; they diverge entirely on the normative kernel each reading commits to, which is why they are authored as three separate constraint stories rather than one story with a variable ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
