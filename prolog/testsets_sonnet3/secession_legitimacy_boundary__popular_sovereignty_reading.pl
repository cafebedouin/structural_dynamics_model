% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__popular_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__popular_sovereignty_reading, []).

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
 *   constraint_id: secession_legitimacy_boundary__popular_sovereignty_reading
 *   human_readable: Popular-Sovereignty Reading of the Secession Legitimacy Boundary
 *   domain: political/federalism/resource_politics
 *
 * SUMMARY:
 *   This story instantiates the popular-sovereignty reading of the secession
 *   legitimacy boundary kernel: the claim that a provincial referendum
 *   majority is, by itself, a sufficient and self-executing act of sovereign
 *   will, independent of federal constitutional process, negotiated grievance
 *   thresholds, or pre-existing treaty consent. Under this reading, the
 *   provincial electorate is treated as the exhaustive relevant demos —
 *   federal authority, sub-provincial minority majorities, and indigenous
 *   treaty nations are all structurally external to the legitimating act. The
 *   reading genuinely solves a coordination problem (aggregating a contested
 *   population's preference into one legible, actionable decision) but does
 *   so by asymmetrically transferring sovereign authority and resource
 *   control to the secessionist majority and the party apparatus
 *   administering the vote, at the expense of parties who had no vote and
 *   whose objections the reading treats as illegitimate by construction. This
 *   is a distinct constraint from its sibling readings
 *   (constitutional_impossibility_reading, grievance_threshold_reading,
 *   treaty_primacy_reading) — each has its own epsilon, its own
 *   beneficiary/victim structure, and its own classification; they are not
 *   measurement variants of one underlying fact but structurally different
 *   claims about what makes secession legitimate.
 *
 * KEY AGENTS:
 *   - provincial_secessionist_majority: primary beneficiary (organized/mobile) — gains sovereignty and resource control from the referendum's self-legitimating force
 *   - provincial_governing_party: agenda_setter (institutional/mobile) — administers and declares the referendum binding
 *   - provincial_minority_opposed_to_secession: payer (moderate/constrained) — bound by a result they voted against
 *   - indigenous_nations_within_provincial_boundaries: excluded (organized/trapped) — treaty consent bypassed by the boundary framing
 *   - federal_government: excluded (institutional/constrained) — treated as subordinate to provincial will
 *   - constitutional_and_international_law_scholars: analytical observer — assesses recognition standards
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__popular_sovereignty_reading, 0.58).
domain_priors:suppression_score(secession_legitimacy_boundary__popular_sovereignty_reading, 0.62).
domain_priors:theater_ratio(secession_legitimacy_boundary__popular_sovereignty_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__popular_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__popular_sovereignty_reading, "Popular-Sovereignty Reading of the Secession Legitimacy Boundary").
narrative_ontology:topic_domain(secession_legitimacy_boundary__popular_sovereignty_reading, "political/federalism/resource_politics").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__popular_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__popular_sovereignty_reading, '2bfde08c-cb7e-4697-9c0e-3e6ac59dfd59').
narrative_ontology:cs_kernel_codification('2bfde08c-cb7e-4697-9c0e-3e6ac59dfd59', distributed).
narrative_ontology:cs_authority_grounding('2bfde08c-cb7e-4697-9c0e-3e6ac59dfd59', distributed).
narrative_ontology:cs_reading_relation('2bfde08c-cb7e-4697-9c0e-3e6ac59dfd59', secession_legitimacy_boundary__constitutional_impossibility_reading, forecloses).
narrative_ontology:cs_reading_relation('2bfde08c-cb7e-4697-9c0e-3e6ac59dfd59', secession_legitimacy_boundary__grievance_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('2bfde08c-cb7e-4697-9c0e-3e6ac59dfd59', secession_legitimacy_boundary__treaty_primacy_reading, influences).
narrative_ontology:cs_axiom('2bfde08c-cb7e-4697-9c0e-3e6ac59dfd59', foundational, provincial_majority_is_ultimate_sovereign).
narrative_ontology:cs_axiom_status(provincial_majority_is_ultimate_sovereign, holdable).
narrative_ontology:cs_axiom_grounding('2bfde08c-cb7e-4697-9c0e-3e6ac59dfd59', provincial_majority_is_ultimate_sovereign, deontological).
narrative_ontology:cs_axiom('2bfde08c-cb7e-4697-9c0e-3e6ac59dfd59', secondary, referendum_result_requires_no_external_ratification).
narrative_ontology:cs_axiom_status(referendum_result_requires_no_external_ratification, holdable).
narrative_ontology:cs_axiom_grounding('2bfde08c-cb7e-4697-9c0e-3e6ac59dfd59', referendum_result_requires_no_external_ratification, conventional).
narrative_ontology:cs_reference_frame('2bfde08c-cb7e-4697-9c0e-3e6ac59dfd59', provincial_electorate_as_sovereign_demos).
narrative_ontology:cs_drift_state('2bfde08c-cb7e-4697-9c0e-3e6ac59dfd59', post_referendum_declaration, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('2bfde08c-cb7e-4697-9c0e-3e6ac59dfd59', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_secessionist_majority).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_governing_party).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_minority_opposed_to_secession).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, federal_taxpayers_outside_province).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, indigenous_nations_within_provincial_boundaries).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, internal_ethnic_and_linguistic_minorities).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__popular_sovereignty_reading, referendum_self_legitimation_doctrine).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__popular_sovereignty_reading, territorial_majoritarianism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Votes yes in a referendum framed as the sole legitimating act for provincial independence. Controls the provincial legislature and referendum machinery, sets the threshold and question wording, and treats a bare majority result as conclusive regardless of federal constitutional process. Gains control over resource revenue, taxation, and border authority currently shared with or subordinate to the federal government.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_secessionist_majority, beneficiary,
    organized, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_secessionist_majority, agenda_setter).

% Administers the referendum, drafts the question, sets turnout and threshold rules, and declares the mandate self-executing. Its political survival and historical legacy are tied to the secession outcome; it has structural incentive to interpret any majority, however narrow, as sufficient sovereignty expression.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_governing_party, agenda_setter,
    institutional, biographical, mobile, regional).

% Voted no or would have, but the referendum's self-legitimating logic treats the majority result as binding on them without further negotiation. Their citizenship, currency, pensions, and legal status change without their consent once the province declares independence on the strength of the vote alone. Exit means relocating out of the province or accepting a new sovereign status they rejected.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_minority_opposed_to_secession, payer,
    moderate, biographical, constrained, regional).

% Bear the fiscal, currency, debt-apportionment, and border-security costs of a unilateral secession they had no vote in and no seat in the province's referendum process. The popular-sovereignty reading treats their exclusion as structurally correct, since sovereignty is scoped to the province, not the federation.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, federal_taxpayers_outside_province, payer,
    moderate, biographical, trapped, national).

% Hold treaty relationships predating the province and the federation itself, but the popular-sovereignty reading counts only the provincial electorate as the sovereign demos, folding indigenous territory into the seceding unit's boundaries without separate consent. Their objection is structurally unrepresented in the referendum's design.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, indigenous_nations_within_provincial_boundaries, excluded,
    organized, civilizational, trapped, regional).

% Concentrated in border regions or enclaves within the province, they voted against secession in higher proportion but are bound by the aggregate provincial result. Their own sub-provincial majorities carry no legitimating weight under this reading, which recognizes only the provincial boundary as the relevant demos.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, internal_ethnic_and_linguistic_minorities, payer,
    powerless, biographical, constrained, regional).

% Holds constitutional authority over amendment and territorial integrity but is treated, under this reading, as subordinate to the provincial referendum's self-declared mandate. Its objections and legal processes are characterized as illegitimate impositions on popular will rather than as a competing sovereign claim.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, federal_government, excluded,
    institutional, generational, constrained, national).

% Assess whether referendum results alone satisfy recognized standards for self-determination under international law, and whether the popular-sovereignty reading's disregard of federal process and minority consent would be recognized by other states.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, constitutional_and_international_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_secessionist_majority).
narrative_ontology:fixing_cost_class(secession_legitimacy_boundary__popular_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, legible decision procedure for a genuinely contested political question — whether a defined population wants to remain within a larger federation — by aggregating preferences through a single binding vote, avoiding indefinite negotiation paralysis.
% TRANSFER_FUNCTION: Moves sovereign authority, resource revenue, taxation power, and citizenship status from the federal level and from provincial minorities to the secessionist majority and the party apparatus that administers the referendum, on the strength of an aggregate vote count that treats the provincial boundary as the exhaustive relevant demos.
% ABSENT_VOICES: Indigenous nations whose treaty territories are absorbed into the seceding unit without separate consent; sub-provincial minority enclaves whose local majorities are outvoted by the aggregate; federal taxpayers and citizens outside the province who bear fiscal and border costs with no vote. All are structurally outside the referendum's electorate by the reading's own design.
% DISAPPEARANCE_RATIONALE: If the popular-sovereignty reading disappeared as a legitimating frame, secessionist movements would need to secure negotiated exit through constitutional amendment or demonstrate a grievance threshold — the provincial governing party's mandate would lose immediate self-executing force, and indigenous and minority consent would likely become a required precondition rather than an excluded consideration. Secessionist proponents dispute this, holding that popular sovereignty is prior to and independent of constitutional recognition.
% FOUNDING_PROBLEM: Addressing a province's claim that federal governance systematically disregards its distinct political will, and providing a legible mechanism (a vote) for expressing and acting on that will without requiring the federation's own consent to the process.
% FOUNDING_PROBLEM_CORROBORATION: The provincial governing party and secessionist movement attest the founding problem — federal disregard for provincial will — remains live and unresolved by ordinary federal process. Federal constitutional scholars, indigenous treaty nations, and international law commentators outside the secessionist coalition attest the reading substitutes a majoritarian procedural shortcut for the harder, better-evidenced questions of treaty consent and minority protection, and that no external body has validated referendum results alone as sufficient for recognized self-determination in comparable cases.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__popular_sovereignty_reading, contested).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__popular_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__popular_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(secession_legitimacy_boundary__popular_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__popular_sovereignty_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__popular_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(secession_legitimacy_boundary__popular_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(secession_legitimacy_boundary__popular_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.58 (moderate-high, not maximal) because the coordination function is real — a referendum genuinely aggregates preference more legibly than indefinite negotiation — but the reading's insistence that the provincial boundary alone defines the demos transfers real costs onto excluded parties (federal taxpayers, treaty nations, internal minorities) who had no vote. Suppression is authored at 0.62: the reading requires active insistence that federal objection and treaty claims are illegitimate impositions, which is a suppressive posture toward competing legitimacy claims, not passive coordination. Accessibility collapse is moderate (0.45) — alternative legitimating frames (negotiated amendment, grievance threshold, treaty primacy) remain visibly available and contested, so alternatives have not collapsed the way they would under a genuine natural-law mountain. Resistance is high (0.70) because federal governments, treaty nations, and international law bodies actively contest the sufficiency of a bare referendum result. The temporal series models an secessionist movement's mandate hardening over a 40-unit interval as the governing party escalates from proposing a referendum to treating its result as self-executing and increasingly suppressing federal and treaty counter-claims.
 *
 * PERSPECTIVAL GAP:
 *   From the provincial secessionist majority's seat, this is a rope: a clean coordination mechanism resolving a real self-determination question with minimal coercive overhead. From the seat of excluded treaty nations, outvoted minority enclaves, and non-consenting federal taxpayers, the same referendum operates as an enforced transfer dressed in majoritarian legitimacy language — the engine's computed divergence between these seats is the analytical content of this story, not an error to be reconciled.
 *
 * DIRECTIONALITY LOGIC:
 *   The provincial secessionist majority and the governing party administering the vote sit near the beneficiary end: they collect sovereign authority and resource control, and they control the referendum's design and interpretation. Provincial minorities, federal taxpayers outside the province, indigenous treaty nations, and internal sub-provincial minorities sit near the target end: each bears costs (status change, fiscal burden, territorial absorption, outvoted local will) imposed by a legitimating act in which they had no effective vote — trapped or constrained exit reinforces this. Indigenous nations and the federal government are marked 'excluded' rather than merely 'payer' because the reading's structural design treats their objection as outside the legitimating conversation entirely, not merely outvoted within it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — federal disregard for a province's distinct political will — may remain genuinely live in some cases even as the specific mechanism (bare referendum self-legitimation) outruns its justification by expanding to bind parties never in its electorate. Classifying this as tangled_rope rather than snare preserves the real coordination function (aggregating a contested population's will legibly) while still registering the asymmetric extraction from excluded and outvoted parties — collapsing it to pure snare would erase the genuine self-determination grievance driving the movement; calling it pure rope would erase the treaty and minority costs the reading structurally externalizes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demos_boundary_selection,
    'Why is the provincial boundary, rather than the federal boundary, a sub-provincial region, or a treaty territory, the correct scope for the legitimating demos?',
    'Comparative analysis of how international law and prior secession precedents (Quebec, Catalonia, Scotland, South Sudan) have adjudicated demos-scope disputes, and whether any principled non-question-begging criterion selects the provincial boundary specifically.',
    'If no principled criterion exists, the popular-sovereignty reading''s boundary selection is itself an act of the secessionist coalition''s political convenience rather than a neutral sovereignty fact — this would raise the reading''s effective extraction and lower confidence in its self-legitimation claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demos_boundary_selection, conceptual, 'Whether the provincial boundary is a principled or a question-begging choice of demos.').

omega_variable(
    referendum_threshold_legitimacy,
    'Does a bare majority (50%+1) referendum result carry the same self-legitimating force as a supermajority or a result with entrenched minority safeguards, under this reading''s own logic?',
    'Examine whether the reading''s proponents accept lower thresholds when politically convenient and higher thresholds when inconvenient — a test of principled versus outcome-driven threshold selection.',
    'Inconsistent threshold application would indicate the self-legitimation claim is doing rhetorical work for a predetermined outcome rather than expressing a stable normative principle, increasing measured theater_ratio and suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(referendum_threshold_legitimacy, empirical, 'Whether the reading applies referendum thresholds consistently or opportunistically.').

omega_variable(
    committer_framing_alternative,
    'Is the kernel better framed as a single contested legitimacy question with multiple readings (as authored here), or as several genuinely independent legal questions (constitutional procedure, moral grievance, democratic mandate, treaty consent) that only appear unified under the colloquial label ''secession legitimacy''?',
    'Legal and political-theory analysis of whether courts and international bodies treat these as one adjudicable question or as separable doctrinal tracks with different evidentiary standards and different deciding bodies.',
    'If the four readings are genuinely separable doctrinal tracks rather than competing answers to one question, the kernel model itself may need further decomposition rather than four sibling readings of one kernel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_framing_alternative, conceptual, 'Whether the four-reading kernel structure is the right level of decomposition for this contest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__popular_sovereignty_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t0, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(sece_tr_t8, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 8, 0.21).
narrative_ontology:measurement(sece_tr_t16, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement(sece_tr_t24, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 24, 0.27).
narrative_ontology:measurement(sece_tr_t32, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 32, 0.29).
narrative_ontology:measurement(sece_tr_t40, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(sece_be_t0, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sece_be_t8, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(sece_be_t16, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 16, 0.48).
narrative_ontology:measurement(sece_be_t24, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 24, 0.53).
narrative_ontology:measurement(sece_be_t32, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(sece_be_t40, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t0, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(sece_su_t8, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 8, 0.47).
narrative_ontology:measurement(sece_su_t16, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 16, 0.53).
narrative_ontology:measurement(sece_su_t24, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 24, 0.58).
narrative_ontology:measurement(sece_su_t32, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 32, 0.6).
narrative_ontology:measurement(sece_su_t40, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__popular_sovereignty_reading, identity_coordination).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary__constitutional_impossibility_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary__grievance_threshold_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary__treaty_primacy_reading).

% DUAL FORMULATION NOTE:
% Four sibling stories decompose the colloquial 'secession legitimacy' question per the kernel-reading framework: constitutional_impossibility_reading treats unilateral exit as impermissible absent amendment; grievance_threshold_reading conditions legitimacy on a substantive injustice threshold; popular_sovereignty_reading (this story) treats a bare provincial referendum as self-legitimating; treaty_primacy_reading subordinates all three to indigenous treaty consent. Each carries its own epsilon, beneficiary/victim structure, and classification — they are linked, not merged, and the differences in extraction and victim sets across the four are the analytical content, not noise to be averaged out.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(secession_legitimacy_boundary__popular_sovereignty_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
