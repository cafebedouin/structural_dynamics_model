% ============================================================================
% CONSTRAINT STORY: unranked_substrate_as_negative_commons
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unranked_substrate_as_negative_commons, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: unranked_substrate_as_negative_commons
 *   human_readable: The Unranked Substrate as Negative Commons
 *   domain: bureaucratic/institutional/social
 *
 * SUMMARY:
 *   Upstream, categorical_nonexistence_as_soft_denial describes the
 *   tangled-rope mechanism by which the widower is administratively 'fused'
 *   out of individual standing and the orphan is rendered 'uncordable' — both
 *   denied not through explicit rejection but through the absence of a
 *   category that would let the system see them at all. This story describes
 *   what happens downstream of that denial: an ungated, unlit, untiered
 *   physical space (the bedrock) that no agency claims, where victims of
 *   unrelated foreclosure mechanisms co-locate. Crucially, they do not
 *   exchange the diagnostic information that would let either repair the
 *   other's exclusion — the widower cannot fix the orphan's guardianship
 *   chain, the orphan cannot unfuse the widower's household code. What they
 *   share is purely structural: neither of them is weighed by any system
 *   operating nearby. This is solidarity constituted by mutual illegibility,
 *   not by mutual understanding or mutual aid. The claim is rope: this is a
 *   low-coercion, low-extraction space that solves a real (if minimal)
 *   coordination problem — somewhere to be, unranked — without suppressing
 *   alternatives or requiring enforcement.
 *
 * KEY AGENTS:
 *   - fused_widower: Primary beneficiary (powerless/trapped) — uses the space as refuge from a categorization scheme that erased his distinct standing
 *   - uncordable_orphan: Primary beneficiary (powerless/trapped) — uses the space as refuge from a guardianship-linking failure with no schema fit
 *   - ranking_authority: Excluded by its own absence — has no jurisdiction over, and no operational interest in, a space its instruments cannot grade
 *   - caseworkers_and_auditors: Analytical observer — sees the pattern but has no mandate to act on it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unranked_substrate_as_negative_commons, 0.12).
domain_priors:suppression_score(unranked_substrate_as_negative_commons, 0.08).
domain_priors:theater_ratio(unranked_substrate_as_negative_commons, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unranked_substrate_as_negative_commons, extractiveness, 0.12).
narrative_ontology:constraint_metric(unranked_substrate_as_negative_commons, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(unranked_substrate_as_negative_commons, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unranked_substrate_as_negative_commons, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(unranked_substrate_as_negative_commons, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unranked_substrate_as_negative_commons, rope).
narrative_ontology:human_readable(unranked_substrate_as_negative_commons, "The Unranked Substrate as Negative Commons").
narrative_ontology:topic_domain(unranked_substrate_as_negative_commons, "bureaucratic/institutional/social").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unranked_substrate_as_negative_commons, fused_widower).
narrative_ontology:constraint_beneficiary(unranked_substrate_as_negative_commons, uncordable_orphan).
narrative_ontology:constraint_beneficiary(unranked_substrate_as_negative_commons, other_unweighed_residents).
narrative_ontology:constraint_vindicates(unranked_substrate_as_negative_commons, solidarity_without_recognition_is_possible).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Was folded into a household categorization scheme after his spouse's death, algorithmically 'fused' into a household unit that no longer legibly represents his actual situation to any assistance program. In the ranking system upstream he is invisible as a distinct claimant. Down here, on the bedrock — an ungated, unlit patch of ground behind the processing center that no agency claims jurisdiction over — he sits near others in the same non-status without needing to explain it. No one here can fix his paperwork. No one tries.
narrative_ontology:constraint_stakeholder(unranked_substrate_as_negative_commons, fused_widower, beneficiary,
    powerless, biographical, trapped, local).

% A minor whose guardianship chain broke in a way the intake schema has no field for — she cannot be 'corded' (linked) to any case file because the linking logic requires a category she does not fit. She is administratively unlocatable. She comes to the same unlit ground the widower frequents. They do not exchange names, causes, or remedies. They share only the fact that neither of them registers on any system's scale.
narrative_ontology:constraint_stakeholder(unranked_substrate_as_negative_commons, uncordable_orphan, beneficiary,
    powerless, biographical, trapped, local).

% The agencies and scoring systems that produced both exclusions never enter the bedrock — it is defined by their absence, not their presence. They have no operational reason to survey a space their own instruments cannot legibly grade, so they neither administer nor contest it. Their absence is what makes the space possible.
narrative_ontology:constraint_stakeholder(unranked_substrate_as_negative_commons, ranking_authority, excluded,
    institutional, generational, analytical, national).

% Occasionally pass near the bedrock in the course of other duties. They recognize informally that 'people who don't fit anywhere end up there' but have no mandate, budget line, or intake category that would let them process what they see. They neither disturb it nor resource it.
narrative_ontology:constraint_stakeholder(unranked_substrate_as_negative_commons, caseworkers_and_auditors, observer,
    moderate, immediate, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unranked_substrate_as_negative_commons, diffuse).
narrative_ontology:fixing_cost_class(unranked_substrate_as_negative_commons, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The bedrock provides a literal and social space where people excluded from every legibility system can co-locate without any requirement to translate their situation into terms a system would recognize. The coordination it solves is minimal but real: the problem of having nowhere to simply be while unranked.
% TRANSFER_FUNCTION: Nothing measurable transfers. No resources, no case information, no redress moves between the widower and the orphan or from any institution to them. What is shared is a negative fact — mutual illegibility — not a positive good, service, or recognition.
% ABSENT_VOICES: The ranking authority and the schema designers who created the categories that exclude both figures are entirely absent from the bedrock and have no occasion to hear what happens there. If present, they might either try to formalize the space (destroying its unranked character) or dismiss it as irrelevant since no metric registers activity there.
% DISAPPEARANCE_RATIONALE: From the ranking authority's perspective, nothing would change if the bedrock were paved over or lit and gated — no metric would move, no case count would shift. From the widower's and orphan's perspective, the only space where their non-status is not itself a problem to be solved would vanish, and they would be pushed back into spaces where their illegibility is either forcibly resolved (bad) or forcibly ignored under surveillance (worse). Whether 'the world' rearranges depends entirely on whose world is being asked.
% FOUNDING_PROBLEM: No one built the bedrock to solve anything. It exists because it fell outside every agency's jurisdictional map — a residual, unclaimed patch of ground adjacent to the intake systems that produced the exclusions in the first place. Its coordination function (a place for the unweighed to co-locate) emerged as a side effect of institutional boundary-drawing, not as a designed remedy.
% FOUNDING_PROBLEM_CORROBORATION: Neither the widower nor the orphan asserts the bedrock 'solves' their exclusion — they describe it only as somewhere they can be without being processed. Caseworkers who pass nearby corroborate informally that the space exists and that people accumulate there, without claiming any institutional role in it; no ranking authority representative has ever been asked and none has offered an account, which is itself the corroborating absence.
narrative_ontology:disappearance_verdict(unranked_substrate_as_negative_commons, contested).
narrative_ontology:founding_problem_status(unranked_substrate_as_negative_commons, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unranked_substrate_as_negative_commons, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-17',
    'uke_narrative', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'temperature=0.2,max_tokens=8192').
narrative_ontology:story_seed(unranked_substrate_as_negative_commons, 'none', 1).
narrative_ontology:epsilon_provenance(unranked_substrate_as_negative_commons, 0.12, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unranked_substrate_as_negative_commons_tests).
:- end_tests(unranked_substrate_as_negative_commons_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.12) and essentially flat over the interval because nothing is extracted from anyone by the bedrock's existence — no rent, no compliance labor, no data harvested. Suppression is low (0.08) because no one is coerced into using or avoiding the space; it is simply unadministered. Theater ratio is near zero and stable because there is no performative maintenance — no agency stages anything there, because no agency acknowledges it exists. Accessibility collapse is moderate-low (0.2): alternatives to the bedrock (other unranked spaces) are not systematically foreclosed, they are simply rare by construction of the surrounding legibility systems. Resistance is low (0.15) because nothing resists the bedrock's existence — it persists by neglect, not by anyone defending it.
 *
 * PERSPECTIVAL GAP:
 *   From the ranking authority's seat, the bedrock is not merely low-priority — it is definitionally invisible, since any instrument that could grade it would eliminate the property that makes it what it is. From the widower's and orphan's seats, it is one of the only spaces where their unweighed status is not itself an active problem. The gap is total: one seat cannot even perceive the object the other seat depends on.
 *
 * DIRECTIONALITY LOGIC:
 *   Both named beneficiaries are declared powerless with trapped exit — but the exit metric here is inverted from the usual extraction reading: their 'trapped' status refers to their trapped exclusion from the ranking system generally, not to entrapment within the bedrock itself, which is freely enterable and freely leaveable. The bedrock does not extract from them; it is the one place their entrapment (upstream, in the ranking systems) is not additionally taxed. There is no victim group because no one bears a cost through the bedrock's operation — the costs were already imposed upstream by the categorization failures. This is why victims[] is empty and the constraint is authored as rope rather than tangled_rope: it has a genuine, if minimal, coordination function and no identifiable party paying through THIS structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem interview deliberately returns 'live' with a thin corroboration: no one built the bedrock to solve anything, so there is no original mandate to have outlived. This blocks a mandatrophy misreading in the other direction too — one might expect an under-administered residual space to be flagged as a decayed institution (piton), but there was never an institution here to decay. Classifying it as rope rather than piton turns on this: a piton is an atrophied FORMER function; the bedrock never had a former function to atrophy. It is native low-extraction coordination that emerged from the boundary-drawing of adjacent institutions, not their residue.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    solidarity_without_information_exchange,
    'Does co-location without information exchange constitute genuine coordination (rope), or is it merely the absence of a system rather than a positive coordination good at all?',
    'Longitudinal interviews with bedrock users on whether the co-presence changes their subjective experience of exclusion, compared to matched excluded individuals with no equivalent unranked space available.',
    'If co-presence measurably reduces psychological cost of exclusion without any information exchange, the rope classification is well-grounded as a minimal but real coordination function. If it produces no measurable effect, the ''coordination function'' may be a retrospective narrative imposed on pure absence, and the constraint might better be modeled as a non-constraint (a null category) rather than a rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(solidarity_without_information_exchange, conceptual, 'Whether mutual illegibility, without information exchange, constitutes a coordination good.').

omega_variable(
    formalization_destroys_the_good,
    'If any agency attempted to formalize, resource, or administratively recognize the bedrock, would the coordination function survive?',
    'Compare outcomes in jurisdictions where similar informal unranked spaces have been formalized (turned into shelters, drop-in centers, or monitored zones) versus those left unadministered.',
    'If formalization destroys the function (because being weighed by even a benevolent system reintroduces the ranking logic the space exists outside of), then any well-intentioned policy intervention to ''help'' bedrock users would eliminate the very good it targets — an important caution for downstream policy readings of this constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formalization_destroys_the_good, empirical, 'Whether administrative recognition of the space would eliminate its coordinating property.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unranked_substrate_as_negative_commons, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unra_tr_t0, unranked_substrate_as_negative_commons, theater_ratio, 0, 0.02).
narrative_ontology:measurement(unra_tr_t4, unranked_substrate_as_negative_commons, theater_ratio, 4, 0.03).
narrative_ontology:measurement(unra_tr_t8, unranked_substrate_as_negative_commons, theater_ratio, 8, 0.03).
narrative_ontology:measurement(unra_tr_t12, unranked_substrate_as_negative_commons, theater_ratio, 12, 0.04).
narrative_ontology:measurement(unra_tr_t16, unranked_substrate_as_negative_commons, theater_ratio, 16, 0.04).
narrative_ontology:measurement(unra_tr_t20, unranked_substrate_as_negative_commons, theater_ratio, 20, 0.05).
narrative_ontology:measurement(unra_tr_t24, unranked_substrate_as_negative_commons, theater_ratio, 24, 0.05).

% Extraction over time
narrative_ontology:measurement(unra_be_t0, unranked_substrate_as_negative_commons, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(unra_be_t4, unranked_substrate_as_negative_commons, base_extractiveness, 4, 0.1).
narrative_ontology:measurement(unra_be_t8, unranked_substrate_as_negative_commons, base_extractiveness, 8, 0.11).
narrative_ontology:measurement(unra_be_t12, unranked_substrate_as_negative_commons, base_extractiveness, 12, 0.11).
narrative_ontology:measurement(unra_be_t16, unranked_substrate_as_negative_commons, base_extractiveness, 16, 0.12).
narrative_ontology:measurement(unra_be_t20, unranked_substrate_as_negative_commons, base_extractiveness, 20, 0.12).
narrative_ontology:measurement(unra_be_t24, unranked_substrate_as_negative_commons, base_extractiveness, 24, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(unranked_substrate_as_negative_commons, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unranked_substrate_as_negative_commons, attachment_coordination).
narrative_ontology:boltzmann_floor_override(unranked_substrate_as_negative_commons, 0.05).
narrative_ontology:affects_constraint(unranked_substrate_as_negative_commons, categorical_nonexistence_as_soft_denial).

% DUAL FORMULATION NOTE:
% categorical_nonexistence_as_soft_denial (tangled_rope) is the upstream mechanism: institutional categorization schemes that produce soft denial by omitting a needed category, coordinating administrative simplicity for agencies while extracting standing from those who fall outside the schema (the widower's household-fusion, the orphan's uncordability). unranked_substrate_as_negative_commons is downstream and structurally distinct: it describes what the victims of that upstream mechanism do with each other in the space the mechanism's own boundary-drawing leaves unclaimed. The upstream story's ε is substantial (contested extraction via enforced categorization); this downstream story's ε is low (no extraction occurs in the bedrock itself). They are linked, not identical: one names the injury, the other names an emergent, non-remedial commons that forms in the injury's blind spot.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
