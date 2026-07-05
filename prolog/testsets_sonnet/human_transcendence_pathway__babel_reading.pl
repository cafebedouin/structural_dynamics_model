% ============================================================================
% CONSTRAINT STORY: human_transcendence_pathway__babel_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_human_transcendence_pathway__babel_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: human_transcendence_pathway__babel_reading
 *   human_readable: Babel Reading: Unified Techno-Linguistic Power as Self-Sufficient Substitute for Transcendence
 *   domain: religious/political/technological
 *
 * SUMMARY:
 *   This story instantiates the Babel reading of the
 *   human_transcendence_pathway kernel: the claim that collective human
 *   power, exercised through a unified technological and linguistic system,
 *   can secure stability and self-sufficiency without any reference to
 *   transcendent authority. Structurally, this reading requires enforced
 *   uniformity — one language, one technique, one command structure —
 *   presented as the coordination solution to the problem of dispersal and
 *   vulnerability. The coordination story (a shared tongue and shared method
 *   genuinely permit large-scale building and mutual defense) is real, but it
 *   is yoked to an asymmetric extraction: architects and administrators
 *   capture the prestige and command authority the unification produces,
 *   while conscripted laborers bear the physical cost and dispersed
 *   communities lose their languages and local forms of life to make the
 *   system legible and controllable. Suppression is high and rising because
 *   the uniformity does not hold on its own; it requires continuous
 *   administrative and coercive maintenance to keep alternative languages and
 *   methods from re-emerging. This is a distinct constraint from the sibling
 *   readings — jerusalem_reading (participatory rebuilding under blessing,
 *   integrating plurality into communion) and
 *   technocratic_vs_incarnational_reading (optimization/limit-elimination vs.
 *   grace-in-vulnerability) — each of which has its own epsilon, its own
 *   beneficiary/victim structure, and its own claimed type. They are linked
 *   here via network.affects_constraints, not merged into this file.
 *
 * KEY AGENTS:
 *   - tower_architects: Primary agenda-setter (institutional/arbitrage) — designs and administers the unified project, captures command authority
 *   - centralized_administrative_elite: Beneficiary (powerful/mobile) — staffs the apparatus, gains status from conformity credentialing
 *   - suppressed_language_communities: Primary target (powerless/trapped) — loses linguistic and cultural autonomy to the mandated standard
 *   - conscripted_laborers: Primary target (powerless/trapped) — bears the physical cost of construction
 *   - displaced_local_cultures: Excluded (powerless/trapped) — no voice in what counts as the one legitimate form of collective life
 *   - surrounding_peoples: Analytical observer (moderate/mobile) — inherits the scattering when the coercive core fails
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(human_transcendence_pathway__babel_reading, 0.81).
domain_priors:suppression_score(human_transcendence_pathway__babel_reading, 0.88).
domain_priors:theater_ratio(human_transcendence_pathway__babel_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(human_transcendence_pathway__babel_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(human_transcendence_pathway__babel_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(human_transcendence_pathway__babel_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(human_transcendence_pathway__babel_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(human_transcendence_pathway__babel_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(human_transcendence_pathway__babel_reading, snare).
narrative_ontology:human_readable(human_transcendence_pathway__babel_reading, "Babel Reading: Unified Techno-Linguistic Power as Self-Sufficient Substitute for Transcendence").
narrative_ontology:topic_domain(human_transcendence_pathway__babel_reading, "religious/political/technological").

domain_priors:requires_active_enforcement(human_transcendence_pathway__babel_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(human_transcendence_pathway__babel_reading, 'c5641e29-0df4-4bab-893d-fe7d5456bc25').
narrative_ontology:cs_kernel_codification('c5641e29-0df4-4bab-893d-fe7d5456bc25', fixed_text).
narrative_ontology:cs_authority_grounding('c5641e29-0df4-4bab-893d-fe7d5456bc25', extraction).
narrative_ontology:cs_interpretation_layer_present('c5641e29-0df4-4bab-893d-fe7d5456bc25').
narrative_ontology:cs_reading_relation('c5641e29-0df4-4bab-893d-fe7d5456bc25', human_transcendence_pathway__jerusalem_reading, forecloses).
narrative_ontology:cs_reading_relation('c5641e29-0df4-4bab-893d-fe7d5456bc25', human_transcendence_pathway__technocratic_vs_incarnational_reading, influences).
narrative_ontology:cs_axiom('c5641e29-0df4-4bab-893d-fe7d5456bc25', foundational, self_sufficiency_without_transcendent_reference_achievable).
narrative_ontology:cs_axiom_status(self_sufficiency_without_transcendent_reference_achievable, holdable).
narrative_ontology:cs_axiom_grounding('c5641e29-0df4-4bab-893d-fe7d5456bc25', self_sufficiency_without_transcendent_reference_achievable, empirically_contingent).
narrative_ontology:cs_axiom('c5641e29-0df4-4bab-893d-fe7d5456bc25', foundational, uniformity_is_precondition_for_collective_security).
narrative_ontology:cs_axiom_status(uniformity_is_precondition_for_collective_security, holdable).
narrative_ontology:cs_axiom_grounding('c5641e29-0df4-4bab-893d-fe7d5456bc25', uniformity_is_precondition_for_collective_security, instrumental).
narrative_ontology:cs_reference_frame('c5641e29-0df4-4bab-893d-fe7d5456bc25', post_diluvian_unified_settlement).
narrative_ontology:cs_drift_state('c5641e29-0df4-4bab-893d-fe7d5456bc25', post_dispersal_scattering, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('c5641e29-0df4-4bab-893d-fe7d5456bc25', '').
narrative_ontology:cs_kernel_id(human_transcendence_pathway__babel_reading, human_transcendence_pathway).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__babel_reading, tower_architects).
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__babel_reading, centralized_administrative_elite).
narrative_ontology:constraint_victim(human_transcendence_pathway__babel_reading, suppressed_language_communities).
narrative_ontology:constraint_victim(human_transcendence_pathway__babel_reading, displaced_local_cultures).
narrative_ontology:constraint_victim(human_transcendence_pathway__babel_reading, conscripted_laborers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and administer the unified project — one language, one technical standard, one command structure — and present it as the guarantee of collective security and self-sufficiency. They set what counts as legitimate speech and technique, and they capture the prestige, resources, and command authority the unification generates.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__babel_reading, tower_architects, agenda_setter,
    institutional, generational, arbitrage, regional).

% Staff the bureaucratic and technical apparatus that keeps the single system running. They benefit from the premium placed on conformity to the unified standard, since fluency in it is the credential that grants them status and access; they have little incentive to preserve alternative languages or methods that would dilute their position.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__babel_reading, centralized_administrative_elite, beneficiary,
    powerful, biographical, mobile, regional).

% Their native tongues and local knowledge systems are displaced by the mandated common language and technique. Continued participation in the collective project requires abandoning the linguistic and cultural forms that carried their distinct identity; refusal means exclusion from the security and provisioning the unified system controls.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__babel_reading, suppressed_language_communities, payer,
    powerless, generational, trapped, regional).

% Provide the physical labor that raises the tower and maintains the unified infrastructure. They bear the immediate bodily cost of the project's ambition — brick-making, hauling, building — while the strategic and reputational gains accrue to the architects who direct them.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__babel_reading, conscripted_laborers, payer,
    powerless, biographical, trapped, local).

% Practices, rituals, and forms of communal life that do not fit the unified technical-linguistic standard are treated as obstacles to coordination and quietly or forcibly erased. They have no seat in the decisions that define what counts as the one legitimate way of organizing collective life.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__babel_reading, displaced_local_cultures, excluded,
    powerless, generational, trapped, regional).

% Watch the unified project from outside its direct control, neither conscripted into its labor nor folded into its administrative elite. They can observe the brittleness of a stability purchased through enforced sameness, and they inherit the scattered peoples and fragmented languages once the project's coercive core fails.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__babel_reading, surrounding_peoples, observer,
    moderate, generational, mobile, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(human_transcendence_pathway__babel_reading, tower_architects).
narrative_ontology:fixing_cost_class(human_transcendence_pathway__babel_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A single language and shared technical standard genuinely solve a real coordination problem: large-scale construction, resource pooling, and mutual defense require common communication and interoperable method.
% TRANSFER_FUNCTION: Moves cultural, linguistic, and physical autonomy from dispersed communities and laborers to a centralized architectural and administrative core, in exchange for inclusion in a promised collective security that is administered, not shared, by that core.
% ABSENT_VOICES: The communities whose languages and local practices are suppressed to make the unified system work are not present in the decision to unify; their objection — that plurality is not simply an obstacle to coordination but a good in itself — is structurally excluded from the project's own self-description.
% DISAPPEARANCE_RATIONALE: If the enforced uniformity collapsed, the coordination advantage it purchased would disappear immediately along with the coercive apparatus maintaining it; construction would halt, administrative command would fragment, and communities would revert to or reconstitute their own languages and local forms of organization — precisely the scattering the narrative reports as the consequence of the project's failure.
% FOUNDING_PROBLEM: A dispersed human population sought protection against future catastrophe (recurrence of deluge-scale destruction) and against the vulnerability of scattering, by building a name, a city, and a tower whose top would reach the heavens — a self-secured permanence requiring no dependence on any authority beyond the collective itself.
% FOUNDING_PROBLEM_CORROBORATION: The architects and administrative elite attest the project as necessary self-preservation and civilizational achievement. The narrative's own outside verdict — attributed to divine judgment on the presumption of self-sufficient unity — and the subsequent testimony of the scattered communities (their persistence in distinct languages after the collapse) corroborate, from outside the benefiting parties, that the founding problem was never actually solved by uniformity and that the coercive unification was itself the deeper problem.
narrative_ontology:disappearance_verdict(human_transcendence_pathway__babel_reading, world_rearranges).
narrative_ontology:founding_problem_status(human_transcendence_pathway__babel_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(human_transcendence_pathway__babel_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(human_transcendence_pathway__babel_reading, 'none', 1).
narrative_ontology:epsilon_provenance(human_transcendence_pathway__babel_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(human_transcendence_pathway__babel_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(human_transcendence_pathway__babel_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(human_transcendence_pathway__babel_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.81) and rising over the interval because the unification's coordination benefit is increasingly outweighed by the concentration of command and prestige in the architect/administrator seats, at growing cost to laborers and suppressed communities. Suppression is authored even higher (0.88) and rising faster, because uniformity of language and technique does not persist voluntarily once dispersal pressure exists — it requires escalating administrative and coercive maintenance (standardization enforcement, displacement of competing practices) to hold. Theater ratio rises moderately (0.42) reflecting that some of the late-stage 'unification' activity becomes performative consolidation of authority rather than functional coordination gain. All three series share one time grid across the interval.
 *
 * DIRECTIONALITY LOGIC:
 *   Tower architects and the administrative elite are structural beneficiaries: they set the terms of the unified standard and their positions are constituted by fluency in and control of that standard, giving them low d (subsidized by the constraint). Suppressed language communities and conscripted laborers are structural targets: trapped exit options and generational/biographical time horizons under a system whose entire justification (self-sufficient stability) depends on their conformity, giving them high d. Displaced local cultures are excluded rather than coordinated — their erasure is not incidental cost but the mechanism by which uniformity is achieved.
 *
 * MANDATROPHY ANALYSIS:
 *   The Babel reading resists being mislabeled as pure coordination (a rope) precisely because the coordination function, while real, is inseparable from an asymmetric extraction that requires active enforcement to sustain — this is the tangled_rope/snare boundary the framework exists to detect. It equally resists being read as a mountain: the metrics show real, contestable resistance (0.58) and incomplete accessibility collapse (0.62), meaning alternative languages and local practices persist underground even under suppression — this is not an irreducible natural law but a constructed, actively defended arrangement whose claimed self-sufficiency is the very premise the narrative's outside verdict falsifies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    babel_naturalness_vs_construction,
    'Is the drive toward unified techno-linguistic self-sufficiency a natural feature of large-scale human coordination (and therefore something like an inevitable stage), or is it a constructed political choice that could have taken a pluralist form instead?',
    'Comparative institutional history: examine whether large-scale coordination projects that did NOT suppress linguistic/cultural plurality achieved comparable stability and self-sufficiency outcomes.',
    'If pluralist coordination achieves comparable outcomes, the uniformity requirement is exposed as a constructed extraction mechanism rather than a coordination necessity, strengthening the snare/tangled_rope reading over any mountain-adjacent naturalization of the pattern.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(babel_naturalness_vs_construction, conceptual, 'Whether enforced uniformity is coordination-necessary or a constructed extraction mechanism.').

omega_variable(
    kernel_reading_selection_criterion,
    'What determines which reading of the human_transcendence_pathway kernel (babel, jerusalem, technocratic/incarnational) an interpreter selects, and is that selection itself theologically or politically motivated?',
    'Trace how each reading is invoked in Catholic Social Doctrine commentary and technology-ethics literature — whether the babel_reading is typically invoked specifically to criticize centralizing technological projects (a diagnostic use) versus used descriptively.',
    'If the babel_reading is primarily a diagnostic/critical framework applied retrospectively to condemn specific projects, its epsilon and victim structure may be partly a function of which historical episodes get labeled ''Babel'' versus ''Jerusalem'' rather than a stable structural fact independent of the labeling act.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_criterion, conceptual, 'Whether reading-selection among kernel siblings is itself a contested, motivated act.').

omega_variable(
    collapse_mechanism_ambiguity,
    'Does the unified system fail because of an external/transcendent intervention (as the narrative states), or does it fail from purely internal structural causes (administrative overreach, suppressed resistance resurfacing, coordination costs exceeding capacity)?',
    'Structural analysis of comparable historical unification projects that collapsed without any claimed transcendent intervention, to see whether the same failure pattern (linguistic/cultural fragmentation following collapse of centralized coercive capacity) recurs on purely secular causal grounds.',
    'If the collapse pattern recurs secularly, the babel_reading''s implicit claim that self-sufficiency without transcendent reference is inherently unstable gains empirical support independent of theological framing; if it does not recur, the theological framing is doing more interpretive work than the structural facts warrant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collapse_mechanism_ambiguity, empirical, 'Whether the project''s collapse is best explained by internal structural fragility or requires an external/transcendent causal account.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(human_transcendence_pathway__babel_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, human_transcendence_pathway__babel_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(huma_tr_t8, human_transcendence_pathway__babel_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement(huma_tr_t16, human_transcendence_pathway__babel_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement(huma_tr_t24, human_transcendence_pathway__babel_reading, theater_ratio, 24, 0.36).
narrative_ontology:measurement(huma_tr_t32, human_transcendence_pathway__babel_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement(huma_tr_t40, human_transcendence_pathway__babel_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, human_transcendence_pathway__babel_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(huma_be_t8, human_transcendence_pathway__babel_reading, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(huma_be_t16, human_transcendence_pathway__babel_reading, base_extractiveness, 16, 0.65).
narrative_ontology:measurement(huma_be_t24, human_transcendence_pathway__babel_reading, base_extractiveness, 24, 0.73).
narrative_ontology:measurement(huma_be_t32, human_transcendence_pathway__babel_reading, base_extractiveness, 32, 0.78).
narrative_ontology:measurement(huma_be_t40, human_transcendence_pathway__babel_reading, base_extractiveness, 40, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t0, human_transcendence_pathway__babel_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(huma_su_t8, human_transcendence_pathway__babel_reading, suppression_requirement, 8, 0.66).
narrative_ontology:measurement(huma_su_t16, human_transcendence_pathway__babel_reading, suppression_requirement, 16, 0.75).
narrative_ontology:measurement(huma_su_t24, human_transcendence_pathway__babel_reading, suppression_requirement, 24, 0.81).
narrative_ontology:measurement(huma_su_t32, human_transcendence_pathway__babel_reading, suppression_requirement, 32, 0.85).
narrative_ontology:measurement(huma_su_t40, human_transcendence_pathway__babel_reading, suppression_requirement, 40, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(human_transcendence_pathway__babel_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(human_transcendence_pathway__babel_reading, 0.08).
narrative_ontology:affects_constraint(human_transcendence_pathway__babel_reading, human_transcendence_pathway__jerusalem_reading).
narrative_ontology:affects_constraint(human_transcendence_pathway__babel_reading, human_transcendence_pathway__technocratic_vs_incarnational_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the human_transcendence_pathway kernel. babel_reading claims collective self-sufficiency through enforced uniformity (high epsilon, snare-adjacent, victims = suppressed language/culture communities). jerusalem_reading claims authentic community rebuilt through participatory labor integrating plurality into communion (expected low-moderate epsilon, rope-adjacent). technocratic_vs_incarnational_reading contrasts limit-elimination transcendence against grace-received-in-vulnerability (a bifurcated reading with its own internal contest). Each reading is authored as its own constraint file with its own epsilon and stakeholder structure; this file's network edges record the kernel-level linkage without merging their classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
