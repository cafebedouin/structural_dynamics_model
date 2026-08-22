% ============================================================================
% CONSTRAINT STORY: state_commitment_installation_mechanism__exogenous_imposition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_commitment_installation_mechanism__exogenous_imposition_reading, []).

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
 *   constraint_id: state_commitment_installation_mechanism__exogenous_imposition_reading
 *   human_readable: State Authority Exogenous Commitment Installation (Top-Down Mandate Reading)
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This reading instantiates a state-centered legitimacy mechanism for new
 *   commitments: the state (holding a transformation mandate) decrees that
 *   certain new institutional commitments will be adopted uniformly, enforces
 *   compliance through coercion and credential gatekeeping, and collects
 *   legitimacy debt from subordinate actors who must absorb adoption costs
 *   without having shaped the decision. The reading contrasts with
 *   endogenous-climb (where superior commitments rise by demonstrated
 *   fitness) and hybrid-cascade (where apex installation requires fringe
 *   validation to stabilize). This reading foregrounds extraction: the state
 *   benefits from unified control; institutional periphery actors and
 *   grassroots constituencies pay by losing autonomy and bearing friction
 *   costs. The constraint is CLAIMED as tangled_rope because it coordinates a
 *   unified commitment field while extracting autonomy from subordinate
 *   actors; the authored metrics describe substantial suppression and rising
 *   extractiveness over the interval, consistent with enforcement
 *   intensification as resistance manifests.
 *
 * KEY AGENTS:
 *   - state_apparatus: institutional beneficiary holding transformative mandate; agenda-setter
 *   - transformation_authority: cadre implementing top-down installation; career incentives; enforces fringe exclusion
 *   - institutional_periphery_actors: moderate-power moderates paying adoption costs; constrained exit
 *   - grassroots_constituencies: powerless payers locked by identity to mandated commitments; internalization costs
 *   - fringe_validators: excluded resource (researchers, practitioners) whose absence is enforced
 *   - competing_legitimacy_frameworks: excluded alternatives (religious/cultural authorities) delegitimized by decree
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.68).
domain_priors:suppression_score(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.72).
domain_priors:theater_ratio(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_commitment_installation_mechanism__exogenous_imposition_reading, tangled_rope).
narrative_ontology:human_readable(state_commitment_installation_mechanism__exogenous_imposition_reading, "State Authority Exogenous Commitment Installation (Top-Down Mandate Reading)").
narrative_ontology:topic_domain(state_commitment_installation_mechanism__exogenous_imposition_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(state_commitment_installation_mechanism__exogenous_imposition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_commitment_installation_mechanism__exogenous_imposition_reading, '980a9324-93b1-48d3-8960-f8fcc26f0129').
narrative_ontology:cs_kernel_codification('980a9324-93b1-48d3-8960-f8fcc26f0129', formalized).
narrative_ontology:cs_authority_grounding('980a9324-93b1-48d3-8960-f8fcc26f0129', extraction).
narrative_ontology:cs_interpretation_layer_present('980a9324-93b1-48d3-8960-f8fcc26f0129').
narrative_ontology:cs_reading_relation('980a9324-93b1-48d3-8960-f8fcc26f0129', state_commitment_installation_mechanism__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('980a9324-93b1-48d3-8960-f8fcc26f0129', state_commitment_installation_mechanism__hybrid_cascade_reading, influences).
narrative_ontology:cs_axiom('980a9324-93b1-48d3-8960-f8fcc26f0129', foundational, state_mandate_suffices_legitimacy).
narrative_ontology:cs_axiom_status(state_mandate_suffices_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('980a9324-93b1-48d3-8960-f8fcc26f0129', state_mandate_suffices_legitimacy, conventional).
narrative_ontology:cs_axiom('980a9324-93b1-48d3-8960-f8fcc26f0129', secondary, fringe_validation_unnecessary_for_authority).
narrative_ontology:cs_axiom_status(fringe_validation_unnecessary_for_authority, holdable).
narrative_ontology:cs_axiom_grounding('980a9324-93b1-48d3-8960-f8fcc26f0129', fringe_validation_unnecessary_for_authority, empirically_contingent).
narrative_ontology:cs_reference_frame('980a9324-93b1-48d3-8960-f8fcc26f0129', state_transformative_authority_framework).
narrative_ontology:cs_drift_state('980a9324-93b1-48d3-8960-f8fcc26f0129', contemporary_mandate_failure_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('980a9324-93b1-48d3-8960-f8fcc26f0129', '').
narrative_ontology:cs_kernel_id(state_commitment_installation_mechanism__exogenous_imposition_reading, state_commitment_installation_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__exogenous_imposition_reading, state_apparatus).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__exogenous_imposition_reading, transformation_authority).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__exogenous_imposition_reading, institutional_periphery_actors).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__exogenous_imposition_reading, grassroots_constituencies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the transformative mandate to reorganize institutional commitments. Issues decrees installing new commitments across the domain without consulting the institutional periphery or grassroots constituencies that will operate them. Benefits from the appearance of coherent, unified commitment landscape and from the extraction of legitimacy debt from subordinate actors who must absorb adoption costs. The mandate itself is the state's claim to override endogenous preference formation.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% The executive or reforming cadre designated to implement the state's transformation mandate. Collects authority, career advancement, and institutional prestige from successful top-down installation. Their legitimacy depends on the mandate's acceptance, which they defend by suppressing grassroots resistance and fringe validation questions. They do not coordinate with base actors; they install commitments into them.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, transformation_authority, beneficiary,
    institutional, biographical, mobile, national).

% Institutional actors (local authorities, professional bodies, organizational leaders) who were developing or evaluating new commitments at the periphery before the state's decree. They bear the cost of abandoning their own evaluation and adoption processes, retraining staff, and restructuring operations to comply with the top-down mandate. Resistance is possible but the state's coercive apparatus constrains it; exit means losing institutional standing.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, institutional_periphery_actors, payer,
    moderate, biographical, constrained, regional).

% Citizens, workers, and community members who must live under and internalize the newly mandated commitments. They had no voice in the decision and no pathway to resist without identity rupture (refusing the state-mandated commitment means marking oneself as disloyal or outside the community). They bear internalization costs, adaptation friction, and the loss of autonomy in what counts as legitimate commitment.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, grassroots_constituencies, payer,
    powerless, immediate, identity_locked, local).

% Researchers, dissenting intellectuals, alternative institutional practitioners, and communities that could attest whether the mandated commitment actually works or serves the stated function. They are structurally excluded from the legitimation process because top-down mandate needs no fringe validation — to admit fringe validators to the process would be to give endogenous actors leverage. Their absence is enforced by the state's control of credentialing and platform access.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, fringe_validators, excluded,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(state_commitment_installation_mechanism__exogenous_imposition_reading, fringe_validators, observer).

% Alternative authority structures (religious bodies, regional traditions, professional guilds, cultural assemblies) that held competing claims to legitimate new commitments through their own mechanisms. The state's decree supersedes them, not by intellectual argument but by coercive backing. They are forced to either capitulate and affirm the state's commitment or face delegitimation and suppression.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, competing_legitimacy_frameworks, excluded,
    organized, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_commitment_installation_mechanism__exogenous_imposition_reading, state_apparatus).
narrative_ontology:fixing_cost_class(state_commitment_installation_mechanism__exogenous_imposition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Installs a unified, state-coherent commitment framework across disparate institutional actors, preventing a fragmented landscape where different regions or sectors operate under different commitments. The coordination function is the state's unified field, not bottom-up convergence.
% TRANSFER_FUNCTION: Moves legitimacy, authority, and institutional autonomy from periphery actors and grassroots constituencies to the state apparatus and its transformation authority. Periphery actors pay in retraining costs, lost evaluation pathways, and credibility damage if the mandate fails. Grassroots constituencies pay in identity-lock costs and adaptive friction. The state collects unified authority and reduced need to justify its commitments through performance.
% ABSENT_VOICES: Fringe validators (researchers, practitioners who could test whether the commitment works) are structurally absent — top-down mandate bypasses validation. Competing legitimacy frameworks (religious bodies, cultural authorities, professional guilds) are also absent because admitting them would legitimize endogenous alternatives. The constraint's persistence depends on keeping these voices out of the process.
% DISAPPEARANCE_RATIONALE: If the top-down installation mechanism vanished, institutional periphery actors would resume their own evaluation and slow adoption of new commitments. Grassroots constituencies would regain interpretive autonomy and could refuse identity-lock to mandates they do not accept. The state's unified commitment field would fragment into regional and sectoral variation. The transformation authority would lose its coercive-backed legitimacy and would have to negotiate or demonstrate superiority — moving the constraint toward an endogenous climb reading.
% FOUNDING_PROBLEM: New commitments gain legitimacy through top-down state installation because fragmented institutional adoption produces coordination failure — competing frameworks, delayed adoption, uneven implementation, and regional defection undermine state capacity to govern a unified population.
% FOUNDING_PROBLEM_CORROBORATION: State historical actors and transformation theorists attest the coordination problem is live and top-down installation solves it. Institutional historians, fringe validators, and competing-framework defenders attest that bottom-up validation produces better fitness for purpose and that top-down imposition trades coordination for error and exploitation. Legislative records, post-mandate failure case studies, and ethnographic work from outside the benefiting parties support the contested reading.
narrative_ontology:disappearance_verdict(state_commitment_installation_mechanism__exogenous_imposition_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_commitment_installation_mechanism__exogenous_imposition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_commitment_installation_mechanism__exogenous_imposition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_commitment_installation_mechanism__exogenous_imposition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_commitment_installation_mechanism__exogenous_imposition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_commitment_installation_mechanism__exogenous_imposition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_commitment_installation_mechanism__exogenous_imposition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts moderate (0.48) because the mandate initially frames itself as pure coordination and the state can point to real coordination gains (unified field). It rises to 0.68 over 40 years as the hidden costs accumulate: periphery actors experience mandate failure (wrongly-fitted commitments, wasted retraining), grassroots constituencies internalize costs as resentment, and the state must intensify suppression to maintain the facade. Suppression is high and rises (0.55→0.72) because the legitimacy debt grows — fringe validators could expose the mandate's errors but are kept excluded; competing frameworks could absorb dissent but are suppressed. Theater ratio rises (0.28→0.41) because enforcement increasingly becomes performative (the transformation authority stages demonstrations of commitment success, maintains ceremonial installation events) rather than functional (commitments that improve outcomes). The measurement series share one time grid per the alignment rule.
 *
 * PERSPECTIVAL GAP:
 *   From the state's seat, the arrangement solves a critical coordination problem (fragmented commitment adoption) and is legitimate authority exercising mandate. From the peripheral institution's seat, the same arrangement imposes costs without consultation and overrides superior local evaluation. From the grassroots seat, the arrangement is coercive identity-lock. These are not disagreements about facts — they are structural differences in what the constraint costs and who benefits. The engine should compute three different classifications, one per power level.
 *
 * DIRECTIONALITY LOGIC:
 *   State apparatus and transformation authority benefit from unified control without justification burden (low d). Institutional periphery actors pay adoption costs and bear wrongly-fitted commitments (high d). Grassroots constituencies internalize costs and lose autonomy (very high d). Excluded fringe validators would expose errors but are kept out. The constraint's persistence depends on suppressing the voices that could reveal the mounting extraction. Rising theater ratio indicates the state's increasing need for ceremonial proof of success as actual performance diverges from promise.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading is tangled_rope, not snare, because the state genuinely coordinates a unified commitment field — that is not a cover story, it is a real coordination function. But it extracts autonomy and imposes costs asymmetrically through the same structure. A snare would be pure extraction (e.g., the state mandates commitments purely to expand control, with no coordination benefit). A rope would be symmetric (state and subordinate actors both benefit equally, no coercion needed). This constraint is hybrid: real coordination + asymmetric extraction + enforcement to maintain the asymmetry. The mandatrophy question is whether the coordination benefit persists or atrophies: if mandated commitments consistently fail to fit actual conditions (measuring theater_ratio→high), the coordination function decays while extraction persists, and the constraint drifts toward snare or piton. The measurement series shows theater rising, suggesting early atrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandate_sufficiency_ambiguity,
    'Does the state''s transformation mandate suffice to legitimize new commitments, or is performance-based validation required for durable acceptance?',
    'Post-mandate historical analysis: compare jurisdictions that enforced top-down installation vs. those that allowed fringe validation. Measure long-term commitment persistence, adoption quality, and resistance levels. Survey institutional actors on whether they accepted the mandate due to coercion or due to belief in its legitimacy.',
    'If mandate suffices (exogenous reading correct), extraction can persist indefinitely on enforcement alone. If validation is required (hybrid reading correct), extraction eventually fails as fringe validators expose mandate errors. This determines whether the constraint stabilizes as tangled_rope or drifts toward piton/snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_sufficiency_ambiguity, empirical, 'Whether state mandate alone legitimizes or requires fringe validation').

omega_variable(
    coordination_function_atrophy,
    'As mandated commitments accumulate and performance diverges from promise, does the real coordination function decay while suppression machinery persists?',
    'Track theater_ratio and base_extractiveness separately over extended intervals. If theater rises while coordination benefits decline (measured by outcome divergence from mandate''s stated purpose), the constraint is atrophying from tangled_rope toward piton. If theater stays flat while extractiveness rises, pure extraction is replacing coordination.',
    'Atrophy diagnosis triggers mandatrophy determination: a constraint whose founding coordination function has died but whose extraction structure persists is a piton candidate. This reclassifies the constraint even if enforcement is still intense.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_function_atrophy, empirical, 'Whether the mandate''s real coordination function persists or atrophies over time').

omega_variable(
    identity_lock_mechanism_structural_vs_internalized,
    'For grassroots constituencies, is suppression structural (legal prohibition, credential gatekeeping, economic dependency on mandate compliance) or internalized (belief that the mandate is legitimate, identity fusion with mandated commitment)?',
    'Post-mandate collapse experiment or jurisdiction transition: if grassroots constituencies abandon the mandated commitment immediately upon legal prohibition removal, suppression is structural; if they persist in supporting it even after removal and choose not to return to pre-mandate alternatives, suppression is internalized.',
    'Structural suppression means the constraint''s effective extraction ends when coercive backing ends. Internalized suppression persists as identity-lock and carries the extraction forward even after legal removal. A constraint with high internalized suppression is more durable and extraction-resistant than one with only structural suppression.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_structural_vs_internalized, empirical, 'Whether grassroots suppression is structural or internalized in the mandated commitment').

omega_variable(
    alternative_sibling_reading_possibility,
    'Could the observed installation pattern be better explained by the endogenous_climb_reading or hybrid_cascade_reading rather than pure exogenous imposition?',
    'Examine historical records for grassroots advocacy, fringe validation phases, or cascading adoption patterns that preceded the state''s decree. If substantial endogenous climbing occurred before the decree, the reading may be misclassified (should be hybrid_cascade). If the decree contradicts rather than follows demonstrated superiority, exogenous imposition is correct.',
    'If the reading is misclassified, the ε value and beneficiary/victim structure may be misassigned. A constraint that appears extractive via exogenous reading may be less extractive if it is actually a hybrid cascade that beneficiaries are using selective framing to justify coercively.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_sibling_reading_possibility, conceptual, 'Whether this installation case fits exogenous imposition or a sibling reading better').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_commitment_installation_mechanism__exogenous_imposition_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(stat_tr_t5, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(stat_tr_t10, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement(stat_tr_t15, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(stat_tr_t25, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(stat_tr_t40, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(stat_be_t5, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(stat_be_t10, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(stat_be_t15, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(stat_be_t25, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(stat_be_t40, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(stat_su_t5, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(stat_su_t10, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(stat_su_t15, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(stat_su_t25, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(stat_su_t40, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_commitment_installation_mechanism__exogenous_imposition_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.12).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__exogenous_imposition_reading, state_commitment_installation_mechanism__endogenous_climb_reading).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__exogenous_imposition_reading, state_commitment_installation_mechanism__hybrid_cascade_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a kernel concerning how new commitments gain legitimacy in state societies. The kernel has three structurally distinct readings, each with its own constraint story, ε value, beneficiary/victim set, and type classification. The exogenous_imposition_reading (this file) instantiates top-down authority-driven installation as the legitimacy mechanism, with the state as primary beneficiary and periphery/grassroots as payers. The endogenous_climb_reading instantiates demonstrated-superiority as the legitimacy mechanism, with innovators and institutional practitioners as beneficiaries. The hybrid_cascade_reading instantiates installation-plus-validation as a two-phase mechanism. These are not interpretations of a single constraint — they are structurally different constraints arising from different readings of the same contested kernel. Each has its own ε because the referent differs: exogenous measures the state's top-down installation arrangement; endogenous measures the demonstration-and-adoption arrangement; hybrid measures the cascading arrangement. The network edges allow corpus analysis to track how the readings relate (which forecloses which, which coexists with which) and how misreading a kernel as a single constraint (vs. decomposing into three) affects classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_commitment_installation_mechanism__exogenous_imposition_reading, powerless, 0.88).
constraint_indexing:directionality_override(state_commitment_installation_mechanism__exogenous_imposition_reading, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
