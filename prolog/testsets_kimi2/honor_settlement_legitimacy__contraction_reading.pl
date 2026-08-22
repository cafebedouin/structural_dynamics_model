% ============================================================================
% CONSTRAINT STORY: honor_settlement_legitimacy__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_settlement_legitimacy__contraction_reading, []).

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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: honor_settlement_legitimacy__contraction_reading
 *   human_readable: Honor Settlement Legitimacy â Contraction Reading
 *   domain: historical sociology / legal history / cultural anthropology
 *
 * SUMMARY:
 *   This constraint is the contraction reading of the kernel
 *   honor_settlement_legitimacy. It models the European aristocratic
 *   institution of dueling as a mechanism for settling affronts to honor.
 *   During the interval (roughly 1750â1850/1900), the constraint undergoes
 *   a severe lifecycle drift: beginning as a vigorously enforced tangled rope
 *   that coordinated aristocratic status reproduction while extracting bodily
 *   risk from participants, and ending as a degraded, largely theatrical
 *   remnant that became cognitively unthinkable as legitimate action. The
 *   contraction reading claims that honor culture itself exited the normative
 *   possibility space â dueling was not merely prohibited but rendered
 *   incomprehensible. The claimed type remains tangled_rope because that
 *   captures the constraint's structural essence during its operative life;
 *   the end-state metrics describe its severe degradation, producing a
 *   deliberate divergence for the engine to measure.
 *
 * KEY AGENTS:
 *   - aristocratic_elite: Primary beneficiary (powerful/identity_locked) â captures diffuse status reproduction
 *   - dueling_seconds: Agenda setter (moderate/constrained) â administers ritual and enforces procedural compliance
 *   - obligated_duelists: Primary target (moderate/identity_locked) â bears physical risk and social coercion
 *   - challenged_parties: Secondary target (moderate/identity_locked) â compelled to participate under threat of social death
 *   - civil_magistrates: Excluded institutional actor (institutional/constrained) â legally opposed but socially powerless
 *   - religious_moralists: Excluded organized actor (organized/mobile) â morally opposed but structurally absent from masculine governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__contraction_reading, 0.22).
domain_priors:suppression_score(honor_settlement_legitimacy__contraction_reading, 0.28).
domain_priors:theater_ratio(honor_settlement_legitimacy__contraction_reading, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, theater_ratio, 0.78).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__contraction_reading, tangled_rope).
narrative_ontology:human_readable(honor_settlement_legitimacy__contraction_reading, "Honor Settlement Legitimacy â Contraction Reading").
narrative_ontology:topic_domain(honor_settlement_legitimacy__contraction_reading, "historical sociology / legal history / cultural anthropology").

domain_priors:requires_active_enforcement(honor_settlement_legitimacy__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__contraction_reading, '767b6c2d-9000-4aed-8756-12d38702259e').
narrative_ontology:cs_kernel_codification('767b6c2d-9000-4aed-8756-12d38702259e', distributed).
narrative_ontology:cs_authority_grounding('767b6c2d-9000-4aed-8756-12d38702259e', practice).
narrative_ontology:cs_interpretation_layer_present('767b6c2d-9000-4aed-8756-12d38702259e').
narrative_ontology:cs_reading_relation('767b6c2d-9000-4aed-8756-12d38702259e', honor_settlement_legitimacy__drop_reading, forecloses).
narrative_ontology:cs_reading_relation('767b6c2d-9000-4aed-8756-12d38702259e', honor_settlement_legitimacy__composite_reading, coexists_with).
narrative_ontology:cs_axiom('767b6c2d-9000-4aed-8756-12d38702259e', foundational, honor_violence_incomprehensible).
narrative_ontology:cs_axiom_status(honor_violence_incomprehensible, holdable).
narrative_ontology:cs_axiom_grounding('767b6c2d-9000-4aed-8756-12d38702259e', honor_violence_incomprehensible, empirically_contingent).
narrative_ontology:cs_reference_frame('767b6c2d-9000-4aed-8756-12d38702259e', aristocratic_honor_sovereignty).
narrative_ontology:cs_drift_state('767b6c2d-9000-4aed-8756-12d38702259e', post_civilizing_process_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('767b6c2d-9000-4aed-8756-12d38702259e', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__contraction_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__contraction_reading, aristocratic_elite).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__contraction_reading, obligated_duelists).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__contraction_reading, challenged_parties).
narrative_ontology:constraint_vindicates(honor_settlement_legitimacy__contraction_reading, aristocratic_autonomy_doctrine).
narrative_ontology:constraint_vindicates(honor_settlement_legitimacy__contraction_reading, private_violence_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their collective status and class boundaries are reproduced through the honor code. Refusing the code means abandoning aristocratic identity itself and accepting social death within their peer group. They collect the diffuse status benefit of a closed, self-governing caste.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, aristocratic_elite, beneficiary,
    powerful, biographical, identity_locked, national).

% Administer the rituals of the duel, negotiate terms, ensure procedural fairness, and witness the encounter. Their authority derives from mastery of the honor code's arcane rules; they enforce compliance by threatening social ostracism for breaches of form.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, dueling_seconds, agenda_setter,
    moderate, biographical, constrained, national).

% Men bound by the honor code who must accept challenges or face total ostracism. Their physical survival is wagered against their social standing; exit means renouncing their identity and class position, which is psychologically and socially prohibitive.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, obligated_duelists, payer,
    moderate, immediate, identity_locked, local).

% Those who receive a challenge and are compelled by the code to respond. They bear the risk of death or injury regardless of whether they initiated the dispute, and cannot decline without suffering the same social death as the challenger.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, challenged_parties, payer,
    moderate, immediate, identity_locked, local).

% State authorities who criminalize dueling and claim jurisdiction over interpersonal violence, but lack effective enforcement capacity within the aristocratic private sphere. Their legal prohibitions are routinely ignored or treated as irrelevant by the honor culture.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, civil_magistrates, excluded,
    institutional, generational, constrained, national).

% Clergy and moral reformers who publicly condemn dueling as sinful and barbaric, but are structurally excluded from the masculine honor culture's internal governance and have no seat at the table where challenges are negotiated.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, religious_moralists, excluded,
    organized, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_settlement_legitimacy__contraction_reading, aristocratic_elite).
narrative_ontology:fixing_cost_class(honor_settlement_legitimacy__contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a decentralized, self-enforcing mechanism for resolving severe interpersonal affronts among social equals without recourse to state courts, while simultaneously reproducing aristocratic class boundaries and masculine honor identity.
% TRANSFER_FUNCTION: Transferred physical risk, bodily injury, and death from challenged and obligated parties to the collective aristocratic order, which used the ritual to maintain status differentiation and internal hierarchy.
% ABSENT_VOICES: Women, who bore the consequences of male violence but were excluded from the honor code's masculine sphere; religious authorities who condemned dueling but were treated as irrelevant by the aristocratic culture; state magistrates whose legal prohibitions were actively circumvented.
% DISAPPEARANCE_RATIONALE: The aristocratic class lost its distinctive private jurisdiction over affronts; masculine identity reorganized around civic and commercial virtue rather than blood honor; state courts became the sole legitimate arena for serious interpersonal grievances.
% FOUNDING_PROBLEM: In a decentralized society where aristocratic status depended on public reputation and the state lacked a monopoly on legitimate violence, how could severe interpersonal affronts among equals be settled without triggering dynastic blood feuds or state intervention that would degrade aristocratic autonomy?
% FOUNDING_PROBLEM_CORROBORATION: Historians Norbert Elias and others attest the problem was live in the early modern period. Legal historians and sociologists outside the aristocratic beneficiary class corroborate that the rise of absolutist states and bureaucratic status systems solved the founding problem by the 19th century, rendering the private honor mechanism obsolete.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_settlement_legitimacy__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_settlement_legitimacy__contraction_reading, 0.22, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_settlement_legitimacy__contraction_reading_tests).
:- end_tests(honor_settlement_legitimacy__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is authored at 0.22 to reflect the constraint's near-dissolution by interval end; the measurement series shows a monotonic decline from 0.72 as the cultural framework contracted. Suppression tracks similarly from 0.85 to 0.28, reflecting the erosion of the social enforcement machinery as state institutions monopolized violence. Theater_ratio rises from 0.25 to 0.78 because the constraint's final phase was dominated by performative maintenance and nostalgic ritual rather than functional dispute resolution. The trajectory represents a lifecycle drift from active tangled rope toward inertial piton-like behavior without stabilizing there â the contraction reading claims the constraint dissolved rather than fossilized. Accessibility_collapse is low at end (0.20) because by the interval's close, alternatives (state courts, bourgeois status competition) were readily available and cognitively dominant. Resistance is low at end (0.15) because the constraint expired with a whimper, not a climax of resistance; most agents had already exited the identity frame.
 *
 * PERSPECTIVAL GAP:
 *   The aristocratic beneficiary seat and the obligated duelist payer seat should compute very differently. From the aristocratic perspective, the duel was a sacred coordination mechanism preserving caste autonomy and masculine virtue; from the compelled participant's perspective, it was a lethal extraction mechanism enforcing class discipline. The engine computes this divergence from the structural data: the elite are identity_locked beneficiaries (d near 0.0), while duelists are identity_locked payers (d near 1.0). The divergence is sharpened by the fact that both are locked through the same identity mechanism â aristocratic masculinity â yet one profits structurally while the other pays with bodily risk.
 *
 * DIRECTIONALITY LOGIC:
 *   The aristocratic elite are declared beneficiaries with identity_locked exit, deriving near-beneficiary directionality (low d) â the constraint subsidizes their status reproduction. Obligated duelists and challenged parties are declared victims with identity_locked exit, deriving near-target directionality (high d) â the constraint extracts their physical security. Dueling seconds occupy an agenda_setter role with constrained exit; they are not beneficiaries in the base_properties arrays, so their d derives from their structural position as administrators without victim status, placing them near symmetric or slightly toward beneficiary. Civil magistrates and religious moralists are excluded/observers with no beneficiary/victim declaration, so their d reverts to the canonical fallback for their power atom.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â decentralized dispute resolution among status equals without state intervention â was solved by the rise of absolutist state monopolies on violence and bureaucratic status systems. The constraint persisted well past the death of its founding problem, but in this reading it did not fossilize as a pure piton. Instead, the entire cultural framework contracted, rendering the practice unthinkable rather than merely obsolete. This prevents mislabeling the final phase as simple institutional inertia: the cognitive exit from the honor frame was deeper than mere neglect. The metric profile (declining extraction, rising theater, declining suppression) is consistent with mandatrophy, but the narrative frame specifies dissolution rather than theatrical maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contraction_vs_fringe_persistence,
    'Did honor culture genuinely exit the normative possibility space entirely (contraction), or did it persist as a residual fringe practice among isolated adherents (drop)?',
    'Microhistorical study of late 19th-century aristocratic subcultures and colonial enclaves to detect whether dueling remained cognitively available as a legitimate option even where rarely exercised.',
    'If residual adherents maintained the cognitive framework, the contraction reading overstates the completeness of the transformation and the drop reading gains support; if no residual framework existed, contraction is vindicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contraction_vs_fringe_persistence, empirical, 'Whether the honor framework fully contracted or persisted in residual pockets.').

omega_variable(
    cultural_vs_coercive_driver,
    'Was dueling''s decline primarily driven by endogenous cultural framework transformation (the Eliasian civilizing process), or by exogenous state coercion and legal prohibition that suppressed the practice while the cultural framework persisted latent?',
    'Comparative analysis across jurisdictions with varying enforcement intensity: if dueling declined similarly under weak and strong state prohibition, cultural transformation is the stronger explanation; if decline tracks enforcement capacity, coercion dominates.',
    'Resolves whether the constraint''s dissolution was a genuine contraction of the normative space or a suppression-driven drop in observable behavior with latent persistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_vs_coercive_driver, conceptual, 'Whether cultural transformation or state coercion drove the decline.').

omega_variable(
    identity_lock_vs_structural_trap,
    'Was the duelist''s inability to exit primarily a structural trap (external ostracism) or an internalized identity lock (self-concept constituted through honor culture)?',
    'Analysis of memoirs and correspondence from reluctant duelists to determine whether refusal was unthinkable due to internalized masculine aristocratic identity or merely due to calculable external sanctions.',
    'If internalized, the effective extraction was higher than structural measures suggest; if purely external, the constraint is more readily classifiable as pure coercion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_vs_structural_trap, empirical, 'Structural versus internalized suppression mechanism in honor culture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__contraction_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(honor_settlement_legitimacy__contraction_reading_tr_t0, honor_settlement_legitimacy__contraction_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(honor_settlement_legitimacy__contraction_reading_tr_t20, honor_settlement_legitimacy__contraction_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(honor_settlement_legitimacy__contraction_reading_tr_t40, honor_settlement_legitimacy__contraction_reading, theater_ratio, 40, 0.5).
narrative_ontology:measurement(honor_settlement_legitimacy__contraction_reading_tr_t60, honor_settlement_legitimacy__contraction_reading, theater_ratio, 60, 0.62).
narrative_ontology:measurement(honor_settlement_legitimacy__contraction_reading_tr_t80, honor_settlement_legitimacy__contraction_reading, theater_ratio, 80, 0.7).
narrative_ontology:measurement(honor_settlement_legitimacy__contraction_reading_tr_t100, honor_settlement_legitimacy__contraction_reading, theater_ratio, 100, 0.78).

% Extraction over time
narrative_ontology:measurement(honor_settlement_legitimacy__contraction_reading_be_t0, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(honor_settlement_legitimacy__contraction_reading_be_t20, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(honor_settlement_legitimacy__contraction_reading_be_t40, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 40, 0.45).
narrative_ontology:measurement(honor_settlement_legitimacy__contraction_reading_be_t60, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 60, 0.35).
narrative_ontology:measurement(honor_settlement_legitimacy__contraction_reading_be_t80, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 80, 0.28).
narrative_ontology:measurement(honor_settlement_legitimacy__contraction_reading_be_t100, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 100, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(honor_settlement_legitimacy__contraction_reading_su_t0, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(honor_settlement_legitimacy__contraction_reading_su_t20, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(honor_settlement_legitimacy__contraction_reading_su_t40, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 40, 0.55).
narrative_ontology:measurement(honor_settlement_legitimacy__contraction_reading_su_t60, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 60, 0.42).
narrative_ontology:measurement(honor_settlement_legitimacy__contraction_reading_su_t80, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 80, 0.35).
narrative_ontology:measurement(honor_settlement_legitimacy__contraction_reading_su_t100, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 100, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_settlement_legitimacy__contraction_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
