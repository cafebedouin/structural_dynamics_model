% ============================================================================
% CONSTRAINT STORY: decalogue_image_prohibition__iconodule_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_decalogue_image_prohibition__iconodule_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: decalogue_image_prohibition__iconodule_reading
 *   human_readable: Iconodule Reading of Decalogue Image Prohibition
 *   domain: theological/religious_authority/visual_culture
 *
 * SUMMARY:
 *   This constraint instantiates the iconodule reading of the Decalogue image
 *   prohibition: the commandment forbids worship directed to images (latria)
 *   but permits honor directed through images to their divine prototypes
 *   (dulia), grounded in the Christological premise that the Incarnation
 *   sanctifies matter. The reading functions as a coordination mechanism
 *   enabling embodied, sensory encounter with the divine through canonically
 *   regulated visual culture, while structurally excluding aniconic
 *   alternatives. It is authored as one reading of a contested kernel; the
 *   iconoclast reading (universal prohibition) and moderate iconoclast
 *   reading (2D permitted, 3D forbidden) are sibling constraints with
 *   distinct structural profiles.
 *
 * KEY AGENTS:
 *   - iconodule_episcopate: Agenda-setter (institutional/civilizational) â maintains the latria/dulia distinction and canonical standards
 *   - orthodox_laity: Beneficiary (moderate/biographical) â receives spiritual coordination through sanctioned icon veneration
 *   - iconographers: Beneficiary (moderate/biographical) â practice sanctified under doctrinal authorization
 *   - iconoclast_communities: Excluded (organized/generational) â structurally absent from doctrinal deliberation, practice suppressed where iconodulia holds power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decalogue_image_prohibition__iconodule_reading, 0.28).
domain_priors:suppression_score(decalogue_image_prohibition__iconodule_reading, 0.42).
domain_priors:theater_ratio(decalogue_image_prohibition__iconodule_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__iconodule_reading, rope).
narrative_ontology:human_readable(decalogue_image_prohibition__iconodule_reading, "Iconodule Reading of Decalogue Image Prohibition").
narrative_ontology:topic_domain(decalogue_image_prohibition__iconodule_reading, "theological/religious_authority/visual_culture").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__iconodule_reading, 'b8be5d4d-f73b-450a-a021-d088856205f3').
narrative_ontology:cs_kernel_codification('b8be5d4d-f73b-450a-a021-d088856205f3', fixed_text).
narrative_ontology:cs_authority_grounding('b8be5d4d-f73b-450a-a021-d088856205f3', lineage).
narrative_ontology:cs_interpretation_layer_present('b8be5d4d-f73b-450a-a021-d088856205f3').
narrative_ontology:cs_reading_relation('b8be5d4d-f73b-450a-a021-d088856205f3', decalogue_image_prohibition__iconoclast_reading, forecloses).
narrative_ontology:cs_reading_relation('b8be5d4d-f73b-450a-a021-d088856205f3', decalogue_image_prohibition__moderate_iconoclast_reading, coexists_with).
narrative_ontology:cs_axiom('b8be5d4d-f73b-450a-a021-d088856205f3', foundational, incarnation_sanctifies_matter).
narrative_ontology:cs_axiom_status(incarnation_sanctifies_matter, holdable).
narrative_ontology:cs_axiom_grounding('b8be5d4d-f73b-450a-a021-d088856205f3', incarnation_sanctifies_matter, theological).
narrative_ontology:cs_axiom('b8be5d4d-f73b-450a-a021-d088856205f3', foundational, latria_dulia_distinction).
narrative_ontology:cs_axiom_status(latria_dulia_distinction, holdable).
narrative_ontology:cs_axiom_grounding('b8be5d4d-f73b-450a-a021-d088856205f3', latria_dulia_distinction, deontological).
narrative_ontology:cs_reference_frame('b8be5d4d-f73b-450a-a021-d088856205f3', nicaean_iconodule_framework).
narrative_ontology:cs_drift_state('b8be5d4d-f73b-450a-a021-d088856205f3', post_reformation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b8be5d4d-f73b-450a-a021-d088856205f3', '').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__iconodule_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, orthodox_laity).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, iconographers).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, iconodule_communion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains and transmits the doctrinal distinction between latria and dulia through canon law, liturgical regulation, and ecumenical council tradition; authorizes iconographers and adjudicates orthodox depiction, anathematizing iconoclasm as heresy.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, iconodule_episcopate, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Receives spiritual coordination through veneration of sanctioned icons in liturgy and private devotion; taught to direct honor through the image to its divine prototype while avoiding idolatry.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, orthodox_laity, beneficiary,
    moderate, biographical, constrained, global).

% Practice sacred iconography under canonical norms; their craft is sanctified and protected by the doctrinal authorization of material mediation, giving their labor liturgical significance and ecclesiastical patronage.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, iconographers, beneficiary,
    moderate, biographical, constrained, regional).

% Reject all image-veneration as idolatry; their voice is structurally excluded from doctrinal deliberation and their practice suppressed where the iconodule reading holds political and ecclesiastical power.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, iconoclast_communities, excluded,
    organized, generational, trapped, global).

narrative_ontology:fixing_cost_class(decalogue_image_prohibition__iconodule_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables sensory, material mediation of the divine for embodied worshippers by authorizing honor-through-images (dulia) while forbidding worship-of-images (latria), solving the coordination problem of how finite creatures encounter infinite presence without idolatry.
% TRANSFER_FUNCTION: Moves spiritual attention and ritual practice from aniconic or unmediated worship toward canonically sanctioned visual forms, and consolidates regulatory authority over religious art to the episcopate and ecumenical tradition.
% ABSENT_VOICES: Iconoclast communities, aniconic Jewish and Muslim voices, and later Protestant reformers are structurally excluded from the doctrinal conversation after ecumenical codification. They would argue that any material representation in worship constitutes idolatry, and that the latria/dulia distinction is a sophistic evasion of the commandment's plain meaning.
% DISAPPEARANCE_RATIONALE: If the iconodule doctrinal framework disappeared, Orthodox liturgy would lose its organizing visual logic; icon screens, festal processions, and private devotion would collapse into aniconic austerity or unregulated visual piety. The sacred art economy and the guilds of iconographers would dissolve. The ecclesiastical boundary between orthodoxy and heresy on this point would vanish.
% FOUNDING_PROBLEM: How can embodied, sensory humans encounter the divine without falling into idolatry, given that matter is created by God and has been sanctified by the Incarnation?
% FOUNDING_PROBLEM_CORROBORATION: Iconodule theologians from John of Damascus to Theodore the Studite attest the problem from within the tradition. External corroboration from aniconic Jewish, Islamic, and certain Protestant scholars disputes that images solve the problem, arguing instead that aniconism is the correct response; this dissent is itself signal that the problem remains contested rather than self-asserted.
narrative_ontology:disappearance_verdict(decalogue_image_prohibition__iconodule_reading, world_rearranges).
narrative_ontology:founding_problem_status(decalogue_image_prohibition__iconodule_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__iconodule_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(decalogue_image_prohibition__iconodule_reading, 'none', 1).
narrative_ontology:epsilon_provenance(decalogue_image_prohibition__iconodule_reading, 0.28, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(decalogue_image_prohibition__iconodule_reading_tests).
:- end_tests(decalogue_image_prohibition__iconodule_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.28) because the primary flow is spiritual coordination rather than material extraction; the constraint moves devotional attention toward sanctioned forms rather than concentrating rents. Suppression is moderate (0.42) because the iconodule triumph at Nicaea II and subsequent enforcement anathematized iconoclasm, closing some alternatives, though the core mechanism remains catechetical. Theater ratio is low (0.18): the Christological and sacramental logic is functionally central, not performative cover. Accessibility collapse is elevated (0.65) because once the Incarnation premise is accepted, aniconism becomes theologically incoherent within the framework. Resistance (0.38) reflects persistent iconoclast minorities and later Protestant aniconism.
 *
 * PERSPECTIVAL GAP:
 *   The episcopal seat experiences the constraint as custodianship of apostolic tradition â a rope that prevents idolatry while preserving creaturely access to the divine. The laity and iconographers experience it as enabling infrastructure. Iconoclast communities, where excluded, experience the same structure as suppression of their aniconic practice. The engine computes this divergence from structural data: agenda-setter with identity-locked exit reads beneficiary-direction; excluded/trapped parties read target-direction.
 *
 * DIRECTIONALITY LOGIC:
 *   The iconodule episcopate and the Orthodox communion benefit from doctrinal unity and authorized mediation (low d). The laity and iconographers receive coordination subsidy through the sanctioned visual culture (low-to-mid d). Iconoclast communities bear the cost of exclusion and historical suppression where the reading holds political power (high d). Because no single agent captures extracted material rents, the effective extraction is diffuse across the communion rather than concentrated.
 *
 * MANDATROPHY ANALYSIS:
 *   The rope classification prevents misreading the constraint as pure extraction (snare) because the coordination function â solving how embodied humans encounter the divine without idolatry â is genuine and structurally prior. However, the tangled_rope possibility is flagged via omega because the same structure that coordinates laity also suppresses iconoclast alternatives. The classification rope is sustained by the absence of a concentrated beneficiary capturing rent from a trapped population; the costs to iconoclasts are real but diffuse and incidental to the coordination function rather than its purpose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    iconodule_suppression_ambiguity,
    'Does the iconodule reading extract from iconoclast minorities by suppressing their aniconic practice, or is the coordination benefit for the majority separable from the suppression of alternatives?',
    'Historical case analysis: examine whether iconoclast communities were materially persecuted, excluded from civic life, or merely theologically anathematized under iconodule political dominance.',
    'If suppression is structurally coupled to coordination, the constraint is tangled_rope rather than rope; if separable, the rope classification holds and the suppression is incidental political overlay.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(iconodule_suppression_ambiguity, empirical, 'Whether iconoclast suppression is structurally coupled to iconodule coordination.').

omega_variable(
    latria_dulia_stability,
    'Is the boundary between latria and dulia theologically stable without escalating enforcement, or does it require ever-finer interpretive control to prevent drift into idolatry?',
    'Examination of historical iconographic controversies (e.g., hesychast debates, modern liturgical reform) to see if the boundary has required periodic reassertion or expansion of enforcement.',
    'If the boundary is inherently unstable, theater_ratio may rise and the constraint may drift toward tangled_rope or piton as enforcement atrophies into performance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(latria_dulia_stability, conceptual, 'Theological stability of the latria/dulia boundary over time.').

omega_variable(
    kernel_reading_committer,
    'This constraint is one reading of kernel decalogue_image_prohibition. How would classification change if the iconoclast reading were adopted instead?',
    'Compare the sibling constraint stories: the iconoclast reading would declare aniconic communities as beneficiaries and iconodule practitioners as victims, likely computing as a snare or tangled_rope depending on enforcement history.',
    'The kernel is not one constraint with ambiguous epsilon but multiple constraints with opposed beneficiary/victim structures; conflating them would produce incoherent metrics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer, conceptual, 'Commitment structure of the kernel reading decomposition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__iconodule_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(iconodule_tr_t0, decalogue_image_prohibition__iconodule_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(iconodule_tr_t200, decalogue_image_prohibition__iconodule_reading, theater_ratio, 200, 0.12).
narrative_ontology:measurement(iconodule_tr_t400, decalogue_image_prohibition__iconodule_reading, theater_ratio, 400, 0.14).
narrative_ontology:measurement(iconodule_tr_t600, decalogue_image_prohibition__iconodule_reading, theater_ratio, 600, 0.13).
narrative_ontology:measurement(iconodule_tr_t800, decalogue_image_prohibition__iconodule_reading, theater_ratio, 800, 0.22).
narrative_ontology:measurement(iconodule_tr_t1000, decalogue_image_prohibition__iconodule_reading, theater_ratio, 1000, 0.19).
narrative_ontology:measurement(iconodule_tr_t1200, decalogue_image_prohibition__iconodule_reading, theater_ratio, 1200, 0.18).

% Extraction over time
narrative_ontology:measurement(iconodule_be_t0, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(iconodule_be_t200, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 200, 0.24).
narrative_ontology:measurement(iconodule_be_t400, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 400, 0.26).
narrative_ontology:measurement(iconodule_be_t600, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 600, 0.28).
narrative_ontology:measurement(iconodule_be_t800, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 800, 0.32).
narrative_ontology:measurement(iconodule_be_t1000, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 1000, 0.3).
narrative_ontology:measurement(iconodule_be_t1200, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 1200, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(iconodule_su_t0, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(iconodule_su_t200, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 200, 0.38).
narrative_ontology:measurement(iconodule_su_t400, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 400, 0.42).
narrative_ontology:measurement(iconodule_su_t600, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 600, 0.4).
narrative_ontology:measurement(iconodule_su_t800, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 800, 0.5).
narrative_ontology:measurement(iconodule_su_t1000, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 1000, 0.45).
narrative_ontology:measurement(iconodule_su_t1200, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 1200, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decalogue_image_prohibition__iconodule_reading, attachment_coordination).

% DUAL FORMULATION NOTE:
% The natural-language label 'Decalogue image prohibition' conflates three structurally distinct readings: the iconoclast reading (universal prohibition), the iconodule reading (latria/dulia distinction with Incarnation logic), and the moderate iconoclast reading (2D permitted, 3D forbidden). Each reading carries distinct epsilon values, beneficiary structures, and enforcement histories. They form a constraint family linked by competitive interpretive dependency rather than causal downstream influence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
