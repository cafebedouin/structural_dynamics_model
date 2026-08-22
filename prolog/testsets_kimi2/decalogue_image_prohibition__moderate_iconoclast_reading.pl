% ============================================================================
% CONSTRAINT STORY: decalogue_image_prohibition__moderate_iconoclast_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_decalogue_image_prohibition__moderate_iconoclast_reading, []).

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
 *   constraint_id: decalogue_image_prohibition__moderate_iconoclast_reading
 *   human_readable: Decalogue Image Prohibition (Moderate Iconoclast Reading)
 *   domain: theological/religious_authority
 *
 * SUMMARY:
 *   The moderate iconoclast reading of the decalogue image prohibition
 *   interprets the commandment as forbidding three-dimensional cult statuary
 *   (viewed as carrying higher idolatry risk due to tactile and spatial
 *   presence) while permitting two-dimensional images under strict clerical
 *   regulation. This reading instantiates a constraint that presents itself
 *   as protecting worship integrity but structurally operates as a
 *   gatekeeping mechanism: the regulatory authority extracts compliance labor
 *   and interpretive submission from worship communities and artisans, while
 *   maintaining a monopoly on legitimate visual mediation. The constraint is
 *   a contested reading within the broader decalogue image prohibition
 *   kernel, differing from the strict iconoclast reading (which forbids all
 *   material images) and the iconodule reading (which sanctifies material
 *   mediation including three-dimensional icons).
 *
 * KEY AGENTS:
 *   - clerical_authority: Agenda-setter and beneficiary (institutional/arbitrage) â defines the dimensional boundary, monitors compliance, and collects interpretive control
 *   - worship_communities: Primary payer (powerless/constrained) â bear compliance costs of regulated image use and are barred from three-dimensional devotional objects
 *   - artisans_and_image_makers: Secondary payer (moderate/constrained) â subject to production monitoring and exclusion from three-dimensional religious statuary
 *   - iconodule_communities: Excluded voice (moderate/trapped) â hold an alternative reading of the kernel but are structurally excluded from regulatory conversation
 *   - textual_scholars: Analytical observer (analytical/analytical) â examine philological and historical basis of the dimensional distinction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decalogue_image_prohibition__moderate_iconoclast_reading, 0.72).
domain_priors:suppression_score(decalogue_image_prohibition__moderate_iconoclast_reading, 0.68).
domain_priors:theater_ratio(decalogue_image_prohibition__moderate_iconoclast_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__moderate_iconoclast_reading, snare).
narrative_ontology:human_readable(decalogue_image_prohibition__moderate_iconoclast_reading, "Decalogue Image Prohibition (Moderate Iconoclast Reading)").
narrative_ontology:topic_domain(decalogue_image_prohibition__moderate_iconoclast_reading, "theological/religious_authority").

domain_priors:requires_active_enforcement(decalogue_image_prohibition__moderate_iconoclast_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__moderate_iconoclast_reading, '340731d3-896a-4688-9d7c-54b2fd5b1e48').
narrative_ontology:cs_kernel_codification('340731d3-896a-4688-9d7c-54b2fd5b1e48', fixed_text).
narrative_ontology:cs_authority_grounding('340731d3-896a-4688-9d7c-54b2fd5b1e48', lineage).
narrative_ontology:cs_interpretation_layer_present('340731d3-896a-4688-9d7c-54b2fd5b1e48').
narrative_ontology:cs_reading_relation('340731d3-896a-4688-9d7c-54b2fd5b1e48', decalogue_image_prohibition__iconoclast_reading, coexists_with).
narrative_ontology:cs_reading_relation('340731d3-896a-4688-9d7c-54b2fd5b1e48', decalogue_image_prohibition__iconodule_reading, coexists_with).
narrative_ontology:cs_axiom('340731d3-896a-4688-9d7c-54b2fd5b1e48', foundational, three_dimensional_cult_statuary_prohibited).
narrative_ontology:cs_axiom_status(three_dimensional_cult_statuary_prohibited, holdable).
narrative_ontology:cs_axiom_grounding('340731d3-896a-4688-9d7c-54b2fd5b1e48', three_dimensional_cult_statuary_prohibited, theological).
narrative_ontology:cs_axiom('340731d3-896a-4688-9d7c-54b2fd5b1e48', foundational, two_dimensional_images_permissible_under_regulation).
narrative_ontology:cs_axiom_status(two_dimensional_images_permissible_under_regulation, holdable).
narrative_ontology:cs_axiom_grounding('340731d3-896a-4688-9d7c-54b2fd5b1e48', two_dimensional_images_permissible_under_regulation, theological).
narrative_ontology:cs_reference_frame('340731d3-896a-4688-9d7c-54b2fd5b1e48', dimensional_aniconic_distinction).
narrative_ontology:cs_drift_state('340731d3-896a-4688-9d7c-54b2fd5b1e48', devotional_material_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('340731d3-896a-4688-9d7c-54b2fd5b1e48', '').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__moderate_iconoclast_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__moderate_iconoclast_reading, clerical_authority).
narrative_ontology:constraint_victim(decalogue_image_prohibition__moderate_iconoclast_reading, worship_communities).
narrative_ontology:constraint_victim(decalogue_image_prohibition__moderate_iconoclast_reading, artisans_and_image_makers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the decalogue as forbidding three-dimensional cult statuary while permitting two-dimensional images under strict regulatory conditions. Sets the criteria for permissible images, monitors worship communities and artisans for compliance, and adjudicates violations. Derives institutional legitimacy from continuity with the scriptural kernel and maintains interpretive monopoly over material mediation.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, clerical_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Must submit their devotional material culture to clerical oversight. Are barred from using three-dimensional religious statuary in worship and must ensure any two-dimensional images conform to regulatory standards. Bear the compliance costs of monitoring, education, and restricted devotional expression.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, worship_communities, payer,
    powerless, biographical, constrained, local).

% Subject to production regulation and inspection of religious imagery. Prohibited from crafting three-dimensional devotional statues; two-dimensional works require approval or licensing. Livelihood and creative practice are constrained by the authority's gatekeeping criteria.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, artisans_and_image_makers, payer,
    moderate, biographical, constrained, local).

% Hold that material mediationâincluding three-dimensional iconsâis a sanctified conduit to the divine. Their reading of the kernel is structurally excluded from the regulatory conversation; they are labeled as doctrinally deviant and their practices are suppressed rather than accommodated.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, iconodule_communities, excluded,
    moderate, generational, trapped, regional).

% Analyze the philological, historical, and comparative evidence for the dimensional distinction between three-dimensional and two-dimensional images in ancient Near Eastern texts. Do not administer or bear the constraint, but provide external assessment of its textual grounding.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, textual_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(decalogue_image_prohibition__moderate_iconoclast_reading, clerical_authority).
narrative_ontology:fixing_cost_class(decalogue_image_prohibition__moderate_iconoclast_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents idolatry by distinguishing higher-risk three-dimensional cult images from lower-risk two-dimensional representations, providing a regulated framework for permissible visual culture in worship.
% TRANSFER_FUNCTION: Moves compliance labor, devotional restriction, interpretive submission, and regulatory oversight from worship communities and artisans to the clerical authority, concentrating gatekeeping power in the interpretive center.
% ABSENT_VOICES: Iconodule communities who regard material mediation as sanctifiedâincluding three-dimensional iconsâare structurally excluded from the regulatory conversation. Artisans who would freely produce religious statuary are present as regulated subjects but excluded from rule-setting.
% DISAPPEARANCE_RATIONALE: If the prohibition vanished, worship communities would reintroduce three-dimensional devotional objects, artisans would resume unrestricted religious production, and the clerical authority's interpretive monopoly over material culture would collapse. The reorganized devotional economy would shift toward iconodule or unrestricted material practice.
% FOUNDING_PROBLEM: The risk of idolatry arising from material representations of the divine in worship contexts, particularly from cult statuary that invites sensory confusion between creature and Creator.
% FOUNDING_PROBLEM_CORROBORATION: Independent textual scholars and historians attest that the kernel's original lexical scope is ambiguous regarding dimensional distinctions; iconodule communities from outside the benefiting seat attest that the problem is differently framed and that material mediation is theologically legitimate. The clerical authority's self-attestation is the primary beneficiary claim, but external philological analysis corroborates contestation rather than the authority's specific regulatory distinction.
narrative_ontology:disappearance_verdict(decalogue_image_prohibition__moderate_iconoclast_reading, world_rearranges).
narrative_ontology:founding_problem_status(decalogue_image_prohibition__moderate_iconoclast_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__moderate_iconoclast_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(decalogue_image_prohibition__moderate_iconoclast_reading, 'none', 1).
narrative_ontology:epsilon_provenance(decalogue_image_prohibition__moderate_iconoclast_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(decalogue_image_prohibition__moderate_iconoclast_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(decalogue_image_prohibition__moderate_iconoclast_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(decalogue_image_prohibition__moderate_iconoclast_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint decouples regulatory burden from proportional worship-protection benefit: the monitoring and permission structures impose persistent costs on communities and artisans. Suppression (0.68) is substantial because the constraint requires active enforcementâdistinguishing forbidden 3D from permitted 2D, inspecting images for abuse, and excluding iconodule alternatives. Theater_ratio (0.50) is elevated because the regulatory apparatus performs diligence that increasingly serves the gatekeeping function itself rather than the original idolatry-prevention goal. Accessibility_collapse (0.60) reflects that while iconodule alternatives exist historically, they are suppressed within the regulatory framework. Resistance (0.55) captures moderate but persistent pushback from iconodule communities and artisans. Temporal measurements trace rising extraction and theater over the interval as the regulatory apparatus elaborates.
 *
 * PERSPECTIVAL GAP:
 *   The clerical authority experiences the constraint as legitimate coordinationâprotecting worship from idolatry through necessary interpretive oversight. Worship communities and artisans experience it as extraction: they surrender autonomy over devotional material culture and labor under surveillance. The engine computes this divergence from the structural data (agenda_setter with arbitrage exit versus payers with constrained exit).
 *
 * DIRECTIONALITY LOGIC:
 *   The clerical authority is the declared beneficiary and agenda_setter; its directionality sits near the beneficiary end (low d), reducing effective extraction or inverting it into interpretive subsidy. Worship communities and artisans are declared payers with constrained exit, placing their directionality near the target end (high d), amplifying effective extraction. Iconodule communities are excluded rather than coordinated; their exclusion is the enforcement object itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint claims a coordination function (preventing idolatry), which prevents simple snare classification without examining whether the coordination is genuine or cover. The authored metrics treat the coordination story as partially real but increasingly theatrical: the 3D/2D distinction does map onto different sensory engagement, but the elaborate regulatory overlay exceeds what the distinction itself demands. The mandatrophy test is satisfied by the mismatch between founding_problem_status (contested) and disappearance_verdict (world_rearranges): the arrangement persists beyond its contested founding justification, and its disappearance would force material reorganization of worship practice, indicating the constraint has become load-bearing for the authority structure rather than merely protective.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dimensional_distinction_warrant,
    'Does the kernel text (Decalogue) theologically warrant the three-dimensional/two-dimensional distinction, or is this distinction a post-hoc justification for regulatory gatekeeping?',
    'Philological analysis of the Hebrew text and comparative ancient Near Eastern evidence; historical examination of when the dimensional distinction emerged in interpretive tradition.',
    'If the distinction is not textually grounded, the constraint''s coordination story collapses and the classification shifts toward pure extraction; if grounded, part of the regulatory overhead may be reclassified as coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dimensional_distinction_warrant, conceptual, 'Whether the 3D/2D boundary is textually inherent or interpretively constructed').

omega_variable(
    regulatory_cost_benefit_ratio,
    'Does the clerical regulation of two-dimensional images produce measurable reduction in idolatrous practice, or does the cost of compliance exceed the protective benefit?',
    'Historical case studies of communities with and without such regulation; anthropological analysis of devotional practice under varying degrees of material restriction.',
    'If the regulatory cost exceeds the protective benefit, the coordination function is cover for extraction, supporting snare classification; if proportional, the constraint may reclassify as tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_cost_benefit_ratio, empirical, 'Whether regulation produces proportional worship protection').

omega_variable(
    gatekeeping_power_concentration,
    'Is the interpretive monopoly over permissible images concentrated in the clerical authority as a necessary feature of theological order, or as a transferable power resource?',
    'Comparative analysis across religious traditions with varying degrees of lay interpretive access; examination of reform movements that redistributed interpretive authority.',
    'If the gatekeeping is transferable and contested, the constraint functions as a snare capturing interpretive power; if structurally necessary, it may be tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeping_power_concentration, conceptual, 'Whether clerical gatekeeping is structurally necessary or power-extractive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__moderate_iconoclast_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deca_tr_t0, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(deca_tr_t6, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 6, 0.26).
narrative_ontology:measurement(deca_tr_t12, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 12, 0.32).
narrative_ontology:measurement(deca_tr_t18, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 18, 0.4).
narrative_ontology:measurement(deca_tr_t24, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 24, 0.45).
narrative_ontology:measurement(deca_tr_t30, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 30, 0.5).

% Extraction over time
narrative_ontology:measurement(deca_be_t0, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(deca_be_t6, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(deca_be_t12, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(deca_be_t18, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 18, 0.64).
narrative_ontology:measurement(deca_be_t24, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement(deca_be_t30, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 30, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(deca_su_t0, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(deca_su_t6, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 6, 0.48).
narrative_ontology:measurement(deca_su_t12, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 12, 0.54).
narrative_ontology:measurement(deca_su_t18, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 18, 0.6).
narrative_ontology:measurement(deca_su_t24, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 24, 0.65).
narrative_ontology:measurement(deca_su_t30, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(decalogue_image_prohibition__moderate_iconoclast_reading, iconoclast_reading).
narrative_ontology:affects_constraint(decalogue_image_prohibition__moderate_iconoclast_reading, iconodule_reading).

% DUAL FORMULATION NOTE:
% The decalogue_image_prohibition kernel decomposes into three structurally distinct constraints: the strict iconoclast reading (forbids all images), the moderate iconoclast reading (forbids 3D, regulates 2D), and the iconodule reading (permits veneration of images including 3D). Each reading has a distinct epsilon, beneficiary/victim structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
