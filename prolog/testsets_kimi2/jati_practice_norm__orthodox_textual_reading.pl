% ============================================================================
% CONSTRAINT STORY: jati_practice_norm__orthodox_textual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jati_practice_norm__orthodox_textual_reading, []).

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
 *   constraint_id: jati_practice_norm__orthodox_textual_reading
 *   human_readable: Orthodox Textual Jati-Varna Hierarchy
 *   domain: social/religious/political_economy
 *
 * SUMMARY:
 *   This constraint instantiates the orthodox textual reading of the jati
 *   practice norm kernel. It holds that jati boundaries are fixed derivations
 *   of an ancient scriptural varna framework, enforced through the concept of
 *   ritual pollution. Brahminical authorities monopolize textual
 *   interpretation, while dominant caste elites capture economic and status
 *   benefits. Dalit communities and lower-jati laborers bear the extraction
 *   through assigned polluting occupations and blocked mobility. The
 *   constraint is claimed as natural-cosmic order by its beneficiaries; the
 *   authored metrics treat it as a constructed snare with high extraction and
 *   active suppression. This divergence is intentional and measured by the
 *   engine.
 *
 * KEY AGENTS:
 *   - brahminical_authority: Primary agenda-setter (institutional/mobile) â interprets scripture and enforces pollution norms.
 *   - dominant_caste_elites: Primary beneficiary (powerful/mobile) â captures status and labor surplus from categorical rigidity.
 *   - dalit_communities: Primary target (powerless/identity_locked) â assigned polluting occupations, excluded from public and ritual space.
 *   - lower_jati_laborers: Secondary target (powerless/trapped) â occupational immobility and deference extraction.
 *   - anti_caste_reformers: Excluded voice (moderate/constrained) â objects to the framework but is ritually and politically marginalized.
 *   - colonial_administrators: External observer/excluded (institutional/analytical) â their census categories are rejected as illegitimate in this reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_practice_norm__orthodox_textual_reading, 0.85).
domain_priors:suppression_score(jati_practice_norm__orthodox_textual_reading, 0.8).
domain_priors:theater_ratio(jati_practice_norm__orthodox_textual_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__orthodox_textual_reading, snare).
narrative_ontology:human_readable(jati_practice_norm__orthodox_textual_reading, "Orthodox Textual Jati-Varna Hierarchy").
narrative_ontology:topic_domain(jati_practice_norm__orthodox_textual_reading, "social/religious/political_economy").

domain_priors:requires_active_enforcement(jati_practice_norm__orthodox_textual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__orthodox_textual_reading, '360ab1ab-addb-4703-a48e-864c125e5474').
narrative_ontology:cs_kernel_codification('360ab1ab-addb-4703-a48e-864c125e5474', fixed_text).
narrative_ontology:cs_authority_grounding('360ab1ab-addb-4703-a48e-864c125e5474', lineage).
narrative_ontology:cs_interpretation_layer_present('360ab1ab-addb-4703-a48e-864c125e5474').
narrative_ontology:cs_reading_relation('360ab1ab-addb-4703-a48e-864c125e5474', jati_practice_norm__localized_practice_reading, forecloses).
narrative_ontology:cs_reading_relation('360ab1ab-addb-4703-a48e-864c125e5474', jati_practice_norm__colonial_census_reading, influences).
narrative_ontology:cs_axiom('360ab1ab-addb-4703-a48e-864c125e5474', foundational, varna_jati_scriptural_fixity).
narrative_ontology:cs_axiom_status(varna_jati_scriptural_fixity, holdable).
narrative_ontology:cs_axiom_grounding('360ab1ab-addb-4703-a48e-864c125e5474', varna_jati_scriptural_fixity, theological).
narrative_ontology:cs_axiom('360ab1ab-addb-4703-a48e-864c125e5474', foundational, deviation_as_ritual_pollution).
narrative_ontology:cs_axiom_status(deviation_as_ritual_pollution, holdable).
narrative_ontology:cs_axiom_grounding('360ab1ab-addb-4703-a48e-864c125e5474', deviation_as_ritual_pollution, theological).
narrative_ontology:cs_reference_frame('360ab1ab-addb-4703-a48e-864c125e5474', scriptural_varna_order).
narrative_ontology:cs_drift_state('360ab1ab-addb-4703-a48e-864c125e5474', post_independent_reform_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('360ab1ab-addb-4703-a48e-864c125e5474', '').
narrative_ontology:cs_kernel_id(jati_practice_norm__orthodox_textual_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__orthodox_textual_reading, brahminical_authority).
narrative_ontology:constraint_beneficiary(jati_practice_norm__orthodox_textual_reading, dominant_caste_elites).
narrative_ontology:constraint_victim(jati_practice_norm__orthodox_textual_reading, dalit_communities).
narrative_ontology:constraint_victim(jati_practice_norm__orthodox_textual_reading, lower_jati_laborers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Monopolizes the interpretation of Sanskrit textual sources linking varna to jati, adjudicates disputes over ritual purity and pollution, and receives economic patronage and social deference from communities seeking ritual legitimacy.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, brahminical_authority, agenda_setter,
    institutional, generational, mobile, national).

% Hold land and local political power within the jati hierarchy, benefit from the blocked mobility of laboring communities below them, and underwrite the enforcement of endogamy and occupational boundaries.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, dominant_caste_elites, beneficiary,
    powerful, generational, mobile, regional).

% Assigned occupations deemed ritually polluting, barred from temple entry and shared water sources, and socially obligated to perform deference; their jati status is fused with stigmatized identity.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, dalit_communities, payer,
    powerless, generational, identity_locked, local).

% Restricted to traditional occupations by endogamy and purity norms, economically dependent on upper-caste patrons, with limited geographic or social mobility due to the ritualized division of labor.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, lower_jati_laborers, payer,
    powerless, generational, trapped, local).

% Advocate for the abolition of jati boundaries and the equality of Dalits; they are structurally excluded from Brahminical interpretive authority and their positions are dismissed as heretical or Western-influenced.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, anti_caste_reformers, excluded,
    moderate, biographical, constrained, national).

% Operated census and gazetteer projects that classified jati populations for governance; within the orthodox textual reading, their categories are treated as illegitimate external impositions that distort the scriptural order.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, colonial_administrators, excluded,
    institutional, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jati_practice_norm__orthodox_textual_reading, diffuse).
narrative_ontology:fixing_cost_class(jati_practice_norm__orthodox_textual_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social reproduction by assigning fixed ritual-occupational statuses derived from scriptural varna categories, thereby preventing inter-jati contact that the framework defines as cosmically polluting.
% TRANSFER_FUNCTION: Moves labor, deference, and economic surplus from Dalit and lower-jati communities to upper-jati elites and Brahminical institutions, while concentrating interpretive authority over social classification in the Brahminical textual tradition.
% ABSENT_VOICES: Dalit reformers, anti-caste intellectuals, and practitioners of fluid local jati customs are excluded from the textual interpretive process; their advocacy is treated as heretical or ritually polluting.
% DISAPPEARANCE_RATIONALE: If the fixed varna-jati linkage and its pollution sanctions disappeared, occupational monopolies would break, temple access and public space usage would shift, endogamy norms would destabilize, and the Brahminical interpretive monopoly would collapse; the social order organized around this reading would reorganize rapidly.
% FOUNDING_PROBLEM: Maintaining cosmic and social order by preventing the ritual pollution thought to arise from inter-varna mixing, and organizing labor specialization in an agrarian society.
% FOUNDING_PROBLEM_CORROBORATION: Brahminical textual traditions and dominant-caste institutions attest the problem. Anti-caste scholars, Dalit movements, and secular historians outside the beneficiary set attest that the problem was manufactured to justify labor extraction and status enclosure; no independent corroboration exists outside the benefiting parties.
narrative_ontology:disappearance_verdict(jati_practice_norm__orthodox_textual_reading, world_rearranges).
narrative_ontology:founding_problem_status(jati_practice_norm__orthodox_textual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__orthodox_textual_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jati_practice_norm__orthodox_textual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jati_practice_norm__orthodox_textual_reading, 0.85, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jati_practice_norm__orthodox_textual_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jati_practice_norm__orthodox_textual_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jati_practice_norm__orthodox_textual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.85) is high because the constraint systematically transfers labor, deference, and opportunity from lower to upper jatis. Suppression (0.80) reflects active enforcement through ritual ostracism, violence, and pollution norms. Theater_ratio (0.50 at interval end) captures the performative maintenance of purity rules under modern legal abolition. Accessibility_collapse (0.80) is high because identity-lock and pollution ideology make exit nearly unthinkable for targets. Resistance (0.55) is moderate because anti-caste movements persist but face structural suppression. Measurements share a single time grid (0â100) to prevent misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   The Brahminical and dominant-caste seats should compute as low directionality (beneficiaries of a naturalized order), while Dalit and lower-jati seats should compute as high directionality (targets of extraction). The colonial and reformer seats sit at intermediate analytical distances. The engine will compute per-seat types: the beneficiary seats may see a rope or mountain (coordination/cosmic order), while target seats will see snare or tangled_rope; this divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahminical authority and dominant caste elites are named in beneficiaries and have mobile exit; derived d is near 0.0 (beneficiary). Dalit communities and lower jati laborers are named in victims with trapped/identity_locked exit; derived d is near 1.0 (target). Anti-caste reformers are excluded but structurally opposed; colonial administrators are external observers with analytical exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (ritual purity maintenance) is contested: anti-caste scholars argue it was always a post-hoc justification for extraction. The constraint persists with substantial theater, but the orthodox reading denies obsolescence. The mandatrophy signal is therefore suppressed within the reading itself, while external observers detect drift between the reference frame (scriptural varna order) and contemporary practice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scriptural_naturalness_ambiguity,
    'Does the varna-jati linkage represent a genuine natural-law or divine ontological order, or is it a constructed constraint that benefits identifiable upper-jati and Brahminical parties?',
    'Historical-critical philology and archaeological sociology demonstrating the anachronistic projection of later jati categories onto early varna texts; or convergent textual evidence of an unbroken ancient linkage.',
    'If constructed, the constraint reclassifies as snare or tangled_rope; if genuinely scriptural-natural, it would approach a mountain-like false summit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scriptural_naturalness_ambiguity, empirical, 'Whether the textual basis is constructed or genuinely ancient.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (enforced by upper-jati institutions and violence) or internalized (lower-jati subjects believe in their own ritual impurity)?',
    'Post-reform suppression trajectory: if caste discrimination persists after legal abolition and economic restructuring, suppression is partially internalized.',
    'If internalized, effective extraction exceeds structural measures because targets carry the constraint after formal exit routes open.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism.').

omega_variable(
    colonial_reification_role,
    'To what extent does the contemporary rigidity of jati boundaries depend on colonial census reification rather than pre-colonial textual orthodoxy?',
    'Comparative historical analysis of jati fluidity indices pre- and post-colonial administrative fixation.',
    'If colonial reification is the primary source of current rigidity, the orthodox textual reading overclaims scriptural continuity and the constraint''s effective extraction may trace to a different genealogy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colonial_reification_role, empirical, 'Colonial administrative contribution to current boundary rigidity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__orthodox_textual_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_orthodox_tr_t0, jati_practice_norm__orthodox_textual_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(jati_orthodox_tr_t20, jati_practice_norm__orthodox_textual_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(jati_orthodox_tr_t40, jati_practice_norm__orthodox_textual_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(jati_orthodox_tr_t60, jati_practice_norm__orthodox_textual_reading, theater_ratio, 60, 0.38).
narrative_ontology:measurement(jati_orthodox_tr_t80, jati_practice_norm__orthodox_textual_reading, theater_ratio, 80, 0.45).
narrative_ontology:measurement(jati_orthodox_tr_t100, jati_practice_norm__orthodox_textual_reading, theater_ratio, 100, 0.5).

% Extraction over time
narrative_ontology:measurement(jati_orthodox_be_t0, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 0, 0.8).
narrative_ontology:measurement(jati_orthodox_be_t20, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 20, 0.81).
narrative_ontology:measurement(jati_orthodox_be_t40, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 40, 0.83).
narrative_ontology:measurement(jati_orthodox_be_t60, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 60, 0.84).
narrative_ontology:measurement(jati_orthodox_be_t80, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 80, 0.85).
narrative_ontology:measurement(jati_orthodox_be_t100, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 100, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(jati_orthodox_su_t0, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(jati_orthodox_su_t20, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 20, 0.78).
narrative_ontology:measurement(jati_orthodox_su_t40, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 40, 0.8).
narrative_ontology:measurement(jati_orthodox_su_t60, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 60, 0.76).
narrative_ontology:measurement(jati_orthodox_su_t80, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 80, 0.79).
narrative_ontology:measurement(jati_orthodox_su_t100, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 100, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(jati_practice_norm__orthodox_textual_reading, jati_practice_norm__localized_practice_reading).
narrative_ontology:affects_constraint(jati_practice_norm__orthodox_textual_reading, jati_practice_norm__colonial_census_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the jati_practice_norm kernel. The orthodox textual reading claims ancient scriptural fixity with high extraction; the localized practice reading claims continuous renegotiation; the colonial census reading claims external administrative reification. Each reading instantiates a structurally distinct constraint with its own epsilon and stakeholder geometry.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
