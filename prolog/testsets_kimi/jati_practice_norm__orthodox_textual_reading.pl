% ============================================================================
% CONSTRAINT STORY: jati_practice_norm__orthodox_textual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   human_readable: Jati Boundaries as Fixed Scriptural Varna Order
 *   domain: social/religious/political_economy
 *
 * SUMMARY:
 *   This constraint instantiates the orthodox textual reading of the
 *   jati_practice_norm kernel: the claim that jati boundaries derive from a
 *   fixed scriptural varna framework, deviation from which constitutes ritual
 *   pollution. In this reading, lower jaatis are assigned hereditary
 *   polluting occupations and blocked from mobility by the doctrine of ritual
 *   pollution. The Brahminical interpretive class and upper varna jaatis
 *   benefit from the categorical rigidity. This is one of three readings; the
 *   localized practice reading sees continuous renegotiation, while the
 *   colonial census reading sees external administrative reification. Sibling
 *   readings are modeled as separate constraints linked in a family network.
 *
 * KEY AGENTS:
 *   - brahminical_interpreters: Primary agenda-setter (institutional/identity_locked) â administers scriptural interpretation and ritual legitimacy
 *   - upper_varna_jaatis: Primary beneficiary (powerful/constrained) â accrues ritual and material privilege from fixed hierarchy
 *   - lower_polluting_jaatis: Primary target (powerless/trapped) â bears hereditary pollution assignment and blocked mobility
 *   - local_renegotiators: Excluded voice (moderate/constrained) â would renegotiate boundaries but suppressed by textual fixity claims
 *   - social_reform_analysts: Analytical observer (analytical/analytical) â documents structural extraction from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_practice_norm__orthodox_textual_reading, 0.85).
domain_priors:suppression_score(jati_practice_norm__orthodox_textual_reading, 0.9).
domain_priors:theater_ratio(jati_practice_norm__orthodox_textual_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__orthodox_textual_reading, snare).
narrative_ontology:human_readable(jati_practice_norm__orthodox_textual_reading, "Jati Boundaries as Fixed Scriptural Varna Order").
narrative_ontology:topic_domain(jati_practice_norm__orthodox_textual_reading, "social/religious/political_economy").

domain_priors:requires_active_enforcement(jati_practice_norm__orthodox_textual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__orthodox_textual_reading, 'eb5b942b-2ce0-40d3-a559-cf72f6a5a392').
narrative_ontology:cs_kernel_codification('eb5b942b-2ce0-40d3-a559-cf72f6a5a392', fixed_text).
narrative_ontology:cs_authority_grounding('eb5b942b-2ce0-40d3-a559-cf72f6a5a392', lineage).
narrative_ontology:cs_interpretation_layer_present('eb5b942b-2ce0-40d3-a559-cf72f6a5a392').
narrative_ontology:cs_reading_relation('eb5b942b-2ce0-40d3-a559-cf72f6a5a392', jati_practice_norm__localized_practice_reading, forecloses).
narrative_ontology:cs_reading_relation('eb5b942b-2ce0-40d3-a559-cf72f6a5a392', jati_practice_norm__colonial_census_reading, influences).
narrative_ontology:cs_axiom('eb5b942b-2ce0-40d3-a559-cf72f6a5a392', foundational, varna_origin_scriptural_fixity).
narrative_ontology:cs_axiom_status(varna_origin_scriptural_fixity, holdable).
narrative_ontology:cs_axiom_grounding('eb5b942b-2ce0-40d3-a559-cf72f6a5a392', varna_origin_scriptural_fixity, theological).
narrative_ontology:cs_axiom('eb5b942b-2ce0-40d3-a559-cf72f6a5a392', foundational, pollution_doctrine_enforceable_cosmic_threat).
narrative_ontology:cs_axiom_status(pollution_doctrine_enforceable_cosmic_threat, holdable).
narrative_ontology:cs_axiom_grounding('eb5b942b-2ce0-40d3-a559-cf72f6a5a392', pollution_doctrine_enforceable_cosmic_threat, theological).
narrative_ontology:cs_reference_frame('eb5b942b-2ce0-40d3-a559-cf72f6a5a392', scriptural_varna_cosmology).
narrative_ontology:cs_drift_state('eb5b942b-2ce0-40d3-a559-cf72f6a5a392', contemporary_modernity, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('eb5b942b-2ce0-40d3-a559-cf72f6a5a392', '').
narrative_ontology:cs_kernel_id(jati_practice_norm__orthodox_textual_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__orthodox_textual_reading, brahminical_interpreters).
narrative_ontology:constraint_beneficiary(jati_practice_norm__orthodox_textual_reading, upper_varna_jaatis).
narrative_ontology:constraint_victim(jati_practice_norm__orthodox_textual_reading, lower_polluting_jaatis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold exclusive authority to interpret scriptural varna classifications and adjudicate ritual purity violations. Their institutional identity is constituted by the constraint; abandoning textual fixity would dissolve their role as gatekeepers of sacred knowledge and ritual legitimacy.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, brahminical_interpreters, agenda_setter,
    institutional, generational, identity_locked, regional).

% Occupy the privileged tiers of the varna hierarchy. Their social and economic standing is protected by the scriptural order and the pollution doctrine, which blocks upward mobility from below. They benefit from hereditary access to land, education, and ritual status, and from the segregated labor of service communities.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, upper_varna_jaatis, beneficiary,
    powerful, generational, constrained, regional).

% Assigned hereditary occupations deemed ritually polluting by the scriptural framework. Excluded from temples, common water sources, and inter-dining. Mobility is blocked because any departure from the assigned role is read as ritual pollution threatening cosmic and social order. They bear the material and symbolic costs of the hierarchy.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, lower_polluting_jaatis, payer,
    powerless, generational, trapped, local).

% Village-level actors and fluid occupational groups who would renegotiate jati standing based on economic mobility or local power shifts. Their attempts at reclassification are suppressed by the orthodox textual insistence on fixed scriptural categories; they are denied voice in the scriptural interpretive process.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, local_renegotiators, excluded,
    moderate, biographical, constrained, local).

% Historians, anthropologists, and anti-caste intellectuals who document the gap between scriptural claims and historical practice. They observe the constraint from outside the ritual economy and attest to its extractive effects.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, social_reform_analysts, observer,
    analytical, civilizational, analytical, national).

narrative_ontology:fixing_cost_class(jati_practice_norm__orthodox_textual_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Presents itself as maintaining cosmic and social order through alignment of human society with an eternal scriptural hierarchy, ensuring ritual purity and dharmic stability by fixing each community's duties and interactions.
% TRANSFER_FUNCTION: Moves hereditary labor, material surplus, and symbolic deference from lower jaatis assigned polluting occupations to upper varna groups; transfers interpretive authority and ritual legitimacy to Brahminical gatekeepers.
% ABSENT_VOICES: Local renegotiators who would reclassify boundaries based on practice; colonial administrators reading jati through census legibility; subaltern voices whose mobility is blocked by the pollution doctrine. They are excluded from scriptural interpretation and ritual adjudication.
% DISAPPEARANCE_RATIONALE: If the fixed scriptural varna framework and its pollution doctrine vanished, hereditary occupational assignments would lose their ritual backing, temple and water-source exclusion would collapse, and the entire material and symbolic economy of caste would reorganize around different principles of stratification.
% FOUNDING_PROBLEM: How to maintain ritual purity and cosmic order in a society understood as hierarchically ordered by divine injunction, preventing the pollution that arises from intermixture of varnas and duties.
% FOUNDING_PROBLEM_CORROBORATION: Brahminical interpreters assert the problem is eternally live as a matter of dharma. Social reform analysts and anti-caste movements attest that the 'founding problem' is a post-hoc theological rationalization for material extraction; no corroboration exists from outside the benefiting parties that the cosmic order genuinely requires this specific hierarchical fixity. Colonial ethnographers documented local fluidity that contradicts the scriptural rigidity claim.
narrative_ontology:disappearance_verdict(jati_practice_norm__orthodox_textual_reading, world_rearranges).
narrative_ontology:founding_problem_status(jati_practice_norm__orthodox_textual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__orthodox_textual_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.85 at interval end) because the constraint systematically transfers labor, deference, and material surplus from lower to upper groups while blocking mobility. Suppression is very high (0.90) because the pollution doctrine is enforced through temple exclusion, social boycott, and hereditary occupational lock-in â alternatives are actively suppressed, not merely unavailable. Theater ratio rises from 0.25 to 0.62 over the interval: in the earlier period the ritual function was more fully believed and performed; as modernity and reform movements challenge the system, an increasing share of enforcement activity becomes theatrical maintenance of a crumbling cosmology. Accessibility collapse is high (0.80) because once the scriptural framework is accepted, there is no legitimate path out of assigned jati status â the alternatives are ritually polluting and socially fatal. Resistance is moderate (0.60) because subaltern movements and reformers have continually challenged the system, but the suppression machinery has largely contained them. The claim of snare reflects the assessment that the coordination story (cosmic order) is cover for extraction; the metrics are authored independently to describe the constraint's actual operation.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (brahminical_interpreters) experiences the constraint as a sacred duty and a genuine cosmological necessity; from this seat the arrangement appears as mountain-like natural law. The beneficiary seat (upper_varna_jaatis) experiences it as legitimate social order that happens to confer privilege. The payer seat (lower_polluting_jaatis) experiences it as an inescapable extraction mechanism enforced through ritual terror. The engine computes this divergence from the same structural data: high suppression and blocked exit for the trapped powerless seat yield high effective extraction, while the institutional agenda-setter with identity-locked exit sits at a much lower directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahminical interpreters are agenda-setters with identity-locked exit: their authority is constituted by the constraint, giving them very low directionality (near-beneficiary). Upper varna jaatis are pure beneficiaries with constrained exit: low directionality, their effective extraction is damped. Lower polluting jaatis are trapped payers: very high directionality near full target, and because their spatial scope is local while the constraint operates regionally, their effective extraction is amplified. Local renegotiators are excluded and constrained: high directionality, though their moderate power slightly moderates effective extraction compared to the powerless payers.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling by testing the coordination claim against the beneficiary structure and enforcement pattern. If this were genuine coordination (rope or scaffold), we would expect symmetric benefits or a sunset clause, and victims would be absent. Instead, the constraint has clear victims (lower polluting jaatis), concentrated beneficiaries (upper varna groups and interpretive authorities), and requires active enforcement through ritual boycott and exclusion â the signature of snare. The founding problem (ritual purity) is contested and corroborated only by the benefiting parties; no external corroboration exists. This indicates the mandate is either cover or obsolete, and the persistence is driven by the extraction it enables rather than the problem it claims to solve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scriptural_origin_contest,
    'Is the varna-jati linkage a genuinely scriptural and primordial framework, or a retroactive textual anchoring of historically fluid social boundaries?',
    'Historical philology and epigraphic analysis tracing jati codification against textual stratification; comparison of regional scriptural variants to test for uniformity.',
    'If retroactive, the constraint''s claimed mountain-like naturality collapses and the orthodox reading reclassifies toward snare or tangled rope; if genuinely primordial and uniformly scriptural, the extraction reading weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scriptural_origin_contest, empirical, 'Whether the scriptural varna framework is historically anterior or posterior to the jati system it claims to ground.').

omega_variable(
    suppression_mechanism_mix,
    'Is the suppression of deviation primarily internalized through ritual belief, or enforced through external social and economic boycott?',
    'Comparative analysis of communities where religious belief in pollution has declined but occupational segregation persists; if segregation persists without belief, suppression is substantially external/structural.',
    'If internalized, the constraint''s effective suppression exceeds the structural measure; if external, suppression depends on continued enforcement capacity and is more vulnerable to institutional reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_mix, empirical, 'Internalized versus external suppression mechanism').

omega_variable(
    colonial_mediation_ambiguity,
    'Did British colonial census and administrative practice construct the rigid jati boundaries this reading treats as scripturally fixed, or merely reify pre-existing textual categories?',
    'Archival analysis of pre-colonial local records versus colonial census enumerations; comparison of jati categories before and after administrative stabilization.',
    'If colonial construction is substantial, the orthodox textual reading''s claim of scriptural fixity is undermined, strengthening the colonial census reading and weakening this constraint''s legitimacy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(colonial_mediation_ambiguity, conceptual, 'Colonial administrative role in jati rigidity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__orthodox_textual_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_orthodox_tr_t0, jati_practice_norm__orthodox_textual_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(jati_orthodox_tr_t20, jati_practice_norm__orthodox_textual_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement(jati_orthodox_tr_t40, jati_practice_norm__orthodox_textual_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(jati_orthodox_tr_t60, jati_practice_norm__orthodox_textual_reading, theater_ratio, 60, 0.48).
narrative_ontology:measurement(jati_orthodox_tr_t80, jati_practice_norm__orthodox_textual_reading, theater_ratio, 80, 0.55).
narrative_ontology:measurement(jati_orthodox_tr_t100, jati_practice_norm__orthodox_textual_reading, theater_ratio, 100, 0.62).

% Extraction over time
narrative_ontology:measurement(jati_orthodox_be_t0, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 0, 0.78).
narrative_ontology:measurement(jati_orthodox_be_t20, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 20, 0.8).
narrative_ontology:measurement(jati_orthodox_be_t40, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 40, 0.82).
narrative_ontology:measurement(jati_orthodox_be_t60, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 60, 0.83).
narrative_ontology:measurement(jati_orthodox_be_t80, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 80, 0.84).
narrative_ontology:measurement(jati_orthodox_be_t100, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 100, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(jati_orthodox_su_t0, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(jati_orthodox_su_t20, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 20, 0.78).
narrative_ontology:measurement(jati_orthodox_su_t40, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 40, 0.82).
narrative_ontology:measurement(jati_orthodox_su_t60, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 60, 0.85).
narrative_ontology:measurement(jati_orthodox_su_t80, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 80, 0.88).
narrative_ontology:measurement(jati_orthodox_su_t100, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 100, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(jati_practice_norm__orthodox_textual_reading, jati_practice_norm__localized_practice_reading).
narrative_ontology:affects_constraint(jati_practice_norm__orthodox_textual_reading, jati_practice_norm__colonial_census_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the jati_practice_norm kernel. The orthodox textual reading claims scriptural fixity; the localized practice reading claims continuous renegotiation; the colonial census reading claims external administrative reification. They are linked as a constraint family because the natural-language label 'jati' conflates these structurally distinct claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
