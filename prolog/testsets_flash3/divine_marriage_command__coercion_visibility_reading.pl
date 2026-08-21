% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__coercion_visibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command__coercion_visibility_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: divine_marriage_command__coercion_visibility_reading
 *   human_readable: Divine Marriage Command (Coercion Visibility Reading)
 *   domain: religious/political_theology
 *
 * SUMMARY:
 *   This constraint represents a reading of the 'divine marriage command'
 *   kernel where the Manifesto (a declaration ending polygamy) is explicitly
 *   acknowledged as a response to federal coercion, and the theological
 *   legitimacy of the doctrinal shift is derived from the necessity of
 *   institutional survival. This reading closes the M-set gap by admitting
 *   exogenous pressure as a valid input for doctrinal change, potentially
 *   leading to a legitimacy crisis for those who believe revelation should be
 *   uncoerced. The constraint operates as a Tangled Rope, coordinating the
 *   church's legal status while extracting compliance from adherents and
 *   purists.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__coercion_visibility_reading, 0.65).
domain_priors:suppression_score(divine_marriage_command__coercion_visibility_reading, 0.7).
domain_priors:theater_ratio(divine_marriage_command__coercion_visibility_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__coercion_visibility_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__coercion_visibility_reading, "Divine Marriage Command (Coercion Visibility Reading)").
narrative_ontology:topic_domain(divine_marriage_command__coercion_visibility_reading, "religious/political_theology").

domain_priors:requires_active_enforcement(divine_marriage_command__coercion_visibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__coercion_visibility_reading, '1ca0d34f-bed8-4aff-b595-f4455ab49b94').
narrative_ontology:cs_kernel_codification('1ca0d34f-bed8-4aff-b595-f4455ab49b94', formalized).
narrative_ontology:cs_authority_grounding('1ca0d34f-bed8-4aff-b595-f4455ab49b94', extraction).
narrative_ontology:cs_interpretation_layer_present('1ca0d34f-bed8-4aff-b595-f4455ab49b94').
narrative_ontology:cs_reading_relation('1ca0d34f-bed8-4aff-b595-f4455ab49b94', divine_marriage_command__continuationist_reading, coexists_with).
narrative_ontology:cs_reading_relation('1ca0d34f-bed8-4aff-b595-f4455ab49b94', divine_marriage_command__substitutionist_reading, coexists_with).
narrative_ontology:cs_axiom('1ca0d34f-bed8-4aff-b595-f4455ab49b94', foundational, institutional_survival_as_theological_imperative).
narrative_ontology:cs_axiom_status(institutional_survival_as_theological_imperative, holdable).
narrative_ontology:cs_axiom_grounding('1ca0d34f-bed8-4aff-b595-f4455ab49b94', institutional_survival_as_theological_imperative, instrumental).
narrative_ontology:cs_axiom('1ca0d34f-bed8-4aff-b595-f4455ab49b94', foundational, exogenous_pressure_as_valid_doctrinal_input).
narrative_ontology:cs_axiom_status(exogenous_pressure_as_valid_doctrinal_input, holdable).
narrative_ontology:cs_axiom_grounding('1ca0d34f-bed8-4aff-b595-f4455ab49b94', exogenous_pressure_as_valid_doctrinal_input, conventional).
narrative_ontology:cs_reference_frame('1ca0d34f-bed8-4aff-b595-f4455ab49b94', pragmatic_institutional_adaptation).
narrative_ontology:cs_drift_state('1ca0d34f-bed8-4aff-b595-f4455ab49b94', contemporary_theological_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('1ca0d34f-bed8-4aff-b595-f4455ab49b94', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(divine_marriage_command__coercion_visibility_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__coercion_visibility_reading, institutional_church_leadership).
narrative_ontology:constraint_beneficiary(divine_marriage_command__coercion_visibility_reading, mainstream_adherents).
narrative_ontology:constraint_victim(divine_marriage_command__coercion_visibility_reading, polygamous_adherents).
narrative_ontology:constraint_victim(divine_marriage_command__coercion_visibility_reading, doctrinal_purists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the church, including its doctrinal interpretations and policies. This reading acknowledges the Manifesto as a necessary response to federal coercion, prioritizing institutional survival and legal standing. They benefit from the church's continued existence and public acceptance.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, institutional_church_leadership, agenda_setter,
    institutional, generational, constrained, global).

% Historically practiced polygamy as a divine command. Under this reading, they are compelled to abandon or conceal their practice due to the church's shift, facing social ostracization or excommunication if they do not comply. Their identity is deeply tied to the original practice.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, polygamous_adherents, payer,
    powerless, biographical, identity_locked, local).

% Believe in the immutability of divine commands and view any doctrinal shift, especially under external pressure, as a compromise of theological integrity. They bear the cost of cognitive dissonance and potential loss of faith in the leadership's revelatory authority.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, doctrinal_purists, payer,
    moderate, generational, constrained, national).

% Benefit from the church's integration into mainstream society, avoiding legal persecution and social stigma. They accept the Manifesto as a legitimate, albeit difficult, adaptation necessary for the church's mission and growth.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, mainstream_adherents, beneficiary,
    organized, biographical, mobile, global).

% Exerted legal and coercive pressure (e.g., confiscation of property, imprisonment) to enforce monogamy, leading to the Manifesto. Their role is acknowledged as the external force driving the doctrinal shift.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the church's legal and social integration into the broader society by aligning its marriage practices with federal law, ensuring institutional survival and continued missionary work.
% TRANSFER_FUNCTION: Transfers the burden of legal and social non-compliance from the institutional church to individual adherents who previously practiced polygamy, while securing the church's assets and legal standing.
% ABSENT_VOICES: Those who left the church over the Manifesto's perceived compromise of divine command are absent from the current discourse, as are future generations who might question the theological integrity of a shift driven by coercion.
% DISAPPEARANCE_RATIONALE: If the Manifesto and its coercive context vanished, the church's legal status would be immediately challenged, its property potentially seized, and its members subject to arrest. The institutional structure would collapse, and the social fabric of its communities would be severely disrupted, forcing a re-evaluation of its core tenets.
% FOUNDING_PROBLEM: The church faced existential threat from federal anti-polygamy laws, including property confiscation, disenfranchisement, and imprisonment of its leaders and members, jeopardizing its very existence.
% FOUNDING_PROBLEM_CORROBORATION: Historical records, federal court documents, and contemporary journalistic accounts corroborate the severe federal coercion. While the immediate legal threat is gone, the institutional church leadership maintains that the principle of adapting to external pressures for survival remains a live concern, though this is contested by doctrinal purists.
narrative_ontology:disappearance_verdict(divine_marriage_command__coercion_visibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_marriage_command__coercion_visibility_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__coercion_visibility_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(divine_marriage_command__coercion_visibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_marriage_command__coercion_visibility_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__coercion_visibility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_marriage_command__coercion_visibility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_marriage_command__coercion_visibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the shift imposed significant costs on polygamous adherents and doctrinal purists, forcing them to abandon deeply held practices or beliefs. Suppression is also high, as the church actively enforced the new monogamous standard, backed by federal law. The theater ratio is moderate, reflecting the ongoing effort to frame the Manifesto as divinely guided rather than purely pragmatic, even while acknowledging the coercion. The metrics reflect the ongoing tension between the stated theological basis and the pragmatic, coercive origins.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of institutional leadership, the Manifesto was a necessary, albeit difficult, act of prudence to preserve the church. From the perspective of polygamous adherents, it was a betrayal of divine command, forced by external power. The engine's classification will highlight this divergence, showing a 'tangled rope' for those who pay and a 'rope' for those who benefit from the coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional church leadership and mainstream adherents are beneficiaries, gaining legal protection and social acceptance. Polygamous adherents and doctrinal purists are victims, bearing the costs of forced compliance and theological compromise. The federal government acts as an external agenda-setter, whose coercive power shaped the constraint's form.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (institutional survival) is still live, but its theological grounding is contested. This reading prevents mislabeling the shift as pure revelation (a Mountain) by explicitly acknowledging the coercive context, thus revealing the extractive component of the 'coordination' function. The 'dead' status of the founding problem (federal persecution) combined with 'world_rearranges' verdict signals a potential zombie constraint if the coercion visibility is lost, but this reading actively maintains that visibility.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_legitimacy_of_coercion,
    'Can a doctrinal shift explicitly acknowledged as a response to coercion maintain full theological legitimacy within a revelatory framework?',
    'Analysis of theological discourse and adherence rates among doctrinal purists over time; formal church statements on the nature of revelation and institutional adaptation.',
    'If theological legitimacy is significantly eroded, the constraint''s internal stability and long-term persistence are threatened, potentially reclassifying it as a Piton or even a Snare if the coercion is seen as purely extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_legitimacy_of_coercion, conceptual, 'Ambiguity regarding the theological validity of coerced doctrinal change.').

omega_variable(
    m_set_gap_closure_sustainability,
    'Is the closure of the M-set gap (admitting exogenous pressure as doctrinal input) a stable and sustainable interpretive strategy, or does it open the door to further, potentially arbitrary, doctrinal shifts?',
    'Longitudinal study of subsequent doctrinal developments and the arguments used to justify them; comparative analysis with other religious traditions facing similar external pressures.',
    'If unsustainable, the reading could lead to a crisis of authority, potentially fragmenting the church. If stable, it establishes a new precedent for doctrinal evolution, but at the cost of traditional revelatory claims.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(m_set_gap_closure_sustainability, empirical, 'Sustainability of acknowledging exogenous pressure in doctrinal formation.').

omega_variable(
    kernel_reading_identity,
    'This constraint is one reading of the ''divine_marriage_command'' kernel. What would change structurally if a sibling reading were adopted?',
    'Compare the structural properties (beneficiaries, victims, extractiveness, suppression) of this reading with the ''continuationist_reading'' and ''substitutionist_reading'' siblings.',
    'The ''continuationist_reading'' would likely show lower extractiveness from polygamous adherents (as their practice would be seen as suspended, not rescinded), but higher suppression from the federal government. The ''substitutionist_reading'' would show a stronger claim to new revelation, potentially shifting the source of legitimacy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Structural differences between this reading and its siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__coercion_visibility_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_marriage_command__coercion_visibility_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(divi_tr_t25, divine_marriage_command__coercion_visibility_reading, theater_ratio, 25, 0.3).
narrative_ontology:measurement(divi_tr_t50, divine_marriage_command__coercion_visibility_reading, theater_ratio, 50, 0.4).
narrative_ontology:measurement(divi_tr_t75, divine_marriage_command__coercion_visibility_reading, theater_ratio, 75, 0.38).
narrative_ontology:measurement(divi_tr_t100, divine_marriage_command__coercion_visibility_reading, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(divi_be_t25, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 25, 0.6).
narrative_ontology:measurement(divi_be_t50, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 50, 0.65).
narrative_ontology:measurement(divi_be_t75, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 75, 0.63).
narrative_ontology:measurement(divi_be_t100, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 100, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t0, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(divi_su_t25, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(divi_su_t50, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 50, 0.75).
narrative_ontology:measurement(divi_su_t75, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 75, 0.72).
narrative_ontology:measurement(divi_su_t100, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 100, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__coercion_visibility_reading, identity_coordination).
narrative_ontology:affects_constraint(divine_marriage_command__coercion_visibility_reading, divine_marriage_command__continuationist_reading).
narrative_ontology:affects_constraint(divine_marriage_command__coercion_visibility_reading, divine_marriage_command__substitutionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'divine_marriage_command' kernel. This 'coercion_visibility_reading' explicitly acknowledges federal coercion as a driver for the Manifesto, contrasting with the 'continuationist_reading' (polygamy suspended, not rescinded) and 'substitutionist_reading' (monogamy as new revelation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
