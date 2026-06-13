% ============================================================================
% CONSTRAINT STORY: vatican_ii_authority__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_authority__rupture_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: vatican_ii_authority__rupture_reading
 *   human_readable: Vatican II Authority Structure (Rupture Reading)
 *   domain: theological/ecclesiastical
 *
 * SUMMARY:
 *   Vatican II (1962–1965) was a general council of the Catholic Church that
 *   issued sixteen documents reforming liturgy, ecclesiology, doctrine, and
 *   pastoral practice. The rupture reading asserts the Council represents a
 *   substantive break with pre-conciliar doctrine; Council documents contain
 *   errors or irreconcilable contradictions with the deposit of faith; the
 *   post-conciliar settlement imposes this defective reading by institutional
 *   force. This reading is instantiated by the SSPX (Society of St. Pius X),
 *   many traditionalist Catholic communities, and some conservative
 *   theologians. The beneficiary is the post-conciliar modernist theological
 *   faction, which leverages the Council's ambiguous language and papal
 *   authority to advance doctrinal positions incompatible with tradition. The
 *   victims are traditional Catholic communities (identity-locked in
 *   pre-conciliar doctrine) and the pre-conciliar deposit of faith itself
 *   (rendered indefensible by Council authority). The constraint persists
 *   through institutional suppression: canonical penalties against SSPX,
 *   denial of sacraments to traditionalists, control of seminary curricula to
 *   enforce post-conciliar interpretation.
 *
 * KEY AGENTS:
 *   - post_conciliar_modernist_theologians (agenda_setter/beneficiary, institutional power, arbitrage exit) — control interpretive machinery, advance incompatible theology, benefit from Council authority as cover
 *   - traditional_catholic_communities (victim, powerless, identity-locked) — suffer liturgical and doctrinal changes, resist through SSPX, canonically penalized, cannot exit without losing faith-identity
 *   - vatican_institutional_authority (agenda_setter, institutional power, analytical exit) — administers post-conciliar settlement, enforces unified interpretation, claims papal authority over Council meaning
 *   - conciliar_continuity_parties (excluded, institutional power, constrained exit) — bishops and theologians upholding continuity reading, their position is logically foreclosed by the rupture reading's core premise
 *   - vatican_ii_analytical_observers (observer, analytical power, analytical exit) — historians and scholars examining the Council objectively, marginalized in official interpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__rupture_reading, 0.68).
domain_priors:suppression_score(vatican_ii_authority__rupture_reading, 0.71).
domain_priors:theater_ratio(vatican_ii_authority__rupture_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__rupture_reading, snare).
narrative_ontology:human_readable(vatican_ii_authority__rupture_reading, "Vatican II Authority Structure (Rupture Reading)").
narrative_ontology:topic_domain(vatican_ii_authority__rupture_reading, "theological/ecclesiastical").

domain_priors:requires_active_enforcement(vatican_ii_authority__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__rupture_reading, '7f190c64-c1d7-4166-b83c-1f078734198b').
narrative_ontology:cs_kernel_codification('7f190c64-c1d7-4166-b83c-1f078734198b', fixed_text).
narrative_ontology:cs_authority_grounding('7f190c64-c1d7-4166-b83c-1f078734198b', extraction).
narrative_ontology:cs_interpretation_layer_present('7f190c64-c1d7-4166-b83c-1f078734198b').
narrative_ontology:cs_reading_relation('7f190c64-c1d7-4166-b83c-1f078734198b', vatican_ii_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('7f190c64-c1d7-4166-b83c-1f078734198b', vatican_ii_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('7f190c64-c1d7-4166-b83c-1f078734198b', foundational, council_represents_doctrinal_rupture).
narrative_ontology:cs_axiom_status(council_represents_doctrinal_rupture, holdable).
narrative_ontology:cs_axiom_grounding('7f190c64-c1d7-4166-b83c-1f078734198b', council_represents_doctrinal_rupture, empirically_contingent).
narrative_ontology:cs_axiom('7f190c64-c1d7-4166-b83c-1f078734198b', secondary, magisterial_authority_cannot_impose_doctrinal_error).
narrative_ontology:cs_axiom_status(magisterial_authority_cannot_impose_doctrinal_error, holdable).
narrative_ontology:cs_axiom_grounding('7f190c64-c1d7-4166-b83c-1f078734198b', magisterial_authority_cannot_impose_doctrinal_error, deontological).
narrative_ontology:cs_reference_frame('7f190c64-c1d7-4166-b83c-1f078734198b', pre_conciliar_doctrinal_stability).
narrative_ontology:cs_drift_state('7f190c64-c1d7-4166-b83c-1f078734198b', post_conciliar_contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7f190c64-c1d7-4166-b83c-1f078734198b', '').
narrative_ontology:cs_kernel_id(vatican_ii_authority__rupture_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__rupture_reading, post_conciliar_modernist_theologians).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, traditional_catholic_communities).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, pre_conciliar_doctrinal_certainty).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_authority__rupture_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_authority__rupture_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_authority__rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_authority__rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_authority__rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because the post-conciliar settlement leverages Vatican II's doctrinal authority to impose theological change against pre-conciliar understanding; the constraint extracts doctrinal legitimacy from the traditional position and transfers it to modernist theology. Suppression is higher (0.71) because persistence depends on active institutional force: canonical penalties, liturgical suppression, denial of sacraments, control of education — the constraint is sustained by coercion, not by participant acceptance. Theater is moderate-high (0.52) because post-conciliar institutional messaging frames the reforms as 'authentic council interpretation' and 'organic development,' but the rupture reading claims this is theatrical cover for doctrinal break. The measurement series show suppression rising steeply in the first 12 years (immediate post-conciliar period) as resistance coalesces, then stabilizing as traditionalist communities are peripheralized. Theater rises asymptotically, approaching 0.52 as the original reformers age and the post-conciliar settlement becomes institutionalized. Extractiveness plateaus around year 48 because the constraint achieves its primary transfer (doctrinal authority) and additional extraction faces diminishing returns.
 *
 * PERSPECTIVAL GAP:
 *   The modernist beneficiary seat experiences the constraint as liberating reform, restoring the Church to the authentic tradition freed from medieval accretion. The institutional agenda-setter experiences it as necessary updating and exercise of papal authority. The traditional Catholic victim seat experiences it as violent rupture from revealed truth, imposed by institutional force against conscience. The analytical observer seat sees structural ambiguity in the Council's documents that both readings exploit selectively. The engine computes per-seat classifications from these structural asymmetries: from the beneficiary and agenda-setter seats, the constraint may compute as tangled_rope (coordination problem solved, transfer function present); from the victim seat, as snare (pure extraction, coerced acceptance, trapped alternatives). The rupture reading's AUTHORED claim is snare (pure extraction) because it asserts the Council's authority is weaponized against traditional doctrine with no legitimate coordination function.
 *
 * DIRECTIONALITY LOGIC:
 *   Post-conciliar modernist theologians occupy the beneficiary pole (d near 0.0): they collect doctrinal authority and control of the Church's teaching machinery; institutional power grants them arbitrage (can reinterpret tradition indefinitely and move between institutional positions). Traditional Catholic communities occupy the target pole (d near 1.0): they bear the cost of doctrinal rupture and liturgical suppression; they are powerless at the institutional level and identity-locked (cannot exit without losing faith-identity and community). Vatican institutional authority sits at the agenda-setter position (d at the symmetric point but with control asymmetry): they administer the settlement and claim to serve the whole Church, but the rupture reading asserts they serve modernist interests through interpretive monopoly. Directionality overrides may not be necessary here because the structural derivation from victim/beneficiary + power + exit captures the true relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   The rupture reading must grapple with a critical question: if Vatican II's documents contain doctrinal errors, what is the status of the Council's authority? Two mandatrophy paths emerge. Path 1 (SSPX): The Council is formally valid (convened by the Pope, includes bishops) but materially defective (documents heretical or erroneous). The Church is in crisis because it has bound itself to defective doctrine. The constraint persists because institutional authority apparatus refuses to acknowledge the defect. Fixing it requires declaring the Council gravely defective or reinterpreting its documents in light of tradition. Path 2 (Sedevacantist fringe): The Council was seized by modernists; papal authority was compromised; the Council is invalid. The constraint persists through the illicit occupation of the Church's authority. Fixing it requires restoration of pre-conciliar papal authority and doctrine. The rupture reading does NOT resolve the mandatrophy: it shifts the crisis from 'what does Vatican II mean' to 'what is Vatican II's authority status.' The beneficiary (post-conciliar theologians) has no incentive to resolve it; the institutional authority (Rome) claims the Council is valid and binding; only the victim (traditional communities) demands resolution, and they lack power. The constraint persists because all parties with institutional power have interests aligned with continued ambiguity. The measurement series show theater rising as the conflict becomes institutionalized and the performative aspect (claiming the Council is authoritative while reinterpreting it beyond recognition) becomes visible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    council_documents_objective_ambiguity,
    'Are the apparent contradictions between Vatican II documents and pre-conciliar teaching the result of doctrinal rupture, or does the Council''s language permit continuity readings that resolve the apparent contradictions?',
    'Systematic comparative textual analysis of Council documents alongside pre-conciliar magisterial sources, examining whether the language permits readings consistent with prior doctrine or logically forecloses them.',
    'If continuity readings are textually viable, the Council documents may be ambiguous rather than erroneous, and the constraint shifts from snare (imposed rupture) to tangled_rope (ambiguous document coordinating different interpretations, with modernist faction capturing one reading). If the language logically forecloses continuity readings, the rupture reading is textually grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(council_documents_objective_ambiguity, empirical, 'Whether apparent contradictions are doctrinal rupture or interpretive ambiguity').

omega_variable(
    suppression_mechanism_internalization,
    'Is the suppression of traditionalist resistance purely structural (canonical penalties, institutional barriers) or has it become internalized in Catholic formation (seminaries teaching modernist interpretation as default, lay Catholics internalizing post-conciliar theology as true tradition)?',
    'Post-suppression trajectory: if the institutional barriers to traditionalism were removed, would resistance persist at the same levels or would it collapse? Sociological study of traditionalist exit rates when canonical penalties are lifted (e.g., SSPX regularization scenarios).',
    'If suppression is primarily structural, removing it could restore traditionalist vitality (snare diagnosis confirmed). If suppression has become substantially internalized, traditionalism would persist only in isolated communities and the constraint transitions toward piton (institutional performance with diffuse cost, no concentrated benefit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs. internalized suppression mechanism in post-conciliar Catholic formation').

omega_variable(
    modernist_faction_coherence,
    'Is ''post-conciliar modernist theologians'' a unified faction with shared doctrine and interest, or is it a fragmented set of different theological schools that happened to benefit from conciliar ambiguity?',
    'Doctrinal mapping of post-conciliar theological movements (Rahner, Küng, Schillebeeckx, liberation theology, etc.) to identify shared commitments and conflicts.',
    'If unified, the constraint is a snare with a clear beneficiary faction imposing rupture. If fragmented, the constraint is more accurately described as tangled_rope or composite_overdetermination: multiple incompatible theologies each claim Council authority, and institutional suppression of traditionalism is partly the side effect of enforcing one modernist reading against other modernist readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modernist_faction_coherence, empirical, 'Whether post-conciliar modernism is a unified faction or fragmented schools').

omega_variable(
    mandate_death_vs_persistent_founding_problem,
    'Has the Council''s founding problem (pastoral irrelevance and lay disengagement in modernity) been solved by post-conciliar reforms, rendering the Council''s mandate obsolete? Or does the founding problem persist, justifying continued conciliar authority?',
    'Comparative sociology of Catholic practice and lay engagement pre/post-Vatican II; assessment of whether modern pastoral problems are solved or deepened by post-conciliar reforms.',
    'Mandate death would shift the constraint toward piton (theatrical maintenance of a failed solution, no beneficiary collecting rents, institutional overhead). Mandate persistence would support the rupture reading''s claim that the Council addressed real problems but did so defectively.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandate_death_vs_persistent_founding_problem, empirical, 'Whether Vatican II''s founding pastoral problem persists or has been solved').

omega_variable(
    reading_identity_fusion_in_traditionalism,
    'Is the traditionalist victim seat''s exit_options value correctly classified as identity_locked? Is pre-conciliar doctrine so fused with traditionalist Catholic self-understanding that exit (accepting post-conciliar Church) is phenomenologically impossible, or would it be possible with sufficient institutional tolerance?',
    'Ethnographic study of traditionalist communities and defectors; analysis of whether traditionalists who do exit report it as identity-shattering or as doctrinal correction; comparison with other identity-locked exit scenarios (religious deconversion, professional identity loss).',
    'If identity_locked is accurate, the constraint is snare (coercive, trapped targets). If traditionalist identity is more mutable than claimed, exit_options might be constrained rather than identity_locked, suggesting the suppression is more structural and less totalizing (tangled_rope diagnosis).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_identity_fusion_in_traditionalism, empirical, 'Degree of identity fusion in traditionalist Catholic communities vs. institutional suppression').

omega_variable(
    kernel_validity_vs_authority_status,
    'Does the rupture reading''s claim that Vatican II is ''valid but defective'' remain stable, or does insisting on the defectiveness eventually entail questioning the validity?',
    'Doctrinal and institutional analysis of how the SSPX and traditionalist magisterium navigate the validity/defectiveness distinction over time; examination of whether the distinction collapses under pressure from either the modernist faction (insisting validity entails binding authority) or internal traditionalist fragmentation (sedevacantists pushing toward invalidity).',
    'If the distinction remains stable, the rupture reading occupies a coherent logical position. If it collapses, the rupture reading bifurcates into two incompatible strands (SSPX vs. sedevacantist), and the constraint structure becomes unstable (the beneficiary faction''s position becomes untenable).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_validity_vs_authority_status, conceptual, 'Coherence of the validity/defectiveness distinction in traditionalist ecclesiology').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__rupture_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t0, vatican_ii_authority__rupture_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(vati_tr_t6, vatican_ii_authority__rupture_reading, theater_ratio, 6, 0.32).
narrative_ontology:measurement(vati_tr_t12, vatican_ii_authority__rupture_reading, theater_ratio, 12, 0.38).
narrative_ontology:measurement(vati_tr_t24, vatican_ii_authority__rupture_reading, theater_ratio, 24, 0.45).
narrative_ontology:measurement(vati_tr_t36, vatican_ii_authority__rupture_reading, theater_ratio, 36, 0.5).
narrative_ontology:measurement(vati_tr_t48, vatican_ii_authority__rupture_reading, theater_ratio, 48, 0.51).
narrative_ontology:measurement(vati_tr_t60, vatican_ii_authority__rupture_reading, theater_ratio, 60, 0.52).

% Extraction over time
narrative_ontology:measurement(vati_be_t0, vatican_ii_authority__rupture_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(vati_be_t6, vatican_ii_authority__rupture_reading, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(vati_be_t12, vatican_ii_authority__rupture_reading, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(vati_be_t24, vatican_ii_authority__rupture_reading, base_extractiveness, 24, 0.63).
narrative_ontology:measurement(vati_be_t36, vatican_ii_authority__rupture_reading, base_extractiveness, 36, 0.66).
narrative_ontology:measurement(vati_be_t48, vatican_ii_authority__rupture_reading, base_extractiveness, 48, 0.68).
narrative_ontology:measurement(vati_be_t60, vatican_ii_authority__rupture_reading, base_extractiveness, 60, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t0, vatican_ii_authority__rupture_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(vati_su_t6, vatican_ii_authority__rupture_reading, suppression_requirement, 6, 0.6).
narrative_ontology:measurement(vati_su_t12, vatican_ii_authority__rupture_reading, suppression_requirement, 12, 0.65).
narrative_ontology:measurement(vati_su_t24, vatican_ii_authority__rupture_reading, suppression_requirement, 24, 0.69).
narrative_ontology:measurement(vati_su_t36, vatican_ii_authority__rupture_reading, suppression_requirement, 36, 0.7).
narrative_ontology:measurement(vati_su_t48, vatican_ii_authority__rupture_reading, suppression_requirement, 48, 0.71).
narrative_ontology:measurement(vati_su_t60, vatican_ii_authority__rupture_reading, suppression_requirement, 60, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_authority__rupture_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vatican_ii_authority__rupture_reading, 0.12).
narrative_ontology:affects_constraint(vatican_ii_authority__rupture_reading, vatican_ii_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__rupture_reading, vatican_ii_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% Vatican II authority structure is a contested kernel instantiated by three distinct readings: rupture (this constraint), continuity (sibling), and composite_overdetermination (sibling). Each reading instantiates a different constraint with different ε values, beneficiary/victim structures, and authority-status claims. The readings coexist and foreclose each other in different ways. The rupture reading asserts the Council documents contain doctrinal errors and the post-conciliar settlement imposes rupture through institutional force. The continuity reading asserts the Council represents organic development and the modernist theology is legitimate. The overdetermination reading asserts the Council is logically incoherent and cannot be read as either rupture or continuity. All three readings claim Vatican II's authority; they differ in what that authority authorizes and on the diagnostic question of whether the Council is valid, coherent, and binding.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
