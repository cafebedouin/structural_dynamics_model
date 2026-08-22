% ============================================================================
% CONSTRAINT STORY: tordesillas_demarcation_kernel__spanish_conquest_legitimation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tordesillas_demarcation_kernel__spanish_conquest_legitimation, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: tordesillas_demarcation_kernel__spanish_conquest_legitimation
 *   human_readable: Papal Grant as Spanish Conquest Legitimation West of the Line
 *   domain: international_law/colonial_history/sovereignty_theory
 *
 * SUMMARY:
 *   The 1493-1494 papal donation and Treaty of Tordesillas divided the
 *   non-Christian world between Spain and Portugal along a meridian west of
 *   the Cape Verde islands. THIS READING treats the grant not as mere
 *   interstate coordination but as a positive license for territorial
 *   conquest, indigenous subjugation, and forced conversion west of the line.
 *   The constraint nullified indigenous sovereignty, imposed the encomienda
 *   labor system, and extracted land, mineral wealth, and cultural autonomy
 *   under theological cover. It is a kernel reading distinct from the
 *   Portuguese exploration legitimation reading, which foregrounds exclusion
 *   of European rivals east of the line.
 *
 * KEY AGENTS:
 *   - spanish_crown: Primary agenda-setter (institutional/global) â enforces the papal grant through colonial administration and juridical apparatus, captures tribute and territorial sovereignty.
 *   - encomendero_class: Primary material beneficiary (powerful/regional) â holds grants of indigenous labor, extracts agricultural and mineral surplus.
 *   - catholic_church_missionary_apparatus: Secondary beneficiary with agenda-setting function (institutional/global) â provides theological cover, administers forced conversion, collects tithes.
 *   - indigenous_populations_west_of_line: Primary target and victim (powerless/regional) â stripped of legal personality and territorial title, subject to forced labor and cultural suppression.
 *   - rival_european_powers: Excluded parties (powerful/global) â barred by the line from westward expansion, later contest the framework.
 *   - post_colonial_legal_observers: Analytical observers (analytical/global) â document the gap between theological claims and extractive outcomes.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.88).
domain_priors:suppression_score(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.83).
domain_priors:theater_ratio(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, extractiveness, 0.88).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 0.83).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 0.78).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, snare).
narrative_ontology:human_readable(tordesillas_demarcation_kernel__spanish_conquest_legitimation, "Papal Grant as Spanish Conquest Legitimation West of the Line").
narrative_ontology:topic_domain(tordesillas_demarcation_kernel__spanish_conquest_legitimation, "international_law/colonial_history/sovereignty_theory").

domain_priors:requires_active_enforcement(tordesillas_demarcation_kernel__spanish_conquest_legitimation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tordesillas_demarcation_kernel__spanish_conquest_legitimation, '5fbae620-45fa-45b6-8a95-a28b167198f4').
narrative_ontology:cs_kernel_codification('5fbae620-45fa-45b6-8a95-a28b167198f4', fixed_text).
narrative_ontology:cs_authority_grounding('5fbae620-45fa-45b6-8a95-a28b167198f4', lineage).
narrative_ontology:cs_interpretation_layer_present('5fbae620-45fa-45b6-8a95-a28b167198f4').
narrative_ontology:cs_reading_relation('5fbae620-45fa-45b6-8a95-a28b167198f4', tordesillas_demarcation_kernel__portuguese_exploration_legitimation, coexists_with).
narrative_ontology:cs_axiom('5fbae620-45fa-45b6-8a95-a28b167198f4', foundational, papal_grant_conveys_conquest_license).
narrative_ontology:cs_axiom_status(papal_grant_conveys_conquest_license, overridden).
narrative_ontology:cs_axiom_grounding('5fbae620-45fa-45b6-8a95-a28b167198f4', papal_grant_conveys_conquest_license, theological).
narrative_ontology:cs_axiom('5fbae620-45fa-45b6-8a95-a28b167198f4', foundational, non_christian_princes_lack_dominium).
narrative_ontology:cs_axiom_status(non_christian_princes_lack_dominium, overridden).
narrative_ontology:cs_axiom_grounding('5fbae620-45fa-45b6-8a95-a28b167198f4', non_christian_princes_lack_dominium, theological).
narrative_ontology:cs_reference_frame('5fbae620-45fa-45b6-8a95-a28b167198f4', universal_papal_temporal_sovereignty).
narrative_ontology:cs_drift_state('5fbae620-45fa-45b6-8a95-a28b167198f4', post_westphalian_legal_order, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('5fbae620-45fa-45b6-8a95-a28b167198f4', '').
narrative_ontology:cs_kernel_id(tordesillas_demarcation_kernel__spanish_conquest_legitimation, tordesillas_demarcation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_crown).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, encomendero_class).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, catholic_church_missionary_apparatus).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, indigenous_populations_west_of_line).
narrative_ontology:constraint_vindicates(tordesillas_demarcation_kernel__spanish_conquest_legitimation, doctrine_of_discovery).
narrative_ontology:constraint_vindicates(tordesillas_demarcation_kernel__spanish_conquest_legitimation, papal_temporal_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issued and enforced the papal grant through the Council of the Indies and colonial viceroyalties, asserting exclusive sovereignty over American territories. Administered the encomienda and repartimiento systems, collecting tribute and directing colonial extraction. Retained formal capacity to revoke grants but remained fiscally and dynastically locked into expansion.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_crown, agenda_setter,
    institutional, generational, constrained, global).

% Held royal grants of indigenous labor and tribute. Extracted agricultural surplus, minerals, and personal service from assigned pueblos. Their economic and social standing depended entirely on Crown enforcement of the papal license and the denial of indigenous land title.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, encomendero_class, beneficiary,
    powerful, biographical, constrained, regional).

% Administered the spiritual justification for conquest through mission parishes, forced conversion, and the erasure of indigenous religious practice. Expanded institutional landholdings and collected tithes under the Patronato real, tying ecclesiastical authority to Crown control.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, catholic_church_missionary_apparatus, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(tordesillas_demarcation_kernel__spanish_conquest_legitimation, catholic_church_missionary_apparatus, agenda_setter).

% Lived under the encomienda and mission systems, subject to tribute, forced labor, and cultural suppression. Held no legal personality under Spanish colonial law; pre-existing territorial sovereignty was nullified by the papal donation. Armed resistance was met with military retaliation, and flight into uncolonized territory was progressively closed off.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, indigenous_populations_west_of_line, payer,
    powerless, generational, trapped, regional).

% English, French, and Dutch crowns and merchants were formally excluded from the Spanish sphere by the papal line and treaty. They contested the grant's validity under emerging international law and eventually conducted colonial ventures in open defiance of the Inter Caetera framework.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, rival_european_powers, excluded,
    powerful, biographical, constrained, global).

% Modern historians and international legal scholars analyze the grant as a foundational document of colonial legal ideology. They measure the structural gap between its theological claims and its extractive outcomes, and attest to its delegitimation in contemporary law.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, post_colonial_legal_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Divides newly encountered non-Christian lands between Spain and Portugal along a fixed meridian, preventing direct intra-Iberian military conflict over expansion zones.
% TRANSFER_FUNCTION: Moves territorial sovereignty, mineral wealth, agricultural surplus, and labor service from indigenous populations to the Spanish Crown and encomendero class; moves spiritual allegiance and cultural autonomy from indigenous religions to the Catholic Church under Crown patronage.
% ABSENT_VOICES: Indigenous sovereigns and legal traditions were never party to the donation; non-Iberian European powers were excluded from the partition; later abolitionist and indigenous-rights advocates were structurally absent from the sixteenth-century framework.
% DISAPPEARANCE_RATIONALE: The encomienda system, colonial legal hierarchy, and indigenous land dispossession all depended on the nullification of indigenous sovereignty performed by the grant. If the papal license vanished, indigenous title would reassert as a legal force, the theological cover for forced conversion would collapse, and the Crown's exclusive claim to American surplus would face immediate legitimacy crisis.
% FOUNDING_PROBLEM: To prevent war between Catholic crowns over newly discovered lands and to secure papal monopoly against Ottoman and Protestant rivals by placing non-Christian territories under Iberian-Catholic sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: At founding, no corroboration existed outside the Spanish Crown and Papal court; indigenous voices were excluded by construction. Contemporary post-colonial legal historians and indigenous-rights scholars outside any beneficiary tradition corroborate that the founding problem served extraction and is now superseded.
narrative_ontology:disappearance_verdict(tordesillas_demarcation_kernel__spanish_conquest_legitimation, world_rearranges).
narrative_ontology:founding_problem_status(tordesillas_demarcation_kernel__spanish_conquest_legitimation, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 'none', 1).
narrative_ontology:epsilon_provenance(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.88, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tordesillas_demarcation_kernel__spanish_conquest_legitimation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tordesillas_demarcation_kernel__spanish_conquest_legitimation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tordesillas_demarcation_kernel__spanish_conquest_legitimation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.88) because the constraint transferred entire territories and populations to Crown and encomendero control with negligible reciprocity. Suppression is high (0.83) because the arrangement required continuous military enforcement, legal erasure of indigenous title, and ecclesiastical policing of conversion. Theater ratio rises to 0.78 because the religious justification became increasingly performative relative to the material extraction it obscured. Accessibility collapse is near-total (0.92) for indigenous populations, whose legal and physical exit options were destroyed. Resistance is substantial (0.60) but was systematically crushed.
 *
 * PERSPECTIVAL GAP:
 *   From the Crown and Church seats, the constraint appears as necessary evangelization and lawful empire; from the indigenous seat, it operates as total legal erasure and material extraction. The engine will compute this divergence from beneficiary/victim declarations and the trapped exit of the payer population.
 *
 * DIRECTIONALITY LOGIC:
 *   The Spanish Crown and encomendero class sit near the beneficiary end of directionality: they collect tribute, labor, and land. The Church missionary apparatus also benefits, though its gains are spiritual and institutional. Indigenous populations sit at the full-target end: they bear the extraction and have no exit. Rival European powers are excluded from the framework rather than coordinated.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint was built to solve inter-Iberian coordination and Crusade theological competition, but the mandate was captured by extraction almost immediately after contact. The founding problem of preventing intra-Catholic war was rendered obsolete by the Westphalian system, yet the extractive apparatus persisted for centuries under theatrical theological maintenance. The status is dead, the arrangement persists as inertia, but the extraction was so severe and continuous that the classification remains snare rather than piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    papal_grant_construction_ambiguity,
    'Is the papal donation a constructed legal fiction to authorize extraction, or did it emerge from a coherent theological-political framework independent of colonial material interests?',
    'Analysis of papal correspondence and conciliar debates preceding Inter Caetera versus post-hoc legal rationalizations by Crown jurists.',
    'If purely constructed, classification as snare is strengthened; if coherent independent framework, tangled_rope becomes possible due to genuine coordination between Iberian crowns.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(papal_grant_construction_ambiguity, conceptual, 'Whether the grant was instrumental cover or genuine legal theology').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is indigenous subjugation enforced primarily through structural violence and legal erasure, or through internalized religious conversion and cultural replacement?',
    'Post-abolition trajectory of indigenous communities: persistence of cultural erasure after legal emancipation indicates internalized suppression.',
    'Internalized suppression raises effective extraction beyond structural metrics and flags residual cognitive capture even after formal legal abolition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism').

omega_variable(
    sibling_reading_boundary,
    'Does the Portuguese exploration legitimation reading of the same kernel represent a structurally distinct constraint with a different victim set and lower extractiveness?',
    'Comparative analysis of legal deployment in Spanish versus Portuguese zones and the respective centrality of indigenous subjugation versus maritime rivalry exclusion.',
    'If the Portuguese reading yields a materially different epsilon and victim profile, the kernel is irreducibly split and this reading''s snare classification is confirmed as localized to the Spanish conquest legitimation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_boundary, conceptual, 'Structural boundary between sibling kernel readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0, 250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tord_tr_t0, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 0, 0.3).
narrative_ontology:measurement(tord_tr_t50, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 50, 0.55).
narrative_ontology:measurement(tord_tr_t100, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 100, 0.65).
narrative_ontology:measurement(tord_tr_t150, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 150, 0.7).
narrative_ontology:measurement(tord_tr_t200, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 200, 0.75).
narrative_ontology:measurement(tord_tr_t250, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 250, 0.78).

% Extraction over time
narrative_ontology:measurement(tord_be_t0, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(tord_be_t50, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 50, 0.82).
narrative_ontology:measurement(tord_be_t100, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 100, 0.9).
narrative_ontology:measurement(tord_be_t150, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 150, 0.88).
narrative_ontology:measurement(tord_be_t200, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 200, 0.85).
narrative_ontology:measurement(tord_be_t250, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 250, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(tord_su_t0, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(tord_su_t50, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 50, 0.92).
narrative_ontology:measurement(tord_su_t100, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 100, 0.9).
narrative_ontology:measurement(tord_su_t150, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 150, 0.88).
narrative_ontology:measurement(tord_su_t200, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 200, 0.85).
narrative_ontology:measurement(tord_su_t250, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 250, 0.83).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tordesillas_demarcation_kernel__spanish_conquest_legitimation, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is the Spanish conquest legitimation reading of the tordesillas_demarcation_kernel. It is structurally distinct from the Portuguese exploration legitimation reading, which assigns a different beneficiary/victim structure and lower extractiveness to the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
