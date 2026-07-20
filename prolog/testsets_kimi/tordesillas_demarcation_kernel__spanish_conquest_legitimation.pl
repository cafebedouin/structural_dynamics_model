% ============================================================================
% CONSTRAINT STORY: tordesillas_demarcation_kernel__spanish_conquest_legitimation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:affects_constraint/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: tordesillas_demarcation_kernel__spanish_conquest_legitimation
 *   human_readable: Spanish Conquest Legitimation Reading of the Tordesillas Demarcation
 *   domain: international_law/colonial_history/sovereignty_theory
 *
 * SUMMARY:
 *   The 1494 Treaty of Tordesillas and its underlying papal bulls (Dum
 *   Diversas, Inter Caetera) constitute one reading of a contested kernel:
 *   the Spanish conquest legitimation reading. Under this reading, the papal
 *   demarcation line west of which Spain received exclusive rights functioned
 *   not merely as an interstate coordination device but as a
 *   theological-legal license for territorial conquest, indigenous
 *   subjugation, and resource extraction through the encomienda system.
 *   Indigenous populations west of the line were treated as legal nullities
 *   whose lands and labor could be appropriated under the cover of spiritual
 *   guardianship. This constraint story isolates the Spanish-west reading
 *   only; the Portuguese-east reading is a sibling constraint.
 *
 * KEY AGENTS:
 *   - Spanish Crown and colonial administration: Primary beneficiary and agenda-setter (institutional/arbitrage) â receives papal license, collects tribute and labor.
 *   - Indigenous populations west of the line: Primary target and victim (powerless/trapped) â bear conquest, dispossession, and forced conversion.
 *   - Papal Curia: Agenda-setter (institutional/mobile) â issues and interprets the universal grant, asserts spiritual-temporal authority.
 *   - Catholic missionary orders: Secondary beneficiary (organized/constrained) â receive state protection and a captive population for conversion.
 *   - Rival European powers: Excluded party (powerful/constrained) â barred from the partitioned hemisphere by a religious legal framework they do not accept.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.88).
domain_priors:suppression_score(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.92).
domain_priors:theater_ratio(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, extractiveness, 0.88).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, snare).
narrative_ontology:human_readable(tordesillas_demarcation_kernel__spanish_conquest_legitimation, "Spanish Conquest Legitimation Reading of the Tordesillas Demarcation").
narrative_ontology:topic_domain(tordesillas_demarcation_kernel__spanish_conquest_legitimation, "international_law/colonial_history/sovereignty_theory").

domain_priors:requires_active_enforcement(tordesillas_demarcation_kernel__spanish_conquest_legitimation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tordesillas_demarcation_kernel__spanish_conquest_legitimation, '47f5ad50-aa76-4f32-928f-83984b54a704').
narrative_ontology:cs_kernel_codification('47f5ad50-aa76-4f32-928f-83984b54a704', formalized).
narrative_ontology:cs_authority_grounding('47f5ad50-aa76-4f32-928f-83984b54a704', lineage).
narrative_ontology:cs_interpretation_layer_present('47f5ad50-aa76-4f32-928f-83984b54a704').
narrative_ontology:cs_reading_relation('47f5ad50-aa76-4f32-928f-83984b54a704', tordesillas_demarcation_kernel__portuguese_exploration_legitimation, coexists_with).
narrative_ontology:cs_axiom('47f5ad50-aa76-4f32-928f-83984b54a704', foundational, papal_grant_conveys_temporal_sovereignty).
narrative_ontology:cs_axiom_status(papal_grant_conveys_temporal_sovereignty, overridden).
narrative_ontology:cs_axiom_grounding('47f5ad50-aa76-4f32-928f-83984b54a704', papal_grant_conveys_temporal_sovereignty, theological).
narrative_ontology:cs_axiom('47f5ad50-aa76-4f32-928f-83984b54a704', foundational, non_christians_lack_valid_territorial_title).
narrative_ontology:cs_axiom_status(non_christians_lack_valid_territorial_title, overridden).
narrative_ontology:cs_axiom_grounding('47f5ad50-aa76-4f32-928f-83984b54a704', non_christians_lack_valid_territorial_title, theological).
narrative_ontology:cs_reference_frame('47f5ad50-aa76-4f32-928f-83984b54a704', papal_universal_temporal_authority).
narrative_ontology:cs_drift_state('47f5ad50-aa76-4f32-928f-83984b54a704', post_westphalian_sovereignty_order, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('47f5ad50-aa76-4f32-928f-83984b54a704', '').
narrative_ontology:cs_kernel_id(tordesillas_demarcation_kernel__spanish_conquest_legitimation, tordesillas_demarcation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_colonial_administration).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, catholic_missionary_orders).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, indigenous_populations_west_of_line).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives exclusive papal license to conquer and settle all lands west of the Tordesillas line. Administers the encomienda system, collects quinto real and tribute, and deploys military force to suppress indigenous resistance. Can modify colonial law and appeal to the Crown for policy changes.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_colonial_administration, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_colonial_administration, beneficiary).

% Inhabited the Americas before European contact. After 1494, their territories were declared legally vacant and assigned to the Spanish Crown. Subjected to military conquest, encomienda labor drafts, tribute demands, and forced conversion. No recognized legal standing to challenge the papal grant or Spanish title in European forums; armed resistance was met with systematic retaliation.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, indigenous_populations_west_of_line, payer,
    powerless, generational, trapped, continental).

% Issued the bulls Dum Diversas and Inter Caetera claiming universal spiritual and temporal jurisdiction over non-Christian lands. Allocated the western hemisphere to Spain for conquest and conversion. Retains doctrinal authority to interpret or modify the grant, though in practice the Spanish Crown drove territorial policy.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, papal_curia, agenda_setter,
    institutional, civilizational, mobile, global).

% Received state protection and financing to carry out evangelization in Spanish-claimed territories. Benefited from a captive population subject to conversion and from the legal fusion of religious and civil authority in colonial administration. Could not operate outside the Crown's patronato real framework.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, catholic_missionary_orders, beneficiary,
    organized, generational, constrained, continental).

% Other European crowns, particularly France, England, and the Dutch Republic, were excluded from trade and settlement west of the line by a papal decree they did not recognize. Their exclusion was enforced by Spanish naval power and diplomatic claims rooted in the papal grant.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, rival_european_powers, excluded,
    powerful, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_colonial_administration).
narrative_ontology:fixing_cost_class(tordesillas_demarcation_kernel__spanish_conquest_legitimation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Partitioned the non-Christian world between Iberian crowns to prevent direct military conflict between Spain and Portugal over newly encountered territories, creating a single religious-legal framework for European expansion.
% TRANSFER_FUNCTION: Moves indigenous land, labor, and surplus production from indigenous populations to the Spanish Crown and colonial settlers, mediated through the encomienda system and justified as spiritual guardianship.
% ABSENT_VOICES: Indigenous political and religious leaders were not consulted in the demarcation; their territorial sovereignty was treated as legally void. Rival European powers were present in diplomacy but excluded from the theological-legal framework that allocated their claimed lands.
% DISAPPEARANCE_RATIONALE: If the papal grant and its Spanish conquest reading vanished overnight, the legal structure underwriting the conquest would have collapsed; indigenous polities might have retained diplomatic standing longer, rival European powers would have contested territories without ecclesiastical penalty, and the encomienda system would have lost its claimed universal legal foundation.
% FOUNDING_PROBLEM: How to regulate Iberian competition over newly discovered non-Christian lands and provide a legal mechanism for incorporating those territories into Christendom without triggering intra-European war.
% FOUNDING_PROBLEM_CORROBORATION: Modern international law scholars and post-colonial historians attest that the partition solved only inter-Iberian rivalry while externalizing all costs onto indigenous populations. BartolomÃ© de las Casas and the Salamanca school contested the founding premises during the colonial period itself, and contemporary Vatican doctrine repudiates the allocation of non-Christian territories by papal grant.
narrative_ontology:disappearance_verdict(tordesillas_demarcation_kernel__spanish_conquest_legitimation, world_rearranges).
narrative_ontology:founding_problem_status(tordesillas_demarcation_kernel__spanish_conquest_legitimation, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tordesillas_demarcation_kernel__spanish_conquest_legitimation, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
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
 *   Extractiveness is very high (0.88) because the constraint systematically transferred land, labor, and surplus from indigenous populations to the Spanish colonial apparatus under threat of violence, with no reciprocal benefit to the governed. Suppression is higher (0.92) because the arrangement depended on military conquest, coerced conversion, and the legal erasure of indigenous title; alternatives (autonomous indigenous political existence, rival European settlement) were actively destroyed or excluded. Theater ratio is low (0.25) because the enforcement was brutally functional rather than performative â the encomienda and conquest were materially efficient at extraction, not hollow ritual. Accessibility collapse is very high (0.90) because once the papal-Spanish legal framework was imposed, indigenous legal standing collapsed almost entirely, and European rivals were barred by an inter-state religious treaty. Resistance is moderate (0.40) because indigenous armed and legal resistance was continuous but was systematically overpowered, while rival European powers eventually breached the partition. The founding problem â regulating Iberian competition â was solved at the price of indigenous annihilation, and the constraint persisted as extraction long after its coordination function was obsolete.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of the Spanish Crown, the constraint appeared as a legitimate legal order that prevented European war and advanced the salvation of souls; from the indigenous seat, it was indistinguishable from military invasion and slavery. The papal curia occupied a third seat in which universal spiritual jurisdiction was invoked to license temporal conquest. The engine computes these divergent classifications from the same structural data: the Crownâs exit is arbitrage (it writes the rules), the indigenous exit is trapped (no external appeal against a universal religious decree), and the papal exit is mobile (doctrine can be revised, as it eventually was).
 *
 * DIRECTIONALITY LOGIC:
 *   Spanish colonial administration is named in beneficiaries and has arbitrage-grade exit, placing its directionality near the full-beneficiary end (low d). Indigenous populations are named in victims and are trapped, placing directionality near the full-target end (high d). Catholic missionary orders benefit from state support but do not control the constraint, giving them a moderate-low d. Rival European powers are excluded beneficiaries of the broader kernel but are structurally outside this reading; their d is irrelevant because they are not governed by this constraint. The papal curia is an agenda-setter with mobile exit; its d is ambiguous because it both creates and transcends the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling as coordination by examining the beneficiary set. A genuine rope or tangled rope would require that the coordinated parties include those actually governed by the constraint. Here, the âcoordinationâ (partitioning Iberian claims) was achieved only among European powers; the indigenous populations, who bore the entire cost of the arrangement, were never beneficiaries. Because the governed population was structurally excluded from the coordination benefit and structurally included in the victim set, the constraint cannot be a rope. It is a snare: the coordination story (preventing Spain-Portugal war) was cover for extraction (conquest and encomienda), and the indigenous populations were the identifiable victims from the outset. The founding problem is dead (the Iberian partition is obsolete), but the extraction outlived it, confirming the mandatrophy pattern without upgrading the constraint to piton because the extraction was always the primary function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    papal_temporal_authority_scope,
    'Did the papal bulls intend to grant full temporal sovereignty over non-Christian lands, or merely to assign evangelization duties between Iberian crowns?',
    'Close textual and diplomatic analysis of Dum Diversas and Inter Caetera against contemporary canon-law commentary (e.g., Vitoriaâs Relectio de Indis).',
    'If the grant was only evangelical assignment, the territorial conquest reading is a later imperial extrapolation and the constraintâs extractiveness is even higher than authored; if temporal sovereignty was intended, the theological grounding is more coherent but the human cost remains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(papal_temporal_authority_scope, conceptual, 'Ambiguity in papal grant scope between spiritual mission and temporal sovereignty.').

omega_variable(
    kernel_reading_boundary,
    'Does the Tordesillas kernel decompose cleanly into a Spanish conquest reading and a Portuguese exploration reading, or do the two readings share extraction mechanisms (e.g., indigenous subjugation in Brazil) that collapse the boundary?',
    'Comparative colonial history of Portuguese and Spanish American colonization; inspection of whether the Portuguese reading also entails indigenous victimization.',
    'If the Portuguese reading is also extractive toward indigenous populations, the kernel is a unified snare with two administrative faces rather than two distinct constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether the sibling readings are structurally separable or a single extraction mechanism.').

omega_variable(
    indigenous_legal_personhood_status,
    'Were indigenous polities treated as outright legal nullities under the Spanish reading, or as subordinate legal subjects with limited but recognized rights (e.g., through the Requerimiento and New Laws)?',
    'Archival analysis of colonial legal practice and indigenous litigation in Spanish courts.',
    'If indigenous actors held limited legal standing, accessibility_collapse is slightly lower and the constraint approaches tangled_rope; if standing was purely fictive, the snare classification is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(indigenous_legal_personhood_status, empirical, 'Degree of indigenous legal personhood under the colonial regime.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tord_tr_t0, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 0, 0.1).
narrative_ontology:measurement(tord_tr_t12, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 12, 0.12).
narrative_ontology:measurement(tord_tr_t24, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 24, 0.15).
narrative_ontology:measurement(tord_tr_t36, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 36, 0.18).
narrative_ontology:measurement(tord_tr_t48, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 48, 0.22).
narrative_ontology:measurement(tord_tr_t60, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 60, 0.25).

% Extraction over time
narrative_ontology:measurement(tord_be_t0, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(tord_be_t12, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 12, 0.72).
narrative_ontology:measurement(tord_be_t24, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 24, 0.8).
narrative_ontology:measurement(tord_be_t36, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 36, 0.85).
narrative_ontology:measurement(tord_be_t48, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 48, 0.87).
narrative_ontology:measurement(tord_be_t60, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 60, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(tord_su_t0, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(tord_su_t12, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 12, 0.8).
narrative_ontology:measurement(tord_su_t24, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 24, 0.86).
narrative_ontology:measurement(tord_su_t36, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 36, 0.89).
narrative_ontology:measurement(tord_su_t48, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 48, 0.91).
narrative_ontology:measurement(tord_su_t60, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 60, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__spanish_conquest_legitimation, portuguese_exploration_legitimation).

% DUAL FORMULATION NOTE:
% The Tordesillas demarcation kernel decomposes into two structurally distinct constraints: a Spanish conquest legitimation reading (west) and a Portuguese exploration legitimation reading (east). They share the same papal source text but instantiate different beneficiary/victim structures and extraction profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
