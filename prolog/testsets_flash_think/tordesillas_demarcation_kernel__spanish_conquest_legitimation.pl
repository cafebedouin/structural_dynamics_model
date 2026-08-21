% ============================================================================
% CONSTRAINT STORY: tordesillas_demarcation_kernel__spanish_conquest_legitimation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   human_readable: Papal Grant as Spanish Conquest Legitimation (West of Tordesillas Line)
 *   domain: international_law/colonial_history/sovereignty_theory
 *
 * SUMMARY:
 *   This constraint represents the reading of the Papal grants and the Treaty
 *   of Tordesillas as legitimizing Spanish territorial conquest and
 *   indigenous subjugation in the lands west of the demarcation line. It
 *   provided a legal and moral framework for the Spanish Crown and its
 *   colonial administration to claim sovereignty, exploit resources, and
 *   impose its will on indigenous populations, often under the guise of
 *   religious conversion. This reading emphasizes the extractive and coercive
 *   aspects of the colonial enterprise.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.92).
domain_priors:suppression_score(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.95).
domain_priors:theater_ratio(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, extractiveness, 0.92).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, snare).
narrative_ontology:human_readable(tordesillas_demarcation_kernel__spanish_conquest_legitimation, "Papal Grant as Spanish Conquest Legitimation (West of Tordesillas Line)").
narrative_ontology:topic_domain(tordesillas_demarcation_kernel__spanish_conquest_legitimation, "international_law/colonial_history/sovereignty_theory").

domain_priors:requires_active_enforcement(tordesillas_demarcation_kernel__spanish_conquest_legitimation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tordesillas_demarcation_kernel__spanish_conquest_legitimation, '25745345-8bf2-4ee7-b04c-9b7b2166ccb6').
narrative_ontology:cs_kernel_codification('25745345-8bf2-4ee7-b04c-9b7b2166ccb6', fixed_text).
narrative_ontology:cs_authority_grounding('25745345-8bf2-4ee7-b04c-9b7b2166ccb6', lineage).
narrative_ontology:cs_interpretation_layer_present('25745345-8bf2-4ee7-b04c-9b7b2166ccb6').
narrative_ontology:cs_reading_relation('25745345-8bf2-4ee7-b04c-9b7b2166ccb6', tordesillas_demarcation_kernel__portuguese_exploration_legitimation, coexists_with).
narrative_ontology:cs_axiom('25745345-8bf2-4ee7-b04c-9b7b2166ccb6', foundational, papal_authority_to_grant_territory).
narrative_ontology:cs_axiom_status(papal_authority_to_grant_territory, holdable).
narrative_ontology:cs_axiom_grounding('25745345-8bf2-4ee7-b04c-9b7b2166ccb6', papal_authority_to_grant_territory, theological).
narrative_ontology:cs_axiom('25745345-8bf2-4ee7-b04c-9b7b2166ccb6', foundational, indigenous_lack_of_sovereignty).
narrative_ontology:cs_axiom_status(indigenous_lack_of_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('25745345-8bf2-4ee7-b04c-9b7b2166ccb6', indigenous_lack_of_sovereignty, conventional).
narrative_ontology:cs_reference_frame('25745345-8bf2-4ee7-b04c-9b7b2166ccb6', divine_right_of_conquest).
narrative_ontology:cs_drift_state('25745345-8bf2-4ee7-b04c-9b7b2166ccb6', contemporary_international_law, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('25745345-8bf2-4ee7-b04c-9b7b2166ccb6', '').
narrative_ontology:cs_kernel_id(tordesillas_demarcation_kernel__spanish_conquest_legitimation, tordesillas_demarcation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_colonial_administration).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_crown).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, catholic_church).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, indigenous_populations_west_of_line).
narrative_ontology:constraint_vindicates(tordesillas_demarcation_kernel__spanish_conquest_legitimation, terra_nullius_doctrine).
narrative_ontology:constraint_vindicates(tordesillas_demarcation_kernel__spanish_conquest_legitimation, divine_right_of_conquest).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Directly implemented and enforced the terms of the Papal grants and the Treaty of Tordesillas, establishing encomiendas, extracting resources, and subjugating indigenous populations. Benefited directly from the labor and wealth generated.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_colonial_administration, agenda_setter,
    institutional, generational, arbitrage, global).

% Were the primary targets and victims of the conquest, losing land, sovereignty, culture, and lives. Subjected to forced labor, conversion, and violence. Exit options were limited to resistance, flight, or death.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, indigenous_populations_west_of_line, payer,
    powerless, generational, trapped, regional).

% The ultimate political and economic beneficiary, gaining vast territories, resources, and imperial prestige. The Papal grants provided the legal and moral cover for its expansion.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_crown, beneficiary,
    institutional, civilizational, arbitrage, global).

% Issued the Papal Bulls (e.g., Inter Caetera) that legitimized the Spanish claims, framing conquest as a mission to convert non-Christians. Benefited from the expansion of its spiritual authority and new converts.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, catholic_church, agenda_setter,
    institutional, civilizational, arbitrage, universal).

% While a beneficiary of the overall Tordesillas kernel, this specific reading (Spanish conquest west of the line) excluded Portuguese claims and activities from this designated sphere. They were constrained by the same papal authority that legitimized their own eastern claims.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, portuguese_crown, excluded,
    institutional, generational, constrained, global).

% Other European powers (e.g., England, France, Netherlands) who would later contest Spanish and Portuguese claims, but were initially excluded by the Papal demarcation and lacked the immediate means to challenge it directly. Their exclusion was a key function of the constraint.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, european_rivals, excluded,
    powerful, biographical, mobile, global).

% Modern academic and legal observers who analyze the historical impact and legitimacy of the Papal grants and the Treaty of Tordesillas from a post-colonial perspective, often critiquing their foundational assumptions.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, international_law_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_colonial_administration).
narrative_ontology:fixing_cost_class(tordesillas_demarcation_kernel__spanish_conquest_legitimation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Primarily coordinated European colonial expansion by demarcating spheres of influence between Spain and Portugal, thereby reducing conflict between these two Catholic powers over newly 'discovered' lands.
% TRANSFER_FUNCTION: Transferred vast territories, natural resources, and the labor of indigenous populations from their original inhabitants to the Spanish Crown and its colonial administration, under the religious and legal authority of the Catholic Church.
% ABSENT_VOICES: Indigenous populations, whose sovereignty was entirely disregarded, were structurally absent from the negotiations and decision-making. Their voices would have asserted prior claims to land and self-governance, fundamentally challenging the legitimacy of the grants.
% DISAPPEARANCE_RATIONALE: If the Papal grants and the Treaty of Tordesillas had never existed or were immediately repudiated, the history of European colonization in the Americas would have been fundamentally different, likely leading to a more fragmented and contested scramble for territory, or potentially different forms of interaction with indigenous societies. The entire legal and moral framework for Spanish colonial expansion would have collapsed.
% FOUNDING_PROBLEM: The problem was to legitimize European claims to non-Christian lands and to prevent armed conflict between the burgeoning Spanish and Portuguese empires over newly 'discovered' territories, particularly after Columbus's voyages.
% FOUNDING_PROBLEM_CORROBORATION: While the Spanish Crown and Catholic Church historically asserted the problem was live and their solution legitimate, modern international law and post-colonial scholarship (e.g., from international law scholars, indigenous rights advocates) overwhelmingly attest that the foundational premises (papal authority over non-Christian lands, terra nullius) are dead and were never legitimate. No corroboration exists outside the original benefiting parties and their historical apologists.
narrative_ontology:disappearance_verdict(tordesillas_demarcation_kernel__spanish_conquest_legitimation, world_rearranges).
narrative_ontology:founding_problem_status(tordesillas_demarcation_kernel__spanish_conquest_legitimation, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tordesillas_demarcation_kernel__spanish_conquest_legitimation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 'none', 1).
narrative_ontology:epsilon_provenance(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.92, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is extremely high (0.92) as the entire colonial project was designed to transfer wealth and labor from the Americas to Spain. Suppression is near total (0.95) due to military force, disease, and the systematic dismantling of indigenous political and social structures. Accessibility collapse is high (0.90) as indigenous alternatives to Spanish rule were violently suppressed. Resistance is also high (0.80), reflecting centuries of indigenous uprisings and defiance. Theater ratio starts low but rises (0.40 by 1820) as the religious justification became increasingly transparent as a cover for economic exploitation, especially as other European powers challenged the Papal authority.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Spanish Crown and the Catholic Church, the grants were a legitimate exercise of authority for the spread of Christianity and the orderly expansion of empire. From the perspective of indigenous populations, it was an act of violent dispossession and enslavement, entirely lacking legitimacy. The engine's classification as a Snare reflects the latter, emphasizing the coercive and extractive reality over the claimed coordination or divine mandate.
 *
 * DIRECTIONALITY LOGIC:
 *   The Spanish colonial administration, the Spanish Crown, and the Catholic Church were the primary beneficiaries, deriving immense wealth, power, and spiritual influence. Indigenous populations were the clear targets and victims, bearing the full cost of conquest and subjugation. The Portuguese Crown, while a beneficiary of the broader kernel, was excluded from this specific sphere of influence. Other European rivals were also excluded by the Papal demarcation, though they would later challenge it.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_papal_authority,
    'Was the Papal authority to grant sovereignty over non-Christian lands a genuine moral and legal claim, or a political tool to legitimize European expansion?',
    'Analysis of historical theological debates, contemporary international law principles, and indigenous legal traditions. Resolution depends on the adopted framework of sovereignty and moral authority.',
    'If deemed a genuine claim, the ''theater_ratio'' might be lower, and the ''claimed_type'' might lean more towards a ''Rope'' (for European coordination) or ''Mountain'' (divine law). If deemed a political tool, the ''snare'' classification is reinforced, and ''theater_ratio'' would be higher.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_of_papal_authority, conceptual, 'Ambiguity of the Papal grant''s foundational legitimacy.').

omega_variable(
    indigenous_sovereignty_recognition,
    'Did indigenous populations genuinely lack sovereignty prior to European arrival, or was their sovereignty merely unrecognized/denied by European legal frameworks?',
    'Anthropological and historical research into pre-colonial indigenous governance structures, combined with a re-evaluation of international law''s historical treatment of non-European polities.',
    'If indigenous sovereignty is recognized as pre-existing, the ''accessibility_collapse'' and ''suppression'' metrics are further justified as violent imposition, reinforcing the ''snare'' classification. If indigenous societies are framed as lacking ''true'' sovereignty, the justification for conquest (within the European framework) is strengthened, though still contested.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(indigenous_sovereignty_recognition, empirical, 'The status of indigenous sovereignty before European contact.').

omega_variable(
    kernel_reading_divergence_portuguese,
    'How does the ''portuguese_exploration_legitimation'' reading structurally differ from this ''spanish_conquest_legitimation'' reading?',
    'Comparative analysis of the specific colonial practices, legal justifications, and indigenous experiences in the Portuguese sphere (east of the line) versus the Spanish sphere (west of the line).',
    'The Portuguese reading would likely show a similar ''snare'' profile but with a different beneficiary (Portuguese Crown) and victim set (indigenous populations east of the line), and potentially different nuances in ''extractiveness'' and ''suppression'' based on specific colonial models (e.g., focus on trade vs. direct settlement).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence_portuguese, conceptual, 'Structural differences between Spanish and Portuguese readings of the Tordesillas kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 1494, 1820).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tord_tr_t1494, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1494, 0.1).
narrative_ontology:measurement(tord_tr_t1550, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1550, 0.15).
narrative_ontology:measurement(tord_tr_t1600, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1600, 0.2).
narrative_ontology:measurement(tord_tr_t1650, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1650, 0.28).
narrative_ontology:measurement(tord_tr_t1700, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1700, 0.35).
narrative_ontology:measurement(tord_tr_t1750, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1750, 0.4).
narrative_ontology:measurement(tord_tr_t1820, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1820, 0.45).

% Extraction over time
narrative_ontology:measurement(tord_be_t1494, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1494, 0.85).
narrative_ontology:measurement(tord_be_t1550, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1550, 0.9).
narrative_ontology:measurement(tord_be_t1600, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1600, 0.93).
narrative_ontology:measurement(tord_be_t1650, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1650, 0.94).
narrative_ontology:measurement(tord_be_t1700, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1700, 0.93).
narrative_ontology:measurement(tord_be_t1750, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1750, 0.92).
narrative_ontology:measurement(tord_be_t1820, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1820, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(tord_su_t1494, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1494, 0.88).
narrative_ontology:measurement(tord_su_t1550, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1550, 0.92).
narrative_ontology:measurement(tord_su_t1600, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1600, 0.95).
narrative_ontology:measurement(tord_su_t1650, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1650, 0.96).
narrative_ontology:measurement(tord_su_t1700, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1700, 0.95).
narrative_ontology:measurement(tord_su_t1750, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1750, 0.94).
narrative_ontology:measurement(tord_su_t1820, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1820, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tordesillas_demarcation_kernel__spanish_conquest_legitimation, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
