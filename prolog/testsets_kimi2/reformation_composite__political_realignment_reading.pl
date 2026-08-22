% ============================================================================
% CONSTRAINT STORY: reformation_composite__political_realignment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_composite__political_realignment_reading, []).

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
 *   constraint_id: reformation_composite__political_realignment_reading
 *   human_readable: Political Realignment Reading of the Reformation: Territorial Sovereignty via Confessionalization
 *   domain: historical_epistemology/religious_history/political_economy
 *
 * SUMMARY:
 *   This constraint instantiates the political_realignment_reading of the
 *   contested kernel reformation_composite. It treats the Reformation not as
 *   primarily theological or technological, but as a structural mechanism by
 *   which emerging territorial states leveraged religious differentiation to
 *   capture sovereignty from imperial and papal authority. The cuius regio
 *   eius religio settlement (1555) is the primary observable: it formalized
 *   the transfer of ius reformandi to territorial rulers, solving the
 *   coordination problem of religious warfare while extracting supra-national
 *   authority and subject autonomy. Sibling readings
 *   (theological_fragmentation_reading, technological_mediation_reading) are
 *   structurally distinct constraints and are not folded into this
 *   classification.
 *
 * KEY AGENTS:
 *   - territorial_rulers: Primary beneficiary and agenda-setter (powerful/national/arbitrage) â captures sovereignty and ecclesiastical wealth
 *   - imperial_authority: Primary victim (institutional/continental/constrained) â loses universal jurisdiction and fiscal-military capacity
 *   - papal_authority: Primary victim (institutional/continental/constrained) â loses territorial authority, appointment rights, and revenue
 *   - religious_minorities: Secondary victim (powerless/local/constrained) â bears conformity costs and displacement risk
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__political_realignment_reading, 0.62).
domain_priors:suppression_score(reformation_composite__political_realignment_reading, 0.71).
domain_priors:theater_ratio(reformation_composite__political_realignment_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__political_realignment_reading, tangled_rope).
narrative_ontology:human_readable(reformation_composite__political_realignment_reading, "Political Realignment Reading of the Reformation: Territorial Sovereignty via Confessionalization").
narrative_ontology:topic_domain(reformation_composite__political_realignment_reading, "historical_epistemology/religious_history/political_economy").

domain_priors:requires_active_enforcement(reformation_composite__political_realignment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__political_realignment_reading, '58b64195-ec43-4d10-be46-ece8c0c9ac09').
narrative_ontology:cs_kernel_codification('58b64195-ec43-4d10-be46-ece8c0c9ac09', distributed).
narrative_ontology:cs_authority_grounding('58b64195-ec43-4d10-be46-ece8c0c9ac09', lineage).
narrative_ontology:cs_interpretation_layer_present('58b64195-ec43-4d10-be46-ece8c0c9ac09').
narrative_ontology:cs_reading_relation('58b64195-ec43-4d10-be46-ece8c0c9ac09', reformation_composite__theological_fragmentation_reading, coexists_with).
narrative_ontology:cs_reading_relation('58b64195-ec43-4d10-be46-ece8c0c9ac09', reformation_composite__technological_mediation_reading, coexists_with).
narrative_ontology:cs_axiom('58b64195-ec43-4d10-be46-ece8c0c9ac09', foundational, territorial_sovereignty_supreme_over_universal_church).
narrative_ontology:cs_axiom_status(territorial_sovereignty_supreme_over_universal_church, holdable).
narrative_ontology:cs_axiom_grounding('58b64195-ec43-4d10-be46-ece8c0c9ac09', territorial_sovereignty_supreme_over_universal_church, conventional).
narrative_ontology:cs_axiom('58b64195-ec43-4d10-be46-ece8c0c9ac09', foundational, religious_difference_as_sovereignty_vehicle).
narrative_ontology:cs_axiom_status(religious_difference_as_sovereignty_vehicle, holdable).
narrative_ontology:cs_axiom_grounding('58b64195-ec43-4d10-be46-ece8c0c9ac09', religious_difference_as_sovereignty_vehicle, empirically_contingent).
narrative_ontology:cs_reference_frame('58b64195-ec43-4d10-be46-ece8c0c9ac09', territorial_sovereignty_framework).
narrative_ontology:cs_drift_state('58b64195-ec43-4d10-be46-ece8c0c9ac09', post_confessional_state_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('58b64195-ec43-4d10-be46-ece8c0c9ac09', '').
narrative_ontology:cs_kernel_id(reformation_composite__political_realignment_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__political_realignment_reading, territorial_rulers).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, imperial_authority).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, papal_authority).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, religious_minorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Emerging sovereigns who assert ius reformandi over their territories, confiscate ecclesiastical property, and enforce confessional conformity. They negotiate with emperor and pope from a position of increasing strength, converting between or aligning with confessions as statecraft demands. The cuius regio settlement formalizes their authority.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, territorial_rulers, agenda_setter,
    powerful, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(reformation_composite__political_realignment_reading, territorial_rulers, beneficiary).

% The Holy Roman Emperor, whose universal jurisdictional claims and enforcement capacity erode as princes withhold taxes, troops, and legal deference. Bound by imperial constitutional tradition, the emperor cannot easily exit the framework without abdicating the imperial office's core claims.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, imperial_authority, payer,
    institutional, generational, constrained, continental).

% The Papal Curia, which loses territorial appointment rights, tax streams, and legal supremacy as rulers establish state churches and seize church lands. The papacy must resist the arrangement to maintain its universal ecclesiastical claims, making exit structurally impossible without dissolving its foundational identity.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, papal_authority, payer,
    institutional, civilizational, constrained, continental).

% Subject populations required to conform to the ruler's chosen confession or face exile, dispossession, or death. They bear the direct cost of the cuius regio arrangement through loss of worship, property, and community. Emigration is possible but economically and socially ruinous.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, religious_minorities, payer,
    powerless, biographical, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_composite__political_realignment_reading, territorial_rulers).
narrative_ontology:fixing_cost_class(reformation_composite__political_realignment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a territorially-bounded religious monopoly that converts pan-European theological conflict into a local sovereign prerogative, reducing the scale and frequency of religious warfare by localizing dissent and loyalty within fixed borders.
% TRANSFER_FUNCTION: Moves sovereignty (ius reformandi, ecclesiastical wealth, legal supremacy) from supra-national imperial and papal structures to territorial rulers; moves religious conformity costs and displacement risk from rulers to subject populations.
% ABSENT_VOICES: Anabaptists and radical reformers who rejected state churches altogether; peasants whose revolts were crushed by the same territorial rulers adopting Reformation theology; non-European Christians and Jews for whom the cuius regio framework was an external imperial imposition.
% DISAPPEARANCE_RATIONALE: Without the political-religious coupling, the sovereign state system loses a foundational early-modern legitimation mechanism; imperial and papal jurisdictions would retain substantial supra-territorial authority; the path to Westphalian sovereignty would be unrecognizable.
% FOUNDING_PROBLEM: How to maintain political order and end endemic religious warfare in Latin Christendom after the theological unity of the medieval church fractured irreparably.
% FOUNDING_PROBLEM_CORROBORATION: Territorial rulers and their chancelleries attest the problem as live, citing the need for civil peace. Imperial and papal authorities attest the problem was manufactured by heretical rebellion and princely greed. Independent municipal chronicles and later historiographical schools outside both beneficiary and victim camps corroborate the scale of disorder but dispute whether cuius regio was the necessary or proportionate response.
narrative_ontology:disappearance_verdict(reformation_composite__political_realignment_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_composite__political_realignment_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__political_realignment_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reformation_composite__political_realignment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_composite__political_realignment_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_composite__political_realignment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reformation_composite__political_realignment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reformation_composite__political_realignment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the substantial transfer of authority and wealth from imperial/papal seats to territorial rulers, formalized in the Peace of Augsburg and intensified through the Thirty Years' War. Suppression (0.71) is high because the cuius regio principle required active enforcement of confessional conformity, exile of dissenters, and seizure of ecclesiastical property. Theater ratio (0.28) is moderate-low: much of the confessional apparatus was functionally sovereign, though performative piety increased during wartime. Accessibility collapse (0.58) captures the closure of alternatives such as universal Christendom, free imperial cities, and cross-territorial religious communities. Resistance (0.48) reflects persistent imperial edicts, papal interdicts, and minority recalcitrance. Temporal measurements show extraction rising from 1517 to 1618 as the political mechanism hardened, then slightly moderating at Westphalia (1648) with limited tolerance clauses.
 *
 * PERSPECTIVAL GAP:
 *   The territorial ruler seat experiences the constraint as a hard-won constitutional settlement that restores civil peace and rightful sovereign prerogative; from this seat, extraction is low or negative (they are the beneficiaries). The imperial and papal seats experience the same arrangement as jurisdictional theft and heretical rebellion â their directionality is near full target. Religious minorities experience it as localized tyranny with constrained exit. The engine will compute these divergences from the structural data: beneficiary declaration plus arbitrage exit for rulers versus victim declaration plus constrained exit for the other seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Territorial rulers are declared beneficiaries with arbitrage-grade exit (can negotiate alliances, switch confessions for political gain), yielding low directionality. Imperial and papal authority are declared victims with constrained exit (bound by their universal claims to resist rather than adapt), yielding high directionality. Religious minorities are powerless victims with constrained exit, sitting near full target. No override is needed because the structural derivation accurately captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The cuius regio settlement is vulnerable to both mislabelings. A purely theological reading might classify it as rope (peaceful confessional coexistence), ignoring the sovereign extraction. A purely Realpolitik reading might classify it as snare (princely power grab), ignoring the genuine coordination function of ending the German Wars of Religion. Tangled_rope captures both: the settlement coordinated peace by localizing conflict, while asymmetrically extracting authority from universal institutions and autonomy from subjects. The founding problem (religious warfare) was real but contested in its severity; the solution outlived the immediate crisis and became a permanent constitutional feature, though not a piton because the beneficiaries (territorial states) were highly concentrated and motivated to maintain it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    political_vs_theological_primacy,
    'Does the Reformation''s primary causal and structural force reside in political sovereignty assertion by territorial rulers, or in independent theological soteriological commitments?',
    'Comparative historiographical analysis of princely conversion timelines versus popular theological adoption; archival study of statecraft correspondence versus theological treatise influence.',
    'If theological commitments are primary, the constraint''s extraction profile shifts downward (more genuine coordination through shared belief); if political interests instrumentalize theology, extraction is higher and the coordination function is more strategic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_vs_theological_primacy, conceptual, 'Whether political sovereignty or theological doctrine is the primary driver of the Reformation constraint').

omega_variable(
    cuius_regio_coordination_genuineness,
    'Was the cuius regio eius religio settlement a genuine coordination mechanism to end religious warfare, or primarily a legitimating formula for princely extraction of imperial and ecclesiastical authority?',
    'Counterfactual analysis of warfare frequency before and after 1555; measurement of ecclesiastical wealth transfer to princely treasuries; assessment of alternative pacification mechanisms proposed but rejected.',
    'If genuine coordination, the asymmetric extraction is the necessary cost of peace; if primarily legitimating, the coordination story is cover for a snare of territorial aggrandizement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cuius_regio_coordination_genuineness, conceptual, 'Ambiguity between coordination function and extraction cover in the cuius regio settlement').

omega_variable(
    reformation_kernel_reading_divergence,
    'How does adopting the political_realignment_reading rather than the theological_fragmentation_reading or technological_mediation_reading alter the beneficiary-victim structure and extraction profile of the Reformation constraint?',
    'Comparative stakeholder analysis across readings; tracking which agents gain directional priority in each reading (state, church, or print bourgeoisie).',
    'Shifting the beneficiary seat changes directionality vectors and thus effective extraction; the political reading produces the highest extraction because sovereignty is a zero-sum transfer from imperial and papal seats.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reformation_kernel_reading_divergence, conceptual, 'Structural divergence point between sibling readings of the Reformation kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__political_realignment_reading, 1517, 1648).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1517, reformation_composite__political_realignment_reading, theater_ratio, 1517, 0.12).
narrative_ontology:measurement(refo_tr_t1521, reformation_composite__political_realignment_reading, theater_ratio, 1521, 0.15).
narrative_ontology:measurement(refo_tr_t1530, reformation_composite__political_realignment_reading, theater_ratio, 1530, 0.2).
narrative_ontology:measurement(refo_tr_t1555, reformation_composite__political_realignment_reading, theater_ratio, 1555, 0.25).
narrative_ontology:measurement(refo_tr_t1618, reformation_composite__political_realignment_reading, theater_ratio, 1618, 0.32).
narrative_ontology:measurement(refo_tr_t1648, reformation_composite__political_realignment_reading, theater_ratio, 1648, 0.38).

% Extraction over time
narrative_ontology:measurement(refo_be_t1517, reformation_composite__political_realignment_reading, base_extractiveness, 1517, 0.18).
narrative_ontology:measurement(refo_be_t1521, reformation_composite__political_realignment_reading, base_extractiveness, 1521, 0.31).
narrative_ontology:measurement(refo_be_t1530, reformation_composite__political_realignment_reading, base_extractiveness, 1530, 0.45).
narrative_ontology:measurement(refo_be_t1555, reformation_composite__political_realignment_reading, base_extractiveness, 1555, 0.6).
narrative_ontology:measurement(refo_be_t1618, reformation_composite__political_realignment_reading, base_extractiveness, 1618, 0.68).
narrative_ontology:measurement(refo_be_t1648, reformation_composite__political_realignment_reading, base_extractiveness, 1648, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1517, reformation_composite__political_realignment_reading, suppression_requirement, 1517, 0.25).
narrative_ontology:measurement(refo_su_t1521, reformation_composite__political_realignment_reading, suppression_requirement, 1521, 0.38).
narrative_ontology:measurement(refo_su_t1530, reformation_composite__political_realignment_reading, suppression_requirement, 1530, 0.52).
narrative_ontology:measurement(refo_su_t1555, reformation_composite__political_realignment_reading, suppression_requirement, 1555, 0.7).
narrative_ontology:measurement(refo_su_t1618, reformation_composite__political_realignment_reading, suppression_requirement, 1618, 0.85).
narrative_ontology:measurement(refo_su_t1648, reformation_composite__political_realignment_reading, suppression_requirement, 1648, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
