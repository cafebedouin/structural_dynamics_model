% ============================================================================
% CONSTRAINT STORY: homoousios_nicene__subordinationist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_nicene__subordinationist_reading, []).

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
 *   constraint_id: homoousios_nicene__subordinationist_reading
 *   human_readable: Nicene Homoousios â Subordinationist Reading
 *   domain: historical_theology/ecclesiastical_history
 *
 * SUMMARY:
 *   This constraint story instantiates the subordinationist reading of the
 *   Nicene homoousios kernel: the claim that the Son derives being from the
 *   Father and shares divinity without ontological equality. It is one of
 *   three structurally distinct readings of the same creedal term. The
 *   reading was operative from the Council of Nicaea (325) through the
 *   Council of Constantinople (381), serving as a theological anchor for
 *   Eusebian and Homoian factions while extracting legitimacy from Nicene
 *   equality advocates. The claim/metric independence is maintained: the
 *   reading is claimed as a genuine coordination mechanism (preserving church
 *   unity through ambiguous formula) while the metrics capture its
 *   substantially extractive, asymmetric operation.
 *
 * KEY AGENTS:
 *   - subordinationist_communities: Primary beneficiary (organized/constrained) â gain theological legitimacy and imperial protection
 *   - nicene_orthodox_communities: Primary target (organized/constrained) â bear exclusion and loss of definitional authority
 *   - imperial_religious_authority: Agenda-setter (institutional/mobile) â enforces whichever reading serves political unity
 *   - conciliar_assemblies: Secondary payer (institutional/constrained) â authority leached away by scriptural-priority arguments
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_nicene__subordinationist_reading, 0.64).
domain_priors:suppression_score(homoousios_nicene__subordinationist_reading, 0.61).
domain_priors:theater_ratio(homoousios_nicene__subordinationist_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, extractiveness, 0.64).
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_nicene__subordinationist_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_nicene__subordinationist_reading, "Nicene Homoousios â Subordinationist Reading").
narrative_ontology:topic_domain(homoousios_nicene__subordinationist_reading, "historical_theology/ecclesiastical_history").

domain_priors:requires_active_enforcement(homoousios_nicene__subordinationist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_nicene__subordinationist_reading, '63eb35d6-5b3e-4a52-86a2-ff7296b3cdf5').
narrative_ontology:cs_kernel_codification('63eb35d6-5b3e-4a52-86a2-ff7296b3cdf5', fixed_text).
narrative_ontology:cs_authority_grounding('63eb35d6-5b3e-4a52-86a2-ff7296b3cdf5', lineage).
narrative_ontology:cs_interpretation_layer_present('63eb35d6-5b3e-4a52-86a2-ff7296b3cdf5').
narrative_ontology:cs_reading_relation('63eb35d6-5b3e-4a52-86a2-ff7296b3cdf5', homoousios_nicene__metaphysical_equality_reading, forecloses).
narrative_ontology:cs_reading_relation('63eb35d6-5b3e-4a52-86a2-ff7296b3cdf5', homoousios_nicene__honorific_similarity_reading, influences).
narrative_ontology:cs_axiom('63eb35d6-5b3e-4a52-86a2-ff7296b3cdf5', foundational, monarchical_theology_subordination).
narrative_ontology:cs_axiom_status(monarchical_theology_subordination, holdable).
narrative_ontology:cs_axiom_grounding('63eb35d6-5b3e-4a52-86a2-ff7296b3cdf5', monarchical_theology_subordination, theological).
narrative_ontology:cs_axiom('63eb35d6-5b3e-4a52-86a2-ff7296b3cdf5', foundational, homoousios_compatible_with_derivation).
narrative_ontology:cs_axiom_status(homoousios_compatible_with_derivation, holdable).
narrative_ontology:cs_axiom_grounding('63eb35d6-5b3e-4a52-86a2-ff7296b3cdf5', homoousios_compatible_with_derivation, theological).
narrative_ontology:cs_reference_frame('63eb35d6-5b3e-4a52-86a2-ff7296b3cdf5', monarchical_theology_framework).
narrative_ontology:cs_drift_state('63eb35d6-5b3e-4a52-86a2-ff7296b3cdf5', post_constantinople_381, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('63eb35d6-5b3e-4a52-86a2-ff7296b3cdf5', '').
narrative_ontology:cs_kernel_id(homoousios_nicene__subordinationist_reading, homoousios_nicene).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_nicene__subordinationist_reading, subordinationist_communities).
narrative_ontology:constraint_victim(homoousios_nicene__subordinationist_reading, nicene_orthodox_communities).
narrative_ontology:constraint_victim(homoousios_nicene__subordinationist_reading, conciliar_assemblies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Theological networks including Eusebian and Homoian communities who hold that the Son derives from the Father and is subordinate in mode of origin. They gain legitimacy, episcopal appointments, and imperial protection from a reading of homoousios that permits their Christology without condemning it as heretical.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, subordinationist_communities, beneficiary,
    organized, generational, constrained, continental).

% Communities committed to metaphysical equality of Father and Son. Under this reading they are excluded from orthodoxy, lose episcopal sees, face conciliar condemnation, and are driven into exile when they insist that homoousios requires full ontological equality.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, nicene_orthodox_communities, payer,
    organized, generational, constrained, continental).

% The Roman emperor and court officials who convoke councils, issue theological edicts, and enforce conformity to secure ecclesiastical unity across the empire. They shift between theological readings based on political coalitions but actively enforce whichever reading currently serves imperial stability.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, imperial_religious_authority, agenda_setter,
    institutional, biographical, mobile, continental).

% Ecumenical and regional councils whose authority to define orthodoxy is undermined when scriptural and monarchical theological arguments are elevated above conciliar decrees. Their decisions are treated as negotiable rather than definitive.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, conciliar_assemblies, payer,
    institutional, generational, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(homoousios_nicene__subordinationist_reading, subordinationist_communities).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolving the Arian controversy by affirming the Son's shared divinity with the Father while permitting a hierarchical ordering of origin, thereby maintaining ecclesiastical communion across factions that reject strict ontological equality.
% TRANSFER_FUNCTION: Transfers theological legitimacy, episcopal authority, and imperial protection to subordinationist communities; transfers costs of exclusion, exile, and loss of definitional authority to Nicene equality adherents and conciliar institutions.
% ABSENT_VOICES: Theologians advocating full metaphysical equality are structurally marginalized from the orthodox conversation; non-Nicene Christian groups and pagan critics are excluded from the conciliar framework entirely.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight, subordinationist communities would lose their theological shelter and face renewed equality-based condemnation; Nicene orthodox would regain definitional authority; imperial religious policy would face immediate pressure to choose between enforcing equality or tolerating open theological fragmentation.
% FOUNDING_PROBLEM: The Arian controversy threatened to fracture the imperial church over the ontological status of the Son; the Council of Nicaea (325) adopted homoousios to secure a shared theological term while leaving its interpretation contested.
% FOUNDING_PROBLEM_CORROBORATION: Eusebian and subordinationist sources attest the need for a formula broad enough to include non-Arians; Athanasian and equality-oriented sources from outside the beneficiary set attest that the founding problem was Arian denial of the Son's true divinity, which this reading inadequately resolves by preserving subordination.
narrative_ontology:disappearance_verdict(homoousios_nicene__subordinationist_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_nicene__subordinationist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_nicene__subordinationist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(homoousios_nicene__subordinationist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_nicene__subordinationist_reading, 0.64, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_nicene__subordinationist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(homoousios_nicene__subordinationist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(homoousios_nicene__subordinationist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.64) is substantial because the reading redistributes theological legitimacy and episcopal authority from equality adherents to subordinationist networks. Suppression (0.61) reflects active enforcement through imperial edicts, conciliar condemnation, and episcopal deposition. Theater ratio (0.44) is moderate-to-high: creedal recitation and liturgical conformity increasingly perform a unity that masks incompatible ontological commitments. Resistance (0.71) is high due to sustained Athanasian and pro-Nicene opposition, exiles, and counter-councils. Accessibility collapse (0.52) is moderate: alternative readings remain intellectually available but are institutionally blocked from orthodox expression.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (imperial authority) experiences the constraint as a flexible political instrument for maintaining unity; the beneficiary seat (subordinationist communities) experiences it as hard-won theological vindication; the payer seats (Nicene orthodox, conciliar assemblies) experience it as enforced doctrinal extraction that suppresses their reading of the same creedal term. The engine computes these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Subordinationist communities sit near the beneficiary end: the constraint subsidizes their theological position and protects them from heresy charges. Nicene orthodox communities sit near the target end: they bear the costs of exclusion and exile. Imperial authority sits near symmetric but with mobility: it benefits from ecclesiastical unity but pays political costs when theological fragmentation threatens stability. Conciliar assemblies are secondary targets: they do not pay with exile but with eroded institutional authority.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading risks mislabeling as a rope if one looks only at its coordination function (preserving church unity through shared formula) and ignores the asymmetric extraction (subordinationist communities collect legitimacy while equality adherents pay in exclusion). It risks mislabeling as a snare if one ignores the genuine coordination problem (the Arian controversy genuinely threatened imperial church unity). The tangled_rope classification captures both: real coordination function plus asymmetric extraction sustained by active enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    political_vs_theological_primacy,
    'Does the persistence of the subordinationist reading reflect genuine theological conviction about monarchical theology, or is it primarily sustained by imperial political coalitions that find subordinationism useful for social control?',
    'Prosopographical analysis of episcopal appointments and imperial edicts: if subordinationist bishops cluster around imperial courts and shift allegiance when dynasties change, political primacy is indicated; if subordinationist communities persist across political regimes, theological primacy is indicated.',
    'If primarily political, the constraint tends toward snare (coordination story as cover for imperial extraction of unity); if primarily theological, it remains a tangled rope with genuine coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_vs_theological_primacy, empirical, 'Whether imperial politics or theological conviction drives the reading').

omega_variable(
    homoousios_semantic_stability,
    'Was homoousios at Nicea (325) intended as a term compatible with subordination, or did the subordinationist reading retroactively impose compatibility on a term originally meant to exclude Arianism?',
    'Historical-philological analysis of pre-Nicene and immediate post-Nicene usage; correspondence of Eusebius of Caesarea and council records.',
    'If the term was originally subordinationist-compatible, the constraint began as a rope that became tangled through contest; if retroactively imposed, it is a snare using a coordination term as cover.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(homoousios_semantic_stability, empirical, 'Original semantic intent of homoousios at Nicea').

omega_variable(
    cs_framing_underdetermination,
    'Should the authority structure be framed as lineage-based (grounded in conciliar transmission of the Nicene formula) or extraction-based (imperial and subordinationist elites extracting legitimacy from a fixed text)?',
    'Comparative analysis of authority claims: if the reading''s defenders appeal primarily to conciliar succession and tradition, lineage framing is vindicated; if they appeal to imperial edict and scriptural proof-texts to bypass conciliar definition, extraction framing is indicated.',
    'Lineage framing strengthens the coordination reading and lowers derived extraction; extraction framing foregrounds the active suppression of conciliar authority and raises derived extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Alternative commitment-system framings and their classification consequences').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_nicene__subordinationist_reading, 0, 56).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t0, homoousios_nicene__subordinationist_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(homo_tr_t14, homoousios_nicene__subordinationist_reading, theater_ratio, 14, 0.32).
narrative_ontology:measurement(homo_tr_t28, homoousios_nicene__subordinationist_reading, theater_ratio, 28, 0.48).
narrative_ontology:measurement(homo_tr_t42, homoousios_nicene__subordinationist_reading, theater_ratio, 42, 0.54).
narrative_ontology:measurement(homo_tr_t56, homoousios_nicene__subordinationist_reading, theater_ratio, 56, 0.45).

% Extraction over time
narrative_ontology:measurement(homo_be_t0, homoousios_nicene__subordinationist_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(homo_be_t14, homoousios_nicene__subordinationist_reading, base_extractiveness, 14, 0.52).
narrative_ontology:measurement(homo_be_t28, homoousios_nicene__subordinationist_reading, base_extractiveness, 28, 0.66).
narrative_ontology:measurement(homo_be_t42, homoousios_nicene__subordinationist_reading, base_extractiveness, 42, 0.71).
narrative_ontology:measurement(homo_be_t56, homoousios_nicene__subordinationist_reading, base_extractiveness, 56, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t0, homoousios_nicene__subordinationist_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(homo_su_t14, homoousios_nicene__subordinationist_reading, suppression_requirement, 14, 0.58).
narrative_ontology:measurement(homo_su_t28, homoousios_nicene__subordinationist_reading, suppression_requirement, 28, 0.72).
narrative_ontology:measurement(homo_su_t42, homoousios_nicene__subordinationist_reading, suppression_requirement, 42, 0.76).
narrative_ontology:measurement(homo_su_t56, homoousios_nicene__subordinationist_reading, suppression_requirement, 56, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(homoousios_nicene__subordinationist_reading, metaphysical_equality_reading).
narrative_ontology:affects_constraint(homoousios_nicene__subordinationist_reading, honorific_similarity_reading).

% DUAL FORMULATION NOTE:
% The homoousios_nicene kernel decomposes into three structurally distinct constraints: the subordinationist reading (same essence with hierarchical derivation), the metaphysical equality reading (same essence with full equality), and the honorific similarity reading (similarity without strict identity). Each reading has distinct beneficiary/victim structures, epsilon values, and enforcement mechanisms. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
