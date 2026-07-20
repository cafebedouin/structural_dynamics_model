% ============================================================================
% CONSTRAINT STORY: homoousios_christology__arian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_christology__arian_reading, []).

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
 *   constraint_id: homoousios_christology__arian_reading
 *   human_readable: Arian Christology: Created and Subordinate Logos
 *   domain: historical_theology/ecclesiastical_politics/commitment_systems
 *
 * SUMMARY:
 *   The Arian reading of the Christological kernel holds that the Son is a
 *   created being (ktisma), subordinate to the uncreated Father, and
 *   therefore not homoousios (of identical substance) with him. During the
 *   mid-fourth century, this reading was formalized in imperial creeds
 *   (Sirmium, Rimini-Seleucia) and enforced through the deposition of Nicene
 *   bishops, the exile of resistors, and the patronage of an alternative
 *   episcopal network. It provided genuine theological coordination for a
 *   trans-provincial party while extracting episcopal office, liturgical
 *   legitimacy, and imperial patronage from Nicene and semi-Nicene
 *   Christians. The constraint is claimed as tangled_rope: a coherent
 *   theological system whose persistence required active imperial enforcement
 *   and whose operation suppressed rival readings of the same kernel.
 *
 * KEY AGENTS:
 *   - arian_bishops (institutional/identity_locked) â beneficiaries of imperial legitimacy and episcopal appointment
 *   - imperial_court_arians (institutional/arbitrage) â agenda-setters who enforce theological uniformity for political cohesion
 *   - nicene_bishops (institutional/constrained) â payers who bear exile and deposition
 *   - nicene_monastics (organized/constrained) â payers who preserve resistance outside institutional channels
 *   - semi_arian_theologians (moderate/constrained) â excluded voices whose compromise formula is rejected
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_christology__arian_reading, 0.75).
domain_priors:suppression_score(homoousios_christology__arian_reading, 0.88).
domain_priors:theater_ratio(homoousios_christology__arian_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_christology__arian_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_christology__arian_reading, "Arian Christology: Created and Subordinate Logos").
narrative_ontology:topic_domain(homoousios_christology__arian_reading, "historical_theology/ecclesiastical_politics/commitment_systems").

domain_priors:requires_active_enforcement(homoousios_christology__arian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_christology__arian_reading, 'bb3af4a7-365c-4471-8936-d42fbf3e9b15').
narrative_ontology:cs_kernel_codification('bb3af4a7-365c-4471-8936-d42fbf3e9b15', formalized).
narrative_ontology:cs_authority_grounding('bb3af4a7-365c-4471-8936-d42fbf3e9b15', lineage).
narrative_ontology:cs_interpretation_layer_present('bb3af4a7-365c-4471-8936-d42fbf3e9b15').
narrative_ontology:cs_reading_relation('bb3af4a7-365c-4471-8936-d42fbf3e9b15', homoousios_christology__pro_nicene_reading, forecloses).
narrative_ontology:cs_reading_relation('bb3af4a7-365c-4471-8936-d42fbf3e9b15', homoousios_christology__semi_arian_reading, forecloses).
narrative_ontology:cs_axiom('bb3af4a7-365c-4471-8936-d42fbf3e9b15', foundational, christ_is_created_ktisma).
narrative_ontology:cs_axiom_status(christ_is_created_ktisma, holdable).
narrative_ontology:cs_axiom_grounding('bb3af4a7-365c-4471-8936-d42fbf3e9b15', christ_is_created_ktisma, theological).
narrative_ontology:cs_axiom('bb3af4a7-365c-4471-8936-d42fbf3e9b15', foundational, father_alone_is_agennetos).
narrative_ontology:cs_axiom_status(father_alone_is_agennetos, holdable).
narrative_ontology:cs_axiom_grounding('bb3af4a7-365c-4471-8936-d42fbf3e9b15', father_alone_is_agennetos, theological).
narrative_ontology:cs_reference_frame('bb3af4a7-365c-4471-8936-d42fbf3e9b15', created_logos_monarchy).
narrative_ontology:cs_drift_state('bb3af4a7-365c-4471-8936-d42fbf3e9b15', theodosian_imperial_realignment, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('bb3af4a7-365c-4471-8936-d42fbf3e9b15', '').
narrative_ontology:cs_kernel_id(homoousios_christology__arian_reading, homoousios_christology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_christology__arian_reading, arian_bishops).
narrative_ontology:constraint_beneficiary(homoousios_christology__arian_reading, imperial_court_arians).
narrative_ontology:constraint_victim(homoousios_christology__arian_reading, nicene_bishops).
narrative_ontology:constraint_victim(homoousios_christology__arian_reading, nicene_monastics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Lead dioceses under the Arian theological framework, receiving imperial legitimacy and episcopal appointment security in exchange for adherence to the created-Logos doctrine. Their authority derives from consecration within the imperially-favored ecclesiastical network; exit means deposition and loss of see.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, arian_bishops, beneficiary,
    institutional, generational, identity_locked, continental).

% Imperial household and officials under Constantius II and Valens who enforce Arian conformity through council stacking, exiles, and patronage. They treat theological uniformity as a tool of imperial unity and can pivot creedal allegiance when politically advantageous.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, imperial_court_arians, agenda_setter,
    institutional, biographical, arbitrage, continental).

% Bishops who affirm the Nicene homoousios and refuse the Arian formula. They face deposition, exile, and replacement by Arian appointees. Their institutional power is neutralized by imperial enforcement within the Roman Empire, though they maintain underground networks and alternate sees of refuge.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, nicene_bishops, payer,
    institutional, biographical, constrained, continental).

% Monastic communities that preserve Nicene devotion and scripture copying outside episcopal control. They endure episodic confiscation, harassment, and exclusion from imperially-endorsed church structures while serving as reservoirs of theological resistance.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, nicene_monastics, payer,
    organized, generational, constrained, regional).

% Theologians advocating homoiousios as a mediating position between Nicene and Arian extremes. Their voices are excluded from Arian conciliar assemblies because the strict Arian formula rejects any substantive participation of the Son in the Fatherâs ousia; they are anathematized alongside Nicenes at councils like Constantinople 360.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, semi_arian_theologians, excluded,
    moderate, biographical, constrained, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified theological and liturgical framework for a trans-provincial network of bishops and congregations who distinguish the uncreated Father from the created Son, resolving questions of monotheism and scriptural exegesis through a shared subordinationist cosmology.
% TRANSFER_FUNCTION: Transfers episcopal sees, imperial patronage, and liturgical legitimacy from Nicene and semi-Nicene clergy to Arian-appointed bishops, while transferring the costs of exile, deposition, and anathema onto Nicene believers and monastic communities.
% ABSENT_VOICES: Semi-arian theologians advocating homoiousios are structurally excluded from Arian conciliar assemblies; Egyptian monastic communities under Athanasius are marginalized in imperial court theology; lay congregants with Nicene sympathies lack formal voice in conciliar decree-making.
% DISAPPEARANCE_RATIONALE: If the Arian reading vanished overnight, imperial church appointments would revert to non-Arian candidates, the network of Arian episcopal authority would dissolve, exile decrees would lapse, and the imperial court would require a new theological justification for ecclesiastical unity; the fourth-century ecclesiastical map would reorganize around an alternative Christological formula.
% FOUNDING_PROBLEM: How to maintain monotheistic fidelity to one uncreated God while accounting for the scriptural figure of Jesus Christ, without collapsing Father and Son into a single undifferentiated divine principle.
% FOUNDING_PROBLEM_CORROBORATION: Arian bishops attest the problem remains live from within the beneficiary set. Nicene bishops (Athanasius, Hilary of Poitiers) and modern historians attest the problem was either resolved by homoousios or was itself generated by prior philosophical debates; corroboration from outside the Arian beneficiary set supports the contested status.
narrative_ontology:disappearance_verdict(homoousios_christology__arian_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_christology__arian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_christology__arian_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(homoousios_christology__arian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_christology__arian_reading, 0.75, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_christology__arian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(homoousios_christology__arian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(homoousios_christology__arian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.75) reflects the systematic transfer of sees and patronage from Nicene to Arian clergy under imperial pressure. Suppression (0.88) captures the active enforcement machineryâcouncils, exiles, anathemasârequired to sustain the reading against majority Nicene sentiment in many provinces. Theater_ratio (0.50) indicates that by the height of Constantius II's reign, a substantial portion of conciliar activity served to legitimate imperial policy rather than to resolve genuine theological uncertainty. Accessibility_collapse (0.78) is high because imperial enforcement made Nicene practice dangerous or impossible in many Eastern sees; resistance (0.72) is high due to the sustained Nicene counter-movement (Athanasius, monastic networks). The temporal series show extraction and suppression rising in tandem as imperial enforcement machinery matured between 325 and 365 CE, with theater increasing as the theological debate became a proxy for imperial loyalty.
 *
 * PERSPECTIVAL GAP:
 *   From the Arian episcopal seat, the constraint is genuine coordinationârestoring scriptural monotheism against an innovative philosophical abstraction. From the Nicene seat, the same structure is imperially-enforced extraction that strips legitimate bishops of their sees. The engine computes this divergence from the same structural data: identical spatial_scope and power level, but reversed beneficiary/victim roles and constrained versus identity_locked exit.
 *
 * DIRECTIONALITY LOGIC:
 *   Arian bishops are declared beneficiaries (low d) because the constraint subsidizes their episcopal authority and appoints them to sees. The imperial court is also a beneficiary (low-to-moderate d) because the constraint delivers ecclesiastical unity, though its arbitrage-grade exit means it is not fully captured by the reading. Nicene bishops and monastics are declared victims (high d) because the constraint extracts their offices, security, and liturgical freedom. Semi-arian theologians are excluded (high d) because the strict Arian formula anathematizes their compromise position, making them targets of the same suppression machinery.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the Arian reading as pure coordination (rope) by requiring victim declarations and active enforcement, which are present. It also prevents mislabeling it as pure snare by acknowledging the genuine coordination function it served for a large, trans-provincial theological party that held coherent scriptural and philosophical arguments. The mandatrophy questionâwhether the reading has outlived its founding problemâis addressed in R5: the problem of Christ's relationship to the Father remains live, but the specific Arian solution was contested and eventually repudiated by the imperial church, suggesting a scaffold that failed to sunset rather than a piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    imperial_enforcement_vs_theological_conviction,
    'Is the persistence of the Arian reading due to genuine theological conviction among bishops or primarily due to imperial coercion under Constantius II and Valens?',
    'Historical analysis of Arian persistence after imperial support collapsed, particularly among Germanic peoples and outside the empire, distinguishing voluntary adhesion from coerced conformity.',
    'If purely coerced, the constraint is more snare-like; if genuine coordination, the tangled_rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imperial_enforcement_vs_theological_conviction, empirical, 'Whether Arianism persisted by conviction or coercion.').

omega_variable(
    scriptural_or_philosophical_grounding,
    'Does the Arian reading''s claim that Christ is created rest primarily on scriptural exegesis or on a specific Neo-Platonic/Aristotelian ontology of substance?',
    'Textual analysis of early Arian treatises and creeds to identify the proportion of scriptural versus metaphysical argumentation.',
    'If grounded in a specific philosophy, the constraint''s authority is more contingent and susceptible to axiom_override drift; if purely scriptural, it is more stable as lineage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scriptural_or_philosophical_grounding, conceptual, 'The epistemic grounding of the created-Logos claim.').

omega_variable(
    kernel_reading_beneficiary_inversion,
    'This constraint is the arian_reading of kernel homoousios_christology. How would the beneficiary and victim arrays invert if the pro_nicene_reading were adopted as the imperial standard?',
    'Historical comparison of imperial church structures under Constantius II (Arian enforcement) versus Theodosius I (Nicene enforcement), tracking depositions and exiles in each regime.',
    'The inversion would demonstrate that the structural extraction is reading-relative rather than intrinsic to the imperial church as such, confirming the kernel-decomposition hypothesis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_beneficiary_inversion, conceptual, 'Structural inversion test across sibling kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_christology__arian_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arian_christology_tr_t0, homoousios_christology__arian_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(arian_christology_tr_t8, homoousios_christology__arian_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(arian_christology_tr_t16, homoousios_christology__arian_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement(arian_christology_tr_t24, homoousios_christology__arian_reading, theater_ratio, 24, 0.45).
narrative_ontology:measurement(arian_christology_tr_t32, homoousios_christology__arian_reading, theater_ratio, 32, 0.5).
narrative_ontology:measurement(arian_christology_tr_t40, homoousios_christology__arian_reading, theater_ratio, 40, 0.5).

% Extraction over time
narrative_ontology:measurement(arian_christology_be_t0, homoousios_christology__arian_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(arian_christology_be_t8, homoousios_christology__arian_reading, base_extractiveness, 8, 0.45).
narrative_ontology:measurement(arian_christology_be_t16, homoousios_christology__arian_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(arian_christology_be_t24, homoousios_christology__arian_reading, base_extractiveness, 24, 0.72).
narrative_ontology:measurement(arian_christology_be_t32, homoousios_christology__arian_reading, base_extractiveness, 32, 0.75).
narrative_ontology:measurement(arian_christology_be_t40, homoousios_christology__arian_reading, base_extractiveness, 40, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(arian_christology_su_t0, homoousios_christology__arian_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(arian_christology_su_t8, homoousios_christology__arian_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(arian_christology_su_t16, homoousios_christology__arian_reading, suppression_requirement, 16, 0.75).
narrative_ontology:measurement(arian_christology_su_t24, homoousios_christology__arian_reading, suppression_requirement, 24, 0.88).
narrative_ontology:measurement(arian_christology_su_t32, homoousios_christology__arian_reading, suppression_requirement, 32, 0.9).
narrative_ontology:measurement(arian_christology_su_t40, homoousios_christology__arian_reading, suppression_requirement, 40, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_christology__arian_reading, identity_coordination).
narrative_ontology:affects_constraint(homoousios_christology__arian_reading, pro_nicene_reading).
narrative_ontology:affects_constraint(homoousios_christology__arian_reading, semi_arian_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel homoousios_christology. The sibling readings (pro_nicene_reading, semi_arian_reading) instantiate structurally distinct constraints from the same theological problem. Decomposition is necessary because the epsilon values, beneficiary structures, and enforcement mechanisms differ across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
