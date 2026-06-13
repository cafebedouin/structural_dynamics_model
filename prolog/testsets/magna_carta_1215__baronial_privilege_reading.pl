% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__baronial_privilege_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_1215__baronial_privilege_reading, []).

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
 *   constraint_id: magna_carta_1215__baronial_privilege_reading
 *   human_readable: Magna Carta 1215: Baronial Privilege Reading (Feudal Contract)
 *   domain: constitutional_law/legal_history
 *
 * SUMMARY:
 *   The Magna Carta of 1215 is read in this constraint as a feudal contract
 *   between King John and the English landed magnates (barons, ecclesiastical
 *   magnates, and London merchant oligarchs), not as a declaration of
 *   universal rights. Under this reading, 'free men' means landowning feudal
 *   tenants-in-chief, numbering approximately 200. The constraint operates as
 *   a tangled rope: it coordinates the king-baron feudal relationship
 *   (removing uncertainty from inheritance incidents, wardship profit-taking,
 *   and arbitrary tallages) while simultaneously extracting from the crown
 *   (the payer) and concentrating benefit among landholder beneficiaries. The
 *   commoners, peasants, and unfree laborers of England are structurally
 *   excluded from both the protection set and the negotiating coalition. This
 *   reading treats the Charter as a narrow class victory for the magnate
 *   coalition, not as an early statement of universal procedural due process.
 *   The sibling readings (living_document_reading, universal_rights_reading)
 *   interpret the same text and authority structure (the Charter and its
 *   reissues) but frame the constraint's scope, beneficiary class, and
 *   legitimacy differently.
 *
 * KEY AGENTS:
 *   - landed_barons: Negotiators and primary beneficiaries; roughly 200 tenants-in-chief whose feudal privileges are formalized and protected
 *   - crown (King John and successors): Payer; loses extractive feudal prerogatives; enforcement is enacted through baronial leverage and later ecclesiastical sanction
 *   - ecclesiastical_magnates: Co-beneficiaries and enforcers; bishops and abbots who negotiated and guaranteed the Charter through excommunication threats
 *   - commoners_peasants_unfree: Excluded; structurally absent from the negotiation and protection set; remain in pre-1215 feudal subordination
 *   - london_merchants: Partial beneficiaries; secured municipal freedoms and commercial standardization as allied interests of the baronial coalition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__baronial_privilege_reading, 0.68).
domain_priors:suppression_score(magna_carta_1215__baronial_privilege_reading, 0.72).
domain_priors:theater_ratio(magna_carta_1215__baronial_privilege_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__baronial_privilege_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_1215__baronial_privilege_reading, "Magna Carta 1215: Baronial Privilege Reading (Feudal Contract)").
narrative_ontology:topic_domain(magna_carta_1215__baronial_privilege_reading, "constitutional_law/legal_history").

domain_priors:requires_active_enforcement(magna_carta_1215__baronial_privilege_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__baronial_privilege_reading, '6201f341-bed2-47d4-90f2-48795070b6d8').
narrative_ontology:cs_kernel_codification('6201f341-bed2-47d4-90f2-48795070b6d8', fixed_text).
narrative_ontology:cs_authority_grounding('6201f341-bed2-47d4-90f2-48795070b6d8', lineage).
narrative_ontology:cs_interpretation_layer_present('6201f341-bed2-47d4-90f2-48795070b6d8').
narrative_ontology:cs_reading_relation('6201f341-bed2-47d4-90f2-48795070b6d8', magna_carta_1215__universal_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('6201f341-bed2-47d4-90f2-48795070b6d8', magna_carta_1215__living_document_reading, coexists_with).
narrative_ontology:cs_axiom('6201f341-bed2-47d4-90f2-48795070b6d8', foundational, free_men_equals_landed_magnates).
narrative_ontology:cs_axiom_status(free_men_equals_landed_magnates, holdable).
narrative_ontology:cs_axiom_grounding('6201f341-bed2-47d4-90f2-48795070b6d8', free_men_equals_landed_magnates, conventional).
narrative_ontology:cs_axiom('6201f341-bed2-47d4-90f2-48795070b6d8', foundational, charter_binds_crown_feudal_reciprocity_only).
narrative_ontology:cs_axiom_status(charter_binds_crown_feudal_reciprocity_only, holdable).
narrative_ontology:cs_axiom_grounding('6201f341-bed2-47d4-90f2-48795070b6d8', charter_binds_crown_feudal_reciprocity_only, deontological).
narrative_ontology:cs_reference_frame('6201f341-bed2-47d4-90f2-48795070b6d8', feudal_reciprocal_obligation_codified).
narrative_ontology:cs_drift_state('6201f341-bed2-47d4-90f2-48795070b6d8', early_13th_century_reissue_cycle, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6201f341-bed2-47d4-90f2-48795070b6d8', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__baronial_privilege_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__baronial_privilege_reading, landed_barons).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, crown).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, commoners_peasants_unfree).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__baronial_privilege_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(magna_carta_1215__baronial_privilege_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_1215__baronial_privilege_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_1215__baronial_privilege_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_1215__baronial_privilege_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at the end of the interval (1225) is 0.68 because the Charter constrains the crown's feudal exaction but leaves the feudal extraction of the barons over their own subjects untouched — it is a redistribution of prerogative within the magnate class, not a dismantling of extraction. Suppression is high (0.72) because enforcement depends on active baronial (and ecclesiastical) pressure to keep the crown from reverting to arbitrary will; the crown repeatedly violated the 1215 Charter, necessitating reissues (1217, 1225) with hardening enforcement clauses. Theater is moderate (0.28) because the Charter's stated coordination function is real — it does stabilize the feudal bargain — but increasing enforcement burden shows the constraint's instability. The measurement series shows extractiveness rising from 1215 (0.55) to 1217 (0.68), then stabilizing as the reissued Charters (1217, 1225) formalize enforcement through ecclesiastical sanction and baronial coalition discipline.
 *
 * PERSPECTIVAL GAP:
 *   The landed-baron seat experiences the constraint as a victory: they negotiated it, it protects their interests, and it formalizes their feudal leverage. From the crown's seat, it is enforced extraction of prerogative through collective baronial and ecclesiastical power. From the commoners' seats (structured into the excluded category), the constraint does not materially alter their feudal subordination to the barons — they see no benefit and no protection in Clause 39 (lawful judgment) because 'lawful' means judgment by their feudal lord. The engine should compute different directionalities for these seats: d near 0.0 (beneficiary) for landed barons, d near 1.0 (target) for the crown, and d near 0.5 (unaffected symmetric) for commoners — they neither gain nor lose from this particular constraint because it never applied to them.
 *
 * DIRECTIONALITY LOGIC:
 *   The landed barons are the structural beneficiaries — they extracted the Charter, benefit from codified feudal protection, and lose nothing from the constraint (d ~ 0.15–0.25: full beneficiaries). The crown is the structural target — loses feudal exaction capacity, faces enforcement pressure, has constrained exit (cannot renounce the Charter without facing baronial revolt or ecclesiastical sanction) (d ~ 0.85–0.95: full target). Ecclesiastical magnates are beneficiaries with enforcement power (d ~ 0.2: beneficiary, shifted upward slightly by enforcement cost, but still net-positive). Commoners and peasants are neither beneficiaries nor victims of THIS constraint — they are excluded from its scope; the constraint does not improve or worsen their feudal subordination to the barons (d ~ 0.5: symmetric, because unaffected). London merchants are secondary beneficiaries (d ~ 0.35–0.45: partial beneficiary, gained municipal privileges only through baronial coalition membership). The crown's identity-lock is institutional — renouncing the Charter triggers immediate baronial revolt and ecclesiastical excommunication, so it faces trapped exit, not just constrained exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (arbitrary feudal exaction by King John) remains structurally live in this reading: King John's successors (Henry III, later kings) repeatedly violated the Charter, necessitating reissues and enforcement cycles through the 13th century. The problem does not die — it is re-suppressed through baronial coalition discipline and ecclesiastical sanction. However, the mandatrophy resolution is tricky: the constraint is NOT degraded into a piton (the coordination function — formalized feudal obligation — is real and repeatedly asserted through reissues). Instead, the constraint is continuously reinforced through enforcement cycles. The theater ratio rises from 0.12 to 0.28 because enforcement machinery (excommunication threats, baronial assembly legitimacy) becomes increasingly theatrical: the constraint's enforcement function shifts from the original contractual negotiation toward institutional performance of 'the Charter' as symbol. This is NOT mandatrophy (the constraint's primary function is not atrophied) but rather the beginning of the symbolic reification that enables later 'universal rights' readings to claim the same text.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    free_men_scope_ambiguity,
    'Does ''free men'' (liberi homines) in the original Magna Carta text refer to all persons, all non-enslaved persons, or only landowning feudal tenants?',
    'Philological and contextual analysis of 1215 usage; examination of contemporary charters, legal documents, and Chronicle accounts of the drafting; comparison with how ''free'' and ''free person'' are deployed in other medieval legal texts of the period.',
    'If ''free men'' means all non-enslaved persons, the universal_rights_reading becomes textually grounded; if it means only landowning tenants, the baronial_privilege_reading is more defensible. This ambiguity is inscribed in the original text and cannot be resolved by pure textual analysis — it depends on historical linguistic reconstruction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(free_men_scope_ambiguity, empirical, 'The referent set of ''free men'' in 1215 usage').

omega_variable(
    kernel_versus_readings_legitimacy,
    'Is the Magna Carta 1215 a kernel that can sustain multiple readings, or does it have a single original (baronial privilege) reading that later readings misappropriate?',
    'Historiographical consensus and genealogy of readings: did medieval law itself (13th–15th centuries) recognize the Charter as capable of multiple interpretations, or was the reinterpretation as ''universal rights'' a post-hoc innovation (16th–19th centuries)? At what historical moment did interpretive divergence become deliberate rather than accidental?',
    'If the Charter genuinely sustains multiple readings (kernel status confirmed), all three readings (baronial_privilege, universal_rights, living_document) have equal structural legitimacy and coexist. If it does not, the baronial_privilege reading is the original and the others are extensions/distortions — which affects the type classification (baronial reading is primary Rope/Tangled Rope; others are secondary reinterpretations of that primary constraint).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_versus_readings_legitimacy, conceptual, 'Whether the Magna Carta is a contested kernel or a single-reading constraint misread over time').

omega_variable(
    enforcement_coalescence_and_symbolic_capture,
    'Why does the theater ratio rise from 0.12 (1215) to 0.28 (1225), and what does this rise indicate about the constraint''s evolution?',
    'Examination of enforcement mechanisms: what portion of the Charter''s 1225 enforcement apparatus is active suppression (barons forcing the crown to comply) versus institutional performance (the Charter as symbol, the reissue ceremony as legitimacy theater)? Traced through the Provisions of Oxford (1258) and the Barons'' Wars (1264–1267), which institutionalize the Charter''s role as a symbol of baronial legitimacy against the crown.',
    'Rising theater ratio could indicate (a) degradation into piton, (b) transition from ad-hoc enforcement to institutionalized performance, or (c) the Charter''s capture as a symbolic framework that later enables ''universal rights'' readings to claim the same text. If (c), the rising theater ratio is the harbinger of the reading-shift — the constraint becomes valuable AS SYMBOL before becoming valuable as legal principle to a wider class.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_coalescence_and_symbolic_capture, empirical, 'The role of theatrical enforcement in the Charter''s stabilization and later reinterpretation').

omega_variable(
    commoner_exclusion_structural_versus_rhetorical,
    'Is the exclusion of commoners, peasants, and unfree laborers from the Charter''s protection a structural necessity of feudal law (only barons have standing to sue the king in feudal court), or a rhetorical choice to exclude classes whose protection would have threatened baronial interests?',
    'Comparative feudal law across Europe; examination of whether other feudal monarchies'' charters extended protection to non-landholder classes; examination of whether the barons explicitly debated and rejected extending protection to commoners, or whether the exclusion was implicit in the feudal legal framework.',
    'If structural (feudal law permits only barons to sue the king), the baronial_privilege reading is descriptively accurate and the universal_rights reading is anachronistic. If rhetorical/chosen (barons could have extended protection but chose not to), the baronial reading is deliberately partial and the universal_rights reading reflects a later choice to universalize.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commoner_exclusion_structural_versus_rhetorical, empirical, 'Whether commoner exclusion is inherent to feudal law or a strategic baronial choice').

omega_variable(
    sibling_reading_foreclosure_ambiguity,
    'Does the baronial_privilege reading (narrow scope) logically FORECLOSE the universal_rights reading, or can both readings coexist within different interpretive traditions?',
    'Examine whether a party can hold both interpretations simultaneously without logical contradiction: Can one believe the Charter was ORIGINALLY a baronial privilege AND BECOME a universal rights document through legitimate reinterpretation? Or does claiming one reading''s truth require denying the other reading''s coherence?',
    'If foreclosed: the relation to universal_rights_reading is ''forecloses'' (singular core premise rules out the other). If coexists: the relation is ''coexists_with'' (different parties hold different readings without logical contradiction). The cs_structure.reading_relations field depends on this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_ambiguity, conceptual, 'Whether the baronial and universal readings are logically incompatible or can be held simultaneously in different frameworks').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__baronial_privilege_reading, 1215, 1225).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1215, 0.12).
narrative_ontology:measurement(magn_tr_t1216, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1216, 0.15).
narrative_ontology:measurement(magn_tr_t1217, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1217, 0.2).
narrative_ontology:measurement(magn_tr_t1220, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1220, 0.28).
narrative_ontology:measurement(magn_tr_t1223, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1223, 0.29).
narrative_ontology:measurement(magn_tr_t1225, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1225, 0.28).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1215, 0.55).
narrative_ontology:measurement(magn_be_t1216, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1216, 0.62).
narrative_ontology:measurement(magn_be_t1217, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1217, 0.68).
narrative_ontology:measurement(magn_be_t1220, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1220, 0.68).
narrative_ontology:measurement(magn_be_t1223, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1223, 0.66).
narrative_ontology:measurement(magn_be_t1225, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1225, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1215, 0.58).
narrative_ontology:measurement(magn_su_t1216, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1216, 0.65).
narrative_ontology:measurement(magn_su_t1217, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1217, 0.71).
narrative_ontology:measurement(magn_su_t1220, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1220, 0.72).
narrative_ontology:measurement(magn_su_t1223, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1223, 0.71).
narrative_ontology:measurement(magn_su_t1225, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1225, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__baronial_privilege_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_1215__baronial_privilege_reading, 0.12).
narrative_ontology:affects_constraint(magna_carta_1215__baronial_privilege_reading, magna_carta_1215__universal_rights_reading).
narrative_ontology:affects_constraint(magna_carta_1215__baronial_privilege_reading, magna_carta_1215__living_document_reading).
narrative_ontology:affects_constraint(magna_carta_1215__baronial_privilege_reading, english_common_law_precedential_authority).
narrative_ontology:affects_constraint(magna_carta_1215__baronial_privilege_reading, feudal_reciprocal_obligation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'magna_carta_1215'. Sibling constraints with different readings of the same text: magna_carta_1215__universal_rights_reading (scope extended to all persons) and magna_carta_1215__living_document_reading (legitimacy grounded in interpretive tradition rather than original feudal contract). All three stories link via network.affects_constraints to form the constraint family. The epsilon values differ substantially across readings because the scope of 'free men' and the beneficiary class differ, producing different structural extraction profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
