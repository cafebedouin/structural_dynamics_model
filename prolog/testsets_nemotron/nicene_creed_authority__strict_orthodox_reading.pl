% ============================================================================
% CONSTRAINT STORY: nicene_creed_authority__strict_orthodox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_creed_authority__strict_orthodox_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: nicene_creed_authority__strict_orthodox_reading
 *   human_readable: Nicene Creed Authority (Strict Orthodox Reading)
 *   domain: systematic_theology/ecclesiology/history_of_christian_doctrine
 *
 * SUMMARY:
 *   The strict orthodox reading of the Nicene Creed treats the 325/381
 *   formulations as binding metaphysical truth: the Trinity is one ousia in
 *   three hypostases; Christ is fully God and fully man in two natures united
 *   in one person. Deviation is not difference but heresy — a cognitive and
 *   volitional error that severs the deviant from the Body of Christ and
 *   warrants canonical sanction (excommunication, anathema, deposition). This
 *   reading has been enforced through imperial law, conciliar canons,
 *   inquisitorial courts, and modern canonical penalties. The constraint
 *   claims to be a Rope (genuine coordination of Christian unity), but its
 *   metric profile shows substantial extraction (epistemic monopoly, material
 *   rents, identity capture) and active suppression of alternatives — a
 *   Tangled Rope.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_creed_authority__strict_orthodox_reading, 0.68).
domain_priors:suppression_score(nicene_creed_authority__strict_orthodox_reading, 0.78).
domain_priors:theater_ratio(nicene_creed_authority__strict_orthodox_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_creed_authority__strict_orthodox_reading, tangled_rope).
narrative_ontology:human_readable(nicene_creed_authority__strict_orthodox_reading, "Nicene Creed Authority (Strict Orthodox Reading)").
narrative_ontology:topic_domain(nicene_creed_authority__strict_orthodox_reading, "systematic_theology/ecclesiology/history_of_christian_doctrine").

domain_priors:requires_active_enforcement(nicene_creed_authority__strict_orthodox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_creed_authority__strict_orthodox_reading, '97bf21f7-b285-417b-aea8-2d0488c0c20f').
narrative_ontology:cs_kernel_codification('97bf21f7-b285-417b-aea8-2d0488c0c20f', formalized).
narrative_ontology:cs_authority_grounding('97bf21f7-b285-417b-aea8-2d0488c0c20f', lineage).
narrative_ontology:cs_interpretation_layer_present('97bf21f7-b285-417b-aea8-2d0488c0c20f').
narrative_ontology:cs_reading_relation('97bf21f7-b285-417b-aea8-2d0488c0c20f', nicene_creed_authority__symbolic_confessional_reading, coexists_with).
narrative_ontology:cs_reading_relation('97bf21f7-b285-417b-aea8-2d0488c0c20f', nicene_creed_authority__liturgical_habituation_reading, coexists_with).
narrative_ontology:cs_axiom('97bf21f7-b285-417b-aea8-2d0488c0c20f', foundational, creed_as_metaphysically_necessary_truth).
narrative_ontology:cs_axiom_status(creed_as_metaphysically_necessary_truth, holdable).
narrative_ontology:cs_axiom_grounding('97bf21f7-b285-417b-aea8-2d0488c0c20f', creed_as_metaphysically_necessary_truth, deontological).
narrative_ontology:cs_axiom('97bf21f7-b285-417b-aea8-2d0488c0c20f', foundational, heresy_as_ontological_error_warranting_sanction).
narrative_ontology:cs_axiom_status(heresy_as_ontological_error_warranting_sanction, holdable).
narrative_ontology:cs_axiom_grounding('97bf21f7-b285-417b-aea8-2d0488c0c20f', heresy_as_ontological_error_warranting_sanction, deontological).
narrative_ontology:cs_reference_frame('97bf21f7-b285-417b-aea8-2d0488c0c20f', conciliar_orthodoxy_as_permanent_metaphysical_standard).
narrative_ontology:cs_drift_state('97bf21f7-b285-417b-aea8-2d0488c0c20f', contemporary_post_vatican_ii_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('97bf21f7-b285-417b-aea8-2d0488c0c20f', '').
narrative_ontology:cs_kernel_id(nicene_creed_authority__strict_orthodox_reading, nicene_creed_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_creed_authority__strict_orthodox_reading, hierarchical_clergy).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__strict_orthodox_reading, magisterial_theologians).
narrative_ontology:constraint_victim(nicene_creed_authority__strict_orthodox_reading, heterodox_communities).
narrative_ontology:constraint_victim(nicene_creed_authority__strict_orthodox_reading, lay_interpreters).
narrative_ontology:constraint_victim(nicene_creed_authority__strict_orthodox_reading, reformist_theologians).
narrative_ontology:constraint_vindicates(nicene_creed_authority__strict_orthodox_reading, trinitarian_ontology).
narrative_ontology:constraint_vindicates(nicene_creed_authority__strict_orthodox_reading, christological_definition_of_chalcedon).
narrative_ontology:constraint_vindicates(nicene_creed_authority__strict_orthodox_reading, apostolic_succession_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define and enforce doctrinal boundaries through episcopal authority, conciliar decrees, and canonical sanctions. Their institutional identity is fused with guardianship of the creed; exit would dissolve their vocation and the structure that legitimates their authority. They collect epistemic rents (interpretive monopoly), institutional rents (control of sacramental validity), and material rents (property, endowments tied to orthodox communion).
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, hierarchical_clergy, agenda_setter,
    institutional, generational, identity_locked, global).

% Hold licensed interpretive authority within the orthodox framework. Their careers, publications, and teaching positions depend on the creed's binding authority. They benefit from the constraint's exclusion of rival interpretive communities. Exit means loss of institutional affiliation and scholarly legitimacy within the communion, but they retain portable academic capital.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, magisterial_theologians, beneficiary,
    organized, biographical, constrained, global).

% Communities whose theological convictions diverge from the creed's metaphysical claims (e.g., non-trinitarian, adoptionist, or modalist groups). They bear sanctions: excommunication, denial of sacramental recognition, property seizure, historical persecution. Their identity is fused with their dissent; exit means abandoning their theological convictions and communal continuity. They are the primary extraction targets.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, heterodox_communities, payer,
    moderate, generational, identity_locked, global).

% Ordinary believers who encounter the creed as a condition of communal belonging and sacramental access. They bear the cognitive cost of assent to propositions they may not understand or privately dissent from, and the social cost of conformity. Exit means leaving their faith community, family networks, and cultural identity — possible but costly. They have no interpretive voice in the magisterium.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, lay_interpreters, payer,
    powerless, biographical, constrained, local).

% Theologians working within the communion who push interpretive boundaries. They face censure, silencing, loss of teaching licenses, and excommunication when their work is deemed heretical. They bear the extractive cost of the constraint's enforcement while nominally benefiting from the communion's resources. Exit means schism or submission.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, reformist_theologians, payer,
    moderate, biographical, constrained, global).

% Representatives of other Christian communions (Oriental Orthodox, Protestant, Catholic) engaged in dialogue. They analyze the constraint's role in ecumenical obstruction or convergence. They neither collect nor pay the extraction but their institutional interests are shaped by the creed's authority structure.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, ecumenical_observers, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, binding metaphysical ontology (Trinitarian, Chalcedonian) that constitutes Christian identity across time and space; solves the coordination problem of doctrinal fragmentation by establishing a canonical boundary that makes communion possible.
% TRANSFER_FUNCTION: Moves interpretive authority, sacramental validity, institutional legitimacy, and material resources from heterodox communities, lay believers, and reformist theologians to the hierarchical clergy and magisterial theologians who guard and define the creed's meaning.
% ABSENT_VOICES: Pre-Nicene theological diversity (subordinationist, modalist, adoptionist traditions) silenced by the creed's adoption; contemporary believers in the Global South whose cultural theologies are marginalized by Hellenistic metaphysical categories; women and lay movements historically excluded from conciliar definition.
% DISAPPEARANCE_RATIONALE: If the strict orthodox reading vanished overnight, the hierarchical clergy would lose their interpretive monopoly and canonical sanctioning power; heterodox communities would gain legitimate space within Christian communion; lay interpreters would face no sanction for private dissent; the global ecumenical map would restructure as the primary doctrinal barrier to reunion dissolved.
% FOUNDING_PROBLEM: The 4th-century church faced existential fragmentation: Arianism threatened to define Christ as a created being, undermining the metaphysical coherence of salvation; multiple competing christologies prevented unified worship and imperial cohesion; the empire needed a single doctrinal standard to legitimize Christian unity as imperial unity.
% FOUNDING_PROBLEM_CORROBORATION: The hierarchical clergy attest the Arian threat remains live in modern guises (unitarianism, liberal christology); historical scholarship outside the benefiting parties (e.g., R.P.C. Hanson, Rowan Williams) shows the 'Arian controversy' was a complex, decades-long debate with multiple legitimate positions, not a simple heresy/orthodoxy binary; the founding problem's framing as metaphysical necessity rather than political consolidation is contested by critical historians.
narrative_ontology:disappearance_verdict(nicene_creed_authority__strict_orthodox_reading, world_rearranges).
narrative_ontology:founding_problem_status(nicene_creed_authority__strict_orthodox_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_creed_authority__strict_orthodox_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(nicene_creed_authority__strict_orthodox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_creed_authority__strict_orthodox_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_creed_authority__strict_orthodox_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nicene_creed_authority__strict_orthodox_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nicene_creed_authority__strict_orthodox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high: the creed's authority structure channels interpretive control, sacramental gatekeeping, and institutional property to the hierarchical clergy. Suppression (0.78) is high: enforcement has historically included imperial coercion, inquisitorial torture, and contemporary canonical exclusion. Theater ratio (0.22) is moderate: the conciliar/liturgical performance is real coordination, but a growing share of enforcement defends the hierarchy's interpretive monopoly rather than the metaphysical claim itself. Accessibility collapse (0.75) is high: once the creed is accepted as metaphysically necessary, alternatives appear not just wrong but incoherent. Resistance (0.45) is moderate: heterodox communities persist but are structurally marginalized.
 *
 * PERSPECTIVAL GAP:
 *   From the hierarchical seat, the creed is a Mountain — the metaphysical structure of reality itself, discovered not imposed. From the heterodox seat, it is a Snare — a politically entrenched boundary that extracts their legitimacy and existence. From the lay seat, it is a Tangled Rope — genuine communion benefit mixed with coerced assent. The engine computes these divergences; the authored claim (tangled_rope) is the generating model's structural judgment, not a reconciliation.
 *
 * DIRECTIONALITY LOGIC:
 *   Hierarchical clergy are full beneficiaries (d ≈ 0.1): they collect the rents and their identity is fused with the constraint. Magisterial theologians are partial beneficiaries (d ≈ 0.25): they benefit but have constrained exit. Heterodox communities are full targets (d ≈ 0.95): identity-locked, they bear the sanctions and cannot exit without self-dissolution. Lay interpreters are targets (d ≈ 0.7): constrained exit, they pay cognitive and social costs. Reformist theologians are targets (d ≈ 0.65): they bear enforcement costs while nominally inside. The engine will compute per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Arian imperial threat) is dead — the Roman Empire is gone, Arianism as a live political-theological force is extinct. Yet the constraint persists at full enforcement intensity. The mandatrophy is unresolved: the hierarchy declares the problem live (modern unitarianism, christological liberalism) but critical historians corroborate that the current enforcement serves institutional self-preservation, not the original coordination function. The constraint has become a vehicle for hierarchical authority maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metaphysical_necessity_vs_constructed_boundary,
    'Is the Trinitarian/Chalcedonian ontology a discovered metaphysical necessity (making the constraint a Mountain) or a historically constructed boundary that serves institutional interests (making it a Snare/Tangled Rope)?',
    'Comparative historical analysis of 4th-century christological debates: if multiple coherent positions existed and the ''orthodox'' position won through imperial politics rather than cognitive necessity, the boundary is constructed. Philosophical analysis of whether the ontology is logically forced or convention-dependent.',
    'If metaphysical necessity, the constraint is a genuine Mountain — extraction and suppression are epiphenomenal to truth-preservation. If constructed, the beneficiary/victim structure is the constraint''s actual function, and the Mountain claim is a false summit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(metaphysical_necessity_vs_constructed_boundary, conceptual, 'Whether the creed''s metaphysical claims are discovered truth or constructed boundary.').

omega_variable(
    heresy_sanction_mechanism,
    'Are heresy sanctions (excommunication, anathema) structural (canonical machinery, state power) or internalized (the believer''s own conscience binds them to the creed''s authority)?',
    'Post-exit trajectory study: track individuals/communities who leave orthodox communion — do they continue to experience the creed''s authority as binding (internalized) or does the sanction dissolve (structural)?',
    'If internalized, effective suppression is higher than structural measures suggest — the constraint travels with the agent after exit. This would amplify χ for identity-locked targets beyond the engine''s structural derivation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(heresy_sanction_mechanism, empirical, 'Structural vs. internalized suppression in heresy policing.').

omega_variable(
    kernel_reading_relations,
    'Does the strict orthodox reading''s core premise (creed = metaphysically necessary, cognitively binding) logically foreclose the sibling readings, or do they coexist as live positions held by different parties?',
    'Analyze whether a single framework could hold both the strict reading and a sibling reading. If the strict reading''s premise (metaphysical necessity) directly contradicts the symbolic reading''s premise (historical contingency) such that no framework could affirm both, the relation is forecloses. If different parties hold them simultaneously without logical contradiction within their respective frameworks, coexists_with.',
    'Forecloses would mean the kernel cannot stably host both readings — one must displace the other. Coexists_with means the kernel hosts a permanent structural dispute. Influences would mean this reading''s dominance shapes the siblings'' operating conditions (e.g., ecumenical dialogue terms) without eliminating them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_relations, conceptual, 'Structural relationship between strict orthodox reading and sibling readings of the Nicene kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_creed_authority__strict_orthodox_reading, 325, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nice_tr_t325, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 325, 0.1).
narrative_ontology:measurement(nice_tr_t381, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 381, 0.15).
narrative_ontology:measurement(nice_tr_t451, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 451, 0.18).
narrative_ontology:measurement(nice_tr_t1054, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 1054, 0.25).
narrative_ontology:measurement(nice_tr_t1517, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 1517, 0.3).
narrative_ontology:measurement(nice_tr_t1870, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 1870, 0.28).
narrative_ontology:measurement(nice_tr_t1965, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 1965, 0.2).
narrative_ontology:measurement(nice_tr_t2025, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 2025, 0.22).

% Extraction over time
narrative_ontology:measurement(nice_be_t325, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 325, 0.45).
narrative_ontology:measurement(nice_be_t381, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 381, 0.55).
narrative_ontology:measurement(nice_be_t451, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 451, 0.62).
narrative_ontology:measurement(nice_be_t1054, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 1054, 0.7).
narrative_ontology:measurement(nice_be_t1517, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 1517, 0.68).
narrative_ontology:measurement(nice_be_t1870, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 1870, 0.72).
narrative_ontology:measurement(nice_be_t1965, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 1965, 0.65).
narrative_ontology:measurement(nice_be_t2025, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(nice_su_t325, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 325, 0.65).
narrative_ontology:measurement(nice_su_t381, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 381, 0.72).
narrative_ontology:measurement(nice_su_t451, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 451, 0.78).
narrative_ontology:measurement(nice_su_t1054, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 1054, 0.82).
narrative_ontology:measurement(nice_su_t1517, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 1517, 0.85).
narrative_ontology:measurement(nice_su_t1870, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 1870, 0.8).
narrative_ontology:measurement(nice_su_t1965, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 1965, 0.7).
narrative_ontology:measurement(nice_su_t2025, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 2025, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_creed_authority__strict_orthodox_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(nicene_creed_authority__strict_orthodox_reading, 0.12).
narrative_ontology:affects_constraint(nicene_creed_authority__strict_orthodox_reading, nicene_creed_authority__symbolic_confessional_reading).
narrative_ontology:affects_constraint(nicene_creed_authority__strict_orthodox_reading, nicene_creed_authority__liturgical_habituation_reading).
narrative_ontology:affects_constraint(nicene_creed_authority__strict_orthodox_reading, chalcedonian_definition_authority).
narrative_ontology:affects_constraint(nicene_creed_authority__strict_orthodox_reading, papal_infallibility_doctrine).
narrative_ontology:affects_constraint(nicene_creed_authority__strict_orthodox_reading, ecumenical_council_authority).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the nicene_creed_authority kernel. The symbolic_confessional_reading and liturgical_habituation_reading are sibling constraints with different ε values, beneficiary/victim structures, and claimed types. They are linked via affects_constraints. The strict reading's high extractiveness and enforcement machinery structurally pressure the siblings' operating space (influences relation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
