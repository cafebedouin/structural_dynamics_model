% ============================================================================
% CONSTRAINT STORY: zionist_legitimacy_basis__settler_colonial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zionist_legitimacy_basis__settler_colonial_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: zionist_legitimacy_basis__settler_colonial_reading
 *   human_readable: Zionist Settlement and Indigenous Displacement (Settler-Colonial Reading)
 *   domain: political_history/nationalism/settler_colonialism
 *
 * SUMMARY:
 *   This constraint story models Zionism under the settler-colonial reading:
 *   a movement originating in 19th-century European antisemitic persecution
 *   that solved European Jewish insecurity by establishing territorial
 *   control in Palestine through displacement of the Arab indigenous
 *   population. The reading frames the constraint not as legitimate national
 *   liberation or religious restoration, but as a structure of colonial land
 *   seizure, demographic replacement, and governance enforcement that
 *   persists by suppressing Palestinian political claims and maintaining the
 *   coercive apparatus (military occupation, settlement expansion, legal
 *   discrimination) that enforces the displacement. The extractiveness is
 *   high and stable because the transfer (territory, sovereignty, demographic
 *   dominance) remains in place; theater rises through 1967 and plateaus as
 *   the constraint shifts from settlement-building to occupation-management,
 *   with increasing rhetorical work (security discourse, democratic
 *   legitimacy claims) masking the persistence of the underlying
 *   displacement. This is ONE reading of a contested kernel; the
 *   national-liberation reading and religious-restoration reading are
 *   structurally distinct constraints with different beneficiary sets,
 *   different ε values, and different mandate-status claims. All three
 *   coexist as live political and historiographic positions held by different
 *   parties.
 *
 * KEY AGENTS:
 *   - jewish_european_settlers: Organized political/military movement establishing territorial control through purchase, colonization, and displacement (1882–1948). Power: organized; exit: mobile (could have chosen alternate locations or alternate political strategies). Structural position: agenda-setter and primary beneficiary.
 *   - palestinian_arab_population: Indigenous residents experiencing systematic land dispossession, forced displacement (1948 nakba, 1967), and occupation. Power: powerless; exit: trapped (cannot remain unaffected; flight or resistance are only options). Structural position: primary victim.
 *   - israeli_state_apparatus: Post-1948 institutional structure inheriting and operating the settler-colonial arrangement. Power: institutional; exit: arbitrage (can modulate enforcement while maintaining core displacement). Beneficiary of territory, sovereignty, demographic dominance.
 *   - international_zionist_movement: Global network sustaining legitimacy narrative and material support. Power: powerful; exit: arbitrage (can shift strategy while maintaining narrative framework).
 *   - western_governments: Strategic beneficiaries receiving military/intelligence partnership and validation of liberal-democratic alignment. Power: institutional; exit: arbitrage (can shift policy without fundamental realignment).
 *   - palestinian_resistance_movements: Resistance from within the constraint; unable to exit but capable of contesting legitimacy and mounting armed/political challenge.
 *   - international_law_institutions: Observers documenting violations and adjudicating claims; structurally excluded from enforcement.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__settler_colonial_reading, 0.82).
domain_priors:suppression_score(zionist_legitimacy_basis__settler_colonial_reading, 0.79).
domain_priors:theater_ratio(zionist_legitimacy_basis__settler_colonial_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__settler_colonial_reading, snare).
narrative_ontology:human_readable(zionist_legitimacy_basis__settler_colonial_reading, "Zionist Settlement and Indigenous Displacement (Settler-Colonial Reading)").
narrative_ontology:topic_domain(zionist_legitimacy_basis__settler_colonial_reading, "political_history/nationalism/settler_colonialism").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__settler_colonial_reading, '2bf7ebc1-c15e-4344-850d-4c631c15b34e').
narrative_ontology:cs_kernel_codification('2bf7ebc1-c15e-4344-850d-4c631c15b34e', formalized).
narrative_ontology:cs_authority_grounding('2bf7ebc1-c15e-4344-850d-4c631c15b34e', lineage).
narrative_ontology:cs_interpretation_layer_present('2bf7ebc1-c15e-4344-850d-4c631c15b34e').
narrative_ontology:cs_reading_relation('2bf7ebc1-c15e-4344-850d-4c631c15b34e', zionist_legitimacy_basis__national_liberation_reading, coexists_with).
narrative_ontology:cs_reading_relation('2bf7ebc1-c15e-4344-850d-4c631c15b34e', zionist_legitimacy_basis__religious_restoration_reading, coexists_with).
narrative_ontology:cs_axiom('2bf7ebc1-c15e-4344-850d-4c631c15b34e', foundational, displacement_is_constitutive).
narrative_ontology:cs_axiom_status(displacement_is_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('2bf7ebc1-c15e-4344-850d-4c631c15b34e', displacement_is_constitutive, empirically_contingent).
narrative_ontology:cs_axiom('2bf7ebc1-c15e-4344-850d-4c631c15b34e', foundational, colonial_structure_determines_legitimacy).
narrative_ontology:cs_axiom_status(colonial_structure_determines_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('2bf7ebc1-c15e-4344-850d-4c631c15b34e', colonial_structure_determines_legitimacy, deontological).
narrative_ontology:cs_reference_frame('2bf7ebc1-c15e-4344-850d-4c631c15b34e', colonial_territorial_acquisition_framework).
narrative_ontology:cs_drift_state('2bf7ebc1-c15e-4344-850d-4c631c15b34e', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2bf7ebc1-c15e-4344-850d-4c631c15b34e', '').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__settler_colonial_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, jewish_european_settlers).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, israeli_state_apparatus).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, palestinian_arab_population).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, arab_indigenous_residents).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__settler_colonial_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(zionist_legitimacy_basis__settler_colonial_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zionist_legitimacy_basis__settler_colonial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(zionist_legitimacy_basis__settler_colonial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(zionist_legitimacy_basis__settler_colonial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82 at 2024) and stable post-1948 because the core transfer (territory, sovereignty, political legitimacy) is complete and defended against reversal. The measurement series show extraction accelerating from 1882 to 1948 as settlement expanded and displacement culminated in institutional statehood, then plateauing post-1948 at high levels because the fundamental asymmetry is fixed: Palestinians remain under jurisdiction without political power (1948–1967) or under occupation without sovereignty (1967–present). Suppression is similarly high (0.79) and required because Palestinian resistance to the displacement is continuous and suppressed only by military enforcement, legal restriction, and administrative control — the constraint would not persist if enforcement relaxed. Theater rises from low (0.15, early settlement) through moderate (0.42 by 1948, when displacement is narrated as liberation and statehood) to plateau (0.48 by 1967, stabilizing as occupation adds security justification). The theatrical component is the gap between the constraint's operation (enforced displacement and control) and its narration (national liberation, democratic state, security necessity). Accessibility_collapse is moderate-high (0.71): once Palestinians understand they are displaced populations without political voice, exit by remaining becomes impossible; resistance and flight are the only alternatives — but alternatives are suppressed by enforcement. Resistance is high (0.68) because Palestinian political and armed resistance is continuous, met by superior Israeli military force — the constraint persists not because Palestinians consent but because they cannot successfully resist.
 *
 * PERSPECTIVAL GAP:
 *   From the settler/Israeli state seat, this arrangement is national liberation and state-building: a persecuted people reclaiming ancestral homeland and establishing sovereignty. From the Palestinian seat, it is colonization and dispossession: the transfer of their territory to outsiders under military force, narrated as their own displacement. The engine computes divergent classifications from these opposed structural positions: the Israeli institutional seat would compute moderate extraction (sees coordination benefit from state-building) and lower suppression (sees enforcement as legitimate security); the Palestinian victim seat computes high extraction (territorial loss, political exclusion) and high suppression (military occupation, legal discrimination). The gap is not resolvable through reframing — it reflects the zero-sum nature of the transfer and the asymmetric power to define the constraint's legitimacy.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish settlers and Israeli state apparatus are structural beneficiaries (d near 0.0): they acquire territory, establish political sovereignty, and control the narrative. Their exit options are high (could have pursued alternatives or negotiated differently); their power is organized/institutional. Palestinian population is structural target (d near 1.0): they lose territory, face displacement, and lack political voice. Their exit options are trapped (cannot remain unaffected); their power is powerless. The asymmetry is constitutive of the constraint — the transfer only works because one party has the power to impose it and the other lacks the power to resist it. Directionality overrides are not needed; the structural derivation captures the true relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading declares snare rather than rope because the coordination narrative (national liberation) is cover for the extraction narrative (territorial displacement). The founding problem in this reading was not a Palestinian collective-action problem that needed solving but a European Jewish security problem that was solved by creating a Palestinian displacement problem. The coordination function present in the national-liberation reading dissolves in this reading: there is no genuine problem both beneficiary and victim populations were trying to solve together. Instead, one population solved its security problem by imposing a displacement problem on another. The snare classification prevents misreading the constraint as coordination (which the rope/tangled-rope types would imply) when it is structured as zero-sum extraction masked by coordination rhetoric. Mandatrophy is present (founding problem is partially dead — state security for Jewish population is substantially achieved; but the arrangement persists, narrated as permanent necessity) and the mismatch is the diagnostic signal: the constraint was justified as temporary (build a state, achieve security) but persists as permanent (occupation, settlement, ethnic dominance). This is exactly the zombie pattern the engine is designed to detect.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intentionality_vs_systemic_outcome,
    'Was Palestinian displacement the intentional goal of Zionist settlement, or an unintended but foreseeable outcome of pursuing Jewish territorial consolidation?',
    'Historical textual analysis of Zionist leadership statements, planning documents, and contemporaneous writings; comparison of stated goals with actual implemented policies; testimony from multiple historiographic traditions (Israeli, Palestinian, external).',
    'If intentional: the snare classification holds; the constraint is deliberately extractive. If unintended but systemic: the classification may shift toward tangled_rope (coordination for Jewish security that imposes extraction on Palestinians as side effect) — though the effect is similar, the intentionality distinction affects narrative credibility. The settler-colonial reading assumes intentionality; if the evidence shows structural inevitability without deliberate choice, the reading must adjust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intentionality_vs_systemic_outcome, empirical, 'Intentional displacement vs. structural inevitability as outcome of competing territorial claims').

omega_variable(
    legitimacy_framework_incommensurability,
    'Are the settler-colonial reading and the national-liberation reading genuinely incommensurable, or is there a higher-order framework that could adjudicate between them?',
    'Philosophical analysis of foundational premises: if national self-determination is grounded in ancestral/historical claims, can displacement of an intervening population be legitimate? If it cannot, does international law constrain national-determination claims? If national-determination-by-displacement is indefensible, does the reading framework (liberal nationalism, indigenous rights, settler-colonial theory) supply that constraint universally?',
    'If the readings are incommensurable (each coherent within its own framework, neither reducible to the other), the kernel remains contested indefinitely, and this reading is one pole of an irreducible dispute. If a higher framework adjudicates (e.g., international law establishes displacement as impermissible), then one reading is structurally privileged — but that finding would likely shift institutional consensus, which has not happened. The incommensurability assumption is built into this story; it is an omega because if it is wrong, the entire dispute structure collapses.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_framework_incommensurability, conceptual, 'Whether the settler-colonial and national-liberation readings occupy incommensurable frameworks or are arbitrable by higher authority').

omega_variable(
    suppression_internalization_in_palestinian_identity,
    'Is the measured suppression (0.79) entirely structural (military occupation, legal barriers, administrative control), or is a substantial portion internalized in Palestinian political consciousness (acceptance of displacement as irreversible, diaspora identity formation)?',
    'Post-exit analysis: if a Palestinian state were established with full sovereignty and refugee return permitted, would measured resistance and political participation indicate structural suppression has been overcome, or does Palestinian identity remain shaped by internalized suppression even after structural barriers are removed? Longitudinal study of diaspora communities post-return.',
    'If internalized: the effective suppression is higher than the structural measure (Palestinians carry the suppression-effects with them even after structural constraints are removed), and the constraint''s psychological penetration is deeper — it has colonized Palestinian self-conception, not just their territory. If purely structural: suppression can be lifted by removing the enforcement apparatus; the psychological effects are remediable. This affects remediation strategy and the timeline for dissipation of the constraint''s effects.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization_in_palestinian_identity, empirical, 'Structural vs. internalized suppression in Palestinian populations').

omega_variable(
    alternative_territorial_solutions_foreclosure,
    'Were alternative solutions to European Jewish persecution (diaspora integration, relocation to other territories, international refugee frameworks) foreclosed by structure or by choice, and does that distinction affect the snare classification?',
    'Historical counterfactual analysis: what were the actual options available to European Jewish leadership in the late 19th century? Were they materially impossible or politically rejected? Did alternatives exist but require acceptance of non-territorial solutions?',
    'If alternatives were materially foreclosed (Europe made diaspora impossible), the snare reading is strengthened — one population was forced to displace another. If alternatives existed but were rejected (non-territorial assimilation was available but territorial sovereignty was chosen), the reading shifts: the snare becomes more volitional, less desperate. This affects the narrative of victimhood-into-perpetration and the moral accounting of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_territorial_solutions_foreclosure, empirical, 'Whether territorial solution was necessary or chosen among available alternatives').

omega_variable(
    kernel_reading_coexistence_fragility,
    'Can the settler-colonial reading and the national-liberation reading coexist indefinitely as competing narratives, or does political reality eventually force a winner-take-all outcome?',
    'Track institutional adoption of readings: as international consensus, Palestinian organizational commitment, Israeli public opinion, and policy frameworks evolve, does one reading increasingly dominate? Does coexistence require ongoing violence/contestation, or can it stabilize into acknowledged pluralism?',
    'If coexistence is stable: this constraint remains contested indefinitely, and the engine''s framework for handling irreducible kernel disputes is validated. If one reading eventually dominates: the loser-reading''s constraint becomes classified as false-summit (a reading that lost institutional support) or piton (a reading that persists by inertia despite lack of real support). The trajectory of the kernel itself is a data point for understanding how contested legitimacy actually resolves in the world.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_coexistence_fragility, preference, 'Whether kernel readings coexist indefinitely or eventually resolve to single-winner dominance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__settler_colonial_reading, 1882, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zion_tr_t1882, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1882, 0.15).
narrative_ontology:measurement(zion_tr_t1920, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1920, 0.25).
narrative_ontology:measurement(zion_tr_t1948, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1948, 0.42).
narrative_ontology:measurement(zion_tr_t1967, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1967, 0.48).
narrative_ontology:measurement(zion_tr_t2000, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 2000, 0.49).
narrative_ontology:measurement(zion_tr_t2024, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 2024, 0.48).

% Extraction over time
narrative_ontology:measurement(zion_be_t1882, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1882, 0.35).
narrative_ontology:measurement(zion_be_t1920, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1920, 0.52).
narrative_ontology:measurement(zion_be_t1948, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1948, 0.78).
narrative_ontology:measurement(zion_be_t1967, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1967, 0.81).
narrative_ontology:measurement(zion_be_t2000, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 2000, 0.82).
narrative_ontology:measurement(zion_be_t2024, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 2024, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(zion_su_t1882, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1882, 0.32).
narrative_ontology:measurement(zion_su_t1920, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1920, 0.55).
narrative_ontology:measurement(zion_su_t1948, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1948, 0.78).
narrative_ontology:measurement(zion_su_t1967, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1967, 0.79).
narrative_ontology:measurement(zion_su_t2000, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 2000, 0.79).
narrative_ontology:measurement(zion_su_t2024, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 2024, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__settler_colonial_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(zionist_legitimacy_basis__settler_colonial_reading, 0.18).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, zionist_legitimacy_basis__national_liberation_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, zionist_legitimacy_basis__religious_restoration_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, palestinian_statehood_foreclosure).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, occupation_settlement_expansion_regime).

% DUAL FORMULATION NOTE:
% This constraint is part of a three-reading kernel family (zionist_legitimacy_basis). The settler_colonial_reading reads Zionism through the analytical lens of structural colonialism: European in origin, territorial in method, displacement as constitutive. The national_liberation_reading reads Zionism as indigenous Jewish recovery from diaspora and persecution. The religious_restoration_reading reads Zionism as messianic fulfillment post-1967. These are not variations of a single constraint — they have different ε values (this reading: 0.82 extraction; liberation reading: 0.32 extraction; restoration reading: 0.28 extraction), different victim/beneficiary structures, different founding-problem narratives, and different mandatrophy profiles. They share a kernel (the historical fact of Zionism and state establishment) but read it through incommensurable frameworks. Sibling stories should be authored as separate JSON files. This reading influences (but does not foreclose) the other readings by establishing the displacement as constitutive and intentional, which creates structural pressure on liberation and restoration narratives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(zionist_legitimacy_basis__settler_colonial_reading, powerless, 0.94).
constraint_indexing:directionality_override(zionist_legitimacy_basis__settler_colonial_reading, organized, 0.08).
constraint_indexing:directionality_override(zionist_legitimacy_basis__settler_colonial_reading, institutional, 0.06).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
