% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__hegemonic_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_practice_norm_complex__hegemonic_extraction_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: rbio_practice_norm_complex__hegemonic_extraction_reading
 *   human_readable: Rules-Based International Order Practice Norms — Hegemonic Extraction Reading
 *   domain: international_relations/international_law/political_economy
 *
 * SUMMARY:
 *   The Rules-Based International Order (RBIO) practice norm complex presents
 *   itself as a universal, consent-based framework for international
 *   cooperation and conflict resolution. This reading — the
 *   hegemonic_extraction_reading — argues that RBIO norms function as a
 *   frozen hegemonic project: formally revisable through UN Charter
 *   mechanisms but practically un-amendable due to P5 veto power and deep
 *   institutional path-dependency. Enforcement selectivity (humanitarian
 *   intervention invoked against non-aligned states but not P5 allies;
 *   structural adjustment conditionalities that protect western creditor
 *   interests; sanctions regimes that target adversaries while exempting
 *   allies) reveals extractive intent. The constraint coordinates some
 *   genuine collective-action problems (nuclear non-proliferation, trade
 *   dispute settlement) while extracting asymmetric benefits for U.S. and
 *   European capital through the same structures. Beneficiaries: western
 *   financial institutions, P5 hegemonic actors, transnational capital.
 *   Victims: Global South states and populations subjected to structural
 *   adjustment, sanctioned economies, and intervention conditionality.
 *
 * KEY AGENTS:
 *   - us_european_capital: Primary beneficiary (institutional/arbitrage) — extracts through conditionalities, investment protections, and enforcement selectivity
 *   - western_financial_institutions: Primary beneficiary (institutional/arbitrage) — IMF, World Bank, BIS administer extraction mechanisms
 *   - p5_hegemonic_actors: Agenda setter + beneficiary (institutional/analytical) — control revision pathways via veto, shape enforcement
 *   - global_south_states: Primary victim (organized/constrained) — bear conditionalities, sanctions, intervention; limited exit
 *   - global_south_populations: Primary victim (powerless/trapped) — absorb austerity, displacement, service collapse from adjustment
 *   - structural_adjustment_subjects: Victim (powerless/trapped) — directly subjected to coerced contract terms
 *   - non_aligned_middle_powers: Secondary actor (powerful/constrained) — navigate selectively, sometimes benefit sometimes targeted
 *   - liberal_institutional_scholars: Observer (analytical/analytical) — contest the extraction reading, emphasize coordination function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.78).
domain_priors:suppression_score(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.82).
domain_priors:theater_ratio(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__hegemonic_extraction_reading, tangled_rope).
narrative_ontology:human_readable(rbio_practice_norm_complex__hegemonic_extraction_reading, "Rules-Based International Order Practice Norms — Hegemonic Extraction Reading").
narrative_ontology:topic_domain(rbio_practice_norm_complex__hegemonic_extraction_reading, "international_relations/international_law/political_economy").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__hegemonic_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__hegemonic_extraction_reading, 'ddcdf975-cb8b-45b3-a2a9-435416c22760').
narrative_ontology:cs_kernel_codification('ddcdf975-cb8b-45b3-a2a9-435416c22760', formalized).
narrative_ontology:cs_authority_grounding('ddcdf975-cb8b-45b3-a2a9-435416c22760', extraction).
narrative_ontology:cs_interpretation_layer_present('ddcdf975-cb8b-45b3-a2a9-435416c22760').
narrative_ontology:cs_reading_relation('ddcdf975-cb8b-45b3-a2a9-435416c22760', rbio_practice_norm_complex__liberal_institutional_reading, influences).
narrative_ontology:cs_reading_relation('ddcdf975-cb8b-45b3-a2a9-435416c22760', rbio_practice_norm_complex__sovereignty_maximalist_reading, coexists_with).
narrative_ontology:cs_axiom('ddcdf975-cb8b-45b3-a2a9-435416c22760', foundational, enforcement_selectivity_reveals_hegemonic_intent).
narrative_ontology:cs_axiom_status(enforcement_selectivity_reveals_hegemonic_intent, holdable).
narrative_ontology:cs_axiom_grounding('ddcdf975-cb8b-45b3-a2a9-435416c22760', enforcement_selectivity_reveals_hegemonic_intent, empirically_contingent).
narrative_ontology:cs_axiom('ddcdf975-cb8b-45b3-a2a9-435416c22760', foundational, conditionality_is_coerced_contract).
narrative_ontology:cs_axiom_status(conditionality_is_coerced_contract, holdable).
narrative_ontology:cs_axiom_grounding('ddcdf975-cb8b-45b3-a2a9-435416c22760', conditionality_is_coerced_contract, deontological).
narrative_ontology:cs_axiom('ddcdf975-cb8b-45b3-a2a9-435416c22760', secondary, p5_veto_freezes_kernel_to_preserve_extraction).
narrative_ontology:cs_axiom_status(p5_veto_freezes_kernel_to_preserve_extraction, holdable).
narrative_ontology:cs_axiom_grounding('ddcdf975-cb8b-45b3-a2a9-435416c22760', p5_veto_freezes_kernel_to_preserve_extraction, conventional).
narrative_ontology:cs_reference_frame('ddcdf975-cb8b-45b3-a2a9-435416c22760', post_war_liberal_international_order).
narrative_ontology:cs_drift_state('ddcdf975-cb8b-45b3-a2a9-435416c22760', contemporary_multipolar_contestation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ddcdf975-cb8b-45b3-a2a9-435416c22760', '').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__hegemonic_extraction_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__hegemonic_extraction_reading, us_european_capital).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__hegemonic_extraction_reading, western_financial_institutions).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__hegemonic_extraction_reading, p5_hegemonic_actors).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__hegemonic_extraction_reading, global_south_states).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__hegemonic_extraction_reading, global_south_populations).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__hegemonic_extraction_reading, structural_adjustment_subjects).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__hegemonic_extraction_reading, non_aligned_middle_powers).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__hegemonic_extraction_reading, non_aligned_middle_powers).
narrative_ontology:constraint_vindicates(rbio_practice_norm_complex__hegemonic_extraction_reading, hegemonic_stability_theory).
narrative_ontology:constraint_vindicates(rbio_practice_norm_complex__hegemonic_extraction_reading, neoliberal_governance_legitimacy).
narrative_ontology:constraint_vindicates(rbio_practice_norm_complex__hegemonic_extraction_reading, intervention_conditionality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Financial and industrial capital based in the U.S. and Europe that benefits from investment protections, intellectual property enforcement, market access guarantees, and structural adjustment conditionalities that prioritize creditor repayment over domestic welfare. They shape the rules through IFI governance, trade negotiations, and regulatory capture. Exit is arbitrage-grade: they can move capital, restructure supply chains, and influence policy across jurisdictions.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, us_european_capital, beneficiary,
    institutional, generational, arbitrage, global).

% IMF, World Bank, BIS, and regional development banks that administer the RBIO's economic governance layer. They set conditionalities, design structural adjustment programs, and enforce creditor-friendly frameworks. They collect rents through lending margins, advisory fees, and institutional prestige. They are the primary administrators of the constraint's extraction machinery. Exit is analytical: they study the system but do not leave it — their institutional identity is constituted by it.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, western_financial_institutions, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__hegemonic_extraction_reading, western_financial_institutions, beneficiary).

% The five permanent UN Security Council members (U.S., UK, France, Russia, China) who hold veto power over the constraint's formal revision pathway. They shape enforcement selectivity: authorizing intervention against non-aligned states while shielding allies. They benefit from the kernel's stability — it legitimizes their great-power prerogatives. Exit is analytical: they are the kernel's authors and guardians; leaving would dissolve their privileged position.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, p5_hegemonic_actors, agenda_setter,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__hegemonic_extraction_reading, p5_hegemonic_actors, beneficiary).

% Post-colonial and developing states that bear the costs of RBIO enforcement: structural adjustment conditionalities, sanctions regimes, intervention conditionality, and intellectual property regimes that raise medicine/technology costs. They participate in UN forums and regional blocs (G77, AU, ASEAN) but cannot override P5 veto or IFI governance structures. Exit is constrained: they can build alternative institutions (BRICS, NDB, CRA) but remain embedded in the dollar system and western-led financial architecture.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, global_south_states, payer,
    organized, biographical, constrained, global).

% Populations in Global South countries who absorb the human costs of structural adjustment: austerity-driven service collapse, user fees for health/education, labor flexibilization, displacement from land grabs enabled by investment treaties. They have no voice in IFI boardrooms or UNSC chambers. Exit is trapped: migration is restricted, and the constraint operates through their own governments as transmission belts.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, global_south_populations, payer,
    powerless, biographical, trapped, global).

% Communities and workers directly subjected to IMF/World Bank conditionalities: public sector layoffs, subsidy removal, privatization of utilities, deregulation of labor markets. The conditionalities are negotiated between IFIs and governments — these subjects are not at the table. Exit is trapped: they bear the immediate costs with no institutional avenue for refusal.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, structural_adjustment_subjects, payer,
    powerless, immediate, trapped, local).

% States like India, Brazil, South Africa, Turkey, Indonesia that navigate the RBIO selectively: they benefit from trade dispute settlement, non-proliferation norms, and development finance while resisting conditionalities and enforcement selectivity. They lead alternative institution-building (BRICS, NDB) but remain integrated in the western-led system. Exit is constrained: they can hedge but not fully decouple.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, non_aligned_middle_powers, beneficiary,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__hegemonic_extraction_reading, non_aligned_middle_powers, payer).

% Academics, policymakers, and NGO analysts who defend the RBIO as a genuine coordination achievement. They emphasize the constraint's problem-solving capacity (trade, non-proliferation, climate) and attribute enforcement gaps to capacity limits, not design. They do not collect rents from the constraint but their professional status depends on its legitimacy.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, liberal_institutional_scholars, observer,
    analytical, generational, analytical, global).

% States and movements (Russia post-2014, China on Taiwan/Xinjiang, U.S. unilateralists, Global South anti-imperialists) who reject RBIO legitimacy except when it protects sovereignty. They are excluded from the constraint's interpretive community because they deny its universalist premise. Exit is mobile: they can and do withdraw from specific treaties (ICC, INF, Paris) while remaining in others.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, sovereignty_maximalist_actors, excluded,
    powerful, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universal framework for great-power conflict avoidance (UNSC), trade dispute settlement (WTO), nuclear non-proliferation (NPT/IAEA), and development finance (IFIs) — solving genuine collective-action problems that would otherwise produce more chaotic and violent outcomes.
% TRANSFER_FUNCTION: Moves policy autonomy, fiscal resources, natural resource control, and regulatory space from Global South states and populations to western capital and IFIs through structural adjustment conditionalities, investment treaty arbitration, intellectual property enforcement, and selective sanctions. Moves legitimacy and enforcement authority to P5 actors via veto and Security Council resolutions.
% ABSENT_VOICES: Colonized and indigenous peoples whose sovereignty was never recognized in the kernel's founding; populations in sanctioned countries who have no representation in UNSC or IFI governance; future generations bearing climate costs of the extraction-enabled development model. They are structurally excluded from the rooms where RBIO norms are interpreted and enforced.
% DISAPPEARANCE_RATIONALE: If RBIO practice norms vanished overnight, the dollar system would face immediate challenge, IFI conditionalities would lapse, sanctions regimes would lose legal cover, and intervention justifications would collapse. Global South states would reclaim policy space; western capital would lose enforcement mechanisms; alternative institutions (BRICS, regional) would accelerate. The world would rearrange violently in the short term but toward a multipolar equilibrium.
% FOUNDING_PROBLEM: Post-WWII: prevent great-power war, reconstruct European economies, manage decolonization without chaos, create stable monetary and trade systems. The UN Charter, Bretton Woods, and GATT were built to solve these specific problems.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is partially dead (great-power war prevented, European reconstruction complete) and partially live (nuclear proliferation, climate, pandemics, trade). Western governments and IFIs attest it is still live — citing ongoing threats. Global South states and critical scholars (e.g., UNCTAD reports, South Centre analyses, BRICS declarations) attest the original problems are substantially solved and the arrangement persists as extraction. Corroboration from outside the beneficiary set exists: the 2023 Bridgetown Initiative, the 2024 UN Pact for the Future negotiations, and the 2020s wave of IFI reform proposals all come from parties that do not primarily benefit from the current arrangement.
narrative_ontology:disappearance_verdict(rbio_practice_norm_complex__hegemonic_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(rbio_practice_norm_complex__hegemonic_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rbio_practice_norm_complex__hegemonic_extraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(rbio_practice_norm_complex__hegemonic_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__hegemonic_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rbio_practice_norm_complex__hegemonic_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rbio_practice_norm_complex__hegemonic_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extraction (0.78) is high because the constraint transfers resources and policy autonomy from Global South to western capital through conditionalities, investment treaties, and enforcement asymmetries — the coordination function (dispute settlement, non-proliferation) is real but the extraction layer is structural. Suppression (0.82) is very high because the constraint's persistence depends on active enforcement: P5 veto blocks revision, Security Council resolutions authorize selective intervention, IFIs enforce conditionalities, sanctions regimes coerce compliance. Theater ratio (0.65) is elevated because the 'rules-based' framing and formal revisability mechanisms (UN Charter amendment, ICJ advisory opinions) are increasingly performative — the constraint's actual operation diverges from its declared legitimate process. Accessibility collapse (0.45) is moderate: alternatives exist (regional institutions, BRICS frameworks, non-aligned movement) but are systematically marginalized. Resistance (0.72) is high: Global South coalitions (G77, NAM, BRICS) actively contest the arrangement, but coalition power is insufficient to overcome P5 veto and institutional lock-in.
 *
 * PERSPECTIVAL GAP:
 *   From the liberal_institutional seat, the constraint appears as genuine coordination with implementation gaps. From the sovereignty_maximalist seat, it appears as illegitimate intrusion. From the hegemonic_extraction seat (this reading), it appears as a frozen hegemonic project with a coordination cover. The engine computes these per-seat classifications from the structural data; the divergence IS the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (us_european_capital, western_financial_institutions, p5_hegemonic_actors) have institutional power and arbitrage-grade exit — they shape the rules and can opt out of costs. Victims (global_south_states, global_south_populations, structural_adjustment_subjects) are organized to powerless with constrained to trapped exit — they bear costs and cannot easily escape the constraint's reach. P5_hegemonic_actors are both agenda_setter and beneficiary: they administer the constraint (UNSC veto, IFI governance) and collect its rents. Non_aligned_middle_powers sit in a constrained position: powerful enough to resist some extraction but not to rewrite the kernel. The engine will derive directionality from these structural positions — beneficiaries near d=0.1-0.2, victims near d=0.7-0.9, middle powers near d=0.4-0.5.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (post-WWII collective security and economic cooperation) is partially dead (great power war prevented) but partially live (climate, pandemics, proliferation). The arrangement persists not because the founding problem remains fully live, but because the extraction layer benefits powerful actors who control the revision pathway. This is mandatrophy: the coordination function has atrophied relative to the extraction function, but the constraint is maintained through institutional inertia and active enforcement. The 'frozen' character — formally revisable, practically un-amendable — is the signature of mandatrophy in a tangled_rope: the coordination cover story prevents reclassification as snare, while extraction continues.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Is the hegemonic_extraction_reading a structurally distinct constraint from the liberal_institutional_reading, or are they competing interpretations of the same constraint?',
    'Test ε-invariance: if changing the observable (what counts as ''RBIO enforcement'') changes ε by a wide margin, they are distinct constraints. Decompose into separate constraint stories per DP-001.',
    'If distinct, each reading gets its own ε, stakeholders, and classification. If unified, the constraint would need to accommodate variable extraction across measurement bases — violating ε-invariance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether the RBIO kernel decomposes into multiple constraint stories per ε-invariance').

omega_variable(
    enforcement_selectivity_mechanism,
    'Is enforcement selectivity driven by structural capacity constraints (as liberal_institutional_reading claims) or by deliberate political choice to protect hegemonic interests (as this reading claims)?',
    'Compare enforcement patterns against capacity-neutral indicators: when P5 allies violate norms vs. when non-aligned states do. Track veto usage correlation with resource interests.',
    'If deliberate choice, extraction is structural and the constraint is tangled_rope/snare. If capacity-driven, coordination function dominates and the constraint trends toward rope/scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_selectivity_mechanism, empirical, 'Whether enforcement selectivity reveals extractive intent or capacity limits').

omega_variable(
    p5_veto_path_dependency,
    'Does P5 veto structure make formal revision practically impossible, or is the revision pathway genuinely available but politically difficult?',
    'Analyze UNSC veto patterns on RBIO-related resolutions since 1990; measure successful formal amendments vs. attempted; assess whether institutional practice has created de facto irreversibility.',
    'If practically impossible, the constraint''s ''revisable'' character is theater — frozen hegemonic project. If genuinely available, the constraint retains adaptive capacity and may be scaffold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(p5_veto_path_dependency, empirical, 'Whether formal revisability is genuine or performative').

omega_variable(
    conditionality_as_coerced_contract,
    'Are structural adjustment conditionalities genuinely voluntary agreements or coerced contracts imposed through asymmetric bargaining power?',
    'Examine IMF/World Bank program negotiation records: measure policy space of borrowing governments, alternatives available, and correlation between conditionality and capital flow protection for western creditors.',
    'If coerced, the coordination function is cover for extraction — tangled_rope/snare. If voluntary, the arrangement has genuine rope characteristics despite power asymmetry.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conditionality_as_coerced_contract, conceptual, 'Whether conditionality constitutes genuine coordination or extraction via coercion').

omega_variable(
    cs_framing_underdetermination,
    'Does the RBIO kernel instantiate a commitment system with authority grounded in lineage (UN Charter tradition) or extraction (hegemonic benefit from kernel stability)?',
    'Trace whether interpretive authority absorbs drift (lineage) or whether drift denial IS the authority''s source (extraction). Compare revision attempts that threaten hegemonic interests vs. those that don''t.',
    'If extraction-grounded, cs_structure.authority_grounding=extraction and interpretation_layer_present=true. If lineage-grounded, authority_grounding=lineage with interpretation_layer absorbing drift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Whether CS framing is lineage or extraction — alternative framings produce different cs_pattern classifications').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__hegemonic_extraction_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_tr_t0, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(rbio_tr_t0, observed).
narrative_ontology:measurement(rbio_tr_t10, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 10, 0.42).
narrative_ontology:measurement_basis(rbio_tr_t10, observed).
narrative_ontology:measurement(rbio_tr_t20, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 20, 0.51).
narrative_ontology:measurement_basis(rbio_tr_t20, observed).
narrative_ontology:measurement(rbio_tr_t30, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 30, 0.58).
narrative_ontology:measurement_basis(rbio_tr_t30, observed).
narrative_ontology:measurement(rbio_tr_t40, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 40, 0.62).
narrative_ontology:measurement_basis(rbio_tr_t40, observed).
narrative_ontology:measurement(rbio_tr_t50, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 50, 0.65).
narrative_ontology:measurement_basis(rbio_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(rbio_be_t0, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(rbio_be_t0, observed).
narrative_ontology:measurement(rbio_be_t10, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement_basis(rbio_be_t10, observed).
narrative_ontology:measurement(rbio_be_t20, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(rbio_be_t20, observed).
narrative_ontology:measurement(rbio_be_t30, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 30, 0.73).
narrative_ontology:measurement_basis(rbio_be_t30, observed).
narrative_ontology:measurement(rbio_be_t40, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 40, 0.76).
narrative_ontology:measurement_basis(rbio_be_t40, observed).
narrative_ontology:measurement(rbio_be_t50, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 50, 0.78).
narrative_ontology:measurement_basis(rbio_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(rbio_su_t0, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(rbio_su_t0, observed).
narrative_ontology:measurement(rbio_su_t10, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement_basis(rbio_su_t10, observed).
narrative_ontology:measurement(rbio_su_t20, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(rbio_su_t20, observed).
narrative_ontology:measurement(rbio_su_t30, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 30, 0.76).
narrative_ontology:measurement_basis(rbio_su_t30, observed).
narrative_ontology:measurement(rbio_su_t40, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 40, 0.8).
narrative_ontology:measurement_basis(rbio_su_t40, observed).
narrative_ontology:measurement(rbio_su_t50, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 50, 0.82).
narrative_ontology:measurement_basis(rbio_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__hegemonic_extraction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.12).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__hegemonic_extraction_reading, imf_conditionality_regime).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__hegemonic_extraction_reading, world_bank_structural_adjustment).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__hegemonic_extraction_reading, unsc_veto_architecture).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__hegemonic_extraction_reading, investment_treaty_arbitration).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__hegemonic_extraction_reading, sanctions_regime_architecture).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the rbio_practice_norm_complex kernel. The liberal_institutional_reading (ε≈0.25, claimed rope) and sovereignty_maximalist_reading (ε≈0.65, claimed snare) are separate constraint stories with different beneficiary/victim structures and different claimed types. All three are linked via network.affects_constraints forming a constraint family per DP-001 ε-invariance decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rbio_practice_norm_complex__hegemonic_extraction_reading, institutional, 0.15).
constraint_indexing:directionality_override(rbio_practice_norm_complex__hegemonic_extraction_reading, organized, 0.65).
constraint_indexing:directionality_override(rbio_practice_norm_complex__hegemonic_extraction_reading, powerless, 0.85).
constraint_indexing:directionality_override(rbio_practice_norm_complex__hegemonic_extraction_reading, powerful, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
