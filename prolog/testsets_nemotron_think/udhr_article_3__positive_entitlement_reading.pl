% ============================================================================
% CONSTRAINT STORY: udhr_article_3__positive_entitlement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_article_3__positive_entitlement_reading, []).

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
 *   constraint_id: udhr_article_3__positive_entitlement_reading
 *   human_readable: UDHR Article 3 Positive Entitlement Reading
 *   domain: constitutional_law/human_rights/political_philosophy
 *
 * SUMMARY:
 *   This constraint story captures the positive entitlement reading of UDHR
 *   Article 3 ('Everyone has the right to life, liberty and security of
 *   person') — the interpretation that 'security of person' imposes
 *   affirmative state obligations to provide welfare, healthcare, housing,
 *   and protection from hate speech. The reading emerged from the UDHR's
 *   drafting history where 'security' was deliberately left open between
 *   negative (freedom from state violence) and positive (material security)
 *   understandings. Over 1948–2024, this reading expanded from aspirational
 *   declaration to enforceable constitutional mandate in many jurisdictions,
 *   driving the welfare state's legal architecture, hate speech
 *   criminalization, and redistributive taxation. The claimed type is
 *   tangled_rope: genuine coordination of material provision coexists with
 *   asymmetric extraction from property and expression rights. The engine
 *   will compute per-seat classifications from the structural data below.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_article_3__positive_entitlement_reading, 0.75).
domain_priors:suppression_score(udhr_article_3__positive_entitlement_reading, 0.7).
domain_priors:theater_ratio(udhr_article_3__positive_entitlement_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__positive_entitlement_reading, tangled_rope).
narrative_ontology:human_readable(udhr_article_3__positive_entitlement_reading, "UDHR Article 3 Positive Entitlement Reading").
narrative_ontology:topic_domain(udhr_article_3__positive_entitlement_reading, "constitutional_law/human_rights/political_philosophy").

domain_priors:requires_active_enforcement(udhr_article_3__positive_entitlement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__positive_entitlement_reading, '08527cd8-0507-4405-b916-e3c6f74b546d').
narrative_ontology:cs_kernel_codification('08527cd8-0507-4405-b916-e3c6f74b546d', formalized).
narrative_ontology:cs_authority_grounding('08527cd8-0507-4405-b916-e3c6f74b546d', lineage).
narrative_ontology:cs_interpretation_layer_present('08527cd8-0507-4405-b916-e3c6f74b546d').
narrative_ontology:cs_reading_relation('08527cd8-0507-4405-b916-e3c6f74b546d', udhr_article_3__negative_liberty_reading, forecloses).
narrative_ontology:cs_reading_relation('08527cd8-0507-4405-b916-e3c6f74b546d', udhr_article_3__procedural_hybrid_reading, influences).
narrative_ontology:cs_axiom('08527cd8-0507-4405-b916-e3c6f74b546d', foundational, state_positive_obligation_for_material_conditions).
narrative_ontology:cs_axiom_status(state_positive_obligation_for_material_conditions, holdable).
narrative_ontology:cs_axiom_grounding('08527cd8-0507-4405-b916-e3c6f74b546d', state_positive_obligation_for_material_conditions, deontological).
narrative_ontology:cs_axiom('08527cd8-0507-4405-b916-e3c6f74b546d', secondary, hate_speech_restriction_for_vulnerable_protection).
narrative_ontology:cs_axiom_status(hate_speech_restriction_for_vulnerable_protection, holdable).
narrative_ontology:cs_axiom_grounding('08527cd8-0507-4405-b916-e3c6f74b546d', hate_speech_restriction_for_vulnerable_protection, instrumental).
narrative_ontology:cs_axiom('08527cd8-0507-4405-b916-e3c6f74b546d', secondary, substantive_equality_requires_resource_redistribution).
narrative_ontology:cs_axiom_status(substantive_equality_requires_resource_redistribution, holdable).
narrative_ontology:cs_axiom_grounding('08527cd8-0507-4405-b916-e3c6f74b546d', substantive_equality_requires_resource_redistribution, deontological).
narrative_ontology:cs_reference_frame('08527cd8-0507-4405-b916-e3c6f74b546d', postwar_human_rights_settlement).
narrative_ontology:cs_drift_state('08527cd8-0507-4405-b916-e3c6f74b546d', contemporary_neoliberal_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('08527cd8-0507-4405-b916-e3c6f74b546d', '').
narrative_ontology:cs_kernel_id(udhr_article_3__positive_entitlement_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, vulnerable_populations).
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, welfare_recipients).
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, healthcare_dependent_groups).
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, housing_insecure_populations).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, property_holders).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, taxpayers).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, speech_rights_holders).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, market_economy_participants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, state_administrators).
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, market_economy_participants).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, welfare_recipients).
narrative_ontology:constraint_vindicates(udhr_article_3__positive_entitlement_reading, positive_rights_doctrine).
narrative_ontology:constraint_vindicates(udhr_article_3__positive_entitlement_reading, state_welfare_obligation).
narrative_ontology:constraint_vindicates(udhr_article_3__positive_entitlement_reading, material_conditions_for_dignity).
narrative_ontology:constraint_vindicates(udhr_article_3__positive_entitlement_reading, substantive_security_of_person).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Constitutional courts, legislatures, and executive agencies that interpret and implement Article 3 as a positive mandate. They gain expanded institutional authority and budgetary control through welfare administration, hate speech enforcement, and redistributive bureaucracy. Exit means relinquishing the constitutional mandate that legitimizes their expanded role.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, state_administrators, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(udhr_article_3__positive_entitlement_reading, state_administrators, beneficiary).

% Populations dependent on state-provided welfare, healthcare, and housing for survival. They receive material benefits but have no exit from the system — they cannot opt out of state provision without losing access to life-sustaining resources. Their dependence is structural, not voluntary.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, vulnerable_populations, beneficiary,
    powerless, biographical, trapped, national).

% Working-class and middle-class citizens who receive state healthcare, education, and social insurance while funding them through taxation. They benefit from the coordination function but bear extraction through tax burden. Exit is constrained by residency and citizenship ties.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, welfare_recipients, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(udhr_article_3__positive_entitlement_reading, welfare_recipients, payer).

% Owners of capital, land, and productive assets subject to redistributive taxation justified by Article 3's positive obligations. They bear concentrated extraction but have capital mobility as exit — they can relocate assets or restructure holdings, though at significant cost.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, property_holders, payer,
    powerful, biographical, mobile, national).

% Broad taxpayer base funding the welfare state through income, consumption, and wealth taxes. They bear diffuse but aggregate extraction. Exit is constrained by citizenship and residency; tax avoidance is possible but legally risky and socially sanctioned.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, taxpayers, payer,
    organized, biographical, constrained, national).

% Individuals and groups whose expression is restricted by hate speech laws justified as necessary for the 'security of person' of vulnerable groups. They bear extraction of expressive liberty. Exit is constrained by jurisdiction — they cannot easily escape national speech regulations without emigrating.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, speech_rights_holders, payer,
    organized, biographical, constrained, national).

% Entrepreneurs, employers, and workers in market economies who bear regulatory and tax burdens from positive-rights implementation but benefit from a healthier, more stable workforce and consumer base. They have international mobility as exit option.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, market_economy_participants, payer,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(udhr_article_3__positive_entitlement_reading, market_economy_participants, beneficiary).

% Judicial bodies that adjudicate the scope of Article 3 positive obligations. They neither collect nor pay directly but determine the constraint's operational boundaries. Their analytical exit is absolute — they observe from outside the extraction/coordination flow.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% Academic and legal voices arguing that Article 3 imposes only negative obligations. They are structurally excluded from mainstream constitutional discourse that treats positive rights as settled. Their identity is fused to the negative-liberty framework; abandoning it would dissolve their intellectual project.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, classical_liberal_scholars, excluded,
    moderate, generational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state provision of material conditions necessary for life and security — welfare systems, universal healthcare, public housing, and social insurance — solving the collective-action problem of ensuring no person falls below a dignity threshold through centralized resource pooling and distribution.
% TRANSFER_FUNCTION: Moves financial resources from property holders and taxpayers to vulnerable populations via progressive taxation and redistributive expenditure; moves expressive liberty from speakers to protected groups via hate speech restrictions justified as security-of-person guarantees.
% ABSENT_VOICES: Classical liberal and libertarian constitutional scholars, property-rights advocates, and free-speech absolutists who argue that positive obligations exceed the UDHR's original mandate and constitute illegitimate state overreach. They are excluded from the interpretive consensus that treats welfare rights as self-evident Article 3 requirements, marginalized in international human rights bodies and domestic constitutional courts.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight, the constitutional grounding for the welfare state's positive obligations, hate speech criminalization, and redistributive taxation regimes would collapse. States would lose their Article 3 mandate for material provision, forcing either legislative re-justification of welfare systems or their dismantlement. The global human rights framework's substantive equality architecture would lose its foundational article.
% FOUNDING_PROBLEM: Post-WWII recognition that negative liberty alone cannot secure life and security for populations facing starvation, disease, homelessness, and identity-based violence — the UDHR drafters' intent to constitutionalize material survival as a state obligation, not merely protect against state violence.
% FOUNDING_PROBLEM_CORROBORATION: The drafting history (Cassin, Roosevelt, Malik) supports both positive and negative readings — Cassin's 'four pillars' included welfare, but the text's 'security of person' phrasing remains ambiguous. Contemporary corroboration comes from CESCR General Comments and European Court of Human Rights jurisprudence (positive obligations doctrine), but these are beneficiary-aligned institutions. No significant corroboration exists outside the human rights/welfare state coalition; originalist scholars (e.g., Morsink, Glendon) attest the founding problem was deliberately left unresolved.
narrative_ontology:disappearance_verdict(udhr_article_3__positive_entitlement_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_article_3__positive_entitlement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_article_3__positive_entitlement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(udhr_article_3__positive_entitlement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_article_3__positive_entitlement_reading, 0.75, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_article_3__positive_entitlement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(udhr_article_3__positive_entitlement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(udhr_article_3__positive_entitlement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.75) is high because the reading mandates massive resource transfers (welfare states average 30-50% GDP) and speech restrictions. Suppression (0.70) is high because compliance is enforced through tax law, regulatory states, and criminal hate speech codes — alternatives (private charity, unrestricted speech) are legally foreclosed. Theater ratio (0.40) reflects that rights rhetoric increasingly masks bureaucratic self-perpetuation and interest-group capture of welfare administration. Accessibility collapse (0.70) is high because once a constitution adopts this reading, private alternatives to state provision become legally and politically marginal. Resistance (0.50) is moderate — property and speech rights holders resist but operate within the constitutional framework rather than rejecting it.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (state) experiences this as coordination it must perform; the payers (property holders, speakers) experience it as extraction they cannot avoid; the trapped beneficiaries (vulnerable populations) experience it as survival dependency. The engine computes this divergence from power/exit/role data — the claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   State administrators are structural beneficiaries (d ~ 0.15) — they collect institutional authority and budgets from the mandate. Vulnerable populations are beneficiaries (d ~ 0.1) but identity-locked/trapped — they cannot exit the system that sustains them. Welfare recipients are near-symmetric (d ~ 0.5) — they both pay and receive. Property holders and speech rights holders are targets (d ~ 0.85-0.9) — they bear concentrated extraction with limited exit. Taxpayers are moderate targets (d ~ 0.65). Market participants are near-symmetric (d ~ 0.5). Courts are analytical (d = 0.5). Excluded classical liberals are identity-locked targets (d ~ 0.9) — their intellectual framework is foreclosed by the reading's dominance.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (postwar material insecurity) is contested as live vs. solved. In wealthy OECD states, the original problem is largely solved (universal healthcare, housing floors, starvation eliminated), yet the mandate expands — hate speech restrictions, new positive rights (internet access, gender-affirming care), ever-broader 'security' interpretations. This is mandatrophy: the arrangement persists and grows beyond its founding justification. The reading does not declare mandatrophy_resolved because the contest is live — welfare advocates say insecurity persists; critics say the mandate has become a self-justifying extraction machine.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Does ''security of person'' in Article 3 semantically entail positive state obligations for material provision, or only negative obligations against state-inflicted harm?',
    'Comparative analysis of UDHR drafting records (travaux préparatoires), subsequent treaty interpretation (ICESCR Art. 11, 12; ECHR positive obligations doctrine), and original public meaning at adoption.',
    'If positive obligations are not textually grounded, the reading''s ε reflects judicial/legislative accretion rather than kernel meaning — reclassifying the constraint from tangled_rope (coordination+kernel-grounded extraction) toward snare (extraction without kernel coordination function).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the kernel text supports the positive entitlement reading or whether extraction is accreted interpretation.').

omega_variable(
    extraction_attribution,
    'What fraction of modern welfare state extraction (taxation, regulation) is attributable to Article 3 positive obligations versus independent legislative choice?',
    'Counterfactual legal history: trace constitutional jurisprudence citing Article 3 as basis for welfare rights vs. legislation enacted without Article 3 invocation. Compare jurisdictions with/without constitutional positive rights.',
    'If most extraction flows from legislative choice not Article 3 mandate, this reading''s ε is overstated — the constraint story would capture judicial gloss, not the kernel''s operational force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_attribution, empirical, 'Disentangling kernel-mandated extraction from legislative accretion.').

omega_variable(
    speech_restriction_mechanism,
    'Are hate speech restrictions justified by this reading structurally enforced (criminal law, platform regulation) or partially internalized (chilling effect, self-censorship)?',
    'Post-restriction speech trajectory analysis: measure expressive activity before/after hate speech law enactment in comparable jurisdictions; survey speakers on self-censorship motives.',
    'If suppression is substantially internalized, the constraint''s effective suppression exceeds its structural measure — targets carry the constraint with them after legal exit. This would increase χ for speech_rights_holders beyond the engine''s structural derivation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(speech_restriction_mechanism, empirical, 'Structural vs. internalized suppression in hate speech regulation.').

omega_variable(
    coordination_extraction_separability,
    'Can the welfare coordination function (material provision) be separated from the extraction function (redistribution, speech restrictions) institutionally?',
    'Natural experiments: jurisdictions that decouple welfare provision from rights-based mandates (e.g., Singapore''s non-rights-based healthcare/housing); policy models testing universal basic income vs. in-kind welfare.',
    'If separable, the reading''s tangled_rope classification holds — genuine coordination + asymmetric extraction. If inseparable (coordination requires the extraction), the reading approaches rope (extraction as coordination cost). If coordination is illusory, it approaches snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the coordination and extraction components are structurally separable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__positive_entitlement_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_art3_pos_ent_tr_t1948, udhr_article_3__positive_entitlement_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(udhr_art3_pos_ent_tr_t1966, udhr_article_3__positive_entitlement_reading, theater_ratio, 1966, 0.15).
narrative_ontology:measurement(udhr_art3_pos_ent_tr_t1976, udhr_article_3__positive_entitlement_reading, theater_ratio, 1976, 0.2).
narrative_ontology:measurement(udhr_art3_pos_ent_tr_t1989, udhr_article_3__positive_entitlement_reading, theater_ratio, 1989, 0.25).
narrative_ontology:measurement(udhr_art3_pos_ent_tr_t2000, udhr_article_3__positive_entitlement_reading, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(udhr_art3_pos_ent_tr_t2010, udhr_article_3__positive_entitlement_reading, theater_ratio, 2010, 0.35).
narrative_ontology:measurement(udhr_art3_pos_ent_tr_t2024, udhr_article_3__positive_entitlement_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(udhr_art3_pos_ent_be_t1948, udhr_article_3__positive_entitlement_reading, base_extractiveness, 1948, 0.3).
narrative_ontology:measurement(udhr_art3_pos_ent_be_t1966, udhr_article_3__positive_entitlement_reading, base_extractiveness, 1966, 0.4).
narrative_ontology:measurement(udhr_art3_pos_ent_be_t1976, udhr_article_3__positive_entitlement_reading, base_extractiveness, 1976, 0.5).
narrative_ontology:measurement(udhr_art3_pos_ent_be_t1989, udhr_article_3__positive_entitlement_reading, base_extractiveness, 1989, 0.55).
narrative_ontology:measurement(udhr_art3_pos_ent_be_t2000, udhr_article_3__positive_entitlement_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(udhr_art3_pos_ent_be_t2010, udhr_article_3__positive_entitlement_reading, base_extractiveness, 2010, 0.68).
narrative_ontology:measurement(udhr_art3_pos_ent_be_t2024, udhr_article_3__positive_entitlement_reading, base_extractiveness, 2024, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(udhr_art3_pos_ent_su_t1948, udhr_article_3__positive_entitlement_reading, suppression_requirement, 1948, 0.2).
narrative_ontology:measurement(udhr_art3_pos_ent_su_t1966, udhr_article_3__positive_entitlement_reading, suppression_requirement, 1966, 0.3).
narrative_ontology:measurement(udhr_art3_pos_ent_su_t1976, udhr_article_3__positive_entitlement_reading, suppression_requirement, 1976, 0.4).
narrative_ontology:measurement(udhr_art3_pos_ent_su_t1989, udhr_article_3__positive_entitlement_reading, suppression_requirement, 1989, 0.45).
narrative_ontology:measurement(udhr_art3_pos_ent_su_t2000, udhr_article_3__positive_entitlement_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(udhr_art3_pos_ent_su_t2010, udhr_article_3__positive_entitlement_reading, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(udhr_art3_pos_ent_su_t2024, udhr_article_3__positive_entitlement_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__positive_entitlement_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(udhr_article_3__positive_entitlement_reading, 0.15).
narrative_ontology:affects_constraint(udhr_article_3__positive_entitlement_reading, welfare_state_architecture).
narrative_ontology:affects_constraint(udhr_article_3__positive_entitlement_reading, hate_speech_regulation).
narrative_ontology:affects_constraint(udhr_article_3__positive_entitlement_reading, redistributive_taxation).
narrative_ontology:affects_constraint(udhr_article_3__positive_entitlement_reading, positive_rights_jurisprudence).
narrative_ontology:affects_constraint(udhr_article_3__positive_entitlement_reading, social_housing_mandates).

% DUAL FORMULATION NOTE:
% This constraint is one member of the udhr_article_3 constraint family. The negative_liberty_reading (low ε, mountain-like) and procedural_hybrid_reading (moderate ε, rope-like) are sibling constraints. This positive_entitlement_reading has the highest ε because it instantiates the kernel as affirmative state mandate rather than prohibition or procedure. The family decomposition follows the ε-invariance principle: each reading has a stable ε because each measures extraction against a different structural referent (positive mandate vs. negative prohibition vs. procedural guarantee).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(udhr_article_3__positive_entitlement_reading, institutional, 0.15).
constraint_indexing:directionality_override(udhr_article_3__positive_entitlement_reading, powerless, 0.1).
constraint_indexing:directionality_override(udhr_article_3__positive_entitlement_reading, powerful, 0.88).
constraint_indexing:directionality_override(udhr_article_3__positive_entitlement_reading, organized, 0.65).
constraint_indexing:directionality_override(udhr_article_3__positive_entitlement_reading, moderate, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
