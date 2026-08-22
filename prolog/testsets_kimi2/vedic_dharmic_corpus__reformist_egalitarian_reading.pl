% ============================================================================
% CONSTRAINT STORY: vedic_dharmic_corpus__reformist_egalitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_dharmic_corpus__reformist_egalitarian_reading, []).

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
 *   constraint_id: vedic_dharmic_corpus__reformist_egalitarian_reading
 *   human_readable: Reformist Egalitarian Reading of Vedic Dharmic Corpus
 *   domain: religious_authority/social_stratification/interpretive_legitimacy
 *
 * SUMMARY:
 *   This constraint instantiates the reformist_egalitarian_reading of the
 *   vedic_dharmic_corpus kernel. It treats caste hierarchy as historical
 *   accretion rather than scriptural essence, mandates that textual meaning
 *   conform to constitutional equality principles, and elevates rational
 *   critique over traditional authority. Institutionalized through Indian
 *   constitutional law, anti-discrimination statutes, and court-mandated
 *   temple-entry rights, the constraint coordinates an egalitarian civic
 *   order while actively displacing hereditary interpretive authority.
 *   Sibling readings include hereditary_monopoly_reading (birth-based
 *   Brahminical authority) and bhakti_devotional_reading (devotional bypass
 *   of caste). The epsilon-invariance principle requires separate stories
 *   because the three readings instantiate structurally distinct constraints
 *   with different beneficiary structures, epsilon values, and authority
 *   groundings.
 *
 * KEY AGENTS:
 *   - Dalit movements: Primary beneficiary (organized/constrained) â gain constitutional protections and civic recognition
 *   - Constitutional apparatus: Agenda-setter (institutional/constrained) â enforces equality principles through state power
 *   - Orthodox Brahminical institutions: Primary payer (institutional/constrained) â lose hereditary interpretive monopoly and state privilege
 *   - Hereditary priesthoods: Secondary payer (moderate/constrained) â face delegitimation of birth-based ritual authority
 *   - Traditional communities: Diffuse payer (organized/constrained) â experience erosion of communal autonomy
 *   - Reformist academics: Beneficiary (moderate/mobile) â gain authority from the shift to historical-critical and constitutional interpretive methods
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.45).
domain_priors:suppression_score(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.75).
domain_priors:theater_ratio(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__reformist_egalitarian_reading, tangled_rope).
narrative_ontology:human_readable(vedic_dharmic_corpus__reformist_egalitarian_reading, "Reformist Egalitarian Reading of Vedic Dharmic Corpus").
narrative_ontology:topic_domain(vedic_dharmic_corpus__reformist_egalitarian_reading, "religious_authority/social_stratification/interpretive_legitimacy").

domain_priors:requires_active_enforcement(vedic_dharmic_corpus__reformist_egalitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__reformist_egalitarian_reading, '5d012459-6010-4efb-8c8b-67809aa83550').
narrative_ontology:cs_kernel_codification('5d012459-6010-4efb-8c8b-67809aa83550', fixed_text).
narrative_ontology:cs_authority_grounding('5d012459-6010-4efb-8c8b-67809aa83550', expertise).
narrative_ontology:cs_interpretation_layer_present('5d012459-6010-4efb-8c8b-67809aa83550').
narrative_ontology:cs_reading_relation('5d012459-6010-4efb-8c8b-67809aa83550', vedic_dharmic_corpus__hereditary_monopoly_reading, forecloses).
narrative_ontology:cs_reading_relation('5d012459-6010-4efb-8c8b-67809aa83550', vedic_dharmic_corpus__bhakti_devotional_reading, coexists_with).
narrative_ontology:cs_axiom('5d012459-6010-4efb-8c8b-67809aa83550', foundational, constitutional_equality_supersedes_scriptural_authority).
narrative_ontology:cs_axiom_status(constitutional_equality_supersedes_scriptural_authority, holdable).
narrative_ontology:cs_axiom_grounding('5d012459-6010-4efb-8c8b-67809aa83550', constitutional_equality_supersedes_scriptural_authority, conventional).
narrative_ontology:cs_axiom('5d012459-6010-4efb-8c8b-67809aa83550', foundational, caste_hierarchy_is_historical_accretion).
narrative_ontology:cs_axiom_status(caste_hierarchy_is_historical_accretion, holdable).
narrative_ontology:cs_axiom_grounding('5d012459-6010-4efb-8c8b-67809aa83550', caste_hierarchy_is_historical_accretion, empirically_contingent).
narrative_ontology:cs_reference_frame('5d012459-6010-4efb-8c8b-67809aa83550', constitutional_egalitarian_framework).
narrative_ontology:cs_drift_state('5d012459-6010-4efb-8c8b-67809aa83550', contemporary_hindutva_resurgence, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('5d012459-6010-4efb-8c8b-67809aa83550', '').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__reformist_egalitarian_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, dalit_movements).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, constitutional_apparatus).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, reformist_academics).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__reformist_egalitarian_reading, orthodox_brahminical_institutions).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__reformist_egalitarian_reading, hereditary_priesthoods).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__reformist_egalitarian_reading, traditional_communities).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__reformist_egalitarian_reading, constitutional_equality_doctrine).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__reformist_egalitarian_reading, historical_critical_method).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive constitutional protections, anti-discrimination enforcement, and symbolic recognition of equal civic status. Depend on state legal apparatus to enforce protections against caste violence and discrimination. Exit is constrained because abandoning the constitutional frame means returning to scriptural status hierarchies with no external protection.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, dalit_movements, beneficiary,
    organized, biographical, constrained, national).

% Courts, legislatures, and administrative bodies that interpret and enforce constitutional equality principles against caste discrimination. Derives legitimacy from the constitutional founding and rational-legal authority. Bears the cost of ongoing legitimation struggles with orthodox institutions. Exit is constrained by the constitutional basic structure doctrine and the political consensus that has formed around equality norms.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, constitutional_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Historians, legal scholars, and social scientists who provide the intellectual framework for the historical-accretion thesis and constitutional supremacy. Gain authority and institutional positions from the shift away from traditional interpretive monopoly. Exit is mobile in principle, though professional identity and funding structures are tied to the reformist paradigm.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, reformist_academics, beneficiary,
    moderate, biographical, mobile, national).

% Temple administrations, religious endowments, and caste associations that historically controlled scriptural interpretation and ritual access. Lose state recognition for hereditary privilege and face legal restrictions on caste-discriminatory practices. Exit is constrained: they can maintain private religious practice but cannot enforce caste boundaries in public civic spheres without legal penalty.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, orthodox_brahminical_institutions, payer,
    institutional, generational, constrained, national).

% Lineage-based ritual specialists whose authority derives from birth. Face legal and social delegitimation as the state recognizes non-Brahmin and Dalit priests in some civic and temple contexts. Exit is constrained because ritual expertise is not transferable to secular credentialing systems and their social standing remains tied to caste identity.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, hereditary_priesthoods, payer,
    moderate, biographical, constrained, regional).

% Caste-based communities that organized social and economic life around jati hierarchies. Experience the constraint as erosion of communal autonomy and traditional dispute-resolution authority. Exit is constrained because their social identity and local standing remain embedded in hierarchical networks even as public law mandates equality.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, traditional_communities, payer,
    organized, biographical, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a uniform, state-enforced egalitarian civic order by subordinating scriptural interpretation to constitutional equality norms, enabling cross-caste civic participation and providing a centralized legal mechanism to adjudicate discrimination claims.
% TRANSFER_FUNCTION: Moves interpretive authority and public ritual legitimacy from hereditary religious institutions to constitutional bodies, courts, and historically marginalized groups; transfers social status from jati-based hierarchy to formally equal citizenship.
% ABSENT_VOICES: Orthodox scriptural authorities and conservative traditionalist scholars are partially excluded from constitutional interpretive processes; their readings are delegitimized in state discourse though maintained in private religious practice. Transnational Hindutva movements that reject both orthodox hierarchy and secular constitutionalism are also absent from the reformist frame.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight, caste-based discriminatory practices would lose their primary constitutional and legal prohibition, hereditary authority would reclaim public legitimacy, and the Indian civic order would reorganize around competing communal and scriptural claims rather than uniform equality principles.
% FOUNDING_PROBLEM: Caste-based social hierarchy producing systemic exclusion, untouchability, and violence against Dalits and lower-caste groups, with scriptural authority cited to legitimize inequality.
% FOUNDING_PROBLEM_CORROBORATION: Dalit movements and constitutional historians attest the problem is live; orthodox institutions contest whether the problem was caused by scriptural doctrine or historical distortion; independent sociological studies from outside both camps document persistent caste inequality.
narrative_ontology:disappearance_verdict(vedic_dharmic_corpus__reformist_egalitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_dharmic_corpus__reformist_egalitarian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_dharmic_corpus__reformist_egalitarian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vedic_dharmic_corpus__reformist_egalitarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.45, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_dharmic_corpus__reformist_egalitarian_reading_tests).
:- end_tests(vedic_dharmic_corpus__reformist_egalitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because the constraint delivers genuine coordination benefits â reduced caste violence, uniform civic rights, and centralized adjudication â while simultaneously extracting authority and status from orthodox institutions. Suppression is high (0.75) because the constraint's persistence depends on active legal enforcement against substantial orthodox resistance, including litigation, ritual defiance, and political backlash. Theater ratio is moderate-high (0.45): formal compliance (reservations, anti-discrimination filings) has grown faster than substantive social transformation, producing performative equality that obscures persistent hierarchy. Accessibility collapse is moderate (0.50): orthodox alternatives remain live in private religious spheres despite public-law collapse. Resistance is high (0.70): orthodox institutions mount sustained legal and social resistance. Measurements share a single time grid to prevent misaligned drift dating.
 *
 * PERSPECTIVAL GAP:
 *   From the Dalit-movement seat, the constraint is necessary protective coordination without which caste violence would escalate. From the orthodox-institution seat, the same structure is expropriation of a legitimate interpretive and social order by secular state power. The constitutional apparatus experiences it as the enforcement of founding principles; the engine computes these divergences from the structural asymmetry in power and exit, not from authored claims.
 *
 * DIRECTIONALITY LOGIC:
 *   Dalit movements and reformist academics sit near the beneficiary end: they collect recognition, protection, and authority from the constraint's operation. The constitutional apparatus sits ambiguously â it is the agenda-setter and a beneficiary of expanded jurisdiction, but also bears the ongoing costs of legitimation struggles; its d is structurally low but not at the floor because enforcement consumes institutional capacity. Orthodox institutions, hereditary priesthoods, and traditional communities are the targets: the constraint extracts hereditary authority, communal autonomy, and ritual privilege from them. Their high d is structurally derived from victim declarations plus constrained/identity-locked exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the constraint as either a pure rope (which would ignore the extraction from orthodox institutions and the active enforcement required) or a pure snare (which would deny the genuine coordination function of anti-discrimination law and egalitarian civic order). The mandate â constitutional equality â remains live in the sense that caste inequality persists, but the constraint's operation has accumulated theatrical and bureaucratic overhead that partially decouples it from substantive transformation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scriptural_essence_vs_historical_accretion,
    'Is caste hierarchy a historical accretion onto dharmic texts, or is it essential to scriptural meaning?',
    'Philological and historical-linguistic analysis of earliest strata of Vedic and Dharmashastra texts, cross-referenced with archaeological and epigraphic evidence of social organization.',
    'If scripturally essential, the reformist reading commits a fundamental misreading and its constitutional enforcement rests on false premises; if purely historical, orthodox resistance is defending a later interpolation as divine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scriptural_essence_vs_historical_accretion, empirical, 'Whether caste hierarchy is intrinsic or extrinsic to the textual kernel').

omega_variable(
    state_enforcement_as_coordination_or_capture,
    'Does state entanglement with the reformist reading represent legitimate coordination toward egalitarian order, or asymmetric extraction by secular-legal institutions displacing religious authority?',
    'Comparative analysis of jurisdictions with and without constitutional caste equality mandates, measuring outcomes for both marginalized groups and traditional institutions.',
    'If the state apparatus is the primary beneficiary of expanded jurisdiction, the constraint skews toward snare; if the coordination benefit is broadly distributed and authority transfer is incidental, it remains tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_enforcement_as_coordination_or_capture, conceptual, 'Ambiguity about whether state enforcement is coordination or capture').

omega_variable(
    kernel_committer_sibling_structure,
    'This constraint is one reading of the vedic_dharmic_corpus kernel alongside hereditary_monopoly_reading and bhakti_devotional_reading. How would structural reclassification change if the historical-accretion axiom were substantially empirically challenged?',
    'Axiomatic foreclosure analysis: if the empirically_contingent axiom caste_hierarchy_is_historical_accretion is refuted, the engine would compute axiom_override and potentially shift the forecloses relation to hereditary_monopoly_reading toward coexists_with or influences.',
    'Would alter the kernel''s commitment-system topology and potentially reclassify this reading''s relationship to orthodox authority from logical foreclosure to strategic competition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_committer_sibling_structure, conceptual, 'Committer frame omega documenting sibling-reading structural dependencies').

omega_variable(
    orthodox_resistance_trajectory,
    'Is orthodox resistance to this constraint intensifying structurally, or is it a fading internalized pattern being replaced by strategic accommodation?',
    'Longitudinal analysis of litigation rates, public ritual practice, and caste-endogamy statistics over the measurement interval.',
    'If structural, resistance metric will remain high and suppression requirement must rise; if internalized fading, the constraint may be normalizing and resistance metric should decline.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(orthodox_resistance_trajectory, empirical, 'Whether resistance is structural or normalized acquiescence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__reformist_egalitarian_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vdcre_tr_t0, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(vdcre_tr_t10, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement(vdcre_tr_t20, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(vdcre_tr_t30, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 30, 0.32).
narrative_ontology:measurement(vdcre_tr_t40, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 40, 0.36).
narrative_ontology:measurement(vdcre_tr_t50, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 50, 0.4).
narrative_ontology:measurement(vdcre_tr_t60, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 60, 0.43).
narrative_ontology:measurement(vdcre_tr_t70, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 70, 0.45).

% Extraction over time
narrative_ontology:measurement(vdcre_be_t0, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(vdcre_be_t10, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 10, 0.33).
narrative_ontology:measurement(vdcre_be_t20, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 20, 0.36).
narrative_ontology:measurement(vdcre_be_t30, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 30, 0.39).
narrative_ontology:measurement(vdcre_be_t40, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 40, 0.42).
narrative_ontology:measurement(vdcre_be_t50, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 50, 0.44).
narrative_ontology:measurement(vdcre_be_t60, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 60, 0.45).
narrative_ontology:measurement(vdcre_be_t70, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 70, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(vdcre_su_t0, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(vdcre_su_t10, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(vdcre_su_t20, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(vdcre_su_t30, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 30, 0.64).
narrative_ontology:measurement(vdcre_su_t40, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement(vdcre_su_t50, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 50, 0.71).
narrative_ontology:measurement(vdcre_su_t60, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 60, 0.73).
narrative_ontology:measurement(vdcre_su_t70, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 70, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_dharmic_corpus__reformist_egalitarian_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.08).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__reformist_egalitarian_reading, hereditary_monopoly_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__reformist_egalitarian_reading, bhakti_devotional_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the vedic_dharmic_corpus kernel, decomposed per the epsilon-invariance principle because the hereditary, devotional, and reformist readings instantiate structurally distinct constraints with different epsilon values, beneficiary structures, and authority groundings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
