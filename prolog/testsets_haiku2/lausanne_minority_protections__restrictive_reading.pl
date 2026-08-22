% ============================================================================
% CONSTRAINT STORY: lausanne_minority_protections__restrictive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lausanne_minority_protections__restrictive_reading, []).

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
 *   constraint_id: lausanne_minority_protections__restrictive_reading
 *   human_readable: Lausanne Restrictive Reading: Individual Worship Only
 *   domain: international_law/religious_governance
 *
 * SUMMARY:
 *   The restrictive reading of Lausanne minority protections interprets the
 *   1923 treaty as guaranteeing only individual worship rights while treating
 *   institutional autonomy, property ownership, and theological education as
 *   matters of domestic Turkish law. Under this reading, minority religious
 *   institutions are not protected treaty entities but ordinary organizations
 *   subject to state registration, curriculum approval, and property
 *   controls. The reading consolidates state control over minority
 *   institutional capacity and forecloses independent clergy formation and
 *   asset management. The constraint is CLAIMED as a snare and the authored
 *   metrics describe extraction and active enforcement that substantiate that
 *   claim — the engine measures the structural fit.
 *
 * KEY AGENTS:
 *   - turkish_state_apparatus: Sets and enforces the restrictive interpretation; controls definitional boundaries; collects institutional oversight authority
 *   - minority_religious_institutions: Bear the constraint's costs through loss of institutional autonomy and property control; trapped exit (cannot leave territory without abandoning historical assets)
 *   - clergy_formation_structures: Face foreclosure of independent theological education; identity-locked (religious vocation bound to institutional continuity)
 *   - minority_property_holders: Subject to state gatekeeping on property transfers and inheritance; constrained exit (selling forfeits community function)
 *   - expansive_reading_advocates: Excluded from definitional authority; no direct structural empowerment within Turkish domestic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lausanne_minority_protections__restrictive_reading, 0.82).
domain_priors:suppression_score(lausanne_minority_protections__restrictive_reading, 0.76).
domain_priors:theater_ratio(lausanne_minority_protections__restrictive_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lausanne_minority_protections__restrictive_reading, snare).
narrative_ontology:human_readable(lausanne_minority_protections__restrictive_reading, "Lausanne Restrictive Reading: Individual Worship Only").
narrative_ontology:topic_domain(lausanne_minority_protections__restrictive_reading, "international_law/religious_governance").

domain_priors:requires_active_enforcement(lausanne_minority_protections__restrictive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lausanne_minority_protections__restrictive_reading, '093c60f6-222e-4fd6-870b-801246f5fc3b').
narrative_ontology:cs_kernel_codification('093c60f6-222e-4fd6-870b-801246f5fc3b', fixed_text).
narrative_ontology:cs_authority_grounding('093c60f6-222e-4fd6-870b-801246f5fc3b', extraction).
narrative_ontology:cs_interpretation_layer_present('093c60f6-222e-4fd6-870b-801246f5fc3b').
narrative_ontology:cs_reading_relation('093c60f6-222e-4fd6-870b-801246f5fc3b', lausanne_minority_protections__expansive_reading, forecloses).
narrative_ontology:cs_reading_relation('093c60f6-222e-4fd6-870b-801246f5fc3b', lausanne_minority_protections__guarantor_reading, influences).
narrative_ontology:cs_axiom('093c60f6-222e-4fd6-870b-801246f5fc3b', foundational, minority_protections_exhausted_by_worship_rights).
narrative_ontology:cs_axiom_status(minority_protections_exhausted_by_worship_rights, holdable).
narrative_ontology:cs_axiom_grounding('093c60f6-222e-4fd6-870b-801246f5fc3b', minority_protections_exhausted_by_worship_rights, conventional).
narrative_ontology:cs_axiom('093c60f6-222e-4fd6-870b-801246f5fc3b', foundational, domestic_legal_supremacy_over_treaty_interpretation).
narrative_ontology:cs_axiom_status(domestic_legal_supremacy_over_treaty_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('093c60f6-222e-4fd6-870b-801246f5fc3b', domestic_legal_supremacy_over_treaty_interpretation, deontological).
narrative_ontology:cs_reference_frame('093c60f6-222e-4fd6-870b-801246f5fc3b', turkish_national_legal_sovereignty).
narrative_ontology:cs_drift_state('093c60f6-222e-4fd6-870b-801246f5fc3b', contemporary_european_human_rights_challenge, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('093c60f6-222e-4fd6-870b-801246f5fc3b', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(lausanne_minority_protections__restrictive_reading, lausanne_minority_protections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, minority_religious_institutions).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, clergy_formation_structures).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, minority_property_holders).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__restrictive_reading, domestic_legal_supremacy).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__restrictive_reading, state_sovereignty_over_religious_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Lausanne protections as restricted to individual worship rights. Applies general Turkish law (property law, education regulation, corporate law) to minority institutions, treating them as domestic administrative matters rather than protected minority assets. Sets enforcement priorities: educational institutions must register under state education law; property transfers require state approval; clerical training is regulated as general adult education. Controls the definitional boundary between 'worship' (protected) and 'institutional administration' (unprotected). Justifies this reading as consistent with national sovereignty and equal application of law to all citizens and organizations.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Churches, monasteries, synods, and collective worship structures that pre-existed the 1923 Lausanne treaty and claim continuity of institutional governance. Face legal requirements to register as domestic associations under state corporation law rather than as protected treaty entities. Cannot maintain independent theological seminaries or clergy training under Lausanne protection; such education must comply with state education ministry curricula and licensing. Property acquired or held prior to 1923 cannot be transferred to successor institutions without state permission. Trapped: these institutions cannot exit Turkish territory without abandoning their historical properties and communities; cannot internationally litigate their treaty status without state permission.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, minority_religious_institutions, payer,
    organized, generational, trapped, national).

% Theological schools, seminaries, and ordination councils that historically trained clergy independent of state curricula. The restrictive reading denies them Lausanne protection as 'institutional autonomy' matters. They must either dissolve, register as state-regulated educational institutions with state-approved curricula, or operate clandestinely. Clerical candidates face identity-lock: their religious vocation is bound up with the institution's teaching; leaving to train abroad means effectively abandoning the home community's institutional continuity. The constraint forecloses independent clergy formation.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, clergy_formation_structures, payer,
    moderate, biographical, identity_locked, regional).

% Individuals and institutional holders of properties (churches, monasteries, schools, cemeteries, residences) that predate or align with minority religious communities. Under the restrictive reading, such property is treated as ordinary domestic property subject to Turkish property law, inheritance law, and tax law, not as protected minority heritage. Transfer to successor religious institutions requires approval; abandoned properties are subject to state reversion; tax treatment differs from pre-Lausanne expectations. Exit is constrained: selling the property to non-minority buyers forfeits its function; maintaining it under current restrictions requires constant negotiation with state authorities.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, minority_property_holders, payer,
    moderate, biographical, constrained, national).

% Minority religious leaders, international human rights organizations, guarantor state diplomats, and some Turkish legal scholars who argue Lausanne protections cover institutional autonomy, property continuity, and clergy training. They are excluded from the definitional decision-making: the restrictive reading is set by Turkish state law and courts, not by international adjudication or minority consensus. Their objections are noted but not structurally empowered within the domestic legal framework.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, expansive_reading_advocates, excluded,
    organized, generational, constrained, national).

% France, Britain, Italy, Greece, and other states that were signatory to Lausanne or inherit guarantor obligations. They observe the restrictive reading's implementation and could contest it through diplomatic channels, treaty interpretation disputes, or European human rights mechanisms (for EU members). In practice, their role is muted: they have limited leverage over Turkish domestic law interpretation and face competing interests in maintaining bilateral relations.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, guarantor_state_diplomacy, observer,
    institutional, generational, analytical, global).

% European Court of Human Rights, UN Human Rights Committee, and treaty bodies that receive complaints from minority institutions about property confiscation or educational restrictions. They evaluate whether the restrictive reading violates international human rights law (freedom of conscience, right to form associations, property rights). Their rulings create external pressure but do not directly override Turkish domestic law interpretation.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, international_human_rights_bodies, observer,
    institutional, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus).
narrative_ontology:fixing_cost_class(lausanne_minority_protections__restrictive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a uniform legal framework for all organizations (religious and secular) operating in Turkish territory: all institutions must register under domestic law, comply with state curricula for education, and submit property transfers to state oversight. This solves a coordination problem for the state: it eliminates parallel legal tracks and ensures uniform enforcement of state law.
% TRANSFER_FUNCTION: Transfers institutional autonomy, property control rights, and clergy formation authority from minority religious communities to the Turkish state apparatus. Minority institutions must seek state permission for core functions; the state apparatus gains gatekeeping control over minority institutional capacity.
% ABSENT_VOICES: Minority religious communities (especially those geographically isolated or linguistically isolated from Turkish-language legal proceedings) have limited formal standing to contest the restrictive reading within Turkish courts. International advocates for expansive or guarantor readings are excluded from the domestic law-making process. Guarantor states' interpretations are not integrated into Turkish judicial reasoning.
% DISAPPEARANCE_RATIONALE: If the restrictive reading disappeared and were replaced by either the expansive or guarantor reading, minority institutions would regain autonomy to manage property, train clergy, and self-administer governance without state gatekeeping. Educational capacity, institutional succession, and property stewardship would reorganize around minority community norms rather than state law. The state apparatus would lose significant administrative control and revenue collection leverage over institutional assets.
% FOUNDING_PROBLEM: Post-1923 Turkish state consolidation required integrating all religious institutions under state legal authority to prevent rival power centers and ensure uniform national law. The founding problem: how to reconcile Lausanne's minority protection language with state sovereignty and national legal uniformity.
% FOUNDING_PROBLEM_CORROBORATION: Turkish state legal scholars and officials attest the founding problem is still live: minority institutions must be brought under state law to prevent administrative fragmentation. Minority community leaders and international advocates attest the founding problem was addressed sufficiently by individual worship guarantees and that institutional foreclosure goes beyond what state consolidation required. European human rights bodies have found some minority institutional restrictions disproportionate to legitimate state interests.
narrative_ontology:disappearance_verdict(lausanne_minority_protections__restrictive_reading, world_rearranges).
narrative_ontology:founding_problem_status(lausanne_minority_protections__restrictive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lausanne_minority_protections__restrictive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(lausanne_minority_protections__restrictive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lausanne_minority_protections__restrictive_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lausanne_minority_protections__restrictive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(lausanne_minority_protections__restrictive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(lausanne_minority_protections__restrictive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and rising (0.68→0.82 over interval) because the constraint's operation increasingly consolidates state control over minority institutional functions that were historically self-administered. The state apparatus collects gatekeeping authority; minority institutions lose administrative autonomy without formal recognition of the loss as treaty violation. Suppression is substantial (0.76) because the constraint persists through legal definitions and regulatory enforcement, not through participant agreement — minority institutions comply because the alternative is institutional dissolution. Theater rises (0.25→0.41) as the state increasingly frames institutional oversight as 'equal application of law' or 'educational standards' when the underlying function is consolidating control over minority institutional capacity. Accessibility collapse is moderate-high (0.68): once the restrictive reading is established in Turkish law, alternatives (expansive reading, international arbitration, guarantor state intervention) become institutionally hard to access — the reading locks in through judicial precedent and legislative codification. Resistance is substantial (0.59) because minority communities mount persistent objections through minority religious leadership, international advocacy, and human rights complaints, but the resistance is structurally marginalized by the domestic law framework.
 *
 * PERSPECTIVAL GAP:
 *   The state apparatus seat and the minority institutions seat compute radically differently. From the state's position, the restrictive reading is a neutral, universally-applied legal framework ensuring that all organizations comply with national law — coordination, not extraction. From the minority institutions' positions, the same reading operates as enforced foreclosure of institutional capacity they held prior to 1923 and that the treaty explicitly referenced. The engine computes per-seat directionality: the state holds d near 0.0 (beneficiary, collects gatekeeping authority); minority institutions hold d near 1.0 (targets, lose institutional autonomy). This divergence is the structural signal the corpus exists to measure.
 *
 * DIRECTIONALITY LOGIC:
 *   Turkish state apparatus: d ≈ 0.1 (beneficiary). High power (institutional), long time horizon (generational), arbitrage-grade exit (sets the frame), large scope (national). Controls the definitional boundary; collects gatekeeping authority. Benefits from institutional consolidation without bearing extraction costs. Minority religious institutions: d ≈ 0.88 (target). Organized power but institutionally subordinate to state law, generational time horizon, trapped exit (cannot leave territory), national scope. Bear the extraction directly: lose property control, lose clergy formation autonomy, lose institutional self-administration. Clergy formation structures: d ≈ 0.91 (target). Moderate power, biographical time horizon, identity-locked exit (religious vocation fused to institutional continuity), regional scope. Face foreclosure of independent formation. Minority property holders: d ≈ 0.82 (target). Moderate power, biographical horizon, constrained exit (selling forfeits community function), national scope. Subject to state gatekeeping on asset transfers.
 *
 * MANDATROPHY ANALYSIS:
 *   This is not a mandatrophy case. The founding problem (post-1923 state consolidation reconciling national law with minority protections) remains genuinely contested. The restrictive reading argues the problem is live and requires ongoing institutional subordination to state law. The expansive and guarantor readings argue the problem was adequately solved by individual worship protections and that further institutional foreclosure is extractive overreach. The dispute is not about a lost function — it is a live disagreement about what the founding commitment entailed. The constraint persists because the state has the power to enforce its reading; it is not performance or inertia. Classification: snare (high extraction, active enforcement, trapped or identity-locked exit for victim seats).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_text_ambiguity,
    'Does the Lausanne treaty text explicitly protect minority institutional autonomy and property, or is it ambiguous enough to admit the restrictive reading?',
    'Comparative analysis of the treaty text, negotiation history, and contemporaneous interpretations by signatory states versus Turkish legal scholarship. International Court of Justice or arbitral interpretation could resolve textual scope.',
    'If the text clearly protects institutions, the restrictive reading is a state overreach and should be classified as a false-summit mountain mistakenly asserted as settled law. If the text is genuinely ambiguous, the restrictive reading is a legitimate but contested interpretation, and the snare classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_text_ambiguity, empirical, 'Textual scope of Lausanne minority protections').

omega_variable(
    international_enforcement_mechanism,
    'Can guarantor states or international human rights bodies enforce a different reading of Lausanne, or is Turkish domestic interpretation effectively final?',
    'Empirical: monitor whether European Court of Human Rights rulings on minority religious rights have led to Turkish legal changes, or whether Turkish courts have accepted international interpretation of Lausanne.',
    'If international enforcement proves effective, the constraint''s scope is limited (international pressure creates alternatives). If Turkish courts consistently reject international interpretation, the constraint is more deeply entrenched as snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_enforcement_mechanism, empirical, 'Effectiveness of international constraint on Turkish domestic interpretation').

omega_variable(
    institutional_autonomy_necessity,
    'Is state legal consolidation (the founding problem motivating the restrictive reading) structurally necessary, or could it be achieved while preserving minority institutional autonomy in protected domains (worship, property, clergy formation)?',
    'Comparative study of how other democracies reconcile state legal uniformity with minority institutional protections (e.g., EU states'' treatment of minority churches, Israel''s millet system). Test whether the two goals are genuinely in conflict or whether the restrictive reading uses state consolidation as cover for institutional extraction.',
    'If institutional autonomy is compatible with legal consolidation, the restrictive reading is unnecessary and purely extractive — snare confirmed. If they are genuinely in conflict, part of the measured extraction is the cost of legitimate state consolidation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_autonomy_necessity, conceptual, 'Whether state legal consolidation and minority institutional autonomy are structurally incompatible').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.76) structural (legal barriers, enforcement threat) or internalized (minority institutions have internalized the restrictive reading as legitimate state authority)?',
    'Post-remedy empirical: if a European human rights body ruled in favor of institutional autonomy and Turkey complied, would minority institutions immediately reassert governance and property rights, or would suppression persist through internalized deference?',
    'If structural, legal remedy would restore institutional autonomy. If internalized, legal remedy alone would not restore function — institutional capacity would require rebuilding cultural and organizational authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of minority institutional capacity is structural or internalized').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lausanne_minority_protections__restrictive_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(laus_tr_t0, lausanne_minority_protections__restrictive_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(laus_tr_t0, observed).
narrative_ontology:measurement(laus_tr_t5, lausanne_minority_protections__restrictive_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(laus_tr_t5, observed).
narrative_ontology:measurement(laus_tr_t10, lausanne_minority_protections__restrictive_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(laus_tr_t10, observed).
narrative_ontology:measurement(laus_tr_t15, lausanne_minority_protections__restrictive_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement_basis(laus_tr_t15, observed).
narrative_ontology:measurement(laus_tr_t20, lausanne_minority_protections__restrictive_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement_basis(laus_tr_t20, observed).
narrative_ontology:measurement(laus_tr_t25, lausanne_minority_protections__restrictive_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(laus_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(laus_be_t0, lausanne_minority_protections__restrictive_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement_basis(laus_be_t0, observed).
narrative_ontology:measurement(laus_be_t5, lausanne_minority_protections__restrictive_reading, base_extractiveness, 5, 0.71).
narrative_ontology:measurement_basis(laus_be_t5, observed).
narrative_ontology:measurement(laus_be_t10, lausanne_minority_protections__restrictive_reading, base_extractiveness, 10, 0.75).
narrative_ontology:measurement_basis(laus_be_t10, observed).
narrative_ontology:measurement(laus_be_t15, lausanne_minority_protections__restrictive_reading, base_extractiveness, 15, 0.79).
narrative_ontology:measurement_basis(laus_be_t15, observed).
narrative_ontology:measurement(laus_be_t20, lausanne_minority_protections__restrictive_reading, base_extractiveness, 20, 0.81).
narrative_ontology:measurement_basis(laus_be_t20, observed).
narrative_ontology:measurement(laus_be_t25, lausanne_minority_protections__restrictive_reading, base_extractiveness, 25, 0.82).
narrative_ontology:measurement_basis(laus_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(laus_su_t0, lausanne_minority_protections__restrictive_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(laus_su_t0, observed).
narrative_ontology:measurement(laus_su_t5, lausanne_minority_protections__restrictive_reading, suppression_requirement, 5, 0.66).
narrative_ontology:measurement_basis(laus_su_t5, observed).
narrative_ontology:measurement(laus_su_t10, lausanne_minority_protections__restrictive_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement_basis(laus_su_t10, observed).
narrative_ontology:measurement(laus_su_t15, lausanne_minority_protections__restrictive_reading, suppression_requirement, 15, 0.73).
narrative_ontology:measurement_basis(laus_su_t15, observed).
narrative_ontology:measurement(laus_su_t20, lausanne_minority_protections__restrictive_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement_basis(laus_su_t20, observed).
narrative_ontology:measurement(laus_su_t25, lausanne_minority_protections__restrictive_reading, suppression_requirement, 25, 0.76).
narrative_ontology:measurement_basis(laus_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lausanne_minority_protections__restrictive_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(lausanne_minority_protections__restrictive_reading, 0.12).
narrative_ontology:affects_constraint(lausanne_minority_protections__restrictive_reading, lausanne_minority_protections__expansive_reading).
narrative_ontology:affects_constraint(lausanne_minority_protections__restrictive_reading, lausanne_minority_protections__guarantor_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Lausanne minority protections kernel. The expansive reading affirms institutional autonomy and property protections; the guarantor reading emphasizes international enforcement. The three stories instantiate three structurally distinct constraint interpretations of the same treaty text. Each has its own beneficiary/victim structure, extractiveness, and classification. The restrictive reading here is classified as snare (high extraction, active enforcement); the expansive reading should classify as rope or tangled_rope (genuine coordination with some asymmetry); the guarantor reading's classification depends on whether international enforcement is empirically effective (low extraction if enforcement works, higher if international pressure is symbolic).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lausanne_minority_protections__restrictive_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
