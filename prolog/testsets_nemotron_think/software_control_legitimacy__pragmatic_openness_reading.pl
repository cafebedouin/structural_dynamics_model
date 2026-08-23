% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__pragmatic_openness_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_control_legitimacy__pragmatic_openness_reading, []).

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
 *   constraint_id: software_control_legitimacy__pragmatic_openness_reading
 *   human_readable: Pragmatic Open Source Legitimacy Framing
 *   domain: software_engineering/political_economy/intellectual_property
 *
 * SUMMARY:
 *   The pragmatic openness reading frames software control as a
 *   methodological choice: open source development (peer review, distributed
 *   collaboration) produces objectively better software quality, but
 *   proprietary models remain legitimate alternatives for contexts requiring
 *   commercial sustainability, regulatory compliance, or specialized control.
 *   This reading became the dominant industry consensus after the 1998 Open
 *   Source Initiative founding, displacing the earlier 'free software'
 *   ethical framing in commercial contexts. It legitimates the modern
 *   landscape of corporate-backed open source (Linux Foundation, CNCF, Apache
 *   Foundation) alongside proprietary SaaS and licensed software.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__pragmatic_openness_reading, 0.12).
domain_priors:suppression_score(software_control_legitimacy__pragmatic_openness_reading, 0.08).
domain_priors:theater_ratio(software_control_legitimacy__pragmatic_openness_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, accessibility_collapse, 0.18).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__pragmatic_openness_reading, rope).
narrative_ontology:human_readable(software_control_legitimacy__pragmatic_openness_reading, "Pragmatic Open Source Legitimacy Framing").
narrative_ontology:topic_domain(software_control_legitimacy__pragmatic_openness_reading, "software_engineering/political_economy/intellectual_property").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__pragmatic_openness_reading, '4c63cb19-efa8-478d-88fd-2e67746092b9').
narrative_ontology:cs_kernel_codification('4c63cb19-efa8-478d-88fd-2e67746092b9', distributed).
narrative_ontology:cs_authority_grounding('4c63cb19-efa8-478d-88fd-2e67746092b9', expertise).
narrative_ontology:cs_interpretation_layer_present('4c63cb19-efa8-478d-88fd-2e67746092b9').
narrative_ontology:cs_reading_relation('4c63cb19-efa8-478d-88fd-2e67746092b9', software_control_legitimacy__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('4c63cb19-efa8-478d-88fd-2e67746092b9', software_control_legitimacy__property_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('4c63cb19-efa8-478d-88fd-2e67746092b9', software_control_legitimacy__commons_reading, coexists_with).
narrative_ontology:cs_axiom('4c63cb19-efa8-478d-88fd-2e67746092b9', foundational, pragmatic_methodology_primacy).
narrative_ontology:cs_axiom_status(pragmatic_methodology_primacy, holdable).
narrative_ontology:cs_axiom_grounding('4c63cb19-efa8-478d-88fd-2e67746092b9', pragmatic_methodology_primacy, instrumental).
narrative_ontology:cs_axiom('4c63cb19-efa8-478d-88fd-2e67746092b9', secondary, coexistence_legitimacy).
narrative_ontology:cs_axiom_status(coexistence_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('4c63cb19-efa8-478d-88fd-2e67746092b9', coexistence_legitimacy, conventional).
narrative_ontology:cs_reference_frame('4c63cb19-efa8-478d-88fd-2e67746092b9', engineering_pragmatism_framework).
narrative_ontology:cs_drift_state('4c63cb19-efa8-478d-88fd-2e67746092b9', contemporary_platform_ai_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4c63cb19-efa8-478d-88fd-2e67746092b9', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, open_source_developers).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, proprietary_software_vendors).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, software_users).
narrative_ontology:constraint_vindicates(software_control_legitimacy__pragmatic_openness_reading, peer_review_improves_software_quality).
narrative_ontology:constraint_vindicates(software_control_legitimacy__pragmatic_openness_reading, methodological_pluralism_serves_diverse_contexts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the Open Source Definition, approve licenses, and promote the framing that open source is a superior development methodology while proprietary models remain legitimate choices for different contexts. Their authority derives from community recognition and the practical success of open source projects.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, pragmatic_open_source_advocates, agenda_setter,
    organized, generational, mobile, global).

% Gain from distributed peer review, collaborative improvement, and shared infrastructure. Can choose open or proprietary licenses per project. The pragmatic framing validates their methodological choice without demanding ideological purity.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, open_source_developers, beneficiary,
    moderate, biographical, mobile, global).

% Their commercial model is legitimated as a valid alternative rather than delegitimized. They adopt open source strategically (open core, dual licensing) while maintaining proprietary offerings. The pragmatic framing reduces ideological friction in talent hiring and ecosystem participation.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, proprietary_software_vendors, beneficiary,
    institutional, generational, arbitrage, global).

% Benefit from quality improvements driven by open collaboration (security, features, interoperability) while retaining access to proprietary solutions for specialized needs. Their choice set includes both models without ideological penalty.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, software_users, beneficiary,
    organized, biographical, constrained, global).

% Would object to the legitimization of proprietary software as ethically acceptable. Their framing (software freedom as fundamental right) is not represented in the pragmatic discourse, though they participate in adjacent policy debates.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, freedom_advocates, excluded,
    organized, generational, identity_locked, global).

% Advocate for collective governance of digital infrastructure as a commons rather than methodological choice. Their framing is marginalized in mainstream industry discourse but active in academic and policy circles.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, commons_governance_advocates, excluded,
    moderate, generational, constrained, global).

% Analyzes the evolution of software development models, licensing trends, and their economic impacts. Sees the pragmatic framing as the dominant industry consensus since the late 1990s, with measurable effects on innovation velocity and market structure.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, industry_analyst_observer, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Produces better software through distributed peer review and collaboration while allowing proprietary models for contexts where they fit (specialized domains, regulatory compliance, business model sustainability). Solves the coordination problem of pooling engineering effort across organizational boundaries without requiring ideological uniformity.
% TRANSFER_FUNCTION: Moves recognition and legitimacy to both open and proprietary development models; moves quality improvements (security patches, performance optimizations, feature innovations) from open collaboration to all users including proprietary downstream consumers; moves talent and attention across the open/proprietary boundary fluidly.
% ABSENT_VOICES: Freedom advocates (FSF-aligned) who argue proprietary software is ethically illegitimate regardless of pragmatic outcomes; commons governance advocates who argue software infrastructure requires collective democratic governance not methodological pluralism. Both are structurally excluded from the pragmatic discourse which treats legitimacy as settled by outcomes.
% DISAPPEARANCE_RATIONALE: If the pragmatic legitimacy framing vanished, the discourse would polarize toward freedom-imperative (proprietary = unethical) and property-rights (open source = gift economy) poles. Commercial open source (open core, dual licensing, corporate-backed foundations) would lose its legitimating narrative, potentially reducing cross-boundary collaboration and increasing licensing conflicts. Policy debates on software supply chain, AI training data, and public procurement would shift toward rights-based frames.
% FOUNDING_PROBLEM: Early software development (1970s-1990s) suffered from redundant effort, slow innovation cycles, and vendor lock-in. Open collaboration (BSD, Linux, Apache) demonstrated superior quality and velocity but needed to coexist with commercial software businesses to achieve widespread adoption and sustainability.
% FOUNDING_PROBLEM_CORROBORATION: Eric Raymond's 'The Cathedral and the Bazaar' (1999) articulated the pragmatic case from within the hacker community. Corporate adoption (IBM/Red Hat, Google Android, Microsoft GitHub/Azure, Amazon AWS) validates the coexistence model economically. Contested by FSF (freedom imperative) and Ostrom-style commons scholars (governance framing) who argue the founding problem was mischaracterized as technical rather than political.
narrative_ontology:disappearance_verdict(software_control_legitimacy__pragmatic_openness_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__pragmatic_openness_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__pragmatic_openness_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(software_control_legitimacy__pragmatic_openness_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__pragmatic_openness_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__pragmatic_openness_reading_tests).
:- end_tests(software_control_legitimacy__pragmatic_openness_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is very low (0.12) because the framing explicitly rejects coercion — both models are legitimate, no party is forced into either. Suppression is minimal (0.08) — freedom advocates are excluded from the pragmatic discourse but not silenced; they maintain parallel institutions (FSF, GPL). Theater ratio is low (0.15) but rising slightly as corporations adopt open source performatively (open washing) without full community governance. Accessibility collapse is low (0.18) — developers and users freely choose licenses, platforms, and business models. Resistance is low (0.22) — the framing won the commercial argument; resistance comes from ideological minorities, not structural enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The engine should compute similar types across beneficiary seats (rope) because all gain from the coordination function with minimal extraction. The excluded seats (freedom, commons) would experience the constraint differently if they were centered — for them, the pragmatic framing IS a snare that suppresses their legitimacy claims. The observer seat sees the full structure: a genuine coordination achievement that incidentally marginalizes alternative framings.
 *
 * DIRECTIONALITY LOGIC:
 *   Pragmatic advocates (OSI, foundation staff) are agenda_setters with mobile exit — they define the discourse but can leave it. Open source developers are beneficiaries with mobile exit — they gain quality improvements and can switch projects/licenses. Proprietary vendors are beneficiaries with arbitrage exit — they capture commercial value while accessing open ecosystems. Users are beneficiaries with constrained exit — they gain quality but face switching costs. Freedom advocates are excluded and identity_locked — their self-concept is fused to the ethical framing, making exit from the dispute nearly impossible. Commons advocates are excluded and constrained — they engage in policy but lack industry influence.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (redundant effort, slow innovation) remains live — software complexity grows faster than any single organization can manage. The pragmatic framing has not atrophied; it has expanded to cover AI/ML model sharing, data commons, and hardware designs. No mandatrophy: the coordination function strengthens with scale.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the pragmatic openness reading a distinct constraint with its own ε, or a measurement perspective on the same underlying kernel?',
    'Apply ε-invariance test: if measuring the constraint via ''commercial adoption rates'' gives low ε but measuring via ''user freedom preservation'' gives high ε, they are different constraints. This reading authors low ε for the standing arrangement (coexistence) assessed by pragmatic lights.',
    'Confirms this JSON models one reading only; sibling readings require separate constraint stories with their own ε, beneficiaries, and classifications.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Kernel reading decomposition validity per ε-invariance principle').

omega_variable(
    legitimacy_boundary_ambiguity,
    'What counts as a ''legitimate alternative'' in the pragmatic framing — does it include SaaS-only services, cloud-hosted proprietary APIs, AI model weights behind APIs, or only traditional on-premise proprietary licenses?',
    'Track OSI and foundation position statements on SaaS, cloud, and AI licensing (e.g., OSI''s ''Open Source AI'' definition process, SSPL controversy, AGPL adoption).',
    'If the boundary excludes emerging proprietary forms (cloud, AI), the framing''s coordination function degrades — extraction rises as ''open source'' becomes a marketing label for effectively proprietary systems.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_boundary_ambiguity, empirical, 'Whether the pragmatic framing''s legitimacy boundary holds against new proprietary modalities').

omega_variable(
    quality_metric_contestation,
    'What metrics constitute ''better software'' in the pragmatic claim — security defects, feature velocity, developer productivity, user autonomy, long-term maintainability?',
    'Systematic literature review of empirical software engineering studies comparing open vs proprietary outcomes across dimensions; longitudinal analysis of CVE density, time-to-fix, contributor retention.',
    'If ''better'' only holds for narrow metrics (velocity, feature count) but not others (security, user autonomy), the coordination claim is partial — extraction may be hidden in unmeasured dimensions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(quality_metric_contestation, empirical, 'Whether the pragmatic quality claim holds across all relevant software quality dimensions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__pragmatic_openness_reading, 1998, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t1998, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 1998, 0.05).
narrative_ontology:measurement(soft_tr_t2003, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 2003, 0.08).
narrative_ontology:measurement(soft_tr_t2008, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 2008, 0.1).
narrative_ontology:measurement(soft_tr_t2013, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 2013, 0.12).
narrative_ontology:measurement(soft_tr_t2018, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 2018, 0.14).
narrative_ontology:measurement(soft_tr_t2025, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 2025, 0.15).

% Extraction over time
narrative_ontology:measurement(soft_be_t1998, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 1998, 0.05).
narrative_ontology:measurement(soft_be_t2003, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 2003, 0.07).
narrative_ontology:measurement(soft_be_t2008, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 2008, 0.09).
narrative_ontology:measurement(soft_be_t2013, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 2013, 0.1).
narrative_ontology:measurement(soft_be_t2018, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 2018, 0.11).
narrative_ontology:measurement(soft_be_t2025, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 2025, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t1998, software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 1998, 0.03).
narrative_ontology:measurement(soft_su_t2003, software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 2003, 0.04).
narrative_ontology:measurement(soft_su_t2008, software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 2008, 0.05).
narrative_ontology:measurement(soft_su_t2013, software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 2013, 0.06).
narrative_ontology:measurement(soft_su_t2018, software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 2018, 0.07).
narrative_ontology:measurement(soft_su_t2025, software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 2025, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__pragmatic_openness_reading, information_standard).
narrative_ontology:boltzmann_floor_override(software_control_legitimacy__pragmatic_openness_reading, 0.02).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy__property_rights_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy__commons_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the 'software control legitimacy' kernel into four readings with distinct ε values: pragmatic (low ε, rope), freedom_imperative (high ε for proprietary, snare from user seat), property_rights (moderate ε, tangled_rope), commons (low ε, rope/scaffold). The pragmatic reading influences the property_rights reading by normalizing open source as commercially viable (open core, dual licensing), and influences the commons reading by providing a pragmatic argument for shared infrastructure. It coexists with all three as live positions in ongoing discourse.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
