% ============================================================================
% CONSTRAINT STORY: guide_rna_off_target_variance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_guide_rna_off_target_variance, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: guide_rna_off_target_variance
 *   human_readable: Guide RNA Off-Target Variance in Clinical Genome Editing
 *   domain: biomedical_research/genome_editing/reproductive_medicine
 *
 * SUMMARY:
 *   The guide RNA off-target variance constraint emerges from the empirical
 *   observation that off-target editing frequency is guide-dependent rather
 *   than platform-dependent. Using identical ABE (adenine base editor)
 *   platforms, HBG1/2-targeting sgRNA produces 37% editing at its most active
 *   off-target site, while PCSK9-targeting sgRNA produces <1% editing at only
 *   one off-target site. This 37-fold variance creates asymmetric
 *   information: early clinical programs selected guides before comprehensive
 *   off-target profiling became standard; late programs face higher
 *   regulatory bars. The constraint exhibits genuine coordination function
 *   (the field needs guide selection criteria and off-target assays) layered
 *   with extraction (early movers captured approval with guides later shown
 *   to be high off-target; patients bear biological risk of off-target
 *   mutations; late programs face higher validation costs). Theater ratio
 *   (0.41) reflects that some off-target profiling is performative — assays
 *   detect sites but disclosure thresholds and clinical significance remain
 *   contested, so programs can 'profile' without meaningfully constraining
 *   guide selection. Suppression (0.68) reflects information asymmetry
 *   (patients cannot assess guide quality), regulatory lag (standards are
 *   maturing but not yet harmonized), and sunk cost barriers (late-stage
 *   programs cannot easily switch guides).
 *
 * KEY AGENTS:
 *   - Patients receiving high off-target guides: Primary victim (powerless/trapped) — information asymmetry prevents informed choice; bear biological risk of off-target mutations with no exit post-treatment
 *   - Guide RNA design services: Primary beneficiary (institutional/arbitrage) — capture revenue from computational screening and guide optimization; variance creates demand for their services
 *   - Early adopter clinical programs: Secondary beneficiary (institutional/arbitrage) — captured regulatory approval before stringent off-target profiling; grandfathering protects market position
 *   - Late-stage clinical programs: Secondary victim (moderate/constrained) — face higher regulatory bars and costly re-validation if guide variance emerges late in development
 *   - Platform developers: Mixed position (institutional/constrained) — benefit from off-target assays validating platform safety but constrained by guide-dependent variance they cannot control
 *   - Regulatory harmonization coalition: Organized agents (organized/mobile) — FDA, EMA, industry consortia building mandatory guide profiling standards with 5-8 year sunset
 *   - Analytical observer: Civilizational view (analytical/analytical) — sees genuine coordination need layered with asymmetric extraction and active enforcement maintaining information asymmetry
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(guide_rna_off_target_variance, 0.52).
domain_priors:suppression_score(guide_rna_off_target_variance, 0.68).
domain_priors:theater_ratio(guide_rna_off_target_variance, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(guide_rna_off_target_variance, extractiveness, 0.52).
narrative_ontology:constraint_metric(guide_rna_off_target_variance, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(guide_rna_off_target_variance, theater_ratio, 0.41).

% --- Constraint claim ---
narrative_ontology:constraint_claim(guide_rna_off_target_variance, tangled_rope).
narrative_ontology:human_readable(guide_rna_off_target_variance, "Guide RNA Off-Target Variance in Clinical Genome Editing").
narrative_ontology:topic_domain(guide_rna_off_target_variance, "biomedical_research/genome_editing/reproductive_medicine").

domain_priors:requires_active_enforcement(guide_rna_off_target_variance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(guide_rna_off_target_variance, guide_rna_design_services).
narrative_ontology:constraint_beneficiary(guide_rna_off_target_variance, early_adopter_clinical_programs).
narrative_ontology:constraint_beneficiary(guide_rna_off_target_variance, platform_developers).
narrative_ontology:constraint_victim(guide_rna_off_target_variance, patients_receiving_high_off_target_guides).
narrative_ontology:constraint_victim(guide_rna_off_target_variance, late_stage_clinical_programs).
narrative_ontology:constraint_victim(guide_rna_off_target_variance, regulatory_confidence).
narrative_ontology:constraint_vindicates(guide_rna_off_target_variance, sequence_context_primacy_hypothesis).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PATIENT WITH HIGH OFF-TARGET GUIDE (SNARE) — Trapped by information asymmetry and medical necessity. Cannot assess guide RNA quality pre-treatment; bears full biological risk of off-target edits (37% at HBG1/2 most active site). No exit once edited. Maximum extraction: the variance in guide quality is invisible to the patient but determines their mutation burden.
constraint_indexing:constraint_classification(guide_rna_off_target_variance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: LATE-STAGE CLINICAL PROGRAM (TANGLED ROPE) — Constrained by sunk costs and regulatory commitments. Benefits from the coordination function (standardized off-target assays enable comparability) but bears extraction when guide-dependent variance emerges late in development. Can pivot to alternative guides but at severe cost (re-validation, trial delays). Mixed experience: the assay infrastructure helps, the variance hurts.
constraint_indexing:constraint_classification(guide_rna_off_target_variance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: GUIDE RNA DESIGN SERVICE (ROPE) — Primary beneficiary with arbitrage-grade exit. Captures revenue from guide optimization and off-target prediction algorithms. The variance creates demand for their services: programs pay for computational screening to avoid high off-target guides. Experiences the constraint as pure coordination: the field needs guide selection criteria, and they provide it. Net positive extraction flow toward this agent.
constraint_indexing:constraint_classification(guide_rna_off_target_variance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY HARMONIZATION COALITION (SCAFFOLD) — Organized agents (FDA, EMA, industry consortia) building standardized guide quality metrics and disclosure requirements. See the variance as a temporary coordination failure with a sunset: once guide-specific off-target profiling becomes mandatory pre-clinical (estimated 5-8 years), the information asymmetry collapses. The coalition has agency and sees an exit path through regulatory maturation.
constraint_indexing:constraint_classification(guide_rna_off_target_variance, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: PLATFORM DEVELOPER (TANGLED ROPE) — Benefits from the coordination function (off-target assays validate platform safety claims) but constrained by guide-dependent variance that platform improvements cannot eliminate. ABE platform is identical for HBG1/2 (37% off-target) and PCSK9 (<1% off-target) — the variance is in the guide, not the platform. Mixed position: platform reputation depends on guide selection, which they don't control.
constraint_indexing:constraint_classification(guide_rna_off_target_variance, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, guide-dependent off-target variance reflects genuine coordination need (the field must select guides carefully) layered with asymmetric extraction (early programs captured regulatory approval with guides later shown to be high off-target; late programs face higher bars). The variance is partly inherent to sequence context biology, partly contingent on disclosure norms and regulatory lag. Tangled Rope: real coordination function, real extraction, active enforcement required to maintain information asymmetry.
constraint_indexing:constraint_classification(guide_rna_off_target_variance, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(guide_rna_off_target_variance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(guide_rna_off_target_variance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(guide_rna_off_target_variance, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(guide_rna_off_target_variance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(guide_rna_off_target_variance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Early clinical programs captured regulatory approval with guides later shown to have high off-target rates (HBG1/2 at 37% vs PCSK9 at <1%). Patients receiving high off-target guides bear biological risk they cannot assess pre-treatment. Guide RNA design services capture revenue from the variance (programs pay for computational screening). Late-stage programs face higher validation costs. The extraction is substantial but not maximal — some of the variance reflects genuine biological complexity (sequence context effects) rather than pure rent-seeking. Suppression (0.68): High. Information asymmetry is the primary suppression mechanism: patients cannot assess guide quality; regulatory standards are maturing but not yet harmonized globally; late-stage programs face sunk cost barriers to switching guides. Suppression has increased over the interval as early programs locked in regulatory precedents that late programs must now exceed. Theater ratio (0.41): Moderate. Off-target profiling via targeted deep sequencing is functional (detects real off-target sites) but partly performative: disclosure thresholds are contested, clinical significance of low-frequency off-targets is unclear, and programs can 'profile' without constraining guide selection. Theater has increased as profiling became standard but interpretation and disclosure norms lag behind assay capability.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates indexical classification across power and exit gradients. Patients see pure extraction (Snare) — trapped by information asymmetry and medical necessity, bearing biological risk they cannot assess. Guide RNA design services see pure coordination (Rope) — the field needs guide selection criteria, and they provide it; variance creates legitimate demand for their services. Late-stage clinical programs and platform developers see mixed coordination and extraction (Tangled Rope) — benefit from standardized assays but bear costs when guide variance emerges. The regulatory harmonization coalition sees a temporary problem with a sunset (Scaffold) — mandatory guide profiling will collapse the information asymmetry within 5-8 years. The analytical observer sees Tangled Rope at the civilizational scale — genuine coordination need (guide selection criteria) layered with asymmetric extraction (early mover advantage, patient risk, regulatory lag) requiring active enforcement (information asymmetry maintenance) to persist. The perspectival gap is not a disagreement about facts but a structural consequence of different positions in the extraction flow.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position. Patients receiving high off-target guides are victims with trapped exit → high d → high experienced extraction (maximum burden). Guide RNA design services are beneficiaries with arbitrage exit → low d → negative experienced extraction (net subsidy from the constraint). Early adopter clinical programs are beneficiaries with arbitrage exit → low d → low experienced extraction (captured approval before standards tightened). Late-stage clinical programs are victims with constrained exit → moderate-high d → moderate-high experienced extraction (face higher bars and costly pivots). Platform developers are mixed: beneficiaries (off-target assays validate platform safety) but constrained by guide variance they don't control → moderate d → moderate experienced extraction. Regulatory harmonization coalition is organized with mobile exit → low-moderate d → low experienced extraction (has agency to build alternative pathways). Analytical observer has analytical exit → context-independent d → sees the full structural asymmetry without experiencing extraction directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that Tangled Rope classification captures the dual structure: genuine coordination function (the field must select guides carefully; off-target assays enable comparability) AND asymmetric extraction (early programs captured approval with high off-target guides; patients bear biological risk; late programs face higher bars). The coordination function is real — without guide selection criteria and off-target profiling, the field cannot advance safely. The extraction is also real — the variance in guide quality creates winners (early movers, design services) and losers (patients with high off-target guides, late-stage programs). Active enforcement maintains the asymmetry: information remains opaque to patients; regulatory standards lag behind assay capability; sunk costs trap late-stage programs. The Scaffold perspective (regulatory harmonization coalition) identifies the sunset mechanism: mandatory guide profiling will collapse the information asymmetry, converting the constraint toward Rope as guide quality becomes transparent. The constraint is not 'really' coordination or 'really' extraction — it is structurally both, and the perspectival tuple determines which aspect dominates the experienced classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sequence_context_predictability,
    'Is guide RNA off-target variance fundamentally unpredictable from sequence alone, or do current algorithms simply lack sufficient training data?',
    'Prospective validation of next-generation off-target prediction algorithms on held-out guide sets; comparison of predicted vs observed off-target profiles for guides designed 2026-2030',
    'If fundamentally unpredictable: variance is a natural law (Mountain from more perspectives), and empirical profiling remains mandatory. If algorithmically predictable: variance is a coordination problem (Rope from more perspectives), and computational screening suffices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sequence_context_predictability, empirical, 'Whether guide off-target variance is algorithmically predictable').

omega_variable(
    regulatory_disclosure_threshold,
    'What off-target editing frequency threshold should trigger mandatory disclosure and informed consent?',
    'Risk-benefit analysis correlating off-target frequency with adverse events; stakeholder consensus process involving patients, clinicians, and regulators',
    'If threshold set at 1%: HBG1/2-type guides (37% at most active site) are excluded, reducing treatment options. If threshold set at 50%: most guides pass, preserving access but increasing patient risk. Threshold choice determines extraction distribution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_disclosure_threshold, preference, 'Regulatory threshold for off-target disclosure').

omega_variable(
    platform_vs_guide_attribution,
    'When off-target events occur, should liability and reputational cost accrue to the platform developer or the guide designer?',
    'Legal precedent from early adverse event cases; industry standard-of-care evolution; insurance underwriting practices',
    'If platform-attributed: platform developers bear extraction, incentivizing platform-level safety improvements (but guide variance persists). If guide-attributed: guide designers bear extraction, incentivizing guide-level optimization (but platform developers escape accountability).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(platform_vs_guide_attribution, preference, 'Attribution of liability for off-target events').

omega_variable(
    early_approval_grandfathering,
    'Should guides approved under earlier, less stringent off-target profiling standards be grandfathered, or re-evaluated under current standards?',
    'Regulatory policy decisions by FDA, EMA, and other authorities; political pressure from patient advocacy groups vs industry lobbying',
    'If grandfathered: early programs retain market advantage despite higher off-target burden (extraction persists). If re-evaluated: early programs face costly re-validation (extraction redistributed to early movers).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(early_approval_grandfathering, preference, 'Grandfathering of early-approved guides').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(guide_rna_off_target_variance, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(grna_ot_theater_t0, guide_rna_off_target_variance, theater_ratio, 0, 0.25).
narrative_ontology:measurement(grna_ot_theater_t2, guide_rna_off_target_variance, theater_ratio, 2, 0.32).
narrative_ontology:measurement(grna_ot_theater_t4, guide_rna_off_target_variance, theater_ratio, 4, 0.38).
narrative_ontology:measurement(grna_ot_theater_t6, guide_rna_off_target_variance, theater_ratio, 6, 0.41).

% Extraction over time
narrative_ontology:measurement(grna_ot_extract_t0, guide_rna_off_target_variance, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(grna_ot_extract_t2, guide_rna_off_target_variance, base_extractiveness, 2, 0.42).
narrative_ontology:measurement(grna_ot_extract_t4, guide_rna_off_target_variance, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(grna_ot_extract_t6, guide_rna_off_target_variance, base_extractiveness, 6, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(grna_ot_suppress_t0, guide_rna_off_target_variance, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(grna_ot_suppress_t2, guide_rna_off_target_variance, suppression_requirement, 2, 0.58).
narrative_ontology:measurement(grna_ot_suppress_t4, guide_rna_off_target_variance, suppression_requirement, 4, 0.64).
narrative_ontology:measurement(grna_ot_suppress_t6, guide_rna_off_target_variance, suppression_requirement, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(guide_rna_off_target_variance, information_standard).
narrative_ontology:affects_constraint(guide_rna_off_target_variance, base_editor_platform_safety_claims).
narrative_ontology:affects_constraint(guide_rna_off_target_variance, clinical_trial_design_standards).
narrative_ontology:affects_constraint(guide_rna_off_target_variance, informed_consent_disclosure_requirements).

% DUAL FORMULATION NOTE:
% The guide RNA off-target variance is downstream of base editor platform development but represents a distinct structural constraint. Platform safety (upstream) has its own extractiveness reflecting the empirical status of platform-level claims; guide variance (this constraint) has its own extractiveness reflecting guide-specific information asymmetry and regulatory lag. The two constraints are linked (platform developers benefit from off-target assays validating platform safety) but have different beneficiary/victim structures and different ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(guide_rna_off_target_variance, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
