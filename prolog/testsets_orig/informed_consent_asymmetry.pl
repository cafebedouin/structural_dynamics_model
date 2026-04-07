% ============================================================================
% CONSTRAINT STORY: informed_consent_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_informed_consent_asymmetry, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: informed_consent_asymmetry
 *   human_readable: Informed Consent Asymmetry in Decision Authority Relationships
 *   domain: institutional/medical/legal
 *
 * SUMMARY:
 *   Informed consent asymmetry describes the structural gap between the
 *   disclosure of information by a decision authority (healthcare provider,
 *   employer, institution) and the comprehension and autonomous choice
 *   capacity of the consent bearer. The constraint operates across medical,
 *   employment, research, and institutional domains wherever expert authority
 *   must obtain explicit agreement from a dependent party. The asymmetry is
 *   not inherent to the need for disclosure itself — that serves genuine
 *   coordination and risk-sharing functions — but to the systematic
 *   exploitation of information imbalance, comprehension limits, and
 *   dependency to extract compliance independent of genuine understanding or
 *   volition. The constraint exhibits all six DR types from different
 *   perspectives, making it diagnostically rich for how institutional
 *   asymmetries can be simultaneously necessary coordination mechanisms and
 *   extractive devices.
 *
 * KEY AGENTS:
 *   - Consent Bearer: Primary victim (powerless/trapped) — depends on decision authority for essential services; refusal of consent results in service deprivation; no real exit option; bears full cost through constrained choices
 *   - Decision Authority: Primary beneficiary (institutional/arbitrage) — captures institutional efficiency, liability protection, and behavioral compliance during the consent ritual; can arbitrage between regulatory regimes
 *   - Healthcare/Employment Institution: Secondary beneficiary (institutional/arbitrage) — benefits from documented compliance, reduced legal liability, and operational standardization; maintains performative consent ritual
 *   - Patient Advocacy Coalition: Organized agent (organized/constrained) — building alternative verification pathways through consent literacy, standardized forms, right-to-explanation mandates; sees sunset clause in digital decision-support
 *   - Informed Consumer: Secondary agent (moderate/constrained) — has partial exit capacity through alternative providers; experiences mixed coordination (legitimate risk disclosure) and extraction (information asymmetry exploitation)
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing contingent institutional asymmetry as irreducible feature of expertise
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(informed_consent_asymmetry, 0.58).
domain_priors:suppression_score(informed_consent_asymmetry, 0.65).
domain_priors:theater_ratio(informed_consent_asymmetry, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(informed_consent_asymmetry, extractiveness, 0.58).
narrative_ontology:constraint_metric(informed_consent_asymmetry, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(informed_consent_asymmetry, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(informed_consent_asymmetry, tangled_rope).
narrative_ontology:human_readable(informed_consent_asymmetry, "Informed Consent Asymmetry in Decision Authority Relationships").
narrative_ontology:topic_domain(informed_consent_asymmetry, "institutional/medical/legal").

domain_priors:requires_active_enforcement(informed_consent_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(informed_consent_asymmetry, decision_authority).
narrative_ontology:constraint_beneficiary(informed_consent_asymmetry, institutional_providers).
narrative_ontology:constraint_victim(informed_consent_asymmetry, consent_bearer).
narrative_ontology:constraint_victim(informed_consent_asymmetry, epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSENT BEARER (SNARE) — Structurally dependent on decision authority for essential services (healthcare, employment, institutional access). No real exit option; refusal of consent results in deprivation of the service itself. Bears full extraction cost through constrained choices and forced trade-offs between autonomous decision-making and access to necessity.
constraint_indexing:constraint_classification(informed_consent_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: INFORMED CONSUMER (TANGLED ROPE) — Has some exit capacity (can seek alternative providers, negotiate terms) but faces high costs (time, financial, relational). Experiences genuine coordination function (legitimate need to disclose risks) alongside asymmetric extraction (information asymmetry exploited for institutional benefit). Moderate agency combined with real constraint.
constraint_indexing:constraint_classification(informed_consent_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: HEALTHCARE INSTITUTION (ROPE) — Experiences consent as coordination mechanism: disclosure enables shared decision-making and reduces medical error. Net beneficiary through liability protection and operational efficiency. Can arbitrage between regulatory regimes and exit through policy compliance.
constraint_indexing:constraint_classification(informed_consent_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PATIENT ADVOCACY COALITION (SCAFFOLD) — Organized agents (patient unions, legal aid, informed consent advocacy) building alternative verification pathways through consent literacy programs, standardized form requirements, and right-to-explanation mandates. See informed consent asymmetry as temporary problem solvable through distributed oversight and transparency standards. Sunset clause: as digital decision-support tools mature and patient portals normalize informed choice, the asymmetry diminishes.
constraint_indexing:constraint_classification(informed_consent_asymmetry, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSENT RITUAL (PITON) — The signature consent form persists through institutional inertia despite substantial performative content. Institutions maintain forms to satisfy regulatory theater rather than to enable genuine informed decision-making. The ritual demonstrates degraded function: most consent recipients cannot comprehend or retain the disclosed information; most institutions do not adapt consent materials to individual literacy or language capacity. Maintained through legal liability concerns, not through verification function.
constraint_indexing:constraint_classification(informed_consent_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal perspective, some informed consent asymmetry may appear inherent to professional authority relationships: the expert necessarily knows more than the layperson, creating an irreducible information gap. This perspective risks naturalizing what the structural data shows is a contingent institutional arrangement exploitable through systemic design choices. Engine false summit detection flags this as naturalized institutional asymmetry.
constraint_indexing:constraint_classification(informed_consent_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(informed_consent_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(informed_consent_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(informed_consent_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(informed_consent_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(informed_consent_asymmetry, TR),
    TR >= 0.70.

:- end_tests(informed_consent_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts through information asymmetry (what is disclosed is selected and framed by the decision authority), comprehension capture (complex information presented in formats chosen by institutions rather than optimized for understanding), and dependency leverage (refusal of consent results in service deprivation). The extraction is not maximal because some genuine coordination function exists (legitimate need to disclose material risks) and some consent bearers can navigate alternatives. Suppression (0.65): High. Structural barriers include: literacy requirements that exclude significant populations; time constraints that prevent careful deliberation; cognitive burden of complex technical information; power asymmetry that discourages questioning; professional authority framing that positions the expert as trustworthy rather than as having conflicting interests; dependency on the institution for essential services. Theater ratio (0.68): High. Consent rituals are substantially performative: most institutions use standardized forms disconnected from individual literacy levels or decision capacity; many consent bearers cannot accurately recall or comprehend disclosed information; institutions rarely adapt material based on comprehension testing; the ritual satisfies legal liability concerns rather than enabling genuine informed choice. Trajectory: theater ratio and extractiveness both increase over the interval as institutional compliance documentation expands (more forms, longer disclosures) without corresponding improvement in comprehension outcomes. The performative content accumulates as institutions layer regulatory compliance without redesigning for actual understanding.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival divergence across power levels and exit capacity. The consent bearer sees pure extraction (Snare) — their dependence and lack of alternatives mean the asymmetry is weaponized. The informed consumer sees mixed coordination and extraction (Tangled Rope) — they benefit from risk disclosure but experience manipulation of comprehension and framing. The institutional beneficiary sees coordination (Rope) — they experience consent as an efficient mechanism for shared decision-making and liability management. The patient advocacy coalition sees a temporary problem with a sunset (Scaffold) — digital decision-support and consent literacy are gradually reducing asymmetry. The institutional ritual itself appears degraded (Piton) — maintained through legal requirement rather than function. The civilizational analytical observer risks seeing an immutable feature of expertise (Mountain) — but structural data reveals this as naturalized institutional choice. The perspectival gap reveals that informed consent asymmetry is not a natural law but a design choice maintainable only through continued institutional control of information framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from each agent's structural position relative to the information and power flows. The consent bearer (powerless/trapped) experiences maximum extraction: they depend on the institution, cannot exit without losing essential services, and have minimal capacity to verify claims or push back against framing. The institutional beneficiary (institutional/arbitrage) experiences low extraction: they control the disclosure, can arbitrage between regulatory regimes, and benefit from both the coordination function (legitimate risk-sharing) and the extraction mechanism (liability protection, behavioral compliance). The informed consumer (moderate/constrained) experiences moderate extraction: they have some exit capacity (can seek alternative providers) but face high switching costs, and experience genuine coordination alongside exploitation of information asymmetry. The organized coalition (organized/constrained) has reduced extraction through multiple actors providing verification and advocacy, reducing any single institution's control.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that informed consent operates as a genuine coordination mechanism AND as an extraction device simultaneously. The coordination function (legitimate disclosure of material risks enabling better decision-making) is real and necessary. The extraction mechanism (exploitation of information asymmetry, comprehension limits, and dependency) is also real and maintainable through institutional design choices. The constraint is not a false mountain (not a natural law) nor a false rope (not pure coordination). It is genuinely tangled: the institutional benefit from the coordination function would justify modest extractiveness, but measured extractiveness (0.58) substantially exceeds the coordination cost floor, indicating that institutional incentives favor documentation over comprehension, liability protection over genuine autonomy. The falseness detector flags the mountain perspective as naturalization of design choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    comprehension_vs_disclosure,
    'Is the primary barrier to informed consent the volume/complexity of disclosed information, or the consent bearer''s capacity to comprehend and retain it?',
    'Empirical comprehension testing: measure information retention and decision quality after standardized consent disclosures vs simplified, literacy-matched disclosures',
    'If volume/complexity: the solution is information design and materiality standards (organizational reform). If comprehension capacity: the problem may be intractable without changing the relationship structure itself (shifting from expert paternalism to shared decision-making protocols).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(comprehension_vs_disclosure, empirical, 'Whether asymmetry stems from information volume or comprehension capacity').

omega_variable(
    power_differential_irreducibility,
    'Is the power differential in expert-layperson relationships a structural necessity or a design choice?',
    'Comparative analysis of relationship models: participatory medicine vs paternalistic medicine; informed consent protocols in jurisdictions with mandatory shared decision-making vs those with minimal requirements',
    'If structural necessity: the constraint is a mountain (asymmetry inherent to expertise). If design choice: the constraint is institutional extraction (asymmetry is maintained because it benefits decision authorities).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(power_differential_irreducibility, conceptual, 'Whether expert-layperson power differential is irreducible').

omega_variable(
    institutional_incentive_misalignment,
    'Do institutional incentives reward genuine informed consent (shared decision-making) or merely documented consent (liability protection)?',
    'Institutional audit: analyze quality metrics, performance incentives, and resource allocation for consent literacy programs vs compliance documentation systems',
    'If genuine informed consent incentivized: institutions will invest in comprehension and adaptive disclosure. If documentation incentivized: institutions will maintain performative ritual (piton path). Current data suggests documentation incentivization dominates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_incentive_misalignment, empirical, 'Whether institutions incentivize genuine vs documented consent').

omega_variable(
    digital_literacy_substitution,
    'Can digital decision-support tools (interactive interfaces, personalized risk calculators, AI-assisted comprehension) effectively substitute for human explanation in complex medical decisions?',
    'RCT comparing patient comprehension, decision quality, and satisfaction across human explanation, digital tool, and hybrid approaches; longitudinal tracking of decision regret',
    'If effective substitution: scaffold sunset is real — digital tools reduce asymmetry and exit from institutional theater. If incomplete: human explanation remains essential and asymmetry persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(digital_literacy_substitution, empirical, 'Whether digital tools can substitute for human explanation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(informed_consent_asymmetry, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ica_tr_t0, informed_consent_asymmetry, theater_ratio, 0, 0.55).
narrative_ontology:measurement(ica_tr_t5, informed_consent_asymmetry, theater_ratio, 5, 0.62).
narrative_ontology:measurement(ica_tr_t10, informed_consent_asymmetry, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(ica_be_t0, informed_consent_asymmetry, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ica_be_t5, informed_consent_asymmetry, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(ica_be_t10, informed_consent_asymmetry, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(informed_consent_asymmetry, information_standard).
narrative_ontology:affects_constraint(informed_consent_asymmetry, medical_error_prevention).
narrative_ontology:affects_constraint(informed_consent_asymmetry, institutional_liability_externalization).
narrative_ontology:affects_constraint(informed_consent_asymmetry, healthcare_access_dependency).

% DUAL FORMULATION NOTE:
% Informed consent asymmetry is upstream of specific consent-dependent practices (clinical trials, surgical procedures, employment contracts) but represents a distinct structural constraint. Different consent domains may have different ε values reflecting domain-specific power imbalances and exit options; this story represents the general constraint across domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(informed_consent_asymmetry, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
