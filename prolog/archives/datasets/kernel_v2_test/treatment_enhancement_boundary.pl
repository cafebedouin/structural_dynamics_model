% ============================================================================
% CONSTRAINT STORY: treatment_enhancement_boundary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_treatment_enhancement_boundary, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: treatment_enhancement_boundary
 *   human_readable: Treatment-Enhancement Boundary in Germline Genetic Modification
 *   domain: bioethics/reproductive_medicine/genetic_engineering
 *
 * SUMMARY:
 *   The treatment-enhancement boundary in germline genetic modification
 *   attempts to distinguish legitimate medical intervention (correcting
 *   disease-causing mutations) from impermissible enhancement (improving
 *   non-medical traits). This boundary is structurally unstable because
 *   'disease' itself is a contested category: conditions like deafness, short
 *   stature, and neurodivergence are classified as medical pathologies by
 *   some frameworks and as natural human variation by others. The constraint
 *   exhibits tangled rope structure because it solves a genuine coordination
 *   problem (how to govern powerful genetic technology without either banning
 *   beneficial applications or enabling unconstrained enhancement) while
 *   embedding asymmetric extraction (medicalizing human variation,
 *   concentrating access among the wealthy, eroding disability community
 *   legitimacy). The boundary's extractiveness has increased over the
 *   interval (0.35 → 0.52) as genetic technology has advanced and market
 *   pressure has grown, while suppression has also increased (0.52 → 0.61) as
 *   regulatory bodies have tightened enforcement in response to contested
 *   cases. Theater ratio has risen modestly (0.38 → 0.48) as boundary
 *   adjudication has become more procedurally elaborate without resolving the
 *   underlying conceptual instability.
 *
 * KEY AGENTS:
 *   - Disability Communities: Primary victim (powerless/identity_locked) — identity constituted through disability experience; the boundary's medicalization threatens group legitimacy
 *   - Prospective Parents with Genetic Risk: Mixed position (moderate/constrained) — benefit from disease prevention access but bear costs of medicalized reproductive choices and economic barriers
 *   - Genetic Technology Developers: Primary beneficiary (institutional/arbitrage) — the boundary enables clinical translation and creates viable markets
 *   - Medical Regulatory Bodies: Institutional enforcer (institutional/constrained) — cannot abandon the boundary without losing legitimacy but face perpetual jurisdictional disputes
 *   - Bioethics Consensus-Building Coalitions: Organized agents (organized/mobile) — see the boundary as transitional governance with implicit sunset
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees both genuine coordination and substantial extraction as structurally inseparable
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treatment_enhancement_boundary, 0.52).
domain_priors:suppression_score(treatment_enhancement_boundary, 0.61).
domain_priors:theater_ratio(treatment_enhancement_boundary, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treatment_enhancement_boundary, extractiveness, 0.52).
narrative_ontology:constraint_metric(treatment_enhancement_boundary, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(treatment_enhancement_boundary, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treatment_enhancement_boundary, tangled_rope).
narrative_ontology:human_readable(treatment_enhancement_boundary, "Treatment-Enhancement Boundary in Germline Genetic Modification").
narrative_ontology:topic_domain(treatment_enhancement_boundary, "bioethics/reproductive_medicine/genetic_engineering").

domain_priors:requires_active_enforcement(treatment_enhancement_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treatment_enhancement_boundary, enhancement_advocates).
narrative_ontology:constraint_beneficiary(treatment_enhancement_boundary, fertility_industry).
narrative_ontology:constraint_beneficiary(treatment_enhancement_boundary, genetic_technology_developers).
narrative_ontology:constraint_victim(treatment_enhancement_boundary, disability_communities).
narrative_ontology:constraint_victim(treatment_enhancement_boundary, genetic_variation_advocates).
narrative_ontology:constraint_victim(treatment_enhancement_boundary, economically_constrained_families).
narrative_ontology:constraint_vindicates(treatment_enhancement_boundary, medical_model_of_disability).
narrative_ontology:constraint_vindicates(treatment_enhancement_boundary, genetic_determinism_framework).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISABILITY COMMUNITIES (SNARE) — Identity-locked rather than materially trapped: members are structurally mobile (could advocate for different framings, could participate in policy processes) but their identity is constituted through disability experience and community membership. The boundary's medicalization of their traits as 'defects to be corrected' is an existential threat to group identity. Exit would require abandoning the social model of disability and accepting the medical model the boundary enforces. The constraint extracts by eroding the legitimacy of disability as variation rather than pathology, narrowing the space for non-medical framings of difference.
constraint_indexing:constraint_classification(treatment_enhancement_boundary, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: PROSPECTIVE PARENTS (TANGLED ROPE) — Constrained by medical authority, insurance coverage, and regulatory gatekeeping, but also genuine beneficiaries of the coordination function: the boundary provides a legitimate framework for preventing severe genetic disease. Experience both coordination (access to disease-prevention technology) and extraction (pressure to conform to normative genetic standards, economic barriers to access, medicalization of reproductive choices). The boundary solves a real problem (preventing suffering) while embedding asymmetric costs (who gets to define 'suffering').
constraint_indexing:constraint_classification(treatment_enhancement_boundary, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: GENETIC TECHNOLOGY DEVELOPERS (ROPE) — Primary beneficiaries with arbitrage-level exit: can shift research focus, relocate to permissive jurisdictions, or pivot to adjacent markets. The boundary provides legitimate coordination: a regulatory framework that enables clinical translation while managing safety concerns. Net beneficiaries — the constraint channels demand toward treatable conditions, creating a viable market while maintaining social license. Low effective extraction because the constraint enables rather than restricts their core activity.
constraint_indexing:constraint_classification(treatment_enhancement_boundary, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MEDICAL REGULATORY BODIES (TANGLED ROPE) — Institutional actors with constrained exit: cannot abandon the boundary without losing legitimacy, but also cannot enforce it without active negotiation of contested cases. Experience coordination (the boundary provides a workable framework for case-by-case adjudication) and extraction (the boundary's ambiguity creates perpetual jurisdictional disputes, resource demands, and reputational risk). The constraint solves a genuine governance problem while embedding ongoing costs in the form of boundary maintenance labor.
constraint_indexing:constraint_classification(treatment_enhancement_boundary, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: BIOETHICS COALITIONS (SCAFFOLD) — Organized agents (international bioethics committees, consensus conferences, professional societies) see the boundary as a temporary coordination mechanism with an implicit sunset: as genetic technology matures and social consensus evolves, the treatment-enhancement distinction will either collapse into a more precise framework (specific condition-by-condition adjudication) or be replaced by a capabilities-based approach that doesn't rely on disease classification. The current boundary is transitional governance — it holds the space while better frameworks develop. Low effective extraction because the coalition has agency and sees the boundary as provisional rather than permanent.
constraint_indexing:constraint_classification(treatment_enhancement_boundary, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the boundary exhibits both genuine coordination (preventing eugenic applications, managing safety risks, enabling incremental technology development) and substantial extraction (medicalizing human variation, concentrating genetic modification access among the wealthy, eroding disability community legitimacy). The boundary is not a natural law — 'disease' is a contested social category, not a biological given — but it solves a real collective action problem (how to govern powerful technology without either banning beneficial applications or enabling unconstrained enhancement). The analytical classification is tangled_rope because both functions are structurally present and neither can be eliminated without losing the other.
constraint_indexing:constraint_classification(treatment_enhancement_boundary, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(treatment_enhancement_boundary_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(treatment_enhancement_boundary, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(treatment_enhancement_boundary, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(treatment_enhancement_boundary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(treatment_enhancement_boundary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The boundary extracts by medicalizing human variation (eroding disability community legitimacy), concentrating genetic modification access among wealthy families (economic stratification), and channeling reproductive choices through medical authority (autonomy costs). But extraction is not maximal because the boundary also provides genuine coordination: it prevents eugenic applications, manages safety risks, and enables incremental technology development. The value reflects that roughly half the constraint's operation is extractive overhead rather than coordination cost. Suppression (0.61): Moderate-high. Significant barriers include regulatory gatekeeping (conditions must be classified as 'disease' to qualify), economic barriers (genetic modification is expensive and rarely covered by insurance), medical authority (physicians control access), and jurisdictional enforcement (cross-border restrictions). But suppression is not total — some jurisdictions are permissive, some families can afford private access, and advocacy can shift boundary classifications over time. Theater ratio (0.48): Moderate. Boundary adjudication involves substantial performative content: bioethics committee deliberations, case-by-case reviews, and regulatory hearings that follow elaborate procedures without resolving the underlying conceptual instability of 'disease' as a category. But theater is not dominant — real safety assessments, real stakeholder negotiations, and real enforcement actions occur. The value reflects that roughly half the boundary maintenance activity is functional and half is procedural performance.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how the same structural phenomenon — a regulatory boundary distinguishing treatment from enhancement — appears as pure extraction (Snare) to disability communities whose identity is threatened, as mixed coordination-extraction (Tangled Rope) to prospective parents and regulatory bodies who experience both benefits and costs, as legitimate coordination (Rope) to technology developers who benefit from the framework, and as transitional governance (Scaffold) to bioethics coalitions who see the boundary as provisional. The analytical observer sees tangled rope because both coordination and extraction are structurally present: the boundary solves a real governance problem (how to enable beneficial genetic technology without enabling eugenic enhancement) while embedding real extraction (medicalizing variation, concentrating access, eroding disability legitimacy). The perspectival gap is not a measurement error — it reflects genuine differences in structural position relative to the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Disability communities are victims with identity_locked exit, producing high directionality toward full target (d approaching 1.0) and high effective extraction. Their binding is cognitive rather than material: they could participate in policy processes but their identity frame makes accepting the medical model literally unthinkable from within. Prospective parents are mixed beneficiaries-victims with constrained exit, producing moderate directionality (d around 0.5) and moderate effective extraction — they benefit from disease prevention access but bear costs of medicalization and economic barriers. Genetic technology developers are primary beneficiaries with arbitrage exit, producing low directionality toward beneficiary (d approaching 0.0) and negative effective extraction — the constraint enables their activity rather than restricting it. Regulatory bodies are institutional actors with constrained exit and mixed structural position, producing moderate directionality. Bioethics coalitions are organized beneficiaries with mobile exit, producing low directionality. The analytical observer uses analytical exit and sees the full structure, producing the classification from base metrics without directional modulation.
 *
 * MANDATROPHY ANALYSIS:
 *   The treatment-enhancement boundary resolves mandatrophy by demonstrating that tangled rope is the structurally accurate classification when both coordination and extraction are genuinely present and neither can be eliminated without losing the other. The boundary is not a false summit (it is not a natural law being naturalized) — 'disease' is explicitly contested. It is not pure extraction (snare) — the safety and eugenic concerns are real and the boundary addresses them. It is not pure coordination (rope) — the medicalization of variation and concentration of access are real extraction mechanisms. It is not merely transitional (scaffold) — the underlying conceptual instability may be permanent rather than resolvable. The constraint is tangled rope because the coordination function (governing powerful technology) and the extraction function (medicalizing variation, concentrating access) are structurally coupled: you cannot have the governance framework without the medicalization, and you cannot eliminate the medicalization without losing the governance framework. This is the mandatrophy resolution: some constraints are irreducibly hybrid.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    disease_definition_stability,
    'Is ''disease'' a stable biological category that can ground a durable regulatory boundary, or a historically contingent social construction that will shift as genetic technology advances?',
    'Historical analysis of disease classification changes; cross-cultural comparison of conditions classified as disease vs variation; tracking of boundary disputes over contested conditions (deafness, short stature, neurodivergence)',
    'If stable: the boundary can function as durable coordination (Rope from more perspectives). If contingent: the boundary is a temporary political compromise masking ongoing extraction (Snare from more perspectives, especially for disability communities).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disease_definition_stability, conceptual, 'Whether disease is a stable biological category or contingent social construction').

omega_variable(
    enhancement_demand_trajectory,
    'Will demand for genetic enhancement (intelligence, height, appearance) remain marginal, or will it become mainstream once safety is established?',
    'Survey data on prospective parent preferences; adoption rates of existing enhancement technologies (IVF with embryo selection); cross-national comparison of permissive vs restrictive regulatory regimes',
    'If marginal: the boundary successfully channels technology toward medical applications (coordination function dominant). If mainstream: the boundary becomes an unstable dam against market pressure (extraction function dominant, with economically constrained families as primary victims).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enhancement_demand_trajectory, empirical, 'Whether enhancement demand will remain marginal or become mainstream').

omega_variable(
    disability_community_voice_weight,
    'Do disability communities have sufficient institutional power to prevent the boundary from collapsing toward permissive enhancement, or is their voice structurally marginalized in genetic technology governance?',
    'Analysis of disability representation in bioethics committees, regulatory bodies, and technology development processes; tracking of policy outcomes on contested conditions; measurement of community influence on boundary adjudication',
    'If sufficient power: the boundary can be maintained as a negotiated compromise (Tangled Rope stable). If marginalized: the boundary will drift toward medicalization and the disability community perspective becomes purely extractive (Snare from their position).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disability_community_voice_weight, empirical, 'Whether disability communities have sufficient institutional power in governance').

omega_variable(
    international_regulatory_convergence,
    'Will international regulatory frameworks converge on a shared treatment-enhancement boundary, or will jurisdictional arbitrage create a race to the bottom?',
    'Tracking of cross-national regulatory harmonization efforts; measurement of medical tourism flows for genetic modification; analysis of enforcement capacity in permissive jurisdictions',
    'If convergence: the boundary can function as stable global coordination. If arbitrage: the boundary becomes unenforceable and extraction concentrates among those with mobility and resources.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_regulatory_convergence, empirical, 'Whether international regulatory frameworks will converge or enable arbitrage').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treatment_enhancement_boundary, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(treat_enh_theater_t0, treatment_enhancement_boundary, theater_ratio, 0, 0.38).
narrative_ontology:measurement(treat_enh_theater_t3, treatment_enhancement_boundary, theater_ratio, 3, 0.42).
narrative_ontology:measurement(treat_enh_theater_t6, treatment_enhancement_boundary, theater_ratio, 6, 0.45).
narrative_ontology:measurement(treat_enh_theater_t10, treatment_enhancement_boundary, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(treat_enh_extract_t0, treatment_enhancement_boundary, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(treat_enh_extract_t3, treatment_enhancement_boundary, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(treat_enh_extract_t6, treatment_enhancement_boundary, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(treat_enh_extract_t10, treatment_enhancement_boundary, base_extractiveness, 10, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(treat_enh_suppress_t0, treatment_enhancement_boundary, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(treat_enh_suppress_t3, treatment_enhancement_boundary, suppression_requirement, 3, 0.56).
narrative_ontology:measurement(treat_enh_suppress_t6, treatment_enhancement_boundary, suppression_requirement, 6, 0.59).
narrative_ontology:measurement(treat_enh_suppress_t10, treatment_enhancement_boundary, suppression_requirement, 10, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treatment_enhancement_boundary, enforcement_mechanism).
narrative_ontology:affects_constraint(treatment_enhancement_boundary, preimplantation_genetic_diagnosis_access).
narrative_ontology:affects_constraint(treatment_enhancement_boundary, disability_rights_legal_framework).
narrative_ontology:affects_constraint(treatment_enhancement_boundary, genetic_privacy_regulation).

% DUAL FORMULATION NOTE:
% The treatment-enhancement boundary is upstream of specific genetic modification applications (PGD, germline editing) but represents a distinct structural constraint. The downstream constraints have their own extractiveness values reflecting the specific technologies and access patterns; the boundary has its own extractiveness reflecting the conceptual instability of 'disease' as a regulatory category and the asymmetric distribution of definitional power.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
