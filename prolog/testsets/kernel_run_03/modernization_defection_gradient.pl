% ============================================================================
% CONSTRAINT STORY: modernization_defection_gradient
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-04-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_modernization_defection_gradient, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: modernization_defection_gradient
 *   human_readable: Modernization Defection Gradient: The Aneyoshi Tsunami Stone Commitment (1933-2011)
 *   domain: disaster_anthropology/institutional_memory/commitment_systems
 *
 * SUMMARY:
 *   The Aneyoshi tsunami stone ('Tsunami stone at 56 meters / June 15, 1933')
 *   represents a unique constraint story: a 78-year institutional memory
 *   commitment that persisted across three generations of modernization
 *   pressure, validated by a catastrophic event that vindicated deference to
 *   the ancestor's warning. The constraint exhibits genuine structural
 *   tension between institutional memory systems (jishin kitai
 *   bunka—earthquake preparedness culture) and modernization narratives that
 *   frame traditional practices as superstition to be overcome. The 2011
 *   Tōhoku tsunami demonstrated that the stone's boundary held: the tsunami's
 *   maximum run-up occurred just below the marker line. Neighboring
 *   communities that ignored comparable historical markers were devastated.
 *   Yet this validation masks a deeper institutional degradation: the
 *   constraint's mechanism for transmission has shifted from lived cultural
 *   practice ('we do not build here because our ancestors survived a tsunami
 *   by heeding this warning') to heritage commemoration ('this stone is a
 *   UNESCO example of traditional knowledge'). The constraint persists, but
 *   increasingly through institutional inertia (municipal heritage
 *   designation, tourism marketing, academic interest) rather than through
 *   active cultural understanding. Extractiveness has risen modestly (0.28 →
 *   0.38) as modernization pressures intensified and the constraint became
 *   increasingly costly relative to development opportunity. Theater ratio
 *   has risen more sharply (0.15 → 0.44) as ceremonial maintenance began
 *   replacing functional deference.
 *
 * KEY AGENTS:
 *   - Aneyoshi Residents: Primary victim (powerless/trapped at local scale) — face land-use restrictions with no exit option except emigration; suppressed by both tradition deference and modernization pressure
 *   - Regional Developers: Secondary actor (moderate/constrained at regional scale) — experience mixed coordination (regional economic stability) and extraction (zoning constraints); can exit by shifting to inland development
 *   - Municipal Government / Heritage Institutions: Primary beneficiary (institutional/arbitrage at national scale) — benefit from maintaining institutional memory system; can choose to enforce or ignore the constraint
 *   - Post-2011 Disaster-Response Coalition: Institutional actor (organized/constrained at national scale) — restructuring traditional constraint into formal risk governance; building new institutional scaffold for tsunami risk
 *   - Heritage Preservation Systems: Institutional inertia maintainer (institutional/arbitrage at global scale) — maintain the constraint through commemorative designation rather than functional governance
 *   - Analytical Observer: Cross-position analyst (analytical/analytical at civilizational scale) — risks misclassifying social coordination mechanism as immutable natural law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(modernization_defection_gradient, 0.38).
domain_priors:suppression_score(modernization_defection_gradient, 0.68).
domain_priors:theater_ratio(modernization_defection_gradient, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(modernization_defection_gradient, extractiveness, 0.38).
narrative_ontology:constraint_metric(modernization_defection_gradient, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(modernization_defection_gradient, theater_ratio, 0.44).

% --- Constraint claim ---
narrative_ontology:constraint_claim(modernization_defection_gradient, tangled_rope).
narrative_ontology:human_readable(modernization_defection_gradient, "Modernization Defection Gradient: The Aneyoshi Tsunami Stone Commitment (1933-2011)").
narrative_ontology:topic_domain(modernization_defection_gradient, "disaster_anthropology/institutional_memory/commitment_systems").

domain_priors:requires_active_enforcement(modernization_defection_gradient).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(modernization_defection_gradient, '1157abe7-a6bb-4a3c-8914-d19fc18ba1a0').
narrative_ontology:cs_created_at('1157abe7-a6bb-4a3c-8914-d19fc18ba1a0', '').
narrative_ontology:cs_kernel_codification('1157abe7-a6bb-4a3c-8914-d19fc18ba1a0', fixed_text).
narrative_ontology:cs_authority_grounding('1157abe7-a6bb-4a3c-8914-d19fc18ba1a0', lineage).
narrative_ontology:cs_interpretation_layer_present('1157abe7-a6bb-4a3c-8914-d19fc18ba1a0').
narrative_ontology:cs_reading_relation('1157abe7-a6bb-4a3c-8914-d19fc18ba1a0', scientific_risk_governance_reading, influences).
narrative_ontology:cs_reading_relation('1157abe7-a6bb-4a3c-8914-d19fc18ba1a0', heritage_preservation_reading, coexists_with).
narrative_ontology:cs_axiom('1157abe7-a6bb-4a3c-8914-d19fc18ba1a0', foundational, ancestor_survival_warrant_authority).
narrative_ontology:cs_axiom_status(ancestor_survival_warrant_authority, holdable).
narrative_ontology:cs_axiom_grounding('1157abe7-a6bb-4a3c-8914-d19fc18ba1a0', ancestor_survival_warrant_authority, conventional).
narrative_ontology:cs_axiom('1157abe7-a6bb-4a3c-8914-d19fc18ba1a0', foundational, transmission_through_lived_practice).
narrative_ontology:cs_axiom_status(transmission_through_lived_practice, overridden).
narrative_ontology:cs_axiom_grounding('1157abe7-a6bb-4a3c-8914-d19fc18ba1a0', transmission_through_lived_practice, conventional).
narrative_ontology:cs_reference_frame('1157abe7-a6bb-4a3c-8914-d19fc18ba1a0', ancestral_warning_deference).
narrative_ontology:cs_drift_state('1157abe7-a6bb-4a3c-8914-d19fc18ba1a0', contemporary_post_2011, gap(practice_drift, substantial, true)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(modernization_defection_gradient, institutional_memory_holders).
narrative_ontology:constraint_beneficiary(modernization_defection_gradient, risk_aware_communities).
narrative_ontology:constraint_victim(modernization_defection_gradient, development_pressure_constituencies).
narrative_ontology:constraint_victim(modernization_defection_gradient, modernization_narrative_believers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSTRAINED RESIDENT / TRAPPED IN RISK ZONE (SNARE) — Local residents face maximum extraction: suppressed by social pressure to accept community land-use restrictions ('listen to your elders'), yet simultaneously suppressed by modernization incentives (economic development, housing availability, emigration costs). The stone constraint extracts obedience; exit requires leaving the community entirely. No coordination benefit perceived — only the burden of restricted land use in a region with limited flat developable space.
constraint_indexing:constraint_classification(modernization_defection_gradient, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: PRAGMATIC DEVELOPER / REGIONAL SCALE (TANGLED ROPE) — Regional developers and municipal planners experience genuine coordination (preventing catastrophic coastal losses protects entire regional economic viability) alongside extraction (land-use restrictions reduce developable territory, constrain profit margins, create zoning conflicts). Exit is costly but possible — developers can shift to inland projects or relocate. Moderate experienced extraction; genuine coordination benefit.
constraint_indexing:constraint_classification(modernization_defection_gradient, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSTITUTIONAL MEMORY HOLDER / JISHIN KITAI BUNKA (ROPE) — Municipal governments and heritage institutions benefit from maintaining the constraint: it establishes institutional legitimacy through demonstrable historical wisdom ('we preserved what our ancestors knew'), generates positive regional identity, and—critically—validates the institutional memory system itself. The constraint is experienced as pure coordination: solving the collective action problem of maintaining risk awareness across generations. Arbitrage position: institutional actors can always exit (ignore the stone, allow development), but they benefit more from enforcing it.
constraint_indexing:constraint_classification(modernization_defection_gradient, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: POST-TSUNAMI REFORM COALITION / INSTITUTIONAL REDESIGN (SCAFFOLD) — After 2011, organized agents (disaster-response organizations, geology departments, government reforms) restructured risk communication around the stone into formal early-warning systems, building codes, and scientized tsunami modeling. The old constraint (deference to the stone) transitions to a modern constraint (compliance with seismic-activity protocols). This perspective sees the stone as temporary institutional scaffolding — valuable for maintaining behavioral continuity during the transition to scientific risk governance. Sunset logic: formal warning systems and engineering standards replace deference to tradition.
constraint_indexing:constraint_classification(modernization_defection_gradient, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: HERITAGE PRESERVATION SYSTEM / PERFORMATIVE CONSERVATION (PITON) — International heritage discourse has largely transformed the stone into a commemorative symbol: its functional value (preventing construction below the line) persists, but the mechanism maintaining it has degraded from lived practice ('this is where we do not build because our ancestors warned us') to ceremonial recognition ('this stone is a UNESCO example of traditional disaster knowledge'). Theater ratio is moderate (0.44) because the functional restriction still works—the stone still prevents construction—but the maintenance mechanism relies on institutional inertia (heritage designation) rather than living cultural practice. The constraint persists despite the atrophy of the indigenous justification logic.
constraint_indexing:constraint_classification(modernization_defection_gradient, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / ECOLOGICAL IMMUTABILITY (MOUNTAIN) — From a civilizational/universal perspective, the constraint might appear to be grounded in an immutable natural law: tsunami run-up at Aneyoshi is physically determined by seismic energy release and bathymetry; the stone marks a naturally limiting risk boundary. However, the structural data reveals a false summit. The stone's true functional value is social (maintaining collective memory across generations) and institutional (grounding governance in tradition), not physical. The physical run-up is constant; the constraint's persistence is cultural.
constraint_indexing:constraint_classification(modernization_defection_gradient, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(modernization_defection_gradient_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(modernization_defection_gradient, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(modernization_defection_gradient, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(modernization_defection_gradient, TR),
    TR >= 0.70.

:- end_tests(modernization_defection_gradient_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts obedience from residents (restricted development), but the extraction is not severe because it offers genuine coordination value—preventing catastrophic loss of life and property. The upward drift (0.28 → 0.38) reflects increasing modernization pressure: as development opportunities expanded post-1960, the opportunity cost of restricted land use rose. Suppression (0.68): Moderately high. Multiple suppression mechanisms: (1) social enforcement ('listen to your elders'), (2) municipal zoning/heritage designation (legal barriers), (3) modernization narrative ('tradition is superstition'), (4) exit costs (leaving the community is costly), (5) economic dependency (limited alternative livelihood outside coastal zone). However, suppression is not total—residents are not physically imprisoned; they can and do leave. Theater ratio (0.44): Moderate, rising. The constraint's mechanism shifted from functional deference (living practice) to ceremonial maintenance (heritage symbol). In 1933, the stone's power was primarily functional—recent survivors and their descendants heeded a warning they understood viscerally. By 2011, most residents had never experienced a major tsunami; compliance had become a social and heritage practice. Post-2011 institutionalization of the stone as a 'lesson in traditional knowledge' raised theater ratio further—the constraint is increasingly maintained for its symbolic value as a 'validation of indigenous wisdom' rather than as a living practice.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces a stark perspectival divergence. The local resident (powerless/trapped) experiences pure extraction—restricted land use with no exit. The regional developer (moderate/constrained) experiences tangled rope—genuine regional-stability coordination alongside extraction. The institutional memory holder (institutional/arbitrage) experiences pure coordination—they benefit from the constraint by maintaining the legitimacy of tradition. The post-2011 coalition (organized/constrained) sees the constraint as temporary scaffolding—technology and science will replace deference. The heritage system (institutional/arbitrage) sees ceremonial persistence—the stone no longer governs behavior, but institutional designation maintains it. The analytical observer risks seeing immutable natural law—the tsunami run-up is physically determined. The perspectival gaps expose that different actors are experiencing structurally different constraints layered on the same physical phenomenon. The resident is experiencing a social restriction; the developer is experiencing coordination; the institution is experiencing legitimacy maintenance; the analyst is risking the false summit error.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are institutional memory holders (municipalities, heritage institutions, government agencies maintaining jishin kitai bunka). They benefit from the constraint because it establishes institutional legitimacy through demonstrable historical wisdom and validates the cultural-transmission system itself. Victims are residents facing restricted land use and modernization-sector actors pressuring for development. The constraint's directionality is institutional: beneficiaries have high institutional power and arbitrage options (they can drop the constraint anytime); victims have low power and trapped/constrained exit. This produces high d for victims (~0.90) and low d for beneficiaries (~0.15), feeding the tangled rope classification: genuine coordination (tsunami risk prevention) alongside institutional extraction (land-use control maintaining authority legitimacy).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in this constraint is structural: it persists across perspectives despite the fundamental threat of modernization defection. The constraint avoids mandatrophy through institutional redesign (post-2011 scaffold) that translates traditional memory into formal risk governance, creating a sunset pathway for the old constraint and a succession path for the new one. Without this redesign, the constraint would degrade rapidly as younger generations lose lived cultural understanding of why the stone matters. The piton trajectory is visible in the rising theater_ratio—the constraint's functional mechanism (behavioral compliance through internalized tradition) has attenuated; institutional designation now maintains it. The scaffold perspective (post-2011 coalition) documents the actual institutional response: formal early-warning systems, building codes, and scientific risk modeling are intended to replace deference to tradition. The mandatrophy resolution depends on whether this transition succeeds—if it does, the old constraint becomes a heritage symbol while new risk-governance constraints take functional authority. If it fails (post-reform rollback, loss of institutional commitment to formal risk systems), the constraint will degrade to pure piton (ceremonial persistence) and lose functional capacity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_interpretive_stability,
    'Is the kernel ''do not build below this point'' maintained as a live normative commitment requiring active interpretation, or has it decayed into a heritage symbol requiring only ceremonial maintenance?',
    'Ethnographic documentation of actual land-use decision-making in Aneyoshi and comparable coastal communities post-2011; analysis of municipal zoning records, developer access requests, and community opposition patterns relative to the stone marker.',
    'If live commitment: the constraint is Tangled Rope (coordination + enforcement) maintained through institutional memory. If ceremonial symbol: the constraint is degrading to Piton (inertial persistence). The 2011 validation masks the underlying drift — did compliance happen because of deference to tradition or despite it (developers voluntarily chose safe locations for economic reasons)?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_interpretive_stability, empirical, 'Whether the stone kernel is live institutional practice or commemorative symbol').

omega_variable(
    modernization_narrative_dominance,
    'What fraction of coastal Japanese communities have explicitly rejected historical risk-marker constraints in favor of modernized risk governance (seismic monitoring, building codes, early warning systems), and how does this rejection relate to the constraint''s extractiveness?',
    'Comparative study of communities with historical risk stones vs. those without; correlation between institutional adoption of scientific risk governance and defection from traditional constraint enforcement.',
    'If high rejection rate: modernization constitutes structural defection pressure; extractiveness may be underestimated. The constraint persists in Aneyoshi due to accident (the stone''s prediction held, validating tradition) rather than systematic transmission of institutional memory.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(modernization_narrative_dominance, empirical, 'Rate of modernization-driven defection from historical risk constraints').

omega_variable(
    intergenerational_identity_lock,
    'Are residents who maintain compliance with the stone-marker constraint doing so because they have internalized the institutional memory logic (''this is how our community thinks''), or because external barriers (municipal zoning, heritage designation, social sanctioning) make defection costly?',
    'Interview data on residents'' actual justifications for land-use compliance; analysis of voluntary compliance patterns in communities with stone markers (no zoning) vs. formal zoning (legal mandate).',
    'If identity-locked: the constraint operates through internalized deference-to-tradition; loss of cultural transmission would cause rapid degradation (Piton trajectory). If externally enforced: the constraint persists through institutional machinery (municipal governance) that is decoupled from lived cultural understanding; more structurally stable but theatrically dependent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_identity_lock, empirical, 'Whether stone compliance reflects identity-locked deference or institutional enforcement').

omega_variable(
    false_summit_natural_law_risk,
    'Does the analytical observer''s mountain classification reflect genuine immutability (the tsunami run-up is physically determined) or naturalize a contingent institutional arrangement (the constraint persists because of institutional memory, not physical law)?',
    'Decoupling test: if institutional memory systems fail (community dispersal, heritage designation dropped, modernization narrative dominates), does the physical run-up change? If constraint persistence is purely institutional, decoupling those systems should not affect physical outcomes, but the constraint itself should degrade.',
    'If genuine mountain: constraint persistence is immutable; institutional memory is merely one mechanism preserving compliance. If false summit: constraint is social (Tangled Rope/Piton depending on transmission mechanism); appears immutable only because it aligns with physical reality by accident.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_risk, conceptual, 'Whether mountain classification reflects immutable natural law or naturalized institutional contingency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(modernization_defection_gradient, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(moddef_theater_1933, modernization_defection_gradient, theater_ratio, 0, 0.15).
narrative_ontology:measurement(moddef_theater_1973, modernization_defection_gradient, theater_ratio, 40, 0.32).
narrative_ontology:measurement(moddef_theater_2011, modernization_defection_gradient, theater_ratio, 78, 0.44).
narrative_ontology:measurement(moddef_theater_1938, modernization_defection_gradient, theater_ratio, 5, 0.18).
narrative_ontology:measurement(moddef_theater_1983, modernization_defection_gradient, theater_ratio, 50, 0.38).

% Extraction over time
narrative_ontology:measurement(moddef_extractiveness_1933, modernization_defection_gradient, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(moddef_extractiveness_1973, modernization_defection_gradient, base_extractiveness, 40, 0.35).
narrative_ontology:measurement(moddef_extractiveness_2011, modernization_defection_gradient, base_extractiveness, 78, 0.38).
narrative_ontology:measurement(moddef_extractiveness_1938, modernization_defection_gradient, base_extractiveness, 5, 0.29).
narrative_ontology:measurement(moddef_extractiveness_1983, modernization_defection_gradient, base_extractiveness, 50, 0.36).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(modernization_defection_gradient, identity_coordination).
narrative_ontology:affects_constraint(modernization_defection_gradient, institutional_memory_transmission).
narrative_ontology:affects_constraint(modernization_defection_gradient, indigenous_knowledge_legitimacy).
narrative_ontology:affects_constraint(modernization_defection_gradient, coastal_settlement_zoning_constraints).

% DUAL FORMULATION NOTE:
% The modernization defection gradient describes the institutional maintenance of a single constraint (do not build below the stone) across modernization pressure. Upstream constraints include institutional memory transmission systems (jishin kitai bunka) and the legitimacy claims grounded in indigenous knowledge. Downstream constraints include coastal zoning regimes and post-2011 formal risk-governance systems that inherited the stone's functional role.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(modernization_defection_gradient, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
