% ============================================================================
% CONSTRAINT STORY: westminster_evolution__westminster_export_constitutions
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westminster_evolution__westminster_export_constitutions, []).

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
 *   constraint_id: westminster_evolution__westminster_export_constitutions
 *   human_readable: Westminster Export as Written Constitutional Transplant
 *   domain: political/legal/colonial_inheritance
 *
 * SUMMARY:
 *   The Westminster export constraint represents the transplantation of the
 *   British parliamentary system—originally an evolved, convention-based
 *   framework—into written constitutional form and imposed on post-colonial
 *   societies that had no part in its development. This constraint exhibits
 *   structural extraction despite appearing as coordination: the written
 *   codification provided immediate state-formation capacity and
 *   international legal recognition but simultaneously suppressed
 *   pre-colonial governance forms and locked post-colonial societies into
 *   institutional paths dependent on the former metropole. The constraint
 *   functions differently across temporal and power horizons. At the moment
 *   of independence, successor elites experienced it as coordination and
 *   opportunity—a ready-made system that provided legitimacy without internal
 *   negotiation. Over generations, indigenous governance movements experience
 *   it as entrapment: the Westminster framework's procedures for amendment
 *   and legal challenge were designed in contexts where Westminster itself
 *   was the established baseline, not a contested transplant. The measurement
 *   trajectory shows rising suppression (0.50 → 0.72) as the constitutional
 *   rigidity becomes more entrenched and as post-colonial societies discover
 *   that Westminster's flexibility depended on unwritten conventions that did
 *   not travel. Theater ratio rises (0.55 → 0.68) as parliamentary procedure
 *   becomes increasingly detached from functional deliberation, maintained
 *   through institutional inertia rather than genuine consensus.
 *
 * KEY AGENTS:
 *   - Displaced Indigenous Governance Communities: Primary victims (powerless/trapped) — pre-colonial institutions structurally erased; cannot exit Westminster framework
 *   - Successor Elite (Post-Colonial Government): Primary beneficiary (institutional/arbitrage) — inherits functional state apparatus and international legitimacy; net beneficiary
 *   - Post-Independence Indigenous Movements: Secondary agent (organized/constrained) — organized opposition to transplant; constrained by Westminster amendment procedures but enabled by Westminster rights protections
 *   - International Legal Order (Commonwealth, UN, Trade Partners): Institutional beneficiary (institutional/constrained) — gains coordination benefits from standardized post-colonial legal systems; constrains post-colonial states through path dependence on metropolitan recognition
 *   - Westminster Constitutional Architecture (Formalized System): Institutional actor (institutional/constrained) — degrades from evolved convention-based practice into rigid procedural theater
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent historical choice as structural inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westminster_evolution__westminster_export_constitutions, 0.58).
domain_priors:suppression_score(westminster_evolution__westminster_export_constitutions, 0.72).
domain_priors:theater_ratio(westminster_evolution__westminster_export_constitutions, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westminster_evolution__westminster_export_constitutions, extractiveness, 0.58).
narrative_ontology:constraint_metric(westminster_evolution__westminster_export_constitutions, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(westminster_evolution__westminster_export_constitutions, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westminster_evolution__westminster_export_constitutions, tangled_rope).
narrative_ontology:human_readable(westminster_evolution__westminster_export_constitutions, "Westminster Export as Written Constitutional Transplant").
narrative_ontology:topic_domain(westminster_evolution__westminster_export_constitutions, "political/legal/colonial_inheritance").

domain_priors:requires_active_enforcement(westminster_evolution__westminster_export_constitutions).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westminster_evolution__westminster_export_constitutions, '504a2506-c577-46d2-ac22-57bd8d2f0cda').
narrative_ontology:cs_kernel_codification('504a2506-c577-46d2-ac22-57bd8d2f0cda', fixed_text).
narrative_ontology:cs_authority_grounding('504a2506-c577-46d2-ac22-57bd8d2f0cda', extraction).
narrative_ontology:cs_interpretation_layer_present('504a2506-c577-46d2-ac22-57bd8d2f0cda').
narrative_ontology:cs_reading_relation('504a2506-c577-46d2-ac22-57bd8d2f0cda', westminster_evolution__british_constitution, coexists_with).
narrative_ontology:cs_axiom('504a2506-c577-46d2-ac22-57bd8d2f0cda', foundational, unwritten_practice_codifiable).
narrative_ontology:cs_axiom_status(unwritten_practice_codifiable, holdable).
narrative_ontology:cs_axiom_grounding('504a2506-c577-46d2-ac22-57bd8d2f0cda', unwritten_practice_codifiable, deontological).
narrative_ontology:cs_axiom('504a2506-c577-46d2-ac22-57bd8d2f0cda', foundational, transplant_suppresses_alternatives).
narrative_ontology:cs_axiom_status(transplant_suppresses_alternatives, holdable).
narrative_ontology:cs_axiom_grounding('504a2506-c577-46d2-ac22-57bd8d2f0cda', transplant_suppresses_alternatives, empirically_contingent).
narrative_ontology:cs_reference_frame('504a2506-c577-46d2-ac22-57bd8d2f0cda', universal_codified_parliamentary_legitimacy).
narrative_ontology:cs_drift_state('504a2506-c577-46d2-ac22-57bd8d2f0cda', contemporary_indigenous_assertion_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('504a2506-c577-46d2-ac22-57bd8d2f0cda', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(westminster_evolution__westminster_export_constitutions, westminster_evolution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westminster_evolution__westminster_export_constitutions, successor_elite).
narrative_ontology:constraint_beneficiary(westminster_evolution__westminster_export_constitutions, former_colonial_administration).
narrative_ontology:constraint_victim(westminster_evolution__westminster_export_constitutions, displaced_indigenous_governance).
narrative_ontology:constraint_victim(westminster_evolution__westminster_export_constitutions, post_colonial_legitimacy_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED INDIGENOUS GOVERNANCE (SNARE) — Communities whose pre-colonial institutions (clan councils, consensus systems, land-based authority, customary law) were structurally erased by Westminster transplant. Trapped by the colonial state's monopoly on legal recognition; cannot revive displaced systems without challenging the entire constitutional framework. Maximum suppression — alternatives are not merely costly but delegitimized as 'informal' or 'tribal.' No exit within the Westminster framework.
constraint_indexing:constraint_classification(westminster_evolution__westminster_export_constitutions, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: POST-INDEPENDENCE INDIGENOUS MOVEMENTS (TANGLED ROPE) — Organized groups seeking constitutional recognition of pre-colonial governance. Constrained by the transplanted constitution's entrenchment of Westminster procedures; amendment requires approval within a framework designed to resist their claims. But also partly enabled by Westminster's own recognition of rights, participation mechanisms, and courts — the transplant provides tools for challenging its own structure. Mixed coordination (the constitution does stabilize a legible state apparatus) and extraction (it locks in the delegitimization of alternatives).
constraint_indexing:constraint_classification(westminster_evolution__westminster_export_constitutions, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SUCCESSOR ELITE / POST-COLONIAL GOVERNMENT (ROPE) — Inherits the Westminster framework as a fully functional, internationally recognized system. The written constitution provides immediate legitimacy, procedural clarity, and access to the global order. Experiences the transplant as a coordination solution: it solved the problem of state formation without indigenous negotiation. Can arbitrage between Westminster legitimacy and local power bases. Net beneficiary — the constraint enables their authority.
constraint_indexing:constraint_classification(westminster_evolution__westminster_export_constitutions, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERNATIONAL LEGAL ORDER (TANGLED ROPE) — The Westminster export creates coordination among post-colonial states and the former metropole: standardized legal procedures, mutual recognition, trade-law compatibility. But it also entraps post-colonial states — deviation from Westminster norms risks exclusion from the global order. Constrained by path dependence; benefits from coordination; extraction runs through the dependence on metropolitan-aligned institutions for recognition and capital access.
constraint_indexing:constraint_classification(westminster_evolution__westminster_export_constitutions, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: WESTMINSTER CONSTITUTIONAL ARCHITECTURE (PITON) — The written codification itself is now a degraded institutional form. The unwritten flexibility of the British constitution depended on shared cultural baseline and evolutionary adaptation. The written export froze parliamentary practice as formal rules, losing the adaptive capacity of convention. Post-colonial parliaments apply Westminster procedures rigidly in contexts where the underlying consensus infrastructure does not exist. Theater-heavy: formal voting procedures substitute for genuine deliberation in societies lacking Westminster's embedded trust conventions. Maintained through institutional inertia and the lack of domesticated alternatives.
constraint_indexing:constraint_classification(westminster_evolution__westminster_export_constitutions, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / STRUCTURAL INEVITABILITY (MOUNTAIN) — From a universal/civilizational perspective, the transplant appears as an inevitable consequence of colonial state formation: any rapid state-building requires codified law, and the metropole's proven system is the available template. This perspective treats Westminster export as a natural law of decolonization — no society emerging from colonialism could have avoided it. However, the structural data contradicts this: the extraction, suppression, and beneficiary concentration reveal a contingent choice, not an inherent structural necessity. The analytical observer is at risk of naturalizing a historical contingency.
constraint_indexing:constraint_classification(westminster_evolution__westminster_export_constitutions, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westminster_evolution__westminster_export_constitutions_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(westminster_evolution__westminster_export_constitutions, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(westminster_evolution__westminster_export_constitutions, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(westminster_evolution__westminster_export_constitutions, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(westminster_evolution__westminster_export_constitutions, TR),
    TR >= 0.70.

:- end_tests(westminster_evolution__westminster_export_constitutions_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The transplant extracts through multiple mechanisms: (1) suppression of pre-colonial governance alternatives, which benefits the successor elite by eliminating power rivals and the former metropole by maintaining institutional continuity; (2) path dependence on metropolitan recognition and capital access, which constrains post-colonial states through the international legal order; (3) theft of time and institutional capacity from post-colonial societies forced to operate Westminster procedures without the underlying consensus infrastructure. The extraction is not maximal (0.72+) because Westminster also provides genuine coordination benefits: a stable state apparatus, legal recourse mechanisms, and bureaucratic capacity that post-colonial societies did benefit from. Suppression (0.72): High. Multiple suppression mechanisms operate: (1) legal delegitimization of pre-colonial governance forms as 'informal,' 'tribal,' or 'traditional'; (2) monopoly on legal recognition by Westminster structures; (3) procedural barriers to constitutional amendment that make restoring alternatives extremely costly; (4) international pressure for Westminster compliance through trade, capital access, and diplomatic recognition. These are structural, not merely cultural—they are written into law. Theater ratio (0.68): High-moderate. Westminster parliamentary procedure in post-colonial contexts often becomes performative because it lacks the underlying consensus infrastructure Westminster evolved within. Formal voting procedures substitute for genuine deliberation; question time becomes ritual rather than accountability; committee work proceeds without the shared assumptions that make Westminster deliberation functional. The theater has increased over the interval (0.55 → 0.68) as the rigidity of written rules becomes more apparent and as post-colonial societies discover that Westminster's flexibility came from unwritten convention, not from the procedures themselves.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates radical perspectival divergence. The successor elite experiences Westminster export as coordination and opportunity (Rope)—they inherited a functional system that provided immediate legitimacy and state capacity. Displaced indigenous communities experience it as entrapment with no exit (Snare)—their institutions are not merely subordinated but legally delegitimized. Post-independence indigenous movements experience mixed constraint and opportunity (Tangled Rope)—Westminster's own legal procedures and rights protections enable challenges to Westminster's structure, creating a paradox where the transplanted system provides tools for its own contestation. The international legal order experiences coordination benefits and mild constraint (Tangled Rope)—standardized legal systems enable global commerce and mutual recognition but also lock post-colonial states into dependence on metropolitan alignment. The Westminster constitutional architecture itself has degraded into theater (Piton)—the procedures persist through inertia, not function. The analytical observer risks naturalizing this entire arrangement as an inevitable feature of decolonization (Mountain), when the structural data reveals it as a contingent choice enabled by colonial power asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is computed from the agent's structural position relative to extraction flow. Displaced indigenous communities are trapped victims with zero agency—d approaches 1.0, producing maximum experienced extractiveness. Successor elites are beneficiaries with arbitrage options—d approaches 0.0 to 0.15, producing negative or near-zero effective extraction from their perspective. Organized indigenous movements are constrained victims with some agency—d ≈ 0.65–0.75, producing high but not maximal extraction. The international legal order benefits from coordination but constrains post-colonial states—d is mixed (0.45–0.55 depending on whether measurement is from metropole or post-colonial perspective). The Westminster architecture itself experiences constrained institutional degradation—d ≈ 0.55. The analytical observer's d is derived from pure observation position without structural benefit or cost—canonical d for analytical power ≈ 0.73, producing moderate experienced extractiveness from an external vantage point.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    codification_necessity_vs_contingency,
    'Was the written codification of Westminster practice a structural necessity for post-colonial state formation, or a contingent choice that enabled suppression of alternatives?',
    'Comparative analysis of post-colonial constitutions that diverged from Westminster (e.g., India''s incorporation of customary law, Malaysia''s recognition of traditional sultanates, Botswana''s kgotla structures); identification of whether divergent models faced legitimacy deficits or external pressure.',
    'If necessity: the constraint is closer to mountain (structural inevitability of decolonization). If contingency: the constraint is snare-class extraction (deliberate suppression of alternatives was possible but avoided).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(codification_necessity_vs_contingency, empirical, 'Whether Westminster codification was structurally necessary or a contingent choice').

omega_variable(
    displaced_governance_restoration_feasibility,
    'Could pre-colonial governance structures be legally and institutionally restored within a post-colonial Westminster framework, or does the transplant''s structural logic make restoration impossible?',
    'Legal and institutional analysis of constitutional amendment pathways; documentation of attempted restorations (e.g., Maori governance in New Zealand, First Nations in Canada, tribal governance in US); assessment of whether Westminster tools enabled or blocked restoration.',
    'If restorable: exit options for displaced communities move from trapped to constrained (paths exist, though costly). If impossible: the snare classification is confirmed — Westminster structure locks in the displacement irreversibly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(displaced_governance_restoration_feasibility, empirical, 'Whether Westminster framework can legally accommodate restored pre-colonial governance').

omega_variable(
    unwritten_vs_written_cultural_substrate,
    'Does the conversion of Westminster''s unwritten flexibility into written rules necessarily degrade institutional function, or can written Westminster be equally adaptive in post-colonial contexts?',
    'Longitudinal analysis of parliamentary function in Westminster systems: comparison of amendment rates, constitutional crisis resolution, adaptive capacity in written vs unwritten Westminster democracies; assessment of whether written codification increased or decreased institutional responsiveness.',
    'If written Westminster is equally adaptive: piton classification is overstated — the system retains functional capacity. If written Westminster is systematically less adaptive: piton classification is confirmed — institutional inertia and theater ratio increase with codification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unwritten_vs_written_cultural_substrate, empirical, 'Whether written Westminster rules degrade institutional flexibility relative to unwritten precedent').

omega_variable(
    reading_kernel_distinction,
    'Is this reading (Westminster export) distinct from the sibling reading (British constitution) in a way that instantiates different constraints, or are they the same constraint viewed from different angles?',
    'Structural analysis: if the two readings have different ε values, different victim sets, different suppression mechanisms, or different beneficiary structures, they are distinct constraints. If they have the same structural properties but different normative framings, they are the same constraint with different evaluators.',
    'If distinct: both constraints should be authored separately as constraint stories linked by network.affects_constraints. If same: only one story needed, with perspectives representing different evaluations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_distinction, conceptual, 'Whether Westminster export and British constitution are distinct constraints or perspectival readings of one constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westminster_evolution__westminster_export_constitutions, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_export_theater_t0, westminster_evolution__westminster_export_constitutions, theater_ratio, 0, 0.55).
narrative_ontology:measurement(west_export_theater_t25, westminster_evolution__westminster_export_constitutions, theater_ratio, 25, 0.62).
narrative_ontology:measurement(west_export_theater_t50, westminster_evolution__westminster_export_constitutions, theater_ratio, 50, 0.68).

% Extraction over time
narrative_ontology:measurement(west_export_extract_t0, westminster_evolution__westminster_export_constitutions, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(west_export_extract_t25, westminster_evolution__westminster_export_constitutions, base_extractiveness, 25, 0.52).
narrative_ontology:measurement(west_export_extract_t50, westminster_evolution__westminster_export_constitutions, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(west_export_suppression_t0, westminster_evolution__westminster_export_constitutions, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(west_export_suppression_t25, westminster_evolution__westminster_export_constitutions, suppression_requirement, 25, 0.65).
narrative_ontology:measurement(west_export_suppression_t50, westminster_evolution__westminster_export_constitutions, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westminster_evolution__westminster_export_constitutions, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(westminster_evolution__westminster_export_constitutions, 0.18).
narrative_ontology:affects_constraint(westminster_evolution__westminster_export_constitutions, british_constitution).
narrative_ontology:affects_constraint(westminster_evolution__westminster_export_constitutions, post_colonial_legal_dependency).
narrative_ontology:affects_constraint(westminster_evolution__westminster_export_constitutions, indigenous_governance_delegitimization).

% DUAL FORMULATION NOTE:
% Westminster export constitutions form a constraint family with the British constitution (the source reading) and post-colonial legal dependency structures (downstream institutional entrapment). The export constraint has ε ≈ 0.58 (mixed coordination-extraction); the British constitution reading has lower ε due to absence of suppression mechanisms and victims; post-colonial legal dependency has higher ε (0.65+) due to compounding path dependence. Each story has its own beneficiary/victim structure and is authored separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westminster_evolution__westminster_export_constitutions, institutional, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
