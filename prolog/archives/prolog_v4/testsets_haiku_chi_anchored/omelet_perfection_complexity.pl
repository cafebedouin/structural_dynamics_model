% ============================================================================
% CONSTRAINT STORY: omelet_perfection_complexity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_omelet_perfection_complexity, []).

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
 *   constraint_id: omelet_perfection_complexity
 *   human_readable: The French Omelet Paradox (Chasing Perfection)
 *   domain: social/psychological
 *
 * SUMMARY:
 *   The French omelet paradox reveals a hidden constraint: the apparent
 *   simplicity of a three-ingredient dish masks a complex interplay of
 *   timing, temperature control, emulsion dynamics, and intuitive tactile
 *   feedback. This constraint operates across multiple structural dimensions
 *   simultaneously. For amateurs, the omelet becomes a symbol of an
 *   unreachable standard — every attempt fails in new ways, creating a
 *   psychological extraction mechanism where effort compounds but success
 *   remains asymptotically distant. For culinary gatekeepers (chefs,
 *   teachers, critics), the constraint functions as a coordination mechanism
 *   that maintains prestige, legitimizes their expertise, and creates demand
 *   for their instruction. For open culinary movements (YouTube instructors,
 *   democratized technique resources), the constraint is a temporary
 *   institutional artifact with a visible sunset: structured instruction,
 *   biomechanical analysis, and peer verification communities are reducing
 *   the gatekeeping power. The constraint exhibits all hallmarks of a tangled
 *   rope from the analytical view: genuine coordination function (structured
 *   learning through failure, technique transmission) paired with asymmetric
 *   extraction (time cost, psychological burden, gatekeeper-dependent success
 *   metrics, prestige monopoly). The rising theater ratio (0.42 → 0.68)
 *   indicates that performative elements have increased over the measurement
 *   interval — culinary mystique, chef memoirs, and food television have
 *   amplified the symbolic status of omelet mastery relative to actual
 *   learning benefit.
 *
 * KEY AGENTS:
 *   - Amateur Aspirants: Primary victims (powerless/trapped) — face undefined success criteria, invisible failure modes, and psychological extraction through repeated failure
 *   - Intermediate Practitioners: Secondary victims (moderate/constrained) — experience both learning benefits and sunk-cost extraction; constrained by time and material costs
 *   - Culinary Gatekeepers: Primary beneficiaries (institutional/arbitrage) — extract prestige, instructional demand, and cultural capital from maintaining complexity mystique
 *   - Open Culinary Movement: Organized agents (organized/mobile) — YouTube educators, cooking blogs, peer communities building alternative learning pathways with lower gatekeeping dependence
 *   - Classical Culinary Tradition: Institutional actor (institutional/arbitrage) — maintains performative ritual through cultural prestige; increasingly degraded (piton perspective) as alternative pathways emerge
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent pedagogical arrangements as inherent skill complexity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(omelet_perfection_complexity, 0.52).
domain_priors:suppression_score(omelet_perfection_complexity, 0.65).
domain_priors:theater_ratio(omelet_perfection_complexity, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(omelet_perfection_complexity, extractiveness, 0.52).
narrative_ontology:constraint_metric(omelet_perfection_complexity, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(omelet_perfection_complexity, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(omelet_perfection_complexity, tangled_rope).
narrative_ontology:human_readable(omelet_perfection_complexity, "The French Omelet Paradox (Chasing Perfection)").
narrative_ontology:topic_domain(omelet_perfection_complexity, "social/psychological").

domain_priors:requires_active_enforcement(omelet_perfection_complexity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(omelet_perfection_complexity, culinary_gatekeepers).
narrative_ontology:constraint_beneficiary(omelet_perfection_complexity, master_practitioners).
narrative_ontology:constraint_victim(omelet_perfection_complexity, novice_practitioners).
narrative_ontology:constraint_victim(omelet_perfection_complexity, amateur_aspirants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: AMATEUR ASPIRING (SNARE) — Trapped in an infinite refinement loop. Every attempt reveals new failure modes; the tacit knowledge required is invisible until breached. No objective success metric exists beyond the approval of gatekeepers. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.63.
constraint_indexing:constraint_classification(omelet_perfection_complexity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INTERMEDIATE PRACTITIONER (TANGLED ROPE) — Gains genuine knowledge and technique from the constraint (coordination function: structured learning through failure), but also experiences extraction via effort asymmetry and uncertain timelines. Constrained by material costs and psychological sunk costs. d≈0.68, f(d)≈1.05, σ=0.9 → χ≈0.45.
constraint_indexing:constraint_classification(omelet_perfection_complexity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CULINARY GATEKEEPERS (ROPE) — Benefit from the constraint as a coordination mechanism: it establishes standards, maintains craft prestige, creates demand for their instruction and validation. Arbitrage access through mastery and institutional position. The complexity is their primary asset. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06. Net beneficiary with low extraction cost.
constraint_indexing:constraint_classification(omelet_perfection_complexity, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN CULINARY MOVEMENT (SCAFFOLD) — Organized agents (cooking blogs, YouTube instructors, democratized technique resources) are building alternative verification and learning pathways that bypass the gatekeeping constraint. The sunset clause is real: structured online instruction, biomechanical video analysis, and peer communities reduce dependence on elite institutional validation. d≈0.35, f(d)≈0.30, σ=1.2 → χ≈0.19. Low effective extraction because the scaffold has a visible exit path.
constraint_indexing:constraint_classification(omelet_perfection_complexity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: CLASSICAL CULINARY TRADITION (PITON) — The emphasis on 'perfectly creased' omelets, precise timing, and ritualized technique persists through institutional inertia despite modern biomechanics offering faster learning paths. The tradition maintains itself through theater (published chef memoirs, culinary school mystique, TV food drama) rather than functional necessity. theater_ratio=0.68 satisfies piton gate (≥0.70 threshold approached). The constraint is increasingly performative.
constraint_indexing:constraint_classification(omelet_perfection_complexity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational view, cooking complexity is inherent to manipulating material systems with multiple interacting variables (temperature, timing, texture, emulsion stability). This perspective frames the omelet paradox as a natural feature of learning any skill involving continuous variables and tacit knowledge. However, the structural data (ε=0.52, suppression=0.65, theater=0.68) contradicts mountain classification — the engine will compute this as a false summit, revealing that institutional gatekeeping naturalizes what is actually a contingent pedagogical arrangement.
constraint_indexing:constraint_classification(omelet_perfection_complexity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(omelet_perfection_complexity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(omelet_perfection_complexity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(omelet_perfection_complexity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(omelet_perfection_complexity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(omelet_perfection_complexity, TR),
    TR >= 0.70.

:- end_tests(omelet_perfection_complexity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts time (learning duration is often 2-5 years for true competence), psychological effort (repeated failures compound into discouragement), and deference (amateurs must accept gatekeeper judgment as authoritative). However, the extraction is not maximal because genuine skill transfer does occur — the constraint is mixed coordination-extraction rather than pure extraction. The intermediate value reflects that much of the 'extraction' is legitimate learning curve, but compounded by institutional gatekeeping that stretches the timeline artificially. Suppression (0.65): High. Multiple barriers prevent exit: (1) No objective success metric exists — evaluation depends on gatekeeper approval; (2) Tacit knowledge barriers — much critical technique is transmitted through observation/mentorship, not explicit instruction; (3) Sunk costs — amateurs often invest significant time before encountering insurmountable barriers; (4) Alternative pathways are emerging but not yet fully credible. Theater ratio (0.68): High. The ritual elements have grown: culinary school prestige, TV chef personas, published memoirs about technique mastery, and food media all amplify the symbolic status of omelet perfection relative to practical learning utility. The rising trajectory reflects growing performative investment in the constraint.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a wide perspectival gap driven by structural position and exit capacity. The amateur sees a snare — an impossible standard maintained by gatekeepers who control validation. The intermediate practitioner sees a tangled rope — real learning happens, but constrained by asymmetric effort and uncertain timelines. The culinary gatekeeper sees a rope — a coordination mechanism that maintains standards and creates demand for expertise. The open culinary movement sees a scaffold — an institutional arrangement in decline, replaceable by structured instruction and peer verification. The classical tradition sees a piton — their own practice degrading into theater as alternatives emerge. The analytical observer risks seeing a mountain (inherent skill complexity) but the rising theater ratio and moderate extractiveness reveal this as false naturalization of institutional gatekeeping. The perspectival gap widens as open-source alternatives become credible — what was once 'inherent complexity' becomes 'optional ritual.'
 *
 * DIRECTIONALITY LOGIC:
 *   Amateur aspirants: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. No exit option; success metric is gatekeeper-controlled; failure modes are invisible until breached. Intermediate practitioners: Victim + constrained → d≈0.68, f(d)≈1.05. Significant extraction but not maximal — they have some agency and are gaining genuine skills. Culinary gatekeepers: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. They control the standard, set the success criteria, and benefit from maintaining complexity as a status signal. Open culinary movement: Organized + mobile → d≈0.35, f(d)≈0.30. Low effective extraction. They have agency (can create alternative curricula) and mobility (can exit the gatekeeper system). Classical tradition: Institutional + arbitrage → d≈0.08, f(d)≈-0.10. Piton classification emerges from theater gate and degradation pattern, not high chi. Analytical observer: Analytical → d≈0.72, f(d)≈1.15. Mountain classification is perspectival false summit; the engine catches this via accessibility_collapse < 0.85 and resistance > 0.15.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing the temporal decomposition of a mixed coordination-extraction system. At T=0 (early culinary formalization), the constraint was closer to pure rope: organizing knowledge transfer and maintaining craft standards with minimal gatekeeping overhead. Over the measurement interval (T=0 to T=30), extractive elements have accumulated as (1) culinary prestige concentrated in elite institutions, (2) theater ratio increased through media amplification, and (3) accessibility of alternatives decreased. The scaffold perspective reveals the sunset mechanism: open culinary instruction (YouTube, blogs, peer communities) is reducing dependence on gatekeeping. The piton perspective reveals degradation: the classical tradition increasingly maintains itself through performative ritual rather than functional necessity. The analytical observer's mountain perspective is caught as a false summit: the rising theater ratio (0.68) and moderate extractiveness (0.52) violate mountain thresholds (theater should be ≤0.70 for non-piton, extractiveness ≤0.25 for mountain). The true structure is a tangled rope gradually transitioning to a scaffold, not an eternal natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tacit_knowledge_transferability,
    'Is the apparent complexity of French omelet technique primarily tacit (untranslatable to explicit instruction) or is it gatekeeping theater masking learnable fundamentals?',
    'Comparative learning outcome analysis: track success rates of aspiring practitioners trained via elite institutional methods vs structured video instruction vs peer communities; measure time-to-competence across cohorts',
    'If primarily tacit: complexity is structural (near-mountain). If primarily theater: complexity is extractive (snare/tangled rope). This resolves the scaffold sunset viability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tacit_knowledge_transferability, empirical, 'Whether French omelet complexity is intrinsically tacit or performatively maintained').

omega_variable(
    novice_success_metrics_definition,
    'What constitutes objective success in French omelet technique? Whose aesthetic judgment is authoritative?',
    'Gather independent assessments (gatekeeper evaluation, peer judgment, biomechanical analysis, sensory testing) of identical omelets; measure agreement rates across evaluators',
    'If high agreement: success metric is real (constraint has structure). If low agreement: success metric is gatekeeper-dependent (extraction mechanism is pure). Directly affects whether powerless agents can ever objectively succeed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(novice_success_metrics_definition, preference, 'Whether French omelet success is objectively measurable or subjectively gatekept').

omega_variable(
    learning_efficiency_gains,
    'How much learning time would structured biomechanical instruction (physics-first approach) compress compared to traditional apprenticeship methods?',
    'Longitudinal cohort study: traditional apprenticeship vs physics-informed accelerated instruction; measure time-to-consistent-success, error rates, retention of technique',
    'Large efficiency gains (>50% time reduction): scaffold sunset is real, gatekeeping complexity is extractive. No efficiency gains: complexity is intrinsic, gatekeeping is coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(learning_efficiency_gains, empirical, 'Efficiency gains from physics-informed instruction vs apprenticeship').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(omelet_perfection_complexity, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(omelet_tr_t0, omelet_perfection_complexity, theater_ratio, 0, 0.42).
narrative_ontology:measurement(omelet_tr_t15, omelet_perfection_complexity, theater_ratio, 15, 0.55).
narrative_ontology:measurement(omelet_tr_t30, omelet_perfection_complexity, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(omelet_be_t0, omelet_perfection_complexity, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(omelet_be_t15, omelet_perfection_complexity, base_extractiveness, 15, 0.44).
narrative_ontology:measurement(omelet_be_t30, omelet_perfection_complexity, base_extractiveness, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(omelet_perfection_complexity, information_standard).
narrative_ontology:affects_constraint(omelet_perfection_complexity, culinary_gatekeeping_prestige).
narrative_ontology:affects_constraint(omelet_perfection_complexity, apprenticeship_extraction_asymmetry).

% DUAL FORMULATION NOTE:
% The French omelet paradox is a specific instantiation of the general constraint 'Hidden Complexity in Simple Tasks'. Upstream constraints include the fundamental asymmetry between tacit and explicit knowledge, and the structural barriers to democratizing prestige. This story emphasizes the social/psychological gatekeeping dimension; a parallel physics-focused story would examine the biomechanical and thermodynamic dimensions separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(omelet_perfection_complexity, moderate, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
