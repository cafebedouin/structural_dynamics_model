% ============================================================================
% CONSTRAINT STORY: state_commitment_installation_mechanism__endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_commitment_installation_mechanism__endogenous_climb_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: state_commitment_installation_mechanism__endogenous_climb_reading
 *   human_readable: Endogenous Climb: Fringe-to-Apex Legitimacy Installation
 *   domain: political/historical/institutional
 *
 * SUMMARY:
 *   This constraint describes the mechanism by which new institutional
 *   commitments — a novel form of governance, a revised constitutional
 *   interpretation, a reformed bureaucratic structure, an intellectual
 *   paradigm — gain legitimacy by demonstrating superiority at the
 *   institutional periphery before being adopted by apex authorities. The
 *   mechanism is endogenous: the climb succeeds because the innovation works,
 *   and because early adopters inside the apex structure recognize and
 *   champion it. The institutional world does not rearrange itself if this
 *   mechanism vanishes — but institutional innovation would stall or
 *   destabilize, forcing institutions toward either defensive stagnation or
 *   periodic rupture. This is ONE reading of a contested kernel
 *   (state_commitment_installation_mechanism). Two sibling readings —
 *   exogenous_imposition_reading and hybrid_cascade_reading — describe
 *   alternative mechanisms by which new commitments gain institutional
 *   legitimacy. This story describes only the endogenous climb; the siblings
 *   are different constraints with their own ε values, beneficiary
 *   structures, and classifications.
 *
 * KEY AGENTS:
 *   - Fringe institutional actors: innovators at the margins of established institutions who develop and advocate for alternative commitments.
 *   - Early adopter elites: influential figures within apex institutions who perceive superiority early and champion adoption from within the ruling structure.
 *   - Advocacy communities: coalitions of practitioners and intellectuals who develop the fringe commitment's evidence base and mount campaigns for adoption.
 *   - Apex institutional authorities: formal authority holders (legislatures, executive branches, senior courts) who eventually adopt the fringe commitment.
 *   - Apex institutional incumbents: persons whose authority depends on the prior commitment structure and who pay the climb cost through diminished status.
 *   - Competing fringe movements: alternative innovations that lose patronage and institutional attention as the winning fringe commitment climbs.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_commitment_installation_mechanism__endogenous_climb_reading, 0.28).
domain_priors:suppression_score(state_commitment_installation_mechanism__endogenous_climb_reading, 0.22).
domain_priors:theater_ratio(state_commitment_installation_mechanism__endogenous_climb_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_commitment_installation_mechanism__endogenous_climb_reading, rope).
narrative_ontology:human_readable(state_commitment_installation_mechanism__endogenous_climb_reading, "Endogenous Climb: Fringe-to-Apex Legitimacy Installation").
narrative_ontology:topic_domain(state_commitment_installation_mechanism__endogenous_climb_reading, "political/historical/institutional").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_commitment_installation_mechanism__endogenous_climb_reading, '2046c7ff-de56-4251-96a4-8a7e485b3e84').
narrative_ontology:cs_kernel_codification('2046c7ff-de56-4251-96a4-8a7e485b3e84', distributed).
narrative_ontology:cs_authority_grounding('2046c7ff-de56-4251-96a4-8a7e485b3e84', expertise).
narrative_ontology:cs_interpretation_layer_present('2046c7ff-de56-4251-96a4-8a7e485b3e84').
narrative_ontology:cs_reading_relation('2046c7ff-de56-4251-96a4-8a7e485b3e84', state_commitment_installation_mechanism__exogenous_imposition_reading, coexists_with).
narrative_ontology:cs_reading_relation('2046c7ff-de56-4251-96a4-8a7e485b3e84', state_commitment_installation_mechanism__hybrid_cascade_reading, influences).
narrative_ontology:cs_axiom('2046c7ff-de56-4251-96a4-8a7e485b3e84', foundational, demonstrated_superiority_generates_legitimate_pressure).
narrative_ontology:cs_axiom_status(demonstrated_superiority_generates_legitimate_pressure, holdable).
narrative_ontology:cs_axiom_grounding('2046c7ff-de56-4251-96a4-8a7e485b3e84', demonstrated_superiority_generates_legitimate_pressure, empirically_contingent).
narrative_ontology:cs_axiom('2046c7ff-de56-4251-96a4-8a7e485b3e84', foundational, fringe_actors_are_primary_legitimacy_sources).
narrative_ontology:cs_axiom_status(fringe_actors_are_primary_legitimacy_sources, holdable).
narrative_ontology:cs_axiom_grounding('2046c7ff-de56-4251-96a4-8a7e485b3e84', fringe_actors_are_primary_legitimacy_sources, empirically_contingent).
narrative_ontology:cs_reference_frame('2046c7ff-de56-4251-96a4-8a7e485b3e84', institutional_hierarchy_with_gatekeeping).
narrative_ontology:cs_drift_state('2046c7ff-de56-4251-96a4-8a7e485b3e84', contemporary_institutional_learning, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2046c7ff-de56-4251-96a4-8a7e485b3e84', '').
narrative_ontology:cs_kernel_id(state_commitment_installation_mechanism__endogenous_climb_reading, state_commitment_installation_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__endogenous_climb_reading, fringe_institutional_actors).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__endogenous_climb_reading, early_adopter_elites).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__endogenous_climb_reading, advocacy_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__endogenous_climb_reading, apex_institutional_authorities).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__endogenous_climb_reading, apex_institutional_incumbents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operators at the margins of established institutions — think-tanks, reform movements, experimental governance pilots, dissenting intellectual schools — who develop and advocate for alternative commitments before apex authorities recognize them. They benefit from the climb trajectory because their ideas gain institutional purchase and resources as superiority becomes recognized. Their exit is relatively mobile: they can shift attention to other innovations if the current one stalls.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, fringe_institutional_actors, beneficiary,
    moderate, generational, mobile, national).

% Influential figures within apex institutions who perceive the fringe commitment's superiority early and champion adoption from within the ruling structure. They gain reputational capital as architects of institutional improvement and often gain operational authority over the new commitment as it scales. They have exit options: they can shift their patronage to other innovations or revert to defending the status quo if political costs rise.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, early_adopter_elites, beneficiary,
    powerful, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(state_commitment_installation_mechanism__endogenous_climb_reading, early_adopter_elites, agenda_setter).

% Coalitions of practitioners, intellectuals, and reform advocates who develop the fringe commitment's evidence base and mount campaigns for adoption. They benefit through increased institutional legitimacy for their ideas, access to funding and platforms, and the validation that comes when apex institutions endorse their work. Their exit is constrained: the movement's success is often bound to their professional identity.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, advocacy_communities, beneficiary,
    organized, biographical, constrained, national).

% Holders of formal authority — legislatures, executive branches, senior courts, established churches — who eventually adopt the fringe commitment as it demonstrates superiority. They pay a cost: operational reorganization, the surrender of control over the commitment's initial design phase (fringe actors set the pattern), and the reputational vulnerability that follows if the adopted commitment later fails. They are constrained in exit: abandoning a commitment they have formally adopted and scaled carries high political cost.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, apex_institutional_authorities, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(state_commitment_installation_mechanism__endogenous_climb_reading, apex_institutional_authorities, agenda_setter).

% Alternative innovations at the fringe that do not climb — they lose patronage and institutional attention as apex authorities concentrate resources on the winning fringe commitment. They remain structurally outside the climb process itself; their voices are absent from the adoption debate even though they represent viable alternatives.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, competing_fringe_movements, excluded,
    moderate, biographical, mobile, national).

% Persons and factions within apex institutions whose authority, power, or legacy status depends on the institutions' prior commitment structure. They pay the climb cost through diminished status, operational complexity, and the subordination of their preferred order to the newly legitimized commitment. Their exit is constrained: open resistance to adoption can mark them as obstructing progress.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, apex_institutional_incumbents, payer,
    powerful, biographical, constrained, national).

% Sees the constraint from outside any institutional seat — observes how the climb works structurally, what legitimacy mechanisms operate, and how the endogenous reading differs from imposed or hybrid cascades. Takes no direct stake; performs measurement and classification.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, historical_analyst, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_commitment_installation_mechanism__endogenous_climb_reading, fringe_institutional_actors).
narrative_ontology:fixing_cost_class(state_commitment_installation_mechanism__endogenous_climb_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the institutional innovation adoption problem: when a fringe-developed commitment demonstrably works better than the apex's current arrangement, the climb provides a legitimacy pathway that avoids both the chaos of sudden overthrow and the stagnation of permanent rejection. The process coordinates on the new commitment's superiority without requiring apex authorities to yield ex-ante control over design.
% TRANSFER_FUNCTION: Moves intellectual capital, reputational authority, and operational control from the apex's defensive position toward the fringe actors and early adopters who championed the commitment during its climb. The apex retains formal authority over implementation but yields de facto control of the commitment's initial parameters and scaling strategy.
% ABSENT_VOICES: Competing fringe movements whose alternative commitments lose patronage and institutional attention as resources flow toward the winning innovation. Also absent: citizens or constituencies harmed by the prior commitment who were not part of the advocacy coalition that engineered the climb — they benefit incidentally but are not in the room where the climb's terms were negotiated.
% DISAPPEARANCE_RATIONALE: If the endogenous climb mechanism vanished, institutional innovation would either stall (apex actors defend status quo against all fringe challenge) or destabilize (challengers must resort to rupture rather than demonstration). The institutional world reorganizes around whichever adoption mechanism fills the void — likely toward exogenous imposition or periodic crisis-driven cascade.
% FOUNDING_PROBLEM: Institutions resist change because they embed power distributions that favor incumbents; but they also fail catastrophically when their inherited commitments no longer work. How can institutions absorb innovation from below without ceding all control, and how can fringe actors prove superiority without the resources apex institutions command? The endogenous climb solves this: demonstrated performance over time, combined with inside champions, creates legitimate pressure that converts without rupture.
% FOUNDING_PROBLEM_CORROBORATION: Historical sociologists (Skocpol, Carpenter, Clemens) document repeated climb patterns in welfare state adoption, bureaucratic reform, and intellectual paradigm shifts. Institutional economists (Acemoglu, Robinson) model the tension between institutional rigidity and adaptive pressure. The reading is corroborated by scholars outside any single innovation's beneficiary coalition; it is also contested by exogenous-imposition and hybrid-cascade readings that identify cases where climbs fail or where apex imposition succeeds without fringe validation.
narrative_ontology:disappearance_verdict(state_commitment_installation_mechanism__endogenous_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_commitment_installation_mechanism__endogenous_climb_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_commitment_installation_mechanism__endogenous_climb_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_commitment_installation_mechanism__endogenous_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_commitment_installation_mechanism__endogenous_climb_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_commitment_installation_mechanism__endogenous_climb_reading_tests).
:- end_tests(state_commitment_installation_mechanism__endogenous_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-moderate (0.28 at interval end, rising from 0.12) because the mechanism coordinates genuine institutional improvement: the fringe innovations genuinely solve problems the apex's prior commitments could not. But the climb is not friction-free coordination — fringe actors and early adopters do extract real benefits (reputational capital, operational authority, intellectual legitimacy, resource flows) as they drive adoption, and competing fringe movements are starved of attention. The extractiveness rises during the climb phase (t=0 to t=20) as fringe actors consolidate influence, then plateaus once apex institutions formally adopt the commitment (t=20 to t=40) — the plateau reflects the stabilization of the new institutional order and the absorption of fringe actors into apex structures. Theater is low (0.15) because the mechanism's legitimacy comes from demonstrated performance, not from theatrical display or symbolic elaboration. The performance actually matters; the mechanism is not mostly ritual. Suppression is low but rises during the climb (t=0 to t=20) as incumbent resistance and apex gatekeeping activate to manage the pace of adoption — apex authorities suppress faster climbs than they can politically integrate. Resistance is high (0.58) because apex incumbents and competing fringe movements actively resist the new commitment's adoption. The one-shared-time-grid rule is observed: every metric is authored at every time point on the grid {0, 5, 10, 20, 30, 40}.
 *
 * PERSPECTIVAL GAP:
 *   From the fringe actors' seat, the endogenous climb is the legitimate pathway by which superior innovations finally overcome institutional inertia — they see demonstrated superiority doing the work of conversion. From apex incumbents' seat, the climb is a destabilizing and threatening process in which their authority is questioned and their preferred order is displaced by coalition pressure from inside and outside the institution. From apex institutional authorities' seat (the non-incumbent powerful), the climb is an opportunity to gain reputational capital by championing improvement. From apex authorities who are genuinely neutral (a rare seat), the climb is a mechanism for institutional learning that does not require them to abandon defensive posture but allows superior innovations to prove themselves before adoption. The engine computes per-seat directionality from the base structural data: fringe beneficiaries sit at low d (they collect from the climb without running the entire state); apex incumbents sit at high d (they bear the climb cost through status loss and forced reorganization); apex authorities sit at d near symmetric (they gain reputational capital but also bear operational costs). The spectral reading's frame generates different computed types across seats while the ε value stays fixed (the standing arrangement under contest — the endogenous climb as a mechanism — is assessed the same way by all readings' own lights).
 *
 * DIRECTIONALITY LOGIC:
 *   Fringe institutional actors benefit from the climb: their ideas gain institutional purchase, resources flow toward them, they gain operational authority over the new commitment's initial implementation. They have mobile exit options (shift attention to other innovations if this one stalls), so their directionality is low (beneficiary, light constraint). Early adopter elites within apex institutions benefit from championing the climb: reputational capital, authority over the commitment's operationalization, credit for institutional improvement. They have arbitrage-grade exit (can shift patronage to other innovations, can revert to defending status quo if costs rise), so their directionality is moderate-low (partial beneficiary). Advocacy communities benefit from institutional legitimacy and platform access, but their professional identity is bound to the movement's success, so their exit is constrained — their directionality is moderate. Apex institutional incumbents pay the climb cost (status loss, authority displacement, forced reorganization) and have constrained exit (open resistance marks them as obstructing progress), so their directionality is high (target). Apex institutional authorities also bear operational costs of reorganization, so their directionality is moderate-to-high, but they retain formal authority and can slow the climb's pace, so they are not pure targets — secondary agenda_setter role captures this. The measured extractiveness (0.28) reflects the asymmetric benefits and costs: the climb is not friction-free, and the process concentrates reputational and operational authority upward to early adopters and downward (initially) to fringe champions, while apex incumbents bear diffuse costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live — institutions genuinely struggle with the tension between defensive inertia and adaptive pressure. The disappearance verdict is world_rearranges — if the endogenous climb mechanism vanished, institutional innovation would reorganize around exogenous imposition or crisis-driven cascade, with different distribution of costs and benefits. The founding_problem_status x disappearance_verdict alignment (live + rearranges) indicates the mechanism is genuinely needed and its removal would create institutional strain. There is no mandatrophy signal (a dead problem persisting as theater). The low theater_ratio (0.15) further confirms that the constraint's legitimacy comes from demonstrated institutional improvement, not from symbolic maintenance of a defunct function. The classification (rope) reflects genuine coordination: the climb solves a real institutional innovation problem by channeling fringe ideas through institutional vetting (demonstrated superiority + apex political coalition) to legitimate adoption. The extraction that occurs (0.28) is real but not extractive-dominant: it is the cost of the coordination mechanism itself, not the mechanism's core purpose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    endogenous_vs_exogenous_causality,
    'Does the fringe commitment climb succeed because its demonstrated superiority is inherently compelling to apex institutions, or because early adopters within the apex structure selectively amplify and frame evidence to advance political goals aligned with the fringe innovation?',
    'Institutional history: examine cases where fringe commitments demonstrably failed to climb despite strong performance (evidence that superiority alone is insufficient), and cases where apex imposition succeeded despite fringe resistance (evidence that exogenous authority can override endogenous climb pressures). Compare the causal pathways: if climbs succeed only when early adopter champions are present, the mechanism is partly endogenous (superior performance provides cover) and partly exogenous (apex political coalitions drive adoption).',
    'If resolved toward ''superiority is necessary but not sufficient, and apex politics determine climb speed,'' the reading''s core claim (that demonstrated superiority alone generates legitimate pressure) weakens — the mechanism becomes hybrid cascade rather than pure endogenous climb. If resolved toward ''superiority is sufficient and sufficient quickly,'' the reading''s framing is validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(endogenous_vs_exogenous_causality, empirical, 'The causal weight of performance versus apex political positioning in determining climb trajectories.').

omega_variable(
    legitimacy_as_extraction_cover,
    'Does the endogenous climb mechanism transfer genuine institutional authority to fringe actors and early adopters, or does it create the appearance of grassroots validation while apex institutions retain de facto control over which fringe commitments are permitted to climb?',
    'Examine cases where fringe actors attempted to leverage their climb-phase influence into post-adoption control: Did they retain operational authority over the commitment''s implementation, or did apex institutions reabsorb control once the commitment was formalized? Do fringe-derived innovations retain distinctive characteristics post-adoption, or are they assimilated into apex-standard operating procedures? Track resource flows: did funding and patronage follow fringe actors into the apex, or did apex institutions defund the fringe infrastructure once adoption was complete?',
    'If apex institutions systematically strip operational control from fringe actors post-adoption, the climb is a legitimacy-borrowing mechanism: extraction by the apex of the fringe''s accumulated trust and innovation capital. If fringe actors retain meaningful influence, the mechanism is genuinely coordinate. The measured extractiveness (0.28) sits in the middle of the spectrum — this omega resolves whether that reflects true coordination or sophisticated extraction disguised as inclusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_as_extraction_cover, empirical, 'Whether post-adoption apex control is reabsorbed or genuinely shared with fringe actors.').

omega_variable(
    kernel_reading_contest,
    'Is the endogenous climb reading an accurate description of how new state commitments actually gain legitimacy, or is it an idealized narrative that marginalizes cases where apex imposition or crisis-driven cascade better explain observed adoption patterns?',
    'This is not resolvable by evidence alone — the readings are positioned differently on a kernel (state_commitment_installation_mechanism) such that each reading selects different aspects of the same historical record as salient. The ambiguity is located in whether ''climb'' is the dominant mechanism or one mechanism among three.',
    'If the endogenous climb reading is confirmed as dominant across a broad sample of cases, the mechanism is validated and the other readings are partial or exceptional. If all three readings describe frequent patterns depending on institutional context, the kernel itself requires decomposition: what conditions favor climb, imposition, or cascade? If the readings are incommensurable (each uses different framing to interpret the same cases), the kernel is under-specified and the three constraint stories should link via network.affects_constraints to form a constraint family that together cover the institutional installation landscape.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the endogenous climb is the dominant mechanism for commitment installation or one mechanism among equally frequent alternatives.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'The measured suppression (0.22) reflects incumbent resistance and the selective amplification by apex authorities of fringe innovations they favor. Is this suppression primarily structural (external barriers: gatekeeping, resource control, formal exclusion from the adoption conversation) or internalized (fringe actors anticipate rejection and self-censor their innovation proposals, apex incumbents internalize a narrative of inevitable progress that dampens their own resistance)?',
    'Examine post-climb behavioral changes: If suppression was primarily structural, fringe actors and apex incumbents should show reduced resistance and reduced censorship once the commitment is adopted (the barriers are removed). If suppression was primarily internalized, the patterns should persist even after adoption (actors have internalized the legitimacy of the new commitment and continue to frame alternatives as hopeless). Track intellectual production: Do competing fringe movements that lose the climb race produce intellectual work at the same rate before and after the winner is adopted? Do they continue to develop alternative frameworks or do they migrate to the winning innovation or exit the field?',
    'If suppression is primarily structural, the measured value (0.22) is an artifact of institutional gatekeeping and is reversible. If suppression is primarily internalized, the constraint embeds more deeply in actor self-concepts and is harder to dislodge. The classification consequence: if suppression is internalized, the effective suppression is higher than the structural measure, and the constraint may be reclassified toward snare (the climb is a cover story for the internalization of institutional inevitability) rather than rope (genuine coordination through demonstrated superiority).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'The structural versus internalized character of the suppression that shapes which fringe innovations climb.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_commitment_installation_mechanism__endogenous_climb_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(stat_tr_t5, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 5, 0.11).
narrative_ontology:measurement(stat_tr_t10, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 10, 0.13).
narrative_ontology:measurement(stat_tr_t20, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(stat_tr_t30, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement(stat_tr_t40, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 40, 0.15).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(stat_be_t5, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 5, 0.18).
narrative_ontology:measurement(stat_be_t10, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 10, 0.22).
narrative_ontology:measurement(stat_be_t20, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 20, 0.28).
narrative_ontology:measurement(stat_be_t30, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 30, 0.28).
narrative_ontology:measurement(stat_be_t40, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 40, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(stat_su_t5, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 5, 0.1).
narrative_ontology:measurement(stat_su_t10, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 10, 0.16).
narrative_ontology:measurement(stat_su_t20, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 20, 0.22).
narrative_ontology:measurement(stat_su_t30, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 30, 0.22).
narrative_ontology:measurement(stat_su_t40, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 40, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_commitment_installation_mechanism__endogenous_climb_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(state_commitment_installation_mechanism__endogenous_climb_reading, 0.1).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__endogenous_climb_reading, state_commitment_installation_mechanism__exogenous_imposition_reading).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__endogenous_climb_reading, state_commitment_installation_mechanism__hybrid_cascade_reading).

% DUAL FORMULATION NOTE:
% The state_commitment_installation_mechanism kernel decomposes into three readings: endogenous_climb_reading (fringe-driven legitimacy through demonstrated superiority), exogenous_imposition_reading (apex-driven legitimacy through authority mandate), and hybrid_cascade_reading (apex initiative with fringe validation). Each reading has a different ε value, beneficiary structure, and classification because each reading's framing selects different aspects of state commitment adoption as salient. The three readings form a constraint family: each links to the other two via network.affects_constraints to enable cross-reading analysis of how commitment adoption mechanisms vary by institutional context and reading frame.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_commitment_installation_mechanism__endogenous_climb_reading, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
