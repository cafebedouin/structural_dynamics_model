% ============================================================================
% CONSTRAINT STORY: human_dignity_ai_governance__techno_optimist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_human_dignity_ai_governance__techno_optimist_reading, []).

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
 *   constraint_id: human_dignity_ai_governance__techno_optimist_reading
 *   human_readable: Minimal AI Governance / Techno-Optimist Reading of Human Dignity
 *   domain: theological_ethics/technology_governance/political_economy
 *
 * SUMMARY:
 *   This constraint is ONE READING of a contested kernel: the definition of
 *   human dignity in the context of AI governance. The techno-optimist
 *   reading asserts that human dignity is enhanced through technological
 *   augmentation, AI is a tool for transcending biological limits, and
 *   governance should minimize restrictions to enable innovation and
 *   individual choice. This reading stands in active contest with three
 *   sibling readings: magisterial_integralist (dignity is an ontological gift
 *   grounded in imago Dei, requiring Catholic Social Doctrine guidance),
 *   secular_humanist (dignity is grounded in rational autonomy and universal
 *   rights, requiring democratic deliberation), and pluralist_pragmatic
 *   (dignity is contested and requires negotiated multi-stakeholder
 *   frameworks). This JSON instantiates ONLY the techno-optimist reading as a
 *   structurally clean constraint. The siblings are OTHER constraints, not
 *   part of this story. The contest itself is the kernel; this reading is one
 *   position within it.
 *
 * KEY AGENTS:
 *   - AI developers & researchers: agenda-setters, frame minimal governance as enabling human flourishing, set priorities by technical feasibility and market opportunity.
 *   - Tech capital holders: beneficiaries, accumulate wealth and power through AI infrastructure ownership, have global exit options.
 *   - Early adopters: beneficiaries, access enhancement technologies first, benefit from capability gaps relative to non-adopters.
 *   - Displaced workers: victims, lose employment to AI automation, trapped by geography and resources, no compensation mechanism.
 *   - Low-income populations: victims, experience widening inequality, subjected to unauditable AI systems, cannot afford augmentation.
 *   - Technology-excluded communities: victims and excluded, face geographic/linguistic/economic barriers to technology participation, marginalized by AI systems.
 *   - Magisterial integralists, secular humanists, pluralist pragmatists: excluded stakeholders, their moral frameworks are treated as obstacles rather than legitimate ethical grounding.
 *   - Academic ethicists & regulatory authorities: observers, can document externalities but lack binding authority to alter the constraint or face enormous political/economic pressure to defer.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(human_dignity_ai_governance__techno_optimist_reading, 0.78).
domain_priors:suppression_score(human_dignity_ai_governance__techno_optimist_reading, 0.62).
domain_priors:theater_ratio(human_dignity_ai_governance__techno_optimist_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(human_dignity_ai_governance__techno_optimist_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(human_dignity_ai_governance__techno_optimist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(human_dignity_ai_governance__techno_optimist_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(human_dignity_ai_governance__techno_optimist_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(human_dignity_ai_governance__techno_optimist_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(human_dignity_ai_governance__techno_optimist_reading, rope).
narrative_ontology:human_readable(human_dignity_ai_governance__techno_optimist_reading, "Minimal AI Governance / Techno-Optimist Reading of Human Dignity").
narrative_ontology:topic_domain(human_dignity_ai_governance__techno_optimist_reading, "theological_ethics/technology_governance/political_economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(human_dignity_ai_governance__techno_optimist_reading, 'f5602799-31a3-4be8-b0d1-ac85bc51d454').
narrative_ontology:cs_kernel_codification('f5602799-31a3-4be8-b0d1-ac85bc51d454', distributed).
narrative_ontology:cs_authority_grounding('f5602799-31a3-4be8-b0d1-ac85bc51d454', extraction).
narrative_ontology:cs_interpretation_layer_present('f5602799-31a3-4be8-b0d1-ac85bc51d454').
narrative_ontology:cs_reading_relation('f5602799-31a3-4be8-b0d1-ac85bc51d454', human_dignity_ai_governance__magisterial_integralist_reading, forecloses).
narrative_ontology:cs_reading_relation('f5602799-31a3-4be8-b0d1-ac85bc51d454', human_dignity_ai_governance__secular_humanist_reading, forecloses).
narrative_ontology:cs_reading_relation('f5602799-31a3-4be8-b0d1-ac85bc51d454', human_dignity_ai_governance__pluralist_pragmatic_reading, coexists_with).
narrative_ontology:cs_axiom('f5602799-31a3-4be8-b0d1-ac85bc51d454', foundational, technological_transcendence_enhances_dignity).
narrative_ontology:cs_axiom_status(technological_transcendence_enhances_dignity, holdable).
narrative_ontology:cs_axiom_grounding('f5602799-31a3-4be8-b0d1-ac85bc51d454', technological_transcendence_enhances_dignity, instrumental).
narrative_ontology:cs_axiom('f5602799-31a3-4be8-b0d1-ac85bc51d454', foundational, market_mechanisms_sufficient_governance).
narrative_ontology:cs_axiom_status(market_mechanisms_sufficient_governance, holdable).
narrative_ontology:cs_axiom_grounding('f5602799-31a3-4be8-b0d1-ac85bc51d454', market_mechanisms_sufficient_governance, instrumental).
narrative_ontology:cs_reference_frame('f5602799-31a3-4be8-b0d1-ac85bc51d454', innovation_maximization_framework).
narrative_ontology:cs_drift_state('f5602799-31a3-4be8-b0d1-ac85bc51d454', contemporary_inequality_acceleration_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f5602799-31a3-4be8-b0d1-ac85bc51d454', '').
narrative_ontology:cs_kernel_id(human_dignity_ai_governance__techno_optimist_reading, human_dignity_ai_governance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(human_dignity_ai_governance__techno_optimist_reading, early_adopters).
narrative_ontology:constraint_beneficiary(human_dignity_ai_governance__techno_optimist_reading, ai_developers).
narrative_ontology:constraint_beneficiary(human_dignity_ai_governance__techno_optimist_reading, tech_capital_holders).
narrative_ontology:constraint_victim(human_dignity_ai_governance__techno_optimist_reading, displaced_workers).
narrative_ontology:constraint_victim(human_dignity_ai_governance__techno_optimist_reading, low_income_populations).
narrative_ontology:constraint_victim(human_dignity_ai_governance__techno_optimist_reading, technology_excluded_communities).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(human_dignity_ai_governance__techno_optimist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(human_dignity_ai_governance__techno_optimist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(human_dignity_ai_governance__techno_optimist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(human_dignity_ai_governance__techno_optimist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(human_dignity_ai_governance__techno_optimist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness metric (0.78) reflects that the constraint concentrates power, wealth, and capability-access in the hands of early adopters and tech capital holders while externalizing costs (job loss, algorithmic harm, inequality) onto vulnerable populations without compensation mechanism. The suppression metric (0.62) captures the active suppression of alternative governance frames (religious, humanist, pragmatist) through ideology, capital mobility, and regulatory capture — the constraint's persistence depends on preventing these alternative readings from gaining agenda-setting power. The theater_ratio (0.41, rising from 0.22) models a gradual increase in performative activity: as the contradiction between the enabling-human-flourishing narrative and the actual concentrating-of-power outcome becomes more visible, more communication effort is devoted to justifying the constraint (corporate social responsibility narratives, promises of 'responsible AI', stakeholder initiatives that lack binding power). The accessibility_collapse metric (0.48) is deliberately moderate: alternatives to the techno-optimist reading are not completely collapsed — they persist as organized stakeholder positions (religious communities, democratic advocates, pragmatists) — but they are kept off the agenda-setting table through structural mechanisms (capital concentration, regulatory capture, framing-dominance). The resistance metric (0.59) reflects real push-back from workers, communities, and alternative-reading stakeholders, but it is not yet strong enough to destabilize the constraint. The measurement series tracks extraction rising (as inequality deepens and displacement accelerates) and theater rising (as the gap between narrative and reality widens), while suppression_requirement plateaus (the suppression needed to keep excluded stakeholders off the agenda stabilizes). All metrics are authored from a descriptive stance independent of the claimed_type.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (AI developers, tech capital) experiences this constraint as enabling human flourishing and innovation — genuine coordination solving an important founding problem. The victim seats (displaced workers, low-income populations) experience it as enforced extraction with no compensation. The excluded seats (alternative-reading stakeholders) experience it as delegitimized, their moral frameworks dismissed. The observer seat (academic ethicists, regulators) can see the structure clearly but has limited power to alter it. The engine should compute these seats as experiencing substantially different types from each other. The agenda-setter likely experiences this as rope (genuine coordination with incidental costs); victims should compute as snare (pure extraction); excluded stakeholders should compute as experiencing a constraint they were not party to and cannot shape. The divergence between the agenda-setter's comfortable reading and the victim/excluded readings is the structural fact this story instantiates.
 *
 * DIRECTIONALITY LOGIC:
 *   AI developers and tech capital holders have low directionality (d near 0.0, beneficiary end): they set the agenda, have arbitrage-grade exit options (can move development to permissive jurisdictions), collect the extraction directly, and experience the constraint as enabling their goals. Early adopters have moderate directionality (d near 0.3): they benefit substantially but depend on the innovation stream maintained by developers and capital holders. Displaced workers have high directionality (d near 0.9, target end): they are trapped (no alternative employment available in their region), have no voice in governance, bear the extraction (job loss, income loss) with no compensation, and cannot exit the constraint. Low-income populations have high directionality (d near 0.85): constrained exit (cannot afford relocation, retraining, or premium access to protective services), bear diffuse costs through algorithmic harm and inequality deepening, are excluded from the conversation. Technology-excluded communities have maximum directionality (d = 1.0): completely trapped, identity-locked to exclusion (cultural/geographic/linguistic barriers make participation structurally impossible), victimized without voice. The excluded stakeholders (religious, secular humanist, pragmatist) have directionality near 1.0 as targets of suppression — the constraint's enforcement machinery exists partly to keep them off the agenda.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is claimed as rope (the minimal-governance framing is presented as enabling coordination around innovation) but the authored metrics describe substantially extractive operation (high extractiveness, significant suppression of alternative frames, rising theater as the gap between narrative and reality widens). The mandatrophy question is whether the founding problem (solving existential risks through rapid innovation) is genuinely live or has been superseded by a different problem (managing the inequality and displacement created by unguided acceleration). The founding_problem_status is authored as contested because the tech beneficiaries attest the problem is live while external stakeholders argue the founding problem has been solved and the constraint persists as rent collection. This contest — between the beneficiary narrative of enabling-human-flourishing and the external observation of extraction-and-displacement — is the core signal that the constraint may have suffered mandatrophy (its founding justification is no longer the primary driver of its operation; the primary driver is now wealth/power concentration). The theater_ratio rise (0.22 → 0.41) supports this: as mandatrophy sets in, the framing work intensifies to bridge the gap between the old justification and the new reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_boundary,
    'Is minimal governance genuinely necessary to solve the founding problems (existential risks, biological limits transcendence), or does the minimal-governance framing primarily serve to protect the extractive advantages of early adopters and capital holders?',
    'Comparative analysis of AI deployment outcomes under different governance regimes (e.g., EU regulatory approach vs. US minimal-governance approach) measuring both innovation velocity and societal harm. Documented cases where protective regulation actually accelerated beneficial innovation (e.g., medical device safety standards enabling market growth).',
    'If minimal governance is not necessary for solving founding problems, the constraint reclassifies from coordination-with-extraction to pure extraction (snare). The beneficiary class would lose the legitimating claim that they are enabling solutions to humanity''s problems.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether the founding problem genuinely requires minimal governance for solution.').

omega_variable(
    techno_optimism_as_identity_lock,
    'How much of the suppression of alternative governance readings is structural (capital mobility, regulatory capture, tech industry political power) versus identity-locked (AI developers and tech believers have fused their professional identity and self-concept with the innovation-maximization narrative, making exit from the framing unthinkable)?',
    'Case analysis of AI researchers and technologists who have publicly shifted to favor greater governance — what cognitive or structural shifts enabled the reframe? Post-industry interviews with former advocates of minimal governance examining whether the shift required identity rupture or merely perspective-change.',
    'If the suppression is predominantly structural, override directionality upward for the powerless victims (higher d toward full target). If predominantly identity-locked, the suppression persists even if external constraints are removed, and the constraint''s effective suppression value is higher than the structural measure suggests. The constraint may also be diagnosable as tangled_rope (coordination function of innovation + identity coordination of the developer class, alongside extraction from victims).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(techno_optimism_as_identity_lock, conceptual, 'Structure vs. identity-fusion in the suppression of alternative governance framings.').

omega_variable(
    foundational_axiom_empirical_validation,
    'Is human dignity genuinely advanced by technological augmentation (the core premise of this reading), or does the techno-optimist reading misidentify what dignity is, treating it as instrumental capacity-maximization when it is actually relational and inalienable?',
    'Philosophical analysis and empirical observation: among populations with high AI-augmentation access, do measures of subjective well-being, autonomy, relational satisfaction, and self-reported dignity increase? Do they increase more than non-augmented populations? Does augmentation crowd out other sources of dignity (relational, spiritual, community-based)? Longitudinal studies comparing dignity-as-reported in augmented-heavy vs. augmented-light populations.',
    'If augmentation does not increase reported dignity or crowds out other sources, the foundational axiom of this reading (technological_transcendence_enhances_dignity) is empirically undermined. The reading would lose legitimacy even among neutral observers. This is the core distinction between this reading and the magisterial_integralist and secular_humanist readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(foundational_axiom_empirical_validation, empirical, 'Whether technological augmentation actually enhances human dignity as measured by affected populations.').

omega_variable(
    kernel_contest_resolution_mechanism,
    'Which of the four readings of the human_dignity_ai_governance kernel will gain institutional dominance in actual governance structures (legislatures, regulatory bodies, corporate governance), and through what mechanism (political power, cultural persuasion, empirical vindication, structural lock-in)?',
    'Longitudinal policy analysis tracking which reading (techno-optimist, integralist, humanist, pragmatist) is embedded in actual governance outcomes across major jurisdictions over the next 10–20 years. Which reading proves structurally resilient? Which readings are foreclosed by empirical outcomes or political dynamics?',
    'The kernel contest is irreducible here — the four readings are live, coexisting, and mutually exclusive in their core premises. The resolution will not be philosophical agreement but political/empirical settling. If the techno-optimist reading maintains dominance, extraction and suppression of alternatives will continue. If a rival reading gains ground, this constraint''s structure (minimalist governance, beneficiary-priority framing) will be substantially altered or replaced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contest_resolution_mechanism, empirical, 'Which reading of human dignity in AI governance achieves institutional dominance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(human_dignity_ai_governance__techno_optimist_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, human_dignity_ai_governance__techno_optimist_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(huma_tr_t5, human_dignity_ai_governance__techno_optimist_reading, theater_ratio, 5, 0.27).
narrative_ontology:measurement(huma_tr_t10, human_dignity_ai_governance__techno_optimist_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(huma_tr_t15, human_dignity_ai_governance__techno_optimist_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement(huma_tr_t20, human_dignity_ai_governance__techno_optimist_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(huma_tr_t25, human_dignity_ai_governance__techno_optimist_reading, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, human_dignity_ai_governance__techno_optimist_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(huma_be_t5, human_dignity_ai_governance__techno_optimist_reading, base_extractiveness, 5, 0.64).
narrative_ontology:measurement(huma_be_t10, human_dignity_ai_governance__techno_optimist_reading, base_extractiveness, 10, 0.7).
narrative_ontology:measurement(huma_be_t15, human_dignity_ai_governance__techno_optimist_reading, base_extractiveness, 15, 0.75).
narrative_ontology:measurement(huma_be_t20, human_dignity_ai_governance__techno_optimist_reading, base_extractiveness, 20, 0.77).
narrative_ontology:measurement(huma_be_t25, human_dignity_ai_governance__techno_optimist_reading, base_extractiveness, 25, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t0, human_dignity_ai_governance__techno_optimist_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(huma_su_t5, human_dignity_ai_governance__techno_optimist_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(huma_su_t10, human_dignity_ai_governance__techno_optimist_reading, suppression_requirement, 10, 0.56).
narrative_ontology:measurement(huma_su_t15, human_dignity_ai_governance__techno_optimist_reading, suppression_requirement, 15, 0.6).
narrative_ontology:measurement(huma_su_t20, human_dignity_ai_governance__techno_optimist_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(huma_su_t25, human_dignity_ai_governance__techno_optimist_reading, suppression_requirement, 25, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(human_dignity_ai_governance__techno_optimist_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(human_dignity_ai_governance__techno_optimist_reading, 0.18).
narrative_ontology:affects_constraint(human_dignity_ai_governance__techno_optimist_reading, human_dignity_ai_governance__magisterial_integralist_reading).
narrative_ontology:affects_constraint(human_dignity_ai_governance__techno_optimist_reading, human_dignity_ai_governance__secular_humanist_reading).
narrative_ontology:affects_constraint(human_dignity_ai_governance__techno_optimist_reading, human_dignity_ai_governance__pluralist_pragmatic_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a contested kernel: human_dignity_ai_governance. Four structurally distinct constraints emanate from the same kernel, each instantiating a different stakeholder-reading of what human dignity means and how AI governance should serve it. The techno-optimist reading (this story) treats dignity as enhanceable through augmentation and governance as a minimization problem. The integralist reading treats dignity as ontological (imago Dei) and governance as a common-good obligation. The humanist reading treats dignity as rational autonomy and governance as a democratic requirement. The pragmatist reading treats dignity as contested and governance as a negotiated process. Each reading has a different beneficiary/victim structure, different extractiveness profile, and different perception of whether the constraint enables flourishing or enables extraction. All four readings are live simultaneously across different stakeholder populations; none is foreclosed by the others' core premises. The technical innovation landscape is the shared context, but each reading instantiates a different constraint from that landscape. A single global 'AI governance' constraint that averaged across readings would be analytically useless; the divergence is the signal. Decomposition is required.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(human_dignity_ai_governance__techno_optimist_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
