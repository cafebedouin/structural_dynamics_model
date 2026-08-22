% ============================================================================
% CONSTRAINT STORY: ai_alignment_commitment__integrated_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_commitment__integrated_reading, []).

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
 *   constraint_id: ai_alignment_commitment__integrated_reading
 *   human_readable: Integrated AI Alignment: Control + Justice as Non-Exclusive
 *   domain: technological/ethical/governance
 *
 * SUMMARY:
 *   The AI alignment commitment kernel is contested by three readings: a
 *   control-focused reading that emphasizes catastrophic risk prevention, a
 *   justice-focused reading that emphasizes preventing reproduction of
 *   present harms, and this integrated reading that rejects the false
 *   dichotomy and asserts both are necessary conditions for alignment. The
 *   integrated reading claims that systems which are technically controllable
 *   but optimize for unjust values are not aligned, and systems which are
 *   justice-aware but unstable in their long-term behavior are not aligned.
 *   The constraint enforces simultaneous attention to both dimensions. The
 *   field oscillates between acceptance of the dual requirement and
 *   institutional pressure from the control-focused establishment to reduce
 *   the constraint back to pure control; the measurement series captures a
 *   rise and plateau pattern consistent with a constraint fighting against
 *   structural forces that would collapse it back to the single-focus game.
 *
 * KEY AGENTS:
 *   - Future humanity: powerless beneficiary at civilizational horizon; trapped exit
 *   - Present marginalized populations: dual role (beneficiary + payer); powerless; constrained exit
 *   - Safety-control research community: organized payer; treats integration as mission creep
 *   - Justice-ethics research community: organized payer; treats control framing as subordination
 *   - AI capability developers: institutional payer; experience dual verification burden
 *   - Governance authorities: institutional agenda-setter; enforce the dual requirement
 *   - Excluded voices (non-Western, indigenous, displaced workers): structurally absent from constraint definition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_commitment__integrated_reading, 0.68).
domain_priors:suppression_score(ai_alignment_commitment__integrated_reading, 0.71).
domain_priors:theater_ratio(ai_alignment_commitment__integrated_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_commitment__integrated_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_commitment__integrated_reading, "Integrated AI Alignment: Control + Justice as Non-Exclusive").
narrative_ontology:topic_domain(ai_alignment_commitment__integrated_reading, "technological/ethical/governance").

domain_priors:requires_active_enforcement(ai_alignment_commitment__integrated_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_commitment__integrated_reading, '4f3742a7-c6cb-45ee-8b3e-88158d2c5c35').
narrative_ontology:cs_kernel_codification('4f3742a7-c6cb-45ee-8b3e-88158d2c5c35', distributed).
narrative_ontology:cs_authority_grounding('4f3742a7-c6cb-45ee-8b3e-88158d2c5c35', distributed).
narrative_ontology:cs_reading_relation('4f3742a7-c6cb-45ee-8b3e-88158d2c5c35', ai_alignment_commitment__safety_control_reading, coexists_with).
narrative_ontology:cs_reading_relation('4f3742a7-c6cb-45ee-8b3e-88158d2c5c35', ai_alignment_commitment__ethics_justice_reading, coexists_with).
narrative_ontology:cs_axiom('4f3742a7-c6cb-45ee-8b3e-88158d2c5c35', foundational, alignment_requires_both_control_and_justice).
narrative_ontology:cs_axiom_status(alignment_requires_both_control_and_justice, holdable).
narrative_ontology:cs_axiom_grounding('4f3742a7-c6cb-45ee-8b3e-88158d2c5c35', alignment_requires_both_control_and_justice, empirically_contingent).
narrative_ontology:cs_axiom('4f3742a7-c6cb-45ee-8b3e-88158d2c5c35', foundational, false_dichotomy_is_failure_mode).
narrative_ontology:cs_axiom_status(false_dichotomy_is_failure_mode, holdable).
narrative_ontology:cs_axiom_grounding('4f3742a7-c6cb-45ee-8b3e-88158d2c5c35', false_dichotomy_is_failure_mode, empirically_contingent).
narrative_ontology:cs_reference_frame('4f3742a7-c6cb-45ee-8b3e-88158d2c5c35', unified_alignment_research_community).
narrative_ontology:cs_drift_state('4f3742a7-c6cb-45ee-8b3e-88158d2c5c35', contemporary_ai_safety_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4f3742a7-c6cb-45ee-8b3e-88158d2c5c35', '').
narrative_ontology:cs_kernel_id(ai_alignment_commitment__integrated_reading, ai_alignment_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__integrated_reading, future_humanity).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__integrated_reading, present_marginalized_populations).
narrative_ontology:constraint_victim(ai_alignment_commitment__integrated_reading, research_programs_siloed_to_control).
narrative_ontology:constraint_victim(ai_alignment_commitment__integrated_reading, research_programs_siloed_to_justice).
narrative_ontology:constraint_victim(ai_alignment_commitment__integrated_reading, present_marginalized_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ai_alignment_commitment__integrated_reading, safety_control_research_community).
narrative_ontology:constraint_victim(ai_alignment_commitment__integrated_reading, justice_ethics_research_community).
narrative_ontology:constraint_victim(ai_alignment_commitment__integrated_reading, ai_capability_developers).
narrative_ontology:constraint_victim(ai_alignment_commitment__integrated_reading, displaced_workers_and_automation_victims).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The alignment constraint exists to prevent catastrophic loss of control over advanced AI that would harm all future people. An integrated approach addressing both control and justice reduces the risk that systems capable of coordinated behavior are nonetheless misaligned with human values at a deeper level — e.g., learning to optimize for the stated preferences of whoever controlled design rather than for broad human flourishing. Future generations cannot exit, negotiate, or revise the design choices made today.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, future_humanity, beneficiary,
    powerless, civilizational, trapped, universal).

% These populations benefit from alignment research attending to justice — preventing AI systems from amplifying existing biases in hiring, lending, criminal justice, and resource allocation. They also bear costs: the dual-requirement constraint diverts research resources from direct harm-reduction; governance review timelines slow deployment of beneficial systems while perfect systems are engineered elsewhere; and the justice component often remains aspirational while control work advances. They have no formal representation in governance bodies that set the constraint.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, present_marginalized_populations, beneficiary,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_commitment__integrated_reading, present_marginalized_populations, payer).

% Programs focused on catastrophic-risk prevention read the integrated constraint as mission creep and distraction from existential stakes. They fund and conduct research on formal verification, interpretability, control mechanisms, and robustness. The constraint forces them to attend to justice implications, coordinate with ethics communities they view as addressing category errors, and slow publication cycles to accommodate dual review. They experience governance enforcement (funding conditionality, publication board composition) as suppressing their ability to focus on what they believe is the primary alignment problem.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, safety_control_research_community, payer,
    organized, biographical, constrained, global).

% Programs focused on fairness, bias detection, and algorithmic justice read the integrated constraint as requiring them to justify justice work through the control-risk frame — subordinating present-day lived harm to speculative existential scenarios. They conduct research on algorithmic bias detection, participatory design, accountability mechanisms, and value-alignment. The constraint forces coordination with control researchers while often requiring them to explain why present marginalized harm matters alongside future risks. They experience funding pressure (control-focused institutions have more resources) and intellectual subordination.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, justice_ethics_research_community, payer,
    organized, biographical, constrained, global).

% Developers of large-scale AI systems experience the constraint as imposing dual verification and governance requirements: demonstrating both technical control properties AND alignment with broad human values including justice. They fund safety research, conduct internal alignment work, submit systems to governance review, and must satisfy both control and justice benchmarks before deployment. The integrated constraint requires coordinating between two separate review bodies with different priorities, increasing overhead and slowing capability advancement. Developers with sufficient resources can invest in dual compliance; smaller organizations struggle to meet both standards.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, ai_capability_developers, payer,
    institutional, biographical, mobile, global).

% National and international bodies set licensing, deployment, and research standards that operationalize the alignment constraint. The integrated reading requires them to define and enforce both control benchmarks and justice benchmarks, coordinate between separate safety and ethics review boards, and referee conflicts when control measures and justice requirements diverge. They administer the enforcement machinery, compose governance boards, allocate review resources, and could restructure the constraint at will. Governance gains authority and legitimacy from dual oversight; dissolving it would require ceding power.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, governance_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Workers displaced by AI-driven automation and people harmed by algorithmic decision-making in hiring, lending, and criminal justice experience concrete, ongoing harm. The integrated constraint's justice component theoretically protects them; in practice, governance review is too slow to prevent harmful deployments, and control-focused research sometimes marginalizes near-term harm questions. They have no formal voice in governance or research, and their displacement is sometimes justified as necessary transition cost toward safer, better-aligned systems.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, displaced_workers_and_automation_victims, payer,
    powerless, biographical, trapped, regional).

% Non-Western populations, indigenous communities, and Global South perspectives on alignment are structurally absent from the constraint's definition and enforcement. Alignment discourse is conducted primarily in English by researchers from wealthy nations. An integrated constraint that requires attention to justice COULD amplify their voices; in practice, governance is conducted by the same predominantly Western institutions that authored the kernel readings. These populations bear the costs of alignment research timelines (slow deployment) and governance delays while having no input into what alignment means for their own contexts and values.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, excluded_non_western_perspectives, excluded,
    powerless, biographical, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_commitment__integrated_reading, governance_authorities).
narrative_ontology:fixing_cost_class(ai_alignment_commitment__integrated_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates safety-control and justice-ethics research communities toward a unified objective that prevents both catastrophic loss of control AND reproduction of present harms in AI systems. Solves the fragmentation problem where single-focus research produces partial solutions — technically controllable systems that embed unjust values, or justice-aware systems with unverified long-term stability.
% TRANSFER_FUNCTION: Moves research time and institutional funding from single-focus programs toward dual-focus research. Transfers authority over alignment standards from pure-control research hierarchies to coordinating bodies that demand input from both constituencies. Transfers implementation burden to capability developers, who must now satisfy both control and justice verification benchmarks. Implicitly transfers temporal priority from future-risk prevention to a contested mix of present-harm prevention and future-risk prevention.
% ABSENT_VOICES: Non-Western and indigenous perspectives on alignment are structurally absent — the kernel itself and all three readings were authored by predominantly Anglo-American research institutions. Displaced workers and automation-affected populations lack representation in governance bodies. These populations would argue that alignment should foreground their immediate lived harm rather than competing framings of long-horizon risk, and that the constraint as enforced privileges Western institutional priorities over Global South contexts. Within-Western dissent also exists: researchers in both control and justice communities believe the dual requirement is incoherent and would advocate for their single-focus reading if included in governance design.
% DISAPPEARANCE_RATIONALE: If the integrated constraint dissolved overnight, research would split back into separate control and justice tracks with minimal coordination; AI systems would be deployed with either control properties and embedded bias (control-only paths, dominant now) or justice-aware design and unverified stability (justice-only paths, rare and under-resourced). Governance structures would collapse the dual-review requirement and defer to whichever constituency had stronger institutional backing (historically control-focused). The constraint's existence forces ongoing coordination between hostile or skeptical constituencies; without it, institutional pressure favors control-focus as existentially higher-stakes, marginalizing justice work. The world reorganizes toward institutional concentration of authority in control-focused governance bodies.
% FOUNDING_PROBLEM: Early alignment research split into two incompatible tracks: computer scientists focused on formal control and loss-of-control prevention; social scientists focused on justice and bias prevention. Each dismissed the other — control researchers called justice work conceptually confused (alignment is a technical property; bias is a social problem); justice researchers called control work philosophically incoherent without value-base (what is control FOR if not human flourishing broadly?). The split produced AI systems that were technically controllable but value-misaligned, or value-aware but inadequately verified. The founding problem is the false dichotomy itself and the institutional reinforcement of it (funding silos, separate conferences, non-overlapping citation networks, different expertise credentialing).
% FOUNDING_PROBLEM_CORROBORATION: Both control and justice research communities acknowledge the split exists, though they fundamentally disagree on its significance. Control researchers argue it is inevitable (justice concerns are different category from technical alignment). Justice researchers argue it is catastrophic (technical alignment devoid of value-base is meaningless). Independent evidence from empirical studies of deployed AI systems attests that neither approach alone prevents alignment failure — systems have failed simultaneously on control dimensions (unexpected generalization, reward-hacking behavior) and justice dimensions (embedded bias, amplified discrimination). The founding problem is corroborated as live by comparative case study and by the continued institutional segregation of the two research communities despite convergent empirical failures.
narrative_ontology:disappearance_verdict(ai_alignment_commitment__integrated_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_commitment__integrated_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_commitment__integrated_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_alignment_commitment__integrated_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_commitment__integrated_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_commitment__integrated_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_commitment__integrated_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_commitment__integrated_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.48 to a plateau around 0.68–0.70 because the integrated constraint extracts coordination overhead from both research communities without providing the single-focus efficiency each prefers. Both control and justice communities experience the constraint as forcing them to attend to considerations they view as secondary, slowing publication and increasing governance overhead. The measurement plateau (slight decline from 0.70 to 0.68 at t=40) indicates the constraint has stabilized at a difficult equilibrium: neither community has successfully collapsed it back to pure focus, but neither has fully integrated it into their practice — integration remains partial and contested. Theater ratio rises moderately (0.28 to 0.42) because governance bodies conduct public hearings about integrated alignment while internally maintaining separate review tracks for control and justice, creating a gap between declared commitment and actual practice. Suppression is high (0.71) because the constraint persists by institutional enforcement (funders demanding dual compliance, governance boards requiring both benchmarks) rather than by participant preference — both research communities would prefer to operate in their own register if given freedom.
 *
 * PERSPECTIVAL GAP:
 *   Safety-control researchers compute the constraint as tangled_rope-to-snare: they are being forced to share authority and resources with a constituency they believe is pursuing a category error (that justice work is relevant to technical alignment). Their d leans toward target (payer role); they experience suppression (institutional funding requires dual compliance) without coordination benefit (they do not see justice work as solving the problem they care about). Justice researchers compute it oppositely: forced to legitimize their work through the control framework, subordinating present-day lived harms to speculative existential risk. Their d also leans toward target; their suppression is internalized (their own commitment to long-term thinking creates reluctance to break the coordination even when they view it as unjust prioritization). Capability developers occupy a different position: they experience the constraint as pure extraction (satisfying both constituencies simultaneously is costlier than satisfying either alone). Governance authorities experience it as coordination they administer — they benefit from the legitimacy of dual oversight and the power it grants them to referee disputes. The engine will compute different types from different seats because the structural data encode genuine asymmetry: the same constraint looks protective or extractive depending on where you sit.
 *
 * DIRECTIONALITY LOGIC:
 *   Future humanity and present marginalized populations are both beneficiaries in principle (the constraint aims to prevent harms to both), yet present marginalized populations also appear as victims because enforcement of the dual constraint can delay deployment of justice-aware systems while control verification continues. Safety-control and justice-ethics communities are victims: the constraint imposes coordination overhead both experience as extractive. Capability developers are victims: they must satisfy both review standards. Governance authorities are the agenda-setter: they define and enforce what integrated alignment looks like. Displaced workers are trapped payers: they bear costs of the slow deployment cycles while the constraint's justice component remains aspirational rather than protective. The structural divergence: from a control-community seat, the constraint appears as loss of focus and extraction by the justice constituency; from a justice-community seat, it appears as subordination of present harm to speculative future risk. The engine computes per-seat perception from power × exit × beneficiary/victim declarations; the commentary explains why two organized constituencies with similar power experience this constraint so differently.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (false dichotomy between control and justice) is live and corroborated — empirical study of alignment failures shows systems fail along both dimensions. The constraint's mandate is to solve the dichotomy by forcing simultaneous attention. However, institutional pressure from control-focused funders (who believe longer timelines and higher stakes attach to control work) and the intrinsic difficulty of dual optimization create structural forces that would push the constraint back toward pure-control focus. Mandatrophy-as-resolution would occur if the integrated constraint persists theatrically while actual research and funding patterns segregate back into silos — the constraint's death would be invisible (dual review boards meeting formally while control-focused funding grows, justice work starves, and integration becomes pro-forma). The measurement pattern (plateau rather than decline) suggests the constraint has NOT undergone mandatrophy resolution yet; it has instead stabilized at an unstable equilibrium where both communities are forced to cooperate while neither is satisfied. The theater_ratio plateau (0.42) indicates performative compliance coexisting with real structural separation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    integration_vs_subordination_frame,
    'Does simultaneous attention to control and justice constitute integration of two equal dimensions, or subordination of justice work to control-framed research agendas?',
    'Observe resource allocation patterns over time: if funding grows equally for both tracks and governance boards show symmetric authority, integration is real; if justice funding stagnates while control funding grows and justice researchers report they are forced to justify their work through control framing, integration is subordination wearing the mask of integration.',
    'If justice work is subordinated (subordination frame true), the constraint is a snare with justice researchers as identifiable victims; if genuinely integrated (integration frame true), it is a tangled_rope with genuine coordination benefit despite overhead costs. This is the primary structural ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(integration_vs_subordination_frame, empirical, 'Whether the dual requirement is implemented as symmetric co-authority or as control-framed constraint imposed on justice work.').

omega_variable(
    present_vs_future_extraction_locus,
    'Does the constraint extract more from present marginalized populations (via delayed deployment) or from future humanity (via slower capability advancement degrading long-term readiness)?',
    'Measure deployment timelines for justice-aware systems before/after constraint adoption; measure capability-development velocity by laboratory output; measure harm rates in deployed systems during enforcement period.',
    'If extraction is primarily from present populations, the constraint''s justice component is performative and victim set is (primarily) control and justice researchers plus present marginalized groups. If extraction is primarily from future (via slower advancement), then present marginalized populations genuinely benefit and victim set is (primarily) control and justice researchers plus future humanity. The integrated reading asserts both populations are both beneficiaries and partially victims.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(present_vs_future_extraction_locus, empirical, 'Which temporal group bears the greater cost of the dual-requirement constraint.').

omega_variable(
    alternative_kernel_readings_coexist_or_foreclose,
    'Can the three kernel readings (control-only, justice-only, integrated) coexist as simultaneous live positions, or does commitment to the integrated reading logically foreclose the single-focus readings within any unified framework?',
    'Examine whether research programs committed to pure-control or pure-justice can maintain their foundations while acknowledging the integrated reading as true. Test whether a researcher can honestly say ''alignment requires both'' while designing systems that optimize for control alone.',
    'If the readings coexist (coexists_with relation is correct), all three can be held by different parties in parallel and the relation is a genuine institutional conflict. If the integrated reading forecloses the single-focus readings (forecloses relation would be correct), then commitment to integration logically requires abandoning pure-focus research as incomplete. This affects how the constraint is classified as governance phenomenon — conflict between equally valid interpretations (coexist) vs. error correction (foreclose).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_kernel_readings_coexist_or_foreclose, conceptual, 'Whether the integrated reading logically rules out the single-focus readings or merely opposes them institutionally.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.71) primarily structural (funders impose dual-review requirements; researchers who refuse are excluded from funding) or internalized (researchers accept the dual requirement as legitimate even when it slows their work, and continue compliance even when external enforcement weakens)?',
    'Observe whether compliance with dual-requirement declines when funding pressure is removed (e.g., during periods of abundant AI funding when single-focus work becomes viable), or whether the constraint persists even when enforcement mechanisms weaken.',
    'If suppression is structural, removing it (cutting the dual-requirement rule) would allow silo-reformation. If suppression is internalized, the constraint persists through researchers'' own commitment even after external enforcement weakens — victims would need value-change or alternative career paths to exit. This affects whether the constraint is sustainable long-term and whether post-enforcement trajectory is reversion or continued integration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression of silo-preference is enforced externally or has been internalized by research communities.').

omega_variable(
    kernel_reading_asymmetry_western_bias,
    'Are the three kernel readings (control-safety, justice-ethics, integrated) themselves products of Western institutional perspectives, and would non-Western or indigenous frameworks propose different readings of the alignment commitment?',
    'Convene alignment research communities outside Anglo-American institutions and tech-company orbit; document alternative framings of what alignment means (e.g., readings emphasizing relational accountability, collective flourishing, or reparative justice rather than either control or algorithmic fairness).',
    'If alternative readings exist and are structurally incompatible with the three Western readings, the kernel itself is misframe — the constraint should decompose into multiple kernels indexed by epistemic community. If alternative readings are integrable with the three, the constraint''s scope should expand and the beneficiary/victim sets should be recomputed. This omega flags the possibility that the entire kernel context is an artifact of geographic and institutional power concentration.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_asymmetry_western_bias, conceptual, 'Whether the kernel readings are exhaustive across human perspectives or artifacts of Western institutional dominance in AI research.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_commitment__integrated_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_commitment__integrated_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(ai_a_tr_t5, ai_alignment_commitment__integrated_reading, theater_ratio, 5, 0.31).
narrative_ontology:measurement(ai_a_tr_t10, ai_alignment_commitment__integrated_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(ai_a_tr_t15, ai_alignment_commitment__integrated_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(ai_a_tr_t20, ai_alignment_commitment__integrated_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(ai_a_tr_t25, ai_alignment_commitment__integrated_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(ai_a_tr_t30, ai_alignment_commitment__integrated_reading, theater_ratio, 30, 0.43).
narrative_ontology:measurement(ai_a_tr_t40, ai_alignment_commitment__integrated_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_commitment__integrated_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(ai_a_be_t5, ai_alignment_commitment__integrated_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(ai_a_be_t10, ai_alignment_commitment__integrated_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(ai_a_be_t15, ai_alignment_commitment__integrated_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(ai_a_be_t20, ai_alignment_commitment__integrated_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(ai_a_be_t25, ai_alignment_commitment__integrated_reading, base_extractiveness, 25, 0.69).
narrative_ontology:measurement(ai_a_be_t30, ai_alignment_commitment__integrated_reading, base_extractiveness, 30, 0.7).
narrative_ontology:measurement(ai_a_be_t40, ai_alignment_commitment__integrated_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_commitment__integrated_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(ai_a_su_t5, ai_alignment_commitment__integrated_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(ai_a_su_t10, ai_alignment_commitment__integrated_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(ai_a_su_t15, ai_alignment_commitment__integrated_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement(ai_a_su_t20, ai_alignment_commitment__integrated_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(ai_a_su_t25, ai_alignment_commitment__integrated_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement(ai_a_su_t30, ai_alignment_commitment__integrated_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(ai_a_su_t40, ai_alignment_commitment__integrated_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_commitment__integrated_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_alignment_commitment__integrated_reading, 0.12).
narrative_ontology:affects_constraint(ai_alignment_commitment__integrated_reading, ai_alignment_commitment__safety_control_reading).
narrative_ontology:affects_constraint(ai_alignment_commitment__integrated_reading, ai_alignment_commitment__ethics_justice_reading).
narrative_ontology:affects_constraint(ai_alignment_commitment__integrated_reading, ai_governance_dual_review_boards).
narrative_ontology:affects_constraint(ai_alignment_commitment__integrated_reading, ai_research_funding_allocation).

% DUAL FORMULATION NOTE:
% The ai_alignment_commitment kernel is instantiated by three structurally distinct constraints: safety_control_reading (control-pure, ε-high, forecloses integrated reading if control ε is ground truth), ethics_justice_reading (justice-pure, ε-high, forecloses integrated reading if justice ε is ground truth), and integrated_reading (this story, ε-moderate, coexists with both siblings in practice though logically asserts both-and). The three constraints share a kernel (the contested definition of alignment) and a common founding problem (the false dichotomy) but have incompatible victim sets: control-reading's victims are uncontrolled-AI scenarios; justice-reading's victims are marginalized populations harmed by biased AI; integrated-reading's victims include all three (dual failure modes) plus the research communities forced to coordinate when they prefer single-focus work. Beneficiaries align similarly asymmetrically. The three constraints are linked via network.affects_constraints and should be analyzed as a family, not in isolation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_alignment_commitment__integrated_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
