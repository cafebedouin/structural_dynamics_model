% ============================================================================
% CONSTRAINT STORY: article_9_war_renunciation__strict_pacifist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_9_war_renunciation__strict_pacifist_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: article_9_war_renunciation__strict_pacifist_reading
 *   human_readable: Article 9 Strict Pacifist Reading: Absolute War Renunciation
 *   domain: constitutional/security
 *
 * SUMMARY:
 *   Article 9 of the Japanese Constitution states: 'Aspiring sincerely to an
 *   international peace based on justice and order, the Japanese people
 *   forever renounce war as a sovereign right of the nation and the threat or
 *   use of force as a means of settling international disputes. In order to
 *   accomplish the aim of the preceding paragraph, land, sea, and air forces,
 *   as well as other war potential, shall never be maintained.' The strict
 *   pacifist reading interprets this language as a categorical prohibition on
 *   any organized military capacity, including defensive forces. Under this
 *   reading, Japan's Self-Defense Forces exist in a state of constitutional
 *   violation (though courts have upheld them through interpretive
 *   necessity). This reading instantiates a Tangled Rope: it solves a genuine
 *   coordination problem (post-war peace commitment, anchor against
 *   remilitarization), but it also extracts state security autonomy and locks
 *   Japan into alliance dependence. The constraint is actively enforced
 *   through constitutional court interpretation, domestic pacifist political
 *   mobilization, and implicit US alliance enforcement. The measurement
 *   series (interval 0–80, representing 1945–2025) show extractiveness rising
 *   from 0.35 to 0.68 as the founding constraint problem (preventing
 *   remilitarism) declined in urgency but the constraint persisted, and as
 *   regional security conditions deteriorated, making the constraint's cost
 *   more visible. Theater ratio rises from 0.18 to 0.44, indicating that
 *   enforcement increasingly focuses on defending the reading textually while
 *   the actual security coordination function (what Japan needs from a
 *   military, what US guarantees provide) shifts.
 *
 * KEY AGENTS:
 *   - Japanese state security establishment (payer, institutional) — structurally victimized by the reading, bears security dependence cost
 *   - Pacifist constituencies (beneficiary, organized) — benefit from the reading's codification of non-militarism, have domestic political power
 *   - US security guarantor (agenda-setter, institutional) — enforces through alliance commitment, benefits from Japan's strategic dependence
 *   - Constitutional court and interpreters (agenda-setter, institutional) — adjudicate the reading, balance text against state necessity
 *   - Regional security competitors (excluded, institutional) — benefit from Japan's military incapacity but excluded from negotiation
 *   - International peace norm advocates (beneficiary, organized global) — benefit from Japan as a vindicating case for absolute pacifism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__strict_pacifist_reading, 0.68).
domain_priors:suppression_score(article_9_war_renunciation__strict_pacifist_reading, 0.72).
domain_priors:theater_ratio(article_9_war_renunciation__strict_pacifist_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__strict_pacifist_reading, tangled_rope).
narrative_ontology:human_readable(article_9_war_renunciation__strict_pacifist_reading, "Article 9 Strict Pacifist Reading: Absolute War Renunciation").
narrative_ontology:topic_domain(article_9_war_renunciation__strict_pacifist_reading, "constitutional/security").

domain_priors:requires_active_enforcement(article_9_war_renunciation__strict_pacifist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__strict_pacifist_reading, '1f8f4191-e0a8-45e8-8f6a-195d03480d52').
narrative_ontology:cs_kernel_codification('1f8f4191-e0a8-45e8-8f6a-195d03480d52', fixed_text).
narrative_ontology:cs_authority_grounding('1f8f4191-e0a8-45e8-8f6a-195d03480d52', lineage).
narrative_ontology:cs_interpretation_layer_present('1f8f4191-e0a8-45e8-8f6a-195d03480d52').
narrative_ontology:cs_reading_relation('1f8f4191-e0a8-45e8-8f6a-195d03480d52', article_9_war_renunciation__inherent_right_reading, forecloses).
narrative_ontology:cs_reading_relation('1f8f4191-e0a8-45e8-8f6a-195d03480d52', article_9_war_renunciation__collective_self_defense_reading, forecloses).
narrative_ontology:cs_axiom('1f8f4191-e0a8-45e8-8f6a-195d03480d52', foundational, categorical_military_prohibition).
narrative_ontology:cs_axiom_status(categorical_military_prohibition, holdable).
narrative_ontology:cs_axiom_grounding('1f8f4191-e0a8-45e8-8f6a-195d03480d52', categorical_military_prohibition, deontological).
narrative_ontology:cs_axiom('1f8f4191-e0a8-45e8-8f6a-195d03480d52', foundational, perpetual_pacifism_constitutional_imperative).
narrative_ontology:cs_axiom_status(perpetual_pacifism_constitutional_imperative, holdable).
narrative_ontology:cs_axiom_grounding('1f8f4191-e0a8-45e8-8f6a-195d03480d52', perpetual_pacifism_constitutional_imperative, conventional).
narrative_ontology:cs_reference_frame('1f8f4191-e0a8-45e8-8f6a-195d03480d52', occupation_era_pacifist_constitution).
narrative_ontology:cs_drift_state('1f8f4191-e0a8-45e8-8f6a-195d03480d52', contemporary_post_cold_war, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1f8f4191-e0a8-45e8-8f6a-195d03480d52', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__strict_pacifist_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, pacifist_constituencies).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, international_peace_norm_proponents).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, japanese_state_security_autonomy).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, alliance_dependent_military_capacity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, us_security_guarantor).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, international_peace_norm_advocates).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, japanese_state_security_establishment).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__strict_pacifist_reading, absolute_pacifism_constitutional_doctrine).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__strict_pacifist_reading, war_prohibition_as_categorical_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Faces a constitutional reading that forbids organized military capacity entirely, including defensive forces. Bears the cost of security dependence on the US alliance (cannot independently develop naval or air forces for territorial defense, cannot develop deterrence independent of US commitment, remains vulnerable to US abandonment or coercion). The constraint forces reliance on alliance politics rather than autonomous defense strategy. Must operate the Self-Defense Forces in a state of constitutional ambiguity (permitted by necessity but prohibited by text). Exit would require supermajority constitutional amendment (practically impossible given pacifist constituency power).
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, japanese_state_security_establishment, payer,
    institutional, generational, constrained, national).

% Benefit from the constitutional reading because it codifies their normative commitment to absolute non-militarism as supreme law and blocks institutional paths to rearmament. The reading vindicates their political worldview and gives them a textual shield against military expansion. Can mobilize domestically to defend the reading and can shift between political parties supporting non-militarism, but the constitutional interpretation itself constrains their individual choices (cannot reverse the reading through ordinary politics). Their benefit is sustained across generations.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, pacifist_constituencies, beneficiary,
    organized, generational, mobile, national).

% Enforces and sustains the constraint through maintenance of the bilateral security alliance, making Japan's security dependence structurally viable. Benefits from Japan's military incapacity (Japan cannot independently develop competitive military capacity, remains locked into the bilateral relationship, cannot free-ride on alternative alliances). Administers the constraint through diplomatic support for the strict reading and through the mutual security treaty framework. Has exit options (could withdraw alliance support or encourage Japanese reinterpretation in changed strategic conditions) but benefits from sustained dependence.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, us_security_guarantor, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__strict_pacifist_reading, us_security_guarantor, beneficiary).

% Adjudicates the textual meaning of Article 9 and has repeatedly affirmed the strict pacifist reading in landmark decisions, most notably upholding the Self-Defense Forces' constitutionality despite the apparent categorical prohibition. Interprets the literal language as binding and categorical. Bears pressure from both security establishment advocates (pushing reinterpretation) and pacifist constituencies (demanding fidelity to text). Exit through reinterpretation would be politically and institutionally costly (reversing precedent, legitimizing rearmament). Constrained by the need to maintain institutional authority and democratic legitimacy.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, constitutional_court, agenda_setter,
    institutional, generational, constrained, national).

% Are excluded from the constraint's negotiation by its design: Japan cannot respond to their military buildup with comparable independent capacity, which shifts regional balance-of-power dynamics in their favor. China and Russia benefit passively from Japan's structural military inferiority, but have no say in the constraint's administration and cannot unilaterally alter it. Would benefit from Japan's rearmament (competitive deterrence) but are not party to the constraint's enforcement.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, regional_security_competitors, excluded,
    institutional, generational, trapped, regional).

% Benefit from Japan as a vindicating case study for absolute non-militarism. Cite Article 9 and Japan's strict interpretation as proof that categorical war renunciation is institutionally sustainable and legitimate, strengthening their advocacy for similar commitments elsewhere. Have no formal enforcement power but their support legitimates the reading politically and internationally. Can organize transnational campaigns for peace norms but cannot compel Japan's adherence.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, international_peace_norm_advocates, beneficiary,
    organized, civilizational, mobile, global).

% Would argue for constitutional reinterpretation to permit independent military capacity, citing regional threats and strategic autonomy concerns. Are excluded from the dominant reading's framework because 'never be maintained' is treated as categorical, leaving no legitimate institutional path for rearmament without supermajority constitutional amendment (they lack the political power to achieve amendment). Can advocate publicly but cannot bypass the constitutional constraint through ordinary political process.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, rearmament_advocates, excluded,
    powerful, biographical, constrained, national).

% Examines the constraint's structural effects: how the strict reading generates security dependence, how it vindicates pacifist constituencies while extracting from state autonomy, and how it persists despite sustained pressure from the security establishment. Observes that the constraint's persistence rides on three conditions: US alliance commitment, pacifist domestic political coalition strength, and constitutional court adherence to the strict reading. Monitors the rising theater ratio as a signal of potential degradation toward Piton classification.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, analytical_observer, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_9_war_renunciation__strict_pacifist_reading, pacifist_constituencies).
narrative_ontology:fixing_cost_class(article_9_war_renunciation__strict_pacifist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a durable, constitutionally binding peace commitment that coordinates international norms against Japanese militarism and locks Japan into a peaceful foreign policy regardless of future domestic political shifts. Solves the post-war coordination problem of credibly committing to non-aggression in perpetuity.
% TRANSFER_FUNCTION: Transfers security autonomy from the Japanese state to the US security guarantor (through alliance dependence) and to pacifist constituencies (through constitutional lock-in). Moves decision-making power over Japan's military posture from domestic security strategists to constitutional courts and international alliance structures.
% ABSENT_VOICES: Rearmament advocates and independent defense strategists are structurally excluded by the strict reading's categorical language—they have no legitimate textual path to reinterpretation without supermajority constitutional amendment. Regional security competitors (China, Russia) benefit from Japan's military inferiority but are not party to the constraint's negotiation and have no voice in its administration. A security establishment unconstrained by alliance politics would object to the reading but lacks domestic political power for amendment.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight and Japan reinterpreted Article 9 to permit independent military capacity, the regional balance of power would shift dramatically within years: Japan would develop naval and air forces, regional competitors would accelerate militarization in response, the US-Japan alliance would renegotiate, and the international norm against Japanese militarism would collapse. The constraint's removal would trigger cascading institutional reorganization across East Asia and within Japan's domestic political economy.
% FOUNDING_PROBLEM: Post-1945 Japan faced international distrust of its military capability and capacity for remilitarism; the occupying Allied powers and regional neighbors required categorical disarmament and a constitutional commitment to perpetual peace. Article 9 was written to foreclose any institutional path to Japanese militarism and to anchor Japan's integration into a US-led security architecture.
% FOUNDING_PROBLEM_CORROBORATION: Allied occupation authorities (1945–1952) and Japanese pacifist movements attest the founding problem was preventing Japanese remilitarization and integrating Japan into a permanent peace order. Japanese defense strategists and independent security analysts attest the founding problem is substantially solved (Japan has not engaged in military aggression since 1945, and its democratic integration is secure) and the strict reading now persists as a structural lock-in rather than a necessary restraint. Post-Cold War security analysis from outside Japan (NATO strategists, US Department of Defense) observes that Japan's military incapacity is now a handicap constraining regional stability response, not a safeguard against militarism—this corroboration comes from seats outside the pacifist beneficiary coalition.
narrative_ontology:disappearance_verdict(article_9_war_renunciation__strict_pacifist_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_9_war_renunciation__strict_pacifist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__strict_pacifist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(article_9_war_renunciation__strict_pacifist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_9_war_renunciation__strict_pacifist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_9_war_renunciation__strict_pacifist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_9_war_renunciation__strict_pacifist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at interval end) is high because the constraint transfers security autonomy from Japan to external actors (US ally, regional balance of power) and because the cost of the transfer has grown as regional threats increased post-Cold War. The reading's specificity ('never be maintained') leaves zero legitimate interpretive space for defensive capacity, which amplifies extraction compared to more flexible constitutional language. Suppression (0.72) is high because enforcement relies on active constitutional court vigilance against reinterpretation, domestic political coalition-maintenance, and implicit US alliance pressure. The constraint is regularly challenged by rearmament advocates, and enforcement machinery must continuously defend against reinterpretation. Theater ratio (0.44) rises over the interval because the founding problem (preventing Japanese remilitarism in 1945–1960) is substantially solved, yet enforcement continues with increasing rhetorical emphasis on the text's categorical language rather than on preventing actual remilitarism. Accessibility collapse (0.78) is high because once Japan internalized the reading, alternative military-capacity paths became politically illegitimate; even though the text is contested (sibling readings exist), the strict reading dominates institutions, making the path back to independent military capacity extremely difficult without supermajority constitutional amendment. Resistance (0.61) is moderate because the security establishment continuously pushes for reinterpretation, but lacks supermajority support; pacifist constituencies actively defend the reading. The measurement trajectory shows extractiveness rising as the founding problem declined, suggesting the constraint persists partly through institutional inertia rather than active necessity—a Piton-candidate signal. However, the constraint is still actively enforced (high suppression), which distinguishes it from a pure Piton. Hence: Tangled Rope, with a Piton trajectory emerging.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (Japanese security establishment) computes a Snare from its position: pure extraction from autonomy, forced constraint, no real coordination benefit. The beneficiary seats (pacifist constituencies, US guarantor) compute a Rope or Tangled Rope: genuine coordination anchoring peace, with acceptable or beneficial extraction. Constitutional courts compute a Mountain (the text is categorical, this is just law) until regional security conditions force reinterpretation. The engine computes these divergences from the structural data authored here (beneficiary/victim + exit + power + time_horizon); the interpretation of 'the type is what we claim' vs. 'the type is what the metrics show' is precisely where false-summit detection operates.
 *
 * DIRECTIONALITY LOGIC:
 *   The strict pacifist reading produces asymmetric directionality across seats. The Japanese security establishment (powerful, institutional, national) faces d~0.85 (full target): bears the cost of military incapacity, constrained exit (must reinterpret through supermajority amendment or break the alliance), trapped in the constraint through constitutional and alliance structures. Pacifist constituencies (organized, national, biographical) face d~0.15 (beneficiary): they benefit politically and ideologically, have mobile exit (can shift between political parties supporting the reading), and their time horizon is biographical (they defend the reading generationally, but individuals can switch sides). The US security guarantor (institutional, regional, generational) faces d~0.25 (beneficiary): benefits from Japan's strategic dependence, arbitrary exit (can withdraw or encourage reinterpretation), and maintains the arrangement through alliance administration. Regional competitors (institutional, regional, generational) face d~0.35 (beneficiary through exclusion): benefit from Japan's military inferiority, but exit is trapped (they cannot choose to include Japan in their security calculations). Constitutional courts (institutional, national, generational) face d~0.50 (symmetric): must balance textual fidelity against state necessity, can exit through reinterpretation (but at enormous political and institutional cost), and time horizon is generational (the reading persists through successive generations of judges). No directionality override is needed because the structural derivation (beneficiary/victim + exit_options) produces accurate directionalities without adjustment.
 *
 * MANDATROPHY ANALYSIS:
 *   The strict pacifist reading faces a mandatrophy question: the founding problem (preventing Japanese remilitarism post-1945) appears substantially solved by 2025 (Japan is a stable democracy integrated into the democratic alliance structure), yet the constraint persists. If founding_problem_status=dead and disappearance_verdict=world_rearranges, the constraint is a zombie—it would reorganize significant institutional relationships if removed, but it persists despite the founding justification no longer applying. The rising theater_ratio (0.18 to 0.44) supports this: enforcement increasingly focuses on defending the textual reading textually, rather than preventing actual remilitarism. However, the constraint is not purely theater: active suppression (0.72) of rearmament advocates and continuous constitutional court vigilance show real enforcement. The classification as Tangled Rope (rather than Piton) is justified as long as the constraint solves a live coordination problem (it does: binding Japan to pacifism anchors regional stability and international norms) and extraction remains a genuine structural feature (it does: Japan's security autonomy is transferred). However, a future measurement point where theater_ratio exceeds suppression_requirement would indicate the constraint is degrading into pure theater, signaling Piton reclassification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_natural_law_vs_constructed,
    'Is the strict pacifist reading a natural inference from Article 9''s plain language, or a constructed interpretive choice that benefits identifiable constituencies?',
    'Comparative constitutional analysis: examine whether other democratic constitutions with analogous language reach the same reading, or whether they interpret defensive military capacity as compatible with war renunciation. If sibling readings produce equally textually defensible interpretations, the choice is constructed rather than natural.',
    'If the reading is constructed, it may be reclassified as a Snare (pure extraction) rather than a Tangled Rope. If it is natural, the classification stands and extraction is incidental to coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_natural_law_vs_constructed, conceptual, 'Whether the strict pacifist reading is texturally inevitable or an interpretive construction.').

omega_variable(
    security_autonomy_victimization,
    'Is the Japanese state security establishment truly a victim of this constraint, or does Japan benefit from deniability by outsourcing defense while maintaining alignment?',
    'Structural analysis: if Japan has consistently sought constitutional amendment or military expansion, it is a genuine victim. If Japan has expanded defense capacity incrementally while defending the reading diplomatically, it is complicit in maintaining the constraint for reputational benefit.',
    'If complicit, the victim-set changes and the constraint may be reclassified. If genuinely constrained, the Tangled Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_autonomy_victimization, empirical, 'Whether state security autonomy is genuinely constrained or complicit in the arrangement.').

omega_variable(
    founding_problem_obsolescence,
    'Has the founding problem (preventing Japanese remilitarism) remained live, or did it die as Japan integrated into the democratic alliance?',
    'Historical analysis of remilitarization pressure over time. If the problem died but the constraint persists (high theater_ratio, rising with time), the constraint is a zombie Piton.',
    'Coupled with the theater ratio trajectory, this informs whether the constraint is an active Tangled Rope or a degraded Piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the founding constraint remains necessary or is obsolete.').

omega_variable(
    us_enforcement_structure,
    'Is US alliance commitment sustaining the reading, or is the reading sustained by domestic pacifist coalitions and constitutional interpretation?',
    'Counterfactual analysis: would the US oppose Japanese constitutional reinterpretation, or would it support rearmament in changed strategic conditions? Historical precedent shows US has encouraged Japanese rearmament in some contexts.',
    'If US-enforced, the beneficiary computation changes. If domestically sustained, the directionality stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_enforcement_structure, empirical, 'Whether US enforcement or domestic factors drive the reading''s persistence.').

omega_variable(
    sibling_reading_logical_structure,
    'Do the strict pacifist, inherent-right, and collective-self-defense readings logically foreclose each other, coexist, or influence each other''s plausibility?',
    'Textual and institutional analysis: if the readings are logically incompatible (cannot be held in the same framework), document which pairs foreclose each other. If they coexist (different parties hold different readings simultaneously), document the seats that adopt each reading. If they influence (adoption of one changes epistemic conditions for others), trace the causal structure.',
    'Documents the constraint''s position within the kernel family and informs the classification of sibling constraints when they are authored.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_logical_structure, conceptual, 'Logical relationships among sibling readings of the Article 9 kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__strict_pacifist_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(arti_tr_t0, observed).
narrative_ontology:measurement(arti_tr_t10, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement_basis(arti_tr_t10, observed).
narrative_ontology:measurement(arti_tr_t20, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement_basis(arti_tr_t20, observed).
narrative_ontology:measurement(arti_tr_t35, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 35, 0.35).
narrative_ontology:measurement_basis(arti_tr_t35, observed).
narrative_ontology:measurement(arti_tr_t50, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 50, 0.4).
narrative_ontology:measurement_basis(arti_tr_t50, observed).
narrative_ontology:measurement(arti_tr_t65, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 65, 0.43).
narrative_ontology:measurement_basis(arti_tr_t65, observed).
narrative_ontology:measurement(arti_tr_t80, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 80, 0.44).
narrative_ontology:measurement_basis(arti_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(arti_be_t0, observed).
narrative_ontology:measurement(arti_be_t10, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement_basis(arti_be_t10, observed).
narrative_ontology:measurement(arti_be_t20, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 20, 0.51).
narrative_ontology:measurement_basis(arti_be_t20, observed).
narrative_ontology:measurement(arti_be_t35, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 35, 0.59).
narrative_ontology:measurement_basis(arti_be_t35, observed).
narrative_ontology:measurement(arti_be_t50, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 50, 0.64).
narrative_ontology:measurement_basis(arti_be_t50, observed).
narrative_ontology:measurement(arti_be_t65, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 65, 0.68).
narrative_ontology:measurement_basis(arti_be_t65, observed).
narrative_ontology:measurement(arti_be_t80, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 80, 0.68).
narrative_ontology:measurement_basis(arti_be_t80, observed).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(arti_su_t0, observed).
narrative_ontology:measurement(arti_su_t10, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement_basis(arti_su_t10, observed).
narrative_ontology:measurement(arti_su_t20, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement_basis(arti_su_t20, observed).
narrative_ontology:measurement(arti_su_t35, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 35, 0.65).
narrative_ontology:measurement_basis(arti_su_t35, observed).
narrative_ontology:measurement(arti_su_t50, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 50, 0.7).
narrative_ontology:measurement_basis(arti_su_t50, observed).
narrative_ontology:measurement(arti_su_t65, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 65, 0.72).
narrative_ontology:measurement_basis(arti_su_t65, observed).
narrative_ontology:measurement(arti_su_t80, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 80, 0.72).
narrative_ontology:measurement_basis(arti_su_t80, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__strict_pacifist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, article_9_war_renunciation__inherent_right_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, article_9_war_renunciation__collective_self_defense_reading).

% DUAL FORMULATION NOTE:
% The Article 9 kernel admits three readings with structurally distinct ε values and victim/beneficiary sets. The strict_pacifist_reading (this file) treats 'never be maintained' as categorical; extractiveness is high (0.68) and extraction flows from state security autonomy to alliance dependence and pacifist constituencies. The inherent_right_reading interprets 'war' as prohibiting aggressive action only, permitting defensive capacity; extractiveness would be lower and state security autonomy would be a beneficiary. The collective_self_defense_reading extends inherent right to alliance contexts; extractiveness and beneficiary structure would differ again. These are NOT alternate measurements of one constraint—they are three logically distinct constraints instantiated by different readings of the same kernel. Each has its own ε, its own stakeholder situation, and its own classification. They are linked here via network.affects_constraints because adoption of one reading influences (and partly forecloses) the others, and because they are held simultaneously by different institutional actors within Japan.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
