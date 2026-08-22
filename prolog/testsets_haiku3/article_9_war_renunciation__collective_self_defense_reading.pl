% ============================================================================
% CONSTRAINT STORY: article_9_war_renunciation__collective_self_defense_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_9_war_renunciation__collective_self_defense_reading, []).

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
 *   constraint_id: article_9_war_renunciation__collective_self_defense_reading
 *   human_readable: Article 9 Collective Self-Defense Interpretation
 *   domain: constitutional_law/security_policy
 *
 * SUMMARY:
 *   Japan's Article 9 prohibits war and the maintenance of armed forces. The
 *   strict textual reading forbids any military capacity; the inherent-right
 *   reading permits self-defense forces; the collective-self-defense reading
 *   (this one) extends to overseas military action and alliance participation
 *   when Japan's survival is threatened. This reading instantiates the most
 *   expansive interpretation: it converts a Constitutional limit (war is
 *   forbidden) into an elastic permission (military action is permitted when
 *   survival is threatened and allied). The reading's core claim is that
 *   'survival-threatening' scenarios justify collective action, making the
 *   boundary between self-defense and collective defense negotiable rather
 *   than fixed. The structural effect is mission creep: each reinterpretation
 *   absorbs incremental expansion (overseas deployments, joint operations,
 *   technology sharing), and the victim set includes constituencies relying
 *   on the previous reading's stability. This is NOT the strict pacifist
 *   reading (which forbids all military action) and NOT the inherent-right
 *   reading (which permits only direct self-defense); it is the reading that
 *   makes collective action permissible, thereby undermining the
 *   inherent-right reading's boundary claim.
 *
 * KEY AGENTS:
 *   - Japanese executive (Cabinet, Ministry of Defense): agenda-setter; authors the reading through official doctrine, Cabinet legislative reviews, and military modernization
 *   - Allied security partners (US, South Korea, Australia, India): beneficiaries; gain expanded military partnership and regional security coverage
 *   - Constitutional purists: payers; their reading is progressively displaced by institutional reinterpretation
 *   - Pacifist constituencies: payers and incidental beneficiaries; pay through erosion of constitutional identity, receive regional security
 *   - Inherent-right advocates: excluded; their middle-ground reading is made incoherent by the elastic interpretation
 *   - Strict pacifist advocates: excluded and foreclosed; their reading is logically incompatible with any permissible military action
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__collective_self_defense_reading, 0.68).
domain_priors:suppression_score(article_9_war_renunciation__collective_self_defense_reading, 0.71).
domain_priors:theater_ratio(article_9_war_renunciation__collective_self_defense_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__collective_self_defense_reading, tangled_rope).
narrative_ontology:human_readable(article_9_war_renunciation__collective_self_defense_reading, "Article 9 Collective Self-Defense Interpretation").
narrative_ontology:topic_domain(article_9_war_renunciation__collective_self_defense_reading, "constitutional_law/security_policy").

domain_priors:requires_active_enforcement(article_9_war_renunciation__collective_self_defense_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__collective_self_defense_reading, '17ffaa1b-842e-4c04-9677-a3f7c6291bbf').
narrative_ontology:cs_kernel_codification('17ffaa1b-842e-4c04-9677-a3f7c6291bbf', fixed_text).
narrative_ontology:cs_authority_grounding('17ffaa1b-842e-4c04-9677-a3f7c6291bbf', extraction).
narrative_ontology:cs_interpretation_layer_present('17ffaa1b-842e-4c04-9677-a3f7c6291bbf').
narrative_ontology:cs_reading_relation('17ffaa1b-842e-4c04-9677-a3f7c6291bbf', article_9_war_renunciation__inherent_right_reading, influences).
narrative_ontology:cs_reading_relation('17ffaa1b-842e-4c04-9677-a3f7c6291bbf', article_9_war_renunciation__strict_pacifist_reading, forecloses).
narrative_ontology:cs_axiom('17ffaa1b-842e-4c04-9677-a3f7c6291bbf', foundational, collective_defense_survival_justified).
narrative_ontology:cs_axiom_status(collective_defense_survival_justified, holdable).
narrative_ontology:cs_axiom_grounding('17ffaa1b-842e-4c04-9677-a3f7c6291bbf', collective_defense_survival_justified, instrumental).
narrative_ontology:cs_axiom('17ffaa1b-842e-4c04-9677-a3f7c6291bbf', foundational, elastic_survival_threat_interpretation).
narrative_ontology:cs_axiom_status(elastic_survival_threat_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('17ffaa1b-842e-4c04-9677-a3f7c6291bbf', elastic_survival_threat_interpretation, empirically_contingent).
narrative_ontology:cs_reference_frame('17ffaa1b-842e-4c04-9677-a3f7c6291bbf', post_war_pacifist_settlement).
narrative_ontology:cs_drift_state('17ffaa1b-842e-4c04-9677-a3f7c6291bbf', contemporary_regional_instability_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('17ffaa1b-842e-4c04-9677-a3f7c6291bbf', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__collective_self_defense_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, strategic_alliance_operator).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, regional_stability_advocates).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, constitutional_purists).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, pacifist_constituencies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, allied_security_partners).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, pacifist_constituencies).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__collective_self_defense_reading, survival_threat_doctrine).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__collective_self_defense_reading, elastic_textual_interpretation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Japanese Ministry of Defense and Cabinet Secretariat interpret Article 9 to permit Self-Defense Forces participation in collective security arrangements. They argue that survival-threatening scenarios justify overseas deployments, joint military operations, and alliance-expansion activity. They author official position papers, conduct Cabinet legislative reviews, and enforce this reading through budgeting, military doctrine, and diplomatic alignment with security partners. They benefit from expanded operational capacity and alliance-leadership standing.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, strategic_alliance_operator, agenda_setter,
    institutional, generational, arbitrage, regional).

% The United States and regional allies (South Korea, Australia, India) benefit from Japan's expanded self-defense interpretation: they gain a capable military partner willing to conduct joint operations, forward-deploy assets, and participate in collective defense scenarios. This reading enlarges Japan's de facto military commitments without requiring Japan to formally abrogate Article 9.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, allied_security_partners, beneficiary,
    institutional, generational, mobile, regional).

% Constitutional scholars, civil society advocates, and opposition political parties who read Article 9's text ('war shall never be maintained') as an absolute prohibition on military action, even in self-defense scenarios. They bear the cost of interpretive instability: their preferred reading is progressively displaced by executive reinterpretation; they must constantly litigate, legislate, and mobilize to contest the expanding reading. They lack the institutional standing to author the dominant interpretation.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, constitutional_purists, payer,
    moderate, biographical, constrained, national).

% Segments of the Japanese public and civil society committed to war renunciation as a lived principle. They pay through military taxation, conscription risk (in scenarios of escalation), and erosion of the post-war constitutional settlement they view as foundational to Japanese identity. They receive the benefit of regional security (collective defense reduces direct threats), but at the cost of abandoning the distinctive post-war pacifist constitutional claim.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, pacifist_constituencies, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__collective_self_defense_reading, pacifist_constituencies, beneficiary).

% Scholars and jurists who hold the middle position: Japan retains an inherent right to self-defense but should not expand into collective defense without explicit Constitutional revision. They are excluded from the consensus-building process because this reading (collective self-defense) makes their position incoherent: if survival-threatening collective scenarios are permitted, the boundary between self-defense and collective defense collapses. Their reading's epistemic grounding is undermined by the elastic interpretation.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, inherent_right_advocates, excluded,
    moderate, biographical, identity_locked, national).

% Advocates for the strict reading that Article 9 forbids any armed forces whatsoever. They are structurally excluded from institutional consensus-building because their position is foreclosed by the collective self-defense reading: if defensive military action is permissible (collective self-defense), then some armed forces are constitutional, and their absolute prohibition reading is impossible within the same Constitutional framework.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, strict_pacifist_reading_advocates, excluded,
    powerless, biographical, identity_locked, national).

% Political parties and movements that contest the executive's Article 9 reinterpretation. They introduce legislative countermeasures, demand referenda, and mobilize electoral opposition. They lack the institutional power to author the dominant reading but retain the formal power to propose Constitutional amendments or restrictive legislation.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, opposition_political_coalition, observer,
    moderate, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_9_war_renunciation__collective_self_defense_reading, strategic_alliance_operator).
narrative_ontology:fixing_cost_class(article_9_war_renunciation__collective_self_defense_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Permits Japan to participate in regional security architecture without formal Constitutional revision; resolves the coordination problem of how a pacifist Constitution can accommodate alliance obligations in a contested security environment.
% TRANSFER_FUNCTION: Transfers interpretive authority from the Constitutional text to the executive branch (Cabinet, Ministry of Defense) and allied security partners; transfers security burden from allies (who would otherwise independently defend regional interests) to Japan's Self-Defense Forces; transfers the political cost of pacifism-abandonment from the executive to pacifist constituencies and constitutional purists.
% ABSENT_VOICES: Strict pacifist reading advocates and inherent-right-boundary advocates are structurally excluded from consensus-building: their readings are foreclosed or incoherent within the framework this reading establishes. International pacifist constituencies and academic critics who argue Article 9 represents a historical commitment Japan voluntarily undertook have no institutional seat in the Japanese policy process.
% DISAPPEARANCE_RATIONALE: If this reading were abandoned and replaced by the inherent-right reading (permitting only direct self-defense, not collective), Japan's alliance commitments would require explicit renegotiation, military doctrine would contract to territorial defense, and regional security architecture would reorganize around independent U.S. commitments and multilateral frameworks without Japan's participation. Pacifist constituencies would gain constitutional grounding for their preferred interpretation.
% FOUNDING_PROBLEM: Japan's 1947 Constitutional commitment to war renunciation was written in the context of post-WWII occupation and absolute pacifism. By the 1950s-1960s, Cold War security requirements and alliance obligations created pressure to reconcile pacifism with military capability. The founding problem: how to maintain Constitutional legitimacy while enabling military modernization and alliance participation.
% FOUNDING_PROBLEM_CORROBORATION: The Japanese government and security policy establishment attest the founding problem remains live and the collective self-defense reading is necessary. Constitutional scholars, civil society advocates, and pacifist constituencies dispute this: they argue the problem was a legitimate tension that should have been resolved through Constitutional amendment, not interpretive expansion. Academic historians and international law critics outside Japan attest that the founding problem reflects a clash between post-war pacifism and Cold War realpolitik, not an intrinsic Constitutional ambiguity — the expansion is a choice, not a requirement.
narrative_ontology:disappearance_verdict(article_9_war_renunciation__collective_self_defense_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_9_war_renunciation__collective_self_defense_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__collective_self_defense_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_9_war_renunciation__collective_self_defense_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_9_war_renunciation__collective_self_defense_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_9_war_renunciation__collective_self_defense_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_9_war_renunciation__collective_self_defense_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_9_war_renunciation__collective_self_defense_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts low (0.35 at t=0) because the reading initially appears as a legitimate interpretation of inherent rights. It rises to 0.68 (t=60) as the 'survival-threatening' trigger expands to cover scenarios that do not directly threaten Japan's territory: alliance obligations, regional instability, technological dependencies on secure sea lanes. Theater ratio rises from 0.18 to 0.42, indicating that as extractiveness increases, a growing share of enforcement activity defends the reading's institutional boundaries rather than its original security rationale. The measurement trajectory models interpretive mission creep: each expansion is justified as a survival necessity, but the cumulative effect is to transfer interpretive authority from the Constitution to the executive. Suppression requirement rises from 0.45 to 0.71, capturing the escalating institutional effort required to maintain consensus as the reading diverges from its textual origin.
 *
 * PERSPECTIVAL GAP:
 *   From the Japanese executive and security establishment seat, this reading is a rational accommodation of a constitutional constraint to security reality: survival-threatening scenarios exist, alliances require reciprocal military commitment, and the reading remains textually defensible. From the Constitutional purist and pacifist constituencies seats, the same reading operates as interpretive capture: the executive has unilaterally rewritten the Constitution's foundational commitment without amendment. From the allied security partner seat, it is a beneficial expansion of Japan's de facto military role. From the inherent-right boundary advocate seat, it is a category collapse: if collective self-defense is permitted, the line between self-defense and alliance warfare dissolves. The engine computes these divergences from the structural data: beneficiary seats (executive, allies) derive low directionality; payer and excluded seats (purists, pacifists, inherent-right advocates) derive high directionality or exclusion.
 *
 * DIRECTIONALITY LOGIC:
 *   Strategic alliance operator: beneficiary, institutional power, arbitrage exit → low directionality (d ≈ 0.15), subsidy structure. Allied security partners: beneficiary, institutional power, mobile exit → low directionality (d ≈ 0.20). Constitutional purists: payer, moderate power, constrained exit (institutional position tied to constitutional interpretation), identity-locked on text fidelity → high directionality (d ≈ 0.78). Pacifist constituencies: dual-positioned (payer + incidental beneficiary), organized power, constrained exit (domestic constituencies cannot exit Japan), identity-locked on pacifism → moderate-high directionality (d ≈ 0.62). Inherent-right advocates: excluded, moderate power, identity-locked on boundary maintenance → excluded from directionality derivation, but the exclusion itself is structural: their reading's coherence requires a fixed boundary that this reading dissolves.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to reconcile pacifism with alliance obligations) was live and genuine in the 1950s-1960s when Cold War security requirements first tested Article 9. The collective-self-defense reading's claim is that the problem remains live and the reading addresses it. However, the measurement trajectory and the stakeholder structure suggest mandate atrophy: the founding problem (Cold War bipolarity, acute security dependency) has substantially degraded. The survival-threatening trigger now encompasses scenarios that are not existential threats to Japan itself but rather regional instability or alliance-maintenance concerns. The reading's persistence is increasingly defended by theater (institutional consensus-building, diplomatic alignment, security establishment authority) rather than by the problem it was built to solve. This does not make it a piton — pacifist constituencies still mount real resistance, and the agenda-setter (the executive) actively enforces the reading — but the gap between founding problem and current mission is widening. A mandate-atrophy classification would require higher theater_ratio (current 0.42); a true piton would require the constraint to persist despite active neglect by the enforcer, which is not the case here. The classification remains tangled_rope: the constraint genuinely coordinates regional security (coordination function present), but the asymmetry has deepened as the founding problem's urgency has diminished.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    survival_threat_elasticity,
    'What constitutes a ''survival-threatening'' scenario under this reading, and does the definition remain stable as regional security dynamics evolve?',
    'Institutional doctrine review and comparative analysis with allied security concepts; examination of Cabinet legislative review decisions over time to track whether survival-threat thresholds shift.',
    'If the definition remains tightly bounded (direct military threat to Japanese territory or population), the reading''s coherence is preserved and extractiveness should stabilize. If the definition expands (regional instability, technology dependency, alliance obligation) without formal redefinition, the reading operates as elastic interpretation and extractiveness continues rising — evidence of mandatrophy in the founding problem and false-necessity doctrine.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(survival_threat_elasticity, empirical, 'Whether the survival-threat definition is stable or continuously elastic.').

omega_variable(
    inherent_right_boundary_coherence,
    'Is the boundary between inherent self-defense (authorized by all three readings) and collective self-defense (authorized by this reading alone) structurally defensible, or does this reading dissolve the distinction?',
    'Constitutional analysis comparing the three readings'' definitions and legal precedent tracking; examination of policy documents to determine whether the Ministry of Defense distinguishes self-defense operations from collective operations or treats them as functionally identical.',
    'If the boundary remains doctrinally clear, the inherent-right reading remains a live alternative and coexistence is coherent. If the boundary dissolves (this reading treats collective self-defense as merely an instance of self-defense), the inherent-right reading is effectively foreclosed and the collective-self-defense reading becomes the only permissible constitutional interpretation — evidence that this reading coexists with inherent-right only nominally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inherent_right_boundary_coherence, conceptual, 'Whether the self-defense / collective-self-defense distinction remains coherent under this reading.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.71) primarily structural (institutional power asymmetry, electoral politics making pacifist amendments difficult) or internalized (pacifist constituencies have internalized the necessity of alliance military participation)?',
    'Post-consensus analysis: if pacifist constituencies were empowered to amend the Constitution (institutional barrier removed), would they? Polling, electoral behavior, and civil society mobilization data; longitudinal analysis of generational shifts in pacifism.',
    'If suppression is primarily structural, removing the institutional barrier (electoral threshold, supermajority requirement for amendment) would enable constitutional change and reclassify the reading as less extractive. If suppression is internalized, the barrier''s removal would not suffice and the reading''s extractive character is self-sustaining — the constraint carries its own suppression with it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of pacifist contestation is structural or internalized.').

omega_variable(
    kernel_reading_alternative_framings,
    'Is the contested kernel ''Article 9 war renunciation'' one kernel with three readings, or does the kernel-reading framework under-determine the constitutional contest? Could the contest be better modeled as two separate kernels (one on the maintenance of armed forces, one on overseas military operations)?',
    'Constitutional structure analysis: do all three readings accept the same textual kernel, or do they diverge on what the kernel IS? If inherent-right and collective-self-defense readings both treat ''war'' as the unit of prohibition and accept that self-defense is not war, they share the kernel; if strict-pacifist reading treats ''shall never be maintained'' as governing the institution (armed forces) rather than the activity (war), the kernels differ.',
    'If the kernels are actually distinct, the current three-reading family model understates the constitutional fragmentation: there are two separate contests (what is ''war''? what is ''maintenance of forces?'') that happen to center on the same text. This affects how the engine models kernel-level coherence and axiom consistency.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_alternative_framings, conceptual, 'Whether the three readings share one kernel or whether the kernel boundaries are themselves contested.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__collective_self_defense_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(arti_tr_t0, observed).
narrative_ontology:measurement(arti_tr_t10, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement_basis(arti_tr_t10, observed).
narrative_ontology:measurement(arti_tr_t20, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 20, 0.31).
narrative_ontology:measurement_basis(arti_tr_t20, observed).
narrative_ontology:measurement(arti_tr_t30, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement_basis(arti_tr_t30, observed).
narrative_ontology:measurement(arti_tr_t45, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 45, 0.41).
narrative_ontology:measurement_basis(arti_tr_t45, observed).
narrative_ontology:measurement(arti_tr_t60, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 60, 0.42).
narrative_ontology:measurement_basis(arti_tr_t60, projected).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(arti_be_t0, observed).
narrative_ontology:measurement(arti_be_t10, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement_basis(arti_be_t10, observed).
narrative_ontology:measurement(arti_be_t20, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement_basis(arti_be_t20, observed).
narrative_ontology:measurement(arti_be_t30, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 30, 0.61).
narrative_ontology:measurement_basis(arti_be_t30, observed).
narrative_ontology:measurement(arti_be_t45, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 45, 0.66).
narrative_ontology:measurement_basis(arti_be_t45, observed).
narrative_ontology:measurement(arti_be_t60, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement_basis(arti_be_t60, projected).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(arti_su_t0, observed).
narrative_ontology:measurement(arti_su_t10, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement_basis(arti_su_t10, observed).
narrative_ontology:measurement(arti_su_t20, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 20, 0.61).
narrative_ontology:measurement_basis(arti_su_t20, observed).
narrative_ontology:measurement(arti_su_t30, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement_basis(arti_su_t30, observed).
narrative_ontology:measurement(arti_su_t45, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 45, 0.7).
narrative_ontology:measurement_basis(arti_su_t45, observed).
narrative_ontology:measurement(arti_su_t60, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 60, 0.71).
narrative_ontology:measurement_basis(arti_su_t60, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__collective_self_defense_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_9_war_renunciation__collective_self_defense_reading, 0.12).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, article_9_war_renunciation__inherent_right_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, article_9_war_renunciation__strict_pacifist_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, us_japan_security_treaty__mutual_defense_obligation).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, regional_security_architecture__collective_defense_framework).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel article_9_war_renunciation. Sibling readings instantiate separate constraint stories: inherent_right_reading (permissive to self-defense, restrictive to collective action) and strict_pacifist_reading (prohibitive to all armed forces). The collective-self-defense reading coexists with inherent-right but influences strict-pacifist; it depends causally on US-Japan treaty obligations for its operational coherence. This reading represents the most expansive interpretation and therefore the highest extractiveness from pacifist constituencies' structural position.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_9_war_renunciation__collective_self_defense_reading, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
