% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__graduated_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__graduated_sovereignty, []).

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
 *   constraint_id: westphalian_sovereignty__graduated_sovereignty
 *   human_readable: Graduated Sovereignty: State Capacity as Legitimacy Threshold
 *   domain: international_law/political_philosophy/global_governance
 *
 * SUMMARY:
 *   The graduated sovereignty reading of the Westphalian kernel treats
 *   sovereignty legitimacy as a spectrum determined by measurable state
 *   capacity and institutional governance compliance. Under this doctrine,
 *   states with weak administrative capacity, poor rule-of-law metrics, or
 *   high corruption indices are reclassified as having diminished sovereignty
 *   legitimacy, triggering external institutional oversight, conditional aid,
 *   and justification for humanitarian or security intervention. This reading
 *   emerged from development economics (capacity-building paradigm) and
 *   international law (responsibility to protect doctrine) in the late 20th
 *   century and has become institutionalized through IMF/World Bank
 *   conditionality, UN peacekeeping mandates, and NATO intervention doctrine.
 *   The constraint operates as a snare for low-capacity states — they are
 *   trapped in a framework where external actors unilaterally classify their
 *   legitimacy based on metrics set by those same external actors. It
 *   operates as rope for high-capacity states and development institutions,
 *   who experience it as enabling coordination around governance standards.
 *   The theater ratio has increased from 0.42 to 0.68 over the 30-year
 *   interval, indicating that the constraint's performative content has
 *   grown: ceremonial affirmation of sovereign equality coexists with
 *   normalized external capacity assessment. The extractiveness has risen
 *   from 0.35 to 0.58, reflecting increasing institutional conditionality on
 *   low-capacity states. This is one reading of a contested kernel: the
 *   absolute_sovereignty reading insists on non-interference and formal
 *   equality; the conditional_sovereignty reading grounds conditionality on
 *   specific criteria (human rights, terrorism, genocide) rather than
 *   capacity metrics. The graduated_sovereignty reading treats capacity
 *   assessment as the legitimacy basis and is currently institutionalized.
 *
 * KEY AGENTS:
 *   - Low-Capacity States (powerless/trapped): Primary victims — face reclassification-driven institutional control, austerity conditions, and justification for intervention. Somalia, Yemen, Libya, Haiti exemplify states trapped in graduated sovereignty framework.
 *   - High-Capacity States (institutional/arbitrage): Primary beneficiaries — use capacity metrics to justify conditional aid, institutional oversight, and intervention authority. G7, permanent Security Council members, and donor states.
 *   - International Development Institutions (institutional/arbitrage): Beneficiaries and enforcers — IMF, World Bank, UN system use capacity metrics as justification for conditionality and structural adjustment programs. Set the metrics themselves.
 *   - Regional Coalitions (organized/constrained): Secondary victims with organized resistance — African Union, ASEAN, Non-Aligned Movement contest the graduated sovereignty reading and articulate absolute or conditional alternatives. Constrained by capital dependence and institutional linkage.
 *   - State Officials in Low-Capacity States (moderate/constrained): Individual victims navigating reclassification — government officials face career risk from external assessment changes, conditionality requirements, and intervention justification.
 *   - Westphalian Doctrine Itself (institutional/arbitrage): Vestigial institutional authority — UN Charter's equal sovereignty principle persists in treaty language and ceremonial rhetoric but is functionally degraded by normalized capacity assessment.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__graduated_sovereignty, 0.58).
domain_priors:suppression_score(westphalian_sovereignty__graduated_sovereignty, 0.62).
domain_priors:theater_ratio(westphalian_sovereignty__graduated_sovereignty, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, extractiveness, 0.58).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__graduated_sovereignty, snare).
narrative_ontology:human_readable(westphalian_sovereignty__graduated_sovereignty, "Graduated Sovereignty: State Capacity as Legitimacy Threshold").
narrative_ontology:topic_domain(westphalian_sovereignty__graduated_sovereignty, "international_law/political_philosophy/global_governance").

domain_priors:requires_active_enforcement(westphalian_sovereignty__graduated_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__graduated_sovereignty, 'd6e1873d-1f67-4984-b3b3-f176ec92bfe0').
narrative_ontology:cs_kernel_codification('d6e1873d-1f67-4984-b3b3-f176ec92bfe0', formalized).
narrative_ontology:cs_authority_grounding('d6e1873d-1f67-4984-b3b3-f176ec92bfe0', extraction).
narrative_ontology:cs_interpretation_layer_present('d6e1873d-1f67-4984-b3b3-f176ec92bfe0').
narrative_ontology:cs_reading_relation('d6e1873d-1f67-4984-b3b3-f176ec92bfe0', westphalian_sovereignty__absolute_sovereignty, coexists_with).
narrative_ontology:cs_reading_relation('d6e1873d-1f67-4984-b3b3-f176ec92bfe0', westphalian_sovereignty__conditional_sovereignty, influences).
narrative_ontology:cs_axiom('d6e1873d-1f67-4984-b3b3-f176ec92bfe0', foundational, state_capacity_determines_legitimacy).
narrative_ontology:cs_axiom_status(state_capacity_determines_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('d6e1873d-1f67-4984-b3b3-f176ec92bfe0', state_capacity_determines_legitimacy, empirically_contingent).
narrative_ontology:cs_axiom('d6e1873d-1f67-4984-b3b3-f176ec92bfe0', foundational, external_assessment_authority_legitimate).
narrative_ontology:cs_axiom_status(external_assessment_authority_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('d6e1873d-1f67-4984-b3b3-f176ec92bfe0', external_assessment_authority_legitimate, conventional).
narrative_ontology:cs_reference_frame('d6e1873d-1f67-4984-b3b3-f176ec92bfe0', capacity_based_legitimacy_assessment).
narrative_ontology:cs_drift_state('d6e1873d-1f67-4984-b3b3-f176ec92bfe0', contemporary_post_cold_war_consolidation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d6e1873d-1f67-4984-b3b3-f176ec92bfe0', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__graduated_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, high_capacity_states).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, international_development_institutions).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, external_intervention_authority).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, low_capacity_states).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, local_sovereignty_claim_holders).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, governance_legitimacy_targets).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-CAPACITY STATE (SNARE) — Faces external reclassification of its sovereignty legitimacy based on capacity metrics set by intervening powers. Cannot exit the graduated sovereignty framework without reorganizing state apparatus (structurally trapped). Bears full extraction: loss of non-interference doctrine, justification for conditional aid, legitimacy for external institutional oversight. Maximum experienced suppression.
constraint_indexing:constraint_classification(westphalian_sovereignty__graduated_sovereignty, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: HIGH-CAPACITY STATE / DEVELOPMENT INSTITUTION (ROPE) — Benefits from graduated sovereignty framework as legitimation for conditional lending, institutional oversight, and conditional aid. Experiences the constraint as coordination: communicating capacity metrics enables targeted development assistance. Net beneficiary with arbitrage options (can refuse participation without fundamental loss).
constraint_indexing:constraint_classification(westphalian_sovereignty__graduated_sovereignty, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: REGIONAL COALITION RESISTING GRADUATED SOVEREIGNTY (TANGLED ROPE) — African Union, ASEAN, and regional blocs organize against the graduated sovereignty framework but operate within institutional constraints (capital dependence, trade linkage, political isolation cost). See the constraint as extractive (capacity metrics delegitimize peers) but also coordinate around alternative legitimacy claims (AU sovereignty protocols, ASEAN non-interference norm). Constrained exit: can articulate alternatives but cannot fully escape the global institutional framework.
constraint_indexing:constraint_classification(westphalian_sovereignty__graduated_sovereignty, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 4: WESTPHALIAN DOCTRINE ITSELF (PITON) — The original principle (1648) of absolute sovereign equality, non-interference, and mutual recognition persists in treaty language and institutional rhetoric but is functionally degraded. UN Charter article 2(1) enshrines equal sovereignty; UN Trusteeship Council (now vestigial) instantiated graduated oversight. The doctrine survives through institutional inertia and symbolic authority rather than enforcement. Theater ratio high: ceremonial affirmation of sovereign equality coexists with normalized external capacity assessment.
constraint_indexing:constraint_classification(westphalian_sovereignty__graduated_sovereignty, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (MOUNTAIN) — Sees graduated sovereignty as an immutable consequence of state capacity differentiation: weak states cannot enforce borders or provide public goods; external actors fill vacuums; differential sovereignty becomes natural law. This perspective risks false summitry — naturalizing what is actually a constructed institutional choice about who gets to define 'capacity' and who bears the cost of reclassification.
constraint_indexing:constraint_classification(westphalian_sovereignty__graduated_sovereignty, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: STATE OFFICIAL (SNARE) — Government officials in low-capacity states face constant reclassification risk: missing IMF metrics triggers austerity conditions; governance metrics trigger intervention justification; security sector capacity triggers military intervention authorization. High career risk from external assessment; constrained exit through negotiating aid requirements but no fundamental exit from the graduated framework.
constraint_indexing:constraint_classification(westphalian_sovereignty__graduated_sovereignty, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__graduated_sovereignty_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(westphalian_sovereignty__graduated_sovereignty, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(westphalian_sovereignty__graduated_sovereignty, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalian_sovereignty__graduated_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(westphalian_sovereignty__graduated_sovereignty, TR),
    TR >= 0.70.

:- end_tests(westphalian_sovereignty__graduated_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The graduated sovereignty framework enables substantial extraction through institutional conditionality — low-capacity states experience aid dependency, structural adjustment requirements (privatization, austerity), and loss of policy autonomy. The extraction is not maximal (0.70+) because some benefits accrue through capacity-building assistance and some states retain negotiating capacity. The 23-year rise from 0.35 to 0.58 reflects deepening institutional integration of capacity metrics into aid architecture. Suppression (0.62): High. Multiple barriers prevent exit: capital dependency (no alternative funding source), institutional linkage (IMF/World Bank membership required for many forms of international engagement), diplomatic isolation (capacity downgrade justifies marginalization), and security threats (capacity classification justifies military intervention). However, suppression is not total (0.90+) because some low-capacity states maintain formal sovereignty and some can negotiate aid terms. Theater ratio (0.68): High. The constraint exhibits significant performative content: ceremonial invocation of sovereign equality (UN General Assembly) coexists with normalized external governance assessment; capacity reports circulate as objective facts while their methodological choices embed assessor preferences; conditionality justified as 'technical assistance' while functioning as authority assertion. The rise from 0.42 to 0.68 reflects increasing institutionalization of theater — capacity assessment has become standardized ritual with declining functional content.
 *
 * PERSPECTIVAL GAP:
 *   The same structural phenomenon — external classification of state legitimacy based on capacity metrics — appears as coordination mechanism (rope) to high-capacity states and development institutions, as pure extraction (snare) to low-capacity states, as mixed coordination/extraction (tangled rope) to organized regional coalitions, as degraded ritual (piton) to the Westphalian doctrine itself, and as immutable natural law (mountain/false summit) to analysts who naturalize capacity differentials. The beneficiary perspective sees graduated sovereignty as enabling targeted assistance and governance improvement. The victim perspective sees it as apparatus for institutional control and neo-colonial extraction. The organized resistance sees it as asymmetric power that must be countered through alternative legitimacy claims. The analytical observer risks naturalizing what is a constructed institutional choice about who gets to define capacity and who pays the cost of reclassification.
 *
 * DIRECTIONALITY LOGIC:
 *   High-capacity state/institution directionality: Low d value (~0.10-0.20). These agents are beneficiaries of the graduated sovereignty framework and have arbitrage options (can participate or refuse). The sigmoid function maps this to near-zero or negative f(d), producing low or negative effective extraction from their perspective. They experience χ ≤ 0.30 — the constraint coordinates rather than extracts from them. Low-capacity state directionality: High d value (~0.80-0.92). These agents are trapped victims who cannot exit without fundamental state reorganization. The sigmoid maps this to f(d) ≈ 1.35, producing χ ≈ 0.78 (base_extractiveness 0.58 × f(d) 1.35). They experience maximum extraction from the constraint. Regional coalition directionality: Moderate d value (~0.55-0.65). These organized agents have constrained but real exit options (can develop alternative protocols) and are partially victims (capacity metrics delegitimize peers) and partially beneficiaries (coordination around alternative governance norms). The sigmoid produces f(d) ≈ 0.75-0.85, yielding χ ≈ 0.43-0.49. The perspectival gap reflects the true structural asymmetry: the same constraint produces near-rope dynamics for beneficiaries and snare dynamics for victims.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved through the reading-specific framing: graduated sovereignty is ONE reading of a contested kernel (westphalian_sovereignty). The reading is mandatropic in its classification: snare (pure extraction) for victims, rope (pure coordination) for beneficiaries, tangled rope for organized resistance, piton for vestigial doctrine, mountain (false summit) for analytical observers. This distribution reveals the structure: the constraint IS mandatropic because different institutional positions experience fundamentally different constraint types. The resolution is not to choose one type but to recognize that the perspectival distribution itself demonstrates the kernel contestation. The graduated_sovereignty reading competes with absolute_sovereignty (which would classify as pure rope: equal legitimacy for all states) and conditional_sovereignty (which would ground conditionality on specific criteria rather than capacity metrics). The institutionalization of graduated_sovereignty is itself contestable — the measurement trajectory shows rising extractiveness and theater over 30 years, indicating institutional consolidation of the reading. The omega variables document irreducible uncertainties about whether this reading will persist, degrade into piton status (if capacity metrics lose legitimacy), or be displaced by alternative readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capacity_metrics_constructed,
    'Are ''state capacity'' metrics objective measures of governance function or constructed indices that embed the assessor''s institutional preferences?',
    'Comparative analysis of capacity indices (World Bank, IMF, BTI) across time: do metrics correlate with actual service delivery, or do they proxy institutional alignment with donor preferences? Historical analysis of index changes when political regimes shift without functional governance change.',
    'If objective: graduated sovereignty is a natural response to real capacity differentials. If constructed: graduated sovereignty is a mechanism for embedding external institutional preferences as legitimacy doctrine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_metrics_constructed, empirical, 'Whether state capacity metrics are objective or constructed assessments').

omega_variable(
    intervention_authority_legitimacy,
    'Who has standing to classify a state as low-capacity, and on what authority?',
    'Legal analysis of intervention authorization mechanisms: does graduated sovereignty doctrine require consensus (universal standing) or permit unilateral reclassification by powerful states/institutions? Case analysis of interventions justified by capacity assessment.',
    'If consensus required: graduated sovereignty is coordination mechanism (Rope-type dynamics). If unilateral: graduated sovereignty is extraction mechanism (Snare-type dynamics). The current system permits high-capacity state coalition unilateral reclassification of peers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intervention_authority_legitimacy, conceptual, 'Authority basis for classifying state capacity and sovereignty legitimacy').

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading (graduated_sovereignty) of the contested kernel westphalian_sovereignty. How do the sibling readings (absolute_sovereignty, conditional_sovereignty) compete for institutional adoption?',
    'Document which international institutions, state coalitions, and legal doctrines instantiate each reading; track contestation through treaty language, UN General Assembly positions, regional protocols, and intervention justifications.',
    'If absolute_sovereignty reading dominates: Westphalian non-interference is reaffirmed; graduated sovereignty loses legitimacy. If graduated_sovereignty dominates: low-capacity states face permanent reclassification regime. If conditional_sovereignty dominates: sovereignty conditionality on specific criterion (human rights, climate, terrorism) replaces capacity-based conditionality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Institutional competition between Westphalian sovereignty readings').

omega_variable(
    neo_colonial_extraction_mechanism,
    'Does graduated sovereignty doctrine enable neo-colonial extraction through institutional conditionality (debt, governance reform, privatization) that absolute or conditional readings would foreclose?',
    'Comparative analysis of IMF/World Bank conditionality imposed under absolute vs. graduated sovereignty frameworks; correlation between capacity-based reclassification and subsequent institutional reform requirements; outcome analysis (who captures benefits, who bears costs).',
    'If yes: graduated sovereignty is functionally equivalent to colonial authority (capacity assessment → institutional control). If no: graduated sovereignty is neutral efficiency mechanism. The empirical record shows strong correlation between capacity downgrade and institutional extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(neo_colonial_extraction_mechanism, empirical, 'Whether graduated sovereignty framework enables neo-colonial extraction patterns').

omega_variable(
    state_capacity_feedback_loop,
    'Does capacity-based reclassification itself reduce state capacity through institutional extraction (austerity, privatization, brain drain)?',
    'Longitudinal analysis: does external institutional control following capacity downgrade correlate with further capacity decline? Do states that resist reclassification framework maintain or improve capacity metrics relative to those that accept conditionality?',
    'If yes: graduated sovereignty creates destructive feedback loop (reclassification → extraction → further capacity loss → renewed reclassification). The snare classification is self-perpetuating. If no: graduated sovereignty facilitates external support that improves capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_capacity_feedback_loop, empirical, 'Whether capacity-based reclassification triggers feedback loop reducing state capacity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__graduated_sovereignty, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ws_grad_tr_t0, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 0, 0.42).
narrative_ontology:measurement(ws_grad_tr_t15, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 15, 0.58).
narrative_ontology:measurement(ws_grad_tr_t30, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(ws_grad_be_t0, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ws_grad_be_t15, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(ws_grad_be_t30, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(ws_grad_su_t0, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(ws_grad_su_t15, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 15, 0.55).
narrative_ontology:measurement(ws_grad_su_t30, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__graduated_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalian_sovereignty__graduated_sovereignty, humanitarian_intervention_doctrine).
narrative_ontology:affects_constraint(westphalian_sovereignty__graduated_sovereignty, structural_adjustment_conditionality).
narrative_ontology:affects_constraint(westphalian_sovereignty__graduated_sovereignty, capacity_building_institutional_extraction).
narrative_ontology:affects_constraint(westphalian_sovereignty__graduated_sovereignty, sovereignty_inequality_measurement).

% DUAL FORMULATION NOTE:
% The graduated_sovereignty reading is part of a constraint family decomposed from the Westphalian kernel. The absolute_sovereignty and conditional_sovereignty readings are separate constraints in the family, each with its own ε and institutional instantiation. The network links show how graduated_sovereignty doctrine enables downstream constraints (humanitarian intervention, structural adjustment). Each reading-constraint has its own extractiveness value: absolute_sovereignty ≈ 0.15 (Rope, non-interference coordination), conditional_sovereignty ≈ 0.42 (Tangled Rope, specific-criterion conditionality), graduated_sovereignty ≈ 0.58 (Snare, capacity-metric extraction). The differences in ε reflect the distinct institutional mechanisms and who bears costs under each reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westphalian_sovereignty__graduated_sovereignty, institutional, 0.15).
constraint_indexing:directionality_override(westphalian_sovereignty__graduated_sovereignty, organized, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
