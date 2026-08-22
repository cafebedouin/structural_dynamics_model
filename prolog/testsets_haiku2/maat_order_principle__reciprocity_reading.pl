% ============================================================================
% CONSTRAINT STORY: maat_order_principle__reciprocity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maat_order_principle__reciprocity_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: maat_order_principle__reciprocity_reading
 *   human_readable: Ma'at Reciprocity Obligation (Pharaonic Order)
 *   domain: political/religious/cosmological
 *
 * SUMMARY:
 *   The Ma'at kernel describes a cosmological principle of order, justice,
 *   and balance that grounds Egyptian political authority. The reciprocity
 *   reading instantiates Ma'at as a mutual obligation: the Pharaoh must
 *   provide justice, stability, and proper resource distribution to maintain
 *   cosmic balance and earn the support of the gods and the people. This
 *   reading treats the Pharaoh as subject to Ma'at constraints—failed
 *   obligations justify resistance, withdrawal of elite support, or
 *   replacement. The reading is contested against the divine mandate reading
 *   (Ma'at flows through the Pharaoh's person; the ruler cannot violate it by
 *   definition) and the distributed maintenance reading (all actors in their
 *   stations sustain Ma'at). The reciprocity reading produces a tangled rope:
 *   genuine coordination (the Pharaoh genuinely organizes justice and
 *   resource distribution for the collective good) paired with extraction
 *   (the Pharaoh and priestly apparatus collect rents, labor, and control in
 *   exchange for providing the order). The claim/metric gap is intentional:
 *   the reciprocity reading asserts the constraint as
 *   coordination-plus-enforced-obligation; the metrics describe substantially
 *   extractive operation moderated by reciprocity pressure.
 *
 * KEY AGENTS:
 *   - Pharaoh — the central political actor required to maintain Ma'at through just rule and proper distribution; benefits from the legitimacy the system grants but is structurally constrained by the reciprocity obligation
 *   - Priestly hierarchy — interprets Ma'at, administers temples, certifies the Pharaoh's compliance, and collects significant temple revenues and land; enforces the reciprocity doctrine against competing readings
 *   - Lower classes and provincial populations — provide labor, taxes, and military service; are owed justice and stability in return; can withdraw cooperation or support if obligations are breached
 *   - Elite provincial administrators — middle power, manage resource distribution and local justice; navigate between Pharaonic orders and local constituencies; can resist or negotiate depending on the Pharaoh's reciprocal performance
 *   - Analysts (modern scholars) — observe the constraint from outside; use textual and archaeological evidence to assess the historical operation of the reciprocity reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maat_order_principle__reciprocity_reading, 0.58).
domain_priors:suppression_score(maat_order_principle__reciprocity_reading, 0.62).
domain_priors:theater_ratio(maat_order_principle__reciprocity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_order_principle__reciprocity_reading, tangled_rope).
narrative_ontology:human_readable(maat_order_principle__reciprocity_reading, "Ma'at Reciprocity Obligation (Pharaonic Order)").
narrative_ontology:topic_domain(maat_order_principle__reciprocity_reading, "political/religious/cosmological").

domain_priors:requires_active_enforcement(maat_order_principle__reciprocity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__reciprocity_reading, 'f330d3a0-b42b-435c-ae96-3d879c6c95d2').
narrative_ontology:cs_kernel_codification('f330d3a0-b42b-435c-ae96-3d879c6c95d2', fixed_text).
narrative_ontology:cs_authority_grounding('f330d3a0-b42b-435c-ae96-3d879c6c95d2', extraction).
narrative_ontology:cs_interpretation_layer_present('f330d3a0-b42b-435c-ae96-3d879c6c95d2').
narrative_ontology:cs_reading_relation('f330d3a0-b42b-435c-ae96-3d879c6c95d2', maat_order_principle__divine_mandate_reading, coexists_with).
narrative_ontology:cs_reading_relation('f330d3a0-b42b-435c-ae96-3d879c6c95d2', maat_order_principle__distributed_maintenance_reading, coexists_with).
narrative_ontology:cs_axiom('f330d3a0-b42b-435c-ae96-3d879c6c95d2', foundational, pharaoh_subject_to_maat_constraints).
narrative_ontology:cs_axiom_status(pharaoh_subject_to_maat_constraints, holdable).
narrative_ontology:cs_axiom_grounding('f330d3a0-b42b-435c-ae96-3d879c6c95d2', pharaoh_subject_to_maat_constraints, deontological).
narrative_ontology:cs_axiom('f330d3a0-b42b-435c-ae96-3d879c6c95d2', foundational, failed_obligation_justifies_resistance).
narrative_ontology:cs_axiom_status(failed_obligation_justifies_resistance, holdable).
narrative_ontology:cs_axiom_grounding('f330d3a0-b42b-435c-ae96-3d879c6c95d2', failed_obligation_justifies_resistance, deontological).
narrative_ontology:cs_reference_frame('f330d3a0-b42b-435c-ae96-3d879c6c95d2', balanced_reciprocity_regime).
narrative_ontology:cs_drift_state('f330d3a0-b42b-435c-ae96-3d879c6c95d2', late_period_dynastic_decline, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f330d3a0-b42b-435c-ae96-3d879c6c95d2', '2026-06-12T09:00:00Z').
narrative_ontology:cs_kernel_id(maat_order_principle__reciprocity_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_order_principle__reciprocity_reading, pharaoh).
narrative_ontology:constraint_beneficiary(maat_order_principle__reciprocity_reading, priestly_hierarchy).
narrative_ontology:constraint_victim(maat_order_principle__reciprocity_reading, lower_classes).
narrative_ontology:constraint_victim(maat_order_principle__reciprocity_reading, provincial_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(maat_order_principle__reciprocity_reading, provincial_elite).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The central political actor responsible for maintaining Ma'at through just rule, proper resource distribution, and ensuring cosmic balance. Collects rents, labor, and control from the society. Under the reciprocity reading, the Pharaoh is constrained by the obligation to provide justice and stability; failure justifies elite resistance and popular withdrawal of support. The Pharaoh cannot exit the role (the cosmic and political necessity of the position is assumed) and operates under continuous performance pressure to demonstrate Ma'at maintenance.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, pharaoh, agenda_setter,
    institutional, civilizational, trapped, universal).

% Interprets Ma'at doctrine, administers temples, certifies the Pharaoh's compliance with reciprocal obligations, and collects substantial temple revenues and landholdings. Functions as the institutional arbiter of whether the Pharaoh has failed at Ma'at maintenance. Identity is fused with the cosmic order itself—to exit priesthood is to abandon the entire cosmological framework. Reinforces the reciprocity reading against the divine mandate and distributed maintenance readings by invoking the Pharaoh's accountability.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, priestly_hierarchy, agenda_setter,
    institutional, civilizational, identity_locked, universal).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__reciprocity_reading, priestly_hierarchy, beneficiary).

% Provide labor (corvée), taxes, and military service in exchange for justice, stability, and resource distribution. Are owed Ma'at maintenance by the Pharaoh under the reciprocity reading. Cannot exit the Egyptian order without ceasing to be Egyptian and losing social, spiritual, and material identity. Can withdraw cooperation or engage in low-level resistance (work slowdown, draft avoidance, informal tax evasion) if the Pharaoh fails at obligations, which constrains extraction below snare levels despite powerlessness.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, lower_classes, payer,
    powerless, biographical, trapped, universal).

% Regional administrators and local magnates who manage resource distribution and justice at the provincial level, subordinate to the Pharaoh. Capture local rents and control but are constrained by both Pharaonic orders and local expectations of reciprocal obligation. Can resist or negotiate with the Pharaoh if his Ma'at performance fails, which gives them constrained (not trapped) exit. Navigate between enforcing Pharaonic extraction and meeting local constituencies' expectations of justice and distribution. Represent the mechanism by which reciprocal obligation is enforced at sub-Pharaonic levels.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, provincial_elite, payer,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__reciprocity_reading, provincial_elite, agenda_setter).

% Local communities in provinces distant from central authority. Provide local taxes and labor through provincial administrators. Experience Ma'at maintenance or failure at the local level, mediated by provincial elite. Are identity-locked to regional and Egyptian identity; exit means statelessness or exile. Can coordinate on resistance or withdrawal of cooperation at the provincial scale if local justice fails, which makes elite provincial administrators accountable to them even while subject to Pharaonic orders.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, provincial_populations, payer,
    powerless, biographical, identity_locked, regional).

% Alternative frameworks for understanding order, justice, and political legitimacy (non-Egyptian religions, secular logics, distributed-authority doctrines) are structurally excluded from the Egyptian political system. Not a concrete agent but the excluded set of alternatives that the reciprocity reading suppresses. Their exclusion is the condition of the constraint's operation.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, competing_cosmologies, excluded,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(maat_order_principle__reciprocity_reading, competing_cosmologies).

% Modern scholars analyzing the historical operation of Ma'at and the reciprocity reading. Assess evidence from texts, inscriptions, and archaeology to determine whether the reciprocity reading was institutionally operative or is a modern reconstruction. Neither benefit nor pay within the constraint; observe its structure and trace its historical operation.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, scholar_observer, observer,
    analytical, biographical, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(maat_order_principle__reciprocity_reading, pharaoh).
narrative_ontology:fixing_cost_class(maat_order_principle__reciprocity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of order, justice, and stability across a geographically dispersed, hierarchical society with limited communication infrastructure. The Pharaoh as cosmic agent maintains Ma'at through just rule, proper distribution of resources, and ensuring that the divine and natural orders remain aligned. Without the Pharaoh's coordinating role, justice would devolve into local arbitration (losing the universal order) and resources would be distributed through fragmented negotiation rather than centralized allocation. The reciprocity reading claims the Pharaoh genuinely performs this coordination and is constrained by obligation to do so.
% TRANSFER_FUNCTION: Moves rents (temple lands, portions of trade, state monopolies), labor (corvée, military service, construction projects), and control (monopoly on interpretation of divine will, arbitration of justice) from lower classes and provincial populations to the Pharaoh and priestly apparatus. In exchange, the lower classes and provinces receive justice, protection, resource redistribution in times of shortage, and cosmic stability. The reciprocity reading asserts this is not one-directional extraction but mutual obligation: if the Pharaoh fails to provide the promised goods, the obligation inverts and justifies resistance.
% ABSENT_VOICES: Competing cosmologies and non-Egyptian frameworks for understanding order (other religions, secular logics, distributed-authority doctrines) are structurally excluded. They would argue against the centralized Pharaonic monopoly on cosmic mediation and assert alternative foundations for justice and order. The distributed maintenance reading (all actors sustain Ma'at through proper conduct) is present in Egyptian sources but suppressed by the priestly enforcement of the reciprocity reading. Independent regional magnates with their own authority claims are excluded by the Pharaoh's centralization. The absence of these voices enables the reciprocity reading to go unchallenged as the operative doctrine, though the pressure from provincial elite (who might lean toward distributed maintenance to preserve local authority) is a muted version of the exclusion.
% DISAPPEARANCE_RATIONALE: If the Ma'at reciprocity obligation vanished, the Pharaoh would lose the constraint that limits extraction and obligates redistribution. Rents would rise, justice would become arbitrary, and resource distribution would collapse into pure extraction. The lower classes and provincial elite would reorganize around alternative authority structures (local warlords, priesthoods independent of Pharaonic authority, or distributed-maintenance doctrines that justify local leadership). The entire political order depends on the reciprocity reading: without it, the justification for the Pharaoh's monopoly on cosmic mediation dissolves, and power redistributes. The Egyptian state would not persist in its recognizable form.
% FOUNDING_PROBLEM: A geographically dispersed, hierarchical agricultural civilization needs universal order, justice, and stable resource distribution to survive droughts, manage labor, and prevent local conflicts from fragmenting the polity. Individual communities lack the coordination capacity and information to manage these problems independently. The Pharaoh, positioned as the cosmic intermediary, provides this coordination and justice in exchange for rents and labor.
% FOUNDING_PROBLEM_CORROBORATION: Priestly teaching texts and royal inscriptions attest that Ma'at maintenance and just distribution are the Pharaoh's obligations and that providing them is essential to cosmic stability (priestly sources, beneficiary-side). Archaeological and textual evidence of provincial administration, resource redistribution networks, and legal institutions (Karnak and Memphis administrative records, provincial tomb autobiographies) confirms that the Pharaoh's apparatus genuinely engaged in justice and distribution, not purely extraction (non-beneficiary sources: provincial elite and commoner inscriptions). However, modern scholars note that the actual degree of redistribution and justice declined over dynasties while extraction grew (this is the mandatrophy signal), suggesting the founding problem (need for order and distribution) remained live while the institutional capacity to solve it degraded. The founding problem is attested from outside the Pharaoh's circle as an ongoing requirement of civilization.
narrative_ontology:disappearance_verdict(maat_order_principle__reciprocity_reading, world_rearranges).
narrative_ontology:founding_problem_status(maat_order_principle__reciprocity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(maat_order_principle__reciprocity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(maat_order_principle__reciprocity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(maat_order_principle__reciprocity_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maat_order_principle__reciprocity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(maat_order_principle__reciprocity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(maat_order_principle__reciprocity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58) because the constraint imposes genuine reciprocal duties on the Pharaoh. If the Pharaoh fails to provide justice, stability, or proper distribution, the reciprocity reading authorizes resistance and withdrawal of support. This is not pure extraction—the Pharaoh must deliver actual goods (justice, stable order, resource distribution). However, the extraction floor is substantial: the Pharaoh and priestly apparatus collect rents, labor, and control beyond the marginal cost of providing order. Suppression is higher (0.62) because the constraint's persistence depends on suppressing alternative power structures (removal of independent regional authority, elimination of competing legitimacy claims) and suppressing discourses that deny the Pharaoh's obligation status or assert the distributed maintenance reading. Theater is moderate-low (0.28): the Pharaoh's genuinely manages justice and resources, but an increasing share of ceremonial and artistic output in later periods performs the Ma'at order rather than enacting it. The measurement series shows a rise in extractiveness from 0.48 to a plateau around 0.58–0.60 by the middle of the interval, suggesting that reciprocal obligations remain operant but the burden of provision grows while the expectation of distribution quality declines—theater and suppression rise correspondingly. By the end of the interval, metrics stabilize, indicating the constraint reaches a quasi-stable state where extraction is high enough to attract corruption and diminish faith, but reciprocal obligations remain just strong enough to prevent full snare classification or elite coalition collapse.
 *
 * PERSPECTIVAL GAP:
 *   The Pharaoh and priestly hierarchy experience the constraint as genuine coordination duty—the ruler maintains Ma'at and cosmic order, which justifies the rents and labor extracted. Lower classes and provincial populations experience it as extraction moderated by enforceable obligation—if the Pharaoh fails at justice and distribution, they can withdraw support or resist, which caps extraction below snare levels. Elite provincial administrators occupy an intermediate position: they are constrained by the obligation to deliver localized justice (subordinate obligation), but they also capture local rents, creating a nested extraction/coordination dynamic. The engine's per-seat computation should reflect these divergences: the Pharaoh's seat computes the constraint as moderate coordination with beneficiary directionality; the payers' seats compute it as substantially extractive with obligatory limits. This divergence is the structural asymmetry that justifies the tangled-rope claim.
 *
 * DIRECTIONALITY LOGIC:
 *   The Pharaoh is the primary beneficiary (d near 0.2): collects rents, labor, and centralized control in exchange for providing order. The priestly hierarchy is also a beneficiary (d near 0.25): interprets and certifies compliance, collects temple revenues, and reinforces the legitimacy structure. Lower classes and provincial populations are the primary targets (d near 0.75–0.85): provide labor, taxes, and military service; are owed justice and stability but have limited exit (trapped or identity-locked to the Egyptian order). Elite provincial administrators are secondary targets (d near 0.65): manage distribution and justice locally but are constrained by both Pharaonic orders above and popular expectations below; their exit is constrained but not trapped (they can shift alliance or resist if obligations fail). The spatial scope is universal (Egyptian order claimed to encompass all of Egypt and tributary regions); the time horizon is civilizational (Ma'at is eternal cosmic principle, though individual Pharaohs are biographical). Exit for most actors is identity-locked to the Egyptian cosmological order itself—to exit is to cease being Egyptian and cosmically safe, which is internalized as psychologically impossible.
 *
 * MANDATROPHY ANALYSIS:
 *   The reciprocity reading prevents misclassification as pure extraction (snare) by declaring that the Pharaoh has enforceable obligations. If the founding problem—maintaining cosmic balance and order through just rule—becomes obsolete (which would require external challenge to Egyptian cosmology or successful alternative power structure), then the reciprocity obligation dies but extraction persists, producing a piton or zombie snare. The measurement series shows rising theater and stable suppression even as extractiveness plateaus, which is a mandatrophy signal: the ceremony of Ma'at maintenance grows while the actual delivery of justice may be declining, suggesting the constraint is beginning to devolve into performance. However, the theater ratio remains below 0.5, so the constraint is not yet a piton. The constraint remains tangled rope as long as the reciprocity obligation is institutionally maintained (the priestly hierarchy and competing elites invoke it to constrain or challenge failed Pharaohs) and the lower classes believe withdrawal of support is possible (even if they rarely exercise it).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reciprocity_vs_divine_mandate_framing,
    'Is Ma''at a genuine reciprocal obligation constraining the Pharaoh, or a divine mandate that flows through the Pharaoh''s person such that the ruler cannot structurally violate it?',
    'Historical analysis of failed Pharaohs: were they held accountable (reciprocity reading) or reframed post-hoc as cosmic aberrations who violated Ma''at by definition (divine mandate reading)? Do contemporary sources document contestation over the Pharaoh''s obligation status?',
    'If reciprocity, the Pharaoh''s extraction is moderated by the obligation to provide justice and stability; resistance and withdrawal of support are justified responses to failed obligations. If divine mandate, the Pharaoh cannot fail at Ma''at maintenance — failures are reinterpreted as cosmic violations rather than Pharaonic violations, eliminating the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_vs_divine_mandate_framing, conceptual, 'Whether Ma''at operates as mutual obligation (reciprocity) or as cosmic mandate flowing through the Pharaoh (divine mandate).').

omega_variable(
    suppression_mechanism_internalization,
    'Is the suppression of alternative power structures (removal of competing authority, elimination of regionally independent power) sustained by external force (military, institutional coercion) or by internalized legitimacy (the lower classes accept the Pharaoh''s centrality as cosmically necessary)?',
    'Evidence of coercive capacity at different periods and provinces; stability of elite coalitions across transitions; presence or absence of organized resistance discourses in extant records.',
    'If structural, the suppression metric is accurate to the material constraint but overestimates internalization. If internalized, the effective suppression on elite resistance would be lower but the effective suppression on popular resistance would be higher — the measured scalar does not capture the asymmetry.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs. internalized suppression of competing orders and resistance.').

omega_variable(
    kernel_reading_identity_locked,
    'Which reading of the Ma''at kernel did the Pharaoh and priestly apparatus hold? Is this reciprocity reading historically instantiated or is it a retrospective interpretation imposed by modern scholars reading the texts against the political dynamic they supported?',
    'Textual analysis of what the Pharaoh''s own inscriptions and the priesthood''s teachings claim about the Pharaoh''s obligations and their enforceability. Cross-reference with evidence of actual consequences for failed obligations (removal, legitimacy loss, resistance success).',
    'If the reciprocity reading was institutionally held, the constraint operated as described — mutual obligation with moderate extraction. If it was not, the constraint is better described under the divine mandate or distributed maintenance readings, and this story''s classification changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity_locked, conceptual, 'Historical authenticity of the reciprocity reading as an operative institutional understanding vs. modern scholarly reconstruction.').

omega_variable(
    distributed_coordination_alternative,
    'Did the distributed maintenance reading (all actors from Pharaoh to commoner sustain Ma''at through conduct in their station) coexist as a live institutional alternative to the reciprocity reading, or is distributed maintenance a later scholarly generalization?',
    'Evidence from priestly teaching texts, tomb inscriptions of commoners, and wisdom literature: do contemporary sources assert that lower-station conduct maintains cosmic order, or is this imputed by modern analysis?',
    'If coexisting, the two readings would deflect accountability upward (reciprocity focuses on Pharaoh''s obligations; distributed maintenance distributes obligation thinly). If distributed maintenance is not historically live, the reciprocity reading faces less institutional competition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(distributed_coordination_alternative, empirical, 'Historical coexistence of reciprocity and distributed-maintenance readings as operative institutional framings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__reciprocity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_tr_t0, maat_order_principle__reciprocity_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(maat_tr_t0, projected).
narrative_ontology:measurement(maat_tr_t4, maat_order_principle__reciprocity_reading, theater_ratio, 4, 0.21).
narrative_ontology:measurement_basis(maat_tr_t4, observed).
narrative_ontology:measurement(maat_tr_t8, maat_order_principle__reciprocity_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement_basis(maat_tr_t8, observed).
narrative_ontology:measurement(maat_tr_t12, maat_order_principle__reciprocity_reading, theater_ratio, 12, 0.27).
narrative_ontology:measurement_basis(maat_tr_t12, observed).
narrative_ontology:measurement(maat_tr_t16, maat_order_principle__reciprocity_reading, theater_ratio, 16, 0.29).
narrative_ontology:measurement_basis(maat_tr_t16, observed).
narrative_ontology:measurement(maat_tr_t20, maat_order_principle__reciprocity_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement_basis(maat_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(maat_be_t0, maat_order_principle__reciprocity_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(maat_be_t0, projected).
narrative_ontology:measurement(maat_be_t4, maat_order_principle__reciprocity_reading, base_extractiveness, 4, 0.52).
narrative_ontology:measurement_basis(maat_be_t4, observed).
narrative_ontology:measurement(maat_be_t8, maat_order_principle__reciprocity_reading, base_extractiveness, 8, 0.56).
narrative_ontology:measurement_basis(maat_be_t8, observed).
narrative_ontology:measurement(maat_be_t12, maat_order_principle__reciprocity_reading, base_extractiveness, 12, 0.58).
narrative_ontology:measurement_basis(maat_be_t12, observed).
narrative_ontology:measurement(maat_be_t16, maat_order_principle__reciprocity_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement_basis(maat_be_t16, observed).
narrative_ontology:measurement(maat_be_t20, maat_order_principle__reciprocity_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(maat_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(maat_su_t0, maat_order_principle__reciprocity_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(maat_su_t0, projected).
narrative_ontology:measurement(maat_su_t4, maat_order_principle__reciprocity_reading, suppression_requirement, 4, 0.6).
narrative_ontology:measurement_basis(maat_su_t4, observed).
narrative_ontology:measurement(maat_su_t8, maat_order_principle__reciprocity_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement_basis(maat_su_t8, observed).
narrative_ontology:measurement(maat_su_t12, maat_order_principle__reciprocity_reading, suppression_requirement, 12, 0.64).
narrative_ontology:measurement_basis(maat_su_t12, observed).
narrative_ontology:measurement(maat_su_t16, maat_order_principle__reciprocity_reading, suppression_requirement, 16, 0.62).
narrative_ontology:measurement_basis(maat_su_t16, observed).
narrative_ontology:measurement(maat_su_t20, maat_order_principle__reciprocity_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement_basis(maat_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maat_order_principle__reciprocity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(maat_order_principle__reciprocity_reading, 0.12).
narrative_ontology:affects_constraint(maat_order_principle__reciprocity_reading, maat_order_principle__divine_mandate_reading).
narrative_ontology:affects_constraint(maat_order_principle__reciprocity_reading, maat_order_principle__distributed_maintenance_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Ma'at order kernel. The divine mandate reading asserts the Pharaoh embodies Ma'at and cannot structurally violate it (redefining failures as cosmic aberrations). The distributed maintenance reading asserts all actors sustain Ma'at through proper conduct in their station (distributing obligation and accountability diffusely). The reciprocity reading asserts mutual obligation: the Pharaoh is constrained by duty to provide justice and stability, and failure justifies resistance. The three readings share the kernel but instantiate different constraints with different beneficiary structures, suppression mechanisms, and extraction ceilings. They coexist as live institutional readings held by different parties within Egyptian political theology.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(maat_order_principle__reciprocity_reading, organized, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
