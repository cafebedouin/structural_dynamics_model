% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__coercion_visibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command__coercion_visibility_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: divine_marriage_command__coercion_visibility_reading
 *   human_readable: Post-Manifesto Monogamy Requirement (Coercion-Visibility Reading)
 *   domain: religious_authority/political_theology
 *
 * SUMMARY:
 *   The 1890 Manifesto (Official Declaration 1) ended the LDS Church's public
 *   practice of plural marriage. The coercion-visibility reading holds that
 *   the Manifesto was a direct response to federal coercion — the
 *   Edmunds-Tucker Act had disincorporated the church, seized its assets,
 *   disfranchised polygamists, and imprisoned its leaders. Wilford Woodruff's
 *   journal records the decision as survival-driven: 'The Lord showed me...
 *   if we did not stop the practice, the church would be destroyed.' The
 *   theological legitimacy of the cessation derives from institutional
 *   survival necessity, not new revelation. This reading closes the M-set gap
 *   by admitting exogenous pressure as the operative cause, creating a
 *   potential legitimacy crisis: if doctrinal shifts can be driven by state
 *   coercion, the authority structure's claim to revelatory continuity is
 *   undermined.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__coercion_visibility_reading, 0.78).
domain_priors:suppression_score(divine_marriage_command__coercion_visibility_reading, 0.85).
domain_priors:theater_ratio(divine_marriage_command__coercion_visibility_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__coercion_visibility_reading, snare).
narrative_ontology:human_readable(divine_marriage_command__coercion_visibility_reading, "Post-Manifesto Monogamy Requirement (Coercion-Visibility Reading)").
narrative_ontology:topic_domain(divine_marriage_command__coercion_visibility_reading, "religious_authority/political_theology").

domain_priors:requires_active_enforcement(divine_marriage_command__coercion_visibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__coercion_visibility_reading, 'b56cac0d-9de8-478a-9594-c205c7325e73').
narrative_ontology:cs_kernel_codification('b56cac0d-9de8-478a-9594-c205c7325e73', fixed_text).
narrative_ontology:cs_authority_grounding('b56cac0d-9de8-478a-9594-c205c7325e73', extraction).
narrative_ontology:cs_interpretation_layer_present('b56cac0d-9de8-478a-9594-c205c7325e73').
narrative_ontology:cs_reading_relation('b56cac0d-9de8-478a-9594-c205c7325e73', divine_marriage_command__continuationist_reading, forecloses).
narrative_ontology:cs_reading_relation('b56cac0d-9de8-478a-9594-c205c7325e73', divine_marriage_command__substitutionist_reading, forecloses).
narrative_ontology:cs_axiom('b56cac0d-9de8-478a-9594-c205c7325e73', foundational, manifesto_driven_by_federal_coercion).
narrative_ontology:cs_axiom_status(manifesto_driven_by_federal_coercion, holdable).
narrative_ontology:cs_axiom_grounding('b56cac0d-9de8-478a-9594-c205c7325e73', manifesto_driven_by_federal_coercion, empirically_contingent).
narrative_ontology:cs_axiom('b56cac0d-9de8-478a-9594-c205c7325e73', foundational, theological_legitimacy_from_institutional_survival).
narrative_ontology:cs_axiom_status(theological_legitimacy_from_institutional_survival, holdable).
narrative_ontology:cs_axiom_grounding('b56cac0d-9de8-478a-9594-c205c7325e73', theological_legitimacy_from_institutional_survival, instrumental).
narrative_ontology:cs_reference_frame('b56cac0d-9de8-478a-9594-c205c7325e73', original_revelation_authenticity).
narrative_ontology:cs_drift_state('b56cac0d-9de8-478a-9594-c205c7325e73', post_manifesto_1890, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('b56cac0d-9de8-478a-9594-c205c7325e73', '').
narrative_ontology:cs_kernel_id(divine_marriage_command__coercion_visibility_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__coercion_visibility_reading, lds_first_presidency).
narrative_ontology:constraint_beneficiary(divine_marriage_command__coercion_visibility_reading, lds_quorum_twelve).
narrative_ontology:constraint_beneficiary(divine_marriage_command__coercion_visibility_reading, rank_and_file_members).
narrative_ontology:constraint_victim(divine_marriage_command__coercion_visibility_reading, polygamous_families).
narrative_ontology:constraint_victim(divine_marriage_command__coercion_visibility_reading, fundamentalist_dissenters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(divine_marriage_command__coercion_visibility_reading, rank_and_file_members).
narrative_ontology:constraint_vindicates(divine_marriage_command__coercion_visibility_reading, institutional_survival_justifies_doctrinal_accommodation).
narrative_ontology:constraint_vindicates(divine_marriage_command__coercion_visibility_reading, revelation_can_be_suspended_under_duress).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issued the 1890 Manifesto officially discontinuing plural marriage. Faced federal disincorporation, asset seizure, and imprisonment of leaders. Framed the cessation as divine counsel while privately acknowledging coercion. Administers the ongoing ban through excommunication and temple recommend denial.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, lds_first_presidency, agenda_setter,
    institutional, generational, constrained, national).

% Sustained the Manifesto publicly; several apostles continued plural marriage covertly into the 1900s. Benefited from institutional survival and eventual statehood. Enforces the ban through disciplinary councils and missionary teaching.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, lds_quorum_twelve, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(divine_marriage_command__coercion_visibility_reading, lds_quorum_twelve, beneficiary).

% Men faced imprisonment, fines, and hiding; wives and children lost legal recognition, inheritance rights, and social standing. Forced to choose between family abandonment or exile to Mexico/Canada. No voice in the Manifesto; bore the material and emotional costs of compliance.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, polygamous_families, payer,
    powerless, biographical, trapped, local).

% Rejected the Manifesto as capitulation; maintained plural marriage as binding divine command. Excommunicated and marginalized; formed separate communities (FLDS, etc.). Their objection is structural: they view the constraint as illegitimate because it admits non-revelatory grounds.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, fundamentalist_dissenters, excluded,
    moderate, generational, identity_locked, regional).

% Gained relief from federal persecution, statehood for Utah, and social normalization. Paid the cost of doctrinal whiplash: a once-essential ordinance became grounds for excommunication. Taught to accept the Manifesto as revelation despite leadership's private acknowledgments of duress.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, rank_and_file_members, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(divine_marriage_command__coercion_visibility_reading, rank_and_file_members, payer).

% Applied coercive pressure (Edmunds Act, Edmunds-Tucker Act, disfranchisement, asset seizure) to force compliance. Achieved policy goal of ending territorial theocracy. Not a party to the church's internal constraint system but the exogenous force this reading identifies as the true driver.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, federal_government, observer,
    institutional, generational, analytical, national).

% Analyze the Manifesto as a case study in religious accommodation to state power. Document the gap between public revelation narrative and private coercion acknowledgment. Provide the external corroboration for the coercion-visibility reading.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, scholars_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enabled the LDS Church to survive as a legal corporation and achieve Utah statehood by signaling compliance with federal marriage law, ending the existential threat of disincorporation and asset seizure.
% TRANSFER_FUNCTION: Transferred the status, property rights, and family integrity of polygamous households to the institutional survival of the church hierarchy and the political objectives of the federal state.
% ABSENT_VOICES: Polygamous wives and children had no formal representation in the Manifesto process; their testimony on the lived impact was excluded from the official record. Fundamentalist dissenters were excommunicated rather than heard. Federal authorities' role as coercive driver was acknowledged only in private correspondence, not in the public theological frame.
% DISAPPEARANCE_RATIONALE: If the post-Manifesto monogamy requirement vanished overnight, the LDS Church would face immediate schism: fundamentalist groups would reassert polygamy as binding, mainstream members would confront the acknowledged coercion history, and the church's claim to continuous revelatory authority would fracture. The federal government would lose its settled expectation of compliance.
% FOUNDING_PROBLEM: The federal government's escalating coercion (Edmunds Act 1882, Edmunds-Tucker Act 1887) threatened the LDS Church's corporate existence, leadership liberty, and temple assets. The church faced disincorporation, seizure of all property over $50,000, and imprisonment of its entire priesthood leadership.
% FOUNDING_PROBLEM_CORROBORATION: Federal congressional records, court cases (Late Corporation of the Church of Jesus Christ of Latter-Day Saints v. United States, 1890), and the church's own private correspondence (Woodruff's journal, First Presidency letters) corroborate that the Manifesto was issued under explicit threat of total institutional destruction. Non-beneficiary historians (Arrington, Bitton, Quinn, Shipps) confirm the survival-necessity framing. The church's official essays now acknowledge 'intense pressure' but stop short of naming coercion as the theological ground.
narrative_ontology:disappearance_verdict(divine_marriage_command__coercion_visibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_marriage_command__coercion_visibility_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__coercion_visibility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(divine_marriage_command__coercion_visibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_marriage_command__coercion_visibility_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__coercion_visibility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_marriage_command__coercion_visibility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_marriage_command__coercion_visibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because polygamous families lost legal recognition, property, children's legitimacy, and social standing — a massive transfer from a powerless group to institutional survival. Suppression is higher (0.85) because the ban's persistence depends on active enforcement: excommunication, temple recommend denial, missionary screening, and the Second Manifesto (1904) which added priesthood disqualification. Theater ratio is high (0.65) because the public framing ('revelation from God') diverges sharply from the private acknowledgment ('we would have been destroyed'). The measurement grid shows extraction and suppression rising through the 1890s as enforcement hardened (Second Manifesto, Smoot hearings), with theater peaking when the revelation narrative was most actively maintained against historical evidence.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat, the constraint appears as a tragic but necessary coordination act: the church survived to fulfill its broader mission. From the payer seats (polygamous families, fundamentalists), it appears as a snare: a revelation cover story masking capitulation to state power, extracting from the most vulnerable to preserve the institution. The engine computes this divergence from the structural data — the authored claim (snare) reflects the payer-seat reality, while the institutional seat would compute a scaffold or rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The First Presidency and Quorum of Twelve are structural beneficiaries (d ≈ 0.15) — they collected institutional survival, statehood, and continued authority. Polygamous families are full targets (d ≈ 0.95) — trapped by geography, identity, and law, they bore the extraction with no exit. Fundamentalist dissenters are identity-locked (d ≈ 0.90) — their self-concept is constituted by the original command, making exit unthinkable. Rank-and-file members sit near symmetric (d ≈ 0.50) — genuine relief from persecution balanced against doctrinal whiplash. The federal government is not a seat in the church's constraint system but the exogenous force this reading identifies as the true agenda-setter.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (federal existential threat) is dead — the church achieved statehood, legal recognition, and security. Yet the constraint (monogamy requirement) persists and has hardened. This is mandatrophy: the arrangement outlived its founding justification. The continuationist reading preserves the original mandate (polygamy as eternal); the substitutionist reading invents a new mandate (monogamy as new revelation); this reading exposes the mandate vacuum — survival necessity was the only ground, and it expired. The constraint now persists by institutional inertia and theological path dependence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_identity,
    'How does this reading''s structural classification change if the kernel_id ''divine_marriage_command'' is framed as the original command (D&C 132) versus the post-Manifesto ban?',
    'Disambiguate the constraint referent: if the constraint is the original command, ε ≈ 0 (mountain from believer seat); if the constraint is the post-Manifesto ban, ε is high (snare/tangled_rope). This reading analyzes the ban as constraint.',
    'Misidentifying the referent flips the classification. The kernel_id must bind to a specific structural claim, not the ambiguous label.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_identity, conceptual, 'Kernel referent disambiguation for the divine_marriage_command kernel').

omega_variable(
    coercion_vs_revelation_causation,
    'Was the Manifesto causally driven by federal coercion, or was coercion the occasion for a genuine revelatory shift?',
    'Compare the private documentary record (Woodruff journal, First Presidency minutes, Smoot hearing testimony) against the public revelation narrative. The causal weight of the Edmunds-Tucker Act''s specific penalties (disincorporation, asset seizure) on the decision timeline.',
    'If coercion was the sufficient cause, the constraint is a snare (extraction with revelation cover). If revelation was genuine and coercion merely the occasion, the constraint is a rope or mountain from the believer seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_vs_revelation_causation, empirical, 'Causal attribution of the Manifesto: coercion vs revelation').

omega_variable(
    legitimacy_crisis_condition,
    'Does acknowledging coercion as a valid input to doctrinal shift create a structural legitimacy crisis for the authority structure?',
    'Track whether the authority structure''s subsequent claims (prophetic infallibility, continuous revelation) can absorb the admission that a core doctrine was reversed under duress without revelatory warrant. Observe schism rates and fundamentalist persistence as indicators.',
    'If the legitimacy crisis is structural, the constraint''s persistence depends on suppression of the coercion narrative (high theater). If the authority structure can re-narrativize (e.g., ''God used the pressure''), the crisis is managed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_crisis_condition, conceptual, 'Whether coercion-acknowledgment undermines revelatory authority claims').

omega_variable(
    polygamous_wives_agency_ambiguity,
    'To what extent did polygamous wives experience the constraint as extraction versus liberation, and does this split the victim seat?',
    'Analyze wives'' testimony (exclusion from Manifesto process, property loss vs. relief from rivalry/pregnancy burden, access to civil divorce). Partition the ''polygamous_families'' stakeholder by gender and generational position.',
    'If wives were net beneficiaries (civil rights access), the victim seat fragments and extraction asymmetry weakens. If wives were net victims (loss of status, community, children''s legitimacy), the snare classification strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(polygamous_wives_agency_ambiguity, empirical, 'Gender-differentiated experience of the Manifesto within polygamous households').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__coercion_visibility_reading, 1890, 1910).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dmc_cvr_tr_t1890, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1890, 0.4).
narrative_ontology:measurement(dmc_cvr_tr_t1894, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1894, 0.5).
narrative_ontology:measurement(dmc_cvr_tr_t1898, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1898, 0.58).
narrative_ontology:measurement(dmc_cvr_tr_t1902, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1902, 0.62).
narrative_ontology:measurement(dmc_cvr_tr_t1904, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1904, 0.68).
narrative_ontology:measurement(dmc_cvr_tr_t1906, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1906, 0.7).
narrative_ontology:measurement(dmc_cvr_tr_t1910, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1910, 0.65).

% Extraction over time
narrative_ontology:measurement(dmc_cvr_be_t1890, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1890, 0.55).
narrative_ontology:measurement(dmc_cvr_be_t1894, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1894, 0.62).
narrative_ontology:measurement(dmc_cvr_be_t1898, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1898, 0.7).
narrative_ontology:measurement(dmc_cvr_be_t1902, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1902, 0.74).
narrative_ontology:measurement(dmc_cvr_be_t1904, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1904, 0.78).
narrative_ontology:measurement(dmc_cvr_be_t1906, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1906, 0.8).
narrative_ontology:measurement(dmc_cvr_be_t1910, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1910, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(dmc_cvr_su_t1890, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1890, 0.6).
narrative_ontology:measurement(dmc_cvr_su_t1894, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1894, 0.7).
narrative_ontology:measurement(dmc_cvr_su_t1898, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1898, 0.78).
narrative_ontology:measurement(dmc_cvr_su_t1902, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1902, 0.82).
narrative_ontology:measurement(dmc_cvr_su_t1904, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1904, 0.88).
narrative_ontology:measurement(dmc_cvr_su_t1906, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1906, 0.9).
narrative_ontology:measurement(dmc_cvr_su_t1910, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1910, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__coercion_visibility_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(divine_marriage_command__coercion_visibility_reading, 0.08).
narrative_ontology:affects_constraint(divine_marriage_command__coercion_visibility_reading, divine_marriage_command__continuationist_reading).
narrative_ontology:affects_constraint(divine_marriage_command__coercion_visibility_reading, divine_marriage_command__substitutionist_reading).

% DUAL FORMULATION NOTE:
% This constraint story decomposes the 'divine marriage command' kernel into three structurally distinct readings. The coercion-visibility reading (this file) treats the post-Manifesto ban as a snare driven by federal coercion. The continuationist_reading treats the original command as still binding (mountain/rope from believer seat). The substitutionist_reading treats the Manifesto as new revelation (rope/mountain). The ε values diverge widely: this reading ε ≈ 0.78; continuationist ε ≈ 0.15 (from believer seat); substitutionist ε ≈ 0.25. They are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(divine_marriage_command__coercion_visibility_reading, institutional, 0.15).
constraint_indexing:directionality_override(divine_marriage_command__coercion_visibility_reading, powerless, 0.95).
constraint_indexing:directionality_override(divine_marriage_command__coercion_visibility_reading, moderate, 0.9).
constraint_indexing:directionality_override(divine_marriage_command__coercion_visibility_reading, organized, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
