% ============================================================================
% CONSTRAINT STORY: modern_judicialization__human_rights_act_1998
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_modern_judicialization__human_rights_act_1998, []).

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
 *   constraint_id: modern_judicialization__human_rights_act_1998
 *   human_readable: Human Rights Act 1998: Judicial Authority to Suppress Rights-Blind Statutory Interpretation
 *   domain: constitutional_law/judicial_review
 *
 * SUMMARY:
 *   The Human Rights Act 1998 brought the European Convention on Human Rights
 *   into domestic UK law, creating a structural constraint on statutory
 *   interpretation: courts must read all legislation compatibly with
 *   Convention rights and, when they cannot, declare the statute
 *   incompatible. This constraint is one reading of a contested kernel about
 *   modern judicialization — how the post-1997 legal reforms (the Act,
 *   devolution, EU membership, Supreme Court creation) reshaped the balance
 *   between courts, Parliament, and rights protection. This reading
 *   emphasizes the rights-protective interpretive duty and the declaration of
 *   incompatibility device as the mechanism for reconciling judicial
 *   constraint (courts cannot strike down legislation) with rights
 *   enforcement (courts can compel legislative attention). The constraint
 *   exhibits classic tangled-rope structure: genuine coordination (bringing
 *   rights home solves the access problem) layered with asymmetric extraction
 *   (claimants bear costs, courts have limited remedies, Parliament
 *   determines final relief). The theater ratio has risen slightly over the
 *   interval (0.38 to 0.48) as the gap between declarations of
 *   incompatibility and actual legislative remedy has grown, indicating
 *   increasing performativity in the constraint's operation. Extractiveness
 *   has also risen (0.22 to 0.38) as the courts' interpretive duty has
 *   expanded into areas Parliament did not clearly intend to protect,
 *   creating extraction from Parliament's legislative sovereignty while
 *   protecting claimant rights.
 *
 * KEY AGENTS:
 *   - Convention Claimants in Domestic Court (moderate/constrained): Primary beneficiaries — gain access to Convention remedies in domestic courts without Strasbourg litigation, but bear high costs and face limited remedies via declaration of incompatibility
 *   - UK Courts (institutional/arbitrage): Institutional beneficiary — gain new interpretive authority and coordinating role, experience constraint as empowering rather than extractive
 *   - Parliament (organized/constrained): Dual role — retains formal sovereignty but is constrained to legislate compatibly with rights or face declarations of incompatibility and political pressure
 *   - Rights-Blind Statutory Meaning (powerless/trapped): Structural victim — the statute's unprotective interpretation is now permanently subordinated; no escape from compatibility reading
 *   - Legislative Sovereignty in Narrow Construction (powerless/trapped): Structural victim — Parliament's historical freedom to legislate without rights constraints is suppressed; courts enforce Convention compatibility
 *   - European Convention Framework (institutional/arbitrage): Intermediate — provides the rights floor and legitimacy for the constraint, but the actual protection mechanism is UK judicial interpretation
 *   - International Human Rights Community (organized/arbitrage): Secondary beneficiary — sees the Act as solving access problems and coordinating rights enforcement domestically
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(modern_judicialization__human_rights_act_1998, 0.38).
domain_priors:suppression_score(modern_judicialization__human_rights_act_1998, 0.52).
domain_priors:theater_ratio(modern_judicialization__human_rights_act_1998, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(modern_judicialization__human_rights_act_1998, extractiveness, 0.38).
narrative_ontology:constraint_metric(modern_judicialization__human_rights_act_1998, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(modern_judicialization__human_rights_act_1998, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(modern_judicialization__human_rights_act_1998, tangled_rope).
narrative_ontology:human_readable(modern_judicialization__human_rights_act_1998, "Human Rights Act 1998: Judicial Authority to Suppress Rights-Blind Statutory Interpretation").
narrative_ontology:topic_domain(modern_judicialization__human_rights_act_1998, "constitutional_law/judicial_review").

domain_priors:requires_active_enforcement(modern_judicialization__human_rights_act_1998).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(modern_judicialization__human_rights_act_1998, '769baa06-1afb-45a1-b24d-032ecef1200a').
narrative_ontology:cs_kernel_codification('769baa06-1afb-45a1-b24d-032ecef1200a', formalized).
narrative_ontology:cs_authority_grounding('769baa06-1afb-45a1-b24d-032ecef1200a', lineage).
narrative_ontology:cs_interpretation_layer_present('769baa06-1afb-45a1-b24d-032ecef1200a').
narrative_ontology:cs_reading_relation('769baa06-1afb-45a1-b24d-032ecef1200a', modern_judicialization__devolution_settlements, influences).
narrative_ontology:cs_reading_relation('769baa06-1afb-45a1-b24d-032ecef1200a', modern_judicialization__eu_membership_and_exit, coexists_with).
narrative_ontology:cs_reading_relation('769baa06-1afb-45a1-b24d-032ecef1200a', modern_judicialization__uk_supreme_court_creation, influences).
narrative_ontology:cs_axiom('769baa06-1afb-45a1-b24d-032ecef1200a', foundational, courts_must_read_statutes_compatibly_with_convention_rights).
narrative_ontology:cs_axiom_status(courts_must_read_statutes_compatibly_with_convention_rights, holdable).
narrative_ontology:cs_axiom_grounding('769baa06-1afb-45a1-b24d-032ecef1200a', courts_must_read_statutes_compatibly_with_convention_rights, conventional).
narrative_ontology:cs_axiom('769baa06-1afb-45a1-b24d-032ecef1200a', foundational, declaration_of_incompatibility_respects_parliamentary_sovereignty).
narrative_ontology:cs_axiom_status(declaration_of_incompatibility_respects_parliamentary_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('769baa06-1afb-45a1-b24d-032ecef1200a', declaration_of_incompatibility_respects_parliamentary_sovereignty, instrumental).
narrative_ontology:cs_reference_frame('769baa06-1afb-45a1-b24d-032ecef1200a', rights_protective_statutory_interpretation).
narrative_ontology:cs_drift_state('769baa06-1afb-45a1-b24d-032ecef1200a', contemporary_remedy_gap, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('769baa06-1afb-45a1-b24d-032ecef1200a', '').
narrative_ontology:cs_kernel_id(modern_judicialization__human_rights_act_1998, modern_judicialization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(modern_judicialization__human_rights_act_1998, convention_claimants).
narrative_ontology:constraint_beneficiary(modern_judicialization__human_rights_act_1998, uk_courts).
narrative_ontology:constraint_victim(modern_judicialization__human_rights_act_1998, rights_blind_statutory_meaning).
narrative_ontology:constraint_victim(modern_judicialization__human_rights_act_1998, legislative_sovereignty_in_narrow_construction).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RIGHTS-BLIND STATUTORY MEANING (SNARE) — The plain language of a statute that fails to protect Convention rights is now permanently subordinated. Courts must read it compatibly with rights or declare incompatibility; the statute's own unprotective meaning cannot prevail. No exit: the statute is trapped in the interpretive hierarchy. Maximum suppression — the statute's alternative reading is foreclosed by judicial duty.
constraint_indexing:constraint_classification(modern_judicialization__human_rights_act_1998, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONVENTION CLAIMANT (TANGLED ROPE) — Benefits from the Act's guarantee that Convention rights are enforceable in domestic courts (coordination function: brings rights home). But also experiences extraction: access barriers remain high (legal costs, standing requirements, burden of proof), and the remedy is limited (courts can declare incompatibility but cannot strike down primary legislation). Genuine coordination (rights-protective interpretation) with embedded asymmetry (claimants bear costs of litigation; legislatures determine actual remedy).
constraint_indexing:constraint_classification(modern_judicialization__human_rights_act_1998, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: UK COURT SYSTEM (ROPE) — Gains institutional authority and a new coordinating function: mediating between Parliament's legislation and Convention obligations. Courts experience the Act as empowering (access to rights-protective interpretation), not extractive. The constraint solves a coordination problem: how to reconcile domestic legislation with international human rights commitments without delegating legislative power to courts. Courts benefit from expanded interpretive authority and see the Act as coordination.
constraint_indexing:constraint_classification(modern_judicialization__human_rights_act_1998, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PARLIAMENT (TANGLED ROPE) — Experiences both coordination and extraction. Coordination: Parliament retains formal legislative sovereignty (cannot be overridden by courts). Extraction: Parliament is constrained to legislate compatibly with Convention rights (interpreted by courts), or declare incompatibility and accept political costs. The duty to read statutes compatibly reduces Parliament's de facto interpretive freedom without removing its de jure supremacy. Genuine hybrid: Parliament still coordinates its own legislation, but within rights-protective guardrails.
constraint_indexing:constraint_classification(modern_judicialization__human_rights_act_1998, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: EUROPEAN CONVENTION FRAMEWORK (PITON) — The Act brought the Convention 'home' by making it domestically enforceable, but the substantive work of rights protection is now mediated through UK judicial interpretation and remedial doctrine. The Convention text persists; the actual protection depends on UK courts' willingness to read statutes expansively. Theater is high: the Act creates the appearance of direct Convention protection while the real mechanism is judicial discretion in interpretation and the inability of courts to strike down primary legislation. The Convention's role is performative in relation to binding legal change — it frames, but courts and Parliament decide.
constraint_indexing:constraint_classification(modern_judicialization__human_rights_act_1998, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational analytical perspective, the separation of powers requires courts to interpret narrowly and Parliament to legislate. The duty to read statutes compatibly with rights can be framed as a natural principle of constitutional architecture: the judiciary cannot rewrite legislation, only interpret it charitably. The constraint appears as an immutable feature of constitutional order. However, this reading naturalizes a contingent institutional choice (compatibility reading, declaration of incompatibility device) as a law of constitutional nature. The engine's false summit detector will flag this as potential naturalization.
constraint_indexing:constraint_classification(modern_judicialization__human_rights_act_1998, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: INTERNATIONAL HUMAN RIGHTS COMMUNITY (ROPE) — Sees the Act as genuine coordination: bringing Convention protections into domestic enforceability solves the problem of access to international rights remedies. The Act enables domestic litigation instead of requiring trips to Strasbourg. Organized actors (NGOs, human rights advocates) benefit from expanded domestic access and experience the constraint as coordinating (solving the accessibility problem without extracting from claimants). No significant victim set — this perspective shows the constraint's pure coordination face.
constraint_indexing:constraint_classification(modern_judicialization__human_rights_act_1998, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(modern_judicialization__human_rights_act_1998_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(modern_judicialization__human_rights_act_1998, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(modern_judicialization__human_rights_act_1998, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(modern_judicialization__human_rights_act_1998, TR),
    TR >= 0.70.

:- end_tests(modern_judicialization__human_rights_act_1998_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The Act creates genuine coordination (bringing Convention home solves the access-to-justice problem) but is embedded in asymmetric extraction. Claimants benefit from domestic access but bear litigation costs, face burden of proof, and receive limited remedies (declarations without power to force relief). Courts benefit from expanded authority. Parliament experiences extraction: its legislative freedom is constrained by the duty to read compatibly, and incompatible statutes trigger political pressure to amend. The extractiveness has grown over the interval because courts' compatibility readings have become more expansive, and the gap between declarations and remedies has widened. Suppression (0.52): Moderate-high. The rights-blind interpretation is heavily suppressed — courts are duty-bound to read compatibly or declare incompatibility; pure rights-ignoring interpretation is no longer a live option in domestic law. But suppression is not total: Parliament can still declare incompatibility is acceptable (formal sovereignty), and courts' remedial power is limited (declaration, not injunction or damages). Theater ratio (0.48): Moderate. The Act has genuine functional content (it does bring Convention rights home and does enable domestic litigation), but the theater ratio rises because declarations of incompatibility often do not produce prompt legislative remedy. The performative element increases as the gap between declaring a statute incompatible and actually fixing it grows. The constraint coordinates (bringing rights home) but increasingly performs rather than substantively protects (declarations without remedy).
 *
 * PERSPECTIVAL GAP:
 *   This constraint reveals the full indexical range. The rights-blind statute (powerless/trapped) sees pure suppression (snare). The claimant (moderate/constrained) sees mixed coordination and extraction (tangled rope). The courts (institutional/arbitrage) see empowerment and coordination (rope). Parliament (organized/constrained) sees mixed constraint and retention of formal supremacy (tangled rope). The Convention as a framework (institutional/arbitrage) sees coordination — it is being enforced domestically. The international rights community (organized/arbitrage) sees genuine coordination — the Act solved the problem of access. Only the civilizational analytical observer (analytical/analytical) risks reading this as a natural law of constitutional separation of powers — a mistake the false summit detector should catch, because the constraint is a contested institutional choice (the HRA 1998 was a reform, not a constitutional necessity).
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation maps structural positions to d values. Convention claimants as beneficiaries with constrained exit (moderate power, cannot easily exit litigation system) derive d ≈ 0.55, producing high experienced extraction chi; but they also benefit (claimants are beneficiaries of the right to bring claims), so the net d is lower. Courts as institutional beneficiaries with arbitrage options (can choose how expansively to interpret) derive low d → negative chi — courts experience the constraint as net-beneficial. Parliament as organized beneficiary-and-victim (formal sovereignty retained but constrained by compatibility duty) derives d ≈ 0.50 (symmetric). Rights-blind statutory meaning as a victim with no exit (trapped) derives d ≈ 0.95 → high chi. The perspectival gaps are real and structural: beneficiary courts experience rope; victim Parliament experiences tangled rope; victims of interpretation subordination experience snare.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by declaring which functions are genuinely coordinated and which are extraction. Genuine coordination: bringing Convention rights into domestic courts (solves access-to-justice problem, enables domestic litigation without Strasbourg travel, makes courts active defenders of rights). Embedded extraction: claimants bear costs and face limited remedies; Parliament's legislative freedom is constrained; courts' interpretive power is expanded without clear legitimacy limit. The constraint is not purely one or the other; it is the tangled-rope hybrid. The mandatrophy is resolved by tracking the declaration-to-remedy ratio: when declarations are followed by legislative amendment within reasonable time, the constraint moves toward rope (coordination with prompt remedy). When declarations pile up without remedy, the constraint moves toward piton (performative) or snare (empty promise of relief).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretation_ceiling_ambiguity,
    'What limits the scope of ''compatible'' statutory interpretation? Can courts read statutes as broadly as Convention jurisprudence permits, or must they stop before the statute''s language becomes openly rewritten?',
    'Corpus analysis of judicial reasoning in compatibility cases: how far courts stretch interpretive charity before declaring incompatibility instead. Comparison with courts in other Convention jurisdictions (Irish, French, German) and their interpretation ceilings.',
    'If interpretation ceiling is high: courts exercise substantial creative power (moves toward snare for Parliament). If ceiling is low: courts are genuinely constrained, and the constraint is less extractive (moves toward rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretation_ceiling_ambiguity, conceptual, 'How far can courts stretch statutory interpretation before declaring incompatibility?').

omega_variable(
    declaration_of_incompatibility_remedy_gap,
    'Does the declaration of incompatibility device constitute an effective remedy, or is it a performative gesture that shifts the burden to Parliament while leaving claimants uncompensated?',
    'Tracking: (1) frequency of declarations; (2) legislative response rate and speed; (3) final remedy outcomes for claimants after declaration; (4) claimant satisfaction metrics. Compare against equivalent rights enforcement via European Court remedies.',
    'If declarations produce prompt legislative remedy and claimant relief: the constraint is tangled rope (genuine coordination with asymmetry). If declarations persist without legislative response or claimant relief: the constraint moves toward piton (performative) or snare (empty remedy traps victims in litigation).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(declaration_of_incompatibility_remedy_gap, empirical, 'Whether declaration of incompatibility is an effective remedy or performative gesture').

omega_variable(
    reading_constraint_vs_judicial_power_delegation,
    'Is the duty to read statutes compatibly with rights a genuine constraint on judicial power, or does it delegate substantial law-making power to courts by permitting expansive interpretation?',
    'Comparison of statutory interpretation scope under HRA vs. pre-HRA case law; measurement of statutory construction breadth; correlation between HRA compatibility readings and outcomes that would not have been reachable under traditional narrow construction.',
    'If the duty constrains (courts cannot go beyond what statutory language permits): the constraint is rope with respect to courts (coordinate with Parliament). If the duty enables (courts read statutes much more broadly than before): the constraint is extraction from Parliament''s legislative intent (moves toward snare for Parliament perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_constraint_vs_judicial_power_delegation, empirical, 'Whether compatibility reading is a constraint on or delegation of judicial power').

omega_variable(
    kernel_reading_natural_law_ambiguity,
    'Is this constraint a reading of the modern judicialization kernel (a contested institutional choice about how courts and Parliament coordinate rights protection), or a natural law of constitutional separation of powers?',
    'The kernel_context field identifies this as one reading of modern_judicialization. Sibling readings (devolution, EU membership, Supreme Court creation) treat the same era''s institutional reshaping through different axes. If this reading is one contested option among siblings, it is not a natural law — it is a committer choice. The false summit detector should flag the mountain perspective as naturalization.',
    'If reading (contested choice): the constraint''s legitimacy depends on continued adherence to the HRA framework and rights-protective interpretation culture. If natural law (separation of powers): the constraint should persist regardless of political winds or institutional reform. The difference is whether the constraint can be reformed (reading) or only interpreted (natural law).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_natural_law_ambiguity, conceptual, 'Is this constraint a natural law or a contested reading of modern judicialization?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(modern_judicialization__human_rights_act_1998, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hra_theater_1998, modern_judicialization__human_rights_act_1998, theater_ratio, 0, 0.38).
narrative_ontology:measurement(hra_theater_2005, modern_judicialization__human_rights_act_1998, theater_ratio, 7, 0.45).
narrative_ontology:measurement(hra_theater_2012, modern_judicialization__human_rights_act_1998, theater_ratio, 14, 0.48).

% Extraction over time
narrative_ontology:measurement(hra_extractiveness_1998, modern_judicialization__human_rights_act_1998, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(hra_extractiveness_2005, modern_judicialization__human_rights_act_1998, base_extractiveness, 7, 0.35).
narrative_ontology:measurement(hra_extractiveness_2012, modern_judicialization__human_rights_act_1998, base_extractiveness, 14, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(hra_suppression_1998, modern_judicialization__human_rights_act_1998, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(hra_suppression_2005, modern_judicialization__human_rights_act_1998, suppression_requirement, 7, 0.5).
narrative_ontology:measurement(hra_suppression_2012, modern_judicialization__human_rights_act_1998, suppression_requirement, 14, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(modern_judicialization__human_rights_act_1998, enforcement_mechanism).
narrative_ontology:affects_constraint(modern_judicialization__human_rights_act_1998, modern_judicialization__devolution_settlements).
narrative_ontology:affects_constraint(modern_judicialization__human_rights_act_1998, modern_judicialization__eu_membership_and_exit).
narrative_ontology:affects_constraint(modern_judicialization__human_rights_act_1998, modern_judicialization__uk_supreme_court_creation).

% DUAL FORMULATION NOTE:
% The HRA 1998 reading is structurally linked to three sibling readings of the modern_judicialization kernel. Each reading analyzes a distinct institutional reform from the same era. This story focuses on the rights-protective interpretive duty; sibling stories analyze devolution (territorial redistribution of sovereignty), EU framing (external rights commitment), and Supreme Court creation (visible judicial independence). The four stories together form a presheaf of institutional transformation. The HRA reading has distinct epsilon (0.38) and is not subsumed by the other three — it is a specific focus on judicial-legislative coordination around rights protection.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(modern_judicialization__human_rights_act_1998, organized, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
