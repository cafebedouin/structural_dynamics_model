% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__balanced_contestation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-25
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_boundary__balanced_contestation_reading, []).

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
 *   constraint_id: basic_law_interpretive_boundary__balanced_contestation_reading
 *   human_readable: Basic Law Interpretive Boundary — Balanced Contestation Reading
 *   domain: constitutional_law/comparative_constitutionalism/judicial_review_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the 'balanced contestation' reading of
 *   the basic_law_interpretive_boundary kernel. The reading asserts that
 *   constitutional courts and legislatures each hold legitimate but bounded
 *   authority: courts interpret within their jurisdictional domain, while
 *   legislatures retain ultimate sovereign power constrained by international
 *   obligations and norms of judicial independence. The arrangement produces
 *   institutional dialogue rather than hierarchy; neither institution is
 *   fully dominant; extractiveness varies by policy domain; and a triadic
 *   negotiation among court, executive, and legislature governs enforcement.
 *   The constraint is claimed as a tangled_rope — genuine coordination
 *   (dialogic constitutionalism) combined with asymmetric extraction
 *   (variable ε across domains, compliance costs borne by executive and
 *   minority communities). The sibling readings (judicial_supremacy_reading,
 *   parliamentary_sovereignty_reading) are separate constraint stories linked
 *   via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - constitutional_court: Primary interpreter (institutional/arbitrage) — holds interpretive authority within domain, benefits from judicial independence norms
 *   - knesset: Primary legislator (institutional/arbitrage) — retains ultimate sovereign power, benefits from democratic legitimacy
 *   - executive_branch: Implementation agent (powerful/constrained) — bears compliance costs, limited exit, subject to both court and legislature
 *   - minority_communities: Rights-holders (powerless/trapped) — depend on judicial protection but lack direct voice in boundary-setting
 *   - constitutional_scholars: Analytical observers (analytical/analytical) — track boundary shifts, provide epistemic infrastructure
 *   - international_courts: External normative constraint (organized/analytical) — supply international obligations that bound legislative sovereignty
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__balanced_contestation_reading, 0.38).
domain_priors:suppression_score(basic_law_interpretive_boundary__balanced_contestation_reading, 0.42).
domain_priors:theater_ratio(basic_law_interpretive_boundary__balanced_contestation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__balanced_contestation_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_boundary__balanced_contestation_reading, "Basic Law Interpretive Boundary — Balanced Contestation Reading").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__balanced_contestation_reading, "constitutional_law/comparative_constitutionalism/judicial_review_theory").

domain_priors:requires_active_enforcement(basic_law_interpretive_boundary__balanced_contestation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__balanced_contestation_reading, '6086606f-3e9d-4454-959a-787c4c90ebc7').
narrative_ontology:cs_kernel_codification('6086606f-3e9d-4454-959a-787c4c90ebc7', formalized).
narrative_ontology:cs_authority_grounding('6086606f-3e9d-4454-959a-787c4c90ebc7', lineage).
narrative_ontology:cs_interpretation_layer_present('6086606f-3e9d-4454-959a-787c4c90ebc7').
narrative_ontology:cs_reading_relation('6086606f-3e9d-4454-959a-787c4c90ebc7', basic_law_interpretive_boundary__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('6086606f-3e9d-4454-959a-787c4c90ebc7', basic_law_interpretive_boundary__parliamentary_sovereignty_reading, forecloses).
narrative_ontology:cs_axiom('6086606f-3e9d-4454-959a-787c4c90ebc7', foundational, institutional_authority_is_bounded_and_shared).
narrative_ontology:cs_axiom_status(institutional_authority_is_bounded_and_shared, holdable).
narrative_ontology:cs_axiom_grounding('6086606f-3e9d-4454-959a-787c4c90ebc7', institutional_authority_is_bounded_and_shared, conventional).
narrative_ontology:cs_axiom('6086606f-3e9d-4454-959a-787c4c90ebc7', secondary, legislature_constrained_by_international_obligations_and_judicial_independence_norms).
narrative_ontology:cs_axiom_status(legislature_constrained_by_international_obligations_and_judicial_independence_norms, holdable).
narrative_ontology:cs_axiom_grounding('6086606f-3e9d-4454-959a-787c4c90ebc7', legislature_constrained_by_international_obligations_and_judicial_independence_norms, conventional).
narrative_ontology:cs_reference_frame('6086606f-3e9d-4454-959a-787c4c90ebc7', dialogic_constitutionalism_model).
narrative_ontology:cs_drift_state('6086606f-3e9d-4454-959a-787c4c90ebc7', post_2023_judicial_reform_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6086606f-3e9d-4454-959a-787c4c90ebc7', '2026-07-25T12:00:00Z').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__balanced_contestation_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, constitutional_court).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, knesset).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, executive_branch).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, minority_communities).
narrative_ontology:constraint_vindicates(basic_law_interpretive_boundary__balanced_contestation_reading, dialogic_constitutionalism).
narrative_ontology:constraint_vindicates(basic_law_interpretive_boundary__balanced_contestation_reading, institutional_balance_doctrine).
narrative_ontology:constraint_vindicates(basic_law_interpretive_boundary__balanced_contestation_reading, international_law_as_constraint_on_sovereignty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercises judicial review over legislation and executive action, interpreting Basic Laws within its self-declared jurisdictional domain. Benefits from institutional independence, international judicial networks, and the prestige of constitutional guardianship. Can leverage international law and comparative precedent to reinforce its authority. Exit options include judicial dialogue with foreign courts and strategic avoidance of political questions.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, constitutional_court, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__balanced_contestation_reading, constitutional_court, beneficiary).

% Enacts Basic Laws and ordinary legislation by simple majority, claims ultimate sovereign authority to interpret and amend Basic Laws. Benefits from democratic legitimacy, control over legislative agenda, and the ability to respond to court rulings with new legislation. Constrained by international treaty obligations and norms of judicial independence that limit override strategies. Exit options include constitutional amendment, legislative override bills, and public appeals to democratic mandate.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, knesset, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__balanced_contestation_reading, knesset, beneficiary).

% Implements both court rulings and legislation, bearing compliance costs from contradictory directives. Subject to judicial review of administrative decisions and legislative oversight. In triadic negotiation, the executive sometimes aligns with the court against legislative overreach, sometimes with the legislature against judicial activism. Cannot easily exit either authority; resignation or elections are high-cost exits. Compliance costs vary by policy domain (high in security/human rights, lower in economic regulation).
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, executive_branch, payer,
    powerful, biographical, constrained, national).

% Depend on constitutional court for protection of collective and individual rights (language, education, religious autonomy, land). Lack direct representation in boundary-setting dialogue; their interests are mediated through court petitions and legislative advocacy. When court-legislature dialogue breaks down, minority rights are often the first sacrificed. Exit options are minimal: emigration (high cost), political mobilization (structurally disadvantaged), or litigation (dependent on court willingness).
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, minority_communities, payer,
    powerless, biographical, trapped, national).

% Provide the epistemic infrastructure for all three readings: doctrinal analysis, comparative theory, historical narrative. Their work legitimizes or delegitimizes each reading in public and professional discourse. They do not collect rents from the constraint nor bear its compliance costs. Their exit is analytical — they can shift frameworks, but their professional reputation is tied to the Israeli constitutional project.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% European Court of Human Rights, UN treaty bodies, and other international tribunals supply the international obligations that constrain Knesset sovereignty and reinforce judicial independence norms. They are excluded from domestic dialogue — their rulings are cited but not binding in Israeli domestic law. They would object to parliamentary sovereignty reading's dismissal of international law, but have no formal seat in the Israeli constitutional conversation.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, international_courts, excluded,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_boundary__balanced_contestation_reading, diffuse).
narrative_ontology:fixing_cost_class(basic_law_interpretive_boundary__balanced_contestation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for resolving conflicts between judicial review and legislative supremacy without either institution claiming final authority, enabling incremental constitutional development through dialogue rather than confrontation.
% TRANSFER_FUNCTION: Moves interpretive authority from the legislature to the court in rights-adjudication domains (human rights, minority protection), and moves policy discretion from the court to the legislature in security and economic domains. The transfer is bidirectional and domain-contingent.
% ABSENT_VOICES: Palestinian citizens of Israel and occupied Palestinian residents are structurally excluded from the Knesset's sovereign constituency and have limited standing in the Supreme Court; they would challenge both the court's limited protection and the legislature's majoritarian override power. Their absence is baked into the kernel's founding moment (1948/1967).
% DISAPPEARANCE_RATIONALE: If the balanced contestation boundary vanished, either judicial supremacy (court becomes final arbiter) or parliamentary sovereignty (Knesset can override at will) would likely crystallize. The triadic negotiation space would collapse, international obligations would lose domestic bite, and minority protections would depend entirely on which institution captures the boundary.
% FOUNDING_PROBLEM: The 1992 Basic Laws (Human Dignity, Freedom of Occupation) created a constitutional framework without a constitution, leaving the interpretive boundary between court and legislature undefined. The founding problem was how to exercise judicial review in a system with parliamentary sovereignty and no entrenched constitutional text.
% FOUNDING_PROBLEM_CORROBORATION: The constitutional court attests the problem is live (constitution remains incomplete). The Knesset majority (2023 judicial reform coalition) attests it is dead (Basic Laws are ordinary statutes). Comparative constitutional scholars (Hirschl, Sajó, Landau) attest it is contested — the Israeli model is a distinct species of 'constitutionalism without constitution' where the founding problem persists by design.
narrative_ontology:disappearance_verdict(basic_law_interpretive_boundary__balanced_contestation_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_boundary__balanced_contestation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__balanced_contestation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(basic_law_interpretive_boundary__balanced_contestation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_boundary__balanced_contestation_reading, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__balanced_contestation_reading_tests).
:- end_tests(basic_law_interpretive_boundary__balanced_contestation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) reflects the variable policy-domain extraction: high in human rights (court extracts compliance from legislature/executive), low in security (legislature extracts deference from court). Suppression (0.42) captures the active enforcement needed to maintain the boundary — judicial review strikes down laws, legislative override threats discipline court, international pressure constrains both. Theater ratio (0.28) indicates meaningful but imperfect dialogue: public hearings, written opinions, and legislative responses perform coordination while masking power struggles. Accessibility collapse (0.55) shows alternatives (pure judicial supremacy, pure parliamentary sovereignty) remain conceptually available but politically costly. Resistance (0.61) reflects ongoing contestation from all three institutional seats and civil society. The measurement series uses a shared time grid (1992–2024) with six points capturing the post-1992 constitutional revolution, the 1998-2006 activist period, the 2015-2020 tension, and the 2023-2024 judicial reform crisis.
 *
 * PERSPECTIVAL GAP:
 *   The court experiences the constraint as rope (coordination enabling rights protection). The knesset experiences it as scaffold (transitional arrangement pending constitutional completion). The executive experiences it as snare (extraction without voice). Minority communities experience it as snare (dependence on court they cannot influence). The engine's per-seat classification will capture this divergence; the authored claim (tangled_rope) represents the system-level structural truth.
 *
 * DIRECTIONALITY LOGIC:
 *   The constitutional court and knesset are dual agenda_setters and mutual beneficiaries: each gains legitimate authority from the arrangement (court gets interpretive monopoly, legislature gets final amendment power). Their directionality d is low (beneficiary end) because the constraint subsidizes their institutional position. The executive_branch is a payer: it implements both court rulings and legislation, bears compliance costs, and has constrained exit (cannot easily escape either authority). Its d is high (target end). Minority_communities are payers with trapped exit: they depend on the court for rights protection but have no leverage over the boundary. International_courts are observers with analytical exit: they supply constraining norms but do not participate in domestic enforcement. Constitutional_scholars are pure observers. The engine will compute per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1992 Basic Laws) was establishing a constitutional framework without a single-document constitution, balancing judicial review with parliamentary sovereignty. That problem is contested: the court says it's live (constitution incomplete), the legislature says it's dead (Basic Laws are ordinary statutes), scholars say it's contested. The arrangement persists not because the founding problem is solved but because no actor can unilaterally impose its preferred resolution — a classic tangled_rope dynamic where coordination function (dialogue) and extraction function (domain-variable power) are fused.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'How does the balanced contestation reading of the basic law interpretive boundary relate to the sibling readings (judicial supremacy, parliamentary sovereignty) within the same kernel?',
    'Structural comparison of the three readings'' beneficiary/victim configurations, enforcement logics, and drift trajectories; the engine''s foreclosure computation from cs_structure.reading_relations and axioms will test logical exclusivity.',
    'If the engine computes foreclosure between this reading and either sibling, the kernel is genuinely contested (mutually exclusive frameworks). If coexistence is computed, the kernel admits pluralistic instantiation. The classification of this reading (tangled_rope vs rope vs scaffold) may shift depending on whether the boundary is experienced as coordination or extraction by each institutional seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer-frame structural relationship among the three readings of the basic_law_interpretive_boundary kernel.').

omega_variable(
    extraction_domain_variance,
    'Does the base extractiveness (ε) of this constraint genuinely vary by policy domain, or is the variance an artifact of measuring different observables?',
    'Domain-disaggregated measurement of extractiveness across policy areas (human rights, security, economic regulation, religious affairs) using the same structural referent — the standing arrangement of interpretive authority. If ε varies systematically with domain, the constraint may be a family of domain-specific constraints rather than a single constraint.',
    'If ε is domain-invariant, the balanced contestation reading''s claim of variable extraction is a mischaracterization; the constraint would be a single tangled_rope with uniform ε. If ε varies, the ε-invariance principle requires decomposition into domain-specific constraint stories linked by network.affects_constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_domain_variance, empirical, 'Whether the balanced contestation reading''s variable-extraction claim survives ε-invariance testing.').

omega_variable(
    executive_branch_position_ambiguity,
    'Is the executive branch a victim of the interpretive boundary (bearing compliance costs without voice) or a participant in the triadic negotiation (with its own leverage)?',
    'Analyze executive branch behavior in constitutional crises: does it invoke judicial review strategically, comply under protest, or negotiate directly with both court and legislature? Track exit options and power exercises across crises.',
    'If the executive is a strategic participant, the constraint is a three-way coordination (potentially rope or scaffold). If it is a coerced payer, the extraction asymmetry strengthens the tangled_rope classification and the executive becomes a victim seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(executive_branch_position_ambiguity, empirical, 'Structural position of the executive branch in the court-legislature-executive triad.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__balanced_contestation_reading, 1992, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basic_law_balanced_tr_t1992, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 1992, 0.12).
narrative_ontology:measurement(basic_law_balanced_tr_t1998, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 1998, 0.18).
narrative_ontology:measurement(basic_law_balanced_tr_t2006, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 2006, 0.22).
narrative_ontology:measurement(basic_law_balanced_tr_t2015, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 2015, 0.26).
narrative_ontology:measurement(basic_law_balanced_tr_t2020, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 2020, 0.28).
narrative_ontology:measurement(basic_law_balanced_tr_t2024, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(basic_law_balanced_be_t1992, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 1992, 0.22).
narrative_ontology:measurement(basic_law_balanced_be_t1998, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 1998, 0.28).
narrative_ontology:measurement(basic_law_balanced_be_t2006, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 2006, 0.33).
narrative_ontology:measurement(basic_law_balanced_be_t2015, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 2015, 0.36).
narrative_ontology:measurement(basic_law_balanced_be_t2020, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 2020, 0.38).
narrative_ontology:measurement(basic_law_balanced_be_t2024, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 2024, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(basic_law_balanced_su_t1992, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 1992, 0.35).
narrative_ontology:measurement(basic_law_balanced_su_t1998, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 1998, 0.38).
narrative_ontology:measurement(basic_law_balanced_su_t2006, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 2006, 0.4).
narrative_ontology:measurement(basic_law_balanced_su_t2015, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 2015, 0.42).
narrative_ontology:measurement(basic_law_balanced_su_t2020, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 2020, 0.42).
narrative_ontology:measurement(basic_law_balanced_su_t2024, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__balanced_contestation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(basic_law_interpretive_boundary__balanced_contestation_reading, 0.1).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__balanced_contestation_reading, basic_law_interpretive_boundary__judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__balanced_contestation_reading, basic_law_interpretive_boundary__parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint and its two siblings form the basic_law_interpretive_boundary constraint family. The balanced contestation reading occupies the middle ground: it forecloses both judicial supremacy (by denying court finality) and parliamentary sovereignty (by denying legislative override). The ε values differ: judicial supremacy reading has low ε (court coordination), parliamentary sovereignty reading has low ε (legislative coordination), balanced contestation has higher ε (triadic extraction). The family shares the kernel (Basic Laws as interpretive boundary) but disagrees on the authority grounding and drift trajectory.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(basic_law_interpretive_boundary__balanced_contestation_reading, institutional, 0.15).
constraint_indexing:directionality_override(basic_law_interpretive_boundary__balanced_contestation_reading, powerful, 0.75).
constraint_indexing:directionality_override(basic_law_interpretive_boundary__balanced_contestation_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
