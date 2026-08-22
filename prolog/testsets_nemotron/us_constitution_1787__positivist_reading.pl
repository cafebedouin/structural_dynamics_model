% ============================================================================
% CONSTRAINT STORY: us_constitution_1787__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_1787__positivist_reading, []).

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
 *   constraint_id: us_constitution_1787__positivist_reading
 *   human_readable: US Constitution 1787 — Positivist Reading (Text + Democratic Amendments)
 *   domain: legal/constitutional
 *
 * SUMMARY:
 *   The positivist reading of the US Constitution holds that constitutional
 *   meaning consists exclusively of the written text (including its original
 *   public meaning) plus formal amendments ratified through Article V.
 *   Judicial interpretation is constrained to applying this fixed textual
 *   content — judges may not import evolving societal values, natural law
 *   principles, or policy preferences. This reading positions itself between
 *   originalism (which binds to framers' intent/understanding) and living
 *   constitutionalism (which treats the text as an aspirational framework).
 *   The constraint operates through judicial appointments, academic
 *   discourse, and political rhetoric that elevates textual fidelity as the
 *   touchstone of legitimacy.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_1787__positivist_reading, 0.28).
domain_priors:suppression_score(us_constitution_1787__positivist_reading, 0.45).
domain_priors:theater_ratio(us_constitution_1787__positivist_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_1787__positivist_reading, rope).
narrative_ontology:human_readable(us_constitution_1787__positivist_reading, "US Constitution 1787 — Positivist Reading (Text + Democratic Amendments)").
narrative_ontology:topic_domain(us_constitution_1787__positivist_reading, "legal/constitutional").

domain_priors:requires_active_enforcement(us_constitution_1787__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__positivist_reading, 'ec3c69ae-232f-45ef-b763-e6f6ded072e5').
narrative_ontology:cs_kernel_codification('ec3c69ae-232f-45ef-b763-e6f6ded072e5', fixed_text).
narrative_ontology:cs_authority_grounding('ec3c69ae-232f-45ef-b763-e6f6ded072e5', lineage).
narrative_ontology:cs_interpretation_layer_present('ec3c69ae-232f-45ef-b763-e6f6ded072e5').
narrative_ontology:cs_reading_relation('ec3c69ae-232f-45ef-b763-e6f6ded072e5', us_constitution_1787__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('ec3c69ae-232f-45ef-b763-e6f6ded072e5', us_constitution_1787__living_reading, influences).
narrative_ontology:cs_axiom('ec3c69ae-232f-45ef-b763-e6f6ded072e5', foundational, constitutional_meaning_exhausted_by_text_plus_amendments).
narrative_ontology:cs_axiom_status(constitutional_meaning_exhausted_by_text_plus_amendments, holdable).
narrative_ontology:cs_axiom_grounding('ec3c69ae-232f-45ef-b763-e6f6ded072e5', constitutional_meaning_exhausted_by_text_plus_amendments, conventional).
narrative_ontology:cs_axiom('ec3c69ae-232f-45ef-b763-e6f6ded072e5', foundational, judicial_interpretation_limited_to_textual_analysis).
narrative_ontology:cs_axiom_status(judicial_interpretation_limited_to_textual_analysis, holdable).
narrative_ontology:cs_axiom_grounding('ec3c69ae-232f-45ef-b763-e6f6ded072e5', judicial_interpretation_limited_to_textual_analysis, conventional).
narrative_ontology:cs_axiom('ec3c69ae-232f-45ef-b763-e6f6ded072e5', secondary, article_v_as_exclusive_legitimate_change_mechanism).
narrative_ontology:cs_axiom_status(article_v_as_exclusive_legitimate_change_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('ec3c69ae-232f-45ef-b763-e6f6ded072e5', article_v_as_exclusive_legitimate_change_mechanism, conventional).
narrative_ontology:cs_reference_frame('ec3c69ae-232f-45ef-b763-e6f6ded072e5', ratified_text_plus_formal_amendments).
narrative_ontology:cs_drift_state('ec3c69ae-232f-45ef-b763-e6f6ded072e5', contemporary_judicial_supremacy_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ec3c69ae-232f-45ef-b763-e6f6ded072e5', '').
narrative_ontology:cs_kernel_id(us_constitution_1787__positivist_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, constitutional_text_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, democratic_majorities).
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, legislative_actors).
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, rule_of_law_proponents).
narrative_ontology:constraint_victim(us_constitution_1787__positivist_reading, judicial_activists).
narrative_ontology:constraint_victim(us_constitution_1787__positivist_reading, unpopular_minorities_seeking_judicial_protection).
narrative_ontology:constraint_victim(us_constitution_1787__positivist_reading, living_constitutionalists).
narrative_ontology:constraint_vindicates(us_constitution_1787__positivist_reading, textual_supremacy_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_1787__positivist_reading, democratic_amendment_mechanism).
narrative_ontology:constraint_vindicates(us_constitution_1787__positivist_reading, judicial_restraint_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for constitutional interpretation strictly bound to the written text and formal amendments. They benefit from a stable, predictable constitutional framework that limits judicial discretion and preserves democratic lawmaking authority. Their influence depends on maintaining textual fidelity as the dominant interpretive norm.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, constitutional_text_advocates, beneficiary,
    organized, generational, constrained, national).

% Citizens and their representatives who gain constitutional change authority through the Article V amendment process rather than judicial reinterpretation. They benefit when constitutional meaning shifts only through deliberate democratic action. Their power is collective but episodic — activated during amendment campaigns.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, democratic_majorities, beneficiary,
    powerful, biographical, mobile, national).

% Congress and state legislatures whose policy-making authority expands when courts cannot override legislation through creative constitutional interpretation. They set the agenda for constitutional change through the amendment process and ordinary legislation within textual bounds.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, legislative_actors, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__positivist_reading, legislative_actors, agenda_setter).

% Legal scholars, judges, and citizens who prioritize legal predictability, constraint on government power, and democratic legitimacy. They benefit from a constitutional system where change follows prescribed procedures rather than judicial innovation. Their commitment is ideological and professional.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, rule_of_law_proponents, beneficiary,
    organized, generational, constrained, national).

% Judges and legal theorists who view constitutional interpretation as requiring substantive moral reasoning beyond the text. They bear the constraint's cost by having their interpretive authority limited to textual analysis. Their professional identity and institutional role are fused with expansive judicial review — exit means abandoning their conception of the judicial office.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, judicial_activists, payer,
    institutional, biographical, identity_locked, national).

% Groups that historically relied on judicial expansion of constitutional rights beyond text and original understanding to protect their interests. They bear the cost when democratic majorities can block constitutional protections through the amendment process's high thresholds. Their exit options are minimal — they cannot easily leave the jurisdiction or change the constitutional structure.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, unpopular_minorities_seeking_judicial_protection, payer,
    powerless, biographical, trapped, national).

% Scholars, judges, and advocates who argue constitutional meaning must evolve with societal values. They bear the cost of having their interpretive framework excluded from legitimacy under this reading. Their professional and intellectual identity is constituted through the living constitution paradigm — exit means abandoning their life's work and theoretical commitments.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, living_constitutionalists, payer,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__positivist_reading, living_constitutionalists, excluded).

% Proponents of the sibling originalist reading who share the textual constraint commitment but disagree on the role of historical intent. They observe from a proximate but distinct interpretive position — allied on judicial restraint, divided on historical binding.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, originalist_scholars_and_judges, observer,
    institutional, generational, analytical, national).

% Activists and scholars who seek to amend Article V itself to make the amendment process more accessible. They administer pressure on the constraint's democratic mechanism from within, arguing the current amendment threshold is too high to serve as a genuine democratic safety valve.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, constitutional_amendment_reformers, agenda_setter,
    moderate, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, determinate constitutional framework that coordinates governance across time and political disagreement by anchoring meaning in a fixed text amendable only through supermajoritarian democratic procedures — preventing both legislative drift and judicial aggrandizement.
% TRANSFER_FUNCTION: Moves constitutional change authority from courts (judicial interpretation) to democratic majorities (Article V amendments) — transferring the power to update constitutional meaning from an unelected, identity-locked judicial class to the politically accountable but procedurally constrained amendment process.
% ABSENT_VOICES: Future generations who will live under constitutional settlements they cannot amend easily; territories and populations subject to US constitutional authority without representation in the amendment process (e.g., Puerto Rico, DC); non-citizens affected by US constitutional jurisprudence. These voices are structurally excluded from the democratic amendment mechanism the reading elevates.
% DISAPPEARANCE_RATIONALE: If this constraint vanished overnight, constitutional meaning would shift immediately to whichever competing reading (originalist or living) captured judicial and political institutions. The textual anchor would dissolve, amendment politics would lose their focal point, and the balance between judicial review and democratic amendment would be renegotiated — a fundamental rearrangement of constitutional authority.
% FOUNDING_PROBLEM: The Articles of Confederation created a governing structure too weak to coordinate collective action across states; the 1787 Constitution established a stronger federal government but required a mechanism to prevent both legislative tyranny and judicial overreach while allowing legitimate constitutional change.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the Federalist Papers (external to any modern interpretive faction) and the constitutional text itself. Originalists and living constitutionalists agree the founding problem existed but disagree on whether it persists in the same form — originalists argue the structural solution (enumerated powers, separation of powers) remains operative; living constitutionalists argue modern governance complexity requires judicial adaptation. No single faction monopolizes the founding narrative.
narrative_ontology:disappearance_verdict(us_constitution_1787__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_1787__positivist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_1787__positivist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(us_constitution_1787__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_1787__positivist_reading, 0.28, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_1787__positivist_reading_tests).
:- end_tests(us_constitution_1787__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is moderate (0.28) because the constraint extracts interpretive authority from judges and protections from minorities who rely on judicial expansion, but provides genuine coordination value through stability and democratic legitimacy. Suppression (0.45) reflects the active exclusion of living constitutionalist arguments from mainstream legitimacy and the high barriers to amendment that lock in textual settlements. Theater ratio (0.35) captures the performative textualism that sometimes masks policy-driven outcomes. Accessibility collapse (0.55) and resistance (0.65) reflect the ongoing contest: alternatives (originalism, living constitutionalism) remain live and organized, but the textual anchor makes full exit from the constraint's framework difficult.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (legislators, democratic majorities), this constraint appears as a Rope — genuine coordination solving the problem of legitimate constitutional change. From the payer seats (judicial activists, unpopular minorities), it appears as a Snare — extraction of protective judicial review behind a coordination cover. From the originalist observer seat, it appears as a Tangled Rope — shared coordination function with contested extraction (historical intent vs. textual meaning). The engine computes this seat divergence from the structural data; the claimed_type (rope) reflects the authoring seat's assessment that the coordination function is genuine and dominant.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (textual advocates, democratic majorities, legislators, rule-of-law proponents) sit at low directionality (d ~ 0.2-0.3) — the constraint subsidizes their preferred governance mode. Payers (judicial activists, unpopular minorities, living constitutionalists) sit at high directionality (d ~ 0.7-0.9) — the constraint extracts their interpretive authority and protective remedies. Judicial activists are identity-locked (professional self-concept fused to expansive review); unpopular minorities are trapped (no exit from jurisdiction); living constitutionalists are identity-locked (intellectual life's work). Originalist observers sit at analytical (d ~ 0.5) — proximate allies who contest the historical-intent component. Amendment reformers are agenda-setters with constrained exit — they operate within the framework to modify its democratic mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (creating a governable union with legitimate change mechanisms) remains contested in status. The coordination function (stable textual anchor + democratic amendment) is live but degraded by Article V's near-impossibility — only 27 amendments in 239 years, none since 1992. The constraint persists partly through institutional inertia (judicial precedent, academic tradition) and partly because no faction can muster the supermajority to replace it. Mandatrophy is unresolved: the democratic amendment mechanism has atrophied as a functional safety valve, but the textual constraint itself remains the Schelling point for constitutional legitimacy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_frame_identity,
    'Does the positivist reading''s claim to constrain judicial activism genuinely reduce extraction, or does it merely shift extraction from judges to democratic majorities who can now entrench preferences through textual amendments?',
    'Empirical analysis of post-positivist-adoption constitutional politics: do amendment campaigns increase and succeed, or does judicial restraint simply lock in status quo distributions? Comparative study of jurisdictions with stronger/weaker judicial review.',
    'If extraction shifts rather than decreases, the constraint''s claimed_type (rope) masks a Snare dynamic where democratic majorities become the extractors. This would flip the beneficiary/victim structure: majorities become beneficiaries of extraction, minorities become victims of locked-in majoritarian preferences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_identity, empirical, 'Whether the positivist reading''s coordination function genuinely reduces net extraction or redistributes it.').

omega_variable(
    article_v_functionality,
    'Is the Article V amendment process a genuine democratic safety valve under modern conditions, or has its supermajoritarian threshold rendered it functionally inert — making the positivist reading''s democratic mechanism a theatrical cover for textual entrenchment?',
    'Historical analysis of amendment success rates under varying political conditions; game-theoretic modeling of Article V as a coordination mechanism; comparison with state constitutions with lower amendment thresholds.',
    'If Article V is functionally inert, the constraint''s claimed coordination function collapses — the ''democratic amendment'' beneficiary declaration becomes a false summit marker. The constraint would reclassify toward Snare (extraction via entrenchment) or Piton (atrophied coordination maintained theatrically).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_v_functionality, empirical, 'Whether the democratic amendment mechanism the reading elevates is functionally real or performative.').

omega_variable(
    textual_determinacy,
    'Does the constitutional text actually provide determinate answers to the contested questions that reach courts, or does ''textual constraint'' serve as a Rorschach where judges project preferred outcomes onto ambiguous language?',
    'Linguistic and legal analysis of constitutional text''s semantic precision on litigated issues; inter-coder reliability studies of textualist judges'' outcomes; comparison with statutory textualism where text is more detailed.',
    'If text is indeterminate on key questions, the constraint''s suppression of judicial discretion is illusory — judges still decide, just under a textualist vocabulary. This would increase theater_ratio and shift classification toward Piton (performative constraint) or Tangled Rope (genuine coordination attempt with extractive judicial implementation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_determinacy, conceptual, 'Whether the textual anchor the reading centers is genuinely constraining or interpretively plastic.').

omega_variable(
    minority_protection_gap,
    'How much substantive protection do unpopular minorities lose when constitutional meaning is limited to text + amendments, given Article V''s high threshold and majority bias?',
    'Counterfactual analysis of civil rights expansions: which relied on judicial interpretation beyond text/original understanding? Historical comparison of minority outcomes under strong vs. weak judicial review regimes.',
    'If the protection gap is large and systematic, the unpopular_minorities_seeking_judicial_protection victim declaration understates extraction — the constraint operates as a Snare for these groups. This would demand reclassification or at minimum a higher extractiveness score.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(minority_protection_gap, empirical, 'The magnitude of minority protection loss under textualist constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__positivist_reading, 1787, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_constitution_1787__positivist_reading_tr_t1787, us_constitution_1787__positivist_reading, theater_ratio, 1787, 0.1).
narrative_ontology:measurement(us_constitution_1787__positivist_reading_tr_t1803, us_constitution_1787__positivist_reading, theater_ratio, 1803, 0.12).
narrative_ontology:measurement(us_constitution_1787__positivist_reading_tr_t1868, us_constitution_1787__positivist_reading, theater_ratio, 1868, 0.15).
narrative_ontology:measurement(us_constitution_1787__positivist_reading_tr_t1913, us_constitution_1787__positivist_reading, theater_ratio, 1913, 0.18).
narrative_ontology:measurement(us_constitution_1787__positivist_reading_tr_t1937, us_constitution_1787__positivist_reading, theater_ratio, 1937, 0.25).
narrative_ontology:measurement(us_constitution_1787__positivist_reading_tr_t1954, us_constitution_1787__positivist_reading, theater_ratio, 1954, 0.32).
narrative_ontology:measurement(us_constitution_1787__positivist_reading_tr_t1973, us_constitution_1787__positivist_reading, theater_ratio, 1973, 0.4).
narrative_ontology:measurement(us_constitution_1787__positivist_reading_tr_t2026, us_constitution_1787__positivist_reading, theater_ratio, 2026, 0.35).

% Extraction over time
narrative_ontology:measurement(us_constitution_1787__positivist_reading_be_t1787, us_constitution_1787__positivist_reading, base_extractiveness, 1787, 0.15).
narrative_ontology:measurement(us_constitution_1787__positivist_reading_be_t1803, us_constitution_1787__positivist_reading, base_extractiveness, 1803, 0.18).
narrative_ontology:measurement(us_constitution_1787__positivist_reading_be_t1868, us_constitution_1787__positivist_reading, base_extractiveness, 1868, 0.22).
narrative_ontology:measurement(us_constitution_1787__positivist_reading_be_t1913, us_constitution_1787__positivist_reading, base_extractiveness, 1913, 0.24).
narrative_ontology:measurement(us_constitution_1787__positivist_reading_be_t1937, us_constitution_1787__positivist_reading, base_extractiveness, 1937, 0.35).
narrative_ontology:measurement(us_constitution_1787__positivist_reading_be_t1954, us_constitution_1787__positivist_reading, base_extractiveness, 1954, 0.42).
narrative_ontology:measurement(us_constitution_1787__positivist_reading_be_t1973, us_constitution_1787__positivist_reading, base_extractiveness, 1973, 0.55).
narrative_ontology:measurement(us_constitution_1787__positivist_reading_be_t2026, us_constitution_1787__positivist_reading, base_extractiveness, 2026, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(us_constitution_1787__positivist_reading_su_t1787, us_constitution_1787__positivist_reading, suppression_requirement, 1787, 0.2).
narrative_ontology:measurement(us_constitution_1787__positivist_reading_su_t1803, us_constitution_1787__positivist_reading, suppression_requirement, 1803, 0.25).
narrative_ontology:measurement(us_constitution_1787__positivist_reading_su_t1868, us_constitution_1787__positivist_reading, suppression_requirement, 1868, 0.3).
narrative_ontology:measurement(us_constitution_1787__positivist_reading_su_t1913, us_constitution_1787__positivist_reading, suppression_requirement, 1913, 0.35).
narrative_ontology:measurement(us_constitution_1787__positivist_reading_su_t1937, us_constitution_1787__positivist_reading, suppression_requirement, 1937, 0.45).
narrative_ontology:measurement(us_constitution_1787__positivist_reading_su_t1954, us_constitution_1787__positivist_reading, suppression_requirement, 1954, 0.5).
narrative_ontology:measurement(us_constitution_1787__positivist_reading_su_t1973, us_constitution_1787__positivist_reading, suppression_requirement, 1973, 0.55).
narrative_ontology:measurement(us_constitution_1787__positivist_reading_su_t2026, us_constitution_1787__positivist_reading, suppression_requirement, 2026, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_1787__positivist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_1787__positivist_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_1787__positivist_reading, us_constitution_1787__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_1787__positivist_reading, us_constitution_1787__living_reading).
narrative_ontology:affects_constraint(us_constitution_1787__positivist_reading, judicial_review_authority).
narrative_ontology:affects_constraint(us_constitution_1787__positivist_reading, article_v_amendment_process).

% DUAL FORMULATION NOTE:
% This constraint is one member of the us_constitution_1787 kernel family. The three readings (positivist, originalist, living) decompose the single natural-language concept 'constitutional meaning' into structurally distinct constraints with different ε values, beneficiary/victim structures, and classifications. The positivist reading's ε (0.28) is lower than the living reading's (estimated 0.55+) because it constrains judicial discretion, but higher than a genuine Mountain because it actively suppresses living constitutionalist interpretation. The originalist reading shares the judicial restraint coordination function but adds historical-intent binding (different victim structure: originalist judges vs. positivist judges).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_1787__positivist_reading, institutional, 0.15).
constraint_indexing:directionality_override(us_constitution_1787__positivist_reading, powerless, 0.95).
constraint_indexing:directionality_override(us_constitution_1787__positivist_reading, organized, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
