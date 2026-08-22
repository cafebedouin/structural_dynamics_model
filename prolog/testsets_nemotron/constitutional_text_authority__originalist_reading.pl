% ============================================================================
% CONSTRAINT STORY: constitutional_text_authority__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text_authority__originalist_reading, []).

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
 *   constraint_id: constitutional_text_authority__originalist_reading
 *   human_readable: Originalist Constitutional Interpretation Constraint
 *   domain: constitutional_law/interpretive_jurisprudence
 *
 * SUMMARY:
 *   This constraint story captures the originalist reading of constitutional
 *   authority: the claim that constitutional meaning was fixed at
 *   ratification and derives its authority from the historical public
 *   understanding of the enacting generation. The constraint operates as a
 *   structural limitation on judicial discretion — judges must constrain
 *   their reasoning to historical evidence of original public meaning, and
 *   post-ratification social change cannot alter constitutional meaning
 *   without Article V amendment. The constraint has grown in institutional
 *   force since the 1970s (marked by the rise of the conservative legal
 *   movement and the Federalist Society), with increasing extraction from
 *   progressive legal actors and unrepresented groups who lose access to
 *   evolving constitutional protections. The claimed_type is tangled_rope:
 *   there is a genuine coordination function (providing a fixed, publicly
 *   accessible standard that limits judicial discretion and stabilizes
 *   constitutional meaning across time) combined with asymmetric extraction
 *   (the constraint's institutional beneficiaries — judicial conservatives,
 *   the originalist legal academy, the conservative legal movement — gain
 *   concentrated interpretive authority and institutional power, while the
 *   costs fall on living constitutionalist judges, progressive scholars, and
 *   marginalized groups whose claims require evolutionary interpretation).
 *   Active enforcement is required: the constraint is sustained through
 *   judicial appointments, law school curricula, institutional networks, and
 *   the threat of professional marginalization for deviations.
 *
 * KEY AGENTS:
 *   - judicial_conservatives: Primary beneficiary (institutional/identity_locked) — gain concentrated interpretive authority and career advancement through adherence
 *   - textualist_scholars: Primary beneficiary (organized/constrained) — build academic careers and institutional prestige on the methodology
 *   - originalist_legal_academy: Secondary beneficiary (organized/constrained) — controls journals, clerkships, and hiring pipelines
 *   - conservative_legal_movement: Agenda setter (institutional/arbitrage) — sets the institutional agenda, funds the infrastructure, controls judicial selection pipelines
 *   - living_constitutionalist_judges: Primary victim (powerful/constrained) — constrained from using evolutionary methods, face professional costs for non-adherence
 *   - progressive_legal_scholars: Primary victim (organized/constrained) — excluded from mainstream interpretive debates, marginalized in hiring and publication
 *   - unrepresented_marginalized_groups: Primary victim (powerless/trapped) — lose access to constitutional protections that require evolutionary reading (privacy, dignity, equality expansions)
 *   - future_generations: Victim (powerless/trapped) — bound by fixed meanings they had no role in creating, cannot adapt constitution to new conditions without Article V
 *   - originalist_methodology_itself: Analytical observer — sees full structure of coordination and extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text_authority__originalist_reading, 0.45).
domain_priors:suppression_score(constitutional_text_authority__originalist_reading, 0.68).
domain_priors:theater_ratio(constitutional_text_authority__originalist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text_authority__originalist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text_authority__originalist_reading, "Originalist Constitutional Interpretation Constraint").
narrative_ontology:topic_domain(constitutional_text_authority__originalist_reading, "constitutional_law/interpretive_jurisprudence").

domain_priors:requires_active_enforcement(constitutional_text_authority__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text_authority__originalist_reading, 'bcdefc80-59ef-493e-86b5-039d7823a615').
narrative_ontology:cs_kernel_codification('bcdefc80-59ef-493e-86b5-039d7823a615', fixed_text).
narrative_ontology:cs_authority_grounding('bcdefc80-59ef-493e-86b5-039d7823a615', lineage).
narrative_ontology:cs_interpretation_layer_present('bcdefc80-59ef-493e-86b5-039d7823a615').
narrative_ontology:cs_reading_relation('bcdefc80-59ef-493e-86b5-039d7823a615', constitutional_text_authority__living_constitutionalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('bcdefc80-59ef-493e-86b5-039d7823a615', constitutional_text_authority__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('bcdefc80-59ef-493e-86b5-039d7823a615', foundational, original_public_meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(original_public_meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('bcdefc80-59ef-493e-86b5-039d7823a615', original_public_meaning_fixed_at_ratification, deontological).
narrative_ontology:cs_axiom('bcdefc80-59ef-493e-86b5-039d7823a615', foundational, article_v_exclusive_amendment_path).
narrative_ontology:cs_axiom_status(article_v_exclusive_amendment_path, holdable).
narrative_ontology:cs_axiom_grounding('bcdefc80-59ef-493e-86b5-039d7823a615', article_v_exclusive_amendment_path, conventional).
narrative_ontology:cs_reference_frame('bcdefc80-59ef-493e-86b5-039d7823a615', founding_generation_public_understanding).
narrative_ontology:cs_drift_state('bcdefc80-59ef-493e-86b5-039d7823a615', contemporary_originalist_dominance, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('bcdefc80-59ef-493e-86b5-039d7823a615', '').
narrative_ontology:cs_kernel_id(constitutional_text_authority__originalist_reading, constitutional_text_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, judicial_conservatives).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, textualist_scholars).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, originalist_legal_academy).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, conservative_legal_movement).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, living_constitutionalist_judges).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, progressive_legal_scholars).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, unrepresented_marginalized_groups).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, future_generations).
narrative_ontology:constraint_vindicates(constitutional_text_authority__originalist_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(constitutional_text_authority__originalist_reading, separation_of_powers_doctorine).
narrative_ontology:constraint_vindicates(constitutional_text_authority__originalist_reading, judicial_restraint_principle).
narrative_ontology:constraint_vindicates(constitutional_text_authority__originalist_reading, democratic_legitimacy_of_fixed_meaning).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Federal judges appointed through conservative legal movement pipelines. Their professional identity and career advancement depend on adherence to originalist methodology. They gain concentrated interpretive authority and institutional prestige from the constraint. Exit would mean professional suicide — the methodology constitutes their judicial identity.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, judicial_conservatives, beneficiary,
    institutional, biographical, identity_locked, national).

% Law professors and legal scholars whose academic careers are built on originalist/textualist methodology. They control key journals, clerkship pipelines, and hiring networks. The constraint provides their professional coherence and market value. Exit is constrained — they could shift methodology but would lose their distinctive institutional position.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, textualist_scholars, beneficiary,
    organized, biographical, constrained, national).

% The network of law schools, centers, journals, and conferences that produce and credential originalist scholarship. They set the intellectual agenda, gatekeep entry to the field, and reproduce the methodology across generations. They benefit from the constraint's institutionalization but also administer it.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, originalist_legal_academy, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text_authority__originalist_reading, originalist_legal_academy, agenda_setter).

% The institutional infrastructure (Federalist Society, judicial selection networks, funding apparatus) that originated and sustains the constraint. They set the agenda, control judicial appointments, and extract political capital from the constraint's operation. They have arbitrage-grade exit — they could pivot to other constitutional theories if strategically advantageous.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, conservative_legal_movement, agenda_setter,
    institutional, generational, arbitrage, national).

% Judges who employ evolutionary or living constitutionalist methods. They face professional costs: exclusion from conservative judicial networks, difficulty getting appointed/elevated, marginalization in the legal academy. Their interpretive flexibility is extracted by the constraint. Exit is constrained — they could adopt originalist reasoning but would lose their judicial philosophy.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, living_constitutionalist_judges, payer,
    powerful, biographical, constrained, national).

% Legal academics whose work requires non-originalist methods. They face publishing barriers in top journals, hiring disadvantages, and exclusion from clerkship pipelines. Their intellectual freedom is the extraction. Exit is constrained — they could write originalist scholarship but would abandon their research agenda.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, progressive_legal_scholars, payer,
    organized, biographical, constrained, national).

% Groups not represented at ratification (women, racial minorities, LGBTQ+ people, etc.) whose constitutional claims require evolutionary interpretation (privacy, dignity, equality expansions). The constraint's fixed meaning denies them protections that living constitutionalism would recognize. They are trapped — they cannot exit the constitutional system and have no voice in its interpretation.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, unrepresented_marginalized_groups, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text_authority__originalist_reading, unrepresented_marginalized_groups, excluded).

% People not yet born who will be bound by constitutional meanings fixed centuries ago. They cannot participate in ratification, cannot amend the Constitution easily (Article V supermajority), and must live under interpretations they had no hand in creating. The constraint extracts their adaptive capacity.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, future_generations, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_secondary_role(constitutional_text_authority__originalist_reading, future_generations, excluded).

% The analytical seat that sees the full structure: the coordination function (fixed meaning limiting discretion) and the extraction function (institutional entrenchment of a specific interpretive coalition). This seat does not collect or pay — it classifies.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, originalist_methodology_itself, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text_authority__originalist_reading, conservative_legal_movement).
narrative_ontology:fixing_cost_class(constitutional_text_authority__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a fixed, publicly accessible standard for constitutional meaning that limits judicial discretion, stabilizes the law across time, and anchors constitutional authority in the democratic act of ratification rather than judicial will.
% TRANSFER_FUNCTION: Moves interpretive authority and doctrinal flexibility from living constitutionalist judges, progressive scholars, and unrepresented groups to the conservative legal movement, originalist academy, and judicial conservatives — as the price of accessing the constitutional system's legitimacy.
% ABSENT_VOICES: The unrepresented groups at ratification (women, enslaved people, indigenous nations, propertyless men) and future generations are structurally excluded from the interpretive conversation. They would object to being bound by meanings they had no voice in creating, but the constraint's design (fixed meaning + Article V gate) ensures they cannot participate.
% DISAPPEARANCE_RATIONALE: If the originalist constraint vanished overnight, judicial discretion would expand dramatically, evolutionary constitutional protections would re-emerge, the conservative legal movement would lose its primary interpretive infrastructure, and the federal judiciary's ideological composition would become the central constitutional question. The legal profession, the courts, and constitutional doctrine would reorganize around a new interpretive equilibrium.
% FOUNDING_PROBLEM: The perceived problem of judicial activism by the Warren and Burger Courts (1950s-1970s) — courts creating new constitutional rights without textual basis, overriding democratic processes, and substituting judicial preferences for constitutional meaning.
% FOUNDING_PROBLEM_CORROBORATION: The conservative legal movement and originalist scholars attest the founding problem remains live (courts still exceed their authority). Living constitutionalist scholars and progressive jurists attest the founding problem is substantially solved or misdiagnosed — the Warren Court protected unrepresented groups that the original Constitution excluded. Independent legal historians and political scientists outside the benefiting parties generally corroborate that the 'judicial activism' framing was politically contested from the start and that the originalist movement was built as much to entrench conservative outcomes as to solve a neutral coordination problem.
narrative_ontology:disappearance_verdict(constitutional_text_authority__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text_authority__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text_authority__originalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(constitutional_text_authority__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text_authority__originalist_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text_authority__originalist_reading_tests).
:- end_tests(constitutional_text_authority__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate but rising: the constraint extracts interpretive freedom and doctrinal flexibility from non-originalist actors, concentrating interpretive authority in the originalist network. The extraction has increased over the interval as the conservative legal movement captured the federal judiciary. Suppression (0.68) is substantial: the constraint actively suppresses alternative interpretive methodologies through professional gatekeeping (clerkships, appointments, publications, tenure). Theater_ratio (0.22) is low-moderate: the historical-evidence gate is a real methodological constraint, not purely performative, though its determinacy is contested (omega: historical_evidence_accessibility). Accessibility_collapse (0.78) is high: once the originalist frame is accepted, alternatives (living constitutionalism, pragmatism) appear as illegitimate judicial activism. Resistance (0.42) is moderate: living constitutionalist scholars and judges continue to mount intellectual and institutional resistance, but the constraint's institutional entrenchment has grown.
 *
 * PERSPECTIVAL GAP:
 *   From the judicial conservative/originalist seat (agenda_setter + beneficiary), the constraint is genuine coordination: it solves the problem of judicial discretion run amok, provides democratic legitimacy through fixed meaning, and stabilizes the law. From the living constitutionalist/progressive seat (payer/victim), the same structure operates as extraction: it locks in historical exclusions, denies constitutional protection to groups unrepresented at ratification, and insulates the interpretive preferences of a narrow demographic from democratic updating. The engine computes this divergence from the declared beneficiaries/victims and their exit_options — the originalist actors have identity_locked exit (professional identity fused with methodology) while the victims are trapped or constrained.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: judicial_conservatives, textualist_scholars, originalist_legal_academy, conservative_legal_movement. These agents collect concentrated interpretive authority, career capital, and institutional control from the constraint's operation. Their exit_options are identity_locked or constrained — their professional identity is constituted through originalist methodology. Victims declared: living_constitutionalist_judges, progressive_legal_scholars, unrepresented_marginalized_groups, future_generations. These agents bear the costs: lost interpretive flexibility, professional marginalization, denial of evolving constitutional protections. Their exit_options are trapped (marginalized groups, future generations) or constrained (progressive judges/scholars within the profession). The conservative_legal_movement is the agenda_setter: it administers the constraint through judicial selection, funding, and institutional infrastructure. Vindicated propositions are doctrines the constraint's operation supports but which collect no rents themselves.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — constraining judicial discretion and anchoring constitutional meaning in democratic ratification — remains live (contested status). The originalist reading was built to solve the perceived problem of Warren/Burger Court judicial activism. That problem persists in the eyes of the constraint's beneficiaries. However, the constraint has accumulated extraction: it now serves as an institutional gatekeeping mechanism that benefits a specific legal-political coalition. The mandatrophy is unresolved: the coordination function (constraint on discretion) and the extraction function (entrenchment of conservative interpretive monopoly) are structurally fused. The constraint would not persist in its current form without the extraction — the conservative legal movement maintains it because it delivers power, not merely because it solves the coordination problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of the constitutional_text_authority kernel, and what would sibling readings change structurally?',
    'Comparative analysis of the originalist, living constitutionalist, and positivist readings as separate constraint stories with their own ε, beneficiaries, victims, and claimed types. The kernel_id and reading_id structure this constraint family.',
    'If the kernel has multiple readings, each must be authored as a separate constraint story with its own structural data. Linking via network.affects_constraints enables the engine to analyze the family as a constraint system. The originalist reading''s ε = 0.45 reflects its own assessment of the standing arrangement''s extraction; the living constitutionalist reading would author a different ε for the same referent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is the originalist_reading of the constitutional_text_authority kernel. Siblings: living_constitutionalist_reading, positivist_reading.').

omega_variable(
    historical_evidence_accessibility,
    'How accessible and determinate is the historical public understanding that this reading requires as its gating mechanism?',
    'Empirical study of historical linguistic corpora, ratification-era debates, and founding-era legal materials to measure convergence/divergence in original public meaning across provisions and time.',
    'If historical meaning is highly indeterminate or inaccessible for key provisions, the constraint''s coordination function degrades and its extraction becomes more purely performative (higher theater_ratio). If determinate, the constraint operates as genuine coordination with asymmetric extraction from those who would depart from original meaning.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_evidence_accessibility, empirical, 'Whether the historical evidence gate is a real constraint on outcomes or a rhetorical cover.').

omega_variable(
    unenumerated_rights_closure,
    'Does the originalist reading''s structural exclusion of unenumerated rights constitute extraction from unrepresented groups, or is it a necessary consequence of the coordination function?',
    'Track doctrinal outcomes for marginalized groups under originalist vs. non-originalist regimes; measure whether the constraint''s beneficiaries (judicial conservatives, originalist academy) gain disproportionate institutional power from the exclusion.',
    'If exclusion systematically benefits the declared beneficiaries while extracting from unrepresented groups, the tangled_rope classification is confirmed. If exclusion is the price of the coordination function with no asymmetric benefit, the constraint may be a rope with high accessibility_collapse.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(unenumerated_rights_closure, preference, 'Whether the unenumerated-rights closure is structural extraction or coordination cost.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text_authority__originalist_reading, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t1970, constitutional_text_authority__originalist_reading, theater_ratio, 1970, 0.08).
narrative_ontology:measurement(cons_tr_t1980, constitutional_text_authority__originalist_reading, theater_ratio, 1980, 0.11).
narrative_ontology:measurement(cons_tr_t1990, constitutional_text_authority__originalist_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(cons_tr_t2000, constitutional_text_authority__originalist_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(cons_tr_t2010, constitutional_text_authority__originalist_reading, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(cons_tr_t2020, constitutional_text_authority__originalist_reading, theater_ratio, 2020, 0.22).

% Extraction over time
narrative_ontology:measurement(cons_be_t1970, constitutional_text_authority__originalist_reading, base_extractiveness, 1970, 0.15).
narrative_ontology:measurement(cons_be_t1980, constitutional_text_authority__originalist_reading, base_extractiveness, 1980, 0.22).
narrative_ontology:measurement(cons_be_t1990, constitutional_text_authority__originalist_reading, base_extractiveness, 1990, 0.31).
narrative_ontology:measurement(cons_be_t2000, constitutional_text_authority__originalist_reading, base_extractiveness, 2000, 0.38).
narrative_ontology:measurement(cons_be_t2010, constitutional_text_authority__originalist_reading, base_extractiveness, 2010, 0.42).
narrative_ontology:measurement(cons_be_t2020, constitutional_text_authority__originalist_reading, base_extractiveness, 2020, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t1970, constitutional_text_authority__originalist_reading, suppression_requirement, 1970, 0.35).
narrative_ontology:measurement(cons_su_t1980, constitutional_text_authority__originalist_reading, suppression_requirement, 1980, 0.42).
narrative_ontology:measurement(cons_su_t1990, constitutional_text_authority__originalist_reading, suppression_requirement, 1990, 0.51).
narrative_ontology:measurement(cons_su_t2000, constitutional_text_authority__originalist_reading, suppression_requirement, 2000, 0.58).
narrative_ontology:measurement(cons_su_t2010, constitutional_text_authority__originalist_reading, suppression_requirement, 2010, 0.63).
narrative_ontology:measurement(cons_su_t2020, constitutional_text_authority__originalist_reading, suppression_requirement, 2020, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text_authority__originalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(constitutional_text_authority__originalist_reading, 0.08).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, constitutional_text_authority__living_constitutionalist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, constitutional_text_authority__positivist_reading).

% DUAL FORMULATION NOTE:
% This constraint and its siblings form the constitutional_text_authority constraint family. The originalist reading (this story) and living constitutionalist reading are in tension: both claim the Constitution's authority but instantiate different constraints with different ε, beneficiaries, and victims. The positivist reading occupies a distinct position: it treats the kernel as formal validity without moral content. All three stories link via affects_constraints to enable family-level analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_text_authority__originalist_reading, powerful, 0.75).
constraint_indexing:directionality_override(constitutional_text_authority__originalist_reading, organized, 0.35).
constraint_indexing:directionality_override(constitutional_text_authority__originalist_reading, institutional, 0.15).
constraint_indexing:directionality_override(constitutional_text_authority__originalist_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
