% ============================================================================
% CONSTRAINT STORY: constitutional_interpretive_authority__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_interpretive_authority__judicial_supremacy_reading, []).

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
 *   constraint_id: constitutional_interpretive_authority__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy in Constitutional Interpretation
 *   domain: constitutional_law/political_theory/jurisprudence
 *
 * SUMMARY:
 *   This constraint story instantiates the judicial_supremacy_reading of the
 *   constitutional_interpretive_authority kernel. The reading asserts that
 *   courts possess final interpretive authority via
 *   constitutional/fundamental rights guardianship, with legislative acts
 *   subject to judicial nullification. The constraint is the institutional
 *   arrangement of judicial review as supreme constitutional interpretation.
 *   The judiciary enters the beneficiary set for interpretive authority; the
 *   legislature is subordinated; coercion (nullification) is legitimated via
 *   rights-compliance rather than democratic will. This reading competes with
 *   parliamentary_supremacy_reading (legislature has final authority) and
 *   coordinate_construction_reading (no single branch has final authority).
 *   The claim/metric independence is observed: the reading claims to be a
 *   coordination mechanism (rope-like rights protection) while the metrics
 *   reveal substantial extraction (judiciary gains authority, legislature
 *   loses autonomy) requiring active enforcement — the engine will compute
 *   the structural type from the data.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_interpretive_authority__judicial_supremacy_reading, 0.62).
domain_priors:suppression_score(constitutional_interpretive_authority__judicial_supremacy_reading, 0.58).
domain_priors:theater_ratio(constitutional_interpretive_authority__judicial_supremacy_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_interpretive_authority__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_interpretive_authority__judicial_supremacy_reading, "Judicial Supremacy in Constitutional Interpretation").
narrative_ontology:topic_domain(constitutional_interpretive_authority__judicial_supremacy_reading, "constitutional_law/political_theory/jurisprudence").

domain_priors:requires_active_enforcement(constitutional_interpretive_authority__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_interpretive_authority__judicial_supremacy_reading, 'da9825f8-dbbc-40bf-9af9-47f3917ac8db').
narrative_ontology:cs_kernel_codification('da9825f8-dbbc-40bf-9af9-47f3917ac8db', formalized).
narrative_ontology:cs_authority_grounding('da9825f8-dbbc-40bf-9af9-47f3917ac8db', lineage).
narrative_ontology:cs_interpretation_layer_present('da9825f8-dbbc-40bf-9af9-47f3917ac8db').
narrative_ontology:cs_reading_relation('da9825f8-dbbc-40bf-9af9-47f3917ac8db', constitutional_interpretive_authority__parliamentary_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('da9825f8-dbbc-40bf-9af9-47f3917ac8db', constitutional_interpretive_authority__coordinate_construction_reading, coexists_with).
narrative_ontology:cs_axiom('da9825f8-dbbc-40bf-9af9-47f3917ac8db', foundational, judicial_final_interpretive_authority).
narrative_ontology:cs_axiom_status(judicial_final_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('da9825f8-dbbc-40bf-9af9-47f3917ac8db', judicial_final_interpretive_authority, conventional).
narrative_ontology:cs_axiom('da9825f8-dbbc-40bf-9af9-47f3917ac8db', foundational, rights_guardianship_justifies_nullification).
narrative_ontology:cs_axiom_status(rights_guardianship_justifies_nullification, holdable).
narrative_ontology:cs_axiom_grounding('da9825f8-dbbc-40bf-9af9-47f3917ac8db', rights_guardianship_justifies_nullification, deontological).
narrative_ontology:cs_reference_frame('da9825f8-dbbc-40bf-9af9-47f3917ac8db', marbury_v_madison_framework).
narrative_ontology:cs_drift_state('da9825f8-dbbc-40bf-9af9-47f3917ac8db', contemporary_politicized_court_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('da9825f8-dbbc-40bf-9af9-47f3917ac8db', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__judicial_supremacy_reading, judiciary).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__judicial_supremacy_reading, rights_claimants).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, legislature).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, democratic_majorities).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__judicial_supremacy_reading, judicial_review_legitimacy).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__judicial_supremacy_reading, countermajoritarian_difficulty_resolution).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercises final interpretive authority over constitutional meaning through judicial review. Collects the institutional capital of being the ultimate rights guardian. Justifies nullification of legislative acts as necessary to protect fundamental rights. Controls the development of constitutional doctrine through precedent. Can shape the scope of its own authority over time.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, judiciary, agenda_setter,
    institutional, generational, arbitrage, national).

% Gain enforceable protection for fundamental rights against legislative majorities. Access courts as a venue where rights claims can override democratic outcomes. Their protection depends on judicial willingness to recognize and enforce their claimed rights. Exit means abandoning judicial protection for political mobilization, which is structurally disadvantaged for minority rights.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, rights_claimants, beneficiary,
    organized, biographical, constrained, national).

% Loses final authority over constitutional meaning; legislative acts subject to nullification. Must anticipate judicial reaction when legislating, creating a chilling effect on policy innovation. Can respond through constitutional amendment (prohibitively difficult), court-curbing legislation, or strategic non-compliance, but each carries high institutional cost. The constraint extracts legislative autonomy and transfers it to the judiciary.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, legislature, payer,
    institutional, biographical, constrained, national).

% See their policy preferences nullified by unelected judges invoking abstract rights. The constraint legitimates this through rights-compliance rather than democratic will. Exit means constitutional amendment (near-impossible) or court-packing (institutionally destructive). Bear the cost of having their democratic choices overridden without consent.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, democratic_majorities, payer,
    organized, biographical, constrained, national).

% Argue that constitutional meaning emerges from inter-branch dialogue and political contestation, not judicial monopoly. Are structurally excluded from the operating framework because judicial supremacy treats their view as a theoretical error rather than a competing institutional practice. Would object to the constraint's claim of exclusive judicial authority if included in the constitutional conversation.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, coordinate_construction_advocates, excluded,
    moderate, generational, mobile, national).

% Analyze the constraint's operation, legitimacy, and effects from outside the institutional power structure. Produce the theoretical frameworks that legitimate or critique judicial supremacy. Do not collect rents from the constraint nor bear its direct costs, but shape the intellectual environment in which it operates.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves constitutional disputes authoritatively and protects fundamental rights against legislative majorities by vesting final interpretive authority in courts.
% TRANSFER_FUNCTION: Moves final interpretive authority from legislature to judiciary; moves policy outcomes from democratic majorities to rights-claimants via judicial nullification of legislative acts; moves institutional legitimacy from democratic will to rights-compliance.
% ABSENT_VOICES: Democratic majorities whose enacted preferences are nullified; coordinate construction advocates who view inter-branch dialogue as the proper method of constitutional development; originalist critics who reject judicial supremacy as ahistorical. The excluded coordinate_construction_advocates stakeholder represents the institutionalized absence of the inter-branch dialogue perspective.
% DISAPPEARANCE_RATIONALE: If judicial supremacy vanished overnight, legislatures would reclaim final interpretive authority, constitutional disputes would shift to political resolution, rights protection would depend on legislative majorities rather than judicial enforcement, and the entire institutional ecology of constitutional litigation would collapse. The constraint organizes the constitutional order.
% FOUNDING_PROBLEM: Preventing legislative tyranny and protecting minority rights against majority rule in a system where the legislature is the most powerful branch.
% FOUNDING_PROBLEM_CORROBORATION: The Federalist Papers (Hamilton, Federalist 78) and the Civil Rights Movement attest to the rights-protection function. Democratic theorists (Waldron, Bellamy) and legislative supremacy advocates (UK parliamentary tradition, Canadian notwithstanding clause proponents) attest the founding problem is either solved by democratic culture or the judicial solution is excessive. No consensus exists outside the benefiting parties.
narrative_ontology:disappearance_verdict(constitutional_interpretive_authority__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_interpretive_authority__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_interpretive_authority__judicial_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_interpretive_authority__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_interpretive_authority__judicial_supremacy_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_interpretive_authority__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_interpretive_authority__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_interpretive_authority__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the transfer of final interpretive authority from legislature to judiciary — a substantial institutional asset. Suppression (0.58) captures the constraint on legislative autonomy and democratic majorities, enforced through the credible threat of nullification. Theater ratio (0.32) indicates genuine rights-protection function mixed with performative doctrinal reasoning that masks policy preferences. Accessibility collapse (0.45) shows alternatives (legislative override, amendment, inter-branch dialogue) exist but are institutionally difficult. Resistance (0.55) reflects ongoing court-curbing movements, academic critique, and political pushback. The temporal series shows extraction rising through the Lochner era, dipping during the New Deal settlement, rising again through the Warren Court, then stabilizing at a high plateau.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's seat, the constraint is genuine coordination: it resolves disputes authoritatively and protects rights. From the legislature's seat, it is extraction: final authority removed, policy space constrained. From rights_claimants' seat, it is essential protection. From democratic_majorities' seat, it is illegitimate override. The engine computes per-seat classifications from the structural asymmetry — the authored claim (tangled_rope) does not adjudicate the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Judiciary is the structural beneficiary (d near 0.0): collects interpretive authority, controls doctrine development, has arbitrage-grade exit (life tenure, institutional insulation). Rights_claimants are beneficiaries (d ~0.2): gain rights protection but depend on judicial willingness. Legislature is a target (d ~0.8): pays through lost autonomy, constrained exit (amendment nearly impossible). Democratic_majorities are targets (d ~0.75): lose policy control, constrained exit. Coordinate_construction_advocates are excluded (d not computed): their perspective is structurally absent from the operating framework. Legal_scholars are analytical observers (d=0.5). The engine derives these from beneficiary/victim declarations and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing legislative tyranny) is contested as live/dead. If dead (democratic culture now self-polices; rights protected politically), the constraint persists as mandatrophy — extraction without founding justification. If live, it remains tangled_rope. If the coordination function has atrophied but extraction persists, it drifts toward piton. The theater ratio rise in mid-late 20th century suggests performative maintenance of a contested authority claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Does the constitutional text and structure genuinely commit to judicial supremacy, or is this reading one of several defensible constructions of an ambiguous kernel?',
    'Historical analysis of founding-era understandings (Marbury, Federalist 78, ratification debates) combined with structural analysis of whether the kernel''s ambiguity is resolvable or constitutive.',
    'If the kernel genuinely commits to judicial supremacy, the reading''s authority_grounding is lineage with a strong reference frame. If the kernel is ambiguous, the reading''s authority_grounding shifts toward extraction (institutional self-aggrandizement) and the coordinate_construction_reading gains structural parity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the kernel''s ambiguity is a bug (resolvable by history) or a feature (constitutive of constitutional practice).').

omega_variable(
    coordination_extraction_boundary,
    'How much of judicial review''s operation is genuine rights-protection coordination versus judicial policy-making extraction?',
    'Empirical analysis of nullification rates by issue area, correlation with textual/precedent clarity, and comparison with coordinate-construction systems (UK, Canada, NZ) that protect rights without judicial supremacy.',
    'If coordination dominates, the constraint approaches rope. If extraction dominates, it approaches snare. The tangled_rope classification depends on both being substantially present.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'The coordination/extraction boundary within judicial review''s actual operation.').

omega_variable(
    democratic_legitimacy_deficit,
    'Can rights-compliance legitimate coercion (nullification) to the same degree as democratic will, or does the legitimacy deficit require continuous performative maintenance?',
    'Longitudinal study of public confidence in courts, compliance rates with unpopular decisions, and institutional responses to legitimacy crises (court-packing threats, jurisdiction stripping).',
    'If legitimacy deficit is structural and growing, theater_ratio will rise and the constraint drifts toward piton. If rights-compliance provides stable legitimacy, the constraint remains a stable tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_legitimacy_deficit, preference, 'Whether the rights-compliance legitimation strategy is sustainable or requires increasing performative investment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_interpretive_authority__judicial_supremacy_reading, 1803, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cja_jsr_tr_t1803, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1803, 0.1).
narrative_ontology:measurement(cja_jsr_tr_t1857, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1857, 0.2).
narrative_ontology:measurement(cja_jsr_tr_t1905, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1905, 0.35).
narrative_ontology:measurement(cja_jsr_tr_t1937, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1937, 0.25).
narrative_ontology:measurement(cja_jsr_tr_t1954, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1954, 0.3).
narrative_ontology:measurement(cja_jsr_tr_t1973, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1973, 0.4).
narrative_ontology:measurement(cja_jsr_tr_t2000, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(cja_jsr_tr_t2024, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 2024, 0.32).

% Extraction over time
narrative_ontology:measurement(cja_jsr_be_t1803, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1803, 0.15).
narrative_ontology:measurement(cja_jsr_be_t1857, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1857, 0.25).
narrative_ontology:measurement(cja_jsr_be_t1905, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1905, 0.45).
narrative_ontology:measurement(cja_jsr_be_t1937, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1937, 0.35).
narrative_ontology:measurement(cja_jsr_be_t1954, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1954, 0.55).
narrative_ontology:measurement(cja_jsr_be_t1973, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1973, 0.65).
narrative_ontology:measurement(cja_jsr_be_t2000, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(cja_jsr_be_t2024, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 2024, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(cja_jsr_su_t1803, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1803, 0.2).
narrative_ontology:measurement(cja_jsr_su_t1857, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1857, 0.4).
narrative_ontology:measurement(cja_jsr_su_t1905, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1905, 0.55).
narrative_ontology:measurement(cja_jsr_su_t1937, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1937, 0.3).
narrative_ontology:measurement(cja_jsr_su_t1954, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1954, 0.6).
narrative_ontology:measurement(cja_jsr_su_t1973, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1973, 0.65).
narrative_ontology:measurement(cja_jsr_su_t2000, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(cja_jsr_su_t2024, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 2024, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_interpretive_authority__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_interpretive_authority__judicial_supremacy_reading, 0.1).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_interpretive_authority__parliamentary_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_interpretive_authority__coordinate_construction_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the 'constitutional interpretive authority' kernel into three readings with distinct ε values and beneficiary/victim structures. Judicial supremacy reading: ε=0.62, judiciary benefits, legislature pays. Parliamentary supremacy reading: ε≈0.15, legislature benefits, rights_claimants pay (weak protection). Coordinate construction reading: ε≈0.35, distributed benefits/costs, no final authority. The readings are linked via affects_constraints because each claims the kernel and the upstream reading (judicial supremacy) is often cited as evidence against the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_interpretive_authority__judicial_supremacy_reading, institutional, 0.15).
constraint_indexing:directionality_override(constitutional_interpretive_authority__judicial_supremacy_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
