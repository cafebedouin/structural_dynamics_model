% ============================================================================
% CONSTRAINT STORY: common_law_precedent_corpus__strict_stare_decisis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_law_precedent_corpus__strict_stare_decisis, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: common_law_precedent_corpus__strict_stare_decisis
 *   human_readable: Strict Stare Decisis: Precedent as Backward Binding Constraint
 *   domain: legal/institutional
 *
 * SUMMARY:
 *   Strict stare decisis is one reading of the common-law precedent corpus:
 *   precedent binds as a backward constraint, and departure requires
 *   extraordinary justification (demonstrated grave error, fundamentally
 *   changed circumstances, or systemic incoherence). Under this reading,
 *   judges are bound by prior holdings; litigants seeking norm change face a
 *   high burden; the doctrine accumulates and constrains. This reading
 *   competes with an evolutionary reading (precedent provides an adaptive
 *   framework within which contemporary interpretation is permitted) and a
 *   pluralist reading (precedent weight varies by domain, balancing stability
 *   and adaptation case-by-case). This story instantiates the strict stare
 *   decisis reading only, with its core structural claim: precedent rigidity,
 *   rare overruling, narrow pathways for norm challenge.
 *
 * KEY AGENTS:
 *   - Established doctrine holders (institutions, practitioners, entrenched interests benefiting from locked-in stable law)
 *   - Norm challengers (litigants, advocates seeking to overturn or reinterpret precedent)
 *   - Judiciary (institutionally bound by precedent, constrained from departing without extraordinary justification)
 *   - Legal practitioners (benefit from stable, predictable doctrine)
 *   - Powerless litigants oppressed by entrenched precedent (trapped, unable to mount extraordinary-justification cases)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__strict_stare_decisis, 0.68).
domain_priors:suppression_score(common_law_precedent_corpus__strict_stare_decisis, 0.72).
domain_priors:theater_ratio(common_law_precedent_corpus__strict_stare_decisis, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, extractiveness, 0.68).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__strict_stare_decisis, tangled_rope).
narrative_ontology:human_readable(common_law_precedent_corpus__strict_stare_decisis, "Strict Stare Decisis: Precedent as Backward Binding Constraint").
narrative_ontology:topic_domain(common_law_precedent_corpus__strict_stare_decisis, "legal/institutional").

domain_priors:requires_active_enforcement(common_law_precedent_corpus__strict_stare_decisis).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__strict_stare_decisis, '7d6bfab6-968d-4169-bfd6-af80378fde44').
narrative_ontology:cs_kernel_codification('7d6bfab6-968d-4169-bfd6-af80378fde44', fixed_text).
narrative_ontology:cs_authority_grounding('7d6bfab6-968d-4169-bfd6-af80378fde44', lineage).
narrative_ontology:cs_interpretation_layer_present('7d6bfab6-968d-4169-bfd6-af80378fde44').
narrative_ontology:cs_reading_relation('7d6bfab6-968d-4169-bfd6-af80378fde44', common_law_precedent_corpus__evolutionary_framework, influences).
narrative_ontology:cs_reading_relation('7d6bfab6-968d-4169-bfd6-af80378fde44', common_law_precedent_corpus__pluralist_balancing, influences).
narrative_ontology:cs_axiom('7d6bfab6-968d-4169-bfd6-af80378fde44', foundational, precedent_binding_rigidity).
narrative_ontology:cs_axiom_status(precedent_binding_rigidity, holdable).
narrative_ontology:cs_axiom_grounding('7d6bfab6-968d-4169-bfd6-af80378fde44', precedent_binding_rigidity, instrumental).
narrative_ontology:cs_axiom('7d6bfab6-968d-4169-bfd6-af80378fde44', foundational, extraordinary_justification_requirement).
narrative_ontology:cs_axiom_status(extraordinary_justification_requirement, holdable).
narrative_ontology:cs_axiom_grounding('7d6bfab6-968d-4169-bfd6-af80378fde44', extraordinary_justification_requirement, conventional).
narrative_ontology:cs_reference_frame('7d6bfab6-968d-4169-bfd6-af80378fde44', common_law_doctrine_cumulative_binding).
narrative_ontology:cs_drift_state('7d6bfab6-968d-4169-bfd6-af80378fde44', contemporary_jurisprudential_pressure, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7d6bfab6-968d-4169-bfd6-af80378fde44', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__strict_stare_decisis, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__strict_stare_decisis, established_doctrine_holders).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__strict_stare_decisis, institutional_continuity).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, norm_challengers).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, overruled_litigants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__strict_stare_decisis, legal_practitioners).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, oppressed_by_precedent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Those whose legal interests, institutions, or interpretive frameworks are entrenched in the precedent corpus. Includes entrenched practitioners, institutions built on stable doctrinal assumptions, and parties whose contracts and reliance interests depend on predictable application of standing law. Benefit from the high cost of overruling: their arrangements are locked in, and challengers must meet an extraordinary burden to displace them.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, established_doctrine_holders, beneficiary,
    institutional, generational, analytical, national).

% Litigants and advocates who believe a precedent is wrongly decided or no longer fits contemporary norms, and seek to have it overruled or reinterpreted. Face the extraordinary-justification requirement: must demonstrate not merely error, but sufficiently grave error or changed circumstances to justify the institutional cost of reversal. Their pathways for norm challenge are narrow; they bear the litigation cost while the burden of proof sits on them.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, norm_challengers, payer,
    moderate, biographical, constrained, national).

% The courts that interpret, apply, and occasionally overrule precedent. Under strict stare decisis, they are bound by prior holdings and constrained from departing without extraordinary justification. Their discretion to alter doctrine is actively limited by the binding force of the precedent corpus. They administer the rule but are also its subject.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Lawyers and legal advisors who depend on stable, predictable doctrine to counsel clients and structure transactions. Benefit from high precedent stability: their advice is more reliable, their predictive models more accurate, their client risk assessments more grounded. Can adapt to doctrinal shifts but prefer stability; have some mobility to shift practice areas or jurisdictions if doctrine radically changes.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, legal_practitioners, beneficiary,
    powerful, biographical, mobile, national).

% Parliament or legislature that could amend doctrine through statute but is structurally excluded from the common-law precedent-binding mechanism. Could overturn precedent by legislation but not by ordinary precedent overruling; their exclusion means doctrinal change through the judicial system is the primary mechanism, constrained by stare decisis.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, legislative_body, excluded,
    institutional, generational, analytical, national).

% Those directly harmed by a precedent that has become deeply entrenched but is now seen as unjust or incoherent. May include groups whose fundamental rights or interests are locked into a bad precedent, who lack the institutional resources, legal standing, or strategic position to mount a successful extraordinary-justification case. Their exit is structural—they cannot leave the jurisdiction's law, and the precedent binds their outcome.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, oppressed_by_precedent, payer,
    powerless, immediate, trapped, national).

% Legal scholars, policy advocates, and civil society actors who argue a precedent is incoherent or unjust and should be overruled. Excluded from the formal precedent-binding mechanism: they cannot overturn precedent directly. Can petition courts through amicus briefs or litigate test cases, but face the same extraordinary-justification barrier as norm-challenging litigants. Their influence is real but mediated through the precedent system's constraints.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, doctrinal_reformers, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_law_precedent_corpus__strict_stare_decisis, established_doctrine_holders).
narrative_ontology:fixing_cost_class(common_law_precedent_corpus__strict_stare_decisis, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes stable, predictable legal doctrine across time and jurisdiction: courts apply prior holdings consistently; litigants and practitioners can rely on settled law to structure transactions and advise clients; the legal system operates as a coherent corpus rather than as inconsistent, ad-hoc decisions.
% TRANSFER_FUNCTION: Extracts from norm challengers and those harmed by entrenched precedent (who must overcome extraordinary justification burden) to entrenched doctrine holders and practitioners (whose interests are locked in and protected from disruption). The extraction is the cost of the challenger's constrained access to doctrinal change.
% ABSENT_VOICES: Legislative bodies are structurally excluded from the precedent-binding mechanism itself (though they can overturn via statute). Oppressed groups and powerless litigants bound by bad precedent lack standing, resources, or strategic position to mount extraordinary-justification cases. Doctrinal reformers and civil-society advocates can petition but are mediated through the same constraint structure.
% DISAPPEARANCE_RATIONALE: If strict stare decisis vanished overnight—if courts could freely overturn precedent without extraordinary justification—legal doctrine would rapidly destabilize; entrenched practitioners would lose predictive anchor; decades of accumulated contract law, settled property boundaries, and institutional arrangements built on stable holdings would be at constant risk of judicial reversal. The legal system would reorganize around whatever doctrine emerged from the ensuing rounds of litigation, likely shifting power toward institutional actors with litigation capacity and toward current judicial majorities.
% FOUNDING_PROBLEM: Early common law lacked doctrine stability: judges made inconsistent holdings; litigants had no reliable way to predict outcomes; contracts and property settlements were at constant risk of judicial reversal; the legal system operated case-by-case without cumulative coherence.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and precedent defenders attest the founding problem is partly live: doctrine instability creates real transaction costs and undermines reliance interests. Norm challengers and legal reformers attest the founding problem is substantially solved by modern publication systems, legal scholarship, and appellate review, and that stare decisis now primarily serves entrenchment rather than stability. No corroboration from outside the entrenched-doctrine seats that the current extraordinary-justification burden is necessary.
narrative_ontology:disappearance_verdict(common_law_precedent_corpus__strict_stare_decisis, world_rearranges).
narrative_ontology:founding_problem_status(common_law_precedent_corpus__strict_stare_decisis, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_law_precedent_corpus__strict_stare_decisis, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(common_law_precedent_corpus__strict_stare_decisis, 'none', 1).
narrative_ontology:epsilon_provenance(common_law_precedent_corpus__strict_stare_decisis, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_law_precedent_corpus__strict_stare_decisis_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_law_precedent_corpus__strict_stare_decisis, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_law_precedent_corpus__strict_stare_decisis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the constraint extracts from norm challengers and powerless litigants by imposing the extraordinary-justification burden—a procedural cost that is decoupled from the actual merit of the challenge. Suppression is also high (0.72) because the narrow pathways for overruling actively suppress alternatives: litigants cannot argue their case on fresh grounds if precedent is binding; the constraint's persistence depends on enforcing the binding force, not on participant preference. Theater is moderate (0.38): some of the judicial work labeled as applying precedent is genuinely functional (ensuring consistency), but an increasing share is performative—courts elaborate doctrinal rationales to justify maintaining holdings that have become incoherent, rather than candidly overruling. The measurement series show steady extraction accumulation and theater creep over 40 years: as the precedent corpus grows, the burden of extraordinary justification rises, and the performance cost of maintaining it increases. Extraction flattens near t=30 because the constraint reaches structural saturation—it is as constraining as the stare decisis mechanism can make it given the institutional reality.
 *
 * PERSPECTIVAL GAP:
 *   The judicial seat and the entrenched-beneficiary seats perceive stare decisis as functional coordination solving the real problem of doctrinal stability and rule-of-law predictability. The norm-challenger and oppressed seats perceive the same mechanism as a systematic extraction mechanism: the cost of challenging entrenched holdings is prohibitively high by design. This gap is not a classification error—it is the core structural asymmetry that tangled_rope captures. The engine computes the judicial seat as experiencing more rope-like (coordination-heavy) classification, while challenger seats compute as snare-like (extraction-heavy). The story's measurements and commentary document this divergence; the schema captures it as a tangled_rope claim with explained seat divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Established doctrine holders and practitioners are beneficiaries (d near 0.0–0.3): their interests are protected, their advice is reliable, their reliance is secure. Norm challengers and the powerless are targets (d near 0.7–1.0): they bear the extraordinary-justification burden, face constrained pathways, pay the litigation cost while the burden of proof sits on them. The judiciary sits near symmetric (d ≈ 0.5) because they experience genuine coordination benefit (coherent doctrine, stable rule-of-law commitment) and genuine constraint (bound by precedent, limited from departing freely). The constraint's binding force on the judiciary is structural: they cannot simply overturn precedent at will, even when they believe it is wrong. Directionality overrides are not needed; the structural data (beneficiary/victim + exit options) derives the correct directional picture.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem—doctrinal instability and the inability to rely on settled law—is contested: doctrine defenders attest it is partly live (stability has real value), while reformers attest it is substantially solved (modern publication, precedent databases, and appellate process provide enough clarity). Mandatrophy emerges as the core ambiguity: the constraint may be maintaining real coordination value (preventing destabilizing doctrinal churn), or it may have outlived its founding purpose and now primarily serves entrenchment. The extraordinary-justification burden does reduce frivolous reversals, but it also locks in genuinely unjust or incoherent holdings. The institutional cost of maintaining the constraint (litigation burden on norm challengers, suppression of doctrinal evolution) is increasingly decoupled from the marginal coordination benefit. The constraint's mandate to ensure stable law is intact; whether that mandate justifies the observed extraction level and rigid suppression is the unresolved mandatrophy question.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraordinary_justification_threshold,
    'What standard of justification actually triggers a successful precedent overruling under strict stare decisis? Where is the threshold, and has it shifted over time?',
    'Empirical analysis of precedent-overruling cases over the interval: what facts, degree of doctrinal incoherence, magnitude of changed circumstances, and intensity of challenge actually succeeded in overruling? Where courts stated extraordinary justification was met, what evidence pattern distinguishes those cases from the failures?',
    'If the threshold is consistently high and rarely met, the extraction and suppression metrics are accurate. If the threshold has degraded over time or is applied unevenly across domains, the constraint may be weaker than the strict reading claims, or the suppression may be unevenly distributed across litigants (wealth-dependent access to extraordinary justification).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraordinary_justification_threshold, empirical, 'Operationalizing the extraordinary-justification standard.').

omega_variable(
    foundational_problem_persistence,
    'Does the foundational problem—doctrinal instability and inability to rely on settled law—remain live, or has it been substantially solved by modern legal publishing, precedent databases, and doctrinal scholarship?',
    'Historical-institutional comparison: survey practitioners and scholars about doctrinal predictability and reliance confidence in early common law vs. contemporary law. Compare litigation outcomes when parties rely on settled doctrine: how often are expectations disappointed by doctrinal shifts or inconsistent application?',
    'If the founding problem is substantially solved, the constraint''s mandate has expired and it primarily serves entrenchment (zombification/mandatrophy signal). If the founding problem remains live, the constraint''s coordination function is still justified, though the extraordinary-justification burden may still be too high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foundational_problem_persistence, empirical, 'Whether doctrinal stability remains a binding coordination problem.').

omega_variable(
    reading_coexistence_stability,
    'Can a single legal system coherently hold all three readings (strict stare decisis, evolutionary, pluralist) simultaneously? Or does the strict reading, once institutionalized, foreclose the others within that system?',
    'Jurisdictional comparison: examine common-law systems that have formally endorsed evolutionary or pluralist readings (UK Supreme Court departure from strict stare decisis, Canadian approach to precedent flexibility). Do those systems maintain internal coherence, or does the strict reading persistently reassert? Can litigants invoke flexibility while courts apply rigor case-by-case (pragmatic coexistence), or is the conflict ultimately resolved by one reading winning institutional supremacy?',
    'If the readings can coexist pragmatically, the constraint is weaker than claimed—flexibility is possible case-by-case within the apparent rigidity. If the strict reading forecloses the others when institutionalized, that is a true foreclosure relationship, not mere coexistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_coexistence_stability, conceptual, 'Whether the three precedent readings can coexist within a single legal system.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.72) structural (legal barriers, procedural burdens, high litigation cost) or internalized (litigants believe precedent should bind, have accepted the extraordinary-justification standard as legitimate)?',
    'Post-reform suppression trajectory: if a jurisdiction weakened stare decisis (adopted evolutionary or pluralist reading), did norm-challenging litigation surge and doctrinal fluidity increase, or did suppression persist because practitioners internalized the expectation that precedent should bind? Did the litigation rate and outcome rates for overruling petitions change measurably after the rule shift?',
    'If suppression is primarily structural, weakening the extraordinary-justification requirement would directly increase norm-challenging access. If suppression is internalized, the same rule change might have minimal effect because litigants and judges have absorbed the norm that precedent should constrain. The measurement of suppression is a single scalar; the mechanism distinction informs how the constraint would evolve if the explicit rule were changed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural vs. internalized suppression in stare decisis.').

omega_variable(
    asymmetric_vulnerability_to_bad_precedent,
    'Does the extraordinary-justification requirement fall equally on all litigants, or do powerless groups and those without litigation capacity systematically face higher barriers to overruling precedent that harms them?',
    'Empirical analysis of overruling petitions and outcomes stratified by party type: compare success rates for overruling when petitioned by well-resourced institutional actors vs. individual or powerless litigants. Compare the strength of doctrinal challenge needed when a powerful institution seeks overruling vs. when a marginalized group does.',
    'If the burden is unequally distributed, the constraint operates as a snare on powerless litigants and a tangled rope on well-resourced parties—asymmetric extraction masked by a procedurally neutral rule. This would refine the directionality: powerless seats would compute d near 1.0 (full targets), while organized seats would compute d lower despite formal equality before the extraordinary-justification standard.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asymmetric_vulnerability_to_bad_precedent, empirical, 'Whether stare decisis''s burdens fall equally or stratify by party power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__strict_stare_decisis, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(comm_tr_t0, observed).
narrative_ontology:measurement(comm_tr_t5, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 5, 0.25).
narrative_ontology:measurement_basis(comm_tr_t5, observed).
narrative_ontology:measurement(comm_tr_t10, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 10, 0.28).
narrative_ontology:measurement_basis(comm_tr_t10, observed).
narrative_ontology:measurement(comm_tr_t15, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 15, 0.31).
narrative_ontology:measurement_basis(comm_tr_t15, observed).
narrative_ontology:measurement(comm_tr_t20, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 20, 0.34).
narrative_ontology:measurement_basis(comm_tr_t20, observed).
narrative_ontology:measurement(comm_tr_t25, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 25, 0.36).
narrative_ontology:measurement_basis(comm_tr_t25, observed).
narrative_ontology:measurement(comm_tr_t30, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 30, 0.37).
narrative_ontology:measurement_basis(comm_tr_t30, observed).
narrative_ontology:measurement(comm_tr_t40, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 40, 0.38).
narrative_ontology:measurement_basis(comm_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(comm_be_t0, observed).
narrative_ontology:measurement(comm_be_t5, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 5, 0.56).
narrative_ontology:measurement_basis(comm_be_t5, observed).
narrative_ontology:measurement(comm_be_t10, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 10, 0.6).
narrative_ontology:measurement_basis(comm_be_t10, observed).
narrative_ontology:measurement(comm_be_t15, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 15, 0.63).
narrative_ontology:measurement_basis(comm_be_t15, observed).
narrative_ontology:measurement(comm_be_t20, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 20, 0.65).
narrative_ontology:measurement_basis(comm_be_t20, observed).
narrative_ontology:measurement(comm_be_t25, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(comm_be_t25, observed).
narrative_ontology:measurement(comm_be_t30, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(comm_be_t30, observed).
narrative_ontology:measurement(comm_be_t40, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(comm_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(comm_su_t0, observed).
narrative_ontology:measurement(comm_su_t5, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(comm_su_t5, observed).
narrative_ontology:measurement(comm_su_t10, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 10, 0.65).
narrative_ontology:measurement_basis(comm_su_t10, observed).
narrative_ontology:measurement(comm_su_t15, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(comm_su_t15, observed).
narrative_ontology:measurement(comm_su_t20, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(comm_su_t20, observed).
narrative_ontology:measurement(comm_su_t25, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(comm_su_t25, observed).
narrative_ontology:measurement(comm_su_t30, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(comm_su_t30, observed).
narrative_ontology:measurement(comm_su_t40, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(comm_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__strict_stare_decisis, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(common_law_precedent_corpus__strict_stare_decisis, 0.12).
narrative_ontology:affects_constraint(common_law_precedent_corpus__strict_stare_decisis, common_law_precedent_corpus__evolutionary_framework).
narrative_ontology:affects_constraint(common_law_precedent_corpus__strict_stare_decisis, common_law_precedent_corpus__pluralist_balancing).

% DUAL FORMULATION NOTE:
% The common-law precedent corpus decomposes into three structurally distinct constraint stories, one per reading: (1) strict_stare_decisis (this file) — precedent binds rigidly, overruling rare, extraction high; (2) evolutionary_framework (sibling) — precedent provides adaptive container, reinterpretation within doctrine, extraction lower; (3) pluralist_balancing (sibling) — precedent weight varies by context, case-by-case balancing, extraction intermediate. Each reading instantiates a different ε for the same kernel. The strict reading is upstream in legitimacy (it is the official doctrine in common-law jurisdictions that claim fidelity to precedent), but the evolutionary reading exercises pragmatic influence in practice (courts often reinterpret rather than explicitly overturn). All three coexist as live positions in jurisprudential debate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
