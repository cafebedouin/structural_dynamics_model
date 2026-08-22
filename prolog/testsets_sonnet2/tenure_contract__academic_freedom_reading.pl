% ============================================================================
% CONSTRAINT STORY: tenure_contract__academic_freedom_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tenure_contract__academic_freedom_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: tenure_contract__academic_freedom_reading
 *   human_readable: Tenure as Academic Freedom Guarantee
 *   domain: higher_education_governance/labor_economics/institutional_theory
 *
 * SUMMARY:
 *   This story instantiates the academic-freedom reading of the
 *   tenure_contract kernel: tenure is analyzed as a coordination mechanism
 *   that decouples faculty survival from institutional or political
 *   displeasure, enabling research that would otherwise be suppressed through
 *   career-ending pressure. This reading's ε is deliberately low and stable —
 *   the standing arrangement, assessed by this reading's own lights, produces
 *   modest coordination overhead (administrative due-process costs,
 *   occasional protection of genuinely unproductive researchers) rather than
 *   substantial extraction. The suppression difficulty faced by external
 *   political actors and donors is the mechanism's design goal, not
 *   incidental friction, and shows up here as elevated
 *   resistance/accessibility metrics rather than as extraction against
 *   faculty. Two sibling constraints read the same kernel text differently:
 *   the institutional_extraction_reading treats the identical contractual
 *   mechanism as rent-capture by early-career winners at the expense of
 *   contingent faculty, and the demographic_reproduction_reading treats the
 *   associated peer-review 'fit' criteria as gatekeeping. Those are separate
 *   files with separate ε values and separate beneficiary/victim sets — this
 *   file does not average across them.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__academic_freedom_reading, 0.28).
domain_priors:suppression_score(tenure_contract__academic_freedom_reading, 0.32).
domain_priors:theater_ratio(tenure_contract__academic_freedom_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__academic_freedom_reading, rope).
narrative_ontology:human_readable(tenure_contract__academic_freedom_reading, "Tenure as Academic Freedom Guarantee").
narrative_ontology:topic_domain(tenure_contract__academic_freedom_reading, "higher_education_governance/labor_economics/institutional_theory").

domain_priors:requires_active_enforcement(tenure_contract__academic_freedom_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__academic_freedom_reading, 'bfd31ab7-b452-4102-b085-de1cd5a2c7db').
narrative_ontology:cs_kernel_codification('bfd31ab7-b452-4102-b085-de1cd5a2c7db', formalized).
narrative_ontology:cs_authority_grounding('bfd31ab7-b452-4102-b085-de1cd5a2c7db', practice).
narrative_ontology:cs_interpretation_layer_present('bfd31ab7-b452-4102-b085-de1cd5a2c7db').
narrative_ontology:cs_reading_relation('bfd31ab7-b452-4102-b085-de1cd5a2c7db', tenure_contract__institutional_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('bfd31ab7-b452-4102-b085-de1cd5a2c7db', tenure_contract__demographic_reproduction_reading, coexists_with).
narrative_ontology:cs_axiom('bfd31ab7-b452-4102-b085-de1cd5a2c7db', foundational, survival_independence_enables_truth_seeking).
narrative_ontology:cs_axiom_status(survival_independence_enables_truth_seeking, holdable).
narrative_ontology:cs_axiom_grounding('bfd31ab7-b452-4102-b085-de1cd5a2c7db', survival_independence_enables_truth_seeking, instrumental).
narrative_ontology:cs_axiom('bfd31ab7-b452-4102-b085-de1cd5a2c7db', secondary, institutional_and_political_pressure_reliably_targets_unpopular_findings).
narrative_ontology:cs_axiom_status(institutional_and_political_pressure_reliably_targets_unpopular_findings, holdable).
narrative_ontology:cs_axiom_grounding('bfd31ab7-b452-4102-b085-de1cd5a2c7db', institutional_and_political_pressure_reliably_targets_unpopular_findings, empirically_contingent).
narrative_ontology:cs_reference_frame('bfd31ab7-b452-4102-b085-de1cd5a2c7db', aaup_1940_academic_freedom_statement).
narrative_ontology:cs_drift_state('bfd31ab7-b452-4102-b085-de1cd5a2c7db', contemporary_political_pressure_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('bfd31ab7-b452-4102-b085-de1cd5a2c7db', '').
narrative_ontology:cs_kernel_id(tenure_contract__academic_freedom_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, tenured_faculty).
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, students_and_public).
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, scientific_knowledge_base).
narrative_ontology:constraint_victim(tenure_contract__academic_freedom_reading, external_political_actors).
narrative_ontology:constraint_victim(tenure_contract__academic_freedom_reading, donor_interests_seeking_suppression).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold indefinite appointments that can only be terminated for cause or financial exigency through due process. This lets them pursue controversial, politically unpopular, or commercially unprofitable research programs without fear that a single unpopular finding or public backlash ends their career. Exit to industry or another institution remains available but is costly, which is what makes the guarantee meaningful rather than trivial.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, tenured_faculty, beneficiary,
    moderate, civilizational, mobile, national).

% Receive the downstream benefit of research and teaching that has not been filtered through fear of political or institutional retaliation — including findings inconvenient to funders, governments, or popular opinion. They do not participate in the tenure process and bear no direct cost from it in this reading; their interest is in the quality and independence of the knowledge produced.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, students_and_public, beneficiary,
    powerless, generational, analytical, national).

% Legislators, political appointees, and advocacy campaigns who wish to defund, discipline, or remove faculty whose research or public statements are politically inconvenient. Tenure is the specific mechanism that blocks their preferred lever (termination or non-renewal) from working; they must resort to costlier and more visible tools — funding cuts, public pressure campaigns, statutory intervention — all of which tenure is designed to make harder and slower.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, external_political_actors, payer,
    powerful, immediate, constrained, national).

% Wealthy donors or corporate sponsors who want research embarrassing to their interests suppressed or the responsible researcher removed. Tenure denies the university administration the easy compliance tool of quiet dismissal, forcing the donor to either accept the finding, withdraw funding (a blunt and visible instrument), or mount a sustained public campaign — all costlier and less certain than simple termination would be.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, donor_interests_seeking_suppression, payer,
    powerful, biographical, constrained, national).

% Grants, administers, and is bound by tenure protections; must run due-process termination proceedings rather than at-will dismissal. Administrators experience tenure as a genuine constraint on their own discretion, including when political or donor pressure pushes them to act against a faculty member — the protection binds the administration as much as it binds outside actors.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, university_administration, agenda_setter,
    institutional, generational, constrained, national).

% The accumulated body of research findings, non-agent but named for completeness: benefits in the sense that findings unfiltered by survival fear are more likely to include unpopular but true results, strengthening the long-run reliability of the knowledge produced under this arrangement.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, scientific_knowledge_base, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(tenure_contract__academic_freedom_reading, scientific_knowledge_base).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine problem that truth-seeking research sometimes produces findings unpopular with funders, governments, or public opinion; without a survival guarantee, researchers rationally self-censor toward safe, fundable, uncontroversial conclusions, degrading the reliability of the knowledge base over time.
% TRANSFER_FUNCTION: Moves discretion away from university administrations and the external political/donor actors who would otherwise pressure them, toward individual faculty members, in the specific domain of continued employment. No direct monetary transfer; the transfer is of removal-power away from parties who might use it to suppress inconvenient findings.
% ABSENT_VOICES: External political actors and donor interests are structurally prevented from participating in individual tenure-revocation decisions once granted — that exclusion is precisely the mechanism's point in this reading, not an oversight. They would object that the arrangement insulates faculty from accountability to the public or funders who ultimately support the institution.
% DISAPPEARANCE_RATIONALE: If tenure protections vanished overnight, faculty conducting politically sensitive or commercially inconvenient research would face immediate exposure to termination pressure; risk-averse self-censorship would rise, politically unpopular findings would become rarer, and universities would become substantially more responsive to short-term political and donor pressure in hiring and retention decisions.
% FOUNDING_PROBLEM: Early 20th-century cases of professors dismissed for unpopular economic, political, or scientific views (e.g., the Ross case at Stanford, WWI-era loyalty purges) demonstrated that at-will academic employment let institutional or political displeasure directly terminate inquiry, motivating the 1940 AAUP Statement of Principles establishing tenure as an academic-freedom guarantee.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary academic freedom monitoring organizations (e.g., PEN America, AAUP censure reports) document ongoing legislative and donor pressure campaigns targeting named faculty over research findings, corroborating from outside the beneficiary group that the founding problem — political and financial pressure to remove inconvenient researchers — remains active rather than historical.
narrative_ontology:disappearance_verdict(tenure_contract__academic_freedom_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__academic_freedom_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__academic_freedom_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(tenure_contract__academic_freedom_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tenure_contract__academic_freedom_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tenure_contract__academic_freedom_reading_tests).
:- end_tests(tenure_contract__academic_freedom_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.28) and slowly rising, reflecting genuine but modest coordination cost: due-process termination proceedings, occasional protection of unproductive incumbents that the academic-freedom reading treats as an acceptable false-positive rate rather than the reading's central concern. Suppression (0.32) captures the fact that the arrangement does actively suppress one class of action — at-will removal of faculty for unpopular findings — but this reading treats that suppression as directed at externally-motivated removal attempts, not at faculty themselves. Theater ratio stays low (0.15, drifting slightly upward) because the due-process function remains substantively active rather than becoming purely ceremonial across the interval. Resistance is authored higher (0.55) because political and donor actors who want tenure weakened or circumvented are a real, organized, persistent constituency — the metric reflects the friction genuinely present in the system.
 *
 * PERSPECTIVAL GAP:
 *   From the faculty seat, tenure computes as coordination — a genuine collective-action solution to survival-contingent self-censorship. From the external political/donor seat, the identical structure computes as an obstruction imposed on their legitimate oversight or funding-conditionality interest. The engine computes both from the same structural data; this reading does not adjudicate which seat is 'right,' only documents which coordination function this specific reading is about.
 *
 * DIRECTIONALITY LOGIC:
 *   Faculty and the diffuse student/public beneficiary class sit near the low-χ end: the coordination function directly protects the former and indirectly benefits the latter through research quality, with no direct extraction running toward them in this reading. External political actors and donor interests seeking suppression sit near the high-χ end: the constraint's entire structural purpose, in this reading, is to make their preferred action (rapid removal of an inconvenient researcher) costly and slow. Their 'constrained' exit option reflects that they retain other levers (funding cuts, public campaigns, legislation) but the direct lever is blocked.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (political/institutional removal of researchers for unpopular findings) is corroborated as live by independent academic-freedom monitoring organizations rather than solely by tenured faculty who benefit from the arrangement, which supports treating this reading's coordination claim as substantive rather than a self-serving cover story. This guards against mislabeling the arrangement as pure extraction (the institutional_extraction_reading's claim) by keeping this reading's ε anchored to the specific, externally-corroborated function it protects, rather than to unrelated effects (employment rigidity, demographic composition) that belong to sibling constraints.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tenure_kernel_reading_divergence,
    'Is the single contractual mechanism of tenure better modeled as academic-freedom coordination, rent-extraction by incumbents, or demographic gatekeeping — or does the answer depend irreducibly on which faculty population and which review stage is examined?',
    'Compare denial/revocation case records across the three lenses: cases of political/donor-driven termination attempts blocked (supports this reading), cases of unproductive incumbents shielded at cost to junior/contingent faculty (supports institutional_extraction_reading), and demographic composition of tenure-track hires filtered through ''collegiality'' criteria (supports demographic_reproduction_reading). All three patterns may coexist without any single one being the full account.',
    'If the academic-freedom function is genuinely dominant and externally corroborated (as this reading claims), tenure remains classifiable as rope from the faculty/public seat even while sibling readings correctly classify adjacent effects (contingent-labor cost-loading, demographic reproduction) as tangled_rope or snare from other seats — these are compatible, not contradictory, findings across separate constraint files.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tenure_kernel_reading_divergence, conceptual, 'Whether the tenure_contract kernel''s three readings describe genuinely separable structural functions or an inseparable bundle.').

omega_variable(
    external_suppression_measurement_gap,
    'How much of the observed reduction in political/donor-driven faculty removal is causally attributable to tenure specifically, versus other protective factors (institutional prestige, legal employment protections generally, public opinion norms)?',
    'Comparative analysis of academic freedom outcomes in tenure-track versus non-tenure-track researchers facing comparable political pressure, controlling for institutional type and country.',
    'If tenure''s specific causal contribution is small relative to other protective factors, this reading''s claimed coordination benefit is overstated and ε should be revised upward; if tenure is the decisive protective mechanism, the low ε authored here is well-supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(external_suppression_measurement_gap, empirical, 'Causal attribution of academic-freedom outcomes to tenure specifically versus confounding protective factors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__academic_freedom_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenu_tr_t0, tenure_contract__academic_freedom_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(tenu_tr_t8, tenure_contract__academic_freedom_reading, theater_ratio, 8, 0.09).
narrative_ontology:measurement(tenu_tr_t16, tenure_contract__academic_freedom_reading, theater_ratio, 16, 0.1).
narrative_ontology:measurement(tenu_tr_t24, tenure_contract__academic_freedom_reading, theater_ratio, 24, 0.12).
narrative_ontology:measurement(tenu_tr_t32, tenure_contract__academic_freedom_reading, theater_ratio, 32, 0.14).
narrative_ontology:measurement(tenu_tr_t40, tenure_contract__academic_freedom_reading, theater_ratio, 40, 0.15).

% Extraction over time
narrative_ontology:measurement(tenu_be_t0, tenure_contract__academic_freedom_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(tenu_be_t8, tenure_contract__academic_freedom_reading, base_extractiveness, 8, 0.23).
narrative_ontology:measurement(tenu_be_t16, tenure_contract__academic_freedom_reading, base_extractiveness, 16, 0.24).
narrative_ontology:measurement(tenu_be_t24, tenure_contract__academic_freedom_reading, base_extractiveness, 24, 0.26).
narrative_ontology:measurement(tenu_be_t32, tenure_contract__academic_freedom_reading, base_extractiveness, 32, 0.27).
narrative_ontology:measurement(tenu_be_t40, tenure_contract__academic_freedom_reading, base_extractiveness, 40, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(tenu_su_t0, tenure_contract__academic_freedom_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(tenu_su_t8, tenure_contract__academic_freedom_reading, suppression_requirement, 8, 0.22).
narrative_ontology:measurement(tenu_su_t16, tenure_contract__academic_freedom_reading, suppression_requirement, 16, 0.24).
narrative_ontology:measurement(tenu_su_t24, tenure_contract__academic_freedom_reading, suppression_requirement, 24, 0.28).
narrative_ontology:measurement(tenu_su_t32, tenure_contract__academic_freedom_reading, suppression_requirement, 32, 0.3).
narrative_ontology:measurement(tenu_su_t40, tenure_contract__academic_freedom_reading, suppression_requirement, 40, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__academic_freedom_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(tenure_contract__academic_freedom_reading, 0.1).
narrative_ontology:affects_constraint(tenure_contract__academic_freedom_reading, tenure_contract__institutional_extraction_reading).
narrative_ontology:affects_constraint(tenure_contract__academic_freedom_reading, tenure_contract__demographic_reproduction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the tenure_contract kernel, decomposed per the ε-invariance principle rather than authored as a single observable-dependent story. academic_freedom_reading (this file) authors low, stable ε (~0.28) reflecting a genuinely corroborated coordination function protecting researchers from politically/financially motivated removal. institutional_extraction_reading authors substantially higher ε reflecting rent-capture by tenured incumbents at the expense of contingent faculty. demographic_reproduction_reading authors ε reflecting gatekeeping effects of 'fit' and 'collegiality' criteria in the review process. All three share the same underlying contractual kernel (indefinite appointment absent cause/exigency) but diverge sharply on beneficiary/victim structure and extraction level — they are linked here rather than merged because merging would violate ε-invariance (a single constraint cannot honestly carry three incompatible extraction values).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
