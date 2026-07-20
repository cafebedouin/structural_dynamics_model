% ============================================================================
% CONSTRAINT STORY: separation_of_powers_text__functionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_separation_of_powers_text__functionalist_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: separation_of_powers_text__functionalist_reading
 *   human_readable: Functionalist Reading of Separation of Powers
 *   domain: constitutional_law/administrative_law
 *
 * SUMMARY:
 *   This constraint instantiates the functionalist reading of the U.S.
 *   Constitution's separation of powers provisions. It treats the vesting
 *   clauses as establishing a flexible framework where Congress may delegate
 *   legislative-type authority to administrative agencies provided an
 *   'intelligible principle' guides them, and where the President shares
 *   executive functions with quasi-independent agencies. The reading
 *   coordinates the modern regulatory state but asymmetrically extracts from
 *   presidential unitary control and from regulated parties who face agency
 *   coercion legitimated by this doctrine. It is contested by formalist and
 *   unitary-executive sibling readings of the same constitutional text.
 *
 * KEY AGENTS:
 *   - administrative_agencies: Primary beneficiary (institutional/constrained) â regulatory capacity and legitimacy depend on this reading.
 *   - congress: Primary beneficiary (institutional/mobile) â gains delegation flexibility and avoidance of direct regulatory responsibility.
 *   - federal_judiciary: Agenda-setter (institutional/analytical) â maintains the interpretive framework through balancing tests and precedent.
 *   - president: Payer (powerful/constrained) â loses unitary executive control over independent agencies and shares legislative-type functions.
 *   - regulated_industries: Payer (powerful/constrained) â bear compliance costs from agency actions shielded by the functionalist doctrine.
 *   - formalist_jurists: Excluded voices (organized/constrained) â advocate strict categorical boundaries but are doctrinally marginalized.
 *   - unitary_executive_advocates: Excluded voices (organized/constrained) â argue for concentrated presidential power but are marginalized by tolerance of overlapping authority.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__functionalist_reading, 0.38).
domain_priors:suppression_score(separation_of_powers_text__functionalist_reading, 0.45).
domain_priors:theater_ratio(separation_of_powers_text__functionalist_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__functionalist_reading, tangled_rope).
narrative_ontology:human_readable(separation_of_powers_text__functionalist_reading, "Functionalist Reading of Separation of Powers").
narrative_ontology:topic_domain(separation_of_powers_text__functionalist_reading, "constitutional_law/administrative_law").

domain_priors:requires_active_enforcement(separation_of_powers_text__functionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__functionalist_reading, '10c3bc5c-7234-470d-8495-c4237d441f6e').
narrative_ontology:cs_kernel_codification('10c3bc5c-7234-470d-8495-c4237d441f6e', fixed_text).
narrative_ontology:cs_authority_grounding('10c3bc5c-7234-470d-8495-c4237d441f6e', lineage).
narrative_ontology:cs_interpretation_layer_present('10c3bc5c-7234-470d-8495-c4237d441f6e').
narrative_ontology:cs_reading_relation('10c3bc5c-7234-470d-8495-c4237d441f6e', separation_of_powers_text__formalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('10c3bc5c-7234-470d-8495-c4237d441f6e', separation_of_powers_text__unitary_executive_reading, coexists_with).
narrative_ontology:cs_axiom('10c3bc5c-7234-470d-8495-c4237d441f6e', foundational, practical_equilibrium_constitutionalism).
narrative_ontology:cs_axiom_status(practical_equilibrium_constitutionalism, holdable).
narrative_ontology:cs_axiom_grounding('10c3bc5c-7234-470d-8495-c4237d441f6e', practical_equilibrium_constitutionalism, conventional).
narrative_ontology:cs_axiom('10c3bc5c-7234-470d-8495-c4237d441f6e', foundational, intelligible_principle_sufficiency).
narrative_ontology:cs_axiom_status(intelligible_principle_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('10c3bc5c-7234-470d-8495-c4237d441f6e', intelligible_principle_sufficiency, conventional).
narrative_ontology:cs_reference_frame('10c3bc5c-7234-470d-8495-c4237d441f6e', practical_interbranch_equilibrium).
narrative_ontology:cs_drift_state('10c3bc5c-7234-470d-8495-c4237d441f6e', contemporary_formalist_revival, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('10c3bc5c-7234-470d-8495-c4237d441f6e', '').
narrative_ontology:cs_kernel_id(separation_of_powers_text__functionalist_reading, separation_of_powers_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, administrative_agencies).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, congress).
narrative_ontology:constraint_victim(separation_of_powers_text__functionalist_reading, president).
narrative_ontology:constraint_victim(separation_of_powers_text__functionalist_reading, regulated_industries).
narrative_ontology:constraint_vindicates(separation_of_powers_text__functionalist_reading, regulatory_state_legitimacy).
narrative_ontology:constraint_vindicates(separation_of_powers_text__functionalist_reading, intelligible_principle_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercise rulemaking and adjudicatory authority delegated by Congress under statutes reviewed against the intelligible principle standard. Their entire regulatory capacity depends on courts continuing to interpret the Constitution as permitting overlapping functions and broad delegation.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, administrative_agencies, beneficiary,
    institutional, generational, constrained, national).

% Delegates complex policy choices to expert agencies through broadly worded statutes, retaining oversight capacity without direct operational responsibility. Benefits from flexibility to adjust regulatory scope without re-enacting legislation for every technical detail.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, congress, beneficiary,
    institutional, biographical, mobile, national).

% Maintains the functionalist interpretive framework through case law, applying balancing tests and the intelligible principle doctrine to reject formalist non-delegation challenges. Exercises constitutional interpretive authority by preserving this reading against structural alternatives.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Must share executive and legislative-type functions with independent agencies and congressional committees. Cannot remove certain agency heads at will or direct all executive policy because functionalism legitimizes overlapping authority and for-cause removal protections.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, president, payer,
    powerful, biographical, constrained, national).

% Subject to agency regulations and adjudications whose constitutional legitimacy rests on the functionalist reading. Attempts to challenge agency authority as unconstitutional delegation are routinely rejected by courts applying flexible balancing.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, regulated_industries, payer,
    powerful, biographical, constrained, national).

% Advance a competing constitutional theory requiring strict categorical boundaries between legislative, executive, and judicial power. Their influence is suppressed in administrative law doctrine because the functionalist framework treats such boundaries as impractical.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, formalist_jurists, excluded,
    organized, generational, constrained, national).

% Argue that Article II vests all executive power in the President and that independent agencies violate the Constitution. The functionalist reading's tolerance of overlapping authority and independent agency insulation marginalizes their position in judicial doctrine.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, unitary_executive_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables Congress, the President, and administrative agencies to cooperate and check each other without strict categorical boundaries, allowing the modern regulatory state to function by permitting delegation to expert agencies under judicial supervision.
% TRANSFER_FUNCTION: Transfers discretionary policy-making authority from Congress to administrative agencies, and distributes executive oversight between the President and independent agencies, under a judicial balancing framework.
% ABSENT_VOICES: Formalist jurists and unitary executive theorists are doctrinally marginalized in administrative law; regulated parties challenging agency authority on non-delegation grounds are typically excluded because courts treat the functionalist framework as settled.
% DISAPPEARANCE_RATIONALE: If the functionalist reading vanished, the statutory basis for the modern administrative state would face immediate constitutional crisis; thousands of agency regulations would be vulnerable to non-delegation challenges, and inter-branch power would require urgent renegotiation.
% FOUNDING_PROBLEM: How to empower effective national governance while preventing the concentration of tyrannical power.
% FOUNDING_PROBLEM_CORROBORATION: Progressive-era and New Deal historians attest the functionalist framework was built to solve governance capacity problems; originalist scholars and formalist jurists outside the beneficiary set contest this genealogy, arguing the founders intended strict boundaries. Corroboration is split along interpretive lines, with no neutral party unanimously affirming the functionalist origin story.
narrative_ontology:disappearance_verdict(separation_of_powers_text__functionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(separation_of_powers_text__functionalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(separation_of_powers_text__functionalist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(separation_of_powers_text__functionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(separation_of_powers_text__functionalist_reading, 0.38, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(separation_of_powers_text__functionalist_reading_tests).
:- end_tests(separation_of_powers_text__functionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.38 to reflect the 'lower Îµ' indicated by the source material: the primary operation is coordination of inter-branch governance and regulatory continuity, not rent extraction. However, asymmetric costs exist. Suppression (0.45) captures the doctrinal marginalization of formalist non-delegation and unitary-executive theories. Theater ratio (0.30) reflects that some judicial balancing is performative, but the coordination function is substantial. Accessibility collapse (0.60) indicates that within the functionalist framework, formalist alternatives appear legally untenable even if intellectually available. Resistance (0.55) reflects ongoing formalist and unitary-executive challenges in the judiciary and academy.
 *
 * PERSPECTIVAL GAP:
 *   From the agency and congressional seats, the constraint is experienced as necessary coordination enabling modern governance. From the presidential seat, it is experienced as a structural check on unitary command. From regulated industries, it is experienced as the doctrinal shield behind which agency coercion operates. The engine computes these divergences from the structural data without reconciling them.
 *
 * DIRECTIONALITY LOGIC:
 *   Administrative agencies and Congress are declared beneficiaries because the functionalist reading legitimates delegation and regulatory capacity (low d, effective extraction damped or inverted). The President and regulated industries are declared victims because the reading diffuses executive control and enables agency impositions (high d, effective extraction amplified). The federal judiciary sits as agenda_setter with analytical exit, neither purely beneficiary nor victim. Formalist jurists and unitary executive advocates are excluded, their alternatives suppressed by the doctrinal dominance of functionalism.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling the functionalist reading as pure extraction (snare) because the coordination functionâenabling a functional regulatory state, preventing governmental paralysisâis genuine and substantial. It prevents mislabeling it as pure coordination (rope) because the asymmetric distribution of costs (presidential power diffusion, regulated party subjection) is structurally embedded and requires active judicial enforcement to maintain against formalist revival.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_decomposition,
    'Does the separation_of_powers_text kernel decompose into three distinct constraints with divergent Îµ values, or is it one constraint with observer-relative classification?',
    'Compare the structural metrics, beneficiary/victim sets, and enforcement requirements across all three readings; if they diverge systematically, the Îµ-invariance principle requires separate constraint stories.',
    'If the readings are structurally distinct, each warrants its own classification and no single reading can claim to be the ''true'' separation of powers; if similar, the kernel is one constraint with perspectival variation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_decomposition, conceptual, 'Whether the contested kernel decomposes into distinct constraints per the epsilon-invariance principle.').

omega_variable(
    intelligible_principle_coherence,
    'Does the intelligible principle standard provide a genuine constitutional limit on delegation, or is it a theatrical check that ratifies virtually any statutory delegation?',
    'Empirical analysis of delegations struck down versus upheld since 1935; measurement of statutory vagueness at enactment against judicial outcomes.',
    'If theatrical, the theater_ratio rises and the constraint slides toward piton or snare; if a genuine limit, it confirms the tangled_rope classification with real coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intelligible_principle_coherence, empirical, 'Whether the intelligible principle doctrine is a functional limit or performative ritual.').

omega_variable(
    formalist_revival_dynamics,
    'If the formalist or unitary-executive readings regain doctrinal dominance, does the functionalist constraint become a piton (atrophied but persistent) or dissolve entirely?',
    'Track Supreme Court docket trends, agency abolition or creation rates, and judicial citation patterns to functionalist precedents.',
    'If the constraint persists without functionalist enforcement, it becomes a piton maintained by inertia; if replaced, it confirms the constraint was always contingent on active judicial maintenance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(formalist_revival_dynamics, conceptual, 'Downstream classification if sibling readings displace functionalist dominance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__functionalist_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sop_func_tr_t0, separation_of_powers_text__functionalist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sop_func_tr_t18, separation_of_powers_text__functionalist_reading, theater_ratio, 18, 0.18).
narrative_ontology:measurement(sop_func_tr_t36, separation_of_powers_text__functionalist_reading, theater_ratio, 36, 0.25).
narrative_ontology:measurement(sop_func_tr_t54, separation_of_powers_text__functionalist_reading, theater_ratio, 54, 0.3).
narrative_ontology:measurement(sop_func_tr_t72, separation_of_powers_text__functionalist_reading, theater_ratio, 72, 0.32).
narrative_ontology:measurement(sop_func_tr_t90, separation_of_powers_text__functionalist_reading, theater_ratio, 90, 0.3).

% Extraction over time
narrative_ontology:measurement(sop_func_be_t0, separation_of_powers_text__functionalist_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(sop_func_be_t18, separation_of_powers_text__functionalist_reading, base_extractiveness, 18, 0.28).
narrative_ontology:measurement(sop_func_be_t36, separation_of_powers_text__functionalist_reading, base_extractiveness, 36, 0.34).
narrative_ontology:measurement(sop_func_be_t54, separation_of_powers_text__functionalist_reading, base_extractiveness, 54, 0.38).
narrative_ontology:measurement(sop_func_be_t72, separation_of_powers_text__functionalist_reading, base_extractiveness, 72, 0.4).
narrative_ontology:measurement(sop_func_be_t90, separation_of_powers_text__functionalist_reading, base_extractiveness, 90, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(sop_func_su_t0, separation_of_powers_text__functionalist_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(sop_func_su_t18, separation_of_powers_text__functionalist_reading, suppression_requirement, 18, 0.38).
narrative_ontology:measurement(sop_func_su_t36, separation_of_powers_text__functionalist_reading, suppression_requirement, 36, 0.45).
narrative_ontology:measurement(sop_func_su_t54, separation_of_powers_text__functionalist_reading, suppression_requirement, 54, 0.48).
narrative_ontology:measurement(sop_func_su_t72, separation_of_powers_text__functionalist_reading, suppression_requirement, 72, 0.5).
narrative_ontology:measurement(sop_func_su_t90, separation_of_powers_text__functionalist_reading, suppression_requirement, 90, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(separation_of_powers_text__functionalist_reading, formalist_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__functionalist_reading, unitary_executive_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the separation_of_powers_text kernel. The kernel decomposes into three structurally distinct constraints because the Îµ values, beneficiary structures, and enforcement requirements differ across readings: the formalist reading (high suppression of agency power, strict boundaries), the functionalist reading (lower Îµ, coordination via deference and balancing), and the unitary executive reading (concentrated presidential control, high extraction from agency independence). Each reading has its own constraint_id and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
