% ============================================================================
% CONSTRAINT STORY: constitutional_text__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__judicial_supremacy_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: constitutional_text__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy Reading of Constitutional Text
 *   domain: constitutional_theory/political_philosophy/comparative_law
 *
 * SUMMARY:
 *   This constraint story models the judicial supremacy reading of
 *   constitutional text: the claim that courts possess final interpretive
 *   authority and that judicial invalidation of legislation is the conclusive
 *   determination of constitutional meaning. The reading presents this as a
 *   structural necessity of constitutionalism — a mountain of constitutional
 *   law. The authored metrics describe a constraint that extracts democratic
 *   responsiveness (suppression 0.80, extractiveness 0.65) while providing a
 *   genuine coordination function (rights protection). The divergence between
 *   claimed_type (mountain) and the metric profile is the measurement target:
 *   a false summit candidate where identifiable beneficiaries (courts,
 *   rights_claimants) and victims (democratic_responsiveness,
 *   legislative_majorities) exist beneath a natural-law presentation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__judicial_supremacy_reading, 0.65).
domain_priors:suppression_score(constitutional_text__judicial_supremacy_reading, 0.8).
domain_priors:theater_ratio(constitutional_text__judicial_supremacy_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__judicial_supremacy_reading, mountain).
narrative_ontology:human_readable(constitutional_text__judicial_supremacy_reading, "Judicial Supremacy Reading of Constitutional Text").
narrative_ontology:topic_domain(constitutional_text__judicial_supremacy_reading, "constitutional_theory/political_philosophy/comparative_law").

domain_priors:requires_active_enforcement(constitutional_text__judicial_supremacy_reading).
domain_priors:emerges_naturally(constitutional_text__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__judicial_supremacy_reading, '1590c1ee-5938-41db-b49d-57b39d6cc668').
narrative_ontology:cs_kernel_codification('1590c1ee-5938-41db-b49d-57b39d6cc668', fixed_text).
narrative_ontology:cs_authority_grounding('1590c1ee-5938-41db-b49d-57b39d6cc668', lineage).
narrative_ontology:cs_interpretation_layer_present('1590c1ee-5938-41db-b49d-57b39d6cc668').
narrative_ontology:cs_reading_relation('1590c1ee-5938-41db-b49d-57b39d6cc668', constitutional_text__legislative_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('1590c1ee-5938-41db-b49d-57b39d6cc668', constitutional_text__popular_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('1590c1ee-5938-41db-b49d-57b39d6cc668', foundational, judicial_supremacy_finality).
narrative_ontology:cs_axiom_status(judicial_supremacy_finality, holdable).
narrative_ontology:cs_axiom_grounding('1590c1ee-5938-41db-b49d-57b39d6cc668', judicial_supremacy_finality, conventional).
narrative_ontology:cs_axiom('1590c1ee-5938-41db-b49d-57b39d6cc668', foundational, constitutional_rights_entrenched_against_majorities).
narrative_ontology:cs_axiom_status(constitutional_rights_entrenched_against_majorities, holdable).
narrative_ontology:cs_axiom_grounding('1590c1ee-5938-41db-b49d-57b39d6cc668', constitutional_rights_entrenched_against_majorities, deontological).
narrative_ontology:cs_reference_frame('1590c1ee-5938-41db-b49d-57b39d6cc668', founding_judicial_guardianship).
narrative_ontology:cs_drift_state('1590c1ee-5938-41db-b49d-57b39d6cc668', contemporary_rights_expansion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1590c1ee-5938-41db-b49d-57b39d6cc668', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(constitutional_text__judicial_supremacy_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__judicial_supremacy_reading, rights_claimants).
narrative_ontology:constraint_beneficiary(constitutional_text__judicial_supremacy_reading, constitutional_court).
narrative_ontology:constraint_victim(constitutional_text__judicial_supremacy_reading, democratic_responsiveness).
narrative_ontology:constraint_victim(constitutional_text__judicial_supremacy_reading, legislative_majorities).
narrative_ontology:constraint_vindicates(constitutional_text__judicial_supremacy_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(constitutional_text__judicial_supremacy_reading, judicial_review_as_constitutional_necessity).
narrative_ontology:constraint_vindicates(constitutional_text__judicial_supremacy_reading, rights_as_trumps_against_majorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercises final interpretive authority over constitutional text; invalidates legislation that conflicts with its interpretation; sets the terms of constitutional debate; collects institutional authority and legitimacy as the constitutional guardian. Exit is analytical — the court cannot exit its own role without dissolving the constraint.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, constitutional_court, agenda_setter,
    institutional, generational, analytical, national).

% Gain enforceable constitutional protections against legislative majorities; rely on courts to vindicate rights claims that legislatures would not protect. Their exit is constrained — they depend on the court's authority and have no alternative enforcement mechanism for constitutional rights.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, rights_claimants, beneficiary,
    organized, biographical, constrained, national).

% Lose final say on constitutional meaning; have enactments invalidated by courts; must anticipate judicial reaction when legislating. Their exit is constrained — they cannot override judicial interpretations without constitutional amendment (which courts may also interpret) or court-packing (which undermines the constraint's legitimacy).
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, legislative_majorities, payer,
    powerful, biographical, constrained, national).

% The systemic capacity of democratic politics to adjust constitutional meaning through ordinary legislative politics is extracted. Constitutional change is channeled through courts or supermajoritarian amendment processes, making democratic self-correction slow and difficult. Exit is constrained — the constraint structures the entire democratic game.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, democratic_responsiveness, payer,
    organized, generational, constrained, national).

% Argue that constituent power of the people is the true source of constitutional authority; neither courts nor legislature should be supreme. They are structurally excluded from the constraint's operation — the judicial supremacy reading denies their interpretive authority claim. Exit is trapped — they must either accept judicial supremacy or seek revolutionary/constituent moments to reset the framework.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, popular_sovereignty_advocates, excluded,
    organized, generational, trapped, national).

% Analyze, critique, and theorize the constraint from outside its operation. They do not collect rents or bear costs from the constraint directly. Their exit is analytical — they can change their theoretical position without material consequence.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text__judicial_supremacy_reading, constitutional_court).
narrative_ontology:fixing_cost_class(constitutional_text__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides final authoritative interpretation of constitutional text, resolving disputes about rights and governmental powers without endless legislative ping-pong or constitutional instability; coordinates expectations about constitutional meaning across time and across branches.
% TRANSFER_FUNCTION: Moves final constitutional authority from legislative majorities to courts; rights_claimants gain enforceable protections against majoritarian overreach; democratic_responsiveness and legislative_majorities lose the power to determine constitutional meaning through ordinary democratic politics.
% ABSENT_VOICES: The constituent power (the people as original authors of the constitution) and future generations who inherit the interpretive framework are not present when judicial supremacy operates. They would object to the closure of popular interpretive authority but are structurally excluded from the courtroom where constitutional meaning is conclusively determined.
% DISAPPEARANCE_RATIONALE: If judicial supremacy vanished overnight, legislatures would reclaim final interpretive authority (legislative_sovereignty_reading) or constituent assemblies would be convened (popular_sovereignty_reading); rights protections would become politically contingent; the constitutional order would shift toward one of the sibling readings — the world rearranges because arrangements depend on this constraint.
% FOUNDING_PROBLEM: The problem of ensuring constitutional rights survive temporary majoritarian impulses; the founding generation feared legislative tyranny and created courts as guardians of the constitutional text against democratic overreach.
% FOUNDING_PROBLEM_CORROBORATION: The Federalist Papers (particularly Federalist 78, outside the judicial beneficiary set) attest the anti-majoritarian design rationale. Modern democratic theorists (Dahl, Waldron, Tushnet) and comparative constitutional scholars contest whether the founding problem persists in current form or whether the constraint has drifted into a rights-expansion mechanism beyond the founding design.
narrative_ontology:disappearance_verdict(constitutional_text__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__judicial_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_text__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text__judicial_supremacy_reading, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, ExtMetricName, E),
    domain_priors:suppression_score(constitutional_text__judicial_supremacy_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(constitutional_text__judicial_supremacy_reading),
    narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(constitutional_text__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects the systematic transfer of final constitutional authority from elected branches to courts — democratic majorities lose the power to determine constitutional meaning through ordinary politics. Suppression (0.80) is high because legislative override is structurally impossible under this reading; the constraint's persistence depends on active enforcement (judicial review) and the exclusion of alternative interpretive authorities. Theater ratio (0.30) is moderate-low: judicial review performs real rights-protection work, but a growing share of decisions extend beyond clear textual mandate into contested policy territory. Accessibility collapse (0.85) is high because once judicial supremacy is accepted, alternative interpretive frameworks (legislative, popular) become structurally invisible within the legal system. Resistance (0.50) is moderate: academic, political, and popular challenges exist but have not displaced the constraint in systems where it is entrenched.
 *
 * PERSPECTIVAL GAP:
 *   From the court's seat (agenda_setter, analytical exit), the constraint appears as genuine coordination — the court resolves constitutional disputes authoritatively, preventing interpretive chaos. From the legislative_majorities seat (payer, constrained exit), the same structure operates as extraction — their constitutional judgments are overridden by an unelected body. From the rights_claimants seat (beneficiary, constrained exit), the constraint appears as essential protection — without it, rights are politically contingent. The engine computes this divergence from the structural data; the authored claim (mountain) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The constitutional_court is the agenda_setter (institutional, generational horizon, analytical exit) — it administers the constraint and collects institutional authority. Rights_claimants are beneficiaries (organized, biographical, constrained exit) — they gain enforceable protections against majorities. Legislative_majorities are payers (powerful, biographical, constrained exit) — they bear the cost of having their enactments invalidated. Democratic_responsiveness is a payer (organized, generational, constrained exit) — the systemic capacity of democracy to self-correct through constitutional interpretation is extracted. Popular_sovereignty_advocates are excluded (organized, generational, trapped) — their interpretive authority claim is structurally barred. Legal_scholars are observers (analytical, civilizational, analytical exit) — they analyze but do not participate in the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting rights from temporary majoritarian impulses) remains live but contested. The constraint persists beyond its founding justification because courts have expanded the scope of 'rights' and 'constitutional meaning' far beyond the founding generation's understanding — a practice drift that the authority structure does not acknowledge. This is not pure mandatrophy (the founding problem is not dead) but drift-driven expansion of the constraint's domain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_supremacy,
    'Is judicial supremacy a genuine natural law of constitutional order (mountain) or a constructed constraint that benefits courts and rights-claimants at the expense of democratic responsiveness (false summit)?',
    'Comparative analysis of constitutional systems without judicial supremacy (UK parliamentary sovereignty, NZ before 1990, Switzerland) — if rights protection persists without judicial finality, the mountain claim weakens; if rights systematically degrade, the coordination function is empirically necessary.',
    'If constructed, the constraint reclassifies from mountain to tangled_rope via FSM; the beneficiary structure (courts + rights_claimants) and victim structure (democratic_responsiveness) become the primary classification signal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_supremacy, empirical, 'Whether judicial supremacy is a natural constitutional necessity or a constructed arrangement with identifiable beneficiaries and victims.').

omega_variable(
    kernel_reading_frame_ambiguity,
    'Does the constitutional text itself resolve the interpretive authority question, or does the judicial supremacy reading import a premise not in the text?',
    'Textual analysis of constitutional provisions on judicial power across multiple constitutions; historical analysis of founding-era understanding of judicial role; comparative analysis of how sibling readings ground their authority claims in the same text.',
    'If the text is genuinely indeterminate, the reading''s claimed mountain status (emerges_naturally) is undermined — the constraint is a reading imposed on the text, not derived from it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_frame_ambiguity, conceptual, 'Whether the judicial supremacy reading''s authority claim is textually grounded or reading-imposed.').

omega_variable(
    coordination_necessity_of_judicial_finality,
    'Is judicial finality structurally necessary for the coordination function (rights protection against majoritarian overreach), or could legislative or popular mechanisms achieve the same coordination with less extraction from democratic responsiveness?',
    'Counterfactual analysis of rights protection in systems with legislative override (Canada s.33, UK Human Rights Act, Israel pre-1992); empirical study of rights outcomes under different interpretive authority allocations.',
    'If judicial finality is not necessary for coordination, the constraint''s extractive component (suppression of democratic responsiveness) lacks coordination justification — shifts classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_necessity_of_judicial_finality, empirical, 'Whether the coordination function genuinely requires judicial supremacy or merely uses it as a convenient mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__judicial_supremacy_reading, 1789, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(constitutional_text__judicial_supremacy_reading_tr_t1789, constitutional_text__judicial_supremacy_reading, theater_ratio, 1789, 0.1).
narrative_ontology:measurement(constitutional_text__judicial_supremacy_reading_tr_t1850, constitutional_text__judicial_supremacy_reading, theater_ratio, 1850, 0.12).
narrative_ontology:measurement(constitutional_text__judicial_supremacy_reading_tr_t1900, constitutional_text__judicial_supremacy_reading, theater_ratio, 1900, 0.18).
narrative_ontology:measurement(constitutional_text__judicial_supremacy_reading_tr_t1954, constitutional_text__judicial_supremacy_reading, theater_ratio, 1954, 0.25).
narrative_ontology:measurement(constitutional_text__judicial_supremacy_reading_tr_t1973, constitutional_text__judicial_supremacy_reading, theater_ratio, 1973, 0.28).
narrative_ontology:measurement(constitutional_text__judicial_supremacy_reading_tr_t2000, constitutional_text__judicial_supremacy_reading, theater_ratio, 2000, 0.29).
narrative_ontology:measurement(constitutional_text__judicial_supremacy_reading_tr_t2024, constitutional_text__judicial_supremacy_reading, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(constitutional_text__judicial_supremacy_reading_be_t1789, constitutional_text__judicial_supremacy_reading, base_extractiveness, 1789, 0.35).
narrative_ontology:measurement(constitutional_text__judicial_supremacy_reading_be_t1850, constitutional_text__judicial_supremacy_reading, base_extractiveness, 1850, 0.4).
narrative_ontology:measurement(constitutional_text__judicial_supremacy_reading_be_t1900, constitutional_text__judicial_supremacy_reading, base_extractiveness, 1900, 0.48).
narrative_ontology:measurement(constitutional_text__judicial_supremacy_reading_be_t1954, constitutional_text__judicial_supremacy_reading, base_extractiveness, 1954, 0.58).
narrative_ontology:measurement(constitutional_text__judicial_supremacy_reading_be_t1973, constitutional_text__judicial_supremacy_reading, base_extractiveness, 1973, 0.62).
narrative_ontology:measurement(constitutional_text__judicial_supremacy_reading_be_t2000, constitutional_text__judicial_supremacy_reading, base_extractiveness, 2000, 0.64).
narrative_ontology:measurement(constitutional_text__judicial_supremacy_reading_be_t2024, constitutional_text__judicial_supremacy_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(constitutional_text__judicial_supremacy_reading_su_t1789, constitutional_text__judicial_supremacy_reading, suppression_requirement, 1789, 0.55).
narrative_ontology:measurement(constitutional_text__judicial_supremacy_reading_su_t1850, constitutional_text__judicial_supremacy_reading, suppression_requirement, 1850, 0.6).
narrative_ontology:measurement(constitutional_text__judicial_supremacy_reading_su_t1900, constitutional_text__judicial_supremacy_reading, suppression_requirement, 1900, 0.7).
narrative_ontology:measurement(constitutional_text__judicial_supremacy_reading_su_t1954, constitutional_text__judicial_supremacy_reading, suppression_requirement, 1954, 0.75).
narrative_ontology:measurement(constitutional_text__judicial_supremacy_reading_su_t1973, constitutional_text__judicial_supremacy_reading, suppression_requirement, 1973, 0.78).
narrative_ontology:measurement(constitutional_text__judicial_supremacy_reading_su_t2000, constitutional_text__judicial_supremacy_reading, suppression_requirement, 2000, 0.79).
narrative_ontology:measurement(constitutional_text__judicial_supremacy_reading_su_t2024, constitutional_text__judicial_supremacy_reading, suppression_requirement, 2024, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_text__judicial_supremacy_reading, 0.1).
narrative_ontology:affects_constraint(constitutional_text__judicial_supremacy_reading, constitutional_text__legislative_sovereignty_reading).
narrative_ontology:affects_constraint(constitutional_text__judicial_supremacy_reading, constitutional_text__popular_sovereignty_reading).

% DUAL FORMULATION NOTE:
% BGS-pattern decomposition: constitutional_text kernel decomposes into three constraint stories. Judicial supremacy reading claims mountain status but metric profile shows extraction from democratic responsiveness. Legislative sovereignty and popular sovereignty readings are separate constraints with their own ε values and stakeholder structures. This reading structurally influences both siblings by occupying the interpretive high ground — its claims set the terms of debate for the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_text__judicial_supremacy_reading, institutional, 0.1).
constraint_indexing:directionality_override(constitutional_text__judicial_supremacy_reading, organized, 0.35).
constraint_indexing:directionality_override(constitutional_text__judicial_supremacy_reading, powerful, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
