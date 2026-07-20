% ============================================================================
% CONSTRAINT STORY: fifth_republic_constitution__hyper_presidential_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fifth_republic_constitution__hyper_presidential_reading, []).

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
 *   constraint_id: fifth_republic_constitution__hyper_presidential_reading
 *   human_readable: Fifth Republic Constitution â Hyper-Presidential Reading
 *   domain: constitutional law / political systems / comparative government
 *
 * SUMMARY:
 *   This constraint story captures the HYPER-PRESIDENTIAL READING of the
 *   French Fifth Republic Constitution (kernel_id:
 *   fifth_republic_constitution). In this reading, the president is the
 *   direct sovereign embodying national will, and the legislature is
 *   minimally constrained. The constitutional mechanismsâespecially Article
 *   49.3 (forcing legislation without vote) and Article 16 (emergency
 *   powers)âoperate to transfer deliberative and initiative capacity from
 *   the National Assembly to the executive presidency. The story is authored
 *   as a tangled_rope: the Fifth Republic genuinely coordinates governmental
 *   stability (solving the Fourth Republic's paralysis), but the
 *   hyper-presidential reading asymmetrically extracts from the legislature
 *   to benefit the presidency.
 *
 * KEY AGENTS:
 *   - executive_presidency: Primary beneficiary and agenda-setter (institutional/generational) â collects concentrated constitutional authority and administers the bypass mechanisms.
 *   - incumbent_president: Primary beneficiary (powerful/biographical) â personally exercises the extracted executive dominance.
 *   - national_assembly: Primary target (organized/constrained) â bears the loss of legislative initiative and deliberative capacity.
 *   - parliamentary_opposition: Secondary target (powerless/constrained) â structurally sidelined when presidential majority controls the assembly.
 *   - prime_minister_office: Intermediate payer (moderate/constrained) â implements presidential will and absorbs parliamentary hostility.
 *   - constitutional_council: Analytical observer (institutional/analytical) â reviews constitutionality but often defers to executive interpretation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__hyper_presidential_reading, 0.78).
domain_priors:suppression_score(fifth_republic_constitution__hyper_presidential_reading, 0.72).
domain_priors:theater_ratio(fifth_republic_constitution__hyper_presidential_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__hyper_presidential_reading, tangled_rope).
narrative_ontology:human_readable(fifth_republic_constitution__hyper_presidential_reading, "Fifth Republic Constitution â Hyper-Presidential Reading").
narrative_ontology:topic_domain(fifth_republic_constitution__hyper_presidential_reading, "constitutional law / political systems / comparative government").

domain_priors:requires_active_enforcement(fifth_republic_constitution__hyper_presidential_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__hyper_presidential_reading, 'dbc22cda-bf2d-4225-aa75-8d5570b94dc4').
narrative_ontology:cs_kernel_codification('dbc22cda-bf2d-4225-aa75-8d5570b94dc4', fixed_text).
narrative_ontology:cs_authority_grounding('dbc22cda-bf2d-4225-aa75-8d5570b94dc4', lineage).
narrative_ontology:cs_interpretation_layer_present('dbc22cda-bf2d-4225-aa75-8d5570b94dc4').
narrative_ontology:cs_reading_relation('dbc22cda-bf2d-4225-aa75-8d5570b94dc4', fifth_republic_constitution__parliamentary_constraint_reading, forecloses).
narrative_ontology:cs_reading_relation('dbc22cda-bf2d-4225-aa75-8d5570b94dc4', fifth_republic_constitution__cohabitation_equilibrium_reading, coexists_with).
narrative_ontology:cs_axiom('dbc22cda-bf2d-4225-aa75-8d5570b94dc4', foundational, president_embodies_national_will).
narrative_ontology:cs_axiom_status(president_embodies_national_will, holdable).
narrative_ontology:cs_axiom_grounding('dbc22cda-bf2d-4225-aa75-8d5570b94dc4', president_embodies_national_will, conventional).
narrative_ontology:cs_axiom('dbc22cda-bf2d-4225-aa75-8d5570b94dc4', foundational, legislative_constraint_exceptional_only).
narrative_ontology:cs_axiom_status(legislative_constraint_exceptional_only, holdable).
narrative_ontology:cs_axiom_grounding('dbc22cda-bf2d-4225-aa75-8d5570b94dc4', legislative_constraint_exceptional_only, conventional).
narrative_ontology:cs_reference_frame('dbc22cda-bf2d-4225-aa75-8d5570b94dc4', gaullist_executive_supremacy).
narrative_ontology:cs_drift_state('dbc22cda-bf2d-4225-aa75-8d5570b94dc4', contemporary_fifth_republic, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('dbc22cda-bf2d-4225-aa75-8d5570b94dc4', '').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__hyper_presidential_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__hyper_presidential_reading, executive_presidency).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__hyper_presidential_reading, incumbent_president).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, national_assembly).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, parliamentary_opposition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, prime_minister_office).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The institution of the French presidency as constructed by the 1958 Constitution and interpreted through the hyper-presidential lens. It sets the political agenda, dominates the legislative process through Article 49.3 and Article 16, and structures the executive branch around the president's will. Its exit from this arrangement is constrained by the constitutional text itself, which it simultaneously administers and embodies.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, executive_presidency, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__hyper_presidential_reading, executive_presidency, beneficiary).

% The individual office-holder who benefits from the concentrated authority of the presidency. Elected by direct universal suffrage, the incumbent claims democratic legitimacy as the embodiment of national will. Can leave office through electoral defeat or term limits, but while in office operates with minimal legislative constraint.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, incumbent_president, beneficiary,
    powerful, biographical, mobile, national).

% The lower house of the French Parliament, which under this constitutional reading loses its classical legislative supremacy. It is bypassed through Article 49.3 (forcing passage of legislation without a vote) and its oversight function is neutralized when the president commands a disciplined majority. Dissolution by the president is the ultimate structural constraint on its exit.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, national_assembly, payer,
    organized, biographical, constrained, national).

% Deputies and parliamentary groups that do not belong to the presidential majority. They are structurally sidelined by the executive's control of the parliamentary majority and by constitutional mechanisms that allow the government to override opposition. Their capacity to block or amend legislation is minimal when the president holds a majority.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, parliamentary_opposition, payer,
    powerless, biographical, constrained, national).

% The Prime Minister and cabinet, who in the hyper-presidential reading implement the president's will rather than exercise independent executive authority. They absorb parliamentary hostility and administrative responsibility while the president remains above the fray. Their political survival depends on presidential favor, not parliamentary confidence alone.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, prime_minister_office, payer,
    moderate, biographical, constrained, national).

% The Conseil Constitutionnel, which reviews the constitutionality of legislation. In the hyper-presidential tradition, it often defers to executive interpretation of broad constitutional principles (notably Article 16) and has historically validated rather than blocked the expansion of presidential power.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, constitutional_council, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves governmental instability and executive fragmentation by concentrating authority in a single elected figure (the president) with a direct popular mandate, replacing the multiparty parliamentary coalitions and rapid cabinet turnover of the Fourth Republic with a unified chain of command.
% TRANSFER_FUNCTION: Transfers legislative deliberative authority, policy initiative, and oversight capacity from the National Assembly and parliamentary opposition to the executive presidency and incumbent president, primarily through Article 49.3 (forcing legislation without vote) and Article 16 (emergency full powers).
% ABSENT_VOICES: Parliamentary sovereignty advocates and minority parties outside the presidential majority are present in the assembly but structurally excluded from effective legislative power. Constitutional jurists arguing for a balanced reading of the 1958 text are marginalized in the dominant Gaullist interpretive tradition.
% DISAPPEARANCE_RATIONALE: If the hyper-presidential reading were displaced and the legislature regained genuine initiative and veto authority, French policy-making would reorganize around parliamentary negotiation, coalition-building, and legislative deliberation rather than executive decree. The presidency would revert to a more symbolic or coordinating role, and the current beneficiaries would lose their dominant agenda-setting position.
% FOUNDING_PROBLEM: The Fourth Republic's governmental paralysis caused by shifting parliamentary coalitions, the absence of stable executive leadership, and the inability to decide or act decisively on colonial and economic crises.
% FOUNDING_PROBLEM_CORROBORATION: Political historians corroborate that the Fourth Republic suffered from cabinet instability. However, corroboration from outside the Gaullist beneficiary tradition (e.g., parliamentary party leaders, constitutional comparatists) contests whether this required hyper-presidential supremacy rather than a stronger parliamentary executive. The beneficiary party (presidential institutions) asserts the problem remains live to justify ongoing concentration.
narrative_ontology:disappearance_verdict(fifth_republic_constitution__hyper_presidential_reading, world_rearranges).
narrative_ontology:founding_problem_status(fifth_republic_constitution__hyper_presidential_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__hyper_presidential_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fifth_republic_constitution__hyper_presidential_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fifth_republic_constitution__hyper_presidential_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fifth_republic_constitution__hyper_presidential_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fifth_republic_constitution__hyper_presidential_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fifth_republic_constitution__hyper_presidential_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.78) because the hyper-presidential reading structurally bypasses the legislature through 49.3 and 16, decoupling executive action from deliberative constraint. Suppression is high (0.72) because the constraint persists through active constitutional enforcement that excludes legislative alternatives (no-vote passage, emergency powers). Theater ratio is moderate-low (0.25): the executive supremacy is largely functional rather than performative, though some republican ritual conceals the extraction. Accessibility collapse (0.60) reflects that once the 49.3 mechanism is invoked, legislative alternatives collapse quickly. Resistance (0.55) captures recurring parliamentary and social opposition (e.g., 2023 pension reform protests) that meets the constraint but rarely stops it. The temporal series show a cyclical pattern: peaks under unified presidential majorities (de Gaulle, Sarkozy, Macron) and troughs during cohabitation (late Mitterrand/Chirac, Jospin), demonstrating that extraction oscillates with partisan alignment rather than drifting monotonically.
 *
 * PERSPECTIVAL GAP:
 *   The presidential seat experiences this constraint as legitimate coordination: the president receives a direct popular mandate and uses constitutional tools to overcome legislative obstruction. The legislative seat experiences the same structure as extraction: its deliberative and veto capacities are systematically bypassed. The engine computes this divergence from the structural data (beneficiary vs. victim declarations, differentiated exit options). The same-level actor dynamic is visible within the legislature: the majority faction often supports the bypass because it shares the president's party label, while the opposition faction (same institutional level, same formal power) is trapped because it lacks majority status. Party affiliationânot institutional positionâdifferentiates their exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The executive_presidency and incumbent_president are declared beneficiaries: they collect the extracted authority (low directionality, subsidy). The national_assembly and parliamentary_opposition are declared victims: they bear the cost of bypass and suppression (high directionality, amplified extraction). The prime_minister_office is an intermediate payer: structurally subordinate, implementing presidential will while facing parliamentary censure. No directionality overrides are needed because the structural derivation from beneficiary/victim declarations plus exit options (presidency mobile/constrained vs. parliament constrained/trapped) correctly maps the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâFourth Republic governmental instabilityâis corroborated as historically real, but contested as requiring THIS solution. The mismatch between founding_problem_status (contested) and disappearance_verdict (world_rearranges) flags that the arrangement may have outlived its original justification. However, because the coordination function (governmental stability) remains partially live and the constraint is actively enforced with clear beneficiaries, the classification is tangled_rope rather than piton. A piton reading would require the presidency to no longer benefit enough to maintain the arrangement and the legislature to be too weak to fix itâhere, the presidency actively benefits and enforces, so it is not inertial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_sibling_delta,
    'This constraint is the hyper_presidential_reading of kernel fifth_republic_constitution. How would the structural classification change if the parliamentary_constraint_reading or cohabitation_equilibrium_reading were adopted instead?',
    'Comparative analysis of the same constitutional text under different interpretive frames: the parliamentary reading would remove the legislature from the victim set and reduce executive extractiveness; the cohabitation reading would split the beneficiary seat between president and prime minister.',
    'Adopting a sibling reading would reclassify the constraint toward rope or tangled_rope with symmetric coordination rather than asymmetric extraction, or toward a dual-executive scaffold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_sibling_delta, conceptual, 'Committing omega for kernel reading location and sibling deltas.').

omega_variable(
    cohabitation_anomaly_status,
    'Does the empirical occurrence of cohabitation (1986-1988, 1993-1995, 1997-2002) represent a temporary aberration compatible with the hyper-presidential reading, or a constitutional norm that falsifies it?',
    'Examination of constitutional practice and jurisprudence during cohabitation periods: if institutional behavior normalized power-sharing as a stable constitutional state, the hyper-presidential reading is descriptively incomplete; if cohabitation was treated as an exceptional suspension by all parties, the reading survives.',
    'If cohabitation is a norm, the constraint''s extractiveness is lower than the hyper-presidential reading claims, and the correct classification may be a cyclical tangled_rope rather than a steady-state snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cohabitation_anomaly_status, empirical, 'Whether cohabitation falsifies or merely suspends hyper-presidential supremacy.').

omega_variable(
    constitutional_bypass_legitimacy,
    'Are Article 49.3 and Article 16 legitimate constitutional tools of republican efficiency, or extractive mechanisms that suppress democratic deliberation?',
    'Comparative constitutional analysis of how other republics handle legislative gridlock without Article 49.3-style bypass, and empirical measurement of deliberative quality before and after 49.3 invocation.',
    'If the bypass is structurally unnecessary for coordination, the measured suppression is pure extraction and supports a snare classification; if it is the necessary price of governmental stability, extraction is coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_bypass_legitimacy, conceptual, 'Whether constitutional bypass mechanisms are extraction or coordination cost.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__hyper_presidential_reading, 0, 65).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fifth_rep_hyper_tr_t0, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(fifth_rep_hyper_tr_t8, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(fifth_rep_hyper_tr_t16, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement(fifth_rep_hyper_tr_t24, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 24, 0.15).
narrative_ontology:measurement(fifth_rep_hyper_tr_t32, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 32, 0.3).
narrative_ontology:measurement(fifth_rep_hyper_tr_t40, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement(fifth_rep_hyper_tr_t48, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 48, 0.22).
narrative_ontology:measurement(fifth_rep_hyper_tr_t56, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 56, 0.25).
narrative_ontology:measurement(fifth_rep_hyper_tr_t65, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 65, 0.25).

% Extraction over time
narrative_ontology:measurement(fifth_rep_hyper_be_t0, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(fifth_rep_hyper_be_t8, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 8, 0.68).
narrative_ontology:measurement(fifth_rep_hyper_be_t16, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(fifth_rep_hyper_be_t24, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement(fifth_rep_hyper_be_t32, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 32, 0.45).
narrative_ontology:measurement(fifth_rep_hyper_be_t40, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 40, 0.5).
narrative_ontology:measurement(fifth_rep_hyper_be_t48, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 48, 0.65).
narrative_ontology:measurement(fifth_rep_hyper_be_t56, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 56, 0.72).
narrative_ontology:measurement(fifth_rep_hyper_be_t65, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 65, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(fifth_rep_hyper_su_t0, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(fifth_rep_hyper_su_t8, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 8, 0.65).
narrative_ontology:measurement(fifth_rep_hyper_su_t16, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(fifth_rep_hyper_su_t24, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 24, 0.6).
narrative_ontology:measurement(fifth_rep_hyper_su_t32, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 32, 0.4).
narrative_ontology:measurement(fifth_rep_hyper_su_t40, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 40, 0.45).
narrative_ontology:measurement(fifth_rep_hyper_su_t48, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 48, 0.62).
narrative_ontology:measurement(fifth_rep_hyper_su_t56, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 56, 0.68).
narrative_ontology:measurement(fifth_rep_hyper_su_t65, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 65, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__hyper_presidential_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fifth_republic_constitution__hyper_presidential_reading, parliamentary_constraint_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__hyper_presidential_reading, cohabitation_equilibrium_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the fifth_republic_constitution kernel. The hyper-presidential reading extracts high executive power and treats the legislature as victim; sibling readings redistribute the same constitutional text into different beneficiary/victim structures with lower executive extractiveness.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
