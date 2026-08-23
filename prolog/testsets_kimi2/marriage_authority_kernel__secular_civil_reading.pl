% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__secular_civil_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-10-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__secular_civil_reading, []).

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
 *   constraint_id: marriage_authority_kernel__secular_civil_reading
 *   human_readable: Secular Civil Marriage Authority (Special Marriage Act 1954)
 *   domain: comparative law / constitutional pluralism / religious governance
 *
 * SUMMARY:
 *   This constraint instantiates the secular civil reading of the marriage
 *   authority kernel in Indian law: marriage and family law authority derives
 *   from the Special Marriage Act 1954 and the constitutional framework of
 *   individual rights, administered by civil courts. It is one of five live
 *   readings of the kernel, coexisting alongside Hindu codified law, Muslim
 *   Shariat, Christian canonical law, and Parsi communal law. The secular
 *   reading enables inter-religious marriage and claims the highest
 *   gender-equity standards, but imposes significant social costs on couples
 *   who exit community law to access it. The authored metrics and claimed
 *   type are independent: the constraint is claimed as tangled_rope because
 *   it combines genuine coordination (interfaith legal recognition) with
 *   asymmetric extraction (procedural exposure and social ostracism), while
 *   the metrics describe its actual operation.
 *
 * KEY AGENTS:
 *   - Union legislature and judiciary (agenda_setter, institutional): sets and enforces the SMA and constitutional family-law framework.
 *   - Interfaith couples (payer/beneficiary, moderate/identity_locked): receive legal recognition but bear social costs and harassment risks.
 *   - Religious personal law boards (excluded, organized): displaced from jurisdiction over members who opt out.
 *   - Constitutional scholars (observer, analytical): monitor the tension between secularism and pluralism.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__secular_civil_reading, 0.48).
domain_priors:suppression_score(marriage_authority_kernel__secular_civil_reading, 0.52).
domain_priors:theater_ratio(marriage_authority_kernel__secular_civil_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__secular_civil_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__secular_civil_reading, "Secular Civil Marriage Authority (Special Marriage Act 1954)").
narrative_ontology:topic_domain(marriage_authority_kernel__secular_civil_reading, "comparative law / constitutional pluralism / religious governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__secular_civil_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__secular_civil_reading, '28d12b54-c81c-47d8-a8d3-b1058a186339').
narrative_ontology:cs_kernel_codification('28d12b54-c81c-47d8-a8d3-b1058a186339', formalized).
narrative_ontology:cs_authority_grounding('28d12b54-c81c-47d8-a8d3-b1058a186339', lineage).
narrative_ontology:cs_interpretation_layer_present('28d12b54-c81c-47d8-a8d3-b1058a186339').
narrative_ontology:cs_reading_relation('28d12b54-c81c-47d8-a8d3-b1058a186339', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('28d12b54-c81c-47d8-a8d3-b1058a186339', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('28d12b54-c81c-47d8-a8d3-b1058a186339', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('28d12b54-c81c-47d8-a8d3-b1058a186339', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_axiom('28d12b54-c81c-47d8-a8d3-b1058a186339', foundational, individual_marriage_liberty).
narrative_ontology:cs_axiom_status(individual_marriage_liberty, holdable).
narrative_ontology:cs_axiom_grounding('28d12b54-c81c-47d8-a8d3-b1058a186339', individual_marriage_liberty, deontological).
narrative_ontology:cs_axiom('28d12b54-c81c-47d8-a8d3-b1058a186339', foundational, secular_state_neutrality).
narrative_ontology:cs_axiom_status(secular_state_neutrality, holdable).
narrative_ontology:cs_axiom_grounding('28d12b54-c81c-47d8-a8d3-b1058a186339', secular_state_neutrality, conventional).
narrative_ontology:cs_reference_frame('28d12b54-c81c-47d8-a8d3-b1058a186339', constitutional_individual_rights_framework).
narrative_ontology:cs_drift_state('28d12b54-c81c-47d8-a8d3-b1058a186339', contemporary_personal_law_practice, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('28d12b54-c81c-47d8-a8d3-b1058a186339', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, interfaith_couples).
narrative_ontology:constraint_victim(marriage_authority_kernel__secular_civil_reading, interfaith_couples).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts and interprets the Special Marriage Act and constitutional provisions governing family law. Sets procedural rules including mandatory notice periods and objection mechanisms. Embedded in a constitutional structure that directs a uniform civil code, making exit from this role politically and legally difficult.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, union_legislature_and_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Use the Special Marriage Act to obtain legal recognition for marriages across religious boundaries. Receive state-backed marriage certificates and relatively gender-equitable divorce remedies, but must publish a public notice of intended marriage, exposing them to community harassment, ostracism, and vigilante violence. Exit from the constraint means returning to personal law (often requiring religious conversion) or forgoing legal marriage entirely, both of which carry heavy social and legal costs.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, interfaith_couples, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__secular_civil_reading, interfaith_couples, beneficiary).

% Lose de facto jurisdiction over family matters for members who opt into the secular civil code. Their doctrinal interpretations of valid marriage, divorce, and inheritance are displaced by civil court orders. They are not parties to SMA proceedings and their objections are heard only through political or public-interest channels outside the adjudicative room.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, religious_personal_law_boards, excluded,
    organized, generational, constrained, national).

% Analyze the tension between constitutional secularism and plural personal law systems. Track judicial decisions and legislative debates to assess whether the SMA functions as a rights-protective coordination mechanism or as a vector for majoritarian pressure against minority legal traditions.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal mechanism enabling couples from different religious communities to marry and divorce without converting, and establishes civil courts as neutral arbiters of family disputes outside the doctrinal authority of any single religion.
% TRANSFER_FUNCTION: Moves jurisdictional authority over marriage and family matters from religious personal law systems to civil courts; moves social legitimacy from communal endorsement to state registration, while the couple bears the social cost of exit from their community.
% ABSENT_VOICES: Religious personal law boards and conservative communal leaders who regard marriage as inherently sacred and community-governed are structurally excluded from SMA proceedings; their objections enter only through political pressure or public-interest litigation, not through the constraint's own process.
% DISAPPEARANCE_RATIONALE: If the secular civil authority vanished overnight, interfaith couples would lose their primary legal pathway to marry without converting, civil courts would cede a domain of family law back to religious authorities, and the plural legal landscape would reconfigure around exclusive communal boundaries, forcing couples into religious conversion or extralegal unions.
% FOUNDING_PROBLEM: Post-colonial India needed a marriage law for citizens who did not belong to a single religious community or who rejected religious governance, without forcing them to convert or remain legally unrecognized.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional framers and subsequent Supreme Court jurisprudence (e.g., Lata Singh, Shakti Vahini) attest the ongoing need for a secular marriage pathway from outside the benefiting parties; however, communal authorities and some political parties contest that the problem is better solved by conversion and assimilation into personal law rather than secular exit.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__secular_civil_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__secular_civil_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__secular_civil_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority_kernel__secular_civil_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__secular_civil_reading, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__secular_civil_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority_kernel__secular_civil_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority_kernel__secular_civil_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48) is moderate because the constraint genuinely solves a coordination problem for interfaith couples, but the mandatory public notice procedure and the social ostracism it triggers extract heavily from the very agents it coordinates. Suppression (0.52) reflects both the procedural suppression of privacy and the communal suppression of exit. Theater ratio (0.25) is relatively low because civil courts are functionally adjudicating, though the notice procedure has acquired performative dimensions that enable harassment. Accessibility collapse (0.45) is partial: personal law alternatives remain available to others, but for the couple that opts in, community law paths collapse. Resistance (0.55) captures organized communal and vigilante opposition to interfaith unions. Temporal measurements show a slow rise in extractiveness and suppression as political majoritarianism has intensified the social costs of secular exit.
 *
 * PERSPECTIVAL GAP:
 *   From the state's seat, the SMA is a rights-protective coordination mechanism that resolves the problem of interfaith marriage without conversion. From the couple's seat, the same framework is a hazardous gateway: the notice requirement exposes them to community violence, and the legal benefit is purchased at the price of identity-based exile. The engine computes this divergence from the structural asymmetry in power, exit options, and cost-bearing.
 *
 * DIRECTIONALITY LOGIC:
 *   The union legislature and judiciary sit near the beneficiary end as agenda-setters who expand state jurisdiction and constitutional governance. Interfaith couples are declared in both beneficiary and victim arrays, but their structural situationâidentity_locked exit, biographical time horizon, and payer roleâpushes their derived directionality toward the target end, reflecting that the constraint extracts social costs from them even as it coordinates legal recognition. Religious personal law boards are excluded from the process but bear a diffuse cost of displaced authority.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâproviding a secular marriage pathway for Indians outside communal boundariesâremains live, as evidenced by ongoing judicial reliance on the SMA and political demand for a uniform civil code. There is no mandatrophy: the constraint has not outlived its function. However, the notice procedure may be experiencing functional drift, acquiring an extractive suppression role that was not part of the original design.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_future_status,
    'Does the secular civil reading of marriage authority foreclose communal readings in a future unified legal framework, or will it remain an optional alternative indefinitely?',
    'Legislative adoption or rejection of a Uniform Civil Code; judicial doctrine on the primacy of constitutional rights over personal law claims.',
    'If a UCC is enacted, this reading would likely become dominant or sole framework, shifting relations toward forecloses for some siblings. If rejected, the reading remains a minority alternative and its extractiveness on exiters may deepen due to stigmatization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_future_status, conceptual, 'Committing-frame uncertainty about whether this reading is transitional or terminal').

omega_variable(
    suppression_source_ambiguity,
    'Are the social costs of exiting community law structurally produced by the SMA''s mandatory notice and transparency procedures, or are they produced by communal enforcement independent of the legal framework?',
    'Comparative analysis of jurisdictions with confidential civil marriage registries versus public notice regimes; measurement of harassment incidents pre- and post-filing under SMA notice requirements.',
    'If the SMA''s own procedure is the primary suppression mechanism, the constraint is more extractive than a neutral coordination framework; if suppression is purely external, the constraint''s extraction score should be discounted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_source_ambiguity, empirical, 'Whether legal procedure or communal action generates the suppression').

omega_variable(
    constitutional_rights_epistemic_status,
    'Are constitutional individual rights in family law a constructed legal-political arrangement that benefits the secular state, or do they represent a natural-law baseline that personal law systems deviate from?',
    'Cross-cultural comparative jurisprudence; analysis of whether rights-based family law produces measurably different equity outcomes than communitarian systems.',
    'If purely constructed, the constraint''s legitimacy depends on state power and its classification as tangled_rope is strengthened; if natural-law, the constraint trends toward rope with lower extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(constitutional_rights_epistemic_status, conceptual, 'Epistemic status of the rights grounding').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__secular_civil_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority_kernel__secular_civil_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(marr_tr_t14, marriage_authority_kernel__secular_civil_reading, theater_ratio, 14, 0.12).
narrative_ontology:measurement(marr_tr_t28, marriage_authority_kernel__secular_civil_reading, theater_ratio, 28, 0.15).
narrative_ontology:measurement(marr_tr_t42, marriage_authority_kernel__secular_civil_reading, theater_ratio, 42, 0.2).
narrative_ontology:measurement(marr_tr_t56, marriage_authority_kernel__secular_civil_reading, theater_ratio, 56, 0.22).
narrative_ontology:measurement(marr_tr_t70, marriage_authority_kernel__secular_civil_reading, theater_ratio, 70, 0.25).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(marr_be_t14, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 14, 0.28).
narrative_ontology:measurement(marr_be_t28, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 28, 0.34).
narrative_ontology:measurement(marr_be_t42, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 42, 0.4).
narrative_ontology:measurement(marr_be_t56, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 56, 0.44).
narrative_ontology:measurement(marr_be_t70, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 70, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(marr_su_t14, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 14, 0.4).
narrative_ontology:measurement(marr_su_t28, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 28, 0.45).
narrative_ontology:measurement(marr_su_t42, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 42, 0.48).
narrative_ontology:measurement(marr_su_t56, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 56, 0.5).
narrative_ontology:measurement(marr_su_t70, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 70, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__secular_civil_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
