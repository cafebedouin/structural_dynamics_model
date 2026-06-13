% ============================================================================
% CONSTRAINT STORY: marriage_authority__judicial_harmonization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__judicial_harmonization_reading, []).

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
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: marriage_authority__judicial_harmonization_reading
 *   human_readable: Judicial Harmonization of Marriage Authority via Constitutional Floor
 *   domain: legal_pluralism/constitutional_law
 *
 * SUMMARY:
 *   In countries with legal pluralism (India, Nigeria, Malaysia, Canada),
 *   marriage authority is fragmented across religious and community-based
 *   personal law codes that coexist with state civil law. The Supreme Court
 *   in such jurisdictions gradually imposes constitutional floors—gender
 *   equality, consent, property protection—via case-by-case judicial review
 *   without formally enacting a Uniform Civil Code. This reading describes
 *   the institutional mechanism of that judicial harmonization: the court as
 *   the agent, constitutional interpretation as the tool, incremental
 *   constraint-imposition as the process. The constraint is CLAIMED as
 *   scaffold (transitional, with a sunset condition: it ends when either
 *   formal unification occurs OR community codes voluntarily converge, OR
 *   political will for legislative replacement emerges). The extracted
 *   content is judicial authority itself: the court gains scope and
 *   legitimacy; religious councils lose exclusive autonomy; women in
 *   restrictive communities gain protection but at the cost of external
 *   intervention in internal affairs.
 *
 * KEY AGENTS:
 *   - Supreme Court: institutional agenda-setter, imposes constitutional floors via decided cases, collects authority and symbolic role as arbiter of fundamental rights
 *   - Religious community councils: organized payers, lose autonomous scope, constrained exit (identity-locked to tradition-keeping role)
 *   - Women in minority communities: powerless beneficiaries, gain substantive protections but remain trapped between identity and individual rights
 *   - Secular parliament: institutional excluded voice, advocates formal Uniform Civil Code rather than judicial piecemeal reform
 *   - Conservative personal law guardians: moderate payers, identity-locked, experience erosion of interpretive authority
 *   - Progressive constitutional advocates: powerful beneficiaries, use test-case litigation to advance gender equality through judicial authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__judicial_harmonization_reading, 0.58).
domain_priors:suppression_score(marriage_authority__judicial_harmonization_reading, 0.41).
domain_priors:theater_ratio(marriage_authority__judicial_harmonization_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__judicial_harmonization_reading, scaffold).
narrative_ontology:human_readable(marriage_authority__judicial_harmonization_reading, "Judicial Harmonization of Marriage Authority via Constitutional Floor").
narrative_ontology:topic_domain(marriage_authority__judicial_harmonization_reading, "legal_pluralism/constitutional_law").

domain_priors:requires_active_enforcement(marriage_authority__judicial_harmonization_reading).
narrative_ontology:has_sunset_clause(marriage_authority__judicial_harmonization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__judicial_harmonization_reading, '6969c109-c9f8-4fd7-92ec-31bd13c6c492').
narrative_ontology:cs_kernel_codification('6969c109-c9f8-4fd7-92ec-31bd13c6c492', distributed).
narrative_ontology:cs_authority_grounding('6969c109-c9f8-4fd7-92ec-31bd13c6c492', extraction).
narrative_ontology:cs_interpretation_layer_present('6969c109-c9f8-4fd7-92ec-31bd13c6c492').
narrative_ontology:cs_reading_relation('6969c109-c9f8-4fd7-92ec-31bd13c6c492', marriage_authority__communal_autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('6969c109-c9f8-4fd7-92ec-31bd13c6c492', marriage_authority__federalist_millet_reading, influences).
narrative_ontology:cs_reading_relation('6969c109-c9f8-4fd7-92ec-31bd13c6c492', marriage_authority__gender_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('6969c109-c9f8-4fd7-92ec-31bd13c6c492', marriage_authority__secularist_reading, influences).
narrative_ontology:cs_axiom('6969c109-c9f8-4fd7-92ec-31bd13c6c492', foundational, judicial_constitutional_interpretation_as_harmonization).
narrative_ontology:cs_axiom_status(judicial_constitutional_interpretation_as_harmonization, holdable).
narrative_ontology:cs_axiom_grounding('6969c109-c9f8-4fd7-92ec-31bd13c6c492', judicial_constitutional_interpretation_as_harmonization, instrumental).
narrative_ontology:cs_axiom('6969c109-c9f8-4fd7-92ec-31bd13c6c492', secondary, incremental_harmonization_without_formal_codification).
narrative_ontology:cs_axiom_status(incremental_harmonization_without_formal_codification, holdable).
narrative_ontology:cs_axiom_grounding('6969c109-c9f8-4fd7-92ec-31bd13c6c492', incremental_harmonization_without_formal_codification, instrumental).
narrative_ontology:cs_reference_frame('6969c109-c9f8-4fd7-92ec-31bd13c6c492', legal_pluralism_with_judicial_oversight).
narrative_ontology:cs_drift_state('6969c109-c9f8-4fd7-92ec-31bd13c6c492', contemporary_gender_equality_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('6969c109-c9f8-4fd7-92ec-31bd13c6c492', '').
narrative_ontology:cs_kernel_id(marriage_authority__judicial_harmonization_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__judicial_harmonization_reading, supreme_court_institution).
narrative_ontology:constraint_beneficiary(marriage_authority__judicial_harmonization_reading, gender_equality_advocates).
narrative_ontology:constraint_victim(marriage_authority__judicial_harmonization_reading, religious_community_councils).
narrative_ontology:constraint_victim(marriage_authority__judicial_harmonization_reading, conservative_personal_law_guardians).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__judicial_harmonization_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(marriage_authority__judicial_harmonization_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__judicial_harmonization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority__judicial_harmonization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority__judicial_harmonization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58 at interval end) is moderate-high because the court expands institutional scope and creates binding authority without legislative process, yet the constraint also accomplishes genuine reform (women's protections) that benefits real constituency. Suppression is moderate (0.41) because the constraint does not rely on coercive force—it operates through law's interpretive authority and the institutional obligation to follow precedent; resistance from religious communities is real but channeled through litigation and legislative advocacy rather than sustained active resistance. Theater ratio (0.48) is substantial and rising because the constraint's framing as 'constitutional enforcement' rather than law-making performs legitimacy work that allows the court to accomplish legislative-scale change without legislative authorization. The time series tracks the gradual expansion: 1950 is post-independence (personal law codes newly pluralized by constitutional pluralism); 1970 marks early landmark cases on women's property rights; 1990s see sustained constitutional expansion into gender discrimination; 2010s show theater and suppression requirement stabilizing as the court's role becomes normalized, but extractiveness plateaus as resistance hardens and the constraint reaches the limits of what case-by-case adjudication can accomplish without formal legislative backing.
 *
 * PERSPECTIVAL GAP:
 *   From the court's perspective, the constraint is legitimate constitutional enforcement and procedural protection of fundamental rights—the court is solving coordination failures in a plural system. From the religious council perspective, it is illegitimate power-seizure by judicial authority that should remain within legislative boundaries—the court is accomplishing undemocratic legal change. From women's perspective, it is genuine protection against oppressive rules, but also external intervention that risks deepening communal backlash. From the secular parliament perspective, it is an incomplete and inefficient substitute for formal legislative unification. Each seat computes a different type from the same structural data: the court experiences coordination (solving pluralism), communities experience extraction (losing autonomy), women experience both (protection via external authority), parliament experiences scaffolding (transitional mechanism awaiting legislative replacement). The engine computes these divergences from beneficiary/victim data and exit options; the reading does not reconcile them.
 *
 * DIRECTIONALITY LOGIC:
 *   The Supreme Court occupies the agenda-setter role with institutional power and arbitrage-grade exit (it can change its doctrine if political will shifts). Its directionality is low (beneficiary end): d ≈ 0.2. Religious community councils are organized payers with constrained exit (losing autonomy is costly but they cannot leave the system); d ≈ 0.75. Women in minority communities are powerless beneficiaries with constrained exit (they gain protection but remain embedded in community); d ≈ 0.45 (symmetric, because they benefit from protections but bear diffuse costs of external intervention). Conservative personal law guardians are moderate payers with identity-locked exit (their role IS tradition-keeping; exiting means professional death); d ≈ 0.80. Progressive advocates are powerful beneficiaries with arbitrage-grade exit (they use the court but could switch to legislative strategy); d ≈ 0.15. Secular parliament is excluded, institutional power, trapped exit (cannot exit the democratic process); d ≈ 0.55 (asymmetric pressure from being unable to accomplish its agenda through the forum where change is actually occurring).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits mandatrophy dynamics that justify the Scaffold classification. The founding problem—fragmented marriage authority with no protection for women—was real and remains contested in status. The institutional mechanism (judicial harmonization) was designed to solve it via a transitional pathway: incremental constitutional narrowing of permissible variance. However, the mechanism shows signs of mandatrophy: (1) the founding problem's scope has contracted (major reforms are accomplished; remaining issues are marginal within most communities), yet the constraint persists and expands; (2) theater ratio is rising (the court increasingly performs legitimacy rather than enforcing uncontroversial floors); (3) suppression requirement is stabilizing (the court's enforcement posture becomes routine, then ritualized); (4) extractiveness plateaus (the constraint can accomplish no further reform through case-by-case adjudication alone, yet persists). The scaffold's sunset clause should trigger formal legislative action—either formal Uniform Civil Code adoption (secular reading's path) or deliberate reaffirmation of legal pluralism with transparent community negotiation (communal autonomy reading's path). The current trajectory is gridlock: the judicial constraint accomplishes its transitional function but the transition does not complete. Mandatrophy resolves if the constraint either converts to Piton (becomes performative and inertial, preserved by institutional habit) or if one of the sibling readings' political conditions are met (legislature formally acts, OR communities formally renegotiate autonomy).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_judicial_authority,
    'Is judicial constitutional interpretation a legitimate source of law-making authority in marriage governance, or does it usurp legislative prerogative?',
    'Jurisprudential analysis of the separation-of-powers doctrine in the jurisdiction; empirical examination of which institution''s outcomes are actually followed (behavioral compliance); comparative analysis of successful marital reform pathways across multiple plural jurisdictions.',
    'If judicial authority is deemed illegitimate, the constraint transitions to Snare (coercive authority without democratic process). If deemed legitimate, it remains Scaffold (justified but transitional). If the reading shifts to acknowledge both legitimacy and need for legislative completion, the sunset clause becomes operative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_judicial_authority, conceptual, 'Whether the institutional mechanism (judicial interpretation) has legitimate authority to accomplish marriage law reform without formal legislative enactment.').

omega_variable(
    committer_reading_identity,
    'Is this constraint primarily instantiating a neutral institutional mechanism (how harmonization occurs), or is it already committed to one normative reading (that judicial harmonization is the correct institutional path)?',
    'Examine the story''s treatment of sibling readings—are they presented as errors, or as live alternatives with different legitimacy grounds? Assess whether the authored metrics reflect judicial-mechanism operation or ideological preference for that mechanism over legislative or communal alternatives.',
    'If the reading is ideologically committed to the judicial path, the omegas should forefront the committer structure (Rule 2: route to omega). If it is neutral on mechanism, the story describes process without prejudging outcome. The theta-bias (committer frame bias) resides here.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_reading_identity, conceptual, 'Committer-frame neutrality: is the reading institutionally agnostic or institutionally committed to the judicial mechanism as superior?').

omega_variable(
    gender_equality_embedding,
    'Is gender equality protection a genuine foundational axiom of this reading, or is it instrumentally invoked to justify judicial expansion?',
    'Trace the history of gender-equality language in landmark decisions: does it appear in early cases (foundational from the start), or does it emerge later as subsequent justification for an already-expanding judicial role? Examine decisions where gender equality is invoked but produces outcomes that constrain other fundamentals (e.g., gender equality justifying secular law override of community autonomy).',
    'If gender equality is truly foundational, it is a genuine beneficiary-interest underpinning the constraint. If it is instrumentally deployed, the constraint''s extraction structure may be less about protecting women and more about institutional authority-expansion. This affects whether women-in-communities should be classified as beneficiaries or co-victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gender_equality_embedding, empirical, 'Whether gender equality is a foundational axiom or an instrumental justification for judicial authority.').

omega_variable(
    suppression_mechanism_structure_vs_internalization,
    'Is the suppression of religious community councils'' authority structural (enforced by judicial precedent and state power), or internalized (communities internalize constitutional norms as legitimate)?',
    'Behavioral study of community adaptation: do councils enforce constitutional floors because they believe they are legitimate, or because they fear legal consequences? Track communities that leave the formal system entirely (complete exit) versus those that nominally comply while preserving informal authority—the ratio indicates internalization vs. structural suppression.',
    'If suppression is primarily structural, it persists as long as judicial enforcement machinery operates. If internalized, communities carry the constraint''s logic even if formal enforcement relaxes. Internalized suppression indicates deeper institutional capture and higher effective suppression than the scalar (0.41) suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structure_vs_internalization, empirical, 'Whether suppression of religious community authority is structural (external enforcement) or internalized (adopted as legitimate).').

omega_variable(
    sunset_clause_operability,
    'What conditions would trigger the sunset clause, and are they politically feasible?',
    'Scenario analysis: formal Uniform Civil Code passage (requires legislative coalition not currently present); voluntary convergence of personal law codes (requires internal community reform); deliberate reaffirmation of pluralism via new framework negotiation (requires political will to legitimize pluralism formally). Track which scenarios are politically live in the jurisdiction.',
    'If sunset conditions are impossible to satisfy, the Scaffold becomes a permanent Snare (declared as transitional but structurally permanent). If conditions are live, the constraint retains legitimate transitional character. The operability of the sunset clause is diagnostic of whether the scaffold''s justification is real or performative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sunset_clause_operability, empirical, 'Whether the Scaffold''s sunset clause describes feasible political outcomes or performative legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__judicial_harmonization_reading, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1950, marriage_authority__judicial_harmonization_reading, theater_ratio, 1950, 0.12).
narrative_ontology:measurement(marr_tr_t1970, marriage_authority__judicial_harmonization_reading, theater_ratio, 1970, 0.22).
narrative_ontology:measurement(marr_tr_t1990, marriage_authority__judicial_harmonization_reading, theater_ratio, 1990, 0.35).
narrative_ontology:measurement(marr_tr_t2010, marriage_authority__judicial_harmonization_reading, theater_ratio, 2010, 0.43).
narrative_ontology:measurement(marr_tr_t2025, marriage_authority__judicial_harmonization_reading, theater_ratio, 2025, 0.48).

% Extraction over time
narrative_ontology:measurement(marr_be_t1950, marriage_authority__judicial_harmonization_reading, base_extractiveness, 1950, 0.15).
narrative_ontology:measurement(marr_be_t1970, marriage_authority__judicial_harmonization_reading, base_extractiveness, 1970, 0.28).
narrative_ontology:measurement(marr_be_t1990, marriage_authority__judicial_harmonization_reading, base_extractiveness, 1990, 0.42).
narrative_ontology:measurement(marr_be_t2010, marriage_authority__judicial_harmonization_reading, base_extractiveness, 2010, 0.54).
narrative_ontology:measurement(marr_be_t2025, marriage_authority__judicial_harmonization_reading, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1950, marriage_authority__judicial_harmonization_reading, suppression_requirement, 1950, 0.18).
narrative_ontology:measurement(marr_su_t1970, marriage_authority__judicial_harmonization_reading, suppression_requirement, 1970, 0.28).
narrative_ontology:measurement(marr_su_t1990, marriage_authority__judicial_harmonization_reading, suppression_requirement, 1990, 0.36).
narrative_ontology:measurement(marr_su_t2010, marriage_authority__judicial_harmonization_reading, suppression_requirement, 2010, 0.4).
narrative_ontology:measurement(marr_su_t2025, marriage_authority__judicial_harmonization_reading, suppression_requirement, 2025, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__judicial_harmonization_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(marriage_authority__judicial_harmonization_reading, 0.12).
narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, marriage_authority__communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, marriage_authority__secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, marriage_authority__gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, marriage_authority__federalist_millet_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the marriage_authority kernel. The kernel contest involves five constraint stories, each instantiating a different institutional and normative reading of who holds legitimate authority over marriage law in a legally plural society. The judicial_harmonization_reading describes authority flowing through Supreme Court constitutional interpretation. Sibling readings decompose the kernel into: (1) communal_autonomy_reading—authority resides in communities; state enforces but does not author; (2) secularist_reading—authority belongs to democratic legislature; pluralism is transitional; (3) gender_rights_reading—authority contested on grounds of intra-community gender equality; (4) federalist_millet_reading—authority deliberately fragmented as anti-tyranny mechanism. Each reading has a distinct ε (this reading's judicial mechanism produces moderate extraction; communal autonomy produces minimal extraction; secularist produces higher extraction via code imposition; gender_rights produces extraction via judicial override of community norms; federalist produces minimal extraction via legitimized pluralism). All readings share the same empirical referent (the legal system's actual marriage authority structures) but differ in which institution's authority is deemed legitimate and why. The network edges establish that this reading's structure depends on and influences the others: if secular legislative replacement occurs, this scaffold ends; if communities successfully renegotiate autonomy, this reading's necessity evaporates; if gender-equality commitments are formalized in code, the judicial mechanism's temporary character is vindicated.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority__judicial_harmonization_reading, organized, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
