% ============================================================================
% CONSTRAINT STORY: marriage_authority__secularist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__secularist_reading, []).

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
 *   constraint_id: marriage_authority__secularist_reading
 *   human_readable: Secular Legislative Monopoly on Marriage Authority via Uniform Civil Code
 *   domain: legal/political/constitutional
 *
 * SUMMARY:
 *   This constraint instantiates the secularist reading of the marriage
 *   authority kernel in post-colonial constitutional orders (specifically
 *   India). It treats marriage, divorce, inheritance, and family status as
 *   matters for democratic legislation under a Uniform Civil Code (UCC),
 *   framing personal law pluralism as a colonial holdover and transitional
 *   anomaly rather than a legitimate constitutional settlement. The
 *   constraint coordinates under a secular nation-building project while
 *   asymmetrically extracting religious autonomy from minority communities.
 *
 * KEY AGENTS:
 *   - state_legislature: agenda-setter (institutional/constrained/national) â claims constitutional authority to displace personal law
 *   - secular_modernist_coalition: beneficiary (organized/constrained/national) â draws nation-building legitimacy from uniformization
 *   - minority_religious_communities: payer (organized/identity_locked/national) â bear loss of legal autonomy fused with religious identity
 *   - constitutional_court: observer (institutional/analytical/national) â mediates between UCC ambition and existing pluralism
 *   - religious_autonomy_advocates: excluded (organized/constrained/national) â delegitimized as communal
 *   - gender_rights_advocates: excluded (organized/constrained/national) â preferred judicial mechanism sidelined
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__secularist_reading, 0.78).
domain_priors:suppression_score(marriage_authority__secularist_reading, 0.72).
domain_priors:theater_ratio(marriage_authority__secularist_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__secularist_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority__secularist_reading, "Secular Legislative Monopoly on Marriage Authority via Uniform Civil Code").
narrative_ontology:topic_domain(marriage_authority__secularist_reading, "legal/political/constitutional").

domain_priors:requires_active_enforcement(marriage_authority__secularist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__secularist_reading, '7e812d72-d829-4965-9101-cfd0879d5b1b').
narrative_ontology:cs_kernel_codification('7e812d72-d829-4965-9101-cfd0879d5b1b', formalized).
narrative_ontology:cs_authority_grounding('7e812d72-d829-4965-9101-cfd0879d5b1b', lineage).
narrative_ontology:cs_interpretation_layer_present('7e812d72-d829-4965-9101-cfd0879d5b1b').
narrative_ontology:cs_reading_relation('7e812d72-d829-4965-9101-cfd0879d5b1b', marriage_authority__communal_autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('7e812d72-d829-4965-9101-cfd0879d5b1b', marriage_authority__federalist_millet_reading, forecloses).
narrative_ontology:cs_reading_relation('7e812d72-d829-4965-9101-cfd0879d5b1b', marriage_authority__gender_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('7e812d72-d829-4965-9101-cfd0879d5b1b', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_axiom('7e812d72-d829-4965-9101-cfd0879d5b1b', foundational, democratic_legislature_supreme_in_family_law).
narrative_ontology:cs_axiom_status(democratic_legislature_supreme_in_family_law, holdable).
narrative_ontology:cs_axiom_grounding('7e812d72-d829-4965-9101-cfd0879d5b1b', democratic_legislature_supreme_in_family_law, conventional).
narrative_ontology:cs_axiom('7e812d72-d829-4965-9101-cfd0879d5b1b', foundational, legal_pluralism_provisional_anomaly).
narrative_ontology:cs_axiom_status(legal_pluralism_provisional_anomaly, holdable).
narrative_ontology:cs_axiom_grounding('7e812d72-d829-4965-9101-cfd0879d5b1b', legal_pluralism_provisional_anomaly, instrumental).
narrative_ontology:cs_reference_frame('7e812d72-d829-4965-9101-cfd0879d5b1b', secular_legislative_supremacy).
narrative_ontology:cs_drift_state('7e812d72-d829-4965-9101-cfd0879d5b1b', contemporary_political_order, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7e812d72-d829-4965-9101-cfd0879d5b1b', '').
narrative_ontology:cs_kernel_id(marriage_authority__secularist_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__secularist_reading, secular_modernist_coalition).
narrative_ontology:constraint_victim(marriage_authority__secularist_reading, minority_religious_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims constitutional authority to enact a Uniform Civil Code governing marriage, divorce, inheritance, and family status for all citizens regardless of religion. Uses Directive Principles and democratic mandate to justify displacing personal law systems. Faces electoral and coalition constraints that delay enactment but maintains the institutional ambition.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, state_legislature, agenda_setter,
    institutional, generational, constrained, national).

% Draws political and cultural legitimacy from the nation-building project of replacing communal identities with uniform citizenship. Benefits electorally and ideologically from framing minority personal law as backward or transitional. Exit would require abandoning the central plank of their secular-modernist platform.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, secular_modernist_coalition, beneficiary,
    organized, generational, constrained, national).

% Bear the loss of institutional autonomy over family law that has historically been administered by community courts, religious councils, and personal law traditions. Their religious identity is legally fused with personal law status; exit from the constraint means exiting recognized community membership or accepting civil override of sacred norms.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, minority_religious_communities, payer,
    organized, generational, identity_locked, national).

% Reviews personal law cases under constitutional equality and fundamental rights frameworks. Currently mediates between the legislative UCC ambition and existing personal law protections through case-by-case adjudication, producing constitutional floors without fully displacing pluralism.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, constitutional_court, observer,
    institutional, generational, analytical, national).

% Argue that marriage and family law are inseparable from religious community self-definition and that the Constitution protects this pluralism. They are systematically delegitimized in legislative debate as communal or anti-modern, and their preferred framework is not on the legislative agenda.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, religious_autonomy_advocates, excluded,
    organized, generational, constrained, national).

% Seek gender equality within or against personal law, but many favor judicial constitutionalism or internal community reform rather than legislative uniformization. Their equality claims are selectively appropriated by the secularist coalition while their preferred mechanisms are sidelined.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, gender_rights_advocates, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority__secularist_reading, secular_modernist_coalition).
narrative_ontology:fixing_cost_class(marriage_authority__secularist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, territorially uniform marriage and family law code eliminating forum-shopping and inter-jurisdictional conflict across religious communities, grounding family status in democratically enacted legislation rather than religious tradition.
% TRANSFER_FUNCTION: Transfers authority to define marriage validity, divorce, inheritance, and family status from religious community institutions and personal law traditions to the centralized democratic legislature and its uniform code.
% ABSENT_VOICES: Communal autonomy advocates who view marriage as inseparable from religious community self-definition; federalist pluralists who see personal law as consociational protection against majoritarianism; gender-rights jurists who prefer constitutional equality-based judicial reform over legislative uniformization; and minority community leaders who hold that personal law is a protected cultural right.
% DISAPPEARANCE_RATIONALE: If the secular legislative monopoly and UCC project vanished overnight, marriage authority would revert to personal law systems, religious community courts and councils would regain substantive jurisdiction, the legislative secular modernist coalition would lose a primary nation-building instrument, and the constitutional architecture would shift from unified code to pluralized family law.
% FOUNDING_PROBLEM: Post-colonial nation-building required replacing fragmented colonial personal law and princely state variations with a unified legal framework to construct a modern secular citizenry and eliminate colonial-era religious categorizations as governing legal principles.
% FOUNDING_PROBLEM_CORROBORATION: Secular modernist historians and constitutional framers attest the founding problem as the need to overcome colonial divide-and-rule through uniform law. Minority community historians and pluralist constitutional scholars attest that the plural arrangement was a negotiated constitutional compromise (Article 44 as non-justiciable Directive Principle), not a transitional anomaly, and that the 'founding problem' narrative is retroactive majoritarian justification. External comparative law scholars note that personal law pluralism was deliberately retained as a constitutional settlement, not a temporary holdover.
narrative_ontology:disappearance_verdict(marriage_authority__secularist_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__secularist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__secularist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority__secularist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__secularist_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__secularist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority__secularist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority__secularist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the constraint eliminates minority legal autonomy and transfers definitional authority to a majoritarian legislature; suppression (0.72) reflects the active political and legal suppression of personal law forums and the delegitimation of pluralism. Theater ratio (0.50) is elevated because UCC advocacy frequently operates as performative secular nationalism masking majoritarian cultural homogenization. Accessibility collapse (0.65) is substantial: personal law alternatives are being delegitimized but have not fully disappeared. Resistance (0.68) is significant from minority communities, federalist scholars, and pluralist jurists. The measurement series share one time grid so every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The secular modernist coalition experiences this as legitimate democratic coordination (national unity, modernization, gender-progressive reform), while minority religious communities experience it as targeted extraction of institutional autonomy and identity-locked legal status. The agenda-setter (state legislature) sits between: it gains jurisdiction but also bears the political friction of enforcement. The engine computes this divergence from the structural data rather than the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   The secular_modernist_coalition is the structural beneficiary (low directionality, subsidized by nation-building legitimacy and electoral majoritarianism). Minority_religious_communities are the structural targets (high directionality, identity_locked exit amplifies extraction because religious identity is fused with personal law). The state_legislature is agenda_setter with moderate directionality: it gains institutional power but also absorbs political resistance. Constitutional courts and excluded advocates sit outside the direct transfer.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by requiring both coordination (uniform law, democratic legitimacy, reduced forum shopping) and extraction (autonomy elimination, identity-locked targets). Without the victim declaration, it might read as rope or scaffold; without the coordination function, it would be pure snare. The tangled_rope classification captures that the nation-building coordination is real but inseparable from the asymmetric extraction of minority autonomy. The founding problem status is contested, preventing automatic mandatrophy resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    secular_majoritarian_naturalization,
    'Is the secular nation-state''s monopoly on marriage authority a natural evolution of democratic sovereignty, or a constructed majoritarian project that recodes minority difference as backwardness?',
    'Comparative historical analysis of whether post-colonial states with similar plural inheritances uniformly centralized family law, or whether pluralism stabilized as legitimate constitutional choice.',
    'If constructed and majoritarian, extraction is higher and the coordination story is cover; if natural democratic evolution, extraction is lower and coordination is primary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_majoritarian_naturalization, conceptual, 'Whether secular legislative supremacy is natural or constructed majoritarianism.').

omega_variable(
    coordination_extraction_separability,
    'Can the genuine coordination benefits of uniform marriage law (reduced forum shopping, clear state records, gender-equal baseline) be achieved without eliminating minority personal law autonomy?',
    'Comparative analysis of optional civil codes, concurrent jurisdiction models, or harmonization-with-respect-for-difference frameworks.',
    'If separable, the constraint is tangled rope using coordination to legitimate extraction; if inseparable, the extraction is the necessary cost of the coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether uniform law benefits require autonomy elimination.').

omega_variable(
    ucc_enforcement_capacity,
    'Does the constraint''s persistence depend on electoral majority consolidation alone, or on structural suppression of personal law institutions through court decisions, criminal law overlaps, and administrative absorption?',
    'Mapping of institutional substitution: whether personal law forums have been hollowed out by parallel state institutions regardless of electoral cycles.',
    'If structural substitution is deep, the constraint persists through institutional inertia even without active enforcement; if electoral-dependent, it is more volatile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ucc_enforcement_capacity, empirical, 'Whether enforcement is electoral or structural.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__secularist_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority__secularist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(marr_tr_t15, marriage_authority__secularist_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement(marr_tr_t30, marriage_authority__secularist_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement(marr_tr_t45, marriage_authority__secularist_reading, theater_ratio, 45, 0.38).
narrative_ontology:measurement(marr_tr_t60, marriage_authority__secularist_reading, theater_ratio, 60, 0.45).
narrative_ontology:measurement(marr_tr_t75, marriage_authority__secularist_reading, theater_ratio, 75, 0.5).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority__secularist_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(marr_be_t15, marriage_authority__secularist_reading, base_extractiveness, 15, 0.3).
narrative_ontology:measurement(marr_be_t30, marriage_authority__secularist_reading, base_extractiveness, 30, 0.4).
narrative_ontology:measurement(marr_be_t45, marriage_authority__secularist_reading, base_extractiveness, 45, 0.55).
narrative_ontology:measurement(marr_be_t60, marriage_authority__secularist_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(marr_be_t75, marriage_authority__secularist_reading, base_extractiveness, 75, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority__secularist_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(marr_su_t15, marriage_authority__secularist_reading, suppression_requirement, 15, 0.45).
narrative_ontology:measurement(marr_su_t30, marriage_authority__secularist_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(marr_su_t45, marriage_authority__secularist_reading, suppression_requirement, 45, 0.62).
narrative_ontology:measurement(marr_su_t60, marriage_authority__secularist_reading, suppression_requirement, 60, 0.7).
narrative_ontology:measurement(marr_su_t75, marriage_authority__secularist_reading, suppression_requirement, 75, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(marriage_authority__secularist_reading, communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, federalist_millet_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% This constraint is the secularist reading of the marriage authority kernel, which decomposes into structurally distinct claims about where marriage authority resides: in the democratic legislature (this reading), in religious community tradition (communal_autonomy_reading), in fragmented consociational design (federalist_millet_reading), in judicial equality guarantees (gender_rights_reading), or in case-by-case Supreme Court review (judicial_harmonization_reading). Each reading has a distinct epsilon, beneficiary structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
