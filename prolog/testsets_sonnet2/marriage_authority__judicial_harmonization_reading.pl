% ============================================================================
% CONSTRAINT STORY: marriage_authority__judicial_harmonization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: marriage_authority__judicial_harmonization_reading
 *   human_readable: Judicial Harmonization of Personal Law via Case-by-Case Constitutional Floor
 *   domain: legal_pluralism/constitutional_law/comparative_family_law
 *
 * SUMMARY:
 *   In jurisdictions where multiple religious or communal personal law codes
 *   govern marriage, divorce, and inheritance, and where the legislature has
 *   repeatedly declined to enact a unifying statute (a Uniform Civil Code),
 *   the constitutional court becomes the de facto site of harmonization:
 *   individual litigants bring equality and dignity claims against specific
 *   provisions of their community's code, and the court's rulings accumulate
 *   into an unlegislated constitutional floor. This story is ONE READING
 *   within a larger kernel contest over where marriage authority properly
 *   sits — communal, legislative, gender-egalitarian, federalist, or
 *   judicial. This reading treats the mechanism itself (case-by-case judicial
 *   convergence absent legislation) as the constraint's defining structural
 *   feature, distinct from the normative question of which authority OUGHT to
 *   hold marriage jurisdiction. The judiciary is authored here as a genuine
 *   beneficiary: each ruling both resolves a live grievance and expands the
 *   court's own doctrinal reach and institutional indispensability, a
 *   scaffold that is meant to be transitional (bridging to eventual
 *   legislation) but which structurally incentivizes its own continuation.
 *
 * KEY AGENTS:
 *   - supreme_court_judiciary: agenda_setter/beneficiary (institutional/analytical) — accumulates doctrinal authority through each ruling
 *   - constitutional_equality_litigants: beneficiary (moderate/constrained) — wins case-specific relief, must re-litigate for others
 *   - personal_law_community_authorities: payer (organized/constrained) — loses predictive control over own doctrine without negotiation
 *   - litigants_awaiting_case_resolution: payer (powerless/trapped) — bears cost of unknowable, unlitigated legal content
 *   - legislature_institutional_standing: payer (institutional/constrained) — institutional standing erodes as courts normalize filling its vacancy
 *   - centralizing_state_apparatus: beneficiary (institutional/analytical) — gets convergence without spending political capital
 *   - religious_minority_communities: excluded (powerless/trapped) — doctrine tested and overridden without their sequencing input
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__judicial_harmonization_reading, 0.52).
domain_priors:suppression_score(marriage_authority__judicial_harmonization_reading, 0.38).
domain_priors:theater_ratio(marriage_authority__judicial_harmonization_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__judicial_harmonization_reading, scaffold).
narrative_ontology:human_readable(marriage_authority__judicial_harmonization_reading, "Judicial Harmonization of Personal Law via Case-by-Case Constitutional Floor").
narrative_ontology:topic_domain(marriage_authority__judicial_harmonization_reading, "legal_pluralism/constitutional_law/comparative_family_law").

domain_priors:requires_active_enforcement(marriage_authority__judicial_harmonization_reading).
narrative_ontology:has_sunset_clause(marriage_authority__judicial_harmonization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__judicial_harmonization_reading, 'c61264a0-4956-4648-a08d-a612cdc0aa5d').
narrative_ontology:cs_kernel_codification('c61264a0-4956-4648-a08d-a612cdc0aa5d', distributed).
narrative_ontology:cs_authority_grounding('c61264a0-4956-4648-a08d-a612cdc0aa5d', practice).
narrative_ontology:cs_interpretation_layer_present('c61264a0-4956-4648-a08d-a612cdc0aa5d').
narrative_ontology:cs_reading_relation('c61264a0-4956-4648-a08d-a612cdc0aa5d', marriage_authority__communal_autonomy_reading, influences).
narrative_ontology:cs_reading_relation('c61264a0-4956-4648-a08d-a612cdc0aa5d', marriage_authority__secularist_reading, influences).
narrative_ontology:cs_reading_relation('c61264a0-4956-4648-a08d-a612cdc0aa5d', marriage_authority__gender_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('c61264a0-4956-4648-a08d-a612cdc0aa5d', marriage_authority__federalist_millet_reading, influences).
narrative_ontology:cs_axiom('c61264a0-4956-4648-a08d-a612cdc0aa5d', foundational, constitutional_floor_binds_regardless_of_legislative_action).
narrative_ontology:cs_axiom_status(constitutional_floor_binds_regardless_of_legislative_action, holdable).
narrative_ontology:cs_axiom_grounding('c61264a0-4956-4648-a08d-a612cdc0aa5d', constitutional_floor_binds_regardless_of_legislative_action, conventional).
narrative_ontology:cs_axiom('c61264a0-4956-4648-a08d-a612cdc0aa5d', secondary, case_by_case_adjudication_is_adequate_substitute_for_codification).
narrative_ontology:cs_axiom_status(case_by_case_adjudication_is_adequate_substitute_for_codification, holdable).
narrative_ontology:cs_axiom_grounding('c61264a0-4956-4648-a08d-a612cdc0aa5d', case_by_case_adjudication_is_adequate_substitute_for_codification, instrumental).
narrative_ontology:cs_reference_frame('c61264a0-4956-4648-a08d-a612cdc0aa5d', post_independence_legislative_deferral_settlement).
narrative_ontology:cs_drift_state('c61264a0-4956-4648-a08d-a612cdc0aa5d', contemporary_docket_accumulation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c61264a0-4956-4648-a08d-a612cdc0aa5d', '').
narrative_ontology:cs_kernel_id(marriage_authority__judicial_harmonization_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__judicial_harmonization_reading, supreme_court_judiciary).
narrative_ontology:constraint_beneficiary(marriage_authority__judicial_harmonization_reading, constitutional_equality_litigants).
narrative_ontology:constraint_beneficiary(marriage_authority__judicial_harmonization_reading, centralizing_state_apparatus).
narrative_ontology:constraint_victim(marriage_authority__judicial_harmonization_reading, personal_law_community_authorities).
narrative_ontology:constraint_victim(marriage_authority__judicial_harmonization_reading, litigants_awaiting_case_resolution).
narrative_ontology:constraint_victim(marriage_authority__judicial_harmonization_reading, legislature_institutional_standing).
narrative_ontology:constraint_vindicates(marriage_authority__judicial_harmonization_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(marriage_authority__judicial_harmonization_reading, judicial_review_competence_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decides individual marriage, divorce, maintenance, and inheritance disputes brought under various personal law codes, and in doing so incrementally lays down a constitutional floor (equality, dignity, due process) that personal law codes must meet. It does this without any legislature enacting a uniform code, so the floor exists only as an accumulating body of case law. The Court gains institutional authority and doctrinal reach with each ruling; it bears no cost if the resulting patchwork is incoherent, since incoherence becomes the next case's docket item.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, supreme_court_judiciary, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__judicial_harmonization_reading, supreme_court_judiciary, beneficiary).

% Individuals, disproportionately women, who bring challenges against discriminatory provisions of their community's personal law (unequal maintenance, unilateral divorce, unequal inheritance) directly to constitutional courts because no legislative reform is forthcoming. They win real, concrete relief in their own cases, but each win is scoped to the litigated fact pattern rather than codified as general law, so the next similarly situated person must re-litigate.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, constitutional_equality_litigants, beneficiary,
    moderate, biographical, constrained, national).

% Religious and communal bodies that administer marriage, divorce, and inheritance under their own codes lose predictive control over their own law's content: doctrine is now subject to case-by-case override by a court applying an external constitutional standard, with no negotiated settlement or legislative process through which the community had a voice. They cannot appeal to a legislature that never acted, and cannot exit the jurisdiction of the courts that are rewriting their internal law piecemeal.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, personal_law_community_authorities, payer,
    organized, generational, constrained, national).

% People whose marriages, divorces, or inheritance claims sit in legal limbo because the governing rule for their situation has not yet been tested before the Supreme Court, or was decided for a different community's code and its applicability to theirs is unclear. They bear the cost of a legal system whose content is genuinely unknowable in advance, and cannot afford or access the appellate process that would resolve their case.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, litigants_awaiting_case_resolution, payer,
    powerless, biographical, trapped, local).

% The elected body with formal constitutional authority to enact a Uniform Civil Code or otherwise legislate personal law reform, but which has not acted for decades owing to electoral cost. Each judicial intervention further normalizes the pattern of courts filling the vacancy the legislature leaves, eroding the political cost of continued legislative inaction and, over time, the legislature's own claim to being the primary author of family law.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, legislature_institutional_standing, payer,
    institutional, civilizational, constrained, national).

% The broader executive and administrative state benefits from a de facto constitutional floor emerging without the political risk of forcing a Uniform Civil Code through a fractious legislature; the judiciary absorbs the controversy and the state gets convergence pressure toward uniform norms without having spent political capital to legislate it.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, centralizing_state_apparatus, beneficiary,
    institutional, generational, analytical, national).

% Minority religious communities whose personal law is most frequently the target of constitutional challenge experience the harmonization process as selective: their family law is scrutinized and overridden case-by-case while majority-community family norms, where codified in ordinary statute, face comparatively less individualized judicial override. They are not consulted on the pace or sequencing of which doctrines get tested first.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, religious_minority_communities, excluded,
    powerless, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority__judicial_harmonization_reading, supreme_court_judiciary).
narrative_ontology:fixing_cost_class(marriage_authority__judicial_harmonization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for extending baseline constitutional protections (equality, dignity, due process) across a fragmented personal law landscape in the absence of any legislature willing to enact a single uniform code — coordinating around a floor without requiring political consensus on a ceiling.
% TRANSFER_FUNCTION: Moves normative authority over family law from community authorities and the legislature to the judiciary, one case at a time; moves concrete relief to individual successful litigants while moving legal certainty away from everyone whose situation has not yet been adjudicated.
% ABSENT_VOICES: Community authorities whose doctrine is overridden without a negotiated process, and future litigants whose situations will be governed by precedent set in cases they were not party to, are structurally absent from the harmonization process — it happens docket by docket, not through any forum in which affected communities as a whole can be heard.
% DISAPPEARANCE_RATIONALE: If judicial harmonization stopped tomorrow — courts declining to adjudicate personal law equality claims and deferring entirely to community codes and legislative silence — discriminatory provisions in currently unlitigated personal law codes would persist unchallenged, the legislature would face renewed pressure (or renewed absence of pressure) to act, and the incremental constitutional floor built case by case would freeze at whatever level it had reached, with future claimants losing their only currently functioning avenue for relief.
% FOUNDING_PROBLEM: Legislatures repeatedly declined, across decades and electoral cycles, to enact uniform family law reform because of the political cost of appearing to interfere with religious community autonomy, leaving individuals harmed by discriminatory personal law provisions with no forum for relief except the courts already empowered to hear constitutional claims.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and comparative constitutional law commentators outside the judiciary and outside the litigant bar attest that legislative inaction on personal law reform has persisted across multiple electoral cycles independent of which party holds power, corroborating that the underlying legislative-vacancy problem the courts are filling remains genuinely unresolved rather than merely a pretext judges use to expand their own docket.
narrative_ontology:disappearance_verdict(marriage_authority__judicial_harmonization_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__judicial_harmonization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__judicial_harmonization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_authority__judicial_harmonization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__judicial_harmonization_reading, 0.52, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at a moderate 0.52 at interval end: the mechanism delivers genuine relief to individual litigants (a real coordination function) but does so by transferring durable authority to an institution (the judiciary) that bears none of the costs of the resulting unevenness and gains reach with each intervention — this is the scaffold's characteristic asymmetry. Suppression is comparatively low (0.38) because no one is coercively blocked from litigating; the harm is closer to structural unpredictability and unaddressed backlog than active coercion. Theater ratio rises over the interval (0.20 to 0.44) as the mechanism, originally justified as filling an urgent gap, increasingly performs the function of a permanent alternative to legislation rather than a bridge toward it — courts issue rulings framed as narrow and case-specific while the aggregate effect functions as sweeping doctrine, and the 'awaiting eventual UCC' framing becomes less credible as decades pass without legislative action.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's seat, each ruling is principled constitutional adjudication filling an urgent gap. From the community authority's seat, the same ruling is an externally imposed override of internally legitimate doctrine, arrived at without any process the community participated in designing. From the awaiting-litigant's seat, the mechanism is simply unpredictable — the law they live under is whatever the next case happens to decide. The engine should compute meaningfully different per-seat classifications from these same structural facts.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary sits near the beneficiary end: institutional, analytical exit (it can decline cases or narrow holdings at will), and it accumulates authority regardless of outcome. Litigants who win are real beneficiaries but with constrained exit — they cannot compel systemic reform, only individual relief. Community authorities and legislature both sit toward the target end: organized/institutional power respectively, but constrained exit because neither can unilaterally stop the harmonization process once it is underway — the community cannot appeal to an inactive legislature, and the legislature cannot easily reclaim jurisdiction the courts have already occupied without itself legislating, which is precisely the action it has avoided.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legislative vacancy leaving discriminatory provisions unchallenged) remains live per outside corroboration, which argues against mandatrophy in the strong sense — the mechanism is not purely inertial theater. But the has_sunset_clause declaration is aspirational rather than operative: there is no actual mechanism by which judicial harmonization terminates once a UCC is passed, because no UCC is being passed, and the mechanism itself reduces the political pressure that would produce one. This is the scaffold-becoming-permanent pattern: sunset is declared in the self-understanding of the institution ('this is filling a gap until legislation arrives') but nothing structurally forces the transition, and the rising theater_ratio is exactly the signature of that drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scaffold_or_permanent_default,
    'Is case-by-case judicial harmonization a genuine transitional scaffold pending eventual legislation, or has it become the permanent default mode of family law reform precisely because it relieves the legislature of the political cost of acting?',
    'Track whether legislative attempts at comprehensive personal law reform increase or decrease in frequency and seriousness as the body of judicial precedent grows; a declining trend would indicate the scaffold has become self-perpetuating rather than transitional.',
    'If permanent, the has_sunset_clause declaration is aspirational rather than structural, and the constraint''s classification should drift toward tangled_rope or piton over a longer interval than modeled here; if genuinely transitional, scaffold classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffold_or_permanent_default, empirical, 'Whether judicial harmonization is transitional or self-perpetuating.').

omega_variable(
    judiciary_as_beneficiary_or_reluctant_actor,
    'Does the judiciary structurally benefit from expanding into personal law adjudication (doctrinal reach, institutional indispensability), or is it a reluctant actor filling a vacancy it would prefer the legislature occupy?',
    'Examine judicial opinions and extrajudicial commentary by justices for explicit invitations to the legislature to act versus doctrinal language asserting continuing jurisdiction; also examine whether courts narrow or expand the scope of relief granted over time.',
    'If the judiciary is a genuine reluctant actor, its role should be closer to observer/agenda_setter without secondary beneficiary status, reducing the authored extraction; if it actively expands its own reach, the beneficiary declaration is warranted as authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judiciary_as_beneficiary_or_reluctant_actor, conceptual, 'Whether the judiciary''s institutional-authority gain is incidental or structurally incentivized.').

omega_variable(
    kernel_framing_alternative_at_community_level,
    'If this constraint were reframed at the level of a single religious community''s internal doctrinal contest (rather than the state-wide judicial-institutional mechanism), would the classification shift from scaffold to snare, given that from inside the community the process may appear as an externally imposed override with no internal legitimating process?',
    'Author a sibling story from the communal_autonomy_reading''s own lights (already declared as a separate kernel reading) and compare computed classifications directly rather than trying to average the two framings into one constraint.',
    'Confirms the ε-invariance principle: the state-institutional-mechanism framing (this story) and the community-autonomy framing (sibling story) are properly two different constraints with two different ε values, not one constraint viewed two ways.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_alternative_at_community_level, conceptual, 'Alternative framing at the community level versus the institutional-mechanism level authored here.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__judicial_harmonization_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority__judicial_harmonization_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(marr_tr_t8, marriage_authority__judicial_harmonization_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement(marr_tr_t16, marriage_authority__judicial_harmonization_reading, theater_ratio, 16, 0.32).
narrative_ontology:measurement(marr_tr_t24, marriage_authority__judicial_harmonization_reading, theater_ratio, 24, 0.37).
narrative_ontology:measurement(marr_tr_t32, marriage_authority__judicial_harmonization_reading, theater_ratio, 32, 0.41).
narrative_ontology:measurement(marr_tr_t40, marriage_authority__judicial_harmonization_reading, theater_ratio, 40, 0.44).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority__judicial_harmonization_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(marr_be_t8, marriage_authority__judicial_harmonization_reading, base_extractiveness, 8, 0.36).
narrative_ontology:measurement(marr_be_t16, marriage_authority__judicial_harmonization_reading, base_extractiveness, 16, 0.42).
narrative_ontology:measurement(marr_be_t24, marriage_authority__judicial_harmonization_reading, base_extractiveness, 24, 0.47).
narrative_ontology:measurement(marr_be_t32, marriage_authority__judicial_harmonization_reading, base_extractiveness, 32, 0.5).
narrative_ontology:measurement(marr_be_t40, marriage_authority__judicial_harmonization_reading, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority__judicial_harmonization_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(marr_su_t8, marriage_authority__judicial_harmonization_reading, suppression_requirement, 8, 0.3).
narrative_ontology:measurement(marr_su_t16, marriage_authority__judicial_harmonization_reading, suppression_requirement, 16, 0.33).
narrative_ontology:measurement(marr_su_t24, marriage_authority__judicial_harmonization_reading, suppression_requirement, 24, 0.35).
narrative_ontology:measurement(marr_su_t32, marriage_authority__judicial_harmonization_reading, suppression_requirement, 32, 0.37).
narrative_ontology:measurement(marr_su_t40, marriage_authority__judicial_harmonization_reading, suppression_requirement, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__judicial_harmonization_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(marriage_authority__judicial_harmonization_reading, 0.12).
narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, federalist_millet_reading).

% DUAL FORMULATION NOTE:
% This story is one of five linked constraint stories decomposing the natural-language concept 'marriage authority under legal pluralism' per the epsilon-invariance principle. Each sibling reading (communal_autonomy_reading, secularist_reading, gender_rights_reading, federalist_millet_reading) is a separate constraint with its own epsilon, beneficiary/victim structure, and claimed type, sharing the same underlying kernel (marriage_authority) but instantiating structurally distinct claims about where legitimate authority sits and how it should evolve. This reading is distinctive in centering an institutional MECHANISM (judicial case-by-case convergence absent legislation) rather than a normative claim about who ought to hold authority; it interacts with all four normative siblings by altering the political-institutional conditions under which each of their normative claims can be pursued.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
