% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__hybrid_pragmatic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy__hybrid_pragmatic_reading, []).

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
 *   constraint_id: marriage_commitment_legitimacy__hybrid_pragmatic_reading
 *   human_readable: LDS Manifesto Hybrid Pragmatic Reading: Strategic Adaptation of Marriage Legitimacy
 *   domain: religious_institutional_history/political_theology/commitment_systems
 *
 * SUMMARY:
 *   The 1890 Manifesto (Official Declaration 1) issued by LDS President
 *   Wilford Woodruff officially discontinued the practice of plural marriage
 *   under intense federal pressure. This constraint story models the 'hybrid
 *   pragmatic reading': the Manifesto as strategic institutional adaptation
 *   where prophetic authority was deployed to manage an exogenous crisis
 *   (federal disincorporation threat) while preserving core theological
 *   commitments (the eternality of sealing, the prophetic office's settlement
 *   authority) through deliberate scope ambiguity — the text declares
 *   cessation of practice but is silent on whether the underlying doctrine
 *   (D&C 132) is abrogated, suspended, or still binding in principle. This
 *   ambiguity serves the institutional leadership (beneficiary) by retaining
 *   doctrinal flexibility and avoiding a schism that would dissolve the
 *   community, while the rank-and-file members and polygamous families
 *   (victims) bear interpretive uncertainty, legitimacy ambiguity for their
 *   existing sealings, and the material costs of compliance. The constraint
 *   is a tangled rope: it performs genuine coordination (institutional
 *   survival, legal normalization, avoidance of schism) AND asymmetric
 *   extraction (leadership gains flexibility; laity bears uncertainty and
 *   polygamous families bear concentrated harm). Active enforcement is
 *   required — post-Manifesto polygamy was disciplined through
 *   excommunication (Second Manifesto 1904, smoot hearings 1904-1907).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.58).
domain_priors:suppression_score(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.62).
domain_priors:theater_ratio(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_legitimacy__hybrid_pragmatic_reading, "LDS Manifesto Hybrid Pragmatic Reading: Strategic Adaptation of Marriage Legitimacy").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__hybrid_pragmatic_reading, "religious_institutional_history/political_theology/commitment_systems").

domain_priors:requires_active_enforcement(marriage_commitment_legitimacy__hybrid_pragmatic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__hybrid_pragmatic_reading, '47145bdd-201e-4edb-a6c8-51d7434cbf08').
narrative_ontology:cs_kernel_codification('47145bdd-201e-4edb-a6c8-51d7434cbf08', formalized).
narrative_ontology:cs_authority_grounding('47145bdd-201e-4edb-a6c8-51d7434cbf08', extraction).
narrative_ontology:cs_interpretation_layer_present('47145bdd-201e-4edb-a6c8-51d7434cbf08').
narrative_ontology:cs_reading_relation('47145bdd-201e-4edb-a6c8-51d7434cbf08', marriage_commitment_legitimacy__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_reading_relation('47145bdd-201e-4edb-a6c8-51d7434cbf08', marriage_commitment_legitimacy__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('47145bdd-201e-4edb-a6c8-51d7434cbf08', foundational, prophetic_authority_serves_institutional_survival).
narrative_ontology:cs_axiom_status(prophetic_authority_serves_institutional_survival, holdable).
narrative_ontology:cs_axiom_grounding('47145bdd-201e-4edb-a6c8-51d7434cbf08', prophetic_authority_serves_institutional_survival, instrumental).
narrative_ontology:cs_axiom('47145bdd-201e-4edb-a6c8-51d7434cbf08', foundational, scope_ambiguity_preserves_doctrinal_continuity).
narrative_ontology:cs_axiom_status(scope_ambiguity_preserves_doctrinal_continuity, holdable).
narrative_ontology:cs_axiom_grounding('47145bdd-201e-4edb-a6c8-51d7434cbf08', scope_ambiguity_preserves_doctrinal_continuity, conventional).
narrative_ontology:cs_reference_frame('47145bdd-201e-4edb-a6c8-51d7434cbf08', prophetic_settlement_authority).
narrative_ontology:cs_drift_state('47145bdd-201e-4edb-a6c8-51d7434cbf08', post_manifesto_polygamy_persistence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('47145bdd-201e-4edb-a6c8-51d7434cbf08', '').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__hybrid_pragmatic_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__hybrid_pragmatic_reading, institutional_leadership).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, rank_and_file_members).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, polygamous_families).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__hybrid_pragmatic_reading, prophetic_authority_settles_doctrinal_boundaries).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__hybrid_pragmatic_reading, ecclesiastical_continuity_requires_institutional_survival).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Church presidency and apostolic quorum who issued the Manifesto. They gained legal protection for church assets, avoided leadership imprisonment, and retained doctrinal flexibility through deliberate ambiguity about whether the Manifesto was revelation or policy. They control the interpretive apparatus that defines what the Manifesto means. Their exit options include institutional relocation, doctrinal reinterpretation, and succession management — they hold the keys to the constraint's evolution.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, institutional_leadership, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_legitimacy__hybrid_pragmatic_reading, institutional_leadership, beneficiary).

% Ordinary Latter-day Saints whose religious identity, family structure, and communal standing were bound to the pre-Manifesto marriage theology. They bear the interpretive uncertainty: told the practice was divinely mandated, then told it was suspended, without clear doctrinal resolution. Their exit is identity-locked — leaving means abandoning the religious framework that constitutes their self-understanding, family relationships, and eternal cosmology. They experience the constraint as legitimacy ambiguity: are their sealings valid? Their ancestors' marriages legitimate? The theology they built their lives on is now ambiguously suspended.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, rank_and_file_members, payer,
    organized, biographical, identity_locked, global).

% Men, women, and children in plural marriages at the time of the Manifesto. They faced immediate legal jeopardy, social ostracization, and theological whiplash. Men risked prosecution; women faced uncertain marital status and inheritance rights; children faced bastardy stigma. Their exit options were near-zero: geographic relocation to Mexico/Canada (colonies) was difficult and temporary; public abandonment of families destroyed social and religious standing; secret continuation invited excommunication. They are the most concentrated extraction point — the constraint's enforcement falls hardest here.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, polygamous_families, payer,
    powerless, biographical, trapped, regional).

% U.S. Congress, Department of Justice, and federal courts that enacted and enforced anti-polygamy legislation (Edmunds Act, Edmunds-Tucker Act). They applied exogenous pressure: disincorporation of the church, seizure of assets, disenfranchisement of polygamists, imprisonment of leaders. They are not coordinated by the constraint — they are the exogenous crisis it adapts to. Their structural role is the enforcement environment that makes the constraint necessary from leadership's perspective.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, federal_authority, observer,
    institutional, generational, analytical, national).

% Members who rejected the Manifesto as authoritative and continued plural marriage, eventually forming fundamentalist breakaway groups. They were excluded from the institutional conversation — their dissent was disciplined (excommunication) rather than engaged. They would argue the Manifesto was neither revelation nor strategic adaptation but capitulation. Their exclusion is structural: the constraint's legitimacy depends on their silence or removal.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, dissident_fundamentalists, excluded,
    moderate, biographical, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Managed the existential threat of federal disincorporation and leadership imprisonment while preserving the church as a legal entity and the prophetic office as a legitimate authority structure. Solved the coordination problem of institutional survival under hostile state power without triggering schism that would dissolve the community.
% TRANSFER_FUNCTION: Moves interpretive certainty and communal legitimacy from the laity (who lose clear doctrinal grounding for their marital theology) to institutional leadership (who gain doctrinal flexibility and institutional survival). Moves the cost of federal compliance from the institutional center (which would face asset seizure and imprisonment) to polygamous families (who bear legal jeopardy and social destabilization). Moves the authority to define 'marriage legitimacy' from the fixed textual kernel (D&C 132) to the living prophetic office.
% ABSENT_VOICES: Polygamous wives and children whose marital status, inheritance rights, and social standing were legally and theologically destabilized by the Manifesto. They were not consulted in the decision, had no representational voice in the quorum deliberations, and their objections were treated as resistance to prophetic authority rather than legitimate theological concern. Also absent: the fundamentalist dissenters who would become the excluded seat — their perspective was disciplined out of the institutional frame.
% DISAPPEARANCE_RATIONALE: If the Manifesto and its interpretive framework vanished overnight, the LDS Church would face immediate re-litigation of its corporate status, tax exemption, and temple recommend standards. The theological legitimacy of all post-1890 sealings would be contested. The prophetic office's authority to settle doctrinal boundaries would be destabilized. Fundamentalist groups would claim institutional continuity. The entire legal-theological settlement enabling the church's 20th-century growth would collapse.
% FOUNDING_PROBLEM: Federal anti-polygamy legislation (Edmunds Act 1882, Edmunds-Tucker Act 1887) threatening the legal existence of the Church corporation, seizure of all church assets including temples, disenfranchisement of polygamist members, and imprisonment of the entire senior leadership. The church faced institutional death — not just persecution but legal dissolution.
% FOUNDING_PROBLEM_CORROBORATION: The federal threat is historically documented: Edmunds-Tucker Act disincorporated the church and seized its property (confirmed by Supreme Court in Late Corporation of the Church v. United States, 1890); church leadership went into hiding; assets were only returned after the Manifesto and subsequent testimony. This is attested by congressional records, court decisions, and non-Mormon historical scholarship — sources outside the benefiting institutional leadership.
narrative_ontology:disappearance_verdict(marriage_commitment_legitimacy__hybrid_pragmatic_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_legitimacy__hybrid_pragmatic_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__hybrid_pragmatic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_legitimacy__hybrid_pragmatic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_legitimacy__hybrid_pragmatic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects moderate but real extraction: leadership gains institutional survival and doctrinal control; laity loses interpretive certainty; polygamous families lose legal protection and social standing. Suppression (0.62) is substantial: the constraint's persistence depends on active enforcement against new plural marriages (excommunication, temple recommend denial) and on suppressing the fundamentalist dissent that would expose the ambiguity. Theater ratio (0.45) is moderate-high: the performance of 'revelation' vs. 'policy' ambiguity is maintained in official discourse while the practical enforcement machinery targets only new polygamy, not the theological ambiguity itself. Accessibility collapse (0.52) is moderate: alternatives (fundamentalist schism, secular exit) exist but are costly due to identity-lock. Resistance (0.48) is moderate: fundamentalist schisms occurred but remained marginal; most laity accepted the ambiguity.
 *
 * PERSPECTIVAL GAP:
 *   The engine should compute significant seat divergence. From the institutional leadership seat (agenda_setter, arbitrage exit), the constraint appears as genuine coordination — it solved the existential crisis and preserved the community. From the rank-and-file seat (payer, identity_locked), it appears as extraction masked by ambiguity — they lost doctrinal certainty without consent. From the polygamous family seat (payer, trapped), it appears as snare-like extraction — concentrated harm with no voice. The claimed_type (tangled_rope) reflects the structural reality: coordination function IS real (institutional survival), but extraction IS asymmetric and enforced. The leadership's beneficiary status is not performative — they genuinely solved a coordination problem — but the solution extracts from those with no exit.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional leadership is the structural beneficiary (d near 0.0-0.15): they collect institutional survival, doctrinal flexibility, and interpretive monopoly. Their exit is arbitrage — they control the constraint's evolution. Rank-and-file members are payers (d near 0.6-0.7): they bear interpretive uncertainty and legitimacy ambiguity with identity-locked exit — their religious self-concept is fused to the framework the constraint governs. Polygamous families are the most extracted (d near 0.8-0.9): trapped exit, concentrated legal and social costs. Federal authority is an observer (d = 0.5 analytical): they are the exogenous pressure, not coordinated by the constraint. Dissident fundamentalists are excluded (d undefined): their exclusion is the enforcement object.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (federal existential threat) is dead — the legal threat was resolved by 1896 statehood and asset return. Yet the constraint persists and its enforcement intensified (Second Manifesto 1904, Smoot hearings). This is classic mandatrophy: the arrangement outlived its founding justification. The constraint now serves to maintain the prophetic office's authority to define doctrinal boundaries ambiguously — a power that benefits leadership but was not the original mandate. The classification as tangled_rope (not snare) captures this: the coordination function (institutional continuity, schism prevention) remains live, but the extraction has layered onto it. The ambiguity is the extraction mechanism: by never resolving whether D&C 132 is abrogated, leadership retains the ability to invoke it selectively (temple sealings, eternal marriage theology) while disavowing its practice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'Is the hybrid_pragmatic_reading a distinct structural constraint from its siblings, or a meta-reading that synthesizes them?',
    'Test whether the three readings produce different ε values, different beneficiary/victim structures, and different seat classifications when authored as independent constraints. If ε differs materially, they are distinct constraints per ε-invariance.',
    'If distinct, each reading gets its own constraint story with independent metrics and classification. If synthesis, the hybrid reading is not a separate constraint but a commentary on the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Whether the three declared readings instantiate three ε-invariant constraints or one constraint with three interpretive frames.').

omega_variable(
    ambiguity_as_extraction_mechanism,
    'Is the Manifesto''s scope ambiguity (silence on doctrine vs. practice) a deliberate extraction mechanism or an inevitable feature of crisis management?',
    'Analyze leadership discourse 1890-1910: did they exploit the ambiguity to authorize new sealings (post-Manifesto polygamy) while publicly denying the practice? Compare private minutes vs. public statements.',
    'If deliberate, the ambiguity is engineered extraction — leadership gains flexibility while laity bears uncertainty. If inevitable, the extraction is a byproduct of genuine crisis management, not its purpose.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ambiguity_as_extraction_mechanism, empirical, 'Whether doctrinal ambiguity serves leadership''s extraction or is a crisis-management necessity.').

omega_variable(
    polygamous_families_as_distinct_victim_class,
    'Are polygamous_families a structurally distinct victim class from rank_and_file_members, or a subset with higher extraction intensity?',
    'Measure exit_options divergence: polygamous_families = trapped (geographic relocation failed, public abandonment destroys standing); rank_and_file = identity_locked (can stay in community but with legitimacy uncertainty). If exit atoms differ structurally, they are distinct seats.',
    'If distinct, the constraint has two victim seats with different directionalities, requiring separate χ computation. If subset, single payer seat with intensity gradient.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(polygamous_families_as_distinct_victim_class, empirical, 'Whether the victim structure has one payer seat with gradient or two structurally distinct payer seats.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 1890, 1910).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1890, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 1890, 0.25).
narrative_ontology:measurement(marr_tr_t1895, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 1895, 0.32).
narrative_ontology:measurement(marr_tr_t1900, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 1900, 0.4).
narrative_ontology:measurement(marr_tr_t1904, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 1904, 0.45).
narrative_ontology:measurement(marr_tr_t1907, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 1907, 0.45).
narrative_ontology:measurement(marr_tr_t1910, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 1910, 0.45).

% Extraction over time
narrative_ontology:measurement(marr_be_t1890, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 1890, 0.35).
narrative_ontology:measurement(marr_be_t1895, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 1895, 0.45).
narrative_ontology:measurement(marr_be_t1900, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 1900, 0.52).
narrative_ontology:measurement(marr_be_t1904, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 1904, 0.58).
narrative_ontology:measurement(marr_be_t1907, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 1907, 0.58).
narrative_ontology:measurement(marr_be_t1910, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 1910, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1890, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 1890, 0.4).
narrative_ontology:measurement(marr_su_t1895, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 1895, 0.5).
narrative_ontology:measurement(marr_su_t1900, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 1900, 0.58).
narrative_ontology:measurement(marr_su_t1904, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 1904, 0.62).
narrative_ontology:measurement(marr_su_t1907, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 1907, 0.62).
narrative_ontology:measurement(marr_su_t1910, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 1910, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_legitimacy__hybrid_pragmatic_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.08).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__hybrid_pragmatic_reading, marriage_commitment_legitimacy__endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__hybrid_pragmatic_reading, marriage_commitment_legitimacy__exogenous_override_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the marriage_commitment_legitimacy kernel. The endogenous_reinterpretation_reading claims negligible extraction (genuine revelation, coordination only). The exogenous_override_reading claims high extraction but attributes it to exogenous force (federal coercion), not institutional strategy. This hybrid_pragmatic_reading claims moderate extraction from institutional leadership's strategic deployment of ambiguity. The three readings have different ε values, different beneficiary/victim structures, and different claimed_types — they are distinct constraints per ε-invariance, linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_commitment_legitimacy__hybrid_pragmatic_reading, institutional, 0.1).
constraint_indexing:directionality_override(marriage_commitment_legitimacy__hybrid_pragmatic_reading, organized, 0.65).
constraint_indexing:directionality_override(marriage_commitment_legitimacy__hybrid_pragmatic_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
