% ============================================================================
% CONSTRAINT STORY: biblical_authority__sola_scriptura_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_authority__sola_scriptura_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: biblical_authority__sola_scriptura_reading
 *   human_readable: Sola Scriptura: Scripture as Sufficient and Self-Interpreting Authority
 *   domain: theology/religious_history
 *
 * SUMMARY:
 *   This constraint models the sola scriptura reading of the biblical
 *   authority kernel: the claim that scripture is sufficient and
 *   self-interpreting, requiring no external magisterium, council, or living
 *   tradition to establish authoritative doctrine. This is one of three
 *   sibling readings of the same kernel (the others being the
 *   tradition-scripture reading, in which magisterium guards the deposit of
 *   faith, and the conciliar reading, in which councils and patristic
 *   consensus mediate interpretation). Each reading is authored as its own
 *   constraint with its own ε, beneficiary/victim structure, and
 *   classification; this file concerns only the sola scriptura reading,
 *   assessed by its own lights, not the alternatives it displaced or the
 *   alternatives that displace it in other communities.
 *
 * KEY AGENTS:
 *   - congregational_leadership: local agenda-setters who both benefit from and are structurally dependent on the reading
 *   - lay_believer_autonomy: the reading's primary intended beneficiary
 *   - vernacular_bible_publishers: incidental commercial beneficiary
 *   - cross_community_doctrinal_coherence: the diffuse, non-agent casualty of the reading's lack of adjudicative machinery
 *   - denominational_minority_dissenters: individuals harmed by the absence of an appeal mechanism
 *   - rival_magisterial_authorities: excluded voices whose adjudicative claims this reading's core premise rules out
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_authority__sola_scriptura_reading, 0.28).
domain_priors:suppression_score(biblical_authority__sola_scriptura_reading, 0.32).
domain_priors:theater_ratio(biblical_authority__sola_scriptura_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_authority__sola_scriptura_reading, tangled_rope).
narrative_ontology:human_readable(biblical_authority__sola_scriptura_reading, "Sola Scriptura: Scripture as Sufficient and Self-Interpreting Authority").
narrative_ontology:topic_domain(biblical_authority__sola_scriptura_reading, "theology/religious_history").

domain_priors:requires_active_enforcement(biblical_authority__sola_scriptura_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_authority__sola_scriptura_reading, '0c545f58-5eb1-4ff7-83db-d620e1f01312').
narrative_ontology:cs_kernel_codification('0c545f58-5eb1-4ff7-83db-d620e1f01312', fixed_text).
narrative_ontology:cs_authority_grounding('0c545f58-5eb1-4ff7-83db-d620e1f01312', distributed).
narrative_ontology:cs_reading_relation('0c545f58-5eb1-4ff7-83db-d620e1f01312', biblical_authority__tradition_scripture_reading, forecloses).
narrative_ontology:cs_reading_relation('0c545f58-5eb1-4ff7-83db-d620e1f01312', biblical_authority__conciliar_reading, coexists_with).
narrative_ontology:cs_axiom('0c545f58-5eb1-4ff7-83db-d620e1f01312', foundational, scripture_alone_sufficient_for_doctrine).
narrative_ontology:cs_axiom_status(scripture_alone_sufficient_for_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('0c545f58-5eb1-4ff7-83db-d620e1f01312', scripture_alone_sufficient_for_doctrine, deontological).
narrative_ontology:cs_axiom('0c545f58-5eb1-4ff7-83db-d620e1f01312', foundational, scripture_self_interpreting_without_magisterial_mediation).
narrative_ontology:cs_axiom_status(scripture_self_interpreting_without_magisterial_mediation, holdable).
narrative_ontology:cs_axiom_grounding('0c545f58-5eb1-4ff7-83db-d620e1f01312', scripture_self_interpreting_without_magisterial_mediation, conventional).
narrative_ontology:cs_reference_frame('0c545f58-5eb1-4ff7-83db-d620e1f01312', apostolic_scripture_as_complete_and_final_deposit).
narrative_ontology:cs_drift_state('0c545f58-5eb1-4ff7-83db-d620e1f01312', contemporary_denominational_landscape, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0c545f58-5eb1-4ff7-83db-d620e1f01312', '').
narrative_ontology:cs_kernel_id(biblical_authority__sola_scriptura_reading, biblical_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, lay_believer_autonomy).
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, congregational_leadership).
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, vernacular_bible_publishers).
narrative_ontology:constraint_victim(biblical_authority__sola_scriptura_reading, cross_community_doctrinal_coherence).
narrative_ontology:constraint_victim(biblical_authority__sola_scriptura_reading, denominational_minority_dissenters).
narrative_ontology:constraint_victim(biblical_authority__sola_scriptura_reading, laity_navigating_competing_interpretations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, laity_navigating_competing_interpretations).
narrative_ontology:constraint_vindicates(biblical_authority__sola_scriptura_reading, perspicuity_of_scripture).
narrative_ontology:constraint_vindicates(biblical_authority__sola_scriptura_reading, priesthood_of_all_believers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Pastors and elders in congregationalist and low-church traditions derive their authority directly from their own reading of the text rather than from an external magisterium or council. They set doctrine for their community, ordain their own successors, and answer to no higher adjudicative body. This frees them from tithes and oversight fees owed upward to a hierarchy, but also means their authority evaporates the moment a rival reading persuades their congregation to leave.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, congregational_leadership, agenda_setter,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(biblical_authority__sola_scriptura_reading, congregational_leadership, beneficiary).

% Individual believers gain the standing to read scripture themselves, in their own language, and to judge doctrine without a priestly intermediary certifying the correct interpretation. This is liberating relative to a system requiring clerical mediation, but it also means the believer bears the full cognitive and spiritual weight of adjudicating among competing claims with no external check besides their own conscience and community.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, lay_believer_autonomy, beneficiary,
    moderate, biographical, mobile, national).

% Printers, translators, and later denominational publishing houses profit from a theological commitment that mandates every believer possess and personally engage a vernacular text. Their commercial interest is served by the doctrine independent of whether it resolves any given interpretive dispute.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, vernacular_bible_publishers, beneficiary,
    organized, generational, arbitrage, national).

% There is no actor here, only an outcome: a shared, stable doctrinal consensus across congregations bearing the name of the same faith. Under this reading there is no adjudicative monopoly capable of resolving disputes about baptism, communion, church governance, or eschatology once two sincere readers disagree. The predictable result is schism — thousands of denominations each claiming the same sole authority arrived at incompatible conclusions.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, cross_community_doctrinal_coherence, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(biblical_authority__sola_scriptura_reading, cross_community_doctrinal_coherence).

% Members whose reading of scripture diverges from their congregation's majority interpretation have no external court of appeal — no council, no magisterium — to which they can bring their case. Their options are submission, silent dissent, or departure to found or join yet another congregation, each exit further fragmenting the coherence of the tradition they still consider themselves part of.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, denominational_minority_dissenters, payer,
    powerless, biographical, constrained, local).

% Ordinary believers seeking doctrinal guidance face a marketplace of mutually contradictory congregations each claiming scripture alone as authority, with no principled way (internal to the reading) to adjudicate between them beyond further private judgment. This freedom is also a cost: they must choose without a reliable arbiter, and error carries no institutional correction mechanism.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, laity_navigating_competing_interpretations, payer,
    powerless, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(biblical_authority__sola_scriptura_reading, laity_navigating_competing_interpretations, beneficiary).

% Councils, magisteria, and living-tradition interpretive bodies from the sibling readings are structurally excluded from having any adjudicative standing under this reading — their historical claims to guard the deposit of faith are treated as, at best, useful commentary and at worst as usurpations of an authority scripture reserves to itself and the individual conscience. They would object that removing the adjudicative body is precisely what produces the fragmentation this reading treats as an acceptable cost.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, rival_magisterial_authorities, excluded,
    institutional, civilizational, trapped, global).

% Study the doctrine's emergence as a polemical tool against a specific historical magisterial claim, its subsequent institutionalization within Protestant traditions, and its long-run fragmentation effects, without needing to adjudicate its theological truth.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, historians_of_the_reformation, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Removes a single point of interpretive failure (a corruptible or politically captured magisterium) by distributing interpretive authority to every believer and congregation, coordinated only by shared appeal to a common text.
% TRANSFER_FUNCTION: Moves interpretive authority and its attendant burdens away from centralized clerical hierarchies and toward individual believers and local congregations; moves commercial opportunity toward vernacular publishers; moves the cost of unresolved doctrinal disputes onto dissenting minorities and onto the coherence of the tradition as a whole.
% ABSENT_VOICES: Rival magisterial and conciliar authorities are excluded by the reading's own core premise — they have no standing to adjudicate under sola scriptura, so their historical argument that removing an adjudicative body produces disorder is heard, if at all, only as an external critique, never as an internal check.
% DISAPPEARANCE_RATIONALE: If sola scriptura ceased to function as the operative authority claim, thousands of independent congregations and denominations that derive their legitimacy directly from unmediated scriptural reading would lose their founding warrant; vernacular publishing markets built around individual devotional reading would restructure; and some form of adjudicative body (formal or informal) would likely re-emerge to manage the doctrinal disputes currently left unresolved.
% FOUNDING_PROBLEM: A perceived corruption and doctrinal overreach in the late medieval Western church, where indulgence sales, extra-scriptural doctrine, and a magisterium unaccountable to the text itself were seen as extracting from believers without scriptural warrant.
% FOUNDING_PROBLEM_CORROBORATION: Confessional Protestant historians and denominational leaders attest the founding problem (clerical overreach) remains a live concern justifying continued reliance on scripture alone. Historians of religion outside any confessional commitment, along with sociologists of religion studying denominational fragmentation, corroborate that the specific medieval abuses motivating the reading have largely been addressed by internal Catholic reform (post-Trent, Vatican II) even within the tradition sola scriptura was framed against, while documenting that the fragmentation cost of the reading itself has continued to compound independently of that original problem's status.
narrative_ontology:disappearance_verdict(biblical_authority__sola_scriptura_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_authority__sola_scriptura_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_authority__sola_scriptura_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(biblical_authority__sola_scriptura_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_authority__sola_scriptura_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_authority__sola_scriptura_reading_tests).
:- end_tests(biblical_authority__sola_scriptura_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.28) because the reading's defining structural move is the removal of a clerical intermediary layer that could extract tithes, fees, or obedience in exchange for interpretive certification — congregational leadership under this reading has comparatively little coercive leverage over departing members. Suppression is moderate (0.32), reflecting real but largely local and social (not centralized-coercive) pressure exerted within individual congregations against dissenters, rather than a civilization-spanning enforcement apparatus. Theater ratio is low-moderate and rises modestly over the interval as denominational institutions built atop the reading (seminaries, confessional statements, denominational courts) accrete some performative function despite the reading's anti-institutional premise. Accessibility collapse is deliberately authored lower than a genuine mountain (0.4) because alternative readings of the same kernel remain visibly, persistently live — the whole diagnostic point of this kernel is that none of the three readings has collapsed the others. Resistance is comparatively high (0.55) because the reading has always existed in active contest with magisterial and conciliar claimants, both historically and in the present.
 *
 * DIRECTIONALITY LOGIC:
 *   Lay believers and congregational leadership sit near the beneficiary end: the reading removes an external rent-collector (a magisterium) and redistributes interpretive standing to them directly. Vernacular publishers are incidental beneficiaries riding on the same structural shift. The victim side is unusual: the primary payer is not an individual actor but an outcome — cross-community doctrinal coherence — modeled as a non-agent entity, since the loss of coherence is diffuse and borne collectively rather than extracted by any identifiable party. Denominational minority dissenters are the clearest individual victims: powerless, locally scoped, with constrained exit (leaving means founding or finding yet another congregation, which perpetuates rather than resolves the fragmentation).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — clerical overreach and extra-scriptural doctrinal accretion in the late medieval Western church — is genuinely contested as to its current liveness: internal Catholic reform has addressed much of what motivated the original protest, even by the lights of historians outside any confessional stake, while the fragmentation cost the reading generates has continued to compound on its own trajectory, independent of whether the original problem persists. This is exactly the mismatch structure the R5 genealogy interview is built to surface: founding_problem_status is authored 'contested' rather than 'live' or 'dead' because different corroborating sources genuinely disagree, and the disappearance_verdict ('world_rearranges') registers that real institutional structures now depend on the reading regardless of the founding problem's current status — a live-institution/contested-founding-problem combination rather than a dead-mandate zombie case.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    perspicuity_versus_fragmentation_causality,
    'Does the doctrine of scriptural perspicuity (that scripture is clear enough to be self-interpreting) genuinely produce doctrinal convergence among sincere readers, or does the reading''s own core premise causally guarantee the fragmentation observed across sola-scriptura-derived denominations?',
    'Comparative denominational sociology: track doctrinal divergence rates in traditions operating under sola scriptura versus traditions retaining an adjudicative magisterium or conciliar body, controlling for other schismatic pressures (political, cultural, linguistic).',
    'If fragmentation tracks the absence of an adjudicative body rather than incidental historical accident, this strengthens the case that cross_community_doctrinal_coherence is a structural victim of the reading''s design rather than a contingent side effect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(perspicuity_versus_fragmentation_causality, empirical, 'Whether fragmentation is a structural consequence of the reading or a contingent historical accident.').

omega_variable(
    sibling_reading_framing_alternative,
    'Could the kernel be more accurately framed not as three readings of ''who interprets scripture'' but as two readings of a prior question — ''is doctrinal unity a value the arrangement should optimize for at all'' — with sola scriptura simply declining to treat unity as the governing value the other readings assume?',
    'Compare confessional self-descriptions: do sola scriptura communities explicitly disclaim doctrinal unity as a goal (treating diversity of interpretation as an acceptable or even desirable consequence of individual conscience), or do they claim their reading should also produce unity and treat fragmentation as an unintended failure?',
    'If sola scriptura communities disclaim unity as a goal, the ''victim'' framing of cross_community_doctrinal_coherence weakens considerably, since a value not held cannot properly be counted as extracted from. If they claim unity as an unmet goal, the victim framing is strengthened. The framing choice was made based on classical Reformation polemics (which frequently do claim perspicuity should yield substantial agreement on essentials) rather than later pluralist accommodations within the tradition; a different corpus of sources could shift this.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_framing_alternative, conceptual, 'Whether doctrinal unity is a value this reading actually claims to deliver, affecting whether its absence counts as a victim cost.').

omega_variable(
    congregational_leadership_extraction_ceiling,
    'Does removing a centralized magisterium eliminate clerical extraction, or does it merely decentralize and locally re-concentrate it (charismatic pastors, radio/media ministries, prosperity-gospel variants) in ways the reading''s own metrics understate by focusing on the absence of a formal hierarchy?',
    'Survey financial and authority-concentration patterns across a representative sample of sola-scriptura-derived congregations and parachurch ministries, comparing per-capita clerical extraction to hierarchical-tradition baselines.',
    'If local re-concentration is substantial, the authored low extractiveness (0.28) is a measurement of the formal structure only and undercounts real-world extraction that migrates to informal or charismatic authority figures; this would argue for a higher ε on empirical grounds without changing the reading''s structural claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(congregational_leadership_extraction_ceiling, empirical, 'Whether decentralization actually reduces extraction or merely relocates it to informal authority figures.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_authority__sola_scriptura_reading, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_authority__sola_scriptura_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(bibl_tr_t0, observed).
narrative_ontology:measurement(bibl_tr_t100, biblical_authority__sola_scriptura_reading, theater_ratio, 100, 0.13).
narrative_ontology:measurement_basis(bibl_tr_t100, observed).
narrative_ontology:measurement(bibl_tr_t200, biblical_authority__sola_scriptura_reading, theater_ratio, 200, 0.16).
narrative_ontology:measurement_basis(bibl_tr_t200, observed).
narrative_ontology:measurement(bibl_tr_t300, biblical_authority__sola_scriptura_reading, theater_ratio, 300, 0.18).
narrative_ontology:measurement_basis(bibl_tr_t300, observed).
narrative_ontology:measurement(bibl_tr_t400, biblical_authority__sola_scriptura_reading, theater_ratio, 400, 0.2).
narrative_ontology:measurement_basis(bibl_tr_t400, observed).
narrative_ontology:measurement(bibl_tr_t500, biblical_authority__sola_scriptura_reading, theater_ratio, 500, 0.22).
narrative_ontology:measurement_basis(bibl_tr_t500, observed).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_authority__sola_scriptura_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(bibl_be_t0, observed).
narrative_ontology:measurement(bibl_be_t100, biblical_authority__sola_scriptura_reading, base_extractiveness, 100, 0.18).
narrative_ontology:measurement_basis(bibl_be_t100, observed).
narrative_ontology:measurement(bibl_be_t200, biblical_authority__sola_scriptura_reading, base_extractiveness, 200, 0.2).
narrative_ontology:measurement_basis(bibl_be_t200, observed).
narrative_ontology:measurement(bibl_be_t300, biblical_authority__sola_scriptura_reading, base_extractiveness, 300, 0.23).
narrative_ontology:measurement_basis(bibl_be_t300, observed).
narrative_ontology:measurement(bibl_be_t400, biblical_authority__sola_scriptura_reading, base_extractiveness, 400, 0.26).
narrative_ontology:measurement_basis(bibl_be_t400, observed).
narrative_ontology:measurement(bibl_be_t500, biblical_authority__sola_scriptura_reading, base_extractiveness, 500, 0.28).
narrative_ontology:measurement_basis(bibl_be_t500, observed).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_authority__sola_scriptura_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(bibl_su_t0, observed).
narrative_ontology:measurement(bibl_su_t100, biblical_authority__sola_scriptura_reading, suppression_requirement, 100, 0.27).
narrative_ontology:measurement_basis(bibl_su_t100, observed).
narrative_ontology:measurement(bibl_su_t200, biblical_authority__sola_scriptura_reading, suppression_requirement, 200, 0.28).
narrative_ontology:measurement_basis(bibl_su_t200, observed).
narrative_ontology:measurement(bibl_su_t300, biblical_authority__sola_scriptura_reading, suppression_requirement, 300, 0.3).
narrative_ontology:measurement_basis(bibl_su_t300, observed).
narrative_ontology:measurement(bibl_su_t400, biblical_authority__sola_scriptura_reading, suppression_requirement, 400, 0.31).
narrative_ontology:measurement_basis(bibl_su_t400, observed).
narrative_ontology:measurement(bibl_su_t500, biblical_authority__sola_scriptura_reading, suppression_requirement, 500, 0.32).
narrative_ontology:measurement_basis(bibl_su_t500, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_authority__sola_scriptura_reading, identity_coordination).
narrative_ontology:affects_constraint(biblical_authority__sola_scriptura_reading, tradition_scripture_reading).
narrative_ontology:affects_constraint(biblical_authority__sola_scriptura_reading, conciliar_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the biblical_authority kernel. tradition_scripture_reading and conciliar_reading are separate constraint files with their own ε, beneficiary/victim structures, and classifications. The three form a constraint family connected by shared origin in a contested kernel (the nature and locus of interpretive authority over scripture) rather than by causal dependency; each reading's operation structurally pressures the others' legitimacy claims and resource base within contested denominational and ecumenical space.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
