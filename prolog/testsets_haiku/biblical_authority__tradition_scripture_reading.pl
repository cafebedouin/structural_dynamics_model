% ============================================================================
% CONSTRAINT STORY: biblical_authority__tradition_scripture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_authority__tradition_scripture_reading, []).

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
 *   constraint_id: biblical_authority__tradition_scripture_reading
 *   human_readable: Scripture-Tradition-Magisterium Authority Structure
 *   domain: theology/religious/institutional
 *
 * SUMMARY:
 *   The tradition-scripture-magisterium reading instantiates a specific
 *   authority structure for interpreting Christian Scripture. It claims that
 *   Scripture cannot be understood correctly without the living interpretive
 *   tradition (the consensus of the fathers, the teaching of the councils,
 *   the living magisterium) and that the magisterium (the teaching office of
 *   the Church) is the authorized guardian and authoritative interpreter of
 *   both Scripture and tradition. This reading emerged fully in the medieval
 *   period and was formally codified at the Council of Trent (1545-1563) in
 *   response to the Protestant Reformation's challenge via sola scriptura.
 *   The kernel — the contested claim about how Scripture, tradition, and
 *   authority relate — remains live: Catholic and Orthodox traditions
 *   maintain the tradition-magisterium reading; Protestant and evangelical
 *   traditions reject it in favor of sola scriptura; some liberal Protestant
 *   and Anglican traditions adopt a conciliar-patristic reading that
 *   emphasizes consensus and ecumenical continuity over magisterial decree.
 *   This constraint story captures ONLY the tradition-scripture-reading as a
 *   clean, ε-invariant constraint. The siblings (sola_scriptura_reading,
 *   conciliar_reading) are OTHER constraints, authoring their own ε,
 *   beneficiaries, and structural data. They are linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - magisterial_authority: institutional agenda-setter, powerful, civilizational horizon — sets/enforces the rules for legitimate scriptural interpretation, binds the faithful to magisterial pronouncements
 *   - clerical_hierarchy: institutional beneficiary + agenda-setter, identity-locked, civilizational horizon — mediates sacramental grace, interprets Scripture authoritatively under magisterial oversight, their status is constituted by the constraint
 *   - lay_faithful: organized payer + beneficiary, constrained exit, biographical horizon — receive sacraments and doctrine but are forbidden independent scriptural interpretation
 *   - independent_interpreters: moderate payer + excluded, trapped exit, biographical horizon — scholars and dissenting clergy suppressed by institutional mechanisms
 *   - lay_mystics_and_prophets: powerless excluded, trapped exit, local scope — lay charisms are subordinated to clerical discernment
 *   - rival_religious_communities: powerful excluded, trapped exit, global scope — Protestant, Orthodox, and other traditions rejected as illegitimate interpreters
 *   - analytical_observer: observer, analytical power — historians and scholars examining the constraint from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_authority__tradition_scripture_reading, 0.68).
domain_priors:suppression_score(biblical_authority__tradition_scripture_reading, 0.72).
domain_priors:theater_ratio(biblical_authority__tradition_scripture_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_authority__tradition_scripture_reading, tangled_rope).
narrative_ontology:human_readable(biblical_authority__tradition_scripture_reading, "Scripture-Tradition-Magisterium Authority Structure").
narrative_ontology:topic_domain(biblical_authority__tradition_scripture_reading, "theology/religious/institutional").

domain_priors:requires_active_enforcement(biblical_authority__tradition_scripture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_authority__tradition_scripture_reading, '6a76f326-aad2-4ddb-8cef-62db6819e8e6').
narrative_ontology:cs_kernel_codification('6a76f326-aad2-4ddb-8cef-62db6819e8e6', formalized).
narrative_ontology:cs_authority_grounding('6a76f326-aad2-4ddb-8cef-62db6819e8e6', extraction).
narrative_ontology:cs_interpretation_layer_present('6a76f326-aad2-4ddb-8cef-62db6819e8e6').
narrative_ontology:cs_reading_relation('6a76f326-aad2-4ddb-8cef-62db6819e8e6', biblical_authority__sola_scriptura_reading, forecloses).
narrative_ontology:cs_reading_relation('6a76f326-aad2-4ddb-8cef-62db6819e8e6', biblical_authority__conciliar_reading, coexists_with).
narrative_ontology:cs_axiom('6a76f326-aad2-4ddb-8cef-62db6819e8e6', foundational, tradition_co_revelatory_with_scripture).
narrative_ontology:cs_axiom_status(tradition_co_revelatory_with_scripture, holdable).
narrative_ontology:cs_axiom_grounding('6a76f326-aad2-4ddb-8cef-62db6819e8e6', tradition_co_revelatory_with_scripture, deontological).
narrative_ontology:cs_axiom('6a76f326-aad2-4ddb-8cef-62db6819e8e6', foundational, magisterium_unilaterally_authoritative_interpreter).
narrative_ontology:cs_axiom_status(magisterium_unilaterally_authoritative_interpreter, holdable).
narrative_ontology:cs_axiom_grounding('6a76f326-aad2-4ddb-8cef-62db6819e8e6', magisterium_unilaterally_authoritative_interpreter, deontological).
narrative_ontology:cs_reference_frame('6a76f326-aad2-4ddb-8cef-62db6819e8e6', apostolic_continuity_via_magisterial_guardianship).
narrative_ontology:cs_drift_state('6a76f326-aad2-4ddb-8cef-62db6819e8e6', post_reformation_and_modern_scholarship, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6a76f326-aad2-4ddb-8cef-62db6819e8e6', '').
narrative_ontology:cs_kernel_id(biblical_authority__tradition_scripture_reading, biblical_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_authority__tradition_scripture_reading, clerical_hierarchy).
narrative_ontology:constraint_beneficiary(biblical_authority__tradition_scripture_reading, magisterial_authority).
narrative_ontology:constraint_victim(biblical_authority__tradition_scripture_reading, lay_faithful).
narrative_ontology:constraint_victim(biblical_authority__tradition_scripture_reading, independent_interpreters).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_authority__tradition_scripture_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(biblical_authority__tradition_scripture_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_authority__tradition_scripture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_authority__tradition_scripture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_authority__tradition_scripture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at interval end) reflects the substantial concentration of interpretive authority in the magisterium and the structural dependence of lay salvation on clerical mediation. This is not primarily economic extraction (though tithes and fees are present in historical practice) but interpretive and spiritual extraction: the lay faithful cannot directly read Scripture for truth; they must accept the magisterium's authoritative interpretation. Suppression (0.72) is correspondingly high because the constraint's persistence depends actively on excluding alternative readings — inquisitorial investigation, book bans, excommunication of dissenting theologians, and internal discipline of clergy who step out of line. Theater (0.41) is moderate-low because the constraint performs real coordination work (doctrinal unity is genuine) but an increasing share of enforcement activity from ~400 onward is devoted to defending magisterial authority itself rather than solving the original doctrinal fragmentation problem. The time series (0-2000) shows extraction accumulating from ~0.35 in the early apostolic period (when magisterial authority was minimal and tradition was living consensus) to 0.68 by the early modern period (after the full crystallization of the hierarchy and the Tridentine codification). Suppression grows steeper (0.28 to 0.72) as institutional mechanisms harden to defend against Protestant challenge. Theater rises gradually as the constraint becomes increasingly self-referential (defending magisterial authority as such, rather than solving doctrinal fragmentation). The coercion grid shows asymmetric pressure: suppression at the individual level stays lower (0.68 at t=2000) than at the structural level (0.78) because the constraint operates partly through internalization (the lay faithful internalize the teaching that they lack interpretive competence); resistance at the individual level (0.54 at t=2000) is lower than organizational resistance (0.58) because the powerless are harder to organize and the constraint locks identity. Accessibility collapse rises across all levels but highest at the structural and stakes-inflation levels (0.72-0.76 at t=2000) because the constraint closes off the entire alternative possibility-space (that a Christian could read Scripture faithfully without magisterial permission).
 *
 * PERSPECTIVAL GAP:
 *   The magisterial authority and clerical hierarchy seats should compute this as genuine coordination (rope) from their internal perspective: they truly believe they are guarding apostolic continuity and preventing doctrinal chaos. From the lay faithful's seat, it should compute as constrained but coordinated (partial beneficiary from sacramental access, partial payer for interpretive constraint). From the independent interpreter's seat, it should compute as snare (trapped, suppressed, excluded). From the analytical observer's seat, it computes as tangled_rope: real coordination benefit (doctrinal unity) yoked to real extraction (magisterial monopoly). The engine computes per-seat: the authored claim (tangled_rope) and the authored metrics should produce divergent seat classifications that the engine's per-seat logic reveals.
 *
 * DIRECTIONALITY LOGIC:
 *   The magisterial authority and clerical hierarchy are clear beneficiaries: they collect interpretive authority, spiritual power, and institutional control. Their directionality (d) should be low — they benefit from the constraint (d near 0.0). The lay faithful are symmetric: they benefit from sacramental access and unified doctrine (low d) but pay through interpretive constraint (higher d) — net near 0.5 or slightly higher. Independent interpreters and lay prophets are clear targets: they are suppressed and excluded (d near 1.0). The analytical observer is unaffected (d = 0.5 analytic). This mixed directionality across seats is what the coercion grid captures: the constraint does different work for different levels and roles. A powerful institutional actor (magisterium) uses the constraint to consolidate power; a lay actor experiences it as coordination with a compliance tax; a powerless actor experiences it as pure suppression.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was real: early Christian communities faced doctrinal fragmentation (Gnosticism, Arianism, Docetism) and needed a way to preserve apostolic continuity. The magisterium-tradition structure was designed to solve this. However, by ~1200 (time 1200 in the interval), the founding problem had substantially been solved: Christian doctrine was stabilized and widely accepted. Yet the constraint persisted and even intensified, as shown by rising extractiveness and suppression. The theater ratio rising from 0.22 to 0.41 indicates increasingly performative activity: defending magisterial authority as such, rather than solving doctrinal fragmentation. The constraint exhibits mandatrophy: the function that justified it (preventing fragmentation) is no longer the live problem, yet the authority structure persists by force and habit. The six_questions answers capture this: the founding_problem_status is 'contested' because the magisterium claims the problem (doctrinal drift, individual interpretation) remains live, while external observers note that the constraint now generates its own problems (suppression of theological development, resistance from educated lay believers). The constraint should be flagged for mandatrophy review: it persists not because the founding problem remains acute but because the beneficiary (clerical hierarchy) has structural interest in its continuance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tradition_continuity_vs_institutional_codification,
    'Is the living tradition a genuine continuous development of apostolic witness, or has it been captured by institutional self-interest and codified into texts (creeds, councils, magisterial decrees) that now represent only power rather than continuity?',
    'Historical analysis of doctrinal development: did the fathers genuinely preserve apostolic teaching, or did they innovate under pressure? Do developments follow organic theological logic or institutional power accumulation? Textual comparison of patristic sources with later magisterial claims about what the tradition ''always held''. Empirical test: Did traditions that broke from magisterial authority (Reformation, schisms) preserve recognizable continuity, or did they fragment into unrecognizable forms?',
    'If tradition represents genuine continuity, the magisterium''s interpretation is binding and the constraint is valid coordination. If tradition has been captured by institutional power, the magisterium is a snare masquerading as rope. This is the key omega that distinguishes whether mandatrophy has occurred.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tradition_continuity_vs_institutional_codification, empirical, 'Whether tradition represents apostolic continuity or institutional consolidation of power.').

omega_variable(
    sacramental_efficacy_independence,
    'Does sacramental grace depend on apostolic succession and magisterial authority, or would sacraments remain efficacious if administered outside the hierarchy?',
    'Theological-empirical: If lay-administered or schismatic sacraments show the same fruits (genuine conversion, spiritual transformation, moral renewal) as hierarchical sacraments, the dependency claim is empirically falsified. If the difference in fruit is zero or unmeasurable, the extraction mechanism (sacramental mediation as the means of grace) loses its structural justification.',
    'If sacraments work outside the hierarchy, the constraint loses its most powerful extraction mechanism: the lay faithful''s dependence on ordained priests for access to grace. The constraint would downgrade from tangled_rope (coordination + extraction) to rope (pure coordination). Lay spiritual agency would be validated and clerical monopoly broken.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sacramental_efficacy_independence, empirical, 'Whether sacramental grace depends on hierarchical mediation or is accessible outside it.').

omega_variable(
    magisterial_accountability_mechanism,
    'Is the magisterium genuinely accountable to Scripture and tradition, or does it have unilateral authority to redefine both in light of its own pronouncements?',
    'Institutional analysis: Can the magisterium be overruled? By whom? By what process? Does it claim infallibility in doctrinal judgment? Has it ever formally reversed its own teaching (not development, but reversal)? Is there an appeal mechanism external to it? Compare actual governance structures with the stated accountability narratives.',
    'If magisterial authority is unilateral and unaccountable, the constraint is structurally a snare: the beneficiary (magisterium) controls the rules and cannot be overridden. If the magisterium is genuinely accountable to councils, Scripture, and tradition, the constraint is more defensibly tangled_rope (coordination with accountability). The difference determines whether the constraint can self-correct or is locked into perpetuating extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(magisterial_accountability_mechanism, empirical, 'Whether the magisterium is accountable to external authority or self-legitimating.').

omega_variable(
    lay_interpreters_vs_clerical_monopoly,
    'Could the coordination benefit of doctrinal unity be preserved while breaking the clerical monopoly on authoritative interpretation?',
    'Comparative institutional: How do traditions that emphasize lay interpretation (some Protestant communities, some Jewish and Muslim schools) handle doctrinal unity? Do they fragment catastrophically, or do they maintain coherence through different mechanisms (lay councils, scholarly consensus, community discernment)? Do schisms occur less frequently in hierarchical or in lay-inclusive systems?',
    'If lay interpretation can preserve coordination without clerical monopoly, the current constraint is extractive by necessity and not by intrinsic requirement. The structure could be reformed to spread interpretive authority while keeping coordination. This would reframe the constraint as not merely tangled_rope but as possibly remediable through structural change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lay_interpreters_vs_clerical_monopoly, empirical, 'Whether clerical monopoly is structurally necessary for doctrinal coordination.').

omega_variable(
    reading_premise_contest,
    'Which sibling reading (sola_scriptura or conciliar) most coherently instantiates apostolic continuity, and does this reading (tradition_scripture_reading) actually preserve what it claims to preserve?',
    'Textual and historical: What did the earliest apostolic fathers actually claim about Scripture, tradition, and authority? Which sibling reading aligns better with patristic sources? Has this tradition_scripture_reading reading been retrospectively imposed on the fathers, or is there genuine continuity? Do the early councils invoke magisterial authority in the way the constraint claims?',
    'If another reading aligns better with apostolic sources, this reading is a misappropriation of authority and a false summit. The constraint would lose its legitimacy and shift toward snare classification. If this reading does align, it gains support as genuinely coordinating. This omega captures the kernel-level uncertainty at which reading actually instantiates the kernel correctly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_premise_contest, conceptual, 'Which reading of biblical authority actually represents apostolic continuity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_authority__tradition_scripture_reading, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_authority__tradition_scripture_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(bibl_tr_t400, biblical_authority__tradition_scripture_reading, theater_ratio, 400, 0.28).
narrative_ontology:measurement(bibl_tr_t800, biblical_authority__tradition_scripture_reading, theater_ratio, 800, 0.33).
narrative_ontology:measurement(bibl_tr_t1200, biblical_authority__tradition_scripture_reading, theater_ratio, 1200, 0.38).
narrative_ontology:measurement(bibl_tr_t1600, biblical_authority__tradition_scripture_reading, theater_ratio, 1600, 0.4).
narrative_ontology:measurement(bibl_tr_t2000, biblical_authority__tradition_scripture_reading, theater_ratio, 2000, 0.41).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_authority__tradition_scripture_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(bibl_be_t400, biblical_authority__tradition_scripture_reading, base_extractiveness, 400, 0.52).
narrative_ontology:measurement(bibl_be_t800, biblical_authority__tradition_scripture_reading, base_extractiveness, 800, 0.61).
narrative_ontology:measurement(bibl_be_t1200, biblical_authority__tradition_scripture_reading, base_extractiveness, 1200, 0.66).
narrative_ontology:measurement(bibl_be_t1600, biblical_authority__tradition_scripture_reading, base_extractiveness, 1600, 0.67).
narrative_ontology:measurement(bibl_be_t2000, biblical_authority__tradition_scripture_reading, base_extractiveness, 2000, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_authority__tradition_scripture_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(bibl_su_t400, biblical_authority__tradition_scripture_reading, suppression_requirement, 400, 0.45).
narrative_ontology:measurement(bibl_su_t800, biblical_authority__tradition_scripture_reading, suppression_requirement, 800, 0.58).
narrative_ontology:measurement(bibl_su_t1200, biblical_authority__tradition_scripture_reading, suppression_requirement, 1200, 0.68).
narrative_ontology:measurement(bibl_su_t1600, biblical_authority__tradition_scripture_reading, suppression_requirement, 1600, 0.71).
narrative_ontology:measurement(bibl_su_t2000, biblical_authority__tradition_scripture_reading, suppression_requirement, 2000, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_authority__tradition_scripture_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(biblical_authority__tradition_scripture_reading, 0.16).
narrative_ontology:affects_constraint(biblical_authority__tradition_scripture_reading, biblical_authority__sola_scriptura_reading).
narrative_ontology:affects_constraint(biblical_authority__tradition_scripture_reading, biblical_authority__conciliar_reading).

% DUAL FORMULATION NOTE:
% The biblical_authority kernel is instantiated in three separate constraint stories, one per reading, rather than authoring 'biblical authority' as a single constraint with three measurement observables. Each reading has a structurally distinct ε value, beneficiary/victim set, and claim-metric profile. The tradition_scripture_reading (this story) features institutional beneficiaries (magisterium, clerical hierarchy) and lay victims (restricted interpretive agency). The sola_scriptura_reading features lay beneficiaries (interpretive agency) and clerical victims (authority loss). The conciliar_reading features diffuse beneficiaries (ecumenical consensus) and concentrated victims (magisterial unilateral authority). These are not perspectives on one constraint; they are different constraints instantiated from one contested kernel. All three are linked via affects_constraints to enable kernel-family analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biblical_authority__tradition_scripture_reading, powerless, 0.92).
constraint_indexing:directionality_override(biblical_authority__tradition_scripture_reading, moderate, 0.78).
constraint_indexing:directionality_override(biblical_authority__tradition_scripture_reading, organized, 0.52).
constraint_indexing:directionality_override(biblical_authority__tradition_scripture_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
