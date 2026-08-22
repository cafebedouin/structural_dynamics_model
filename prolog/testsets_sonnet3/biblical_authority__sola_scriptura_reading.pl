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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Sola Scriptura: Scripture as Sufficient, Self-Interpreting Authority
 *   domain: theology/religious_studies/history_of_christianity
 *
 * SUMMARY:
 *   This constraint models the sola scriptura reading of the biblical
 *   authority kernel: Scripture is held to be sufficient in itself and
 *   interpretable by any believer under ordinary illumination, without
 *   requiring a magisterium or conciliar body to certify correct doctrine.
 *   This reading emerged from the sixteenth-century Reformation as a direct
 *   response to perceived clerical corruption and monopolized interpretive
 *   authority, and it structurally trades adjudicative coherence for
 *   interpretive autonomy. The constraint is claimed as a rope: a genuine
 *   coordination solution (a portable, institution-independent doctrinal
 *   standard) rather than a device built to extract from a defined victim
 *   class. The metrics reflect low but non-zero and slowly rising
 *   extractiveness — largely from the diffuse cost fragmentation imposes on
 *   smaller, less-resourced splinter communities — rather than concentrated
 *   clerical extraction.
 *
 * KEY AGENTS:
 *   - lay_believer_autonomy: primary beneficiary (moderate/mobile) — gains interpretive standing
 *   - independent_congregations: beneficiary and local agenda_setter (moderate/mobile) — sets own doctrine, can split freely
 *   - vernacular_bible_publishers: beneficiary (organized/arbitrage) — commercial and missionary interests align with sufficiency doctrine
 *   - doctrinal_coherence_across_communities: primary payer, non-agent (powerless/trapped) — the coherence eroded by unresolved interpretive plurality
 *   - denominational_minority_splinters: secondary payer (powerless/constrained) — bears practical costs of fragmentation
 *   - catholic_and_orthodox_hierarchies: excluded — structurally written out of this reading's account of legitimate authority
 *   - religious_historians: analytical observer — studies doctrinal proliferation as observable consequence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_authority__sola_scriptura_reading, 0.31).
domain_priors:suppression_score(biblical_authority__sola_scriptura_reading, 0.28).
domain_priors:theater_ratio(biblical_authority__sola_scriptura_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_authority__sola_scriptura_reading, rope).
narrative_ontology:human_readable(biblical_authority__sola_scriptura_reading, "Sola Scriptura: Scripture as Sufficient, Self-Interpreting Authority").
narrative_ontology:topic_domain(biblical_authority__sola_scriptura_reading, "theology/religious_studies/history_of_christianity").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_authority__sola_scriptura_reading, 'b5d41e50-06de-4e8f-a6b2-9971707b725d').
narrative_ontology:cs_kernel_codification('b5d41e50-06de-4e8f-a6b2-9971707b725d', fixed_text).
narrative_ontology:cs_authority_grounding('b5d41e50-06de-4e8f-a6b2-9971707b725d', distributed).
narrative_ontology:cs_reading_relation('b5d41e50-06de-4e8f-a6b2-9971707b725d', biblical_authority__tradition_scripture_reading, forecloses).
narrative_ontology:cs_reading_relation('b5d41e50-06de-4e8f-a6b2-9971707b725d', biblical_authority__conciliar_reading, coexists_with).
narrative_ontology:cs_axiom('b5d41e50-06de-4e8f-a6b2-9971707b725d', foundational, scripture_sufficient_without_magisterium).
narrative_ontology:cs_axiom_status(scripture_sufficient_without_magisterium, holdable).
narrative_ontology:cs_axiom_grounding('b5d41e50-06de-4e8f-a6b2-9971707b725d', scripture_sufficient_without_magisterium, deontological).
narrative_ontology:cs_axiom('b5d41e50-06de-4e8f-a6b2-9971707b725d', foundational, scripture_self_interpreting_under_ordinary_illumination).
narrative_ontology:cs_axiom_status(scripture_self_interpreting_under_ordinary_illumination, holdable).
narrative_ontology:cs_axiom_grounding('b5d41e50-06de-4e8f-a6b2-9971707b725d', scripture_self_interpreting_under_ordinary_illumination, conventional).
narrative_ontology:cs_reference_frame('b5d41e50-06de-4e8f-a6b2-9971707b725d', apostolic_text_as_sole_rule_of_faith).
narrative_ontology:cs_drift_state('b5d41e50-06de-4e8f-a6b2-9971707b725d', contemporary_denominational_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b5d41e50-06de-4e8f-a6b2-9971707b725d', '').
narrative_ontology:cs_kernel_id(biblical_authority__sola_scriptura_reading, biblical_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, lay_believer_autonomy).
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, vernacular_bible_publishers).
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, independent_congregations).
narrative_ontology:constraint_victim(biblical_authority__sola_scriptura_reading, doctrinal_coherence_across_communities).
narrative_ontology:constraint_victim(biblical_authority__sola_scriptura_reading, denominational_minority_splinters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, reformation_era_clergy).
narrative_ontology:constraint_victim(biblical_authority__sola_scriptura_reading, reformation_era_clergy).
narrative_ontology:constraint_vindicates(biblical_authority__sola_scriptura_reading, perspicuity_of_scripture).
narrative_ontology:constraint_vindicates(biblical_authority__sola_scriptura_reading, priesthood_of_all_believers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individual believers and congregations gain the standing to read, interpret, and apply Scripture themselves without requiring a clerical or conciliar intermediary to certify their conclusions as legitimate. This is the reading's central benefit: interpretive authority devolves from institution to the individual reader guided by the text and the Spirit.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, lay_believer_autonomy, beneficiary,
    moderate, biographical, mobile, national).

% Local congregations and their elders/pastors set their own doctrinal statements by direct scriptural argument, free to found new fellowships or split from existing ones without needing permission from a bishop, council, or magisterium. They administer their own reading of the text, which is both their freedom and the mechanism by which they generate new splinters.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, independent_congregations, beneficiary,
    moderate, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(biblical_authority__sola_scriptura_reading, independent_congregations, agenda_setter).

% Printers, translators, and later broadcast/publishing ministries benefit directly from a theology that mandates broad lay access to Scripture in the vernacular; their commercial and missionary interests align with and amplify the sufficiency claim.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, vernacular_bible_publishers, beneficiary,
    organized, generational, arbitrage, global).

% A shared, cross-communal doctrinal settlement is the cost side of this reading: without an adjudicative body empowered to settle interpretive disputes, the same texts generate mutually exclusive doctrines (baptism, eucharist, church governance, eschatology) with no internal mechanism to resolve the disagreement short of separation. Coherence across the wider communion is what erodes as autonomy expands.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, doctrinal_coherence_across_communities, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(biblical_authority__sola_scriptura_reading, doctrinal_coherence_across_communities).

% Smaller breakaway groups formed from good-faith scriptural disagreement often lack the institutional resources, legal standing, or social capital of established denominations; they bear the practical costs of fragmentation — isolation, resource scarcity, vulnerability to charismatic or authoritarian local leadership with no external check.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, denominational_minority_splinters, payer,
    powerless, biographical, constrained, local).

% Reformers and their successors gained release from obligations to a hierarchical magisterium they regarded as corrupted, but also lost the institutional infrastructure, property, and continuity that hierarchical authority had provided; some rebuilt confessional bureaucracies (e.g. Reformed and Lutheran state churches) that partially re-imported adjudicative authority under a new banner.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, reformation_era_clergy, beneficiary,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(biblical_authority__sola_scriptura_reading, reformation_era_clergy, payer).

% Bodies holding that authoritative interpretation requires apostolic succession, conciliar decree, or living tradition are structurally written out of this reading's account of legitimate authority; from within sola scriptura's framework, their interpretive claims carry no more inherent weight than any other reader's, which they experience as an erasure of a a real historical function they perform.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, catholic_and_orthodox_hierarchies, excluded,
    institutional, civilizational, trapped, global).

% Study the doctrinal proliferation (tens of thousands of distinct Protestant denominations globally) as an observable consequence of removing an adjudicative monopoly, and assess whether perspicuity functions as advertised or is itself doing interpretive work that a hidden confessional tradition supplies.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, religious_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides believers a shared, portable, non-institutionally-mediated standard (the biblical text itself) they can appeal to across time and place without dependence on a particular clerical hierarchy's continuity or good faith — solving the coordination problem of how doctrine survives institutional corruption or collapse.
% TRANSFER_FUNCTION: Moves interpretive authority and the associated social capital away from clerical/conciliar office-holders and toward individual readers and local congregations; correspondingly moves the cost of resolving interpretive disagreement away from a central adjudicator and onto each community, which must generate its own resolution or split.
% ABSENT_VOICES: Catholic and Orthodox theologians, and adherents of confessional traditions built on conciliar authority, would object that the reading's account of 'plain meaning' smuggles in an unacknowledged interpretive tradition (early Protestant confessions, translation choices, cultural assumptions) while denying that any tradition is doing this work; they are not parties to this reading's internal self-justification.
% DISAPPEARANCE_RATIONALE: If the sufficiency-and-perspicuity claim were abandoned by its adherent communities, the theological warrant for congregational independence, lay preaching authority, and denominational proliferation without ecclesial permission would collapse; communities would need to locate interpretive authority elsewhere (a magisterium, a council, a charismatic leader, a confession treated as binding) and much of low-church Protestant institutional structure would need to be rebuilt on different grounds.
% FOUNDING_PROBLEM: Sixteenth-century reformers held that a hierarchical church claiming interpretive monopoly had entrenched doctrines and practices (indulgences, clerical financial abuses, restricted vernacular access to text) that Scripture itself did not support, and that lay believers had no legitimate avenue to contest this because interpretive authority was concentrated in the hierarchy being contested.
% FOUNDING_PROBLEM_CORROBORATION: Reformation historians (including scholars outside confessional Protestant institutions) corroborate that clerical financial abuse and restricted scriptural access were real historical grievances at the founding moment. Catholic and Orthodox theologians, from outside the benefiting tradition, argue the founding problem was a genuine but narrower institutional-corruption problem that did not require abandoning interpretive authority as such, and that the doctrine has since generated a distinct, unresolved problem (fragmentation) that the tradition's own advocates rarely treat as a cost of the same magnitude as the original grievance.
narrative_ontology:disappearance_verdict(biblical_authority__sola_scriptura_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_authority__sola_scriptura_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_authority__sola_scriptura_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(biblical_authority__sola_scriptura_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_authority__sola_scriptura_reading, 0.31, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored low (0.31 at interval end) because no concentrated party collects rent from the arrangement in the way a clerical hierarchy might under the tradition-scripture reading; the cost instead falls as a diffuse, structural erosion of cross-communal doctrinal coherence — a genuine cost but not a captured one. Suppression is authored moderate-low (0.28): the reading imposes little coercive apparatus on believers (no inquisition, no excommunication machinery tied to the doctrine itself), though social pressure within specific congregations toward a 'plain reading' can operate coercively at the local level. Theater ratio is low-moderate and rising slowly (0.10 to 0.22) reflecting increasing institutional apparatus (denominational seminaries, confession-writing bodies) that partially re-imports adjudicative functions while still claiming pure scriptural sufficiency — a mild but real Goodhart drift. Accessibility collapse is moderate (0.35): alternatives to lay interpretive authority (submission to a magisterium) remain fully visible and chosen against, not foreclosed. Resistance is comparatively high (0.62) because this reading is permanently contested by conciliar and tradition-scripture communities who regard the doctrine as internally incoherent (claiming no tradition informs interpretation while manifestly relying on one).
 *
 * PERSPECTIVAL GAP:
 *   From within the reading's own framework, this looks like liberation: a rope solving the coordination problem of doctrinal survival independent of institutional corruption. From the excluded conciliar and tradition-scripture seats, the identical structure looks like a tangled or fraying rope — a coordination claim (shared appeal to 'the text') that in practice cannot coordinate because it supplies no mechanism for resolving disagreement, leaving proliferation and fragmentation as the actual observed output. The engine's per-seat computation is expected to diverge along exactly this line: analytical observers weighting the fragmentation cost heavily will compute a less rope-like result than believers weighting autonomy heavily.
 *
 * DIRECTIONALITY LOGIC:
 *   Lay believers, independent congregations, and vernacular publishers sit near the beneficiary end of directionality: the reading transfers interpretive capital to them and they retain mobile or arbitrage-grade exit (they can found new congregations or publish new translations at will). Doctrinal coherence across communities, as a non-agent structural good, and denominational minority splinters, as a powerless and constrained population, sit near the target end: they absorb the cost of unresolved interpretive plurality without holding levers to fix it. Reformation-era clergy are dual-positioned — beneficiaries of release from a corrupted hierarchy, but payers of institutional continuity they had to rebuild from scratch.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (clerical corruption, restricted lay access to Scripture) is substantially resolved in most contemporary contexts where vernacular Scripture is freely available and institutional corruption of the specific sixteenth-century kind is rare; yet the doctrine of sufficiency-and-perspicuity persists at full theological strength, now serving primarily to legitimate an ongoing proliferation of new denominations rather than to solve the original grievance. This is a mild mandatrophy signal, not a full resolution: the founding problem's status is authored as contested (not dead) because episodes of institutional corruption remain a live concern in some traditions, which keeps the doctrine's coordination function from being wholly vestigial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    perspicuity_versus_hidden_tradition,
    'Does ''plain meaning'' interpretation under sola scriptura actually operate without an interpretive tradition, or does it rely on an unacknowledged confessional tradition (translation choices, sixteenth-century confessional assumptions, cultural context) functioning as a de facto magisterium?',
    'Comparative doctrinal analysis of independently-arrived-at ''plain readings'' across isolated sola scriptura communities with no shared confessional lineage; convergence would support genuine perspicuity, while systematic divergence tracking confessional lineage would support the hidden-tradition hypothesis.',
    'If a hidden tradition is doing the interpretive work, the reading''s claimed coordination function (a tradition-independent standard) is partly illusory, and the constraint would sit closer to tangled_rope territory — genuine coordination for insiders of the same confessional lineage, but concealed extraction of interpretive authority from that lineage''s own founders rather than from Scripture alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(perspicuity_versus_hidden_tradition, conceptual, 'Whether perspicuity is genuine or masks an unacknowledged interpretive tradition.').

omega_variable(
    fragmentation_as_feature_or_cost,
    'Is doctrinal fragmentation properly counted as a victim-bearing cost of this reading, or is pluralism itself a value the reading''s adherents would defend as a legitimate feature rather than damage to be minimized?',
    'Survey adherent communities directly: do they characterize denominational proliferation as regrettable but tolerated, or as a positive expression of the priesthood of all believers with no coherence deficit to speak of?',
    'If adherents genuinely do not value cross-communal coherence as a good, then ''doctrinal_coherence_across_communities'' is not a victim in the reading''s own terms, and the ε referent (the standing arrangement assessed by the reading''s own lights) would need lower extraction than authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fragmentation_as_feature_or_cost, preference, 'Whether cross-communal doctrinal coherence is a value internal to this reading or an externally-imposed standard.').

omega_variable(
    coordination_extraction_boundary,
    'Where is the line between the genuine coordination function (a portable doctrinal standard surviving institutional collapse) and the emergent institutional apparatus (denominational seminaries, confessional statements) that quietly re-imports adjudicative authority while denying it does so?',
    'Track the rising theater_ratio against the emergence of binding confessional documents (e.g. Westminster Confession, denominational statements of faith) that function as de facto magisterial texts within specific traditions.',
    'A confirmed re-importation of adjudicative authority under new institutional forms would suggest the constraint''s trajectory bends toward tangled_rope or even a localized version of the tradition_scripture_reading within specific denominational families, undermining the claim that fragmentation is structurally permanent rather than provisionally re-coordinated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether confessional apparatus re-imports the adjudicative function the doctrine claims to reject.').


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
narrative_ontology:measurement(bibl_tr_t100, biblical_authority__sola_scriptura_reading, theater_ratio, 100, 0.14).
narrative_ontology:measurement_basis(bibl_tr_t100, observed).
narrative_ontology:measurement(bibl_tr_t200, biblical_authority__sola_scriptura_reading, theater_ratio, 200, 0.17).
narrative_ontology:measurement_basis(bibl_tr_t200, observed).
narrative_ontology:measurement(bibl_tr_t300, biblical_authority__sola_scriptura_reading, theater_ratio, 300, 0.19).
narrative_ontology:measurement_basis(bibl_tr_t300, observed).
narrative_ontology:measurement(bibl_tr_t400, biblical_authority__sola_scriptura_reading, theater_ratio, 400, 0.21).
narrative_ontology:measurement_basis(bibl_tr_t400, observed).
narrative_ontology:measurement(bibl_tr_t500, biblical_authority__sola_scriptura_reading, theater_ratio, 500, 0.22).
narrative_ontology:measurement_basis(bibl_tr_t500, observed).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_authority__sola_scriptura_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(bibl_be_t0, observed).
narrative_ontology:measurement(bibl_be_t100, biblical_authority__sola_scriptura_reading, base_extractiveness, 100, 0.2).
narrative_ontology:measurement_basis(bibl_be_t100, observed).
narrative_ontology:measurement(bibl_be_t200, biblical_authority__sola_scriptura_reading, base_extractiveness, 200, 0.24).
narrative_ontology:measurement_basis(bibl_be_t200, observed).
narrative_ontology:measurement(bibl_be_t300, biblical_authority__sola_scriptura_reading, base_extractiveness, 300, 0.27).
narrative_ontology:measurement_basis(bibl_be_t300, observed).
narrative_ontology:measurement(bibl_be_t400, biblical_authority__sola_scriptura_reading, base_extractiveness, 400, 0.29).
narrative_ontology:measurement_basis(bibl_be_t400, observed).
narrative_ontology:measurement(bibl_be_t500, biblical_authority__sola_scriptura_reading, base_extractiveness, 500, 0.31).
narrative_ontology:measurement_basis(bibl_be_t500, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(biblical_authority__sola_scriptura_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_authority__sola_scriptura_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(biblical_authority__sola_scriptura_reading, 0.08).
narrative_ontology:affects_constraint(biblical_authority__sola_scriptura_reading, tradition_scripture_reading).
narrative_ontology:affects_constraint(biblical_authority__sola_scriptura_reading, conciliar_reading).

% DUAL FORMULATION NOTE:
% Part of the biblical_authority kernel family (3 readings). tradition_scripture_reading models Scripture-requires-magisterium with higher concentrated clerical extraction and lower fragmentation. conciliar_reading models Scripture-through-councils with intermediate extraction and negotiated coherence via patristic consensus. This story (sola_scriptura_reading) authors low, diffuse extraction and high structural fragmentation as the distinguishing delta. Each story carries its own stable ε per the ε-invariance principle; none averages over the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
