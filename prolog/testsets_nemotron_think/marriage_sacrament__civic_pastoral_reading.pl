% ============================================================================
% CONSTRAINT STORY: marriage_sacrament__civic_pastoral_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_sacrament__civic_pastoral_reading, []).

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
 *   constraint_id: marriage_sacrament__civic_pastoral_reading
 *   human_readable: Marriage as Pastoral Relationship with Compassionate Discernment
 *   domain: religious_doctrine/canon_law/political_sociology
 *
 * SUMMARY:
 *   This constraint story captures the civic pastoral reading of the marriage
 *   sacrament kernel — the position that indissolubility is an aspirational
 *   ideal rather than an ontological absolute, and that pastoral discernment
 *   in the internal forum can admit divorced-remarried Catholics to
 *   communion. The reading emerged from the post-Vatican II pastoral crisis
 *   and was authoritatively articulated in Amoris Laetitia (2016), though its
 *   roots trace to the 1970s-80s debates on Familiaris Consortio. The
 *   constraint operates through a dual structure: formally, the Code of Canon
 *   Law (can. 915) and Catechism retain the hierarchical reading;
 *   practically, episcopal conferences and individual confessors apply
 *   discernment criteria that vary widely. This creates the extraction:
 *   traditional Catholics bear the cost of doctrinal instability
 *   (identity_locked exit), while divorced-remarried Catholics and pastoral
 *   ministers gain access and discretion.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_sacrament__civic_pastoral_reading, 0.45).
domain_priors:suppression_score(marriage_sacrament__civic_pastoral_reading, 0.35).
domain_priors:theater_ratio(marriage_sacrament__civic_pastoral_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_sacrament__civic_pastoral_reading, tangled_rope).
narrative_ontology:human_readable(marriage_sacrament__civic_pastoral_reading, "Marriage as Pastoral Relationship with Compassionate Discernment").
narrative_ontology:topic_domain(marriage_sacrament__civic_pastoral_reading, "religious_doctrine/canon_law/political_sociology").

domain_priors:requires_active_enforcement(marriage_sacrament__civic_pastoral_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_sacrament__civic_pastoral_reading, '410e06de-7159-494b-801b-618ea4ce8e3c').
narrative_ontology:cs_kernel_codification('410e06de-7159-494b-801b-618ea4ce8e3c', formalized).
narrative_ontology:cs_authority_grounding('410e06de-7159-494b-801b-618ea4ce8e3c', lineage).
narrative_ontology:cs_interpretation_layer_present('410e06de-7159-494b-801b-618ea4ce8e3c').
narrative_ontology:cs_reading_relation('410e06de-7159-494b-801b-618ea4ce8e3c', marriage_sacrament__hierarchical_indissolubility_reading, coexists_with).
narrative_ontology:cs_axiom('410e06de-7159-494b-801b-618ea4ce8e3c', foundational, indissolubility_as_pastoral_ideal).
narrative_ontology:cs_axiom_status(indissolubility_as_pastoral_ideal, holdable).
narrative_ontology:cs_axiom_grounding('410e06de-7159-494b-801b-618ea4ce8e3c', indissolubility_as_pastoral_ideal, conventional).
narrative_ontology:cs_axiom('410e06de-7159-494b-801b-618ea4ce8e3c', foundational, compassionate_discernment_in_internal_forum).
narrative_ontology:cs_axiom_status(compassionate_discernment_in_internal_forum, holdable).
narrative_ontology:cs_axiom_grounding('410e06de-7159-494b-801b-618ea4ce8e3c', compassionate_discernment_in_internal_forum, instrumental).
narrative_ontology:cs_reference_frame('410e06de-7159-494b-801b-618ea4ce8e3c', tridentine_canonical_indissolubility).
narrative_ontology:cs_drift_state('410e06de-7159-494b-801b-618ea4ce8e3c', post_amoris_laetitia, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('410e06de-7159-494b-801b-618ea4ce8e3c', '').
narrative_ontology:cs_kernel_id(marriage_sacrament__civic_pastoral_reading, marriage_sacrament).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_sacrament__civic_pastoral_reading, divorced_remarried_catholics).
narrative_ontology:constraint_beneficiary(marriage_sacrament__civic_pastoral_reading, pastoral_ministers).
narrative_ontology:constraint_beneficiary(marriage_sacrament__civic_pastoral_reading, progressive_theologians).
narrative_ontology:constraint_victim(marriage_sacrament__civic_pastoral_reading, traditional_catholics).
narrative_ontology:constraint_victim(marriage_sacrament__civic_pastoral_reading, canonical_traditionalists).
narrative_ontology:constraint_vindicates(marriage_sacrament__civic_pastoral_reading, pastoral_mercy_over_legalism).
narrative_ontology:constraint_vindicates(marriage_sacrament__civic_pastoral_reading, indissolubility_as_ideal_not_ontology).
narrative_ontology:constraint_vindicates(marriage_sacrament__civic_pastoral_reading, conscience_based_discernment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Civilly divorced and remarried Catholics who seek access to communion and full sacramental participation. Under the civic pastoral reading, they benefit from discernment pathways (e.g., Amoris Laetitia footnote 351) that may admit them to communion without formal annulment. Their exit options are constrained: leaving the Church severs communal and familial ties; staying requires navigating ambiguous pastoral discretion.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, divorced_remarried_catholics, beneficiary,
    moderate, biographical, constrained, global).

% Priests, bishops, and lay pastoral ministers who implement discernment processes. They gain discretionary authority to judge individual cases, which enhances their pastoral relevance but also exposes them to criticism from both traditionalist and progressive flanks. Their exit options are mobile — they can request reassignment or move between dioceses with different pastoral cultures.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, pastoral_ministers, agenda_setter,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(marriage_sacrament__civic_pastoral_reading, pastoral_ministers, beneficiary).

% Academic and institutional theologians who develop the theoretical framework for pastoral discernment. They benefit intellectually and professionally from a reading that opens space for development of doctrine. Their exit options are arbitrage-grade: they can publish, teach, and move between institutions that valorize their framework.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, progressive_theologians, beneficiary,
    organized, generational, arbitrage, global).

% Laity and clergy whose Catholic identity is constituted by doctrinal stability and the perceived objectivity of indissolubility. They experience the civic pastoral reading as relativization: the norm they structured their marriages and vocations around becomes subject to case-by-case exception. Their exit is identity-locked — leaving would fracture the self-concept formed through the very doctrine now being reinterpreted; staying means enduring what they experience as institutional betrayal.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, traditional_catholics, payer,
    moderate, generational, identity_locked, global).

% Canon lawyers, tribunal officials, and bishops committed to the classical indissolubility framework. They bear the cost of enforcing a norm that the higher pastoral authority signals is no longer absolute. Their exit is constrained: they can resist quietly, appeal to Rome (with uncertain reception), or retire; open dissent risks canonical penalty.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, canonical_traditionalists, payer,
    organized, generational, constrained, global).

% The Roman pontiff and curial offices that promulgate and guard the kernel. They authorize the civic pastoral reading through documents like Amoris Laetitia while formally retaining the hierarchical reading in the Catechism and Code. Their exit is analytical — they do not exit; they manage the tension between readings as a governance problem.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, hierarchical_magisterium, agenda_setter,
    institutional, civilizational, analytical, universal).

% Scholars of religion, law, and sociology; ecumenical partners; secular jurists studying church-state interaction. They analyze the constraint's operation without being subject to its internal discipline.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, external_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a pastoral pathway for Catholics in irregular marital situations to remain connected to the sacramental life of the Church, avoiding the binary of 'annulment or exclusion.' Coordinates the clergy's discretion with the laity's need for mercy, using discernment as the mechanism rather than automatic rule-application.
% TRANSFER_FUNCTION: Transfers normative authority from the universal canonical norm (indissolubility as binding law) to the local pastoral judgment (discernment in the internal forum). The 'cost' paid by traditional Catholics is the loss of a shared, objective standard; the 'gain' received by divorced-remarried Catholics is access to communion without formal declaration of nullity.
% ABSENT_VOICES: The children of divorce and remarriage — especially those raised in traditionalist homes who experience the pastoral shift as destabilizing their family's witness — are structurally excluded from the discernment conversation. Also absent: the separated-but-not-remarried spouse whose abandonment is tacitly normalized when the remarried partner is admitted to communion.
% DISAPPEARANCE_RATIONALE: If the civic pastoral reading vanished overnight, dioceses would revert to the pre-2016 praxis: near-universal denial of communion to divorced-remarried Catholics without annulment. This would rearrange the sacramental lives of millions, trigger canonical appeals, and force a confrontation between the papacy and traditionalist bishops. The world does not stay the same.
% FOUNDING_PROBLEM: The post-conciliar pastoral crisis: rising divorce rates among Catholics, the pastoral inadequacy of the annulment process (slow, legalistic, perceived as 'Catholic divorce'), and the mass departure of divorced-remarried Catholics from sacramental practice. The arrangement was built to retain these Catholics in the Church while preserving the formal doctrine of indissolubility.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the 1980 Synod on the Family, the 1981 Familiaris Consortio (which acknowledged the problem but affirmed the traditional discipline), and the 2014-2015 Synods that explicitly debated it. Traditionalist bishops and canonists corroborate that the problem exists but dispute that the civic pastoral reading solves it — they argue it dissolves the doctrine it claims to preserve. No neutral third party corroborates the status; the dispute is internal to the Church's interpretive communities.
narrative_ontology:disappearance_verdict(marriage_sacrament__civic_pastoral_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_sacrament__civic_pastoral_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_sacrament__civic_pastoral_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_sacrament__civic_pastoral_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_sacrament__civic_pastoral_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_sacrament__civic_pastoral_reading_tests).
:- end_tests(marriage_sacrament__civic_pastoral_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate: the constraint does not extract material resources but extracts epistemic and identity security from traditional Catholics, transferring normative authority to local discernment. Suppression (0.35) is present but not total: traditionalist clergy face canonical pressure (e.g., restrictions on preaching, removal from tribunals) but lay traditionalists are suppressed mainly through social marginalization in progressive parishes and lack of institutional recourse. Theater ratio (0.32) reflects that the formal doctrine remains unchanged while pastoral practice diverges — the 'performance' of indissolubility in catechesis and liturgy continues alongside its functional suspension in the internal forum. Accessibility collapse (0.42) is moderate: alternatives (annulment, living as brother-sister, Eastern Orthodox communion) exist but are costly or unavailable to many. Resistance (0.48) is significant: the 2016-2017 dubia, the 2023-2024 Synod debates, and the growth of traditionalist communities all manifest resistance.
 *
 * PERSPECTIVAL GAP:
 *   From the traditional Catholic seat, the constraint computes as snare: extraction of identity-security with no exit. From the divorced-remarried seat, it computes as rope: genuine coordination (access to sacraments) with minimal coercion. From the pastoral minister seat, it computes as scaffold: a transitional discernment practice that may eventually formalize into new canon law. The engine computes these divergences from the structural data; the author does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Traditional Catholics are identity-locked payers: their self-concept is fused to the doctrinal stability the constraint erodes. Canonical traditionalists are constrained payers: they enforce a norm their superiors signal is pastoral rather than absolute. Divorced-remarried Catholics are constrained beneficiaries: they gain access but depend on local pastoral discretion (not a right). Pastoral ministers are organized agenda-setters with mobile exit: they wield the discernment authority but can move between diocesan cultures. The hierarchical magisterium is the institutional agenda-setter that maintains the kernel tension as governance strategy.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (pastoral crisis of divorced-remarried Catholics) remains live — divorce rates have not fallen, annulment processes remain burdensome. But the civic pastoral reading may have outlived its coordination function: the discernment pathway has not been codified into universal law, creating a patchwork that extracts from traditionalists without securely benefiting its intended beneficiaries. The mandate (pastoral mercy) persists, but the mechanism (informal discernment) has become a source of the very instability it was meant to resolve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_frame_kernel_reading,
    'Is the civic pastoral reading a distinct constraint from the hierarchical indissolubility reading, or a permissible interpretation within a single constraint?',
    'Structural analysis: if the two readings produce divergent ε, beneficiary/victim sets, and seat classifications, they are distinct constraints per the ε-invariance principle. The civic pastoral reading''s moderate extractiveness and identity-locked victim set differ from the hierarchical reading''s near-zero extractiveness and absence of victims.',
    'If distinct constraints, each gets its own classification; the kernel is a family linked by network.affects_constraints. If a single constraint, the framework must model observable-dependent classification — which DP-001 forbids.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_kernel_reading, conceptual, 'Whether the kernel decomposes into multiple ε-invariant constraints.').

omega_variable(
    discernment_codification_trajectory,
    'Will the discernment pathway be codified into universal canon law, remain episcopal-conference-level norms, or stay at the level of individual confessor discretion?',
    'Track canonical developments: motu proprio on marriage nullity (2015), Amoris Laetitia implementation documents, Synod on Synodality final document, and any future canonical legislation on canon 915.',
    'Codification would reduce theater_ratio (formal alignment of law and practice) but may increase suppression (canonical penalties for non-compliant clergy). Continued informality sustains high theater and variable extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discernment_codification_trajectory, empirical, 'Trajectory of the pastoral discernment mechanism''s formalization.').

omega_variable(
    traditionalist_coalition_viability,
    'Can traditional Catholics and canonical traditionalists form a coalition capable of constraining the civic pastoral reading, or is their exit too identity-locked to generate effective resistance?',
    'Observe traditionalist organizational capacity: growth of traditionalist institutes, episcopal appointments, canonical appeals success rates, and lay mobilization (e.g., petition campaigns, media presence).',
    'If coalition viable, resistance metric rises and the constraint may shift toward scaffold (formalization forced) or snare (hardened suppression). If not viable, extraction accumulates unchecked.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(traditionalist_coalition_viability, empirical, 'Whether the victim set can organize effective resistance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_sacrament__civic_pastoral_reading, 1965, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marriage_sacrament__civic_pastoral_reading_tr_t1965, marriage_sacrament__civic_pastoral_reading, theater_ratio, 1965, 0.05).
narrative_ontology:measurement(marriage_sacrament__civic_pastoral_reading_tr_t1980, marriage_sacrament__civic_pastoral_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(marriage_sacrament__civic_pastoral_reading_tr_t1995, marriage_sacrament__civic_pastoral_reading, theater_ratio, 1995, 0.15).
narrative_ontology:measurement(marriage_sacrament__civic_pastoral_reading_tr_t2005, marriage_sacrament__civic_pastoral_reading, theater_ratio, 2005, 0.2).
narrative_ontology:measurement(marriage_sacrament__civic_pastoral_reading_tr_t2016, marriage_sacrament__civic_pastoral_reading, theater_ratio, 2016, 0.28).
narrative_ontology:measurement(marriage_sacrament__civic_pastoral_reading_tr_t2025, marriage_sacrament__civic_pastoral_reading, theater_ratio, 2025, 0.32).

% Extraction over time
narrative_ontology:measurement(marriage_sacrament__civic_pastoral_reading_be_t1965, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 1965, 0.08).
narrative_ontology:measurement(marriage_sacrament__civic_pastoral_reading_be_t1980, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 1980, 0.12).
narrative_ontology:measurement(marriage_sacrament__civic_pastoral_reading_be_t1995, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 1995, 0.18).
narrative_ontology:measurement(marriage_sacrament__civic_pastoral_reading_be_t2005, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 2005, 0.25).
narrative_ontology:measurement(marriage_sacrament__civic_pastoral_reading_be_t2016, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 2016, 0.38).
narrative_ontology:measurement(marriage_sacrament__civic_pastoral_reading_be_t2025, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 2025, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(marriage_sacrament__civic_pastoral_reading_su_t1965, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 1965, 0.1).
narrative_ontology:measurement(marriage_sacrament__civic_pastoral_reading_su_t1980, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 1980, 0.15).
narrative_ontology:measurement(marriage_sacrament__civic_pastoral_reading_su_t1995, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 1995, 0.2).
narrative_ontology:measurement(marriage_sacrament__civic_pastoral_reading_su_t2005, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 2005, 0.25).
narrative_ontology:measurement(marriage_sacrament__civic_pastoral_reading_su_t2016, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 2016, 0.3).
narrative_ontology:measurement(marriage_sacrament__civic_pastoral_reading_su_t2025, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 2025, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_sacrament__civic_pastoral_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_sacrament__civic_pastoral_reading, 0.08).
narrative_ontology:affects_constraint(marriage_sacrament__civic_pastoral_reading, marriage_sacrament__hierarchical_indissolubility_reading).

% DUAL FORMULATION NOTE:
% The marriage_sacrament kernel decomposes into two structurally distinct constraints: (1) hierarchical_indissolubility_reading — indissolubility as ontological reality, near-zero extractiveness, Mountain classification; (2) civic_pastoral_reading — indissolubility as pastoral ideal, moderate extractiveness, Tangled Rope classification. The hierarchical reading provides the formal kernel that the civic reading reinterprets; the civic reading creates the pastoral pressure that erodes the hierarchical reading's effective authority. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_sacrament__civic_pastoral_reading, moderate, 0.85).
constraint_indexing:directionality_override(marriage_sacrament__civic_pastoral_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
