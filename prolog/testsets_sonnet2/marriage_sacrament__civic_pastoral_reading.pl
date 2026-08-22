% ============================================================================
% CONSTRAINT STORY: marriage_sacrament__civic_pastoral_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: marriage_sacrament__civic_pastoral_reading
 *   human_readable: Marriage as Pastoral Relationship: Discernment-Based Reading of Indissolubility
 *   domain: religious_doctrine/canon_law/political_sociology
 *
 * SUMMARY:
 *   This story instantiates the civic-pastoral reading of the
 *   marriage-sacrament kernel: marriage is understood as a relationship
 *   subject to genuine human failure, and indissolubility functions as an
 *   ideal horizon that pastoral discernment applies compassionately
 *   case-by-case rather than as an exceptionless ontological bar. This
 *   reading emerged institutionally through the 1980 and 2014-2015 Synods on
 *   the Family and crystallized in Amoris Laetitia (2016), particularly the
 *   internal-forum discernment pathway described in footnote 351. It is a
 *   genuinely different constraint from the hierarchical-indissolubility
 *   reading (a separate story, constraint_id
 *   hierarchical_indissolubility_reading): that reading treats
 *   indissolubility as constitutive of the marriage bond itself, adjudicated
 *   exclusively through canonical tribunal processes, with negligible
 *   extraction from any party because the rule is applied uniformly and the
 *   tribunal process is presented as procedurally sufficient. This reading's
 *   ε is measurably higher (0.42 vs. a low mountain-adjacent value in the
 *   sibling) because it generates real winners (divorced-remarried Catholics
 *   gaining sacramental access) and real losers (traditional laity
 *   experiencing normative erosion, canon lawyers whose expertise is
 *   bypassed, abandoned first spouses whose marriages are still formally
 *   valid while functionally treated otherwise) through inconsistent,
 *   diocese-by-diocese enforcement. The two readings are not the same
 *   constraint measured two ways — they have different beneficiary/victim
 *   sets, different enforcement mechanisms, and different ε. Per the
 *   ε-invariance principle, they are authored as separate stories linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - reform_oriented_hierarchy: institutional agenda-setter administering discernment protocols
 *   - divorced_remarried_catholics: primary beneficiaries gaining sacramental access
 *   - traditional_catholic_laity: primary victims experiencing doctrinal relativization
 *   - canon_lawyers_strict_constructionist: professional victims of tribunal bypass
 *   - annulment_denied_first_spouses: victims of functional dissolution without formal annulment
 *   - vatican_doctrinal_congregation: analytical observer monitoring doctrinal drift
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_sacrament__civic_pastoral_reading, 0.42).
domain_priors:suppression_score(marriage_sacrament__civic_pastoral_reading, 0.38).
domain_priors:theater_ratio(marriage_sacrament__civic_pastoral_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_sacrament__civic_pastoral_reading, tangled_rope).
narrative_ontology:human_readable(marriage_sacrament__civic_pastoral_reading, "Marriage as Pastoral Relationship: Discernment-Based Reading of Indissolubility").
narrative_ontology:topic_domain(marriage_sacrament__civic_pastoral_reading, "religious_doctrine/canon_law/political_sociology").

domain_priors:requires_active_enforcement(marriage_sacrament__civic_pastoral_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_sacrament__civic_pastoral_reading, 'c3e5d3b5-5c25-4c77-b075-16432458bdde').
narrative_ontology:cs_kernel_codification('c3e5d3b5-5c25-4c77-b075-16432458bdde', fixed_text).
narrative_ontology:cs_authority_grounding('c3e5d3b5-5c25-4c77-b075-16432458bdde', lineage).
narrative_ontology:cs_interpretation_layer_present('c3e5d3b5-5c25-4c77-b075-16432458bdde').
narrative_ontology:cs_reading_relation('c3e5d3b5-5c25-4c77-b075-16432458bdde', marriage_sacrament__hierarchical_indissolubility_reading, influences).
narrative_ontology:cs_axiom('c3e5d3b5-5c25-4c77-b075-16432458bdde', foundational, indissolubility_as_regulative_ideal).
narrative_ontology:cs_axiom_status(indissolubility_as_regulative_ideal, holdable).
narrative_ontology:cs_axiom_grounding('c3e5d3b5-5c25-4c77-b075-16432458bdde', indissolubility_as_regulative_ideal, instrumental).
narrative_ontology:cs_axiom('c3e5d3b5-5c25-4c77-b075-16432458bdde', foundational, individual_conscience_discernment_competent).
narrative_ontology:cs_axiom_status(individual_conscience_discernment_competent, holdable).
narrative_ontology:cs_axiom_grounding('c3e5d3b5-5c25-4c77-b075-16432458bdde', individual_conscience_discernment_competent, conventional).
narrative_ontology:cs_reference_frame('c3e5d3b5-5c25-4c77-b075-16432458bdde', post_conciliar_pastoral_theology).
narrative_ontology:cs_drift_state('c3e5d3b5-5c25-4c77-b075-16432458bdde', post_amoris_laetitia_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c3e5d3b5-5c25-4c77-b075-16432458bdde', '').
narrative_ontology:cs_kernel_id(marriage_sacrament__civic_pastoral_reading, marriage_sacrament).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_sacrament__civic_pastoral_reading, divorced_remarried_catholics).
narrative_ontology:constraint_beneficiary(marriage_sacrament__civic_pastoral_reading, pastoral_ministers).
narrative_ontology:constraint_beneficiary(marriage_sacrament__civic_pastoral_reading, diocesan_tribunals_reformist_wing).
narrative_ontology:constraint_beneficiary(marriage_sacrament__civic_pastoral_reading, reform_oriented_hierarchy).
narrative_ontology:constraint_victim(marriage_sacrament__civic_pastoral_reading, traditional_catholic_laity).
narrative_ontology:constraint_victim(marriage_sacrament__civic_pastoral_reading, canon_lawyers_strict_constructionist).
narrative_ontology:constraint_victim(marriage_sacrament__civic_pastoral_reading, annulment_denied_first_spouses).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bishops, synod participants, and curial officials who administer internal-forum discernment processes and pastoral accompaniment protocols (e.g. Amoris Laetitia footnote 351 applications). They set diocesan policy on case-by-case admission to communion and interpret indissolubility as an ideal horizon rather than an exceptionless rule enforced identically everywhere. They retain full institutional standing regardless of how any given diocese resolves individual cases.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, reform_oriented_hierarchy, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(marriage_sacrament__civic_pastoral_reading, reform_oriented_hierarchy, beneficiary).

% Catholics in second unions without prior annulment who, under the pastoral reading, may be accompanied toward sacramental participation through internal forum discernment rather than automatic exclusion. They gain access to communion and community standing that the hierarchical reading would deny them, but the access depends on which diocese, which confessor, and which bishop they happen to have.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, divorced_remarried_catholics, beneficiary,
    moderate, biographical, constrained, national).

% Catholics whose religious identity and marital fidelity are structured around indissolubility as a fixed, ontological reality rather than a discernible-around ideal. They experience the pastoral reading as doctrinal relativization: the same sacramental discipline that governed their own marriages, sometimes at real personal cost, is now applied inconsistently to others. Their exit options are narrow because leaving the Church means abandoning a totalizing identity structure, not merely changing institutional affiliation.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, traditional_catholic_laity, payer,
    powerless, generational, identity_locked, national).

% Tribunal officials and canonists trained in the classical jurisprudence of the marriage bond who find their professional expertise devalued as discernment displaces formal adjudication. Their careers and authority were built on rigorous, uniform application of canon 1141's indissolubility norm; the pastoral reading routes decisions around their tribunals into pastoral conversations they do not control.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, canon_lawyers_strict_constructionist, payer,
    moderate, biographical, constrained, national).

% Abandoned spouses whose marriages were never annulled but whose former partners now receive pastoral accompaniment into new unions regardless. They bear the reputational and theological cost of a bond the Church still formally recognizes as valid while practically permitting its functional dissolution for the other party.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, annulment_denied_first_spouses, payer,
    powerless, biographical, trapped, local).

% Parish priests and lay ministers who conduct discernment conversations. They gain pastoral latitude and reduced adversarial burden (fewer bruising tribunal battles to referee) and can adapt doctrine's application to lived circumstance, but they also absorb the discretion and blame when discernment outcomes are contested by parishioners or superiors.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, pastoral_ministers, beneficiary,
    organized, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(marriage_sacrament__civic_pastoral_reading, pastoral_ministers, agenda_setter).

% The Church's central doctrinal office, which monitors whether pastoral discernment practices are drifting into de facto doctrinal change. It issues clarifications, dubia responses, and can discipline bishops who authorize practices judged to exceed pastoral latitude, but has not resolved the underlying tension definitively.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, vatican_doctrinal_congregation, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_sacrament__civic_pastoral_reading, divorced_remarried_catholics).
narrative_ontology:fixing_cost_class(marriage_sacrament__civic_pastoral_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for the Church to retain divorced-and-remarried Catholics, and Catholics in complex marital situations generally, within active sacramental life and community membership rather than losing them entirely to formal exclusion — coordinating pastoral care with doctrinal commitment under conditions of real human failure.
% TRANSFER_FUNCTION: Moves normative clarity and uniform doctrinal treatment away from traditional laity and canon lawyers who relied on it, and moves sacramental access and pastoral standing toward Catholics in irregular unions — mediated through diocesan and confessor-level discretion rather than a uniform rule.
% ABSENT_VOICES: Traditional Catholics who experience the shift as doctrinal betrayal are rarely represented in the synodal and episcopal conversations that shape discernment protocols; annulment-denied first spouses are almost never consulted when their former partners are pastorally accompanied into new unions.
% DISAPPEARANCE_RATIONALE: If pastoral discernment pathways disappeared and indissolubility reverted to uniform tribunal-only adjudication, divorced-and-remarried Catholics currently accompanied toward communion would face renewed formal exclusion, some dioceses' pastoral practice would need wholesale reversal, and the internal Church debate over Amoris Laetitia's proper interpretation would be resolved by fiat rather than ongoing contest.
% FOUNDING_PROBLEM: Rigid, uniform application of indissolubility was producing mass attrition: Catholics in second unions were leaving the sacraments and often the Church entirely, and tribunal annulment processes were seen as inaccessible, slow, or class-biased (favoring the wealthy and well-connected who could navigate them).
% FOUNDING_PROBLEM_CORROBORATION: Reform-oriented bishops and synod documents attest the attrition problem as ongoing and worsening. Independent sociological research on Catholic disaffiliation (Pew, CARA) corroborates declining sacramental participation among divorced Catholics prior to the pastoral shift. Traditional Catholic commentators and some canon lawyers dispute that the problem is being solved rather than merely deferred, arguing the pastoral approach substitutes ambiguity for the annulment process's genuine (if imperfect) adjudicative clarity — this dispute is not resolved by any single authoritative body.
narrative_ontology:disappearance_verdict(marriage_sacrament__civic_pastoral_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_sacrament__civic_pastoral_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_sacrament__civic_pastoral_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_sacrament__civic_pastoral_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_sacrament__civic_pastoral_reading, 0.42, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction (0.42) is moderate, not high, because the coordination function is genuine: attrition of divorced Catholics from sacramental life was a real problem and the pastoral pathway measurably reduces it. But the cost is real too — inconsistency across dioceses means normative clarity is transferred away from those (traditional laity, canon lawyers) whose religious and professional identity depended on uniform application. Suppression is falling over the measured interval (0.55 to 0.38) precisely because active enforcement of the older uniform standard is being relaxed, not intensified — this is a case where suppression decay tracks a genuine loosening of enforcement machinery, following the story's dynamic rather than a static picture, hence the inclusion of the suppression_requirement series. Theater ratio rises modestly (0.12 to 0.31) as institutional language increasingly frames ad hoc diocesan variation as principled 'accompaniment' rather than acknowledging inconsistent enforcement outright.
 *
 * DIRECTIONALITY LOGIC:
 *   Divorced-remarried Catholics and pastoral ministers sit near the beneficiary end: they gain standing, access, and discretion respectively. Traditional laity and canon lawyers sit near the target end: normative clarity and professional standing are extracted from them without compensation, and their exit options are constrained by identity lock (traditional laity) or career dependence (canon lawyers). Annulment-denied first spouses are a distinct victim class: they are trapped by a marriage the Church still formally recognizes as valid while their former spouse is pastorally accompanied past it — a directly asymmetric cost with no coordination benefit flowing back to them at all.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (mass attrition from rigid tribunal-only adjudication) is genuinely contested as live vs. dead: reform-oriented hierarchy and sociological data corroborate ongoing attrition pressure justifying the pastoral pathway's continuation, while traditional critics argue the pathway has become a permanent parallel track rather than a bridge back to clarity — it has not sunset, has no declared sunset clause, and shows no institutional mechanism for re-converging discernment outcomes into uniform doctrine. This is why the story is authored as tangled_rope rather than scaffold: there is a real coordination function (retention of Catholics in irregular unions) and real, ongoing extraction (normative and professional cost to traditional laity and canon lawyers) sustained by active diocesan enforcement variance, not a transitional arrangement with a declared endpoint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discernment_vs_doctrinal_change,
    'Is diocese-by-diocese pastoral discernment a legitimate application of unchanging doctrine to particular circumstances, or is it a de facto change in doctrine achieved through practice rather than formal declaration?',
    'A definitive magisterial ruling either affirming discernment as within traditional doctrinal bounds or declaring it exceeds pastoral latitude; absent that, longitudinal tracking of whether diocesan practices converge toward a stable norm or diverge indefinitely.',
    'If discernment is legitimate application, this reading''s extraction is closer to a rope (a workable coordination mechanism with acceptable friction); if it is de facto doctrinal change achieved without formal process, the extraction from traditional laity and canon lawyers is better characterized as suppressed dissent against an unacknowledged shift, pushing the classification toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(discernment_vs_doctrinal_change, conceptual, 'Whether discernment-based pastoral practice constitutes legitimate doctrinal application or unacknowledged doctrinal change.').

omega_variable(
    kernel_framing_delta,
    'Does the civic-pastoral reading and the hierarchical-indissolubility reading represent two coherent framings of the same underlying commitment, or does the pastoral reading''s practical operation already constitute abandonment of the kernel the hierarchical reading defends?',
    'Comparative analysis of magisterial texts (Familiaris Consortio 84, Amoris Laetitia ch. 8, the 2016 dubia correspondence) to determine whether official teaching treats the two readings as compatible interpretations of one doctrine or as substantively divergent positions.',
    'If the readings are genuinely compatible framings, this story''s classification as tangled_rope reflects real, bounded tension within one institution; if the pastoral reading has effectively superseded the hierarchical one in practice while both remain formally taught, the sibling reading (hierarchical_indissolubility_reading) may itself be drifting toward piton status — a formally maintained doctrine no longer uniformly enforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_delta, conceptual, 'Whether the two kernel readings are compatible framings or substantively divergent, with implications for the sibling reading''s own classification.').

omega_variable(
    annulment_process_reform_confound,
    'How much of the reduction in tribunal-only adjudication traces to the pastoral discernment pathway specifically, versus concurrent streamlining of the annulment process itself (Mitis Iudex Dominus Iesus, 2015), which independently reduced tribunal burden without invoking discernment?',
    'Disaggregated diocesan data comparing annulment case volume/duration pre- and post-Mitis Iudex against internal-forum discernment case volume over the same period.',
    'If most attrition relief comes from faster annulments rather than discernment pathways, the extraction attributed to this reading''s coordination function may be overstated, and part of the measured base_extractiveness trajectory should be attributed to a separate procedural reform constraint rather than this doctrinal reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(annulment_process_reform_confound, empirical, 'Whether attrition relief is attributable to pastoral discernment or a concurrent, separable procedural reform.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_sacrament__civic_pastoral_reading, 1965, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1965, marriage_sacrament__civic_pastoral_reading, theater_ratio, 1965, 0.12).
narrative_ontology:measurement(marr_tr_t1981, marriage_sacrament__civic_pastoral_reading, theater_ratio, 1981, 0.15).
narrative_ontology:measurement(marr_tr_t1997, marriage_sacrament__civic_pastoral_reading, theater_ratio, 1997, 0.19).
narrative_ontology:measurement(marr_tr_t2013, marriage_sacrament__civic_pastoral_reading, theater_ratio, 2013, 0.23).
narrative_ontology:measurement(marr_tr_t2016, marriage_sacrament__civic_pastoral_reading, theater_ratio, 2016, 0.28).
narrative_ontology:measurement(marr_tr_t2025, marriage_sacrament__civic_pastoral_reading, theater_ratio, 2025, 0.31).

% Extraction over time
narrative_ontology:measurement(marr_be_t1965, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 1965, 0.18).
narrative_ontology:measurement(marr_be_t1981, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 1981, 0.22).
narrative_ontology:measurement(marr_be_t1997, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 1997, 0.27).
narrative_ontology:measurement(marr_be_t2013, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 2013, 0.33).
narrative_ontology:measurement(marr_be_t2016, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 2016, 0.38).
narrative_ontology:measurement(marr_be_t2025, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1965, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 1965, 0.55).
narrative_ontology:measurement(marr_su_t1981, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 1981, 0.5).
narrative_ontology:measurement(marr_su_t1997, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 1997, 0.46).
narrative_ontology:measurement(marr_su_t2013, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 2013, 0.42).
narrative_ontology:measurement(marr_su_t2016, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 2016, 0.4).
narrative_ontology:measurement(marr_su_t2025, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 2025, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_sacrament__civic_pastoral_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_sacrament__civic_pastoral_reading, 0.1).
narrative_ontology:affects_constraint(marriage_sacrament__civic_pastoral_reading, hierarchical_indissolubility_reading).

% DUAL FORMULATION NOTE:
% This story and hierarchical_indissolubility_reading decompose the natural-language concept 'Catholic teaching on marriage indissolubility' into two structurally distinct constraints per the ε-invariance principle. This reading (civic_pastoral) authors moderate extraction (0.42) driven by real beneficiary/victim asymmetry under inconsistent diocesan enforcement. The sibling reading authors a different, lower ε consistent with uniform hierarchical adjudication presented as procedurally sufficient with negligible identified victims. The two are linked bidirectionally: each reading's practical dominance in a given diocese structurally suppresses the other's operation there, and institutional resource allocation (tribunal staffing vs. pastoral formation investment) shifts between them over time.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
