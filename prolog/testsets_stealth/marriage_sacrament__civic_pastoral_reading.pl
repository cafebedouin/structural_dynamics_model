% ============================================================================
% CONSTRAINT STORY: marriage_sacrament__civic_pastoral_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: marriage_sacrament__civic_pastoral_reading
 *   human_readable: Pastoral-Discernment Regime for Failed Marriages (Civic-Pastoral Reading)
 *   domain: religious doctrine/canon law/political sociology
 *
 * SUMMARY:
 *   Since the 2016 apostolic exhortation on the family, the operative
 *   discipline in much of the Latin Church treats marriage as a pastoral
 *   relationship subject to human failure: indissolubility remains taught as
 *   an ideal, but admission of divorced-and-remarried Catholics to
 *   sacramental communion is governed by case-by-case discernment conducted
 *   in the internal forum, with regional implementation varying widely. This
 *   story authors THAT standing arrangement, assessed by this reading's own
 *   lights; epsilon's referent is the pastoral-discernment regime itself,
 *   never the hierarchical regime this reading contests. The colloquial label
 *   'Catholic teaching on marriage' decomposes into two structurally distinct
 *   constraints: this pastoral-discernment regime and the sibling
 *   hierarchical-adjudication regime
 *   (marriage_sacrament__hierarchical_indissolubility_reading), linked via
 *   network.affects_constraints. KEY AGENTS (by structural relationship):
 *   pastoral_hierarchs (institutional/arbitrage) set and administer the
 *   framework; pastoral_accompaniment_clergy (organized/constrained) and
 *   divorced_remarried_catholics (moderate/constrained) benefit;
 *   traditional_lay_catholics (moderate/identity_locked) and
 *   abandoned_first_spouses (powerless/trapped) bear the extraction;
 *   resisting_traditionalist_clergy (organized/identity_locked) and
 *   canon_tribunal_officials (organized/constrained) bear secondary costs;
 *   vatican_doctrine_office (institutional/analytical) observes;
 *   independent_traditionalist_communities (organized/mobile) are excluded
 *   from the conversation. The claim/metric gap is deliberate: the reading
 *   presents itself as mercy completing doctrine, while the authored metrics
 *   describe moderate, accumulating extraction with rising enforcement needs.
 *
 * KEY AGENTS:
 *   - pastoral_hierarchs: agenda-setter (institutional/arbitrage) — authors the framework, approves regional guidelines, appoints or passes over ordinaries by reception; collects discretionary authority and factional peace
 *   - pastoral_accompaniment_clergy: beneficiary (organized/constrained) — conducts discernment conversations; regains case-level pastoral discretion after decades of tribunal routing
 *   - divorced_remarried_catholics: beneficiary (moderate/constrained) — recover sacramental access without formal nullity adjudication; access varies by diocese
 *   - traditional_lay_catholics: primary target (moderate/identity_locked) — bear doctrinal relativization; lifetimes of fidelity priced under the fixed rule are retroactively repriced
 *   - abandoned_first_spouses: primary target (powerless/trapped) — kept faith with the bond's reciprocity, which the framework suspends for the deserting spouse's new household
 *   - resisting_traditionalist_clergy: secondary target (organized/identity_locked) — dubia signatories, correcting scholars, non-implementing ordinaries; bear career and canonical costs from inside
 *   - canon_tribunal_officials: secondary target (organized/constrained) — gatekeeping monopoly devalued by streamlined nullities and internal-forum bypass
 *   - vatican_doctrine_office: analytical observer (institutional/analytical) — polices coherence between the framework and the indissolubility doctrine
 *   - independent_traditionalist_communities: excluded (organized/mobile) — object that the framework proves the post-conciliar settlement untenable, from wholly outside the synodal conversation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_sacrament__civic_pastoral_reading, 0.62).
domain_priors:suppression_score(marriage_sacrament__civic_pastoral_reading, 0.5).
domain_priors:theater_ratio(marriage_sacrament__civic_pastoral_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_sacrament__civic_pastoral_reading, tangled_rope).
narrative_ontology:human_readable(marriage_sacrament__civic_pastoral_reading, "Pastoral-Discernment Regime for Failed Marriages (Civic-Pastoral Reading)").
narrative_ontology:topic_domain(marriage_sacrament__civic_pastoral_reading, "religious doctrine/canon law/political sociology").

domain_priors:requires_active_enforcement(marriage_sacrament__civic_pastoral_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_sacrament__civic_pastoral_reading, '097af4f5-f7f6-4311-a76d-9013d59a2651').
narrative_ontology:cs_kernel_codification('097af4f5-f7f6-4311-a76d-9013d59a2651', fixed_text).
narrative_ontology:cs_authority_grounding('097af4f5-f7f6-4311-a76d-9013d59a2651', lineage).
narrative_ontology:cs_interpretation_layer_present('097af4f5-f7f6-4311-a76d-9013d59a2651').
narrative_ontology:cs_reading_relation('097af4f5-f7f6-4311-a76d-9013d59a2651', marriage_sacrament__hierarchical_indissolubility_reading, coexists_with).
narrative_ontology:cs_axiom('097af4f5-f7f6-4311-a76d-9013d59a2651', foundational, indissolubility_regulative_not_constitutive).
narrative_ontology:cs_axiom_status(indissolubility_regulative_not_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('097af4f5-f7f6-4311-a76d-9013d59a2651', indissolubility_regulative_not_constitutive, deontological).
narrative_ontology:cs_axiom('097af4f5-f7f6-4311-a76d-9013d59a2651', foundational, internal_forum_discernment_admits_to_communion).
narrative_ontology:cs_axiom_status(internal_forum_discernment_admits_to_communion, holdable).
narrative_ontology:cs_axiom_grounding('097af4f5-f7f6-4311-a76d-9013d59a2651', internal_forum_discernment_admits_to_communion, instrumental).
narrative_ontology:cs_reference_frame('097af4f5-f7f6-4311-a76d-9013d59a2651', pastoral_discernment_framework).
narrative_ontology:cs_drift_state('097af4f5-f7f6-4311-a76d-9013d59a2651', contemporary_synodal_implementation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('097af4f5-f7f6-4311-a76d-9013d59a2651', '').
narrative_ontology:cs_kernel_id(marriage_sacrament__civic_pastoral_reading, marriage_sacrament).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_sacrament__civic_pastoral_reading, divorced_remarried_catholics).
narrative_ontology:constraint_beneficiary(marriage_sacrament__civic_pastoral_reading, pastoral_accompaniment_clergy).
narrative_ontology:constraint_victim(marriage_sacrament__civic_pastoral_reading, traditional_lay_catholics).
narrative_ontology:constraint_victim(marriage_sacrament__civic_pastoral_reading, abandoned_first_spouses).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_sacrament__civic_pastoral_reading, resisting_traditionalist_clergy).
narrative_ontology:constraint_victim(marriage_sacrament__civic_pastoral_reading, canon_tribunal_officials).
narrative_ontology:constraint_vindicates(marriage_sacrament__civic_pastoral_reading, gradualist_pastoral_theology).
narrative_ontology:constraint_vindicates(marriage_sacrament__civic_pastoral_reading, internal_forum_autonomy).
narrative_ontology:constraint_vindicates(marriage_sacrament__civic_pastoral_reading, episcopal_conference_subsidiarity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The pope and sympathetic bishops' conferences author the pastoral framework: they issued the apostolic exhortation that reframed access discipline, approved regional implementation guidelines, and appoint or pass over ordinaries according to their reception of the framework. They retain final say over what counts as legitimate discernment. Their exit is effectively unlimited: they can reinterpret, narrow, or widen the framework by subsequent teaching.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, pastoral_hierarchs, agenda_setter,
    institutional, generational, arbitrage, global).

% Priests and deacons who conduct the discernment conversations. The framework restores them to a decisive pastoral role with case-by-case discretion after decades in which marriage cases were routed to tribunals. They carry the practical burden of operating inside contested norms, never certain what a given admission will cost them with superiors or with factions in the pews.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, pastoral_accompaniment_clergy, beneficiary,
    organized, biographical, constrained, global).

% Catholics in civil second unions. Under the prior discipline they lived excluded from communion; the framework opens a path of accompaniment, examination of conscience, and possible admission without a formal nullity declaration. Their access now depends on finding a pastor willing to walk the discernment path, which varies sharply by diocese.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, divorced_remarried_catholics, beneficiary,
    moderate, biographical, constrained, global).

% Laypeople whose religious identity is bound to doctrinal fixity, many of whom organized marriages, careers, and family life around the rule that the bond cannot dissolve. The framework retroactively reprices that fidelity: neighbors in identical situations now receive what they were told was impossible. Leaving would mean forfeiting the sacramental life that constitutes their identity, so they stay and absorb the ambiguity.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, traditional_lay_catholics, payer,
    moderate, generational, identity_locked, global).

% Spouses deserted by partners who then entered civil remarriages. They kept faith with the bond, often at severe personal cost, on the understanding that the bond binds both parties equally. The framework admits the deserting spouse's new household to sacramental life while their own fidelity gains no corresponding recognition; their options are resignation or departure from the community.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, abandoned_first_spouses, payer,
    powerless, biographical, trapped, global).

% Bishops, priests, and scholars who publicly challenged the framework: the cardinalial dubia, filial corrections, dioceses that declined to adopt admission guidelines. They bear career and canonical consequences including passed-over promotions, hostile coverage, and pressure from nunciatures. Ordination identity makes resignation unthinkable for most, so they contend from inside the structure.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, resisting_traditionalist_clergy, payer,
    organized, generational, identity_locked, global).

% Judges, defenders of the bond, and tribunal staff. The framework devalues their gatekeeping function twice over: the 2015 streamlined nullity process shortened their caseload cycle, and internal-forum discernment routes cases around adjudication entirely. Their expertise persists but their institutional centrality drains year over year.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, canon_tribunal_officials, payer,
    organized, biographical, constrained, global).

% The Dicastery for the Doctrine of the Faith and allied curial offices. They assess whether the framework's admissions remain theologically coherent with the indissolubility doctrine, respond to formal doubts, and draft clarifications. They neither collect nor pay under the framework, but their rulings move its boundary.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, vatican_doctrine_office, observer,
    institutional, generational, analytical, global).

% Communities outside full communion, or at its margins, that rejected the post-conciliar settlement long before this dispute. They argue the framework demonstrates that the mainstream church cannot hold doctrine, but they stand wholly outside the synodal conversation and address only their own constituencies.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, independent_traditionalist_communities, excluded,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_sacrament__civic_pastoral_reading, pastoral_hierarchs).
narrative_ontology:fixing_cost_class(marriage_sacrament__civic_pastoral_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective problem of what a sacramental community does with marriages that fail: instead of categorical exclusion of millions or universal tribunal adjudication, it distributes discernment to local pastors, keeps divorced-and-remarried members attached to sacramental life, and preserves organizational peace between factions by leaving the hardest questions formally open.
% TRANSFER_FUNCTION: Moves doctrinal certainty out of common possession, converting it into case-specific pastoral judgment, and moves discretionary authority to pastors and hierarchs; moves sacramental access toward divorced-and-remarried Catholics; moves the cost of ambiguity onto members whose identity and past sacrifices were priced under the fixed rule.
% ABSENT_VOICES: Abandoned first spouses had no organized voice in the synodal process that produced the framework; laypeople who structured their lives around the unrevisable rule were surveyed as a category but never seated as a party; independent traditionalist communities are wholly outside the conversation. Each would object that the framework reprices their fidelity without their consent.
% DISAPPEARANCE_RATIONALE: If the framework vanished overnight, divorced-and-remarried Catholics would again face categorical exclusion, nullity tribunals would regain their queue and their gatekeeping centrality, pastors would lose the discernment mandate, and the factional peace the ambiguity purchases would collapse into open jurisdictional conflict between conferences implementing rival disciplines.
% FOUNDING_PROBLEM: Millions of baptized Catholics in civil second unions lived permanently excluded from communion under a uniform constitutive rule, producing mass quiet attrition, while divergent regional practices threatened the unity of discipline the rule was meant to protect.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: diocesan tribunal statistics and the stated rationale of the 2015 streamlining document the caseload crisis; sociological surveys of Catholic family life and longitudinal attendance data document exclusion-driven attrition; pastors in non-implementing dioceses attest the same pastoral emergencies the framework answers. Traditionalist parties corroborate that the problem is real while disputing that it licenses this remedy.
narrative_ontology:disappearance_verdict(marriage_sacrament__civic_pastoral_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_sacrament__civic_pastoral_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_sacrament__civic_pastoral_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_sacrament__civic_pastoral_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_sacrament__civic_pastoral_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_sacrament__civic_pastoral_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_sacrament__civic_pastoral_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_sacrament__civic_pastoral_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate and rising (0.44 to 0.62 across the interval) because the framework's costs are diffuse and cumulative: each widening of discernment-based admission further reprices the fidelity of those who organized their lives under the unrevisable rule, and inconsistent regional application converts doctrinal certainty from a common possession into a jurisdictional lottery. Suppression (0.50) is moderate: no physical coercion, but appointment politics, canonical pressure, and the impossibility of exiting without forfeiting the sacramental life that constitutes traditional-lay identity. Theater ratio (0.38) reflects deliberate ambiguity as a governing technique — provisions drafted to be unenforceably vague so that opposing factions can each read victory, and synodal processes that defer decisions while performing consultation. Accessibility collapse is low-to-moderate (0.40): alternatives persist (independent traditionalist communities, Eastern churches, simple departure), but each carries identity-shattering cost for the locked seats. Resistance is substantial (0.60): public dubia, filial corrections, non-implementing dioceses. Suppression_requirement is tracked temporally because this story specifically traces enforcement-capacity change: the pastoral regime began with almost no enforcement machinery in 2016 and built guidelines, personnel expectations, and appointment discipline over the decade. All three series run on one shared time grid (2016-2026, biennial); 2024-and-earlier points are observed, the 2026 endpoints are projected. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and scope, in the engine's computation.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the agenda-setter seat the framework is genuine coordination it built: it solved a mass-exclusion crisis, retained a disaffected constituency, and preserved unity by tolerating plural implementation. From the traditional-lay seat the same structure operates as extraction: a rule presented as absolute when they complied is presented as discernable once others want relief, and their accumulated sacrifices are devalued without compensation. From the divorced-remarried seat it is liberation from a disciplinary deadlock; from the abandoned-spouse seat it is a double standard that honors reciprocity selectively. The resisting-clergy seat experiences enforcement aimed at its own marginalization, while the accompanist-clergy seat experiences the same enforcement as protection of its new mandate. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Divorced-and-remarried Catholics and accompanist clergy sit near the beneficiary end: the framework subsidizes their access and discretion respectively, and neither bears its diffuse costs. Traditional lay Catholics sit near the full-target end: they bear the extraction (repriced fidelity, lost normative clarity) and their identity-lock removes the exit modulation that would dampen effective extraction. Abandoned first spouses are the sharpest targets: powerless, trapped by the very reciprocity the framework suspends, unable to remarry sacramentally while the deserting spouse's household is accommodated. Resisting clergy and tribunal officials bear secondary costs not captured in the beneficiary/victim arrays; their directionalities derive from exit options and power atoms, and no directionality overrides are authored because overrides bind per power atom and would contaminate heterogeneous seats sharing atoms (for example, accompanist clergy and resisting clergy are both 'organized' but sit at opposite ends).
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two symmetrical errors. Reading the framework as pure extraction (snare) misses its genuine coordination function: it solves a real collective-action problem — millions of members in disciplinary deadlock, divergent regional practices threatening unity — and its participants include net beneficiaries who are not coerced into their gain. Reading it as pure coordination (rope) misses the asymmetric extraction running through the same structure: identifiable victims whose identity-level costs finance the settlement, sustained by active enforcement against internal resistance. The founding problem remains live (the divorced-remarried population and the pastoral emergencies are real and attested from outside the benefiting parties), so this is not resolved mandatrophy; the arrangement has not outlived its function. The drift risk runs toward theater rather than obsolescence: if deliberate ambiguity continues replacing decided discipline, theater_ratio crosses the proxy-substitution threshold and the arrangement degrades toward performance of mercy rather than its administration.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the marriage_sacrament kernel; what would the hierarchical_indissolubility_reading change structurally if it displaced this one?',
    'Comparative classification of the sibling story (marriage_sacrament__hierarchical_indissolubility_reading): its victim set, enforcement locus, and epsilon under the constitutive-rule regime.',
    'Under the sibling reading the victim set inverts: divorced-and-remarried Catholics become the extracted party and tribunals regain gatekeeping rent; this story''s beneficiary/victim declarations would flip polarity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of a contested kernel; sibling displacement inverts the victim set.').

omega_variable(
    victim_attribution_contest,
    'Does the loss of normative clarity experienced by traditional laity constitute genuine extraction, or is it the ordinary price of doctrinal development that they misdescribe as injury?',
    'Longitudinal study of traditional-lay communities distinguishing measurable harms (attrition of participation, giving, vocations) from preference dissatisfaction; test whether affected cohorts behave as expropriated parties rather than mere dissenters.',
    'If the harm is preference-level, effective extraction drops and the constraint trends toward rope; if identity-level and behavioral, the tangled_rope reading with high target-side chi is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_attribution_contest, conceptual, 'Whether doctrinal-relativization cost is extraction or contested doctrinal development.').

omega_variable(
    settlement_stability,
    'Will the pastoral-discernment settlement consolidate as the stable operative discipline, or revert under a successor pontificate or curial realignment?',
    'Track successive diocesan guideline adoptions, appointments to the Dicastery for the Doctrine of the Faith, and any future magisterial clarification narrowing or widening discernment-based admission.',
    'Consolidation entrenches the tangled_rope profile; reversion converts this arrangement into a transitional one whose sunset was implicit, changing lifecycle projections and drift dating.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settlement_stability, empirical, 'Consolidation versus reversion of the pastoral settlement.').

omega_variable(
    dissent_suppression_mechanism,
    'Is the suppression of traditionalist resistance structural (appointment politics, canonical pressure, marginalization of resistant clergy) or internalized (lay self-silencing, deference fused with faith)?',
    'Natural experiment from jurisdictions where resistant ordinaries retained office: if dissent persists where structural pressure is absent, a substantial share is internalized.',
    'An internalized share raises effective suppression above the structural measure and predicts persistence after any structural reversal; a structural share predicts rapid rebound under a favorable pontificate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dissent_suppression_mechanism, empirical, 'Structural versus internalized suppression of traditionalist dissent.').

omega_variable(
    cs_kernel_framing_underdetermination,
    'Is the kernel the codified sacramental texts (fixed_text framing) or the distributed practice of the episcopate (distributed framing)?',
    'Test which framing reproduces observed adjudication: when bishops'' conferences issue conflicting implementation guidelines, does resolution appeal to the texts or to conference practice?',
    'A distributed framing would reclassify authority_grounding and alter the commitment-system pattern; the fixed_text framing adopted here presumes an authoritative interpreter that conflicting-guidelines evidence partially undermines.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_kernel_framing_underdetermination, conceptual, 'Alternative framings of the kernel produce different commitment-system classifications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_sacrament__civic_pastoral_reading, 2016, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t2016, marriage_sacrament__civic_pastoral_reading, theater_ratio, 2016, 0.22).
narrative_ontology:measurement_basis(marr_tr_t2016, observed).
narrative_ontology:measurement(marr_tr_t2018, marriage_sacrament__civic_pastoral_reading, theater_ratio, 2018, 0.26).
narrative_ontology:measurement_basis(marr_tr_t2018, observed).
narrative_ontology:measurement(marr_tr_t2020, marriage_sacrament__civic_pastoral_reading, theater_ratio, 2020, 0.29).
narrative_ontology:measurement_basis(marr_tr_t2020, observed).
narrative_ontology:measurement(marr_tr_t2022, marriage_sacrament__civic_pastoral_reading, theater_ratio, 2022, 0.32).
narrative_ontology:measurement_basis(marr_tr_t2022, observed).
narrative_ontology:measurement(marr_tr_t2024, marriage_sacrament__civic_pastoral_reading, theater_ratio, 2024, 0.35).
narrative_ontology:measurement_basis(marr_tr_t2024, observed).
narrative_ontology:measurement(marr_tr_t2026, marriage_sacrament__civic_pastoral_reading, theater_ratio, 2026, 0.38).
narrative_ontology:measurement_basis(marr_tr_t2026, projected).

% Extraction over time
narrative_ontology:measurement(marr_be_t2016, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 2016, 0.44).
narrative_ontology:measurement_basis(marr_be_t2016, observed).
narrative_ontology:measurement(marr_be_t2018, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 2018, 0.49).
narrative_ontology:measurement_basis(marr_be_t2018, observed).
narrative_ontology:measurement(marr_be_t2020, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 2020, 0.52).
narrative_ontology:measurement_basis(marr_be_t2020, observed).
narrative_ontology:measurement(marr_be_t2022, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 2022, 0.56).
narrative_ontology:measurement_basis(marr_be_t2022, observed).
narrative_ontology:measurement(marr_be_t2024, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 2024, 0.59).
narrative_ontology:measurement_basis(marr_be_t2024, observed).
narrative_ontology:measurement(marr_be_t2026, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 2026, 0.62).
narrative_ontology:measurement_basis(marr_be_t2026, projected).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t2016, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 2016, 0.26).
narrative_ontology:measurement_basis(marr_su_t2016, observed).
narrative_ontology:measurement(marr_su_t2018, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 2018, 0.33).
narrative_ontology:measurement_basis(marr_su_t2018, observed).
narrative_ontology:measurement(marr_su_t2020, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 2020, 0.38).
narrative_ontology:measurement_basis(marr_su_t2020, observed).
narrative_ontology:measurement(marr_su_t2022, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 2022, 0.43).
narrative_ontology:measurement_basis(marr_su_t2022, observed).
narrative_ontology:measurement(marr_su_t2024, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 2024, 0.47).
narrative_ontology:measurement_basis(marr_su_t2024, observed).
narrative_ontology:measurement(marr_su_t2026, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 2026, 0.5).
narrative_ontology:measurement_basis(marr_su_t2026, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_sacrament__civic_pastoral_reading, attachment_coordination).
narrative_ontology:affects_constraint(marriage_sacrament__civic_pastoral_reading, marriage_sacrament__hierarchical_indissolubility_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'Catholic teaching on marriage and indissolubility' conflates two structurally distinct arrangements with different epsilon values and inverted victim sets. This story (civic_pastoral_reading) authors the pastoral-discernment regime: moderate extraction borne by traditional laity and abandoned spouses, coordination gained by divorced-remarried members and accompanist clergy. The sibling (hierarchical_indissolubility_reading) authors the tribunal-adjudication regime: extraction borne by divorced-remarried Catholics excluded from communion, coordination gained through uniform constitutive discipline. Each reading cites the other's instability as evidence for itself; the family link lets contamination analysis trace how drift in one regime propagates legitimacy pressure into the other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
