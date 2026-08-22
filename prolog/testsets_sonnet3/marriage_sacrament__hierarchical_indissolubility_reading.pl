% ============================================================================
% CONSTRAINT STORY: marriage_sacrament__hierarchical_indissolubility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_sacrament__hierarchical_indissolubility_reading, []).

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
 *   constraint_id: marriage_sacrament__hierarchical_indissolubility_reading
 *   human_readable: Sacramental Marriage as Ontological Bond Under Hierarchical Adjudication (Indissolubility as Constitutive)
 *   domain: religious_doctrine/canon_law/political_sociology
 *
 * SUMMARY:
 *   This story instantiates the hierarchical-indissolubility reading of the
 *   marriage_sacrament kernel: marriage is treated as an ontological reality
 *   constituted at the moment of valid consent, not an aspiration that can
 *   fail pastorally. Because the bond is held to be metaphysically real and
 *   permanent, only a hierarchical tribunal empowered to investigate the
 *   conditions of original consent can determine whether it ever existed —
 *   there is no doctrinal room for 'the marriage was real but should now be
 *   treated as dissolved.' This produces a high-extraction structure:
 *   divorced and civilly remarried Catholics are excluded from the Eucharist
 *   until and unless the tribunal declares the first marriage null, and the
 *   tribunal process itself imposes cost, delay, and re-traumatizing
 *   disclosure. This is generated as a single, ε-invariant constraint
 *   describing THIS reading only; the civic_pastoral_reading (indissolubility
 *   as ideal requiring compassionate discernment) is a structurally distinct
 *   constraint with a different beneficiary/victim profile and a different ε,
 *   generated as a separate story and linked via network.affects_constraints.
 *   Per DP-001 (ε-invariance), the two readings are not two measurements of
 *   one constraint but two constraints sharing a contested kernel.
 *
 * KEY AGENTS:
 *   - magisterial_hierarchy: sole doctrinal authority (institutional/analytical) — defines the ontological claim, bears no direct cost
 *   - diocesan_tribunal_system: administers annulment adjudication (institutional/analytical) — collects fees, imposes delay, gains professional legitimacy
 *   - divorced_remarried_catholics: primary target (powerless/trapped) — excluded from Eucharist absent annulment
 *   - annulment_petitioners: bear direct procedural cost (powerless/constrained) — time, money, re-litigated testimony
 *   - abuse_survivors_seeking_remarriage: doctrinally uncategorized victims (powerless/trapped) — validity framework does not accommodate 'marriage was real but became intolerable'
 *   - local_parish_clergy: enforcement-bearing intermediate seat (moderate/constrained) — administers sacramental exclusion without controlling the rule
 *   - civic_pastoral_reading_advocates: excluded theological minority (organized/constrained) — hold a live alternative that does not control tribunal practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_sacrament__hierarchical_indissolubility_reading, 0.68).
domain_priors:suppression_score(marriage_sacrament__hierarchical_indissolubility_reading, 0.71).
domain_priors:theater_ratio(marriage_sacrament__hierarchical_indissolubility_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_sacrament__hierarchical_indissolubility_reading, tangled_rope).
narrative_ontology:human_readable(marriage_sacrament__hierarchical_indissolubility_reading, "Sacramental Marriage as Ontological Bond Under Hierarchical Adjudication (Indissolubility as Constitutive)").
narrative_ontology:topic_domain(marriage_sacrament__hierarchical_indissolubility_reading, "religious_doctrine/canon_law/political_sociology").

domain_priors:requires_active_enforcement(marriage_sacrament__hierarchical_indissolubility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_sacrament__hierarchical_indissolubility_reading, '268d1515-3278-4531-998b-02173f580fb3').
narrative_ontology:cs_kernel_codification('268d1515-3278-4531-998b-02173f580fb3', formalized).
narrative_ontology:cs_authority_grounding('268d1515-3278-4531-998b-02173f580fb3', lineage).
narrative_ontology:cs_interpretation_layer_present('268d1515-3278-4531-998b-02173f580fb3').
narrative_ontology:cs_reading_relation('268d1515-3278-4531-998b-02173f580fb3', marriage_sacrament__civic_pastoral_reading, coexists_with).
narrative_ontology:cs_axiom('268d1515-3278-4531-998b-02173f580fb3', foundational, marriage_ontologically_constitutive_at_valid_consent).
narrative_ontology:cs_axiom_status(marriage_ontologically_constitutive_at_valid_consent, holdable).
narrative_ontology:cs_axiom_grounding('268d1515-3278-4531-998b-02173f580fb3', marriage_ontologically_constitutive_at_valid_consent, deontological).
narrative_ontology:cs_axiom('268d1515-3278-4531-998b-02173f580fb3', foundational, only_hierarchical_tribunal_may_adjudicate_bond_validity).
narrative_ontology:cs_axiom_status(only_hierarchical_tribunal_may_adjudicate_bond_validity, holdable).
narrative_ontology:cs_axiom_grounding('268d1515-3278-4531-998b-02173f580fb3', only_hierarchical_tribunal_may_adjudicate_bond_validity, conventional).
narrative_ontology:cs_reference_frame('268d1515-3278-4531-998b-02173f580fb3', tridentine_canonical_indissolubility).
narrative_ontology:cs_drift_state('268d1515-3278-4531-998b-02173f580fb3', post_amoris_laetitia_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('268d1515-3278-4531-998b-02173f580fb3', '').
narrative_ontology:cs_kernel_id(marriage_sacrament__hierarchical_indissolubility_reading, marriage_sacrament).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_sacrament__hierarchical_indissolubility_reading, diocesan_tribunal_system).
narrative_ontology:constraint_beneficiary(marriage_sacrament__hierarchical_indissolubility_reading, magisterial_hierarchy).
narrative_ontology:constraint_beneficiary(marriage_sacrament__hierarchical_indissolubility_reading, intact_sacramental_couples).
narrative_ontology:constraint_victim(marriage_sacrament__hierarchical_indissolubility_reading, divorced_remarried_catholics).
narrative_ontology:constraint_victim(marriage_sacrament__hierarchical_indissolubility_reading, annulment_petitioners).
narrative_ontology:constraint_victim(marriage_sacrament__hierarchical_indissolubility_reading, abuse_survivors_seeking_remarriage).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_sacrament__hierarchical_indissolubility_reading, local_parish_clergy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines marriage as an ontological reality instituted by divine law, not a contractual or pastoral arrangement subject to revision. Adjudicates validity exclusively through canon law and the tribunal system, and administers access to the sacraments (Eucharist, remarriage within the Church) on the basis of that adjudication. Bears no material cost from maintaining the doctrine and derives institutional coherence, doctrinal continuity, and authority from its position as sole interpreter.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, magisterial_hierarchy, agenda_setter,
    institutional, civilizational, analytical, global).

% Operates the annulment process: canon lawyers, tribunal judges, and defenders of the bond review petitions, gather testimony, and rule on whether a marriage was ever sacramentally valid. Charges fees, requires multi-year processes in many dioceses, and derives professional and institutional legitimacy from being the sole legitimate pathway to remarriage within the Church. Has no personal exit from the system it administers but is not the one bearing its costs.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, diocesan_tribunal_system, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(marriage_sacrament__hierarchical_indissolubility_reading, diocesan_tribunal_system, beneficiary).

% Receive the doctrine's affirmation that their bond is permanent and ontologically real, which the reading treats as a source of stability, meaning, and communal recognition. Do not directly interact with the tribunal system unless their own marriage is later contested, but benefit from the doctrine's symbolic elevation of what they have.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, intact_sacramental_couples, beneficiary,
    moderate, biographical, constrained, local).

% Have civilly remarried without an annulment (or been denied one) and are, under this reading, barred from receiving the Eucharist because they are held to be in an objectively irregular, ongoing state of adultery against an indissoluble first bond. Exit means either living permanently outside full sacramental communion, obtaining a costly and uncertain annulment, or leaving the Church; identity and family ties to the faith community make full departure costly.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, divorced_remarried_catholics, payer,
    powerless, biographical, trapped, local).

% Attempt to have a prior marriage declared null so they can remarry within the Church. Pay tribunal fees, wait months to years, must produce witnesses and testimony re-litigating the most painful period of their lives, and face outcomes they cannot appeal outside the same hierarchical structure that set the standard. Some dioceses waive fees; many do not, and the time cost and emotional cost remain regardless.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, annulment_petitioners, payer,
    powerless, biographical, constrained, regional).

% Fled marriages involving abuse and now seek to remarry and remain in full communion. Must prove, through the tribunal process, that the original marriage was invalid at its inception (lack of due discretion, grave lack of consent, etc.) rather than simply that it failed or became unsafe — the ontological-validity framing gives no doctrinal category for 'the marriage was real but became intolerable,' forcing survivors into a validity narrative that may not fit their actual experience.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, abuse_survivors_seeking_remarriage, payer,
    powerless, biographical, trapped, local).

% Administer the sacraments at the parish level and are bound to withhold Eucharist from those in unresolved irregular unions, even when they have direct pastoral knowledge of a person's suffering, remorse, or the injustice of their prior marriage. Cannot exercise discretion beyond what the hierarchy permits without risking their own standing; absorb the relational cost of enforcement without controlling the rule.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, local_parish_clergy, agenda_setter,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(marriage_sacrament__hierarchical_indissolubility_reading, local_parish_clergy, payer).

% Theologians, pastoral ministers, and lay Catholics who hold that indissolubility is an ideal requiring compassionate discernment rather than a metaphysical fact demanding tribunal adjudication. Their position is a live minority current within Catholic theological discourse (associated with debates following Amoris Laetitia) but is not the operative doctrine enforced by tribunals; their voice shapes pastoral practice at the margins but does not control sacramental access.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, civic_pastoral_reading_advocates, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_sacrament__hierarchical_indissolubility_reading, diffuse).
narrative_ontology:fixing_cost_class(marriage_sacrament__hierarchical_indissolubility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, stable, non-negotiable definition of marital validity across a global institution, preventing ad hoc local variation in who counts as validly married and preserving doctrinal continuity across cultures and centuries.
% TRANSFER_FUNCTION: Moves sacramental standing, communal belonging, and eligibility for full participation in the Church's central rite (the Eucharist) away from those in second unions without annulment, and channels time, money, and emotional disclosure from petitioners toward the tribunal apparatus that adjudicates validity.
% ABSENT_VOICES: Civic/pastoral-reading advocates hold a theologically live alternative but do not control tribunal outcomes. Abuse survivors whose experience does not fit the 'invalid from the start' category have no doctrinal category for their situation under this reading. Divorced Catholics who have de facto left the Church rather than navigate the tribunal are not present to testify to what drove them out.
% DISAPPEARANCE_RATIONALE: If hierarchical adjudication and constitutive indissolubility disappeared overnight, the tribunal system would lose its function, millions of divorced and remarried Catholics currently excluded from Eucharist would gain immediate access, dioceses would lose tribunal fee revenue and canon-lawyer employment, and the Church's doctrinal claim to a single ontological account of marriage would need to be replaced by some other adjudicating principle (likely the civic/pastoral reading's discernment model).
% FOUNDING_PROBLEM: Early and medieval Christian communities needed a stable, non-negotiable answer to who was validly married, to prevent powerful men from discarding wives at will, to protect the legal and social status of spouses and children, and to distinguish sacramental marriage from the more easily dissolved unions permitted under Roman and various civil law.
% FOUNDING_PROBLEM_CORROBORATION: The hierarchy and canon lawyers attest the problem remains live: without a firm doctrine, they argue, marriage becomes infinitely negotiable and the vulnerable spouse loses protection. Independent historians of canon law and sociologists of religion note the protective function was real in eras of near-total male authority over marriage and family property, but argue that in contemporary contexts with civil divorce and legal protections for spouses, the tribunal's protective function has been substantially superseded by civil family law — leaving the doctrine's primary operative effect as sacramental gatekeeping rather than protection of the vulnerable. This corroboration comes from scholarship outside the tribunal system, not from the beneficiary institutions themselves.
narrative_ontology:disappearance_verdict(marriage_sacrament__hierarchical_indissolubility_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_sacrament__hierarchical_indissolubility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_sacrament__hierarchical_indissolubility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_sacrament__hierarchical_indissolubility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_sacrament__hierarchical_indissolubility_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_sacrament__hierarchical_indissolubility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_sacrament__hierarchical_indissolubility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_sacrament__hierarchical_indissolubility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the direct, material cost imposed on divorced/remarried Catholics and annulment petitioners: exclusion from the central sacrament, tribunal fees, multi-year delays, and mandatory re-disclosure of trauma. Suppression (0.71) is authored higher than extractiveness because the mechanism enforcing exclusion — denial of Eucharist, absence of any doctrinal exit route within the framework, and the requirement that all remarriage pass through the same hierarchical adjudication — is a structural, non-negotiable barrier rather than a matter of degree. Theater ratio (0.38) is moderate: the tribunal process does perform genuine fact-finding in many cases, but a meaningful share of its activity (extended timelines, procedural formalism, defender-of-the-bond argumentation regardless of case merits) functions to protect the doctrine's coherence rather than to serve petitioners. Accessibility collapse (0.60) and resistance (0.58) are mid-range: unlike a mountain, real alternative theological framings exist and are actively argued (the civic/pastoral reading, married priests' movements, some bishops' conferences pushing pastoral flexibility post-Amoris Laetitia) — the doctrine has not achieved mountain-grade closure, but the hierarchy's control over sacramental access keeps most alternatives from becoming operative.
 *
 * DIRECTIONALITY LOGIC:
 *   The magisterial hierarchy and diocesan tribunal system sit at the beneficiary/agenda-setter end: they define the ontological claim, administer its adjudication, and bear no direct material cost from maintaining it, while gaining doctrinal coherence and institutional continuity. Divorced/remarried Catholics, annulment petitioners, and abuse survivors sit at the target end: they bear direct costs (sacramental exclusion, fees, delay, forced re-narration of trauma) with limited exit — leaving the Church is costly given identity and family ties, and there is no doctrinal path to remarriage without tribunal approval. Local clergy occupy an intermediate position: they administer enforcement (agenda_setter-adjacent) but personally absorb relational and pastoral costs they did not set (payer-adjacent), which is why they carry a secondary_role.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — protecting vulnerable spouses from arbitrary dissolution in eras of near-total male authority over marriage — is largely superseded by civil family law in most jurisdictions where this doctrine now operates. The doctrine's classification as tangled_rope rather than pure snare rests on the residual, genuine coordination function it still performs: providing a globally stable, non-negotiable definition of sacramental marriage that some communities still rely on to protect the socially vulnerable in contexts where civil protections remain weak. But the tribunal apparatus's persistence in jurisdictions with strong civil protections, its fee structures, and its lack of a doctrinal category for 'marriage was valid but became unsafe' indicate genuine extraction riding on a genuine but partially obsolete coordination claim — exactly the tangled_rope signature, not a case that should be waved through as pure coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_reality_vs_constructed_doctrine,
    'Is marital indissolubility a discovered metaphysical fact about the nature of the sacramental bond, or a doctrinal construction that serves the institutional interests of the hierarchy that adjudicates it?',
    'This is not resolvable by empirical inquiry within the framework itself — it is the central theological dispute the kernel contest exists to represent. Partial evidence: historical variation in how indissolubility was defined and enforced across centuries and rites (the Eastern Orthodox oikonomia tradition permits remarriage under some conditions within a shared apostolic lineage) suggests the specific hierarchical-adjudication mechanism is at least partly a matter of ecclesiastical development rather than pure metaphysical necessity.',
    'If indissolubility is a discovered ontological fact, the tribunal system is a necessary (if costly) mechanism for correctly identifying reality. If it is substantially a doctrinal construction that has hardened institutional authority over time, the extraction is better read as the constitutive mechanism rather than incidental cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ontological_reality_vs_constructed_doctrine, conceptual, 'Whether the reading''s foundational ontological claim is discovered or constructed — the central kernel-level dispute.').

omega_variable(
    sibling_reading_structural_delta,
    'How would classification and victim set change under the civic_pastoral_reading of the same kernel?',
    'Generate civic_pastoral_reading as its own constraint story: authors indissolubility as an ideal requiring compassionate discernment, likely with substantially lower extractiveness (fewer categorical Eucharist exclusions, case-by-case pastoral accompaniment rather than binary tribunal adjudication) and a different, smaller victim set.',
    'The two readings are linked via network.affects_constraints as siblings sharing the marriage_sacrament kernel; where a diocese or bishops'' conference shifts operative practice toward the civic_pastoral_reading (as some have post-Amoris Laetitia), the hierarchical_indissolubility_reading''s effective enforcement and victim count would decline even though the reading itself remains doctrinally live.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Structural delta to the sibling reading in the same kernel contest.').

omega_variable(
    protective_function_obsolescence,
    'In jurisdictions with robust civil family law protecting spouses and children, does the tribunal system''s original protective coordination function still operate, or has it been fully superseded by civil law, leaving only the sacramental-gatekeeping function?',
    'Comparative analysis of tribunal caseloads and outcomes in jurisdictions with strong vs. weak civil family law protections; if the tribunal''s practical effect on child custody, spousal support, and protection from arbitrary dissolution is negligible relative to civil courts, the residual coordination claim is undermined.',
    'If the protective function is fully superseded, this constraint would score closer to a pure snare in high-civil-protection jurisdictions and closer to genuine tangled_rope in jurisdictions where civil protections remain weak — suggesting the classification may itself be jurisdiction-dependent within this single reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protective_function_obsolescence, empirical, 'Whether the doctrine''s original protective coordination function survives civil-law supersession.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_sacrament__hierarchical_indissolubility_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(marr_tr_t10, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(marr_tr_t20, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(marr_tr_t30, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 30, 0.31).
narrative_ontology:measurement(marr_tr_t40, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 40, 0.34).
narrative_ontology:measurement(marr_tr_t50, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 50, 0.36).
narrative_ontology:measurement(marr_tr_t60, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 60, 0.38).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(marr_be_t10, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement(marr_be_t20, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(marr_be_t30, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(marr_be_t40, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(marr_be_t50, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 50, 0.67).
narrative_ontology:measurement(marr_be_t60, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 60, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(marr_su_t10, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 10, 0.59).
narrative_ontology:measurement(marr_su_t20, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(marr_su_t30, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(marr_su_t40, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement(marr_su_t50, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 50, 0.7).
narrative_ontology:measurement(marr_su_t60, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 60, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_sacrament__hierarchical_indissolubility_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_sacrament__hierarchical_indissolubility_reading, marriage_sacrament_civic_pastoral_reading).

% DUAL FORMULATION NOTE:
% This story and marriage_sacrament_civic_pastoral_reading are sibling readings of the same marriage_sacrament kernel (a persisting Catholic commitment to marriage's sacred, permanent character). They are NOT two measurements of one constraint but two structurally distinct constraints: this reading treats indissolubility as constitutive and requires hierarchical tribunal adjudication of original validity (high extraction: categorical Eucharist exclusion, tribunal costs, no doctrinal category for 'valid but now intolerable'); the sibling reading treats indissolubility as an ideal requiring compassionate pastoral discernment case-by-case (expected lower extraction, different victim set, no binary validity gate). Per DP-001 ε-invariance, each carries its own ε and its own classification; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
