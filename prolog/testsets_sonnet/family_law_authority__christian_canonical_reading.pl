% ============================================================================
% CONSTRAINT STORY: family_law_authority__christian_canonical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__christian_canonical_reading, []).

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
 *   constraint_id: family_law_authority__christian_canonical_reading
 *   human_readable: Marriage as Sacrament/Ordinance under Christian Ecclesiastical Authority
 *   domain: comparative_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This story instantiates the Christian canonical reading of the
 *   family_law_authority kernel: marriage as sacrament (Catholic) or
 *   denominationally-governed ordinance (Protestant) under ecclesiastical
 *   authority. The Catholic branch holds sacramental indissolubility — no
 *   valid marriage can be dissolved by human authority, only annulled if
 *   never validly formed. Protestant traditions vary widely, generally
 *   permitting divorce on biblically-grounded criteria while still claiming
 *   denominational authority over what marriage validly is. The coordination
 *   function (a stable, communally witnessed framework for marriage) is real;
 *   the extraction runs through the asymmetry between those whose unions and
 *   exits the institution recognizes and those it does not, and through the
 *   institution's retained monopoly on defining validity even where civil law
 *   has moved on. This is a distinct constraint from the Hindu dharmashastra
 *   reading (dharmic textual/customary governance, different asymmetries
 *   around caste and gotra), the Muslim shariat reading (marriage as civil
 *   contract under Quranic/hadith law, unilateral talaq asymmetries), the
 *   Parsi Zoroastrian reading (community-preservation framing with
 *   intermarriage exclusion dynamics), and the secular contractual reading
 *   (state-adjudicated marriage as a dissolvable civil contract between
 *   formally equal parties) — each has its own beneficiary/victim structure
 *   and its own epsilon; they are linked, not merged.
 *
 * KEY AGENTS:
 *   - ecclesiastical_authorities: agenda_setter, institutional power, civilizational horizon — administers validity determinations and annulment tribunals
 *   - spouses_seeking_dissolution: payer, powerless, trapped exit — canonically bound regardless of civil status under Catholic indissolubility
 *   - same_sex_couples_excluded_from_sacramental_recognition: excluded, powerless, trapped — categorically foreclosed from the sacramental framework
 *   - civil_state_authorities: observer, institutional — runs a parallel civil track independent of ecclesiastical recognition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__christian_canonical_reading, 0.52).
domain_priors:suppression_score(family_law_authority__christian_canonical_reading, 0.58).
domain_priors:theater_ratio(family_law_authority__christian_canonical_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__christian_canonical_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__christian_canonical_reading, "Marriage as Sacrament/Ordinance under Christian Ecclesiastical Authority").
narrative_ontology:topic_domain(family_law_authority__christian_canonical_reading, "comparative_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(family_law_authority__christian_canonical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__christian_canonical_reading, '6b95f2d1-da62-4c32-b33e-edf2ae4a6537').
narrative_ontology:cs_kernel_codification('6b95f2d1-da62-4c32-b33e-edf2ae4a6537', formalized).
narrative_ontology:cs_authority_grounding('6b95f2d1-da62-4c32-b33e-edf2ae4a6537', lineage).
narrative_ontology:cs_interpretation_layer_present('6b95f2d1-da62-4c32-b33e-edf2ae4a6537').
narrative_ontology:cs_reading_relation('6b95f2d1-da62-4c32-b33e-edf2ae4a6537', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('6b95f2d1-da62-4c32-b33e-edf2ae4a6537', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('6b95f2d1-da62-4c32-b33e-edf2ae4a6537', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_reading_relation('6b95f2d1-da62-4c32-b33e-edf2ae4a6537', family_law_authority__secular_contractual_reading, influences).
narrative_ontology:cs_axiom('6b95f2d1-da62-4c32-b33e-edf2ae4a6537', foundational, marriage_as_indissoluble_sacramental_bond).
narrative_ontology:cs_axiom_status(marriage_as_indissoluble_sacramental_bond, holdable).
narrative_ontology:cs_axiom_grounding('6b95f2d1-da62-4c32-b33e-edf2ae4a6537', marriage_as_indissoluble_sacramental_bond, theological).
narrative_ontology:cs_axiom('6b95f2d1-da62-4c32-b33e-edf2ae4a6537', foundational, ecclesiastical_competence_over_marital_validity).
narrative_ontology:cs_axiom_status(ecclesiastical_competence_over_marital_validity, holdable).
narrative_ontology:cs_axiom_grounding('6b95f2d1-da62-4c32-b33e-edf2ae4a6537', ecclesiastical_competence_over_marital_validity, conventional).
narrative_ontology:cs_axiom('6b95f2d1-da62-4c32-b33e-edf2ae4a6537', secondary, denominational_divorce_on_biblical_grounds).
narrative_ontology:cs_axiom_status(denominational_divorce_on_biblical_grounds, holdable).
narrative_ontology:cs_axiom_grounding('6b95f2d1-da62-4c32-b33e-edf2ae4a6537', denominational_divorce_on_biblical_grounds, conventional).
narrative_ontology:cs_reference_frame('6b95f2d1-da62-4c32-b33e-edf2ae4a6537', sacramental_covenant_indissolubility).
narrative_ontology:cs_drift_state('6b95f2d1-da62-4c32-b33e-edf2ae4a6537', post_civil_divorce_normalization, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('6b95f2d1-da62-4c32-b33e-edf2ae4a6537', '').
narrative_ontology:cs_kernel_id(family_law_authority__christian_canonical_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, ecclesiastical_authorities).
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, denominational_institutions).
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, spouses_seeking_recognized_union).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, spouses_seeking_dissolution).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, civilly_divorced_catholics_seeking_remarriage).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, same_sex_couples_excluded_from_sacramental_recognition).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, annulment_petitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, spouses_seeking_recognized_union).
narrative_ontology:constraint_vindicates(family_law_authority__christian_canonical_reading, sacramental_indissolubility_doctrine).
narrative_ontology:constraint_vindicates(family_law_authority__christian_canonical_reading, ecclesiastical_competence_over_marriage_validity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Catholic tribunals (diocesan marriage courts, the Roman Rota) and Protestant denominational bodies adjudicate what counts as a valid marriage, administer annulment or divorce-permission processes, and set the doctrinal terms under which sacramental status is granted, withheld, or dissolved. They derive institutional authority, membership coherence, and doctrinal continuity from controlling this determination.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, ecclesiastical_authorities, agenda_setter,
    institutional, civilizational, analytical, global).

% Parish and congregational structures rely on marriage as a stable, church-mediated life event that reinforces membership, tithing, and generational continuity within the faith community. The sacramental or ordinance framing keeps marriage inside ecclesiastical rather than purely civil space.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, denominational_institutions, beneficiary,
    institutional, generational, constrained, national).

% Couples who marry within the church gain social legitimacy, community standing, and (for believers) a spiritually meaningful covenant witnessed by their tradition. They also submit to the tradition's terms for what makes the marriage valid and what would be required to end it, terms they did not individually negotiate.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, spouses_seeking_recognized_union, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(family_law_authority__christian_canonical_reading, spouses_seeking_recognized_union, payer).

% In traditions holding sacramental indissolubility (Catholic doctrine), a spouse in an unworkable or abusive marriage cannot obtain religious dissolution regardless of civil divorce; they may remain canonically bound, barred from communion if they remarry civilly, and dependent on a contested annulment process to exit with standing intact. In Protestant contexts divorce is more available but often still conditioned on denominationally defined biblical grounds (e.g., adultery, abandonment), leaving some petitioners without a recognized exit.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, spouses_seeking_dissolution, payer,
    powerless, biographical, trapped, local).

% Having obtained a civil divorce, they remain married in the eyes of the church absent an annulment. Remarrying civilly without annulment historically barred them from receiving communion (subject to recent pastoral softening under Amoris Laetitia). Their exit from the first marriage is recognized by the state but not by the institution whose recognition matters most to their religious and social standing.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, civilly_divorced_catholics_seeking_remarriage, payer,
    powerless, biographical, trapped, local).

% Categorically excluded from sacramental or ordinance marriage in the great majority of Catholic and traditional Protestant denominations regardless of civil marital status. They have no standing within the ecclesiastical process at all — not a contested case, but a foreclosed one — and can access only the civil-secular reading of marriage, which the church does not treat as equivalent.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, same_sex_couples_excluded_from_sacramental_recognition, excluded,
    powerless, biographical, trapped, national).

% Must petition a diocesan tribunal to establish that no valid marriage bond ever formed (defect of consent, form, or capacity), a process that can take months to years, involves fees, testimony, and canonical argument, and is not guaranteed to succeed. Petitioners bear the evidentiary and procedural burden to exit a bond the institution itself defines as otherwise permanent.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, annulment_petitioners, payer,
    powerless, biographical, constrained, regional).

% Maintain a parallel civil marriage and divorce regime that operates independently of ecclesiastical recognition. States generally do not enforce canonical indissolubility and permit civil divorce and remarriage regardless of a person's canonical status, creating the dual-track structure in which civil and ecclesiastical marital status can diverge.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, civil_state_authorities, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, tradition-anchored framework for what counts as a valid marriage, publicly witnessed and communally recognized, reducing ambiguity about family formation, inheritance expectations, and communal standing within the faith community across generations.
% TRANSFER_FUNCTION: Moves authority over the definition and dissolution of marriage from the individual couple to the ecclesiastical institution; moves social and spiritual standing away from those whose unions or exits the institution does not recognize, toward those whose unions it validates and toward the institution's own continuity and doctrinal authority.
% ABSENT_VOICES: Spouses trapped in unworkable Catholic marriages with no realistic annulment path, and same-sex couples categorically excluded from the sacramental framework, are not parties to the doctrinal determination of validity — they experience its consequences without a seat in the tribunal or the denominational body that sets the terms.
% DISAPPEARANCE_RATIONALE: If ecclesiastical authority over marriage vanished, civil marriage law would continue to govern property, custody, and dissolution largely unchanged (the state already runs a parallel track) — in that sense the world's legal architecture is not dependent on it. But for practicing believers, communal legitimacy, sacramental standing, and denominational identity would rearrange substantially: the church's claim to define valid marriage is precisely what would be lost, and that loss is disputed in value even where it is not disputed in fact.
% FOUNDING_PROBLEM: Early and medieval Christian communities sought to establish marriage as a covenant witnessed before God and community rather than a purely private or transactional arrangement, providing spiritual meaning, social stability, and a check against arbitrary repudiation of spouses (particularly wives) that existed under some prior legal regimes.
% FOUNDING_PROBLEM_CORROBORATION: Church authorities and many practicing believers attest the founding problem remains live — protecting marital commitment and spousal dignity against unilateral abandonment. Canon lawyers, pastoral counselors outside diocesan tribunal leadership, and sociologists of religion studying annulment case loads report that the doctrine's practical operation today functions substantially to gatekeep institutional standing and sacramental access rather than to protect vulnerable spouses, with the annulment process itself criticized by former tribunal staff as inconsistent and burdensome.
narrative_ontology:disappearance_verdict(family_law_authority__christian_canonical_reading, contested).
narrative_ontology:founding_problem_status(family_law_authority__christian_canonical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__christian_canonical_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(family_law_authority__christian_canonical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__christian_canonical_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__christian_canonical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_law_authority__christian_canonical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(family_law_authority__christian_canonical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) reflects moderate but real asymmetric costs: the institution retains authority over validity and dissolution that individuals cannot simply exit by personal decision, and this authority persists as a source of institutional continuity even as its protective function (guarding against arbitrary repudiation) has been substantially supplanted by civil law protections for spouses. Suppression (0.58, declining slightly over the measured interval as civil alternatives and pastoral softening — e.g., Amoris Laetitia-era accommodations — expand real exit) is moderate rather than severe because civil divorce and remarriage remain available in parallel; what is suppressed is specifically sacramental/denominational standing, not legal exit as such. Theater ratio (0.28, rising) tracks a growing share of tribunal and doctrinal activity oriented toward maintaining institutional coherence and doctrinal continuity rather than adjudicating genuine pastoral hardship, though the annulment process retains real substantive function for a meaningful share of cases.
 *
 * PERSPECTIVAL GAP:
 *   From the ecclesiastical authority's seat, sacramental marriage law is coordination: a stable, spiritually meaningful, communally legible institution that protects vulnerable spouses against arbitrary abandonment. From the seat of a spouse trapped in an unworkable marriage with no annulment path, or a same-sex couple with no path into the framework at all, the same structure operates as enforced exclusion or enforced permanence maintained by an institution that faces no comparable cost. The engine computes these as different seat-level classifications from the same structural data; the divergence is the finding, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Ecclesiastical authorities and denominational institutions sit at the beneficiary end: they set the terms of validity and derive institutional continuity, membership coherence, and doctrinal authority from controlling that determination, with essentially analytical exit (they are the rule-setters, not rule-takers). Spouses seeking recognized union benefit genuinely from the coordination function but also submit to terms they did not individually negotiate, placing them near symmetric with a payer inflection. Spouses seeking dissolution, civilly divorced Catholics seeking remarriage, and annulment petitioners sit near the target end — trapped or constrained exit, bearing the cost of a permanence doctrine or a burdensome validity-contest process. Same-sex couples are excluded rather than coordinated: the constraint does not extract from them through participation, it forecloses their participation entirely, which is a distinct structural position from the payer seats and is marked accordingly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — protecting marital covenant and vulnerable spouses against unilateral repudiation — is contested as live: civil law now provides substantial protection against arbitrary abandonment independent of ecclesiastical doctrine, which suggests the original protective function has been substantially absorbed by the state. What persists distinctively in the ecclesiastical framework is the sacramental/institutional-standing function, which is a different (not illegitimate, but different) good than the original protective rationale. Classifying this as tangled_rope rather than snare preserves the genuine coordination value (communal meaning-making, doctrinal continuity that many members affirmatively value) while still registering the asymmetric extraction borne by those the institution does not recognize or will not release — collapsing it to pure extraction would erase the real coordination good; collapsing it to pure rope would erase the documented victim set.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_sacramental_vs_alternatives,
    'Is the Christian canonical reading''s claim to define marital validity a distinct constraint from, or a variant instantiation of, the broader family_law_authority kernel also read through Hindu dharmashastra, Muslim shariat, Parsi Zoroastrian, and secular contractual frameworks?',
    'This is resolved by construction under the ε-invariance principle: each reading is authored as its own constraint story with its own ε, beneficiaries, and victims, linked via network.affects_constraints. The christian_canonical_reading is authored here; the sibling readings are separate stories.',
    'Confirms that sacramental permanence (Catholic no-divorce), denominational variance (Protestant divorce permitted), and church authority over validity constitute a structurally distinct constraint from the sibling readings'' asymmetries (e.g., unilateral talaq in the shariat reading, dharmic textual governance in the dharmashastra reading) rather than a single kernel requiring one averaged ε.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_sacramental_vs_alternatives, conceptual, 'Committer-frame boundary: this story is one reading among five of the family_law_authority kernel.').

omega_variable(
    sacramental_indissolubility_natural_or_constructed,
    'Is sacramental indissolubility a theologically discovered truth about the nature of marriage (as Catholic doctrine asserts) or an institutionally constructed rule that serves ecclesiastical authority''s interest in retaining exclusive jurisdiction over marital status?',
    'Comparative doctrinal history across Christian traditions (the same scriptural texts producing indissolubility in Catholic doctrine and permitted divorce in most Protestant traditions) as evidence for the constructed-variance reading; internal theological argument from natural law as evidence for the discovered-truth reading. Neither fully resolves the question empirically.',
    'If indissolubility is read as institutionally constructed rather than theologically discovered, the tangled_rope classification is reinforced (a rule serving institutional interest wrapped in coordination language); if read as genuinely discovered doctrine, the extraction borne by trapped spouses is better understood as a cost of a good the institution did not choose to impose but is bound to teach.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sacramental_indissolubility_natural_or_constructed, conceptual, 'Whether Catholic indissolubility doctrine is theological discovery or institutional construction — bears on how much of the measured extraction is attributable to agenda-setter interest versus doctrinal constraint the agenda-setter itself is bound by.').

omega_variable(
    annulment_process_substantive_or_theatrical,
    'What proportion of annulment tribunal activity substantively adjudicates genuine defects of consent/form/capacity versus functions as ritualized institutional gatekeeping producing a predetermined negative or positive outcome dressed as adjudication?',
    'Comparative analysis of annulment grant rates across dioceses and time periods, testimony from former tribunal staff, and canon-law scholarship on procedural consistency.',
    'A high substantive share supports the tangled_rope reading with a genuine (if burdensome) dispute-resolution function; a high theatrical share would push the theater_ratio and effective classification toward snare for the annulment-petitioner seat specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(annulment_process_substantive_or_theatrical, empirical, 'Whether annulment tribunals substantively adjudicate or theatrically ratify predetermined outcomes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__christian_canonical_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t0, family_law_authority__christian_canonical_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(fami_tr_t8, family_law_authority__christian_canonical_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(fami_tr_t16, family_law_authority__christian_canonical_reading, theater_ratio, 16, 0.19).
narrative_ontology:measurement(fami_tr_t24, family_law_authority__christian_canonical_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement(fami_tr_t32, family_law_authority__christian_canonical_reading, theater_ratio, 32, 0.25).
narrative_ontology:measurement(fami_tr_t40, family_law_authority__christian_canonical_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(fami_be_t0, family_law_authority__christian_canonical_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fami_be_t8, family_law_authority__christian_canonical_reading, base_extractiveness, 8, 0.4).
narrative_ontology:measurement(fami_be_t16, family_law_authority__christian_canonical_reading, base_extractiveness, 16, 0.45).
narrative_ontology:measurement(fami_be_t24, family_law_authority__christian_canonical_reading, base_extractiveness, 24, 0.48).
narrative_ontology:measurement(fami_be_t32, family_law_authority__christian_canonical_reading, base_extractiveness, 32, 0.5).
narrative_ontology:measurement(fami_be_t40, family_law_authority__christian_canonical_reading, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t0, family_law_authority__christian_canonical_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(fami_su_t8, family_law_authority__christian_canonical_reading, suppression_requirement, 8, 0.68).
narrative_ontology:measurement(fami_su_t16, family_law_authority__christian_canonical_reading, suppression_requirement, 16, 0.65).
narrative_ontology:measurement(fami_su_t24, family_law_authority__christian_canonical_reading, suppression_requirement, 24, 0.62).
narrative_ontology:measurement(fami_su_t32, family_law_authority__christian_canonical_reading, suppression_requirement, 32, 0.6).
narrative_ontology:measurement(fami_su_t40, family_law_authority__christian_canonical_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__christian_canonical_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(family_law_authority__christian_canonical_reading, 0.1).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, family_law_authority__hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, family_law_authority__muslim_shariat_reading).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, family_law_authority__parsi_zoroastrian_reading).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, family_law_authority__secular_contractual_reading).

% DUAL FORMULATION NOTE:
% This story is one reading of the family_law_authority kernel, decomposed per the ε-invariance principle into five sibling constraint stories, each with its own ε and beneficiary/victim structure. The christian_canonical_reading carries sacramental permanence (Catholic) alongside denominational variance (Protestant) and church-retained authority over validity determination — structurally distinct from the shariat reading's contractual/unilateral-dissolution asymmetry, the dharmashastra reading's textual/customary samskara framing, the Zoroastrian reading's community-preservation/intermarriage-exclusion dynamic, and the secular reading's state-adjudicated dissolvable-contract framing. All five are linked bidirectionally via affects_constraints; none subsumes another.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
