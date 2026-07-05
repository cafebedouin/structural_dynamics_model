% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__christian_canonical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__christian_canonical_reading, []).

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
 *   constraint_id: marriage_authority_kernel__christian_canonical_reading
 *   human_readable: Christian Canonical Reading of Marriage/Family Law Authority (Indian Christian Marriage Act 1872)
 *   domain: comparative_law/constitutional_pluralism/religious_governance
 *
 * SUMMARY:
 *   The Indian Christian Marriage Act, 1872, codifies marital solemnization
 *   and dissolution for Indian Christians using grounds descended from
 *   Christian canonical doctrine on the sacramental character of marriage.
 *   This produced, until amended in 2001, a distinctive gender asymmetry:
 *   wives had to prove adultery plus an additional fault (cruelty or
 *   desertion) to obtain divorce, while husbands needed only adultery. The
 *   statute channels marital-status adjudication partly through civil courts
 *   and partly through the interpretive authority of church bodies over what
 *   counts as a valid marriage or ground for annulment. This story is ONE
 *   reading of the contested marriage-authority kernel in India's
 *   personal-law pluralism: the claim that marriage/family law authority for
 *   Christians derives from canonical law as codified in 1872. It does not
 *   describe or average over the Hindu, Muslim, Parsi, or secular civil
 *   readings of the same underlying kernel — those are separate constraints,
 *   linked here only through network edges, each with its own epsilon and
 *   stakeholder structure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__christian_canonical_reading, 0.42).
domain_priors:suppression_score(marriage_authority_kernel__christian_canonical_reading, 0.58).
domain_priors:theater_ratio(marriage_authority_kernel__christian_canonical_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__christian_canonical_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__christian_canonical_reading, "Christian Canonical Reading of Marriage/Family Law Authority (Indian Christian Marriage Act 1872)").
narrative_ontology:topic_domain(marriage_authority_kernel__christian_canonical_reading, "comparative_law/constitutional_pluralism/religious_governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__christian_canonical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__christian_canonical_reading, '640eba2d-8a57-4665-8253-900a19ad348b').
narrative_ontology:cs_kernel_codification('640eba2d-8a57-4665-8253-900a19ad348b', fixed_text).
narrative_ontology:cs_authority_grounding('640eba2d-8a57-4665-8253-900a19ad348b', lineage).
narrative_ontology:cs_interpretation_layer_present('640eba2d-8a57-4665-8253-900a19ad348b').
narrative_ontology:cs_reading_relation('640eba2d-8a57-4665-8253-900a19ad348b', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('640eba2d-8a57-4665-8253-900a19ad348b', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('640eba2d-8a57-4665-8253-900a19ad348b', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('640eba2d-8a57-4665-8253-900a19ad348b', marriage_authority_kernel__secular_civil_reading, influences).
narrative_ontology:cs_axiom('640eba2d-8a57-4665-8253-900a19ad348b', foundational, marriage_as_sacrament_not_contract).
narrative_ontology:cs_axiom_status(marriage_as_sacrament_not_contract, holdable).
narrative_ontology:cs_axiom_grounding('640eba2d-8a57-4665-8253-900a19ad348b', marriage_as_sacrament_not_contract, theological).
narrative_ontology:cs_axiom('640eba2d-8a57-4665-8253-900a19ad348b', secondary, church_tribunal_competence_over_annulment).
narrative_ontology:cs_axiom_status(church_tribunal_competence_over_annulment, holdable).
narrative_ontology:cs_axiom_grounding('640eba2d-8a57-4665-8253-900a19ad348b', church_tribunal_competence_over_annulment, conventional).
narrative_ontology:cs_axiom('640eba2d-8a57-4665-8253-900a19ad348b', secondary, gender_symmetric_fault_grounds).
narrative_ontology:cs_axiom_status(gender_symmetric_fault_grounds, overridden).
narrative_ontology:cs_axiom_grounding('640eba2d-8a57-4665-8253-900a19ad348b', gender_symmetric_fault_grounds, conventional).
narrative_ontology:cs_reference_frame('640eba2d-8a57-4665-8253-900a19ad348b', canonical_sacramental_indissolubility).
narrative_ontology:cs_drift_state('640eba2d-8a57-4665-8253-900a19ad348b', post_2001_amendment_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('640eba2d-8a57-4665-8253-900a19ad348b', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__christian_canonical_reading, church_authorities).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__christian_canonical_reading, christian_clergy_officiants).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__christian_canonical_reading, male_spouses_in_intact_marriages).
narrative_ontology:constraint_victim(marriage_authority_kernel__christian_canonical_reading, christian_wives_seeking_divorce).
narrative_ontology:constraint_victim(marriage_authority_kernel__christian_canonical_reading, women_in_abusive_christian_marriages).
narrative_ontology:constraint_victim(marriage_authority_kernel__christian_canonical_reading, christian_converts_and_interfaith_spouses).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__christian_canonical_reading, sacramental_indissolubility_of_marriage).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__christian_canonical_reading, ecclesiastical_competence_over_marital_status).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Dioceses and denominational tribunals administer marriage registration, annulment doctrine, and pastoral discipline under the 1872 Act's incorporation of canonical grounds. They set the theological standard for what counts as a valid marriage and a valid ground for its dissolution, and their institutional standing is enhanced by remaining the recognized interpreter of the kernel.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, church_authorities, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Perform marriages and participate in local pastoral adjudication of marital disputes, gaining social authority and often fee-based income from the ceremonies and counsel the Act channels through them. Their livelihood and status are tied to marriage remaining an ecclesiastically mediated event rather than a purely civil one.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, christian_clergy_officiants, beneficiary,
    organized, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__christian_canonical_reading, christian_clergy_officiants, agenda_setter).

% Benefit from the Act's historically asymmetric fault grounds (until 2001 amendments, wives needed to prove adultery plus an additional fault such as cruelty or desertion, while husbands needed only adultery) and from a legal culture that treats marital stability as the default and dissolution as an exception requiring the wife to make the harder case.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, male_spouses_in_intact_marriages, beneficiary,
    moderate, biographical, mobile, national).

% Face a fault-based divorce regime historically requiring higher evidentiary burdens than their husbands', litigation in civil courts that must apply the Act's canonically-derived grounds, and the reputational cost of a church community that treats the marriage as sacramentally binding. Even post-2001 equalization, they carry the delay, expense, and social stigma of proving fault in a system whose grounds were set by ecclesiastical doctrine, not by the parties' own account of the marriage's failure.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, christian_wives_seeking_divorce, payer,
    powerless, biographical, trapped, national).

% Cannot exit on the basis of unhappiness or incompatibility alone; must prove cruelty, adultery, or desertion to a civil court applying grounds that trace to canonical restrictions on dissolving a sacrament. Community and clergy pressure to reconcile rather than divorce compounds the legal barrier, and geographic or economic dependence on the church community narrows exit further.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, women_in_abusive_christian_marriages, payer,
    powerless, immediate, trapped, local).

% Individuals who convert into or marry across the Christian personal-law boundary find their marital status governed by canonical grounds they may not have consented to as a religious matter, with annulment or divorce routed through church-recognized categories that do not map onto their actual reasons for wanting out.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, christian_converts_and_interfaith_spouses, payer,
    powerless, biographical, constrained, national).

% Apply the 1872 Act's statutory language, which encodes canonical fault grounds, while increasingly reading in constitutional equality principles (as in the 2001 amendments equalizing fault grounds). They are the forum where the tension between the canonical kernel and constitutional individual-rights norms actually surfaces and gets partially resolved.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, secular_civil_courts, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__christian_canonical_reading, secular_civil_courts, agenda_setter).

% Christian women's rights groups and legal reformers who argued for decades that the pre-2001 fault asymmetry violated Article 14 were not part of the original ecclesiastical drafting process and had to litigate for statutory amendment from outside the church's interpretive authority; they remain outside the ongoing tribunal-level adjudication of annulment grounds.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, constitutional_equality_advocates, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority_kernel__christian_canonical_reading, church_authorities).
narrative_ontology:fixing_cost_class(marriage_authority_kernel__christian_canonical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, stable, community-recognized framework for solemnizing Christian marriages and adjudicating their validity and dissolution, so that Christian couples, clergy, and courts share one set of grounds rather than each congregation or diocese improvising its own rule.
% TRANSFER_FUNCTION: Moves the practical burden of proving marital breakdown from a symmetric standard onto the party (historically and disproportionately the wife) seeking exit, while channeling social and institutional authority over marital status to church bodies and clergy rather than to the spouses themselves.
% ABSENT_VOICES: Christian women's advocacy groups and interfaith/convert spouses were not present at the 1872 codification and had to seek relief through decades of civil litigation and legislative amendment (culminating in 2001) from outside the ecclesiastical interpretive structure that set the original grounds.
% DISAPPEARANCE_RATIONALE: If the canonical-derivation reading disappeared and Christian marriages were governed purely by a secular fault-neutral standard, church tribunals would lose their adjudicatory relevance to marital status, clergy would lose a source of pastoral authority, and trapped spouses currently unable to exit on incompatibility grounds would gain a route out — a substantial rearrangement of who holds leverage inside Christian marriages.
% FOUNDING_PROBLEM: In 1872, colonial administrators needed a uniform statute to solemnize and dissolve marriages for India's Christian population, and the available template was the canonical framework already familiar to the churches and to British ecclesiastical law, which supplied fault-based grounds rooted in the sacramental view that marriage is not casually dissoluble.
% FOUNDING_PROBLEM_CORROBORATION: Church authorities attest the sacramental-indissolubility problem remains live and justifies retaining fault grounds. The Law Commission of India and Christian women's advocacy organizations (external to church governance) attested in submissions leading to the 2001 amendment that the specific problem the asymmetric grounds solved — deterring frivolous or hasty divorce — was outweighed by the harm of trapping women in abusive marriages, and that the canonical framing had outlived its protective justification even where the underlying institution persists.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__christian_canonical_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__christian_canonical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__christian_canonical_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_authority_kernel__christian_canonical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__christian_canonical_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__christian_canonical_reading_tests).
:- end_tests(marriage_authority_kernel__christian_canonical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.42, declining from 0.62 at codification) tracks the narrowing gap between canonical fault grounds and constitutional equality norms, chiefly via the 2001 amendment equalizing divorce grounds. Suppression (0.58) remains substantial because exit from a Christian marriage still requires proving fault to a standard set originally by ecclesiastical doctrine rather than by mutual consent or no-fault dissolution, and because church-community pressure supplements the legal barrier. Theater ratio rises modestly over the interval (0.12 to 0.28) as the sacramental-indissolubility rationale increasingly functions as institutional legitimation for retained clerical authority over marital status even as the practical legal asymmetry has been substantially reduced by statute.
 *
 * PERSPECTIVAL GAP:
 *   Church authorities experience the arrangement as continuous doctrinal coordination they steward; trapped wives and abuse survivors experience the same statute as an enforced barrier to exit whose content was set by an institution they do not control. The engine computes this divergence from the declared power/exit structure per seat; the claimed_type does not pre-resolve it.
 *
 * DIRECTIONALITY LOGIC:
 *   Church authorities and clergy sit at the beneficiary end: they administer the kernel, retain interpretive authority over what counts as a valid marriage or annulment ground, and derive social and institutional standing from that role. Male spouses in intact marriages benefited structurally from the pre-2001 asymmetric fault standard even without actively administering it. Wives seeking divorce, women in abusive marriages, and converts/interfaith spouses sit at the target end: trapped or constrained exit options and a legal standard whose content was set by a body they had no hand in constituting.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a uniform colonial-era statute for solemnizing Christian marriage, using the doctrinal template of sacramental indissolubility to deter casual dissolution) is contested rather than cleanly dead or live: church authorities maintain the sacramental rationale is still operative, while external corroboration (Law Commission submissions, women's advocacy litigation) established that the specific asymmetric-burden mechanism had outlived any protective justification, producing the 2001 legislative correction. The classification as tangled_rope rather than snare reflects that a genuine coordination function persists (a shared, stable framework for Christian marital status) alongside the asymmetric extraction that required active civil-court and legislative correction to partially unwind — pure extraction would not have yielded to statutory amendment in the way this constraint did.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sacramental_vs_constructed_kernel,
    'Is the canonical-derivation reading a genuine theological necessity flowing from the nature of Christian marriage as a sacrament, or a constructed legal artifact of colonial codification that happens to have been dressed in doctrinal language?',
    'Comparative analysis of how other Christian-majority jurisdictions (e.g., post-Vatican II Catholic canon law reforms, Anglican divorce liberalization) have handled the same sacramental premise without retaining identical fault asymmetries — convergence would suggest colonial-era codification choices, not doctrinal necessity, drove the specific 1872 grounds.',
    'If constructed rather than doctrinally necessary, the persistence of fault-based grounds after 2001 partial reform is better read as institutional inertia protecting clerical authority than as fidelity to an unchangeable theological commitment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sacramental_vs_constructed_kernel, conceptual, 'Whether the canonical grounds reflect theological necessity or colonial-era institutional choice.').

omega_variable(
    kernel_reading_disagreement_location,
    'Among the five readings of the marriage_authority_kernel, where exactly does the disagreement locate — is it about WHO has interpretive authority (church vs. civil court vs. community board) or about WHAT SUBSTANTIVE grounds govern dissolution (fault vs. no-fault, sacramental vs. contractual)?',
    'Map each reading''s axioms onto the two axes (authority-locus, substantive-grounds) and check whether the readings cluster by axis or vary independently — clustering would suggest one master disagreement; independent variation would suggest the kernel actually bundles two separable contests.',
    'If the kernel is separable into an authority-locus dispute and a substantive-grounds dispute, personal-law reform could target one axis (e.g., moving interpretive authority to civil courts while retaining community-specific substantive grounds) without collapsing the whole pluralist structure — informs whether reform of this reading requires displacing church authority entirely or only its exclusive substantive grounds-setting.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Whether the christian_canonical_reading''s dispute with its siblings is about authority or about substantive grounds, or both.').

omega_variable(
    post_amendment_residual_extraction,
    'Now that the 2001 amendment has equalized formal fault grounds between spouses, does meaningful gender-asymmetric extraction persist through informal channels (community pressure, clergy counsel steering toward reconciliation, social stigma) even though the statutory text is now neutral?',
    'Empirical study comparing divorce filing rates, withdrawal rates, and time-to-resolution for Christian wives versus husbands post-2001, controlling for socioeconomic factors, alongside qualitative interviews on clergy counseling practices.',
    'If informal extraction persists despite formal equality, the declining extractiveness trajectory in the measurements overstates the actual improvement in wives'' exit options — suppression would remain substantially internalized/social rather than purely structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_amendment_residual_extraction, empirical, 'Whether formal statutory equalization in 2001 eliminated or merely formalized-over gendered extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__christian_canonical_reading, 1872, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1872, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 1872, 0.12).
narrative_ontology:measurement_basis(marr_tr_t1872, observed).
narrative_ontology:measurement(marr_tr_t1923, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 1923, 0.16).
narrative_ontology:measurement_basis(marr_tr_t1923, observed).
narrative_ontology:measurement(marr_tr_t1950, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 1950, 0.2).
narrative_ontology:measurement_basis(marr_tr_t1950, observed).
narrative_ontology:measurement(marr_tr_t2001, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 2001, 0.24).
narrative_ontology:measurement_basis(marr_tr_t2001, observed).
narrative_ontology:measurement(marr_tr_t2012, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 2012, 0.26).
narrative_ontology:measurement_basis(marr_tr_t2012, observed).
narrative_ontology:measurement(marr_tr_t2024, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 2024, 0.28).
narrative_ontology:measurement_basis(marr_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(marr_be_t1872, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 1872, 0.62).
narrative_ontology:measurement_basis(marr_be_t1872, observed).
narrative_ontology:measurement(marr_be_t1923, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 1923, 0.6).
narrative_ontology:measurement_basis(marr_be_t1923, observed).
narrative_ontology:measurement(marr_be_t1950, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 1950, 0.57).
narrative_ontology:measurement_basis(marr_be_t1950, observed).
narrative_ontology:measurement(marr_be_t2001, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 2001, 0.44).
narrative_ontology:measurement_basis(marr_be_t2001, observed).
narrative_ontology:measurement(marr_be_t2012, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 2012, 0.43).
narrative_ontology:measurement_basis(marr_be_t2012, observed).
narrative_ontology:measurement(marr_be_t2024, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 2024, 0.42).
narrative_ontology:measurement_basis(marr_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1872, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 1872, 0.7).
narrative_ontology:measurement_basis(marr_su_t1872, observed).
narrative_ontology:measurement(marr_su_t1923, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 1923, 0.68).
narrative_ontology:measurement_basis(marr_su_t1923, observed).
narrative_ontology:measurement(marr_su_t1950, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 1950, 0.66).
narrative_ontology:measurement_basis(marr_su_t1950, observed).
narrative_ontology:measurement(marr_su_t2001, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 2001, 0.6).
narrative_ontology:measurement_basis(marr_su_t2001, observed).
narrative_ontology:measurement(marr_su_t2012, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 2012, 0.59).
narrative_ontology:measurement_basis(marr_su_t2012, observed).
narrative_ontology:measurement(marr_su_t2024, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 2024, 0.58).
narrative_ontology:measurement_basis(marr_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__christian_canonical_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, parsi_communal_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, secular_civil_reading).

% DUAL FORMULATION NOTE:
% This story is one of five constraint stories decomposing the natural-language concept 'marriage/family law authority in India' into structurally distinct kernel readings, each tied to a different community's codified personal law and interpretive authority. The christian_canonical_reading (this file) shares the underlying kernel — that marital status and dissolution grounds derive from a communally-specific authority structure rather than a uniform civil code — with hindu_codified_reading, muslim_shariat_reading, parsi_communal_reading, and secular_civil_reading. Each has its own epsilon, beneficiary/victim structure, and classification; the secular_civil_reading is the structural foil that would foreclose all four communal readings if constitutionally mandated as exclusive. Network edges here are influence/coexistence edges within the same personal-law pluralism system, not claims of shared epsilon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
