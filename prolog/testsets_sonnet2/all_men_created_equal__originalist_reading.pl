% ============================================================================
% CONSTRAINT STORY: all_men_created_equal__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_all_men_created_equal__originalist_reading, []).

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
 *   constraint_id: all_men_created_equal__originalist_reading
 *   human_readable: 'All Men Are Created Equal' — Originalist (Founders'-Intent-Bounded) Reading
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the originalist reading of the 'all men are
 *   created equal' kernel: the equality clause's scope is bounded by the
 *   18th-century social taxonomy the founders actually held, and founders'
 *   documented intent — not the text's universal phrasing — governs who is
 *   covered. Under this reading the clause never extended to enslaved people,
 *   Indigenous nations, or women, and its exclusion of propertyless men from
 *   full political equality is likewise original rather than a later
 *   betrayal. This is the reading formalized judicially in Dred Scott v.
 *   Sandford (1857), which held that the founding generation's use of 'men'
 *   did not contemplate Black Americans as members of the political community
 *   the Constitution addressed. The reading peaks in extraction and
 *   suppression intensity around 1857 and partially recedes after the
 *   Thirteenth and Fourteenth Amendments begin to formally contest its
 *   scope-fixing premise, though the interpretive method itself persists into
 *   later jurisprudence beyond this interval's end. Two sibling readings of
 *   the same kernel are treated as separate constraints: the
 *   universalist_reading (equality as a principle requiring iterative
 *   expansion regardless of founders' subjective intent) and the
 *   textualist_paradox_reading (the universal grammatical form of the clause
 *   is irreconcilable with its restricted historical application, a
 *   performative contradiction the text itself indicts). This story's ε is
 *   authored for the originalist reading's own account of the standing
 *   arrangement — high, because by this reading's own lights the
 *   scope-bounding is the correct and intended operation of the clause, not a
 *   distortion of it.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(all_men_created_equal__originalist_reading, 0.81).
domain_priors:suppression_score(all_men_created_equal__originalist_reading, 0.72).
domain_priors:theater_ratio(all_men_created_equal__originalist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(all_men_created_equal__originalist_reading, tangled_rope).
narrative_ontology:human_readable(all_men_created_equal__originalist_reading, "'All Men Are Created Equal' — Originalist (Founders'-Intent-Bounded) Reading").
narrative_ontology:topic_domain(all_men_created_equal__originalist_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(all_men_created_equal__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(all_men_created_equal__originalist_reading, '17f8cdc5-98a2-4f4f-ad10-70b6fa0ec805').
narrative_ontology:cs_kernel_codification('17f8cdc5-98a2-4f4f-ad10-70b6fa0ec805', fixed_text).
narrative_ontology:cs_authority_grounding('17f8cdc5-98a2-4f4f-ad10-70b6fa0ec805', lineage).
narrative_ontology:cs_interpretation_layer_present('17f8cdc5-98a2-4f4f-ad10-70b6fa0ec805').
narrative_ontology:cs_reading_relation('17f8cdc5-98a2-4f4f-ad10-70b6fa0ec805', all_men_created_equal__universalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('17f8cdc5-98a2-4f4f-ad10-70b6fa0ec805', all_men_created_equal__textualist_paradox_reading, influences).
narrative_ontology:cs_axiom('17f8cdc5-98a2-4f4f-ad10-70b6fa0ec805', foundational, founders_documented_intent_controls_clause_scope).
narrative_ontology:cs_axiom_status(founders_documented_intent_controls_clause_scope, holdable).
narrative_ontology:cs_axiom_grounding('17f8cdc5-98a2-4f4f-ad10-70b6fa0ec805', founders_documented_intent_controls_clause_scope, conventional).
narrative_ontology:cs_axiom('17f8cdc5-98a2-4f4f-ad10-70b6fa0ec805', foundational, eighteenth_century_social_taxonomy_is_the_correct_referent_class).
narrative_ontology:cs_axiom_status(eighteenth_century_social_taxonomy_is_the_correct_referent_class, overridden).
narrative_ontology:cs_axiom_grounding('17f8cdc5-98a2-4f4f-ad10-70b6fa0ec805', eighteenth_century_social_taxonomy_is_the_correct_referent_class, empirically_contingent).
narrative_ontology:cs_reference_frame('17f8cdc5-98a2-4f4f-ad10-70b6fa0ec805', founding_era_social_taxonomy).
narrative_ontology:cs_drift_state('17f8cdc5-98a2-4f4f-ad10-70b6fa0ec805', reconstruction_amendments_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('17f8cdc5-98a2-4f4f-ad10-70b6fa0ec805', '').
narrative_ontology:cs_kernel_id(all_men_created_equal__originalist_reading, all_men_created_equal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(all_men_created_equal__originalist_reading, propertied_white_male_founders_and_descendants).
narrative_ontology:constraint_beneficiary(all_men_created_equal__originalist_reading, antebellum_slaveholding_class).
narrative_ontology:constraint_beneficiary(all_men_created_equal__originalist_reading, originalist_judicial_and_legal_establishment).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, enslaved_africans_and_descendants).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, indigenous_nations).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, women_under_coverture).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, non_property_owning_laborers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted the founding language and, under this reading, are the sole intended referents of 'men' as a term of art bounded by contemporary social taxonomy (free, propertied, white, male heads of household). Their descendants inherit standing, property, and the presumption that constitutional equality was never meant to disturb the social order they occupied. They benefit from a reading that treats the founding text's scope as fixed at ratification.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, propertied_white_male_founders_and_descendants, beneficiary,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(all_men_created_equal__originalist_reading, propertied_white_male_founders_and_descendants, agenda_setter).

% Relies directly on the originalist reading to argue that the equality clause does not, and was never intended to, apply to enslaved persons — a reading later formalized in Dred Scott's holding that Black Americans, enslaved or free, were not included among 'the people' the Constitution's founders addressed. Their economic position depends on the equality language remaining scope-limited.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, antebellum_slaveholding_class, beneficiary,
    institutional, generational, arbitrage, national).

% Judges, scholars, and advocates who administer and defend the interpretive method itself — treating founders' subjective and documented intent as the controlling authority over the text's scope. They set the terms of legitimate constitutional argument going forward and can, in principle, revise the interpretive method, but their professional and institutional identity is substantially built on defending it as neutral and constraining rather than as itself a scope-narrowing choice.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, originalist_judicial_and_legal_establishment, agenda_setter,
    institutional, generational, mobile, national).

% Held to be outside the equality clause's intended referent class entirely under this reading; the taxonomy that excludes them is treated as evidence of original meaning rather than as the injustice the clause's universal language might otherwise indict. No exit existed under slavery; the reading's persistence after abolition continues to shape whose historical exclusion counts as constitutionally cognizable.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, enslaved_africans_and_descendants, payer,
    powerless, civilizational, trapped, national).

% Categorized outside the founding social taxonomy as members of separate, treaty-negotiating polities rather than as individuals whose equal standing the clause addresses at all. The originalist reading treats this exclusion as descriptively accurate to founders' intent, foreclosing arguments that the clause's universal phrasing should have applied regardless of the founders' actual beliefs about which peoples counted.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, indigenous_nations, payer,
    powerless, civilizational, trapped, national).

% Excluded from the referent class 'men' both linguistically and legally under coverture, which subsumed a married woman's civil identity into her husband's. The originalist reading treats this as confirming rather than complicating the clause's scope, since the founders' documented social categories did not contemplate women as independent equal citizens.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, women_under_coverture, payer,
    powerless, generational, trapped, national).

% Free white men without property faced narrower but real exclusion from full political equality (suffrage, officeholding) in many founding-era jurisdictions; the originalist reading's insistence on founders' contemporary social taxonomy also historicizes and thereby legitimizes this narrower economic exclusion as part of the original scope.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, non_property_owning_laborers, payer,
    powerless, biographical, constrained, national).

% Argue that equality's textual universality should control over founders' documented social limitations, and that the originalist reading launders historical exclusion into constitutional meaning. They are not the audience the originalist method treats as authoritative — their claims are evaluated against, not incorporated into, the founders'-intent standard.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, civil_rights_movements_and_reformers, excluded,
    organized, generational, constrained, national).

% Document what the founding generation's social taxonomy actually was, whether it was contested even at the time (abolitionist founders, early suffrage arguments), and whether 'founders' intent' is itself a coherent singular fact given documented internal disagreement among the founders.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(all_men_created_equal__originalist_reading, propertied_white_male_founders_and_descendants).
narrative_ontology:fixing_cost_class(all_men_created_equal__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, allegedly neutral method for fixing the scope of a famously abstract phrase, allowing legal and political actors to resolve equal-protection disputes by appeal to a determinate historical referent class rather than open-ended moral argument.
% TRANSFER_FUNCTION: Moves the burden of proving inclusion onto groups excluded from the founders' documented 18th-century social taxonomy, and moves the presumption of standing to those already inside it — effectively transferring constitutional protection, political power, and property security toward the founding elite and away from enslaved people, Indigenous nations, women, and propertyless men.
% ABSENT_VOICES: Enslaved people, Indigenous nations, and women had no voice in either drafting the equality language or in the founding generation's construction of the social taxonomy the originalist reading treats as authoritative; the reading's own evidentiary base is drawn almost entirely from documents produced by the class it benefits.
% DISAPPEARANCE_RATIONALE: If the originalist reading's authority collapsed overnight, an enormous body of case law premised on founders'-intent-bounded scope (Dred Scott's reasoning, coverture-era jurisprudence, property-qualified citizenship arguments) would lose its interpretive anchor, and disputes over who counts within constitutional equality would have to be resolved by textual or evolving-standards methods instead — reordering how exclusion claims are litigated and legitimated.
% FOUNDING_PROBLEM: The founding generation needed constitutional language that could declare a revolutionary principle of political equality against monarchy while remaining compatible with an existing economy and social order built on slavery, coverture, and property-qualified citizenship. Founders' intent as an interpretive anchor solves the problem of reconciling the text's universal language with that order by fixing scope to the taxonomy the founders actually held.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars and jurists attest the founding problem (fixing determinate constitutional meaning against judicial overreach) remains live and the method still solves it. Constitutional historians and civil rights legal scholars — outside the beneficiary class — attest that the 'problem' the reading actually solves is reconciling universal language with a hierarchical social order, and that this problem is not one the equality clause's text obligates later generations to keep solving; abolitionist-era dissenters within the founding generation itself (e.g., recorded objections to slavery's compatibility with the Declaration's language) provide contemporaneous corroboration that founders' intent was internally contested even at ratification.
narrative_ontology:disappearance_verdict(all_men_created_equal__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(all_men_created_equal__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(all_men_created_equal__originalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(all_men_created_equal__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(all_men_created_equal__originalist_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(all_men_created_equal__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(all_men_created_equal__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(all_men_created_equal__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.81) because, under this reading's own terms, the clause's protection is deliberately withheld from the excluded classes as a matter of original scope rather than incidental gap — this is a reading that owns its narrowness as fidelity rather than treating it as a defect. Suppression is high (0.72) because the reading's persistence depended on active legal and social enforcement of the excluded classes' non-membership (slave codes, coverture law, denial of suffrage) and, later, on continued judicial and scholarly defense of founders'-intent methodology against textualist and evolving-standards challengers. Accessibility collapse is comparatively low (0.35) because alternative readings of the same text were live and contested even within the founding generation itself (recorded abolitionist objections, Northern free-state jurisprudence diverging from Southern practice) — the originalist reading never achieved anything close to natural-law-style closure of alternatives; it required continuous political and judicial defense, reflected in the correspondingly high resistance score (0.78).
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (originalist jurists and the founding elite), this reading is coordination: a determinate, principled method for resolving what an abstract phrase means, protecting against judicial invention of rights the founders never intended. From the payer seats, the identical structure operates as enforced extraction: a historically contingent social hierarchy is laundered into constitutional meaning and then defended as if scope-fixing were itself neutral rather than a substantive choice to freeze exclusion in place. The engine's tangled_rope computation should reflect this: genuine coordination function (determinate interpretive method) coexists with asymmetric extraction (excluded classes bear the clause's narrowed scope while the founding class and its interpretive heirs retain standing and authority) under active enforcement (slave codes, coverture, suffrage restriction, and later methodological gatekeeping).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are the founding elite and their descendants, and — more sharply — the slaveholding class whose economic order depended on the excluded classes remaining outside the equality clause's referent. The originalist judicial establishment is a distinct beneficiary/agenda-setter seat: its professional standing rests on defending founders'-intent as the legitimate interpretive method, independent of the specific historical outcomes that method produced. Victims are the four excluded classes, each trapped or constrained with no meaningful exit from the taxonomy that defines them out of the clause's protection — enslaved people and Indigenous nations under conditions of literal captivity or forced treaty subordination, women under coverture's legal erasure of independent civil identity, and propertyless men under a narrower but real exclusion from full political equality that this reading also legitimates as original rather than incidental.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem this reading solves — reconciling revolutionary universal language with an existing hierarchical order — was live at ratification and remained so through 1857, corroborated by the reading's own judicial triumph in Dred Scott. But the founding problem's status becomes contested after 1868: the Fourteenth Amendment's ratification is itself a structural repudiation of founders'-intent-bounded scope as the exclusive controlling method for equality claims, even though originalist methodology as an interpretive tool persists in later doctrine untethered from this specific historical application. Treating this reading as tangled_rope rather than pure snare preserves the fact that founders'-intent interpretation does solve a genuine coordination problem (determinacy against unconstrained judicial discretion) — collapsing it into pure extraction would erase why the method retains adherents who are not merely defending the historical exclusions it produced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    singular_founders_intent_fiction,
    'Is ''founders'' intent'' a coherent singular fact, given that the founding generation included documented internal disagreement about slavery''s compatibility with the Declaration''s language (e.g., early drafts condemning the slave trade, subsequent removal under political pressure)?',
    'Comprehensive historical review of founding-era private correspondence, convention debate records, and contemporaneous abolitionist writing by founders themselves, assessing whether a single controlling intent can be attributed or whether the taxonomy was itself contested at ratification.',
    'If founders'' intent was internally contested rather than uniform, the originalist reading''s claim to recover a determinate original scope is undermined at its foundation — the method would be imposing a retroactively constructed uniformity on a genuinely divided founding generation, strengthening the case that this reading is extraction dressed as historical fidelity rather than a neutral discovery of settled original meaning.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(singular_founders_intent_fiction, empirical, 'Whether founders'' intent is a coherent singular historical fact or a retroactively constructed uniformity.').

omega_variable(
    originalism_as_neutral_method_vs_outcome_selection,
    'Does the originalist method''s persistence into later jurisprudence (beyond the specific 18th-century exclusions) reflect genuine methodological neutrality, or does the method survive specifically because it continues to produce outcomes favorable to institutionally powerful interpreters regardless of the historical case at hand?',
    'Comparative analysis of originalist reasoning''s outcomes across cases where it favors versus disfavors institutionally powerful parties, controlling for case selection effects.',
    'If the method''s application correlates with outcome-favorability to power rather than tracking historical evidence consistently, the tangled_rope classification''s coordination component is weaker than authored and the constraint sits closer to pure snare; if the method is applied with genuine evidentiary discipline regardless of outcome, the coordination component is stronger.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(originalism_as_neutral_method_vs_outcome_selection, conceptual, 'Whether originalism functions as neutral historical method or outcome-selecting cover story.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly does the disagreement between the three kernel readings live — is it a disagreement about what the founders believed (historical fact), about whether founders'' belief should control (interpretive theory), or about whether the text''s grammar overrides any intent-based reading at all (linguistic/textual theory)?',
    'Structural comparison of the three readings'' foundational axioms: this reading''s axiom rests on intent-controls-scope; the universalist reading''s axiom rests on principle-transcends-intent; the textualist_paradox reading''s axiom rests on text-and-application-are-irreconcilable-as-such. Documenting which axiom each party actually contests when they invoke ''the founders'' meant equality differently'' clarifies whether the dispute is empirical, interpretive, or logical.',
    'If the disagreement is purely interpretive-theoretical (not resolvable by more historical evidence), the three readings genuinely coexist as competing normative frameworks rather than one being empirically correctable by the others — supporting coexists_with relations rather than foreclosure between any pair.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Whether the kernel readings disagree on historical fact, interpretive theory, or textual logic.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__originalist_reading, 1776, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(all__tr_t1776, all_men_created_equal__originalist_reading, theater_ratio, 1776, 0.3).
narrative_ontology:measurement(all__tr_t1800, all_men_created_equal__originalist_reading, theater_ratio, 1800, 0.33).
narrative_ontology:measurement(all__tr_t1820, all_men_created_equal__originalist_reading, theater_ratio, 1820, 0.36).
narrative_ontology:measurement(all__tr_t1840, all_men_created_equal__originalist_reading, theater_ratio, 1840, 0.38).
narrative_ontology:measurement(all__tr_t1857, all_men_created_equal__originalist_reading, theater_ratio, 1857, 0.42).
narrative_ontology:measurement(all__tr_t1868, all_men_created_equal__originalist_reading, theater_ratio, 1868, 0.4).

% Extraction over time
narrative_ontology:measurement(all__be_t1776, all_men_created_equal__originalist_reading, base_extractiveness, 1776, 0.7).
narrative_ontology:measurement(all__be_t1800, all_men_created_equal__originalist_reading, base_extractiveness, 1800, 0.74).
narrative_ontology:measurement(all__be_t1820, all_men_created_equal__originalist_reading, base_extractiveness, 1820, 0.78).
narrative_ontology:measurement(all__be_t1840, all_men_created_equal__originalist_reading, base_extractiveness, 1840, 0.8).
narrative_ontology:measurement(all__be_t1857, all_men_created_equal__originalist_reading, base_extractiveness, 1857, 0.85).
narrative_ontology:measurement(all__be_t1868, all_men_created_equal__originalist_reading, base_extractiveness, 1868, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(all__su_t1776, all_men_created_equal__originalist_reading, suppression_requirement, 1776, 0.6).
narrative_ontology:measurement(all__su_t1800, all_men_created_equal__originalist_reading, suppression_requirement, 1800, 0.65).
narrative_ontology:measurement(all__su_t1820, all_men_created_equal__originalist_reading, suppression_requirement, 1820, 0.7).
narrative_ontology:measurement(all__su_t1840, all_men_created_equal__originalist_reading, suppression_requirement, 1840, 0.74).
narrative_ontology:measurement(all__su_t1857, all_men_created_equal__originalist_reading, suppression_requirement, 1857, 0.79).
narrative_ontology:measurement(all__su_t1868, all_men_created_equal__originalist_reading, suppression_requirement, 1868, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(all_men_created_equal__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(all_men_created_equal__originalist_reading, universalist_reading).
narrative_ontology:affects_constraint(all_men_created_equal__originalist_reading, textualist_paradox_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the all_men_created_equal kernel. This constraint (originalist_reading) authors high extractiveness and a narrow victim set fixed at 18th-century social taxonomy; universalist_reading authors the same text as a principle whose scope was always meant to expand and treats founders' intent as non-controlling; textualist_paradox_reading authors the text's universal grammar as structurally irreconcilable with any bounded application, originalist or otherwise, treating the contradiction itself as the constraint's defining feature. The three share ratification-era origin but diverge sharply in ε, beneficiary/victim structure, and classified type; they are linked via affects_constraints rather than merged into one story, per the ε-invariance decomposition principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
