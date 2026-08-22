% ============================================================================
% CONSTRAINT STORY: equality_clause_scope__restrictive_originalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equality_clause_scope__restrictive_originalist, []).

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
 *   constraint_id: equality_clause_scope__restrictive_originalist
 *   human_readable: Equality Clause Scope — Restrictive Originalist Reading
 *   domain: constitutional_law/political_philosophy/civil_rights_history
 *
 * SUMMARY:
 *   This story authors the restrictive originalist reading of the
 *   equality-clause kernel: the claim that the founding-era equality
 *   principle applies, in its own historical terms, only to propertied white
 *   males as political actors within the 18th-century social contract
 *   framework, and that any extension of equality's coverage to persons
 *   outside that class requires a separate constitutional act (amendment),
 *   not judicial reinterpretation of the existing text. This is NOT a claim
 *   that equality is a good or bad idea in the abstract — it is a claim about
 *   the scope the text originally bore and the legitimate mechanism for
 *   changing that scope. The ε authored here (0.68, rising toward 1857 with
 *   Dred Scott, then falling sharply through Reconstruction) describes the
 *   standing arrangement THIS READING is about: an equality clause whose
 *   coverage, on originalist terms, excludes enslaved and free Black persons,
 *   women, and (initially) propertyless men, while formally equalizing the
 *   propertied white male political class. Sibling readings
 *   (expansive_universalist, progressive_textualist) are separate constraint
 *   stories with their own ε and beneficiary/victim structure — this file
 *   does not average over them or hedge between them.
 *
 * KEY AGENTS:
 *   - propertied_white_male_citizens: beneficiary class whose formal political equality the clause secures (powerful/arbitrage)
 *   - founding_era_political_establishment: agenda_setter administering scope (institutional/arbitrage)
 *   - enslaved_black_persons: paradigm excluded class, treated as property not political actors (powerless/trapped)
 *   - women_of_all_classes: excluded from franchise and clause coverage under coverture (powerless/trapped)
 *   - abolitionist_and_suffragist_reformers: excluded advocates redirected into amendment campaigns (organized/constrained)
 *   - constitutional_historians: analytical observer reconstructing original understanding (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__restrictive_originalist, 0.68).
domain_priors:suppression_score(equality_clause_scope__restrictive_originalist, 0.79).
domain_priors:theater_ratio(equality_clause_scope__restrictive_originalist, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, extractiveness, 0.68).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__restrictive_originalist, tangled_rope).
narrative_ontology:human_readable(equality_clause_scope__restrictive_originalist, "Equality Clause Scope — Restrictive Originalist Reading").
narrative_ontology:topic_domain(equality_clause_scope__restrictive_originalist, "constitutional_law/political_philosophy/civil_rights_history").

domain_priors:requires_active_enforcement(equality_clause_scope__restrictive_originalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__restrictive_originalist, '9c603c41-286c-4ed5-8187-1884bc9f7585').
narrative_ontology:cs_kernel_codification('9c603c41-286c-4ed5-8187-1884bc9f7585', fixed_text).
narrative_ontology:cs_authority_grounding('9c603c41-286c-4ed5-8187-1884bc9f7585', lineage).
narrative_ontology:cs_interpretation_layer_present('9c603c41-286c-4ed5-8187-1884bc9f7585').
narrative_ontology:cs_reading_relation('9c603c41-286c-4ed5-8187-1884bc9f7585', equality_clause_scope__expansive_universalist, forecloses).
narrative_ontology:cs_reading_relation('9c603c41-286c-4ed5-8187-1884bc9f7585', equality_clause_scope__progressive_textualist, coexists_with).
narrative_ontology:cs_axiom('9c603c41-286c-4ed5-8187-1884bc9f7585', foundational, equality_principle_bounded_by_original_political_actor_class).
narrative_ontology:cs_axiom_status(equality_principle_bounded_by_original_political_actor_class, holdable).
narrative_ontology:cs_axiom_grounding('9c603c41-286c-4ed5-8187-1884bc9f7585', equality_principle_bounded_by_original_political_actor_class, conventional).
narrative_ontology:cs_axiom('9c603c41-286c-4ed5-8187-1884bc9f7585', foundational, scope_expansion_requires_amendment_not_reinterpretation).
narrative_ontology:cs_axiom_status(scope_expansion_requires_amendment_not_reinterpretation, holdable).
narrative_ontology:cs_axiom_grounding('9c603c41-286c-4ed5-8187-1884bc9f7585', scope_expansion_requires_amendment_not_reinterpretation, conventional).
narrative_ontology:cs_reference_frame('9c603c41-286c-4ed5-8187-1884bc9f7585', founding_era_ratified_public_meaning).
narrative_ontology:cs_drift_state('9c603c41-286c-4ed5-8187-1884bc9f7585', post_reconstruction_amendment_era, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('9c603c41-286c-4ed5-8187-1884bc9f7585', '').
narrative_ontology:cs_kernel_id(equality_clause_scope__restrictive_originalist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__restrictive_originalist, propertied_white_male_citizens).
narrative_ontology:constraint_beneficiary(equality_clause_scope__restrictive_originalist, founding_era_political_establishment).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, enslaved_black_persons).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, free_black_persons).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, women_of_all_classes).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, propertyless_white_men).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, indigenous_peoples).
narrative_ontology:constraint_vindicates(equality_clause_scope__restrictive_originalist, social_contract_theory_as_originally_bounded).
narrative_ontology:constraint_vindicates(equality_clause_scope__restrictive_originalist, constitutional_supremacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the franchise, sit on juries, hold office, and own the property whose protection the equality clause (on this reading) was drafted to secure among political equals. They draft, ratify, and interpret the founding text and benefit from the fact that 'equality' is read to apply to the class they already occupy.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, propertied_white_male_citizens, beneficiary,
    powerful, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(equality_clause_scope__restrictive_originalist, propertied_white_male_citizens, agenda_setter).

% Legislatures, courts, and constitutional conventions dominated by the propertied class administer the equality clause's scope, treating any extension beyond the original political-actor class as requiring a new constitutional act (amendment) rather than a reinterpretation of the existing text.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, founding_era_political_establishment, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Held as property under the same constitutional order that proclaims equality among political actors; the clause's restrictive reading is used to justify their categorical exclusion from personhood-in-law, foreclosing any claim to the equality principle without a separate constitutional amendment (which does not yet exist on this reading's timeline).
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, enslaved_black_persons, payer,
    powerless, immediate, trapped, national).

% Legally free but denied the franchise, denied standing to sue in many jurisdictions, and denied the equality clause's protection on the grounds that the clause's original political-actor scope never contemplated their inclusion. Exit requires either emigration or an amendment they have no formal power to initiate.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, free_black_persons, payer,
    powerless, biographical, trapped, national).

% Excluded from the franchise, from property ownership in marriage in most jurisdictions, and from the equality clause's coverage under the restrictive reading, which treats coverture and domestic dependency as outside the 'political actor' category the clause addresses. Their claims for inclusion are treated as requiring new constitutional text, not judicial recognition of an existing principle.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, women_of_all_classes, payer,
    powerless, generational, trapped, national).

% Share racial and gender status with the beneficiary class but lack the property qualification that originally defined the political-actor category; disenfranchised in many founding-era jurisdictions and only gradually absorbed into the beneficiary class through state-level suffrage reforms, not through the equality clause itself on this reading.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, propertyless_white_men, payer,
    moderate, biographical, constrained, national).

% Treated as external to the political compact entirely under the restrictive reading — neither citizens nor covered political actors, but members of separate nations the constitutional order negotiates with or displaces. Their claims are not merely denied but structurally outside the categories the equality clause was drafted to address at all.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, indigenous_peoples, excluded,
    powerless, civilizational, trapped, national).

% Argue from outside the ratifying class that the equality principle's own logic extends beyond its original beneficiaries. On the restrictive reading their arguments are treated as calls for new law (amendment), not correct readings of existing law, so their advocacy is channeled into decades-long amendment campaigns rather than litigation.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, abolitionist_and_suffragist_reformers, excluded,
    organized, generational, constrained, national).

% Study the drafting record, ratification debates, and contemporaneous practice to determine what the founding generation understood 'equality' to mean and to whom it was understood to apply, without a stake in the outcome of the scope dispute.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, constitutional_historians, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable, legible political class (propertied white males) among whom formal legal and political equality is genuinely secured — no internal caste distinctions of rank or hereditary privilege among that class — solving the coordination problem of replacing monarchical/aristocratic hierarchy with horizontal equality among the designated political actors.
% TRANSFER_FUNCTION: Moves political power, legal standing, and property security to the propertied white male class and consolidates it there by treating the class boundary itself as constitutionally settled; the cost of that consolidation is borne by everyone outside the boundary, whose claims to the same equality principle are redirected into the amendment process rather than recognized as already covered.
% ABSENT_VOICES: Enslaved persons, women, and propertyless men had no formal voice in drafting or ratification; abolitionist and early women's rights advocates existed but were structurally excluded from the ratifying conventions and are treated, on this reading, as petitioners for new law rather than parties whose exclusion the existing text should be read to correct.
% DISAPPEARANCE_RATIONALE: If the restrictive originalist reading disappeared as the controlling interpretation, courts would either extend equality protections to previously excluded groups by reinterpretation (collapsing into the expansive reading) or the amendment-only pathway would lose its gatekeeping force — either way the entire architecture of who must seek constitutional amendment versus who can claim existing protection would reorganize, as it historically did across the 13th, 14th, 15th, and 19th Amendments.
% FOUNDING_PROBLEM: The founding generation needed to replace hereditary monarchical and aristocratic authority with a political order in which the class of political actors — men who owned property and thus had a stake in the polity, by the political theory of the time — held formal equality with one another, ending titles of nobility and formal caste distinction within that class.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians outside the beneficiary class (drawing on ratification debates, contemporaneous correspondence, and comparative constitutional scholarship) attest that the property-and-race-bounded political class the clause was built to equalize no longer exists as a constitutional category — universal adult suffrage and the Reconstruction and women's suffrage amendments substantively resolved the founding problem the restrictive reading describes; the restrictive reading persists today primarily as an interpretive method for resolving unrelated modern disputes (e.g., federalism, textualist statutory construction), not because the original bounded political class survives as a live limitation.
narrative_ontology:disappearance_verdict(equality_clause_scope__restrictive_originalist, world_rearranges).
narrative_ontology:founding_problem_status(equality_clause_scope__restrictive_originalist, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__restrictive_originalist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equality_clause_scope__restrictive_originalist, 'none', 1).
narrative_ontology:epsilon_provenance(equality_clause_scope__restrictive_originalist, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_clause_scope__restrictive_originalist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equality_clause_scope__restrictive_originalist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equality_clause_scope__restrictive_originalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high and rising through the antebellum period (peaking near 1857, the year of Dred Scott, which is the fullest judicial articulation of the restrictive reading's exclusionary logic) because the clause's benefit is concentrated in an increasingly entrenched propertied white male political class while its costs are borne by an expanding population subject to slavery, coverture, and disenfranchisement. Suppression tracks the same arc: enforcement of the racial and gender boundary hardens through fugitive slave law, Black codes, and formal exclusion from suffrage, then collapses sharply after 1865 as the Thirteenth Amendment and Reconstruction dismantle the enforcement machinery this reading depends on. Theater ratio rises through the antebellum period as constitutional rhetoric about 'equality' increasingly diverges from a franchise and property regime that, by the 1850s, is defended more by performative appeals to founding intent than by functioning coordination logic — the coordination function (equality among the political class) remains real but is increasingly cited to justify the exclusionary boundary rather than to describe genuine horizontal equality-in-practice.
 *
 * DIRECTIONALITY LOGIC:
 *   Propertied white male citizens sit at the beneficiary end: the clause, on this reading, was drafted for them and its restrictive scope is precisely what secures their equality without extending obligations or competition from excluded classes. Enslaved persons sit at the extreme target end — trapped, powerless, and categorically defined as outside the class the clause addresses. Free Black persons, women, and propertyless men occupy intermediate but still strongly target-leaning positions: nominally free or nominally male-and-white in the case of the last group, but denied the clause's coverage by the same originalist logic. Indigenous peoples are structurally distinct from 'victims within scope' — the restrictive reading places them entirely outside the compact rather than inside it and denied, which is why they are marked excluded rather than payer.
 *
 * MANDATROPHY ANALYSIS:
 *   The restrictive originalist reading resists mandatrophy misclassification by insisting that the clause's ORIGINAL coordination function (equality among the political class, ending hereditary privilege within that class) is real and was fully achieved — the founding problem is genuinely dead, per the six_questions corroboration. What must not happen is treating the clause's persistence as still solving that dead problem for the classes it never covered: the amendment process, not reinterpretation, is this reading's mechanism for extending coverage, and the amendments (13th, 14th, 15th, 19th) are exactly the evidence that the restrictive reading's own logic predicts — new constitutional acts were required because the original clause did not, on originalist terms, already cover these classes. The high suppression and extraction scores prior to 1865 are not a claim that the founding coordination function was fraudulent; they are a claim that its benefit was narrowly and durably bounded, and that the boundary was actively enforced against those outside it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_meaning_vs_original_expected_application,
    'Does ''the original meaning of equality'' refer to the principle the framers articulated in the abstract, or to the concrete class of persons they expected and intended it to cover?',
    'Close textual and historical analysis distinguishing original public meaning (semantic content) from original expected application (framers'' anticipated scope) — a live methodological split within originalism itself. Ratification-era dictionaries, convention debates, and contemporaneous judicial opinions bear on this but do not resolve it, since originalists themselves disagree on which the doctrine privileges.',
    'If original PUBLIC MEANING is the correct referent and that meaning is genuinely universalist in its terms (even if unevenly applied), this reading collapses toward progressive_textualist or even expansive_universalist. If original EXPECTED APPLICATION is the correct referent, this reading''s narrow beneficiary set is the correct constitutional answer and stands independent of later moral reassessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_meaning_vs_original_expected_application, conceptual, 'Whether originalism''s proper object is semantic meaning or expected scope of application.').

omega_variable(
    amendment_as_correction_vs_amendment_as_extension,
    'Do the Reconstruction and suffrage amendments CORRECT a prior misapplication of an always-latent universal principle, or EXTEND a genuinely bounded original principle to new classes for the first time?',
    'Examine amendment-era legislative history and floor debates for whether proponents argued ''the Constitution already promised this'' (correction framing) or ''we are now choosing to extend this'' (extension framing); both framings appear in the historical record and are not reconcilable by evidence alone.',
    'A correction framing undermines the restrictive reading''s premise that the original scope was genuinely narrow (suggesting instead that judicial and political actors chose narrow enforcement of an available broader text). An extension framing supports this reading directly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(amendment_as_correction_vs_amendment_as_extension, conceptual, 'Whether constitutional amendments corrected misapplication or extended genuinely new scope.').

omega_variable(
    beneficiary_class_naturalness,
    'Is the propertied-white-male political-actor class a natural or logically necessary category for 18th-century social contract theory, or a constructed boundary that contingently served the material interests of those who drew it?',
    'Comparative analysis of contemporaneous social contract theorists (Locke, Rousseau, colonial pamphleteers) who articulated broader or narrower political-actor classes than the one actually adopted, and examination of drafting-convention debates over property and race qualifications for evidence of contested alternatives that were rejected.',
    'If the boundary was one live option among several considered and rejected alternatives were more inclusive, the restrictive reading''s naturalizing frame (this is simply what the theory required) is weakened and the constraint looks more like constructed extraction than natural theoretical limit — relevant because propertied_white_male_citizens are declared beneficiaries of what this reading otherwise treats as principled construction, not simple natural fact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_class_naturalness, conceptual, 'Whether the beneficiary class boundary was a natural implication of the theory or a contingent, contested construction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__restrictive_originalist, 1787, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1787, equality_clause_scope__restrictive_originalist, theater_ratio, 1787, 0.3).
narrative_ontology:measurement(equa_tr_t1800, equality_clause_scope__restrictive_originalist, theater_ratio, 1800, 0.32).
narrative_ontology:measurement(equa_tr_t1820, equality_clause_scope__restrictive_originalist, theater_ratio, 1820, 0.36).
narrative_ontology:measurement(equa_tr_t1840, equality_clause_scope__restrictive_originalist, theater_ratio, 1840, 0.4).
narrative_ontology:measurement(equa_tr_t1857, equality_clause_scope__restrictive_originalist, theater_ratio, 1857, 0.5).
narrative_ontology:measurement(equa_tr_t1865, equality_clause_scope__restrictive_originalist, theater_ratio, 1865, 0.46).
narrative_ontology:measurement(equa_tr_t1868, equality_clause_scope__restrictive_originalist, theater_ratio, 1868, 0.42).

% Extraction over time
narrative_ontology:measurement(equa_be_t1787, equality_clause_scope__restrictive_originalist, base_extractiveness, 1787, 0.7).
narrative_ontology:measurement(equa_be_t1800, equality_clause_scope__restrictive_originalist, base_extractiveness, 1800, 0.71).
narrative_ontology:measurement(equa_be_t1820, equality_clause_scope__restrictive_originalist, base_extractiveness, 1820, 0.73).
narrative_ontology:measurement(equa_be_t1840, equality_clause_scope__restrictive_originalist, base_extractiveness, 1840, 0.72).
narrative_ontology:measurement(equa_be_t1857, equality_clause_scope__restrictive_originalist, base_extractiveness, 1857, 0.76).
narrative_ontology:measurement(equa_be_t1865, equality_clause_scope__restrictive_originalist, base_extractiveness, 1865, 0.55).
narrative_ontology:measurement(equa_be_t1868, equality_clause_scope__restrictive_originalist, base_extractiveness, 1868, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1787, equality_clause_scope__restrictive_originalist, suppression_requirement, 1787, 0.68).
narrative_ontology:measurement(equa_su_t1800, equality_clause_scope__restrictive_originalist, suppression_requirement, 1800, 0.7).
narrative_ontology:measurement(equa_su_t1820, equality_clause_scope__restrictive_originalist, suppression_requirement, 1820, 0.74).
narrative_ontology:measurement(equa_su_t1840, equality_clause_scope__restrictive_originalist, suppression_requirement, 1840, 0.78).
narrative_ontology:measurement(equa_su_t1857, equality_clause_scope__restrictive_originalist, suppression_requirement, 1857, 0.85).
narrative_ontology:measurement(equa_su_t1865, equality_clause_scope__restrictive_originalist, suppression_requirement, 1865, 0.6).
narrative_ontology:measurement(equa_su_t1868, equality_clause_scope__restrictive_originalist, suppression_requirement, 1868, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_clause_scope__restrictive_originalist, enforcement_mechanism).
narrative_ontology:affects_constraint(equality_clause_scope__restrictive_originalist, equality_clause_scope__expansive_universalist).
narrative_ontology:affects_constraint(equality_clause_scope__restrictive_originalist, equality_clause_scope__progressive_textualist).

% DUAL FORMULATION NOTE:
% This story, equality_clause_scope__expansive_universalist, and equality_clause_scope__progressive_textualist form a three-member constraint family decomposing the natural-language label 'the equality clause' into structurally distinct claims about scope and mechanism of change, per the ε-invariance principle. This (restrictive_originalist) reading authors ε=0.68 for a clause whose original coverage is narrow and whose expansion requires amendment; the expansive_universalist reading would author near-zero ε for the same standing arrangement, judged by its own lights, as a moral betrayal of an always-universal text; the progressive_textualist reading shares this reading's amendment-not-reinterpretation mechanism but differs on whether the underlying text was ever narrow. Each carries its own beneficiary/victim structure and is not to be averaged with the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
