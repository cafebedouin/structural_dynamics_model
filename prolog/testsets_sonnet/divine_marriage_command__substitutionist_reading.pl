% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__substitutionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command__substitutionist_reading, []).

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
 *   constraint_id: divine_marriage_command__substitutionist_reading
 *   human_readable: Substitutionist Reading: Manifesto as New Revelation Superseding Plural Marriage Command
 *   domain: religious_authority/political_theology/commitment_systems
 *
 * SUMMARY:
 *   This story instantiates the substitutionist reading of the divine
 *   marriage command kernel: the 1890 Manifesto is treated as new revelation
 *   that doctrinally supersedes the prior plural-marriage command, making
 *   monogamy the operative rule and continued polygamy apostasy. This reading
 *   is generated as a clean, ε-invariant constraint distinct from its
 *   siblings (the continuationist reading, where the command remains valid
 *   and the Manifesto is a prudential suspension under duress; and the
 *   coercion-visibility reading, where theological legitimacy is explicitly
 *   grounded in acknowledged institutional survival). The three readings are
 *   not the same constraint measured differently — each has a distinct ε,
 *   distinct beneficiary/victim structure, and distinct type, because each
 *   reading licenses a different set of downstream institutional actions
 *   (excommunication basis, historical narrative, legitimacy claims). They
 *   are linked via network.affects_constraints and are siblings within one
 *   contested kernel, not three observables of one fact.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__substitutionist_reading, 0.58).
domain_priors:suppression_score(divine_marriage_command__substitutionist_reading, 0.62).
domain_priors:theater_ratio(divine_marriage_command__substitutionist_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__substitutionist_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__substitutionist_reading, "Substitutionist Reading: Manifesto as New Revelation Superseding Plural Marriage Command").
narrative_ontology:topic_domain(divine_marriage_command__substitutionist_reading, "religious_authority/political_theology/commitment_systems").

domain_priors:requires_active_enforcement(divine_marriage_command__substitutionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__substitutionist_reading, '25bbadf1-beac-4498-8d71-41350e2dfd24').
narrative_ontology:cs_kernel_codification('25bbadf1-beac-4498-8d71-41350e2dfd24', formalized).
narrative_ontology:cs_authority_grounding('25bbadf1-beac-4498-8d71-41350e2dfd24', lineage).
narrative_ontology:cs_interpretation_layer_present('25bbadf1-beac-4498-8d71-41350e2dfd24').
narrative_ontology:cs_reading_relation('25bbadf1-beac-4498-8d71-41350e2dfd24', divine_marriage_command__continuationist_reading, forecloses).
narrative_ontology:cs_reading_relation('25bbadf1-beac-4498-8d71-41350e2dfd24', divine_marriage_command__coercion_visibility_reading, influences).
narrative_ontology:cs_axiom('25bbadf1-beac-4498-8d71-41350e2dfd24', foundational, manifesto_constitutes_new_binding_revelation).
narrative_ontology:cs_axiom_status(manifesto_constitutes_new_binding_revelation, holdable).
narrative_ontology:cs_axiom_grounding('25bbadf1-beac-4498-8d71-41350e2dfd24', manifesto_constitutes_new_binding_revelation, theological).
narrative_ontology:cs_axiom('25bbadf1-beac-4498-8d71-41350e2dfd24', foundational, prior_command_doctrinally_rescinded_not_merely_suspended).
narrative_ontology:cs_axiom_status(prior_command_doctrinally_rescinded_not_merely_suspended, holdable).
narrative_ontology:cs_axiom_grounding('25bbadf1-beac-4498-8d71-41350e2dfd24', prior_command_doctrinally_rescinded_not_merely_suspended, theological).
narrative_ontology:cs_axiom('25bbadf1-beac-4498-8d71-41350e2dfd24', secondary, post_manifesto_plural_marriage_constitutes_apostasy).
narrative_ontology:cs_axiom_status(post_manifesto_plural_marriage_constitutes_apostasy, holdable).
narrative_ontology:cs_axiom_grounding('25bbadf1-beac-4498-8d71-41350e2dfd24', post_manifesto_plural_marriage_constitutes_apostasy, conventional).
narrative_ontology:cs_reference_frame('25bbadf1-beac-4498-8d71-41350e2dfd24', continuing_revelation_prophetic_succession).
narrative_ontology:cs_drift_state('25bbadf1-beac-4498-8d71-41350e2dfd24', post_manifesto_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('25bbadf1-beac-4498-8d71-41350e2dfd24', '').
narrative_ontology:cs_kernel_id(divine_marriage_command__substitutionist_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__substitutionist_reading, church_hierarchy).
narrative_ontology:constraint_beneficiary(divine_marriage_command__substitutionist_reading, monogamous_membership_in_good_standing).
narrative_ontology:constraint_beneficiary(divine_marriage_command__substitutionist_reading, church_public_legitimacy_apparatus).
narrative_ontology:constraint_victim(divine_marriage_command__substitutionist_reading, fundamentalist_polygamist_descendants).
narrative_ontology:constraint_victim(divine_marriage_command__substitutionist_reading, excommunicated_plural_families).
narrative_ontology:constraint_victim(divine_marriage_command__substitutionist_reading, women_in_dissolved_plural_marriages).
narrative_ontology:constraint_vindicates(divine_marriage_command__substitutionist_reading, continuing_revelation_doctrine).
narrative_ontology:constraint_vindicates(divine_marriage_command__substitutionist_reading, prophetic_succession_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issued and now administers the Manifesto as the operative doctrinal text, declares it revelation rather than concession, and enforces monogamy as a condition of membership and priesthood standing. Controls temple access, excommunication proceedings, and the historical narrative taught to members. Its institutional survival and legal standing (statehood, property, tax status) depended on the shift and continues to depend on the substitutionist framing holding.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, church_hierarchy, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Members who married and organized family life under the post-Manifesto rule receive full institutional participation, temple privileges, and social standing without the legal or reputational exposure that plural marriage now carries. They can exit the faith or its practices without doctrinal penalty attaching to their marital form.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, monogamous_membership_in_good_standing, beneficiary,
    moderate, generational, mobile, national).

% The public relations, legal, and historical-education functions of the institution that maintain the account of the Manifesto as revelation. They benefit from the substitutionist framing because it converts a survival-driven concession into a doctrinal advance, insulating the institution from a coercion narrative that would undercut its claim to continuing prophetic authority.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, church_public_legitimacy_apparatus, beneficiary,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(divine_marriage_command__substitutionist_reading, church_public_legitimacy_apparatus, agenda_setter).

% Descendants of families that continued plural marriage after 1890, now classified as apostate splinter groups. They inherited a religious practice their own tradition holds to be commanded and unrescinded, and bear excommunication, social isolation, and criminalization for continuing what the parent institution once required. Their identity is fused to a practice the substitutionist reading now defines as heretical.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, fundamentalist_polygamist_descendants, payer,
    powerless, generational, identity_locked, regional).

% Families formally severed from the institution for maintaining plural marriages after the doctrinal cutoff. They lose community, temple sealing status for existing marriages, and standing within the only religious framework that gave their family structure meaning; the substitutionist reading treats their continued practice as grounds for removal rather than as adherence to a still-valid command.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, excommunicated_plural_families, payer,
    powerless, biographical, trapped, regional).

% Women whose marriages were dissolved, demoted to unofficial, or rendered illegitimate by the post-Manifesto legal and doctrinal shift, often without inheritance, custody, or social protection under the new monogamous norm. They bear the cost of a rule change enacted for institutional reasons in which they had no voice.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, women_in_dissolved_plural_marriages, payer,
    powerless, biographical, trapped, regional).

% The historical coercive party (anti-polygamy legislation, property seizure, disenfranchisement) whose pressure produced the Manifesto is structurally absent from the substitutionist account, which recasts an externally forced change as internally generated revelation. Its role would, if foregrounded, dissolve the doctrinal-supersession framing entirely.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, federal_government_historical, excluded,
    institutional, generational, analytical, national).

% Study the documentary record of the Manifesto's drafting, the timing relative to federal legal pressure, and subsequent institutional statements, producing accounts that variously support the revelation, prudential-suspension, or coercion-response readings depending on which sources and framing are weighted.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, religious_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_marriage_command__substitutionist_reading, church_hierarchy).
narrative_ontology:fixing_cost_class(divine_marriage_command__substitutionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, stable, and legally compliant marital norm for the entire membership, ending an internally costly and legally embattled practice, and allowing coordinated institutional survival, temple operations, and statehood negotiation.
% TRANSFER_FUNCTION: Moves legitimacy and continuity from the plural-marriage-practicing membership to the monogamous membership and central hierarchy; moves social, familial, and legal costs from the institution onto the fundamentalist splinter communities and the women whose marriages were dissolved or delegitimized.
% ABSENT_VOICES: The federal government's coercive role is structurally omitted from the substitutionist account, as are the fundamentalist communities who hold the prior command still binding; both would contest that this was revelation rather than survival-driven doctrinal reversal.
% DISAPPEARANCE_RATIONALE: If the substitutionist framing were withdrawn and replaced with an acknowledged-coercion or continuationist account, the institution's claim to unbroken prophetic authority would be directly challenged, its historical narrative would require rewriting, and the excommunication basis for fundamentalist groups would lose theological grounding — reopening the question of which communities are in good standing.
% FOUNDING_PROBLEM: The plural marriage command was, in the fundamentalist and continuationist accounts, instituted to fulfill a specific prophetic mandate; the substitutionist reading reframes the founding problem as: how does the institution maintain unbroken claim to continuing revelation while abandoning a previously commanded practice under existential legal pressure?
% FOUNDING_PROBLEM_CORROBORATION: The church hierarchy attests the Manifesto is genuine revelation and the founding problem (need for updated commanded practice) is resolved. Independent historians, drawing on correspondence and the timing of the 1890 declaration relative to the Edmunds-Tucker Act and looming disincorporation, and the fundamentalist communities themselves, corroborate a different account: that the founding problem was institutional survival under coercion, not doctrinal completion, and that this status remains actively contested outside the benefiting hierarchy.
narrative_ontology:disappearance_verdict(divine_marriage_command__substitutionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_marriage_command__substitutionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__substitutionist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(divine_marriage_command__substitutionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_marriage_command__substitutionist_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__substitutionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_marriage_command__substitutionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_marriage_command__substitutionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58 at 2020) reflects the ongoing cost this reading imposes on fundamentalist descendant communities and dissolved plural families, who bear excommunication and delegitimization as the price of the institution's doctrinal consistency claim. Suppression is high at founding (0.85, reflecting active federal and ecclesiastical coercion in the immediate post-Manifesto period) and gradually declines as fundamentalist splinter communities separate rather than fight for reincorporation, settling near 0.62 as a stable baseline enforcement level (excommunication proceedings, temple exclusion) rather than acute coercion. Theater ratio starts elevated (0.55) reflecting heavy performative emphasis on 'revelation' framing during the vulnerable founding period when the coercion story was most visible and needed most active reframing, then settles as the substitutionist account becomes institutionally sedimented and requires less active theatrical defense, before ticking up slightly in the contemporary period (0.44) as historical scholarship renews scrutiny of the founding narrative.
 *
 * DIRECTIONALITY LOGIC:
 *   The church hierarchy and its public legitimacy apparatus sit at the beneficiary end: they administer the reading, collect the institutional continuity and legal legitimacy it provides, and control the narrative. Monogamous membership benefits from full participation without legal or social exposure. Fundamentalist descendants, excommunicated plural families, and women in dissolved marriages sit at the target end — they had no voice in the reframing and bear its costs (exclusion, delegitimization, loss of protections) as an ongoing structural feature, not a one-time historical event, since the excommunication and non-recognition machinery is still active.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading requires the tangled_rope classification rather than snare because it possesses both a real coordination function (a single stable marital norm enables legal survival, temple administration, and institutional continuity that pure extraction accounts would miss) and a genuine, ongoing extraction (fundamentalist communities and dissolved-marriage women pay through the same doctrinal structure that stabilizes the institution). Reading this purely as extraction would erase the real coordination benefit conferred on the monogamous membership; reading it purely as coordination would erase the asymmetric cost borne by those excommunicated for continuing what their own tradition holds to be a still-binding command.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_vs_survival_framing,
    'Is the 1890 Manifesto best modeled as genuine new revelation (this reading), a prudential suspension under duress with no doctrinal rescission (continuationist_reading), or an acknowledged coercion-driven survival measure (coercion_visibility_reading)?',
    'Comparative documentary analysis of the Manifesto''s drafting correspondence, its timing relative to the Edmunds-Tucker Act and pending disincorporation proceedings, and subsequent institutional statements about its doctrinal status versus its practical necessity.',
    'If the historical record weighs toward survival-driven concession rather than independent revelation, the substitutionist reading''s legitimacy claim weakens substantially, and the excommunication basis for fundamentalist descendants loses theological grounding — this would not change this story''s authored ε (which is fixed for THIS reading) but would shift the credibility weighting an outside analyst assigns across the three sibling readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revelation_vs_survival_framing, conceptual, 'Whether the Manifesto is genuine revelation, prudential suspension, or acknowledged coercion response — the central kernel contest.').

omega_variable(
    fundamentalist_standing_ambiguity,
    'Do communities that continued plural marriage after 1890 hold a legitimate claim under a still-binding prior command, or are they properly classified as post-doctrinal apostates?',
    'Would require either institutional acknowledgment that the prior command was never formally rescinded (supporting continuationist_reading) or continued adherence to the substitutionist framing''s excommunication logic; no neutral arbiter exists since the interpreting authority is also the beneficiary of one reading.',
    'Directly determines whether fundamentalist_polygamist_descendants is correctly modeled as a victim class of this reading or as adherents to a different, still-valid constraint (continuationist_reading) with no victim relationship to this one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fundamentalist_standing_ambiguity, conceptual, 'Whether fundamentalist continuation is apostasy under this reading or legitimate practice under a sibling reading.').

omega_variable(
    institutional_self_interest_in_framing_choice,
    'To what extent was the choice of revelation-framing over coercion-acknowledgment itself driven by the institution''s structural interest in preserving unbroken prophetic authority claims?',
    'Analysis of parallel cases where the institution did or did not frame doctrinal reversals as revelation, correlated with the legal/political stakes present in each case.',
    'High correlation between stakes and revelation-framing would support treating the substitutionist reading itself as partly an artifact of institutional self-preservation rather than purely a theological claim, reinforcing the tangled_rope classification over a cleaner rope reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_self_interest_in_framing_choice, empirical, 'Whether the pattern of revelation-framing choices correlates with institutional legal exposure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__substitutionist_reading, 1890, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t1890, divine_marriage_command__substitutionist_reading, theater_ratio, 1890, 0.55).
narrative_ontology:measurement(divi_tr_t1920, divine_marriage_command__substitutionist_reading, theater_ratio, 1920, 0.48).
narrative_ontology:measurement(divi_tr_t1950, divine_marriage_command__substitutionist_reading, theater_ratio, 1950, 0.44).
narrative_ontology:measurement(divi_tr_t1980, divine_marriage_command__substitutionist_reading, theater_ratio, 1980, 0.42).
narrative_ontology:measurement(divi_tr_t2000, divine_marriage_command__substitutionist_reading, theater_ratio, 2000, 0.43).
narrative_ontology:measurement(divi_tr_t2020, divine_marriage_command__substitutionist_reading, theater_ratio, 2020, 0.44).

% Extraction over time
narrative_ontology:measurement(divi_be_t1890, divine_marriage_command__substitutionist_reading, base_extractiveness, 1890, 0.35).
narrative_ontology:measurement(divi_be_t1920, divine_marriage_command__substitutionist_reading, base_extractiveness, 1920, 0.42).
narrative_ontology:measurement(divi_be_t1950, divine_marriage_command__substitutionist_reading, base_extractiveness, 1950, 0.48).
narrative_ontology:measurement(divi_be_t1980, divine_marriage_command__substitutionist_reading, base_extractiveness, 1980, 0.53).
narrative_ontology:measurement(divi_be_t2000, divine_marriage_command__substitutionist_reading, base_extractiveness, 2000, 0.56).
narrative_ontology:measurement(divi_be_t2020, divine_marriage_command__substitutionist_reading, base_extractiveness, 2020, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t1890, divine_marriage_command__substitutionist_reading, suppression_requirement, 1890, 0.85).
narrative_ontology:measurement(divi_su_t1920, divine_marriage_command__substitutionist_reading, suppression_requirement, 1920, 0.75).
narrative_ontology:measurement(divi_su_t1950, divine_marriage_command__substitutionist_reading, suppression_requirement, 1950, 0.68).
narrative_ontology:measurement(divi_su_t1980, divine_marriage_command__substitutionist_reading, suppression_requirement, 1980, 0.64).
narrative_ontology:measurement(divi_su_t2000, divine_marriage_command__substitutionist_reading, suppression_requirement, 2000, 0.62).
narrative_ontology:measurement(divi_su_t2020, divine_marriage_command__substitutionist_reading, suppression_requirement, 2020, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__substitutionist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(divine_marriage_command__substitutionist_reading, 0.1).
narrative_ontology:affects_constraint(divine_marriage_command__substitutionist_reading, divine_marriage_command__continuationist_reading).
narrative_ontology:affects_constraint(divine_marriage_command__substitutionist_reading, divine_marriage_command__coercion_visibility_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the divine_marriage_command kernel, decomposed per the ε-invariance principle: the natural-language label 'the Manifesto's doctrinal status' conflates at least three structurally distinct claims (new revelation superseding prior command; prudential suspension without rescission; acknowledged coercion response). Each reading has its own ε, beneficiary/victim structure, and classification. This reading (substitutionist) forecloses the continuationist_reading within any single institutional framework (the prior command cannot be simultaneously rescinded and still-binding) while merely influencing the coercion_visibility_reading (adopting the revelation frame changes the institution's legitimacy resources and rhetorical options available to a coercion-acknowledgment account without making that account impossible for other parties to hold).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(divine_marriage_command__substitutionist_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
