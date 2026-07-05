% ============================================================================
% CONSTRAINT STORY: article_9_war_renunciation__strict_pacifist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_9_war_renunciation__strict_pacifist_reading, []).

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
 *   constraint_id: article_9_war_renunciation__strict_pacifist_reading
 *   human_readable: Article 9 Strict Pacifist Reading (Categorical Prohibition on Armed Forces)
 *   domain: constitutional_law/security_policy
 *
 * SUMMARY:
 *   This story instantiates the strict pacifist reading of Article 9 of the
 *   Japanese Constitution as a single, structurally distinct constraint: the
 *   textual clause 'never be maintained' is read as a categorical,
 *   non-negotiable prohibition on any armed forces, defensive or otherwise.
 *   Under this reading the continued existence of the Self-Defense Forces is
 *   not a permitted minimal capacity but an ongoing constitutional violation
 *   papered over by cabinet-level legal interpretation. This is one of three
 *   readings of the article_9_war_renunciation kernel; the
 *   inherent_right_reading and collective_self_defense_reading are separate
 *   constraint stories with different ε, different beneficiary/victim sets,
 *   and different classifications — they are not blended here.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__strict_pacifist_reading, 0.44).
domain_priors:suppression_score(article_9_war_renunciation__strict_pacifist_reading, 0.52).
domain_priors:theater_ratio(article_9_war_renunciation__strict_pacifist_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, extractiveness, 0.44).
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__strict_pacifist_reading, tangled_rope).
narrative_ontology:human_readable(article_9_war_renunciation__strict_pacifist_reading, "Article 9 Strict Pacifist Reading (Categorical Prohibition on Armed Forces)").
narrative_ontology:topic_domain(article_9_war_renunciation__strict_pacifist_reading, "constitutional_law/security_policy").

domain_priors:requires_active_enforcement(article_9_war_renunciation__strict_pacifist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__strict_pacifist_reading, 'e326e40d-5f90-4598-8de8-0bdc409c36d2').
narrative_ontology:cs_kernel_codification('e326e40d-5f90-4598-8de8-0bdc409c36d2', fixed_text).
narrative_ontology:cs_authority_grounding('e326e40d-5f90-4598-8de8-0bdc409c36d2', lineage).
narrative_ontology:cs_interpretation_layer_present('e326e40d-5f90-4598-8de8-0bdc409c36d2').
narrative_ontology:cs_reading_relation('e326e40d-5f90-4598-8de8-0bdc409c36d2', article_9_war_renunciation__inherent_right_reading, forecloses).
narrative_ontology:cs_reading_relation('e326e40d-5f90-4598-8de8-0bdc409c36d2', article_9_war_renunciation__collective_self_defense_reading, forecloses).
narrative_ontology:cs_axiom('e326e40d-5f90-4598-8de8-0bdc409c36d2', foundational, textual_language_admits_no_defensive_exception).
narrative_ontology:cs_axiom_status(textual_language_admits_no_defensive_exception, holdable).
narrative_ontology:cs_axiom_grounding('e326e40d-5f90-4598-8de8-0bdc409c36d2', textual_language_admits_no_defensive_exception, conventional).
narrative_ontology:cs_axiom('e326e40d-5f90-4598-8de8-0bdc409c36d2', foundational, war_renunciation_is_categorical_not_conditional).
narrative_ontology:cs_axiom_status(war_renunciation_is_categorical_not_conditional, holdable).
narrative_ontology:cs_axiom_grounding('e326e40d-5f90-4598-8de8-0bdc409c36d2', war_renunciation_is_categorical_not_conditional, deontological).
narrative_ontology:cs_axiom('e326e40d-5f90-4598-8de8-0bdc409c36d2', secondary, sdf_existence_is_unresolved_constitutional_deviation).
narrative_ontology:cs_axiom_status(sdf_existence_is_unresolved_constitutional_deviation, holdable).
narrative_ontology:cs_axiom_grounding('e326e40d-5f90-4598-8de8-0bdc409c36d2', sdf_existence_is_unresolved_constitutional_deviation, conventional).
narrative_ontology:cs_reference_frame('e326e40d-5f90-4598-8de8-0bdc409c36d2', occupation_era_pacifist_settlement).
narrative_ontology:cs_drift_state('e326e40d-5f90-4598-8de8-0bdc409c36d2', post_2014_reinterpretation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e326e40d-5f90-4598-8de8-0bdc409c36d2', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__strict_pacifist_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, pacifist_civil_society_movement).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, regional_states_wary_of_japanese_remilitarization).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, constitutional_textualist_scholars).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, japan_state_security_autonomy).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, self_defense_forces_personnel).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, frontline_island_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, united_states_alliance_partner).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__strict_pacifist_reading, pacifist_constitutional_settlement_doctrine).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__strict_pacifist_reading, textual_supremacy_over_inherent_right_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Litigates, protests, and votes to hold the government to the literal 'never be maintained' text whenever defense budgets or SDF mandate expansions are proposed. Draws legitimacy and continued relevance from the reading remaining textually absolute; a settled inherent-right reading would dissolve much of their mobilizing purpose.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, pacifist_civil_society_movement, beneficiary,
    organized, generational, mobile, national).

% Benefit diplomatically and strategically from Japan's constitutional inability to maintain conventional armed forces; cite the strict text in bilateral and multilateral fora to resist any expansion of Japanese military capacity, without bearing any of the cost of Japan's actual defense posture.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, regional_states_wary_of_japanese_remilitarization, beneficiary,
    institutional, generational, analytical, regional).

% Build academic and judicial authority on close textual reading of 'never be maintained' as unambiguous; their interpretive framework is vindicated each time the government is forced to justify SDF existence through euphemism (self-defense force, not military) rather than open amendment.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, constitutional_textualist_scholars, beneficiary,
    moderate, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__strict_pacifist_reading, constitutional_textualist_scholars, observer).

% The state cannot openly name, fund, or organize a military under this reading without contradicting its own founding text; it maintains the Self-Defense Forces through decades of legal fiction and constrained doctrine, ceding genuine strategic flexibility and forcing dependence on the US-Japan alliance for any capability beyond minimal self-defense.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, japan_state_security_autonomy, payer,
    institutional, generational, trapped, national).

% Serve in an organization whose constitutional legitimacy is perpetually contested; operate under legal ambiguity about their status as combatants, face restricted rules of engagement and collective self-defense authority, and bear career and legal risk from the unresolved textual question every deployment raises.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, self_defense_forces_personnel, payer,
    moderate, biographical, constrained, national).

% Live under the direct security consequences of a defense posture constitutionally barred from full conventional deterrence capacity; in a regional contingency they bear the immediate physical risk of a security policy shaped by textual absolutism rather than operational necessity.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, frontline_island_populations, payer,
    powerless, immediate, trapped, regional).

% Provides the extended deterrence Japan cannot constitutionally provide itself, in exchange for basing rights and strategic alignment; benefits from Japan's induced dependence on US forces and helped author the postwar settlement that produced the strict-reading text in the first place.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, united_states_alliance_partner, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__strict_pacifist_reading, united_states_alliance_partner, agenda_setter).

% Has never been asked to ratify a formal amendment resolving the textual question despite polling showing divided views; the constitutional amendment threshold (two-thirds of both Diet houses plus national referendum) has never been cleared, leaving the strict reading's continuation a function of political paralysis rather than affirmed popular will.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, japanese_electorate, excluded,
    organized, generational, constrained, national).

% The body that issues authoritative interpretations reconciling SDF existence with the text, historically holding the strict line against collective self-defense until the 2014 reinterpretation partially breached it; administers the ambiguity that lets the state avoid both full rearmament and full compliance with the text's plain words.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, cabinet_legal_bureau, agenda_setter,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a durable postwar settlement: a constitutional guarantee, legible to Japan's neighbors and to its own citizens, that the state will not reconstitute war-making capacity — reducing regional security-dilemma dynamics and domestic militarist revival risk in the decades after 1945.
% TRANSFER_FUNCTION: Moves strategic autonomy and defense-planning latitude away from the Japanese state and its frontline populations toward the pacifist domestic constituency (who receive assurance and legitimacy), regional neighbors (who receive a constrained counterpart), and the United States (who receives a dependent, basing-committed ally).
% ABSENT_VOICES: The Japanese electorate has never been given a binding referendum on Article 9's meaning; SDF personnel serving under constitutionally ambiguous status have no formal channel to resolve their legal footing; frontline populations bearing direct security risk from constrained deterrence capacity are not party to the interpretive dispute among scholars, courts, and cabinet lawyers.
% DISAPPEARANCE_RATIONALE: If the strict textual prohibition vanished overnight (superseded cleanly by, say, the inherent-right reading), Japan could openly reorganize, fund, and name a military without euphemism, alliance dependence on the United States would likely loosen, regional states would recalibrate their own postures, and decades of legal fiction sustaining the Self-Defense Forces would become unnecessary — a substantial rearrangement of security architecture in Northeast Asia.
% FOUNDING_PROBLEM: Post-surrender Japan needed a credible, internationally legible guarantee against remilitarization to satisfy Allied occupation authorities and reassure a region devastated by Japanese aggression, while giving the new constitutional order clean moral distance from the imperial military state.
% FOUNDING_PROBLEM_CORROBORATION: Regional governments (South Korea, China) and academic historians outside the Japanese pacifist movement continue to attest the founding problem — fear of Japanese remilitarization — remains live and cite it in diplomatic protests over SDF capability expansion. Conversely, Japanese security establishment figures and US alliance planners, also outside the domestic pacifist beneficiary group, attest the original problem (a militarist Japanese state capable of unilateral aggression) has been structurally resolved by seven decades of democratic consolidation and alliance integration, making the textual absolutism a residual constraint rather than a live safeguard.
narrative_ontology:disappearance_verdict(article_9_war_renunciation__strict_pacifist_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_9_war_renunciation__strict_pacifist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__strict_pacifist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_9_war_renunciation__strict_pacifist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_9_war_renunciation__strict_pacifist_reading, 0.44, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_9_war_renunciation__strict_pacifist_reading_tests).
:- end_tests(article_9_war_renunciation__strict_pacifist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.44 at 2024) and rising: as the SDF's actual capability and doctrine have expanded (1954 founding, 1990s peacekeeping deployments, 2014 collective self-defense reinterpretation), the gap between the strict text and lived practice has widened, and the reading increasingly serves as a rhetorical/legal cudgel rather than a description of operative constraint. Theater ratio rises sharply (0.15 to 0.58) because the state has never openly repudiated the text nor amended it — instead each expansion of SDF capacity is accompanied by elaborate reinterpretive justification (individual vs. collective self-defense, 'exclusively defense-oriented policy') that performs continued fidelity to the strict reading while permitting practice to diverge from it. Suppression is substantial (0.52) reflecting the real legal and political cost imposed on any actor proposing open constitutional amendment — the two-thirds Diet threshold plus national referendum functions as an entrenchment device that the beneficiary coalition (pacifist movement, textualist scholars, wary neighbors) actively defends.
 *
 * PERSPECTIVAL GAP:
 *   From the pacifist movement's seat this is coordination: a hard-won, textually clear guarantee against militarist revival, worth defending precisely because it is absolute and non-negotiable. From the state security apparatus's seat the same text is a tangled extraction: it requires constant legal contortion to sustain a defense capability the nation's elected leadership and public plausibly want, imposing real strategic and personnel costs to maintain the fiction of compliance. The engine should compute these as different seat classifications from the same structural data, not reconcile them into one verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   The pacifist civil society movement and constitutional textualist scholars are structural beneficiaries: the strict reading is the source of their political and academic standing, and its erosion would diminish both. Regional states wary of remilitarization benefit diplomatically without bearing any of the security cost. The Japanese state itself, SDF personnel, and frontline island populations are the payers: they bear the security-autonomy cost, the legal-status ambiguity, and the immediate physical risk respectively, while having no exit from the jurisdiction whose constitutional text binds them. The United States occupies a dual position — beneficiary of Japan's induced alliance dependence, and simultaneously an agenda-setter given its historical role in drafting the postwar constitution and its ongoing influence over Japan's defense posture through the bilateral security treaty.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — credible international assurance against a remilitarized, aggressive Japanese state — was substantially addressed by decades of democratic consolidation and alliance integration well before 2024, yet the strict textual reading persists as a live interpretive claim rather than being formally resolved through amendment. This is the mismatch the R5 genealogy interview is built to surface: founding_problem_status is contested precisely because the beneficiary coalition (pacifist movement, wary neighbors) attests the problem remains live while corroborating sources outside that coalition attest it has been structurally resolved. Because the state has never held the constitutional-amendment referendum, the reading's continuation cannot be read as an affirmed contemporary popular choice — it persists by institutional inertia and political-cost avoidance as much as by genuine ongoing coordination need, which is the tangled-rope signature: real coordination function at founding, layered extraction (of security autonomy, legal clarity, and personnel certainty) sustained by active political enforcement (the amendment threshold) decades after the founding threat receded.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_absolutism_vs_functional_necessity,
    'Does the phrase ''never be maintained'' admit of a textually honest reading that nonetheless permits the SDF''s actual scope, or does the SDF''s continued existence constitute an unresolved constitutional violation under the plain text?',
    'A definitive Supreme Court ruling squarely addressing SDF constitutionality (the Court has repeatedly avoided direct rulings via political-question doctrine), or a formal constitutional amendment resolving the text one way or the other.',
    'If the strict reading is textually correct, seven decades of SDF operation under cabinet reinterpretation constitute sustained extra-constitutional practice with real victims (state security autonomy, personnel legal certainty). If a more permissive reading is textually available, the strict reading is better understood as a political preference dressed as textual necessity, weakening its claim to categorical status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_absolutism_vs_functional_necessity, conceptual, 'Whether the strict textual reading is genuinely compelled by the constitutional language or is one contestable interpretation among several.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the strict pacifist reading, the inherent-right reading, or the collective-self-defense reading the operative constraint governing Japanese state behavior at any given historical moment?',
    'Track which reading the Cabinet Legal Bureau, the Diet majority, and judicial dicta actually rely on at each juncture (1954 SDF founding, 1991 Gulf War non-participation, 2014 reinterpretation, ongoing 2020s capability debates) — the operative reading has shifted institutionally over time even without formal amendment.',
    'If the operative reading has already shifted de facto toward the inherent-right or collective-self-defense reading while the strict reading persists only in civil-society and opposition-party rhetoric, this story''s classification describes a reading with declining institutional purchase, and the sibling readings carry more of the live constitutional weight than this story alone suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Which of the three kernel readings is actually operative in state practice at any given time, versus asserted rhetorically.').

omega_variable(
    amendment_referendum_never_held,
    'Would a national referendum on Article 9 amendment, if actually held, ratify the strict reading, the inherent-right reading, or produce a novel compromise text?',
    'The constitutionally mandated path (two-thirds Diet approval plus national referendum) has never been attempted for Article 9; polling is suggestive but not equivalent to a binding vote under referendum conditions with a specific amendment text on the ballot.',
    'Absent an actual referendum, the strict reading''s continuation cannot be distinguished from simple political paralysis versus genuine affirmed public will — this bears directly on whether founding_problem_status should be read as contested-but-legitimate or contested-and-stale.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(amendment_referendum_never_held, empirical, 'Whether public will actually favors the strict reading absent a never-attempted binding referendum.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__strict_pacifist_reading, 1947, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1947, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 1947, 0.15).
narrative_ontology:measurement(arti_tr_t1960, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 1960, 0.3).
narrative_ontology:measurement(arti_tr_t1980, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 1980, 0.42).
narrative_ontology:measurement(arti_tr_t2000, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 2000, 0.5).
narrative_ontology:measurement(arti_tr_t2014, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 2014, 0.55).
narrative_ontology:measurement(arti_tr_t2024, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 2024, 0.58).

% Extraction over time
narrative_ontology:measurement(arti_be_t1947, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 1947, 0.22).
narrative_ontology:measurement(arti_be_t1960, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 1960, 0.28).
narrative_ontology:measurement(arti_be_t1980, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 1980, 0.33).
narrative_ontology:measurement(arti_be_t2000, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 2000, 0.36).
narrative_ontology:measurement(arti_be_t2014, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 2014, 0.4).
narrative_ontology:measurement(arti_be_t2024, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 2024, 0.44).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1947, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 1947, 0.35).
narrative_ontology:measurement(arti_su_t1960, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 1960, 0.4).
narrative_ontology:measurement(arti_su_t1980, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 1980, 0.44).
narrative_ontology:measurement(arti_su_t2000, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 2000, 0.47).
narrative_ontology:measurement(arti_su_t2014, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 2014, 0.49).
narrative_ontology:measurement(arti_su_t2024, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 2024, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__strict_pacifist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_9_war_renunciation__strict_pacifist_reading, 0.12).
narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, article_9_war_renunciation__inherent_right_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, article_9_war_renunciation__collective_self_defense_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, us_japan_security_treaty).

% DUAL FORMULATION NOTE:
% This story is one of three ε-invariant decompositions of the colloquial 'Article 9' constraint. The strict_pacifist_reading (this file) claims tangled_rope with moderate rising extraction concentrated on state security autonomy and SDF personnel. The inherent_right_reading and collective_self_defense_reading are separate files with their own beneficiary/victim structures and likely different classifications (plausibly closer to rope or scaffold, given their more permissive victim sets). All three are linked as siblings under the article_9_war_renunciation kernel via cs_structure.reading_relations, and this file's network edges connect it to the alliance structure (us_japan_security_treaty) whose extraction depends heavily on which reading is operative.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
