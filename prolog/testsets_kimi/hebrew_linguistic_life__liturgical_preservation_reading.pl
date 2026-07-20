% ============================================================================
% CONSTRAINT STORY: hebrew_linguistic_life__liturgical_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_linguistic_life__liturgical_preservation_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: hebrew_linguistic_life__liturgical_preservation_reading
 *   human_readable: Liturgical Preservation Definition of Hebrew Linguistic Life
 *   domain: sociolinguistic/religious/nationalist
 *
 * SUMMARY:
 *   This constraint instantiates the liturgical_preservation_reading of the
 *   contested kernel hebrew_linguistic_life. Under this reading, Hebrew never
 *   died as a language because its sacred texts were continuously recited,
 *   studied, and transmitted in an unbroken chain regardless of vernacular
 *   use. The modern vernacular revival led by Ben-Yehuda and successors is
 *   framed not as resurrection but as desecration of a sacred tongue. The
 *   constraint functions as a definitional gate: only liturgical-custodial
 *   activity counts as linguistic life, while secular native-speaker
 *   competence is structurally delegitimized. This generates genuine
 *   coordination (preservation of sacred texts across diaspora) alongside
 *   asymmetric extraction (instrumentalization of tradition for nationalist
 *   continuity claims, consolidation of religious authority over language,
 *   suppression of vernacular legitimacy). The claimed type is tangled_rope;
 *   the metrics are authored independently to describe actual operation.
 *
 * KEY AGENTS:
 *   - liturgical_authorities (agenda_setter/institutional/constrained): Rabbinic and religious institutions that control textual transmission and define legitimate Hebrew use
 *   - traditionalist_institutions (beneficiary/institutional): Yeshivas, religious councils, and diaspora communities whose authority is vindicated by the unbroken-chain narrative
 *   - secular_hebrew_community (payer/organized): Native Hebrew speakers whose daily vernacular use is classified as desecration
 *   - sacred_tradition (payer/non-agent): The textual and liturgical corpus instrumentalized to bear nationalist-linguistic burdens
 *   - modern_hebrew_intelligentsia (excluded/moderate): Secular writers, poets, and academics producing non-liturgical Hebrew culture
 *   - academic_linguists (observer/institutional): Analytical observers studying the sociology of Hebrew revival and liturgy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__liturgical_preservation_reading, 0.71).
domain_priors:suppression_score(hebrew_linguistic_life__liturgical_preservation_reading, 0.64).
domain_priors:theater_ratio(hebrew_linguistic_life__liturgical_preservation_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__liturgical_preservation_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_linguistic_life__liturgical_preservation_reading, "Liturgical Preservation Definition of Hebrew Linguistic Life").
narrative_ontology:topic_domain(hebrew_linguistic_life__liturgical_preservation_reading, "sociolinguistic/religious/nationalist").

domain_priors:requires_active_enforcement(hebrew_linguistic_life__liturgical_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__liturgical_preservation_reading, '86f4682a-424d-43d8-b9f2-1a69c746850e').
narrative_ontology:cs_kernel_codification('86f4682a-424d-43d8-b9f2-1a69c746850e', fixed_text).
narrative_ontology:cs_authority_grounding('86f4682a-424d-43d8-b9f2-1a69c746850e', lineage).
narrative_ontology:cs_interpretation_layer_present('86f4682a-424d-43d8-b9f2-1a69c746850e').
narrative_ontology:cs_reading_relation('86f4682a-424d-43d8-b9f2-1a69c746850e', hebrew_linguistic_life__native_generational_reading, forecloses).
narrative_ontology:cs_reading_relation('86f4682a-424d-43d8-b9f2-1a69c746850e', hebrew_linguistic_life__marketplace_pidgin_reading, forecloses).
narrative_ontology:cs_axiom('86f4682a-424d-43d8-b9f2-1a69c746850e', foundational, sacred_transmission_constitutes_linguistic_life).
narrative_ontology:cs_axiom_status(sacred_transmission_constitutes_linguistic_life, holdable).
narrative_ontology:cs_axiom_grounding('86f4682a-424d-43d8-b9f2-1a69c746850e', sacred_transmission_constitutes_linguistic_life, theological).
narrative_ontology:cs_axiom('86f4682a-424d-43d8-b9f2-1a69c746850e', foundational, vernacular_secularization_desecrates_tongue).
narrative_ontology:cs_axiom_status(vernacular_secularization_desecrates_tongue, holdable).
narrative_ontology:cs_axiom_grounding('86f4682a-424d-43d8-b9f2-1a69c746850e', vernacular_secularization_desecrates_tongue, theological).
narrative_ontology:cs_reference_frame('86f4682a-424d-43d8-b9f2-1a69c746850e', unbroken_liturgical_chain).
narrative_ontology:cs_drift_state('86f4682a-424d-43d8-b9f2-1a69c746850e', post_vernacular_revival, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('86f4682a-424d-43d8-b9f2-1a69c746850e', '').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, liturgical_authorities).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, traditionalist_institutions).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, secular_hebrew_community).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, sacred_tradition).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__liturgical_preservation_reading, unbroken_transmission_doctrine).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__liturgical_preservation_reading, diasporic_continuity_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rabbinic and religious institutions that control the transmission, interpretation, and recitation of Hebrew sacred texts. They set the definitional standard that only liturgical-custodial activity constitutes linguistic life, and enforce it through religious education, canonical boundaries, and ritual competence requirements. Their authority is subsidized by the constraint because they become the sole arbiters of whether Hebrew is 'alive.'
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, liturgical_authorities, agenda_setter,
    institutional, generational, constrained, global).

% Yeshivas, religious councils, and diaspora communities whose continuity is vindicated by the unbroken-chain narrative. They benefit from institutional prestige and resource allocation tied to being the preservers of linguistic life, without necessarily administering the constraint directly.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, traditionalist_institutions, beneficiary,
    institutional, generational, constrained, global).

% Native and fluent Hebrew speakers who use the language for daily secular functions, commerce, literature, and domestic life. Their vernacular competence is delegitimized as 'not real Hebrew' or active desecration by the liturgical framework, despite constituting the vast majority of actual Hebrew speech. Exit is constrained because the polity's educational, legal, and cultural institutions embed the liturgical definition.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, secular_hebrew_community, payer,
    organized, biographical, constrained, national).

% The corpus of Hebrew sacred texts, liturgical practices, and interpretive traditions transmitted across two millennia. It is compelled to carry the burden of proving Hebrew linguistic continuity and nationalist vitality despite having been maintained for religious rather than political-linguistic purposes. The tradition is trapped because it cannot reject the instrumentalization without breaking the chain that preserves it.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, sacred_tradition, payer,
    powerless, immediate, trapped, universal).
narrative_ontology:stakeholder_non_agent(hebrew_linguistic_life__liturgical_preservation_reading, sacred_tradition).

% Secular writers, poets, academics, and cultural producers who create non-liturgical Hebrew literature, theater, and scholarship. They are structurally excluded from the definition of linguistic life because their production is not sacred textual transmission; their speech is categorized as desecration or at best irrelevant noise.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, modern_hebrew_intelligentsia, excluded,
    moderate, biographical, constrained, national).

% Sociolinguists and historians who study Hebrew revival, language death, and liturgical continuity from outside the religious beneficiary set. They document the empirical reality of native secular Hebrew acquisition and the historical construction of the 'unbroken chain' narrative.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, academic_linguists, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_linguistic_life__liturgical_preservation_reading, liturgical_authorities).
narrative_ontology:fixing_cost_class(hebrew_linguistic_life__liturgical_preservation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves Jewish textual and liturgical continuity across two millennia of diaspora without territorial sovereignty, ensuring that Hebrew remains a language of sacred study and ritual performance even as daily vernacular shifted to other languages.
% TRANSFER_FUNCTION: Moves authority over Hebrew linguistic legitimacy from vernacular speakers and secular institutions to liturgical custodians and traditional religious authorities; transfers the burden of nationalist continuity onto sacred textual practice, making the tradition responsible for proving the nation did not suffer linguistic death.
% ABSENT_VOICES: Secular Hebrew poets, modern Israeli writers, Ben-Yehuda revivalists, and vernacular-first linguists are structurally excluded from the definition of linguistic life; their speech is classified as desecration rather than legitimate language use, and they are not present in the rooms where liturgical legitimacy is adjudicated.
% DISAPPEARANCE_RATIONALE: If this definitional constraint vanished, secular Hebrew would be recognized as fully legitimate linguistic life, the nationalist burden on sacred tradition would lift, liturgical authorities would lose their monopoly on linguistic legitimacy, and the historical narrative would shift from 'Hebrew never died' to acknowledging a modern revival. Religious educational curricula and family-language policy would rearrange around vernacular reality.
% FOUNDING_PROBLEM: Maintaining Jewish textual and ritual continuity across two millennia of diaspora without territorial sovereignty or political statehood, ensuring Hebrew remained a language of sacred study even as daily vernacular shifted to Yiddish, Ladino, Arabic, and other languages.
% FOUNDING_PROBLEM_CORROBORATION: Religious historians and diaspora studies scholars corroborate the continuity problem from within the tradition. Secular Zionist historians and modern sociolinguists corroborate from outside the beneficiary set that the founding problem's character fundamentally changed with modern statehood and vernacular revival; they attest that the problem is either solved or transformed, not still live in its original form.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__liturgical_preservation_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__liturgical_preservation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__liturgical_preservation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_linguistic_life__liturgical_preservation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_linguistic_life__liturgical_preservation_reading, 0.71, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_linguistic_life__liturgical_preservation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_linguistic_life__liturgical_preservation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_linguistic_life__liturgical_preservation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.71) is high because the constraint extracts vernacular linguistic legitimacy and transfers it to liturgical custodians; it makes sacred tradition carry nationalist water. Suppression (0.64) reflects active institutional enforcement through religious education, canonical control, and delegitimation of secular usage. Theater_ratio (0.42) captures the mix of genuine devotional practice with performed continuity for nationalist consumption. Accessibility_collapse (0.70) is high within the traditionalist framework but moderate externally; alternatives (vernacular definitions) are collapsed in religious educational contexts. Resistance (0.58) comes from secular Hebrew speakers, modern literature, and academic linguistics. The temporal series show extraction intensifying as the Zionist vernacular project succeeded, provoking harder defensive enforcement of the liturgical boundary.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (liturgical authorities) experiences this constraint as sacred duty and genuine continuity; the engine will compute a coordination-weighted type from their structural position. The payer seats (secular community, sacred tradition itself) experience extraction of legitimacy and instrumentalization; the engine will compute higher effective extraction. The excluded seat (modern intelligentsia) is simply outside the definitional frame. The divergence is structural: same constraint, opposite directionalities.
 *
 * DIRECTIONALITY LOGIC:
 *   Liturgical authorities and traditionalist institutions are beneficiaries: the constraint subsidizes their authority by making them the sole arbiters of linguistic life (d near the beneficiary end). Secular Hebrew speakers and modern intelligentsia are targets: the constraint extracts their linguistic legitimacy and renders their speech invisible or desecrated (d near the target end). Sacred tradition, though a non-agent, is structurally victimized by being conscripted into nationalist discourse. The directionality is amplified for trapped targets (secular speakers in a polity where religious authority controls education and family law) and damped for the agenda setter.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as pure extraction (snare) by acknowledging the genuine coordination function: two millennia of diasporic text preservation is real and socially valuable. It prevents mislabeling as pure coordination (rope) by naming the victims: secular speakers whose language is delegitimized, and sacred tradition itself burdened by nationalist framing. Tangled_rope captures both. If the coordination function atrophied and only theatrical maintenance remained, it would degrade toward piton; current metrics show active coordination still present, so piton is not the right claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sacred_tradition_agency_ambiguity,
    'Is sacred tradition a genuine victim-agent of instrumentalization, or merely a reified narrative device projected onto texts?',
    'Examine whether the constraint deforms religious practice by subordinating liturgical study to nationalist continuity claims; interview religious practitioners on whether the ''unbroken chain'' framing alters their relationship to the texts.',
    'If tradition is not a genuine victim, the victim set contracts to secular communities only and effective extraction may drop; if it is genuinely instrumentalized, the tangled_rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sacred_tradition_agency_ambiguity, conceptual, 'Whether sacred tradition is an agentive victim or a narrative projection').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (institutional control of education, family law, and religious bureaucracy) or internalized (identity fusion with liturgical Hebrew such that secular usage feels like self-desecration)?',
    'Post-exit trajectory analysis: observe whether secular Hebrew speakers who leave religious communities continue to delegitimize their own vernacular, or whether suppression collapses upon structural exit.',
    'If internalized, the constraint''s effective suppression exceeds the structural measure and the victim set includes the speakers'' own self-concept; if purely structural, extraction is more externally bounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism').

omega_variable(
    nationalist_continuity_contest,
    'Is the liturgical-preservation claim primarily a theological commitment about sacred language, or a nationalist construct using tradition to deny the historical rupture of diaspora and revival?',
    'Historical genealogy of the ''Hebrew never died'' narrative: trace when and by whom the liturgical criterion was elevated to a linguistic-life definition, and whether this preceded or followed modern nationalist mobilization.',
    'If primarily nationalist, the coordination function is thinner than claimed and extraction is higher; if primarily theological, the beneficiary structure is more internally coherent and the tangled rope is more coordination-heavy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(nationalist_continuity_contest, empirical, 'Theological versus nationalist origin of the definitional claim').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__liturgical_preservation_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(heb_lit_pres_tr_t0, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(heb_lit_pres_tr_t10, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(heb_lit_pres_tr_t20, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(heb_lit_pres_tr_t30, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(heb_lit_pres_tr_t40, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 40, 0.43).
narrative_ontology:measurement(heb_lit_pres_tr_t50, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(heb_lit_pres_be_t0, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(heb_lit_pres_be_t10, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(heb_lit_pres_be_t20, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(heb_lit_pres_be_t30, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(heb_lit_pres_be_t40, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 40, 0.7).
narrative_ontology:measurement(heb_lit_pres_be_t50, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 50, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(heb_lit_pres_su_t0, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(heb_lit_pres_su_t10, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(heb_lit_pres_su_t20, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(heb_lit_pres_su_t30, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 30, 0.63).
narrative_ontology:measurement(heb_lit_pres_su_t40, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 40, 0.64).
narrative_ontology:measurement(heb_lit_pres_su_t50, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 50, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__liturgical_preservation_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life__native_generational_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life__marketplace_pidgin_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the hebrew_linguistic_life kernel. The kernel decomposes into three structurally distinct constraints because the natural-language label 'Hebrew linguistic life' conflates competing definitions with different epsilon values, victim sets, and authority structures. This reading (liturgical_preservation) has low coordination cost but high extraction from secular speakers; the native_generational reading has a different victim set (liturgical elites) and different extraction profile; the marketplace_pidgin reading treats sacred function as irrelevant. They are linked as a constraint family via network edges and cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
