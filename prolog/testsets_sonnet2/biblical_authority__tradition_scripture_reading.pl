% ============================================================================
% CONSTRAINT STORY: biblical_authority__tradition_scripture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_authority__tradition_scripture_reading, []).

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
 *   constraint_id: biblical_authority__tradition_scripture_reading
 *   human_readable: Magisterial Tradition-Scripture Authority (Sacramental Mediation Reading)
 *   domain: theology/institutional_authority
 *
 * SUMMARY:
 *   This story instantiates one reading of the biblical_authority kernel: the
 *   tradition-scripture reading, in which Scripture is held to require
 *   authoritative tradition for correct interpretation, and a magisterial
 *   teaching office guards a fixed deposit of faith against error. This is
 *   distinct from a conciliar reading (interpretive authority as living
 *   collegial consensus among councils and fathers, not a standing office)
 *   and from sola scriptura (Scripture as self-interpreting, requiring no
 *   external magisterial ratification). The interval spans roughly the
 *   patristic period through the Council of Trent and its aftermath, tracing
 *   how a coordination function (doctrinal unity across a dispersed, largely
 *   illiterate population) accumulated increasing extractive overhead
 *   (sacramental gatekeeping, suppression of vernacular translation,
 *   centralized adjudication backed by coercive sanction) as the institution
 *   consolidated and then faced schismatic pressure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_authority__tradition_scripture_reading, 0.62).
domain_priors:suppression_score(biblical_authority__tradition_scripture_reading, 0.58).
domain_priors:theater_ratio(biblical_authority__tradition_scripture_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_authority__tradition_scripture_reading, tangled_rope).
narrative_ontology:human_readable(biblical_authority__tradition_scripture_reading, "Magisterial Tradition-Scripture Authority (Sacramental Mediation Reading)").
narrative_ontology:topic_domain(biblical_authority__tradition_scripture_reading, "theology/institutional_authority").

domain_priors:requires_active_enforcement(biblical_authority__tradition_scripture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_authority__tradition_scripture_reading, '71ddec16-ecd4-4c0c-9923-2916ba9e0af7').
narrative_ontology:cs_kernel_codification('71ddec16-ecd4-4c0c-9923-2916ba9e0af7', formalized).
narrative_ontology:cs_authority_grounding('71ddec16-ecd4-4c0c-9923-2916ba9e0af7', lineage).
narrative_ontology:cs_interpretation_layer_present('71ddec16-ecd4-4c0c-9923-2916ba9e0af7').
narrative_ontology:cs_reading_relation('71ddec16-ecd4-4c0c-9923-2916ba9e0af7', biblical_authority__sola_scriptura_reading, forecloses).
narrative_ontology:cs_reading_relation('71ddec16-ecd4-4c0c-9923-2916ba9e0af7', biblical_authority__conciliar_reading, coexists_with).
narrative_ontology:cs_axiom('71ddec16-ecd4-4c0c-9923-2916ba9e0af7', foundational, scripture_insufficient_without_magisterial_interpretation).
narrative_ontology:cs_axiom_status(scripture_insufficient_without_magisterial_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('71ddec16-ecd4-4c0c-9923-2916ba9e0af7', scripture_insufficient_without_magisterial_interpretation, conventional).
narrative_ontology:cs_axiom('71ddec16-ecd4-4c0c-9923-2916ba9e0af7', foundational, sacraments_require_ordained_mediation_for_grace).
narrative_ontology:cs_axiom_status(sacraments_require_ordained_mediation_for_grace, holdable).
narrative_ontology:cs_axiom_grounding('71ddec16-ecd4-4c0c-9923-2916ba9e0af7', sacraments_require_ordained_mediation_for_grace, theological).
narrative_ontology:cs_axiom('71ddec16-ecd4-4c0c-9923-2916ba9e0af7', secondary, magisterial_office_holds_final_interpretive_authority).
narrative_ontology:cs_axiom_status(magisterial_office_holds_final_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('71ddec16-ecd4-4c0c-9923-2916ba9e0af7', magisterial_office_holds_final_interpretive_authority, conventional).
narrative_ontology:cs_reference_frame('71ddec16-ecd4-4c0c-9923-2916ba9e0af7', apostolic_succession_continuous_transmission).
narrative_ontology:cs_drift_state('71ddec16-ecd4-4c0c-9923-2916ba9e0af7', post_reformation_and_modern_biblical_criticism, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('71ddec16-ecd4-4c0c-9923-2916ba9e0af7', '').
narrative_ontology:cs_kernel_id(biblical_authority__tradition_scripture_reading, biblical_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_authority__tradition_scripture_reading, clerical_hierarchy).
narrative_ontology:constraint_beneficiary(biblical_authority__tradition_scripture_reading, magisterial_teaching_office).
narrative_ontology:constraint_victim(biblical_authority__tradition_scripture_reading, lay_interpretive_agency).
narrative_ontology:constraint_victim(biblical_authority__tradition_scripture_reading, vernacular_reform_movements).
narrative_ontology:constraint_vindicates(biblical_authority__tradition_scripture_reading, apostolic_succession_doctrine).
narrative_ontology:constraint_vindicates(biblical_authority__tradition_scripture_reading, deposit_of_faith_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds and exercises the authority to declare what Scripture means, adjudicating disputes through councils, curial offices, and papal pronouncement. Administers the sacramental system that channels grace through ordained clergy. Determines which readings are orthodox and which are anathema, and controls the institutional consequences of dissent (excommunication, loss of office, condemnation of texts).
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, magisterial_teaching_office, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(biblical_authority__tradition_scripture_reading, magisterial_teaching_office, beneficiary).

% Occupies the ordained offices through which sacramental grace is mediated to the laity. Their institutional standing, livelihood, and social authority depend on the doctrine that valid interpretation and valid sacraments require apostolic succession running through them. They benefit directly from any arrangement that makes lay access to grace and correct doctrine dependent on clerical mediation.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, clerical_hierarchy, beneficiary,
    organized, generational, arbitrage, global).

% Ordinary believers who read or hear Scripture but are told their own interpretation is insufficient and potentially dangerous without magisterial ratification. They pay in dependency: for authoritative doctrine, for the sacraments understood as grace-conferring, and for absolution, they must go through the clerical apparatus. Leaving the interpretive framework means leaving the institution's sacramental economy entirely, which for most of the historical period meant social and spiritual exile.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, lay_interpretive_agency, payer,
    powerless, biographical, constrained, local).

% Groups and individuals (early vernacular translators, lay reading circles, proto-reform movements) who sought direct textual access and interpretive authority independent of the hierarchy. Historically met with suppression of translations, prohibition of unsupervised lay reading, and charges of heresy. Their alternative — unmediated scriptural authority — is the very possibility this constraint exists to foreclose.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, vernacular_reform_movements, payer,
    moderate, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(biblical_authority__tradition_scripture_reading, vernacular_reform_movements, excluded).

% The historical witness of councils and church fathers, which this reading cites as its evidentiary basis but which a rival reading (conciliar) treats as living collegial continuity rather than a possession administered top-down by a magisterial office. Not an agent in the ordinary sense, but its testimony is selectively invoked to legitimate current magisterial authority rather than treated as an ongoing collegial process.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, patristic_conciliar_tradition, excluded,
    moderate, civilizational, constrained, global).

% Scholars of doctrinal history who examine how the tradition-magisterium relationship developed, whether it reflects continuous apostolic transmission or later institutional consolidation (e.g., post-Tridentine centralization), and how it compares structurally to sibling readings that distribute or deny magisterial interpretive monopoly.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, historical_theologians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_authority__tradition_scripture_reading, clerical_hierarchy).
narrative_ontology:fixing_cost_class(biblical_authority__tradition_scripture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, centrally adjudicated reading of Scripture that resolves interpretive disputes, preserves doctrinal continuity across centuries and languages, and gives ordinary believers a stable, authoritative account of belief and practice without requiring each person to resolve exegetical controversies independently.
% TRANSFER_FUNCTION: Moves interpretive authority and sacramental gatekeeping from individual readers and local communities to a centralized clerical hierarchy; in return, the laity receive doctrinal certainty and sacramental access, but only through continued dependency on ordained mediation and submission to magisterial rulings.
% ABSENT_VOICES: Vernacular reform movements and lay readers who sought or claimed independent interpretive competence are structurally excluded from the adjudicating body; their objections historically surface as heresy trials, banned translations, and schismatic movements rather than as recognized voices within the interpretive process itself.
% DISAPPEARANCE_RATIONALE: If magisterial interpretive authority vanished overnight, doctrinal unity would fragment rapidly into competing local and individual readings (the very outcome this reading is structured to prevent), clerical sacramental gatekeeping would lose its exclusive claim, and lay believers would gain direct, unmediated interpretive standing — the entire economy of sacramental mediation and centralized doctrinal adjudication would need to reorganize or dissolve.
% FOUNDING_PROBLEM: Early Christian communities faced the problem of scriptural ambiguity, competing interpretations, and the practical need for doctrinal unity across a geographically dispersed, textually inconsistent, and often illiterate population — some stable mechanism was needed to say authoritatively what the text meant and what practice followed from it.
% FOUNDING_PROBLEM_CORROBORATION: The magisterium and clerical hierarchy attest the founding problem remains live (doctrinal unity, protection from error, guidance for the faithful). Historians of the Reformation and comparative religious scholars — outside the beneficiary set — document that the specific institutional form (centralized magisterial monopoly over interpretation, sacramental mediation as grace-conferring) developed and hardened well after the apostolic period, and that literacy, textual criticism, and translation technology have substantially reduced the practical necessity the arrangement was originally built to address, even as the institution continues to assert it as fully live.
narrative_ontology:disappearance_verdict(biblical_authority__tradition_scripture_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_authority__tradition_scripture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_authority__tradition_scripture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(biblical_authority__tradition_scripture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_authority__tradition_scripture_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_authority__tradition_scripture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_authority__tradition_scripture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_authority__tradition_scripture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from a modest 0.35 in the early centuries (when tradition functioned more as living memory and shared practice) to a peak near 0.65 around the high medieval/early modern consolidation of magisterial authority, before settling slightly lower post-Trent as the institution formalized and to some degree routinized its claims. Suppression tracks a similar arc, peaking during periods of active vernacular translation suppression and heresy prosecution, then declining somewhat as enforcement shifted from coercive sanction toward doctrinal and institutional gatekeeping. Theater ratio rises modestly and steadily — an increasing share of magisterial activity over time is procedural/ceremonial affirmation of authority rather than active resolution of live interpretive disputes.
 *
 * DIRECTIONALITY LOGIC:
 *   The magisterial teaching office and clerical hierarchy are structural beneficiaries: their institutional standing, economic position, and social authority are constituted by the claim that valid interpretation and valid sacraments require their mediation. Lay interpretive agency and vernacular reform movements are structural targets: they bear the cost of dependency (in access to grace, to authoritative doctrine, to legitimate religious practice) and, historically, the cost of active suppression when they sought independent access. The 'excluded' patristic/conciliar tradition is invoked as evidentiary support but is not treated, in this reading, as an ongoing collegial authority independent of the magisterium — that treatment is precisely what distinguishes this reading from the conciliar sibling.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — doctrinal fragmentation across a dispersed, textually unstable, largely illiterate Christian population — was genuinely live in the early centuries and gives the arrangement real coordination content; this is why the classification is tangled_rope rather than pure snare. But literacy, textual criticism, and translation technology have substantially reduced the practical necessity of centralized magisterial mediation for the specific problem of textual access and basic doctrinal transmission, while the institutional claim to interpretive monopoly and sacramental necessity persists largely unchanged in scope. This is the classic tangled-rope signature: a genuine original coordination function persisting alongside an extraction structure that has outlived the acuity of the problem it was built to solve, sustained now primarily by institutional continuity claims (apostolic succession, deposit of faith) rather than by the original practical necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_is_one_of_three,
    'This constraint authors ε and structure for the tradition-scripture reading specifically. The sola_scriptura_reading and conciliar_reading are separate constraints with different beneficiary/victim structures and, plausibly, different ε values — where exactly does the disagreement between these readings live?',
    'Compare the three linked constraint files'' beneficiary/victim declarations and extractiveness values directly; the disagreement is located specifically at (a) whether interpretive authority requires a standing office versus living conciliar consensus versus no external authority at all, and (b) whether sacraments require ordained mediation to confer grace.',
    'Averaging or blending these readings into a single constraint would erase the exact structural facts (sacramental mediation requirement, degree of centralization, presence or absence of a standing adjudicating office) that make them classify differently — this omega documents that the three-way split is intentional and structurally load-bearing, not an artifact of narrative convenience.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_is_one_of_three, conceptual, 'Documents the committer structure: three sibling readings of one kernel, disagreement located in office-vs-council-vs-none and mediation requirements.').

omega_variable(
    natural_continuity_vs_constructed_consolidation,
    'Is the magisterial teaching office''s interpretive authority a natural, continuous unfolding of apostolic commission (as the reading''s own tradition holds), or a historically contingent institutional consolidation that occurred well after the apostolic period and was then retrojected as continuous?',
    'Historical-critical examination of the documentary record for interpretive authority claims across the first five centuries, compared against the record of institutional centralization from the medieval period through Trent; look for discontinuities in the scope and exclusivity of the claimed authority.',
    'If the authority is substantially a later consolidation rather than continuous apostolic transmission, the tangled_rope classification''s coordination component is weaker than the tradition''s own self-account suggests, and the extraction component is correspondingly more central to the arrangement''s actual persistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_continuity_vs_constructed_consolidation, empirical, 'Whether magisterial interpretive monopoly is continuous apostolic practice or later institutional construction.').

omega_variable(
    sacramental_grace_ontology,
    'Is sacramental grace genuinely and exclusively channeled through ordained mediation (a claim not empirically adjudicable), or is this a doctrinal claim whose main structural function is to make lay access to grace dependent on the clerical hierarchy regardless of its theological truth-value?',
    'No empirical resolution mechanism exists for the theological claim itself; the structural question — whether alternative religious communities without ordained mediation report comparable subjective and communal outcomes — can be examined empirically even though the underlying metaphysical claim cannot.',
    'If comparable outcomes exist without ordained mediation, this weakens the coordination justification for the extraction and strengthens the reading of clerical mediation as primarily a rent-collection mechanism attached to a genuine but not exclusively necessary function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sacramental_grace_ontology, preference, 'Whether grace-conferring sacramental mediation is theologically necessary or a structurally convenient claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_authority__tradition_scripture_reading, 0, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_authority__tradition_scripture_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(bibl_tr_t300, biblical_authority__tradition_scripture_reading, theater_ratio, 300, 0.15).
narrative_ontology:measurement(bibl_tr_t700, biblical_authority__tradition_scripture_reading, theater_ratio, 700, 0.2).
narrative_ontology:measurement(bibl_tr_t1100, biblical_authority__tradition_scripture_reading, theater_ratio, 1100, 0.25).
narrative_ontology:measurement(bibl_tr_t1400, biblical_authority__tradition_scripture_reading, theater_ratio, 1400, 0.27).
narrative_ontology:measurement(bibl_tr_t1600, biblical_authority__tradition_scripture_reading, theater_ratio, 1600, 0.28).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_authority__tradition_scripture_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(bibl_be_t300, biblical_authority__tradition_scripture_reading, base_extractiveness, 300, 0.45).
narrative_ontology:measurement(bibl_be_t700, biblical_authority__tradition_scripture_reading, base_extractiveness, 700, 0.55).
narrative_ontology:measurement(bibl_be_t1100, biblical_authority__tradition_scripture_reading, base_extractiveness, 1100, 0.65).
narrative_ontology:measurement(bibl_be_t1400, biblical_authority__tradition_scripture_reading, base_extractiveness, 1400, 0.6).
narrative_ontology:measurement(bibl_be_t1600, biblical_authority__tradition_scripture_reading, base_extractiveness, 1600, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_authority__tradition_scripture_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(bibl_su_t300, biblical_authority__tradition_scripture_reading, suppression_requirement, 300, 0.4).
narrative_ontology:measurement(bibl_su_t700, biblical_authority__tradition_scripture_reading, suppression_requirement, 700, 0.55).
narrative_ontology:measurement(bibl_su_t1100, biblical_authority__tradition_scripture_reading, suppression_requirement, 1100, 0.7).
narrative_ontology:measurement(bibl_su_t1400, biblical_authority__tradition_scripture_reading, suppression_requirement, 1400, 0.6).
narrative_ontology:measurement(bibl_su_t1600, biblical_authority__tradition_scripture_reading, suppression_requirement, 1600, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_authority__tradition_scripture_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(biblical_authority__tradition_scripture_reading, 0.12).
narrative_ontology:affects_constraint(biblical_authority__tradition_scripture_reading, sola_scriptura_reading).
narrative_ontology:affects_constraint(biblical_authority__tradition_scripture_reading, conciliar_reading).

% DUAL FORMULATION NOTE:
% Part of the three-member biblical_authority kernel family. tradition_scripture_reading claims the highest ε of the three siblings because it alone requires ordained sacramental mediation as a condition of both valid interpretation and valid grace, producing a concentrated institutional beneficiary (clerical hierarchy) and a diffuse structural victim (lay interpretive agency) not present in the same form in either sibling reading. sola_scriptura_reading is expected to show near-zero clerical extraction but higher doctrinal fragmentation costs; conciliar_reading is expected to show intermediate extraction with authority distributed across councils rather than concentrated in a standing office. All three should be read as distinct constraints sharing one kernel, never averaged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
