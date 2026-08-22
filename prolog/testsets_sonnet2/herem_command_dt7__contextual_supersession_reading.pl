% ============================================================================
% CONSTRAINT STORY: herem_command_dt7__contextual_supersession_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_herem_command_dt7__contextual_supersession_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: herem_command_dt7__contextual_supersession_reading
 *   human_readable: Herem (Deut. 7) Read as Historically-Bounded, Superseded Directive
 *   domain: religious/ethical/hermeneutical
 *
 * SUMMARY:
 *   This story authors the contextual-supersession reading of the herem
 *   command in Deuteronomy 7: the position that the destruction/separation
 *   mandate applied specifically and only to Israel's settlement-period
 *   conflict with named Canaanite nations, and that its moral force was
 *   subsequently overridden by prophetic universalism (Isaiah, Ruth, Jonah)
 *   and, for Christian readers, by the new covenant's abrogation of ethnic
 *   boundary-marking as a condition of belonging. Under this reading the
 *   extraction the text could otherwise license against intermarriage,
 *   outsider status, and ethnic separation is treated as historically closed
 *   rather than currently operative. The only residual extraction this
 *   reading identifies is the sociological cost borne by individuals in
 *   congregations or families that have not accepted supersession and
 *   continue enforcing separation logic. This is a low-extraction,
 *   low-victim-set instantiation by design of the reading itself — the
 *   durable_separation_reading and allegorical_displacement_reading are
 *   separate constraints with their own ε values and are not blended into
 *   this one.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__contextual_supersession_reading, 0.22).
domain_priors:suppression_score(herem_command_dt7__contextual_supersession_reading, 0.35).
domain_priors:theater_ratio(herem_command_dt7__contextual_supersession_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__contextual_supersession_reading, piton).
narrative_ontology:human_readable(herem_command_dt7__contextual_supersession_reading, "Herem (Deut. 7) Read as Historically-Bounded, Superseded Directive").
narrative_ontology:topic_domain(herem_command_dt7__contextual_supersession_reading, "religious/ethical/hermeneutical").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__contextual_supersession_reading, 'e37afcba-dcbe-4e9d-ad96-8616e39cf3fe').
narrative_ontology:cs_kernel_codification('e37afcba-dcbe-4e9d-ad96-8616e39cf3fe', fixed_text).
narrative_ontology:cs_authority_grounding('e37afcba-dcbe-4e9d-ad96-8616e39cf3fe', lineage).
narrative_ontology:cs_interpretation_layer_present('e37afcba-dcbe-4e9d-ad96-8616e39cf3fe').
narrative_ontology:cs_reading_relation('e37afcba-dcbe-4e9d-ad96-8616e39cf3fe', herem_command_dt7__durable_separation_reading, forecloses).
narrative_ontology:cs_reading_relation('e37afcba-dcbe-4e9d-ad96-8616e39cf3fe', herem_command_dt7__allegorical_displacement_reading, coexists_with).
narrative_ontology:cs_axiom('e37afcba-dcbe-4e9d-ad96-8616e39cf3fe', foundational, herem_temporally_bounded_to_conquest_period).
narrative_ontology:cs_axiom_status(herem_temporally_bounded_to_conquest_period, holdable).
narrative_ontology:cs_axiom_grounding('e37afcba-dcbe-4e9d-ad96-8616e39cf3fe', herem_temporally_bounded_to_conquest_period, empirically_contingent).
narrative_ontology:cs_axiom('e37afcba-dcbe-4e9d-ad96-8616e39cf3fe', foundational, prophetic_and_covenantal_ethics_override_prior_command).
narrative_ontology:cs_axiom_status(prophetic_and_covenantal_ethics_override_prior_command, holdable).
narrative_ontology:cs_axiom_grounding('e37afcba-dcbe-4e9d-ad96-8616e39cf3fe', prophetic_and_covenantal_ethics_override_prior_command, theological).
narrative_ontology:cs_axiom('e37afcba-dcbe-4e9d-ad96-8616e39cf3fe', secondary, ethnic_ancestry_as_membership_criterion).
narrative_ontology:cs_axiom_status(ethnic_ancestry_as_membership_criterion, overridden).
narrative_ontology:cs_axiom_grounding('e37afcba-dcbe-4e9d-ad96-8616e39cf3fe', ethnic_ancestry_as_membership_criterion, theological).
narrative_ontology:cs_created_at('e37afcba-dcbe-4e9d-ad96-8616e39cf3fe', '').
narrative_ontology:cs_kernel_id(herem_command_dt7__contextual_supersession_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__contextual_supersession_reading, interfaith_families).
narrative_ontology:constraint_beneficiary(herem_command_dt7__contextual_supersession_reading, convert_believers).
narrative_ontology:constraint_beneficiary(herem_command_dt7__contextual_supersession_reading, mainline_denominational_leadership).
narrative_ontology:constraint_victim(herem_command_dt7__contextual_supersession_reading, believers_coerced_by_residual_literalist_enforcement).
narrative_ontology:constraint_vindicates(herem_command_dt7__contextual_supersession_reading, prophetic_universalism_doctrine).
narrative_ontology:constraint_vindicates(herem_command_dt7__contextual_supersession_reading, progressive_revelation_doctrine).
narrative_ontology:constraint_vindicates(herem_command_dt7__contextual_supersession_reading, new_covenant_supersession_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Teaches that herem was a time-and-place-bound command tied to the conquest period and is morally superseded by prophetic ethics (Isaiah's inclusion of foreigners, Jonah's Nineveh) and by the Christian new covenant. Administers catechesis, seminary curricula, and pastoral guidance that relocate the text's authority from binding law to historical artifact. Faces no material cost from this reading and gains coherence, ecumenical standing, and reduced apologetic burden.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, mainline_denominational_leadership, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(herem_command_dt7__contextual_supersession_reading, mainline_denominational_leadership, beneficiary).

% Marry or partner across ethnic or religious lines within traditions that trace descent from this text. Under the supersession reading, herem's intermarriage prohibitions are read as addressed to a specific historical threat (idolatrous assimilation in the conquest era) rather than as an ongoing ethnic bar, so their unions face no doctrinal challenge from this passage. Their situation improves directly as the reading displaces the durable-separation alternative.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, interfaith_families, beneficiary,
    moderate, biographical, mobile, regional).

% Enter the faith community from outside its ancestral ethnic lines. The supersession reading relocates the boundary marker from bloodline to confession and consent, so their membership is treated as fully legitimate rather than as a categorical exception requiring special justification.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, convert_believers, beneficiary,
    moderate, biographical, mobile, national).

% Live inside congregations or family systems that reject the supersession reading and still apply herem-adjacent separation logic to marriage, membership, or associational boundaries. The supersession reading exists in the wider tradition but has not displaced local literalist enforcement, so these individuals bear social sanction, family rupture, or expulsion threats regardless of the doctrine's majority theological status. They are the narrow residual victim class this reading itself identifies and seeks to delegitimate.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, believers_coerced_by_residual_literalist_enforcement, payer,
    powerless, biographical, constrained, local).

% Hold that herem encodes a durable mandate rather than a superseded historical directive and object that supersession is itself an unwarranted innovation that dissolves scriptural authority. Their objection is doctrinally live and organized but is treated by mainline institutional leadership as a minority holdout position rather than incorporated into curriculum or pastoral guidance.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, literalist_congregations, excluded,
    organized, generational, constrained, regional).

% Evaluate historical-critical, canonical, and redaction evidence for whether herem functions in the text as time-bound conquest instruction, as enduring law, or as literary/typological device. Their work is cited by all three kernel readings selectively and does not itself resolve the theological dispute.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, biblical_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a way for communities descended from a text containing a violent ethnic-cleansing command to retain the text as scripture while morally repudiating its plain-sense application, coordinating continued canonical authority with contemporary ethical commitments against ethnic violence and exclusion.
% TRANSFER_FUNCTION: Moves interpretive authority away from a plain-sense ethnic/genealogical reading of herem toward a historical-critical and covenantal reading; relocates the operative boundary condition for group membership from ancestry to belief or consent, and relocates moral responsibility for the conquest narrative onto a closed historical period rather than an ongoing command.
% ABSENT_VOICES: Descendants of peoples named as herem's historical targets (Canaanite-descent framings in some modern receptions) are not present as an organized party in the mainline reception debate; the dispute is conducted almost entirely among the text's inheritor traditions rather than including the perspective of the group the text describes as targeted. Literalist congregations are present but treated as a minority holdout rather than an equal party to the interpretive settlement.
% DISAPPEARANCE_RATIONALE: If the supersession reading vanished from mainline theology overnight, interfaith families and converts would lose an available doctrinal shield and would face renewed textual pressure toward exclusionary readings in traditions that still treat herem as authoritative; literalist congregations would feel vindicated rather than disrupted, since the reading's disappearance restores their preferred plain-sense frame. Whether 'the world rearranges' depends entirely on which party is asked, which is why this is contested rather than settled either way.
% FOUNDING_PROBLEM: Reconciling continued canonical authority of a text commanding total destruction of named populations (Deuteronomy 7, Deuteronomy 20, Joshua's conquest narratives) with post-exilic prophetic ethics of inclusion (Isaiah, Ruth, Jonah) and later covenantal theology that treats violence and ethnic boundary-marking as morally superseded.
% FOUNDING_PROBLEM_CORROBORATION: Historical-critical biblical scholars outside any confessional beneficiary group corroborate that herem functioned within an ancient Near Eastern conquest-ideology genre and that prophetic and post-exilic material shows a documented trajectory toward universalism, supporting the 'live-then-superseded' genealogy on textual-historical grounds independent of doctrinal interest. However, literalist traditions dispute that this trajectory constitutes moral supersession rather than progressive elaboration of a single unchanging mandate, so no consensus corroboration exists on whether supersession is the correct theological (as opposed to historical) conclusion.
narrative_ontology:disappearance_verdict(herem_command_dt7__contextual_supersession_reading, contested).
narrative_ontology:founding_problem_status(herem_command_dt7__contextual_supersession_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__contextual_supersession_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(herem_command_dt7__contextual_supersession_reading, 'none', 1).
narrative_ontology:epsilon_provenance(herem_command_dt7__contextual_supersession_reading, 0.22, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(herem_command_dt7__contextual_supersession_reading_tests).
:- end_tests(herem_command_dt7__contextual_supersession_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts moderate (0.55) reflecting the residual force of the plain-sense text before the supersession reading achieves institutional dominance, and falls to 0.22 as mainline denominational teaching increasingly relocates the command to a closed historical period. Theater ratio rises over the interval (0.20 to 0.40) because as the underlying extractive function fades, the doctrinal work required to explain WHY a text commanding ethnic destruction remains canonical scripture becomes increasingly a performative/apologetic exercise rather than a live moral-legal function — the hallmark of piton drift. Suppression (0.35) and accessibility_collapse (0.30) are both moderate-low: this reading does not foreclose alternative interpretations by force, but it does structurally marginalize literalist readings within mainline institutional space. Resistance (0.45) reflects the organized, ongoing objection from literalist congregations who treat supersession itself as an illegitimate move.
 *
 * DIRECTIONALITY LOGIC:
 *   Mainline denominational leadership and biblical-scholarly consensus set the terms of this reading and bear essentially no cost from adopting it — it resolves an apologetic problem in their favor. Interfaith families and converts are direct structural beneficiaries: the reading removes a textual basis for treating their unions or membership as illegitimate. The narrow victim class — believers under residual literalist enforcement — sits at high suppression despite the reading's overall low extraction, because for them the supersession reading has not actually displaced the operative local rule; their exit options are constrained by family and congregational embeddedness even though the wider tradition has moved on.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling conquest-era violence with the canon's claim to moral authority) is itself contested as live-vs-dead: biblical scholarship independently corroborates a textual-historical trajectory toward universalism, but literalist traditions deny this constitutes theological supersession rather than mere elaboration. Classifying this reading as piton rather than mountain or rope captures that the reading has become largely a stable, low-friction doctrinal settlement (declining extraction) maintained increasingly through apologetic/explanatory performance (rising theater_ratio) rather than through active enforcement against a contest that mainline institutions have mostly already won — the remaining function is administering a settled position, not adjudicating a live fight.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    supersession_vs_innovation,
    'Is ''moral supersession'' a legitimate theological development recognized within the tradition''s own historical self-understanding, or is it a modern ethical import read back onto the text to solve a problem the original authors did not intend to leave open?',
    'Comparative analysis of pre-modern reception history: do patristic, rabbinic, and medieval sources treat herem as time-bound and closed, or as an unresolved live command requiring ongoing moral reckoning? Convergent early evidence for a closed reading would support supersession as continuous with tradition rather than a modern innovation.',
    'If supersession is a modern innovation without deep pre-modern precedent, the reading''s institutional dominance rests more on contemporary ethical pressure than on doctrinal continuity, which would strengthen the durable_separation_reading''s claim that supersession is itself an unwarranted departure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supersession_vs_innovation, conceptual, 'Whether supersession is doctrinally continuous or a modern ethical retrofit.').

omega_variable(
    residual_enforcement_scope,
    'How large is the population of believers actually coerced by residual literalist enforcement of herem-adjacent separation logic, versus believers who hold literalist views without imposing coercive costs on others?',
    'Sociological survey data on congregational discipline practices tied to intermarriage or membership boundary enforcement in communities that reject supersession.',
    'A larger-than-assumed coerced population would mean this reading''s claimed ''narrow victim set'' understates ongoing harm; a smaller population would confirm the victim set is genuinely marginal relative to the reading''s broad institutional adoption.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(residual_enforcement_scope, empirical, 'Size of the population still harmed by non-adoption of this reading.').

omega_variable(
    canonical_retention_vs_repudiation,
    'Does retaining herem as scripture-with-superseded-application meaningfully differ, in its practical moral effect, from repudiating the text''s authority outright?',
    'Compare communities that formally excise or bracket herem passages from liturgical/catechetical use against communities that retain but reframe them; measure downstream attitudes toward outgroup violence and ethnic boundary-marking in each.',
    'If practical effects converge, the supersession reading''s continued canonical retention of a violent-conquest text is largely theater relative to outright repudiation, reinforcing the piton classification. If they diverge significantly, the retained-but-reframed status carries independent moral weight.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(canonical_retention_vs_repudiation, empirical, 'Whether reframing versus removing herem produces different real-world outcomes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__contextual_supersession_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(here_tr_t0, herem_command_dt7__contextual_supersession_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(here_tr_t40, herem_command_dt7__contextual_supersession_reading, theater_ratio, 40, 0.26).
narrative_ontology:measurement(here_tr_t80, herem_command_dt7__contextual_supersession_reading, theater_ratio, 80, 0.31).
narrative_ontology:measurement(here_tr_t120, herem_command_dt7__contextual_supersession_reading, theater_ratio, 120, 0.35).
narrative_ontology:measurement(here_tr_t160, herem_command_dt7__contextual_supersession_reading, theater_ratio, 160, 0.38).
narrative_ontology:measurement(here_tr_t200, herem_command_dt7__contextual_supersession_reading, theater_ratio, 200, 0.4).

% Extraction over time
narrative_ontology:measurement(here_be_t0, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(here_be_t40, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 40, 0.45).
narrative_ontology:measurement(here_be_t80, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 80, 0.35).
narrative_ontology:measurement(here_be_t120, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 120, 0.28).
narrative_ontology:measurement(here_be_t160, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 160, 0.24).
narrative_ontology:measurement(here_be_t200, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 200, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(herem_command_dt7__contextual_supersession_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(herem_command_dt7__contextual_supersession_reading, herem_command_dt7__durable_separation_reading).
narrative_ontology:affects_constraint(herem_command_dt7__contextual_supersession_reading, herem_command_dt7__allegorical_displacement_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the herem_command_dt7 kernel. contextual_supersession_reading (this story) claims the command was time-bound to Israel's conquest period and morally overridden by later revelation, yielding low extractiveness (0.22) and a narrow victim set limited to those under residual literalist enforcement. durable_separation_reading claims the command encodes a timeless mandate for bounded identity preservation and would author substantially higher extractiveness and a broader victim set (intermarried/outsider populations under active enforcement). allegorical_displacement_reading claims the 'nations' are typological placeholders for internal spiritual struggle rather than ethnic groups, which would author near-zero extractiveness against any literal ethnic population but raises its own distinct questions about whether this reading itself performs an erasure of the text's historical violence. The three stories share ε-invariance discipline: none averages across the others' claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
