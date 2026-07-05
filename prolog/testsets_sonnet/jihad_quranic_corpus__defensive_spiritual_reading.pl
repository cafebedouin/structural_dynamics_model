% ============================================================================
% CONSTRAINT STORY: jihad_quranic_corpus__defensive_spiritual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jihad_quranic_corpus__defensive_spiritual_reading, []).

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
 *   constraint_id: jihad_quranic_corpus__defensive_spiritual_reading
 *   human_readable: Jihad as Internal Struggle and Constrained Defensive Response (Defensive-Spiritual Reading)
 *   domain: religious/legal/political
 *
 * SUMMARY:
 *   This story instantiates one reading of the contested jihad_quranic_corpus
 *   kernel: jihad as primarily jihad al-nafs (internal spiritual struggle)
 *   with armed jihad permitted only as defensive response to aggression,
 *   gated by state authority, bounded by proportionality, and categorically
 *   excluding noncombatants. This is a low-extraction, low-suppression
 *   coordination structure precisely because its coordination function —
 *   restraining religiously-framed violence and protecting noncombatants — is
 *   not layered with asymmetric extraction from an identifiable victim class.
 *   The reading does not claim jurisdiction over non-Muslims except as
 *   potential aggressors, and it privileges coexistence frameworks
 *   (dhimma-style covenants, treaties) over perpetual conflict obligations.
 *   Two sibling readings of the SAME textual corpus —
 *   expansionist_legalist_reading (offensive campaigns to establish Islamic
 *   governance) and revolutionary_vanguard_reading (individual obligation
 *   bypassing state authority via takfir) — are NOT this constraint; they are
 *   separate stories with their own ε values, their own beneficiary/victim
 *   structures, and their own classifications. Do not average across them or
 *   treat this story as representing 'jihad' generically.
 *
 * KEY AGENTS:
 *   - ordinary_believers: primary beneficiary of the spiritual-primacy framing (moderate/constrained)
 *   - muslim_communities_seeking_coexistence: beneficiary relying on the reading for pluralist legitimacy (organized/mobile)
 *   - non_combatant_populations: beneficiary of the categorical immunity doctrine (powerless/trapped)
 *   - state_authorities_administering_just_war_doctrine: agenda-setter administering the authority-gate (institutional/constrained)
 *   - religious_minorities_under_muslim_rule: beneficiary of coexistence-privileging framework (powerless/constrained)
 *   - classical_and_contemporary_jurists: agenda-setter/observer constructing and defending the reading (institutional/analytical)
 *   - advocates_of_rival_readings: excluded contestants operating outside this reading's own frame (organized/mobile)
 *   - comparative_religion_scholars: analytical observer of interpretive competition (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__defensive_spiritual_reading, 0.18).
domain_priors:suppression_score(jihad_quranic_corpus__defensive_spiritual_reading, 0.22).
domain_priors:theater_ratio(jihad_quranic_corpus__defensive_spiritual_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__defensive_spiritual_reading, rope).
narrative_ontology:human_readable(jihad_quranic_corpus__defensive_spiritual_reading, "Jihad as Internal Struggle and Constrained Defensive Response (Defensive-Spiritual Reading)").
narrative_ontology:topic_domain(jihad_quranic_corpus__defensive_spiritual_reading, "religious/legal/political").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__defensive_spiritual_reading, '3bc5c8c2-1848-4256-8a8a-1f2f8334b28c').
narrative_ontology:cs_kernel_codification('3bc5c8c2-1848-4256-8a8a-1f2f8334b28c', fixed_text).
narrative_ontology:cs_authority_grounding('3bc5c8c2-1848-4256-8a8a-1f2f8334b28c', lineage).
narrative_ontology:cs_interpretation_layer_present('3bc5c8c2-1848-4256-8a8a-1f2f8334b28c').
narrative_ontology:cs_reading_relation('3bc5c8c2-1848-4256-8a8a-1f2f8334b28c', jihad_quranic_corpus__expansionist_legalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('3bc5c8c2-1848-4256-8a8a-1f2f8334b28c', jihad_quranic_corpus__revolutionary_vanguard_reading, forecloses).
narrative_ontology:cs_axiom('3bc5c8c2-1848-4256-8a8a-1f2f8334b28c', foundational, jihad_al_nafs_as_primary_referent).
narrative_ontology:cs_axiom_status(jihad_al_nafs_as_primary_referent, holdable).
narrative_ontology:cs_axiom_grounding('3bc5c8c2-1848-4256-8a8a-1f2f8334b28c', jihad_al_nafs_as_primary_referent, deontological).
narrative_ontology:cs_axiom('3bc5c8c2-1848-4256-8a8a-1f2f8334b28c', foundational, state_authority_required_for_licit_armed_jihad).
narrative_ontology:cs_axiom_status(state_authority_required_for_licit_armed_jihad, holdable).
narrative_ontology:cs_axiom_grounding('3bc5c8c2-1848-4256-8a8a-1f2f8334b28c', state_authority_required_for_licit_armed_jihad, conventional).
narrative_ontology:cs_axiom('3bc5c8c2-1848-4256-8a8a-1f2f8334b28c', secondary, noncombatant_immunity_categorically_binding).
narrative_ontology:cs_axiom_status(noncombatant_immunity_categorically_binding, holdable).
narrative_ontology:cs_axiom_grounding('3bc5c8c2-1848-4256-8a8a-1f2f8334b28c', noncombatant_immunity_categorically_binding, deontological).
narrative_ontology:cs_reference_frame('3bc5c8c2-1848-4256-8a8a-1f2f8334b28c', classical_defensive_just_war_framework).
narrative_ontology:cs_drift_state('3bc5c8c2-1848-4256-8a8a-1f2f8334b28c', post_colonial_and_post_9_11_contestation, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('3bc5c8c2-1848-4256-8a8a-1f2f8334b28c', '').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__defensive_spiritual_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, muslim_communities_seeking_coexistence).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, non_combatant_populations).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, state_authorities_administering_just_war_doctrine).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, religious_minorities_under_muslim_rule).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, ordinary_believers).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__defensive_spiritual_reading, primacy_of_inner_moral_struggle).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__defensive_spiritual_reading, proportionality_as_binding_norm).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__defensive_spiritual_reading, noncombatant_immunity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Practice jihad al-nafs — struggle against ego, greed, anger, spiritual laxity — as the primary meaning of the term in daily religious life. Draw on this reading to understand their obligations as personal moral discipline rather than military mobilization. Can leave interpretive communities that read jihad expansively, though social cost of doing so varies by context.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, ordinary_believers, beneficiary,
    moderate, biographical, constrained, global).

% Live as religious or political minorities, or in pluralist states, and rely on the defensive-spiritual reading to ground claims that Islam does not obligate perpetual conflict with non-Muslim neighbors or states. This reading underwrites treaties, interfaith cooperation, and legal coexistence frameworks they depend on for security and standing.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, muslim_communities_seeking_coexistence, beneficiary,
    organized, generational, mobile, global).

% Civilians in or near zones of armed conflict involving Muslim-majority states or movements. Under this reading, they fall categorically outside legitimate targeting regardless of religious identity, because noncombatant immunity is doctrinally binding rather than a discretionary tactical preference. Their situation improves precisely to the extent this reading holds jurisprudential ground against readings that loosen the immunity standard.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, non_combatant_populations, beneficiary,
    powerless, immediate, trapped, regional).

% Legitimate governments and their religious-legal institutions (ministries of religious affairs, state muftis, military chaplaincies) that hold the authority the reading requires before armed jihad can be declared. They administer the high threshold for declaration, adjudicate proportionality, and bear responsibility for restraining unauthorized armed action claimed in jihad's name. Their institutional legitimacy is itself partly constituted by this reading — without state-authority gating, non-state actors could bypass them entirely.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, state_authorities_administering_just_war_doctrine, agenda_setter,
    institutional, generational, constrained, national).

% Historically and currently, non-Muslim populations living under Muslim political authority. Under this reading they are outside the scope of legitimate jihad unless they commit aggression, and covenant/dhimma-style coexistence frameworks are privileged over expansionist obligation. Their protection is contingent on this reading's continued authority within the interpretive tradition, not on any independent guarantee.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, religious_minorities_under_muslim_rule, beneficiary,
    powerless, generational, constrained, regional).

% Scholars across schools of law who articulate and transmit the conditions under which armed jihad is defensive, proportionate, and authority-gated. They construct and defend this reading against sibling readings within the same textual corpus, citing hadith on noncombatant protection and the classical requirement of imam authorization. Their scholarly authority is partly what stabilizes the reading against rival claimants.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, classical_and_contemporary_jurists, agenda_setter,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__defensive_spiritual_reading, classical_and_contemporary_jurists, observer).

% Proponents of the expansionist-legalist and revolutionary-vanguard readings of the same corpus contest this reading's textual and historical claims, arguing it understates jihad's expansionist or emergency-mobilizational dimensions. They are not victims of this constraint — they are rival interpretive communities operating outside this reading's boundaries, present in the broader discourse but not addressed within this reading's own internal logic.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, advocates_of_rival_readings, excluded,
    organized, generational, mobile, global).

% Study how the three readings compete for authority within the same textual corpus and how each reading's dominance in a given community shapes real-world political and military outcomes. Take no side but document the structural stakes of which reading holds interpretive ground.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jihad_quranic_corpus__defensive_spiritual_reading, diffuse).
narrative_ontology:fixing_cost_class(jihad_quranic_corpus__defensive_spiritual_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared normative framework restraining the use of religiously-framed violence: it establishes that legitimate armed jihad requires state authority, is defensive in trigger, is bounded by proportionality, and categorically excludes noncombatants — coordinating expectations among believers, jurists, and states about when force is licit.
% TRANSFER_FUNCTION: Moves interpretive authority toward state institutions and established jurisprudential bodies and away from individuals or non-state actors claiming unilateral authorization; moves protective status toward non-combatants and non-aggressor populations who would otherwise be exposed under looser readings.
% ABSENT_VOICES: Historical minority-status communities whose long-term security depended on this reading prevailing are rarely direct parties to the interpretive contest; contemporary victims of violence committed under rival readings (expansionist or vanguardist) are also outside this reading's own frame, since this reading's field of application excludes them by construction rather than by adjudicated fact.
% DISAPPEARANCE_RATIONALE: If this reading lost interpretive ground entirely, the normative floor restraining state-authorized proportionate defensive force would weaken; rival readings with lower thresholds for declaration and looser noncombatant protections would face less doctrinal resistance, materially changing which acts of violence could claim religious legitimacy within affected communities and states.
% FOUNDING_PROBLEM: Early Muslim community faced existential military threats (Meccan persecution, subsequent wars) requiring a doctrine of legitimate defensive force, while parallel Quranic and hadith material emphasized internal moral struggle as jihad's primary register — the founding problem was reconciling communal survival with restraint against unlimited violence and against conflating personal piety with holy war.
% FOUNDING_PROBLEM_CORROBORATION: Classical jurists across multiple schools (Hanafi, Shafi'i, Maliki, Hanbali) independently converged on authority-gating and noncombatant-immunity requirements, suggesting cross-school corroboration rather than a single interested party's construction. Contemporary human rights scholars and comparative religion scholars outside the Muslim jurisprudential tradition corroborate that the defensive-spiritual reading correlates historically with lower rates of religiously-framed violence where it holds institutional dominance, though this correlation is itself contested by proponents of rival readings who dispute the causal story.
narrative_ontology:disappearance_verdict(jihad_quranic_corpus__defensive_spiritual_reading, world_rearranges).
narrative_ontology:founding_problem_status(jihad_quranic_corpus__defensive_spiritual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__defensive_spiritual_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jihad_quranic_corpus__defensive_spiritual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jihad_quranic_corpus__defensive_spiritual_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jihad_quranic_corpus__defensive_spiritual_reading_tests).
:- end_tests(jihad_quranic_corpus__defensive_spiritual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.18, rising marginally over the interval) because this reading's coordination function is not shadowed by a corresponding extraction from an identifiable victim group — there are no declared victims. Suppression is authored moderate-low (0.22) reflecting the real but limited coercive apparatus involved in enforcing state-authority gating and proportionality norms against unauthorized armed action, not suppression of dissenting believers' personal practice. Theater ratio is low (0.15) because the doctrinal apparatus (jurisprudential conditions, authority requirements) performs genuine restraining work rather than empty ritual. Accessibility collapse is moderate (0.35) — alternative interpretations remain visible and contested (per the omega on kernel contestation) rather than fully foreclosed, which is itself the honest state of a live, disputed textual tradition. Resistance is moderate-low (0.3), coming primarily from rival interpretive communities rather than from within this reading's own constituency.
 *
 * DIRECTIONALITY LOGIC:
 *   Ordinary believers, coexistence-seeking communities, noncombatants, and religious minorities are beneficiaries because the reading's operation subsidizes their security and moral standing rather than extracting from them — d sits near the beneficiary end for all of them. State authorities and jurists are agenda-setters whose institutional legitimacy is partly constituted by the reading, giving them a stake close to but not identical with pure beneficiary status (they administer costs of restraint, e.g. political costs of refusing to authorize popular military mobilization). No victim group is declared because this reading's structural claim is specifically that it does NOT extract from non-aggressor non-Muslims or from noncombatants — that is the delta this reading asserts against its expansionist and vanguardist siblings.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling communal survival with restraint against unlimited violence) remains contested rather than dead: state-authorized defensive force doctrines continue to be invoked in genuinely defensive contexts, and the internal-struggle framing continues to organize the devotional lives of the majority of practicing Muslims independent of any military context at all. This is not mandatrophy — the mandate has not outlived its function; rather, the reading persists because its coordination function (restraining religiously-framed violence, protecting noncombatants, privileging coexistence) remains actively contested by rival readings that would remove those restraints, making its continued articulation and defense by jurists a live rather than vestigial project.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_dominance_contingency,
    'Which reading of the jihad_quranic_corpus kernel holds institutional and interpretive dominance in a given community at a given time, and what determines that dominance — textual argument, political power, colonial disruption, state sponsorship, or some combination?',
    'Comparative historical and sociological analysis of which reading held authority across different Muslim-majority polities and eras, cross-referenced against documented rates of religiously-framed political violence and treaty-based coexistence arrangements.',
    'If dominance is primarily a function of political power and state sponsorship rather than textual-jurisprudential argument, then this reading''s low-extraction profile is partly contingent on external political conditions rather than being an intrinsic textual property — meaning its persistence is more fragile and more dependent on continued state-authority backing than the internal jurisprudential argument alone would suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_dominance_contingency, conceptual, 'Whether this reading''s dominance in any given time/place is textually or politically determined.').

omega_variable(
    accessibility_collapse_across_readings,
    'Given that this reading coexists with two structurally distinct sibling readings of the same corpus, how completely does adopting this reading foreclose the alternatives for a given interpretive community — is accessibility_collapse better modeled as low (readings genuinely compete) or does dominant institutional backing in some contexts push it much higher?',
    'Track whether communities that formally adopt this reading (via state fatwa councils, established madrasah curricula) subsequently see meaningful uptake of rival readings, or whether institutional adoption effectively forecloses them in practice.',
    'A finding of near-total foreclosure in institutionally-dominant contexts would justify raising accessibility_collapse substantially above the currently authored 0.35, while a finding of genuine persistent contestation supports the current moderate value.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(accessibility_collapse_across_readings, empirical, 'Whether this reading''s practical dominance in specific contexts produces higher foreclosure of sibling readings than the corpus-level contest suggests.').

omega_variable(
    state_authority_gate_capture_risk,
    'Does the requirement that legitimate armed jihad requires state authority create a structural opening for authoritarian states to capture and weaponize the doctrine — declaring ''defensive'' wars that are not genuinely defensive while relying on the doctrine''s legitimacy?',
    'Case analysis of historical instances where state actors invoked this reading''s authority-gated defensive framework to legitimate wars later judged, by independent historical assessment, to be non-defensive or disproportionate.',
    'If capture is common, part of the reading''s low extractiveness score understates a real extraction risk that operates through state instrumentalization rather than through the doctrine''s own textual content — this would argue for a higher suppression or extractiveness value in state-instrumentalized instances, potentially warranting a separate downstream constraint story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_authority_gate_capture_risk, empirical, 'Whether state-authority gating is itself exploitable by authoritarian actors to launder non-defensive wars as legitimate jihad.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__defensive_spiritual_reading, 610, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jiha_tr_t610, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 610, 0.1).
narrative_ontology:measurement_basis(jiha_tr_t610, observed).
narrative_ontology:measurement(jiha_tr_t900, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 900, 0.11).
narrative_ontology:measurement_basis(jiha_tr_t900, observed).
narrative_ontology:measurement(jiha_tr_t1300, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 1300, 0.12).
narrative_ontology:measurement_basis(jiha_tr_t1300, observed).
narrative_ontology:measurement(jiha_tr_t1800, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 1800, 0.13).
narrative_ontology:measurement_basis(jiha_tr_t1800, observed).
narrative_ontology:measurement(jiha_tr_t1950, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 1950, 0.14).
narrative_ontology:measurement_basis(jiha_tr_t1950, observed).
narrative_ontology:measurement(jiha_tr_t2026, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 2026, 0.15).
narrative_ontology:measurement_basis(jiha_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(jiha_be_t610, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 610, 0.12).
narrative_ontology:measurement_basis(jiha_be_t610, observed).
narrative_ontology:measurement(jiha_be_t900, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 900, 0.14).
narrative_ontology:measurement_basis(jiha_be_t900, observed).
narrative_ontology:measurement(jiha_be_t1300, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 1300, 0.15).
narrative_ontology:measurement_basis(jiha_be_t1300, observed).
narrative_ontology:measurement(jiha_be_t1800, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 1800, 0.16).
narrative_ontology:measurement_basis(jiha_be_t1800, observed).
narrative_ontology:measurement(jiha_be_t1950, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 1950, 0.17).
narrative_ontology:measurement_basis(jiha_be_t1950, observed).
narrative_ontology:measurement(jiha_be_t2026, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 2026, 0.18).
narrative_ontology:measurement_basis(jiha_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(jiha_su_t610, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 610, 0.2).
narrative_ontology:measurement_basis(jiha_su_t610, observed).
narrative_ontology:measurement(jiha_su_t900, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 900, 0.2).
narrative_ontology:measurement_basis(jiha_su_t900, observed).
narrative_ontology:measurement(jiha_su_t1300, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 1300, 0.21).
narrative_ontology:measurement_basis(jiha_su_t1300, observed).
narrative_ontology:measurement(jiha_su_t1800, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 1800, 0.21).
narrative_ontology:measurement_basis(jiha_su_t1800, observed).
narrative_ontology:measurement(jiha_su_t1950, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 1950, 0.22).
narrative_ontology:measurement_basis(jiha_su_t1950, observed).
narrative_ontology:measurement(jiha_su_t2026, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 2026, 0.22).
narrative_ontology:measurement_basis(jiha_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jihad_quranic_corpus__defensive_spiritual_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jihad_quranic_corpus__defensive_spiritual_reading, 0.08).
narrative_ontology:affects_constraint(jihad_quranic_corpus__defensive_spiritual_reading, jihad_quranic_corpus__expansionist_legalist_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__defensive_spiritual_reading, jihad_quranic_corpus__revolutionary_vanguard_reading).

% DUAL FORMULATION NOTE:
% This story, jihad_quranic_corpus__expansionist_legalist_reading, and jihad_quranic_corpus__revolutionary_vanguard_reading form a constraint family sharing the jihad_quranic_corpus kernel. Each reading of the same textual/jurisprudential corpus produces a structurally distinct constraint: this reading (defensive-spiritual) shows no declared victims and low extraction; the expansionist-legalist reading is expected to show a different beneficiary/victim structure tied to populations under conditioned offensive campaigns; the revolutionary-vanguard reading is expected to show the highest extraction and suppression, given its bypass of state authority and use of takfir against both apostate rulers and, in practice, broader populations. Per the ε-invariance principle, these are three separate files, each with its own ε, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
