% ============================================================================
% CONSTRAINT STORY: jihad_quranic_corpus__defensive_spiritual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: jihad_quranic_corpus__defensive_spiritual_reading
 *   human_readable: Jihad as Internal Struggle and Constrained Defensive Response (Classical Coexistence Reading)
 *   domain: religious/legal/political
 *
 * SUMMARY:
 *   This story authors one reading of a contested kernel — the Quranic and
 *   juristic corpus on jihad — that treats the concept as primarily jihad
 *   al-nafs (internal moral struggle) and, in its armed dimension, as a
 *   defensively-triggered, state-authorized, proportionality-bound response
 *   to aggression that categorically excludes non-combatants and treats
 *   coexistence with non-Muslim populations as the default rather than the
 *   exception. This is NOT a claim that the sibling readings
 *   (expansionist-legalist, revolutionary-vanguard) are false; it is a
 *   structurally distinct constraint with its own beneficiary/victim set, its
 *   own ε, and its own classification, per the ε-invariance principle. The
 *   three readings are linked in network.affects_constraints and are not
 *   merged here.
 *
 * KEY AGENTS:
 *   - muslim_polity_under_threat: institutional beneficiary authorized to invoke bounded defensive force
 *   - individual_believers_seeking_moral_discipline: powerless beneficiaries of the internal-struggle primary meaning
 *   - non_combatant_populations: powerless beneficiaries whose protection depends on doctrine being honored
 *   - religious_minorities_under_muslim_governance: moderate-power beneficiaries of the coexistence framework
 *   - aggressor_forces_engaged_in_armed_conflict: organized payers, but only after triggering aggression
 *   - classical_jurists_and_scholarly_authorities: institutional agenda-setters administering the interpretive conditions
 *   - expansionist_and_vanguard_claimants: excluded rival readers of the same corpus
 *   - state_authorities_authorizing_force: institutional agenda-setters holding exclusive authorization power
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
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__defensive_spiritual_reading, rope).
narrative_ontology:human_readable(jihad_quranic_corpus__defensive_spiritual_reading, "Jihad as Internal Struggle and Constrained Defensive Response (Classical Coexistence Reading)").
narrative_ontology:topic_domain(jihad_quranic_corpus__defensive_spiritual_reading, "religious/legal/political").

domain_priors:requires_active_enforcement(jihad_quranic_corpus__defensive_spiritual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__defensive_spiritual_reading, '98a2cff0-1faf-45ae-ba38-1f059fe72467').
narrative_ontology:cs_kernel_codification('98a2cff0-1faf-45ae-ba38-1f059fe72467', fixed_text).
narrative_ontology:cs_authority_grounding('98a2cff0-1faf-45ae-ba38-1f059fe72467', lineage).
narrative_ontology:cs_interpretation_layer_present('98a2cff0-1faf-45ae-ba38-1f059fe72467').
narrative_ontology:cs_reading_relation('98a2cff0-1faf-45ae-ba38-1f059fe72467', jihad_quranic_corpus__expansionist_legalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('98a2cff0-1faf-45ae-ba38-1f059fe72467', jihad_quranic_corpus__revolutionary_vanguard_reading, forecloses).
narrative_ontology:cs_axiom('98a2cff0-1faf-45ae-ba38-1f059fe72467', foundational, armed_jihad_requires_prior_aggression).
narrative_ontology:cs_axiom_status(armed_jihad_requires_prior_aggression, holdable).
narrative_ontology:cs_axiom_grounding('98a2cff0-1faf-45ae-ba38-1f059fe72467', armed_jihad_requires_prior_aggression, deontological).
narrative_ontology:cs_axiom('98a2cff0-1faf-45ae-ba38-1f059fe72467', foundational, armed_jihad_requires_state_authorization).
narrative_ontology:cs_axiom_status(armed_jihad_requires_state_authorization, holdable).
narrative_ontology:cs_axiom_grounding('98a2cff0-1faf-45ae-ba38-1f059fe72467', armed_jihad_requires_state_authorization, conventional).
narrative_ontology:cs_axiom('98a2cff0-1faf-45ae-ba38-1f059fe72467', secondary, non_combatants_categorically_immune).
narrative_ontology:cs_axiom_status(non_combatants_categorically_immune, holdable).
narrative_ontology:cs_axiom_grounding('98a2cff0-1faf-45ae-ba38-1f059fe72467', non_combatants_categorically_immune, deontological).
narrative_ontology:cs_reference_frame('98a2cff0-1faf-45ae-ba38-1f059fe72467', classical_defensive_juristic_consensus).
narrative_ontology:cs_drift_state('98a2cff0-1faf-45ae-ba38-1f059fe72467', post_colonial_and_post_9_11_contestation, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('98a2cff0-1faf-45ae-ba38-1f059fe72467', '').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__defensive_spiritual_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, muslim_polity_under_threat).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, non_combatant_populations).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, individual_believers_seeking_moral_discipline).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, religious_minorities_under_muslim_governance).
narrative_ontology:constraint_victim(jihad_quranic_corpus__defensive_spiritual_reading, aggressor_forces_engaged_in_armed_conflict).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, state_authorities_authorizing_force).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__defensive_spiritual_reading, proportionality_doctrine).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__defensive_spiritual_reading, non_combatant_immunity_doctrine).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__defensive_spiritual_reading, state_authority_requirement_for_armed_jihad).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A recognized Muslim political authority facing external aggression. Under this reading, it may authorize defensive armed jihad only after aggression has occurred, only through legitimate state command (imam or equivalent authority), and only within limits of proportionality. The framework gives it a bounded, juridically disciplined tool rather than an open mandate for conquest.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, muslim_polity_under_threat, beneficiary,
    institutional, generational, constrained, regional).

% Ordinary Muslims for whom jihad al-nafs (struggle against the self) is the primary, everyday meaning of the term — resisting sin, cultivating discipline, pursuing ethical improvement. They are not conscripted into armed conflict by this reading and retain the coordination benefit of a moral vocabulary without a martial obligation attached to daily life.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, individual_believers_seeking_moral_discipline, beneficiary,
    powerless, biographical, mobile, local).

% Civilians on any side of an armed conflict conducted under this reading's rules. The non-combatant immunity doctrine is meant to categorically exclude them from legitimate targeting. Their situation is that of people whose protection depends entirely on the belligerent actually honoring the doctrine rather than invoking it as cover.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, non_combatant_populations, beneficiary,
    powerless, immediate, trapped, regional).

% Non-Muslim communities living under Muslim political authority. Under this reading they are outside the scope of legitimate jihad entirely unless they become active aggressors — the coexistence framework privileges treaty relations, protected-minority status, and non-aggression over conquest or forced conversion. Their exit from persecution risk depends on rulers actually adhering to this reading rather than the expansionist or vanguard alternatives.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, religious_minorities_under_muslim_governance, beneficiary,
    moderate, generational, constrained, regional).

% Combatants who have initiated aggression and against whom a proportionate defensive response is authorized. They bear the cost of the armed component of this reading, but only after crossing the aggression threshold, and even then only combatants (not their civilian populations) are legitimate targets under the doctrine's own terms.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, aggressor_forces_engaged_in_armed_conflict, payer,
    organized, immediate, mobile, regional).

% The scholarly tradition (fuqaha) that articulates and transmits the conditions under which armed jihad is licit — state authorization, proportionality, non-combatant immunity, exhaustion of alternatives. They administer the interpretive apparatus that determines whether a given use of force qualifies as legitimate defensive jihad or falls outside the doctrine altogether.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, classical_jurists_and_scholarly_authorities, agenda_setter,
    institutional, civilizational, analytical, global).

% Actors and movements who read the same textual corpus as licensing offensive campaigns to establish Islamic governance, or as authorizing individual believers to bypass state authority via takfir against rulers deemed apostate. From this reading's vantage they are misreading or overriding the doctrinal constraints; they are not part of the coordination this reading describes, though they contest its authority to exclude them.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, expansionist_and_vanguard_claimants, excluded,
    organized, immediate, mobile, global).

% Political rulers or governing bodies who hold the exclusive authority under this reading to declare and direct armed jihad. This concentrates legitimate martial authorization in the state and denies it to individuals or sub-state actors, which both empowers state authorities and constrains them to the doctrine's proportionality and threshold requirements.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, state_authorities_authorizing_force, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__defensive_spiritual_reading, state_authorities_authorizing_force, beneficiary).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared ethical-legal vocabulary that (a) channels most of the concept's everyday use toward internal moral struggle rather than violence, and (b) where armed conflict does occur, disciplines it with authorization, proportionality, and non-combatant immunity requirements — reducing the scope and brutality of conflict relative to unconstrained warfare and preventing private or vigilante declarations of holy war.
% TRANSFER_FUNCTION: Under peaceful conditions, moves nothing materially — it structures individual moral effort. Under conflict conditions, it authorizes a proportionate transfer of harm from a defending polity onto an aggressor's combatant forces, while withholding that authorization from targeting the aggressor's civilians or from initiating unprovoked force.
% ABSENT_VOICES: Adherents of the expansionist-legalist and revolutionary-vanguard readings are excluded from this reading's own account of what jihad licenses — they would object that the defensive-only, state-authorized framing under-reads the classical juristic permission for offensive campaigns and over-reads modern liberal sensibilities into the doctrine. Non-Muslim polities historically on the receiving end of jihad campaigns conducted under sibling readings are also absent from this reading's victim set, since this reading's own scope excludes them unless they are aggressors — a scope decision the sibling readings would dispute.
% DISAPPEARANCE_RATIONALE: If this specific reading vanished as a live interpretive position, the underlying textual corpus and juristic tradition would remain, but the coexistence-privileging, defensively-bounded understanding that currently anchors most mainstream and state-aligned Muslim religious authority would lose its most articulate institutional voice. Proponents say the world would rearrange toward greater vulnerability to expansionist and vanguard readings filling the vacuum; critics of this reading say the underlying restraint is more robust than any one interpretive school and would persist through other channels — hence contested rather than a clean verdict either way.
% FOUNDING_PROBLEM: Early Muslim community facing military threat and persecution needed doctrine distinguishing licit self-defense and moral struggle from unrestrained warfare, while a competing concern (state monopoly on legitimate force, protection of the vulnerable, control of freelance violence) needed the same doctrine to withhold license from private actors and untriggered aggression.
% FOUNDING_PROBLEM_CORROBORATION: Mainstream classical and contemporary jurists across multiple schools attest this reading's threshold and authorization requirements as continuous with founding-era practice, and comparative historians of early Islamic warfare (a source outside the tradition's own beneficiary set) corroborate that early campaigns were frequently defensive-triggered and subject to explicit non-combatant restrictions. Scholars aligned with the expansionist-legalist and revolutionary-vanguard readings dispute that the founding problem was ever primarily defensive, arguing the restrictive reading is a later, politically convenient narrowing — so the status of the founding problem itself remains contested across the kernel's readings, not resolved by this reading alone.
narrative_ontology:disappearance_verdict(jihad_quranic_corpus__defensive_spiritual_reading, contested).
narrative_ontology:founding_problem_status(jihad_quranic_corpus__defensive_spiritual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__defensive_spiritual_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored low (0.18) because this reading's own structure withholds legitimate force from anyone except in response to prior aggression, and even then confines it to combatants under proportionality limits — the doctrine's design goal is to minimize, not maximize, extraction. Suppression is authored moderate-low (0.22): the doctrine does constrain individual believers from freelancing armed jihad (a real suppressive function reserving authorization to the state), but this is coordination-shaped suppression, not extraction-shaped. Theater ratio is low (0.15) reflecting that, on this reading's own terms, the proportionality and non-combatant-immunity requirements are treated as substantively binding rather than performative — though the story acknowledges via omega that in practice compliance varies. Accessibility collapse is moderate (0.35): the internal-struggle meaning is nearly universally available to believers (low collapse there), but the specific juristic conditions for licit armed jihad are technical and contested, so alternative readings of the same corpus remain very much alive — this is not a mountain-like collapse. Resistance is moderate (0.4): both secular critics who doubt any version of jihad doctrine can reliably restrain violence, and rival Muslim readings who reject the restrictive framing, actively contest this reading's claim to represent the tradition.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-Muslim populations are placed outside the victim set by this reading's own structural design, appearing only as potential victims if they become aggressors — this is the central structural delta from the sibling readings, where non-Muslims or apostate rulers are structurally within scope regardless of triggering conduct. Muslim believers, non-combatants generally, and religious minorities under Muslim governance are beneficiaries because the doctrine's coordination function (restraint, threshold requirements, protected-minority status) operates in their favor. Aggressor combatants are the sole payer group, and only conditionally — the directionality here is triggered by their own conduct rather than assigned by status, which is itself a claim this reading makes about proportionate justice.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (distinguishing licit self-defense/moral struggle from unrestrained warfare, and preventing private declarations of holy war) is authored as contested rather than resolved or dead: some jurists and historians hold it remains fully live (state monopoly on force and civilian protection are perennial concerns), while critics from sibling readings hold that the 'defensive-only' framing is itself a later narrowing not present in the earliest juristic layer. This story does not adjudicate that contest — it authors this reading's own account and flags the contest via the founding_problem_status and the omega on textual/juristic indeterminacy, avoiding the mislabeling risk of treating either 'jihad is purely coordination/restraint' or 'jihad is purely conquest' as settled fact.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'Does the classical and Quranic textual-juristic corpus more accurately support the defensive-spiritual reading (bounded, state-authorized, coexistence-privileging) or one of the sibling readings (expansionist-legalist offensive permission; revolutionary-vanguard individual obligation bypassing state authority)?',
    'No single empirical resolution exists — this is a live interpretive contest within Islamic jurisprudence spanning centuries of scholarly disagreement across schools (madhabib), historical periods, and political contexts. Comparative textual-historical scholarship on early Islamic military campaigns, classical fiqh treatises across schools, and contemporary juristic consensus-formation processes are the closest approximations to resolution, but the kernel itself may be genuinely under-determined by the corpus.',
    'If the defensive-spiritual reading is judged the more textually/historically grounded reading, this constraint''s low-extraction, coordination-dominant classification is well-founded. If a sibling reading is judged more accurate to the corpus''s original scope, this reading''s exclusion of non-aggressor non-Muslims from the victim set would be revealed as a narrowing not present in the source material — though this would not change THIS story''s own ε, since ε is authored per-reading, not per-topic.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the defensive-spiritual reading or a sibling reading is the more textually/historically grounded interpretation of the jihad kernel.').

omega_variable(
    state_practice_compliance_gap,
    'In actual historical and contemporary practice by states and movements claiming this reading, is the proportionality and non-combatant-immunity doctrine substantively honored, or does it function as post-hoc justification for conduct that would fail the doctrine''s own tests?',
    'Comparative case analysis of specific conflicts where this reading was invoked, cross-checked against independent (non-belligerent) casualty and targeting data; assessment of whether declared adherence to proportionality correlates with observed combatant/non-combatant targeting ratios.',
    'If compliance is high, the low theater_ratio and low extractiveness scores are vindicated as descriptively accurate. If compliance is systematically low, this reading functions partly as legitimating cover for conduct the doctrine formally prohibits, which would push the theater_ratio and extractiveness scores upward and warrant re-examination of the claimed_type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_practice_compliance_gap, empirical, 'Whether real-world invocation of this reading matches its own proportionality and civilian-protection standards.').

omega_variable(
    coexistence_framework_scope_ambiguity,
    'Does the coexistence framework this reading privileges genuinely extend equal protection to religious minorities under Muslim governance across all historical periods, or does protected-minority (dhimmi-type) status carry its own asymmetric burdens that complicate the ''beneficiary'' classification for that group?',
    'Historical and legal analysis of protected-minority status across different Muslim polities and periods — examining tax burdens, legal standing, and social restrictions alongside the protections claimed.',
    'If protected-minority status historically carried substantial asymmetric burdens, religious_minorities_under_muslim_governance may be better modeled with a dual beneficiary/payer role rather than pure beneficiary, which would be a data point for a possible sibling story on that specific arrangement rather than a change to this constraint''s own ε.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coexistence_framework_scope_ambiguity, empirical, 'Whether protected-minority status under this reading''s coexistence framework is unambiguously beneficial or carries offsetting costs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__defensive_spiritual_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jiha_tr_t0, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(jiha_tr_t20, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 20, 0.11).
narrative_ontology:measurement(jiha_tr_t40, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement(jiha_tr_t60, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 60, 0.13).
narrative_ontology:measurement(jiha_tr_t80, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 80, 0.14).
narrative_ontology:measurement(jiha_tr_t100, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(jiha_be_t0, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(jiha_be_t20, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 20, 0.16).
narrative_ontology:measurement(jiha_be_t40, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 40, 0.17).
narrative_ontology:measurement(jiha_be_t60, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 60, 0.17).
narrative_ontology:measurement(jiha_be_t80, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 80, 0.18).
narrative_ontology:measurement(jiha_be_t100, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 100, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(jiha_su_t0, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(jiha_su_t20, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 20, 0.2).
narrative_ontology:measurement(jiha_su_t40, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 40, 0.21).
narrative_ontology:measurement(jiha_su_t60, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 60, 0.21).
narrative_ontology:measurement(jiha_su_t80, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 80, 0.22).
narrative_ontology:measurement(jiha_su_t100, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 100, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jihad_quranic_corpus__defensive_spiritual_reading, identity_coordination).
narrative_ontology:affects_constraint(jihad_quranic_corpus__defensive_spiritual_reading, jihad_quranic_corpus__expansionist_legalist_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__defensive_spiritual_reading, jihad_quranic_corpus__revolutionary_vanguard_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked constraints decomposing the natural-language label 'jihad' per the ε-invariance principle. The defensive_spiritual_reading (this file) authors low extraction and a rope-leaning classification, restricting the victim set to triggering aggressors. The expansionist_legalist_reading authors a broader offensive license with non-Muslim polities generally in scope. The revolutionary_vanguard_reading authors an individual-obligation framework that bypasses state authority via takfir, producing a distinct victim set (apostate rulers, occupiers) and a distinct enforcement structure (individual/vanguard rather than state). Each story carries its own ε, beneficiaries, victims, and claimed_type; none averages over or is derived from the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
