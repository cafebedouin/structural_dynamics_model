% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__performance_only, []).

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
 *   constraint_id: sacrifice_obligation_continuity__performance_only
 *   human_readable: Sacrifice Obligation Requires Physical Performance (Performance-Only Reading)
 *   domain: religious_law/ritual_studies/textual_tradition
 *
 * SUMMARY:
 *   This story instantiates the performance_only reading of the
 *   sacrifice-obligation-continuity kernel: physical sacrificial performance
 *   is the only act that discharges the biblical commandment, and study of
 *   the sacrificial laws — however rigorous — is preparatory scaffolding for
 *   a future restored cult, not fulfillment in itself. Under this reading the
 *   obligation remains fully binding and fully unperformable for the current
 *   generation, producing an open, structurally unfulfillable duty. This is a
 *   distinct constraint from its siblings: study_as_performance treats
 *   textual engagement as itself satisfying the commandment (near-zero
 *   extraction, obligation discharged continuously); messianic_suspension
 *   treats the obligation as formally suspended pending restoration (low
 *   extraction, no guilt attaches to non-performance); archival_preservation
 *   treats the law as non-binding entirely (near-zero extraction, no
 *   obligation at all). Only performance_only produces the high-extraction
 *   profile: guilt without remedy, borne by a population with no legal path
 *   to compliance.
 *
 * KEY AGENTS:
 *   - current_generation_adherents: Primary target (powerless/trapped) — bear an unfulfillable obligation and its accompanying guilt
 *   - temple_restoration_movements: Primary agenda-setter (organized/constrained) — administers and sustains the performance-only reading
 *   - priestly_lineage_claimants: Primary beneficiary (moderate/constrained) — retains exclusive future ritual authority under this reading
 *   - textual_scholars: Secondary payer/beneficiary (moderate/constrained) — labor demoted to preparatory status rather than fulfillment
 *   - rival_interpretive_authorities: Excluded voice (organized/constrained) — hold competing readings but lack standing within performance-only communities
 *   - halakhic_courts_and_academies: Analytical observer (institutional/analytical) — adjudicates which reading governs practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__performance_only, 0.71).
domain_priors:suppression_score(sacrifice_obligation_continuity__performance_only, 0.58).
domain_priors:theater_ratio(sacrifice_obligation_continuity__performance_only, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, extractiveness, 0.71).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__performance_only, tangled_rope).
narrative_ontology:human_readable(sacrifice_obligation_continuity__performance_only, "Sacrifice Obligation Requires Physical Performance (Performance-Only Reading)").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__performance_only, "religious_law/ritual_studies/textual_tradition").

domain_priors:requires_active_enforcement(sacrifice_obligation_continuity__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__performance_only, '3ece6764-9a07-4500-a8b4-4c3d8f775881').
narrative_ontology:cs_kernel_codification('3ece6764-9a07-4500-a8b4-4c3d8f775881', fixed_text).
narrative_ontology:cs_authority_grounding('3ece6764-9a07-4500-a8b4-4c3d8f775881', lineage).
narrative_ontology:cs_interpretation_layer_present('3ece6764-9a07-4500-a8b4-4c3d8f775881').
narrative_ontology:cs_reading_relation('3ece6764-9a07-4500-a8b4-4c3d8f775881', sacrifice_obligation_continuity__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('3ece6764-9a07-4500-a8b4-4c3d8f775881', sacrifice_obligation_continuity__messianic_suspension, coexists_with).
narrative_ontology:cs_reading_relation('3ece6764-9a07-4500-a8b4-4c3d8f775881', sacrifice_obligation_continuity__archival_preservation, influences).
narrative_ontology:cs_axiom('3ece6764-9a07-4500-a8b4-4c3d8f775881', foundational, physical_performance_is_sole_discharge_mechanism).
narrative_ontology:cs_axiom_status(physical_performance_is_sole_discharge_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('3ece6764-9a07-4500-a8b4-4c3d8f775881', physical_performance_is_sole_discharge_mechanism, deontological).
narrative_ontology:cs_axiom('3ece6764-9a07-4500-a8b4-4c3d8f775881', foundational, study_constitutes_preparation_not_fulfillment).
narrative_ontology:cs_axiom_status(study_constitutes_preparation_not_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('3ece6764-9a07-4500-a8b4-4c3d8f775881', study_constitutes_preparation_not_fulfillment, conventional).
narrative_ontology:cs_reference_frame('3ece6764-9a07-4500-a8b4-4c3d8f775881', temple_era_sacrificial_cult_normative).
narrative_ontology:cs_drift_state('3ece6764-9a07-4500-a8b4-4c3d8f775881', post_destruction_diaspora_contemporary, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('3ece6764-9a07-4500-a8b4-4c3d8f775881', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__performance_only, temple_restoration_movements).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__performance_only, priestly_lineage_claimants).
narrative_ontology:constraint_victim(sacrifice_obligation_continuity__performance_only, current_generation_adherents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__performance_only, textual_scholars).
narrative_ontology:constraint_victim(sacrifice_obligation_continuity__performance_only, textual_scholars).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__performance_only, temple_centrality_doctrine).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__performance_only, sacrificial_cult_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bound by the commandment to bring sacrificial offerings but structurally unable to perform them — there is no standing altar, no functioning priestly service in the required state of ritual purity, no restored site. Under this reading their study of the sacrificial laws is preparatory, not satisfying: it does not discharge the obligation. They carry the weight of an unfulfillable duty indefinitely, with no exit from the obligation and no legitimate path to compliance within their own lifetimes.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, current_generation_adherents, payer,
    powerless, biographical, trapped, global).

% Organizations and factions actively working toward physical rebuilding of the site and reconstitution of priestly service (training candidate priests, preparing ritual implements, lobbying politically and religiously). They administer the performance-only reading because it sustains the urgency and legitimacy of their restoration project — a fulfilled or suspended obligation would remove their reason for existing. They set the interpretive agenda that keeps the obligation open and unmet.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, temple_restoration_movements, agenda_setter,
    organized, generational, constrained, regional).

% Individuals and families who trace descent from the priestly line and stand to receive elevated status, training stipends, and future ritual authority if physical sacrifice resumes. They benefit from the performance-only reading's insistence that only they, in a restored state, can discharge the community's obligation — this preserves their distinct claim to a role that study-based or suspension-based readings would dissolve.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, priestly_lineage_claimants, beneficiary,
    moderate, generational, constrained, regional).

% Devote careers to studying sacrificial law in exhaustive technical detail. Under this reading their labor is officially demoted to mere preparation — valuable but not itself the commandment's fulfillment. They gain communal respect and pedagogical standing from the work, but the performance-only reading denies them the deeper claim (available under the study_as_performance reading) that their scholarship discharges the obligation in its own right.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, textual_scholars, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_continuity__performance_only, textual_scholars, beneficiary).

% Communities and legal authorities holding the study_as_performance, messianic_suspension, or archival_preservation readings are present in the broader tradition but are not the administering authority for this reading's communities. They would argue the obligation is either already fulfilled through study, formally suspended without guilt, or no longer binding — but within the performance-only framework their positions are treated as insufficiently rigorous rather than engaged with on equal footing.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, rival_interpretive_authorities, excluded,
    organized, generational, constrained, global).

% Rabbinic and legal-scholarly bodies that adjudicate which reading of the sacrificial obligation governs practice in a given community. They can examine the four readings, cite precedent, and issue rulings that shift which reading has practical authority, but do not themselves bear the obligation's weight.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, halakhic_courts_and_academies, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_continuity__performance_only, priestly_lineage_claimants).
narrative_ontology:fixing_cost_class(sacrifice_obligation_continuity__performance_only, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps a scattered community oriented toward a single restorative project — coordinating expectation, ritual preparedness, priestly training, and political/religious advocacy around the eventual resumption of physical sacrificial worship, preventing the tradition from drifting into purely symbolic or memorial practice.
% TRANSFER_FUNCTION: Moves psychological and communal weight — guilt, unmet obligation, deferred hope, and status-anticipation — from a specific priestly and restorationist minority onto the entire adherent population, who bear the obligation without any means of discharging it, while priestly claimants and restoration organizers accrue standing, purpose, and future authority from keeping the obligation open.
% ABSENT_VOICES: Adherents who hold the study_as_performance or messianic_suspension readings within the same broader tradition would object that the performance-only framing manufactures unnecessary guilt; they are present in the tradition generally but structurally outside the interpretive authority of communities that have adopted performance-only as binding practice, so their competing readings are not engaged with as live legal alternatives there.
% DISAPPEARANCE_RATIONALE: If the performance-only reading were abandoned in favor of study_as_performance or messianic_suspension, current adherents would be relieved of an unfulfillable duty and its associated guilt; restoration movements would lose their distinctive urgency and much of their fundraising and recruitment rationale; priestly lineage claimants would lose their exclusive claim to future ritual authority. The community's psychological and institutional structure depends on the obligation remaining open and performance-conditioned.
% FOUNDING_PROBLEM: After the destruction of the sacrificial site, the tradition needed to explain why an explicit, detailed biblical and legal commandment to perform sacrifice was going unperformed without concluding either that the law had lapsed or that the community was in continuous, willful violation.
% FOUNDING_PROBLEM_CORROBORATION: Restoration movements and priestly claimants attest the founding problem remains fully live — the obligation is real and unmet, and study is explicitly not sufficient. Adherents outside these movements, along with academies favoring the study_as_performance and messianic_suspension readings, attest from outside the beneficiary set that the same historical rupture is better read as either fulfilled-through-study or formally suspended, and that treating it as live-and-unfulfillable serves institutional restoration interests more than legal necessity.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__performance_only, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__performance_only, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_obligation_continuity__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__performance_only, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_obligation_continuity__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_obligation_continuity__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.71 by interval end) because the reading imposes a real, felt obligation with no legally available discharge mechanism for the party who bears it — the current generation pays in guilt, ritual anxiety, and unmet duty with no offsetting benefit reaching them directly. Suppression is moderate-high (0.58): the reading is maintained less by coercive enforcement than by strong communal and rabbinic consensus that alternative readings are insufficiently rigorous, which functions as social and interpretive suppression of the sibling readings within these communities. Theater ratio is moderate (0.42) and rising: an increasing share of restoration-movement activity (priestly training programs, ritual implement preparation, garment weaving) is symbolic anticipation rather than functional progress toward an actually imminent restoration, and this performative share has grown as political/physical restoration has remained perpetually deferred. Accessibility collapse is moderate (0.4) — the sibling readings are visible and held by real communities, so alternatives have not collapsed globally, only within the specific communities that have adopted performance-only as binding. Resistance is moderate (0.55): individual adherents and rival authorities do push back against the guilt-without-remedy structure, but organized resistance is muted by the reading's deep textual and traditional grounding.
 *
 * DIRECTIONALITY LOGIC:
 *   Current generation adherents are declared victims: they hold the obligation, cannot discharge it, and are trapped by the reading's own logic (leaving the interpretive community means abandoning the tradition's authority structure entirely, not merely this one reading) — d sits near the full-target end. Temple restoration movements and priestly lineage claimants are declared beneficiaries: the movements derive their organizational purpose and the claimants their future exclusive status from the obligation remaining open and performance-conditioned — d sits near the full-beneficiary end. Textual scholars occupy a mixed position: they benefit from communal respect for their expertise but are simultaneously payers because their central life's work is denied ultimate legal effect under this reading, unlike under study_as_performance.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope (rather than pure snare) preserves the genuine coordination function this reading performs: it keeps a historically dispersed community oriented toward a shared restorative project, preserves ritual knowledge that would otherwise atrophy, and provides continuity of legal/textual tradition across centuries of non-performance. That coordination function is real and would be lost if the reading were abandoned entirely. But the same structure imposes asymmetric extraction — guilt and unmet obligation concentrated on ordinary adherents while status and organizational purpose concentrate on restoration movements and priestly claimants — sustained by active interpretive enforcement (rabbinic rulings denying that study or suspension discharge the commandment). Calling this pure snare would erase the genuine memorial and coordinative function; calling it pure rope would erase the asymmetric, unremedied burden borne by the current generation. Tangled rope is the classification that holds both facts simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    performance_only_vs_study_as_performance_authority,
    'Which reading — performance_only or study_as_performance — has stronger textual/legal grounding within the tradition''s own interpretive methods, and is the choice between them a matter of legal derivation or of institutional interest?',
    'Comparative analysis of the classical legal sources each reading cites, cross-checked against communities'' actual institutional incentives (does the community that holds performance_only also disproportionately house restoration movements and priestly-lineage institutions?).',
    'If the correlation between reading-choice and institutional interest is strong, performance_only''s persistence looks more like interest-driven interpretive capture than neutral legal derivation, strengthening the tangled_rope classification''s extraction component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_only_vs_study_as_performance_authority, conceptual, 'Whether the reading choice tracks legal derivation or institutional interest.').

omega_variable(
    guilt_without_remedy_severity,
    'How severe and how widespread is the actual psychological guilt burden this reading imposes on ordinary adherents, versus how much is assumed by outside analysis?',
    'Ethnographic or survey-based study of adherent communities under performance_only versus matched communities under messianic_suspension, measuring reported guilt, anxiety, or communal distress specifically attributable to the unfulfilled sacrificial obligation.',
    'If measured guilt burden is low despite the reading''s structural unfulfillability, extractiveness should be revised downward; if high, it corroborates the high extraction score authored here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(guilt_without_remedy_severity, empirical, 'Empirical magnitude of psychological cost borne by adherents.').

omega_variable(
    kernel_framing_choice,
    'Is the correct unit of analysis the single kernel with four competing readings (as authored here), or should performance_only itself be split further by community (e.g., communities with active restoration political movements vs. quietist communities holding performance_only without organizational apparatus)?',
    'Survey of communities holding performance_only as binding: do their extraction profiles diverge sharply based on whether an organized restoration movement is present locally?',
    'If quietist performance_only communities show materially lower extraction than restoration-movement-adjacent ones, this reading itself decomposes into two further constraints, changing which classification (tangled_rope vs. a lower-extraction rope) applies to each.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_choice, conceptual, 'Whether performance_only requires further decomposition by community type.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__performance_only, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_continuity__performance_only, theater_ratio, 0, 0.25).
narrative_ontology:measurement(sacr_tr_t12, sacrifice_obligation_continuity__performance_only, theater_ratio, 12, 0.3).
narrative_ontology:measurement(sacr_tr_t24, sacrifice_obligation_continuity__performance_only, theater_ratio, 24, 0.34).
narrative_ontology:measurement(sacr_tr_t36, sacrifice_obligation_continuity__performance_only, theater_ratio, 36, 0.37).
narrative_ontology:measurement(sacr_tr_t48, sacrifice_obligation_continuity__performance_only, theater_ratio, 48, 0.4).
narrative_ontology:measurement(sacr_tr_t60, sacrifice_obligation_continuity__performance_only, theater_ratio, 60, 0.42).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_continuity__performance_only, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(sacr_be_t12, sacrifice_obligation_continuity__performance_only, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(sacr_be_t24, sacrifice_obligation_continuity__performance_only, base_extractiveness, 24, 0.63).
narrative_ontology:measurement(sacr_be_t36, sacrifice_obligation_continuity__performance_only, base_extractiveness, 36, 0.66).
narrative_ontology:measurement(sacr_be_t48, sacrifice_obligation_continuity__performance_only, base_extractiveness, 48, 0.69).
narrative_ontology:measurement(sacr_be_t60, sacrifice_obligation_continuity__performance_only, base_extractiveness, 60, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_continuity__performance_only, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(sacr_su_t12, sacrifice_obligation_continuity__performance_only, suppression_requirement, 12, 0.46).
narrative_ontology:measurement(sacr_su_t24, sacrifice_obligation_continuity__performance_only, suppression_requirement, 24, 0.5).
narrative_ontology:measurement(sacr_su_t36, sacrifice_obligation_continuity__performance_only, suppression_requirement, 36, 0.53).
narrative_ontology:measurement(sacr_su_t48, sacrifice_obligation_continuity__performance_only, suppression_requirement, 48, 0.56).
narrative_ontology:measurement(sacr_su_t60, sacrifice_obligation_continuity__performance_only, suppression_requirement, 60, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__performance_only, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_continuity__performance_only, 0.08).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__performance_only, study_as_performance).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__performance_only, messianic_suspension).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__performance_only, archival_preservation).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the sacrifice_obligation_continuity kernel, decomposed per the ε-invariance principle because the four readings produce materially different ε values from the same underlying text and historical rupture. performance_only carries the highest extractiveness (0.71) because it alone treats the obligation as live, binding, and undischargeable by study for the current generation. study_as_performance would carry near-zero extractiveness (obligation continuously satisfied through textual engagement). messianic_suspension would carry low extractiveness (obligation suspended, no guilt attaches). archival_preservation would carry near-zero extractiveness (no binding obligation remains). All four are linked here rather than merged into one story with a measurement parameter, per the authoring rule: disambiguate the label, don't complicate the logic.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
