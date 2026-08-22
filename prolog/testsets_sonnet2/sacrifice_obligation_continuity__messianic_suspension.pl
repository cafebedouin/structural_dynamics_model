% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__messianic_suspension
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__messianic_suspension, []).

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
 *   constraint_id: sacrifice_obligation_continuity__messianic_suspension
 *   human_readable: Sacrifice Obligation Held in Messianic Suspension, Maintained Through Study
 *   domain: religious_law/ritual_studies/textual_tradition
 *
 * SUMMARY:
 *   Following the destruction of the Second Temple, the sacrificial
 *   commandments (korbanot) could no longer be physically performed. This
 *   reading of the kernel holds that the obligation is suspended pending
 *   messianic restoration of the Temple — neither fulfilled (as the
 *   study-as-performance reading holds) nor rendered obsolete (as the
 *   archival-preservation reading holds) nor requiring active preparatory
 *   performance now (as the performance-only reading holds). Under
 *   suspension, non-performance carries no violation because the means of
 *   performance was removed by circumstance beyond the community's control,
 *   not by choice or doctrinal abandonment. Study of the sacrificial laws
 *   functions as a maintenance protocol: it keeps the technical knowledge,
 *   priestly genealogies, and procedural detail alive so that restoration, if
 *   and when it occurs, can be an activation rather than a reconstruction.
 *   This is a deliberately narrow reading of one specific claim within the
 *   sacrifice-obligation kernel; it does not attempt to adjudicate the
 *   sibling readings, which are separate constraints.
 *
 * KEY AGENTS:
 *   - rabbinic_academies: administer the suspension doctrine and its study curriculum (institutional/identity_locked)
 *   - religious_court_authorities: retain jurisdiction over suspended-but-live ritual law (institutional/constrained)
 *   - lay_practitioners: bear diffuse readiness costs without present ritual outlet (moderate/constrained)
 *   - kohanim_descendants: carry hereditary restrictions tied to a non-operative function (moderate/identity_locked)
 *   - reform_and_reconstructionist_communities: excluded alternative reading, treats obligation as superseded (organized/mobile)
 *   - textual_scholars: analytical observers of doctrinal history (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__messianic_suspension, 0.38).
domain_priors:suppression_score(sacrifice_obligation_continuity__messianic_suspension, 0.42).
domain_priors:theater_ratio(sacrifice_obligation_continuity__messianic_suspension, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, extractiveness, 0.38).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__messianic_suspension, scaffold).
narrative_ontology:human_readable(sacrifice_obligation_continuity__messianic_suspension, "Sacrifice Obligation Held in Messianic Suspension, Maintained Through Study").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__messianic_suspension, "religious_law/ritual_studies/textual_tradition").

domain_priors:requires_active_enforcement(sacrifice_obligation_continuity__messianic_suspension).
narrative_ontology:has_sunset_clause(sacrifice_obligation_continuity__messianic_suspension).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__messianic_suspension, '90ffa078-6759-437a-9ea5-88039718d0de').
narrative_ontology:cs_kernel_codification('90ffa078-6759-437a-9ea5-88039718d0de', fixed_text).
narrative_ontology:cs_authority_grounding('90ffa078-6759-437a-9ea5-88039718d0de', lineage).
narrative_ontology:cs_interpretation_layer_present('90ffa078-6759-437a-9ea5-88039718d0de').
narrative_ontology:cs_reading_relation('90ffa078-6759-437a-9ea5-88039718d0de', sacrifice_obligation_continuity__archival_preservation, forecloses).
narrative_ontology:cs_reading_relation('90ffa078-6759-437a-9ea5-88039718d0de', sacrifice_obligation_continuity__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('90ffa078-6759-437a-9ea5-88039718d0de', sacrifice_obligation_continuity__study_as_performance, influences).
narrative_ontology:cs_axiom('90ffa078-6759-437a-9ea5-88039718d0de', foundational, obligation_suspended_not_voided_by_external_impossibility).
narrative_ontology:cs_axiom_status(obligation_suspended_not_voided_by_external_impossibility, holdable).
narrative_ontology:cs_axiom_grounding('90ffa078-6759-437a-9ea5-88039718d0de', obligation_suspended_not_voided_by_external_impossibility, conventional).
narrative_ontology:cs_axiom('90ffa078-6759-437a-9ea5-88039718d0de', foundational, study_maintains_readiness_without_constituting_fulfillment).
narrative_ontology:cs_axiom_status(study_maintains_readiness_without_constituting_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('90ffa078-6759-437a-9ea5-88039718d0de', study_maintains_readiness_without_constituting_fulfillment, instrumental).
narrative_ontology:cs_reference_frame('90ffa078-6759-437a-9ea5-88039718d0de', second_temple_sacrificial_practice).
narrative_ontology:cs_drift_state('90ffa078-6759-437a-9ea5-88039718d0de', contemporary_orthodox_practice, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('90ffa078-6759-437a-9ea5-88039718d0de', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__messianic_suspension, rabbinic_academies).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__messianic_suspension, religious_court_authorities).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__messianic_suspension, communal_identity_continuity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__messianic_suspension, kohanim_descendants).
narrative_ontology:constraint_victim(sacrifice_obligation_continuity__messianic_suspension, lay_practitioners).
narrative_ontology:constraint_victim(sacrifice_obligation_continuity__messianic_suspension, kohanim_descendants).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__messianic_suspension, divine_covenant_permanence).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__messianic_suspension, temple_restoration_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the study curriculum that keeps sacrificial law in active legal discussion (halakhic sugyot on korbanot are taught and adjudicated as if the Temple could be rebuilt tomorrow). Sets the interpretive terms under which suspension, rather than obsolescence, is the operative frame. Draws institutional authority, curricular centrality, and continuity of communal leadership from maintaining the readiness posture.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, rabbinic_academies, agenda_setter,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_continuity__messianic_suspension, rabbinic_academies, beneficiary).

% Retains adjudicative relevance over questions of ritual purity, priestly lineage (kohanim status), and sacrificial procedure that would otherwise have no live docket. The suspension framing keeps these bodies of law within active jurisdiction rather than relegating them to antiquarian interest.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, religious_court_authorities, beneficiary,
    institutional, civilizational, constrained, national).

% Bears the readiness burden in diffuse form: liturgical recitation of sacrificial passages, communal fasts and mourning rites tied to the Temple's absence, financial and time investment in study that has no present practical outlet. No guilt attaches for non-performance (the obligation is suspended, not violated), but the ongoing psychological and temporal cost of orienting communal life around a contingency that may never arrive is real and largely non-negotiable within the tradition's own terms.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, lay_practitioners, payer,
    moderate, biographical, constrained, regional).

% Carries hereditary priestly status and its attendant restrictions (marriage limitations, corpse-contact avoidance) as ongoing lived obligations tied to a sacrificial system that is not presently operative. Benefits from elevated communal status; pays through restricted personal autonomy maintained on behalf of a suspended function.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, kohanim_descendants, payer,
    moderate, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_continuity__messianic_suspension, kohanim_descendants, beneficiary).

% Have largely exited the readiness framework entirely, treating sacrificial law as superseded rather than suspended. Their position — that continued readiness maintenance is neither obligatory nor meaningful — is present in broader Jewish discourse but is not represented within the interpretive communities that administer the messianic-suspension reading; from inside that reading their view does not register as a live halakhic option.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, reform_and_reconstructionist_communities, excluded,
    organized, generational, mobile, national).

% Studies the historical development of the suspension doctrine (from Talmudic sources through Maimonides' codification and later messianic-restoration literature) without being bound by its normative claims. Can trace how the doctrine's function has shifted across centuries of Temple absence.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, textual_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_continuity__messianic_suspension, rabbinic_academies).
narrative_ontology:fixing_cost_class(sacrifice_obligation_continuity__messianic_suspension, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the technical, legal, and communal infrastructure needed to resume sacrificial practice on short notice — priestly genealogy records, procedural halakhah, ritual-purity law, architectural specifications — so that restoration, if it occurs, is not a reconstruction from scratch but a reactivation of maintained knowledge.
% TRANSFER_FUNCTION: Moves ongoing interpretive authority, curricular centrality, and communal leadership legitimacy to the institutions that administer sacrificial law as live (rather than dead) doctrine; moves diffuse psychological, temporal, and status-based costs to lay practitioners and hereditary priestly families who orient practice around a contingency without present enactment.
% ABSENT_VOICES: Reform and Reconstructionist voices who hold the sacrificial obligation to be superseded, not suspended, are not part of the interpretive community that maintains this reading and do not shape its terms from within.
% DISAPPEARANCE_RATIONALE: If the suspension framing were abandoned in favor of treating the obligation as simply obsolete, the affected institutions dispute what would change: administering rabbinic academies argue the readiness posture is load-bearing for covenantal continuity and its removal would sever a live theological claim; outside observers and non-Orthodox movements argue functionally little would change, since the sacrificial system is not presently practiced under either reading — the dispute is over doctrinal status, not present-day ritual behavior.
% FOUNDING_PROBLEM: After the Second Temple's destruction (70 CE), the sacrificial commandments could no longer be physically performed, creating an acute legal and theological problem: how can a covenant built partly around commanded sacrifice remain intact, and its adherents remain non-culpable, when the means of fulfillment has been destroyed by external force rather than abandoned by choice?
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's original urgency — imminent covenantal rupture — is attested by Talmudic-era sources themselves (an internal, not fully independent, attestation). Outside the benefiting institutions, comparative religion scholars and historians of Second Temple Judaism corroborate that the destruction created a genuine crisis requiring doctrinal resolution; those same scholars, from outside the tradition's own normative commitments, generally regard the crisis as historically resolved by two millennia of adaptation (rabbinic Judaism's prayer-and-study substitution) rather than awaiting resolution by restoration, making the 'still live' status a claim corroborated only from within the tradition.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__messianic_suspension, contested).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__messianic_suspension, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__messianic_suspension, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_obligation_continuity__messianic_suspension, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__messianic_suspension, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__messianic_suspension_tests).
:- end_tests(sacrifice_obligation_continuity__messianic_suspension_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 by the interval's end) because the suspension reading imposes a real but non-punitive readiness burden — time, liturgical practice, communal orientation — without attaching guilt or legal culpability to non-performance; this is structurally lighter than a snare precisely because the 'debt' is suspended rather than owed. Suppression sits at 0.42, reflecting real but declining enforcement of the doctrinal frame: early centuries after the destruction required more active theological defense of 'suspended, not abandoned' against competing readings; over centuries the frame stabilized into normative consensus within Orthodox institutions, requiring less active suppression of alternatives (though it still forecloses casual treatment of the obligation as void). Theater ratio rises modestly (0.15 to 0.30) as institutional study of sacrificial law increasingly serves identity-maintenance and curricular functions independent of any near-term restoration expectation — a slow drift toward performative maintenance the T17 trigger would flag as worth investigating if base_extractiveness were rising rather than falling.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic academies and religious courts are structural beneficiaries: the suspension frame keeps their interpretive domain live and their authority intact. Lay practitioners and kohanim descendants are payers: they carry the readiness burden (ritual restrictions, liturgical time, communal orientation around an uncertain contingency) without the compensating institutional authority. Kohanim are dual-positioned — elevated status is itself a form of benefit, but the restrictions that come with it are a real ongoing cost, hence the secondary beneficiary role alongside payer. Reform and Reconstructionist communities are excluded rather than victimized — they have exited the framework rather than being extracted from within it, which is why they appear as excluded/mobile rather than as a victim group under base_properties.
 *
 * MANDATROPHY ANALYSIS:
 *   The suspension reading is a scaffold precisely because it declares its own sunset condition (messianic restoration) as the terminus of the arrangement, distinguishing it from a piton (which persists with no terminus in view) or a snare (which has no genuine coordination function). The founding problem — how to remain covenantally intact after forced cessation of commanded practice — was live and acute in the immediate post-destruction centuries; whether it remains live today is contested precisely along the fault line this reading occupies against the archival-preservation reading. The mismatch check (founding_problem_status=contested x disappearance_verdict=contested) does not resolve to a mandatrophy flag on its own; that ambiguity is the honest state of a doctrine whose adherents assert continued liveness against outside historical readings that see the problem as functionally metabolized.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suspension_vs_obsolescence_ambiguity,
    'Is the sacrifice obligation genuinely suspended (retaining latent normative force, awaiting a real triggering condition) or is ''suspension'' a doctrinal fiction that functions identically to obsolescence while preserving institutional authority that outright obsolescence would dissolve?',
    'Compare institutional behavior under a counterfactual: if messianic restoration were declared theologically impossible by consensus (not merely deferred), would the study curriculum and institutional authority structures dissolve, or would they persist under a relabeled justification? Persistence under relabeling would support the fiction reading.',
    'If suspension functions identically to obsolescence in institutional practice, the scaffold classification''s sunset-clause condition is honored only nominally, and the constraint drifts toward piton (readiness theater maintained by inertia rather than genuine anticipation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suspension_vs_obsolescence_ambiguity, conceptual, 'Whether messianic suspension is a live contingency or a stabilized doctrinal fiction functioning as permanent status quo.').

omega_variable(
    readiness_burden_measurement,
    'How much of the lay practitioner readiness burden (liturgical time, restricted practices, communal orientation) is attributable specifically to the suspension doctrine, versus other independent religious observances that would persist regardless of the sacrifice-obligation framing?',
    'Comparative study of communities that hold the archival-preservation reading versus the messianic-suspension reading, isolating practice differences attributable specifically to the suspension frame (e.g., specific fast days, specific liturgical insertions tied to sacrificial restoration hope).',
    'If the marginal burden attributable to suspension specifically (beyond general religious observance) is small, the extractiveness score should be revised downward; if the suspension frame independently drives substantial additional practice, 0.38 may understate the burden.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(readiness_burden_measurement, empirical, 'Isolating the suspension-specific component of the overall readiness burden from general religious observance.').

omega_variable(
    corroboration_asymmetry,
    'Given that the founding problem''s continued liveness is corroborated primarily from within the tradition administering the suspension doctrine, does the absence of strong independent (non-tradition) corroboration for ''still live'' status itself constitute evidence for the archival-preservation sibling reading rather than this one?',
    'Systematic review of historians of religion and comparative theologians outside any Jewish denominational commitment, assessing whether they treat the post-Temple sacrificial crisis as resolved (via rabbinic substitution) or genuinely pending (awaiting restoration).',
    'If outside corroboration strongly favors ''resolved,'' the messianic_suspension reading''s claim to represent a still-live doctrinal state (rather than a stabilized substitute practice wearing suspension language) weakens relative to its siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corroboration_asymmetry, conceptual, 'Whether the corroboration asymmetry between internal and external attestation favors this reading or the archival_preservation sibling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__messianic_suspension, 0, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(sacr_tr_t0, observed).
narrative_ontology:measurement(sacr_tr_t300, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 300, 0.2).
narrative_ontology:measurement_basis(sacr_tr_t300, observed).
narrative_ontology:measurement(sacr_tr_t700, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 700, 0.24).
narrative_ontology:measurement_basis(sacr_tr_t700, observed).
narrative_ontology:measurement(sacr_tr_t1200, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 1200, 0.27).
narrative_ontology:measurement_basis(sacr_tr_t1200, observed).
narrative_ontology:measurement(sacr_tr_t1650, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 1650, 0.29).
narrative_ontology:measurement_basis(sacr_tr_t1650, observed).
narrative_ontology:measurement(sacr_tr_t1950, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 1950, 0.3).
narrative_ontology:measurement_basis(sacr_tr_t1950, observed).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(sacr_be_t0, observed).
narrative_ontology:measurement(sacr_be_t300, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 300, 0.48).
narrative_ontology:measurement_basis(sacr_be_t300, observed).
narrative_ontology:measurement(sacr_be_t700, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 700, 0.42).
narrative_ontology:measurement_basis(sacr_be_t700, observed).
narrative_ontology:measurement(sacr_be_t1200, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 1200, 0.4).
narrative_ontology:measurement_basis(sacr_be_t1200, observed).
narrative_ontology:measurement(sacr_be_t1650, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 1650, 0.39).
narrative_ontology:measurement_basis(sacr_be_t1650, observed).
narrative_ontology:measurement(sacr_be_t1950, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 1950, 0.38).
narrative_ontology:measurement_basis(sacr_be_t1950, observed).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 0, 0.65).
narrative_ontology:measurement_basis(sacr_su_t0, observed).
narrative_ontology:measurement(sacr_su_t300, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 300, 0.58).
narrative_ontology:measurement_basis(sacr_su_t300, observed).
narrative_ontology:measurement(sacr_su_t700, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 700, 0.52).
narrative_ontology:measurement_basis(sacr_su_t700, observed).
narrative_ontology:measurement(sacr_su_t1200, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 1200, 0.47).
narrative_ontology:measurement_basis(sacr_su_t1200, observed).
narrative_ontology:measurement(sacr_su_t1650, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 1650, 0.44).
narrative_ontology:measurement_basis(sacr_su_t1650, observed).
narrative_ontology:measurement(sacr_su_t1950, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 1950, 0.42).
narrative_ontology:measurement_basis(sacr_su_t1950, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__messianic_suspension, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_continuity__messianic_suspension, 0.1).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__messianic_suspension, study_as_performance).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__messianic_suspension, performance_only).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__messianic_suspension, archival_preservation).

% DUAL FORMULATION NOTE:
% Part of a four-member constraint family decomposing the natural-language 'sacrifice obligation status' claim, which conflates four structurally distinct positions on whether the obligation is fulfilled, suspended, violated-but-deferred, or void. Each sibling reading is authored as its own constraint with its own epsilon and beneficiary/victim structure per the ε-invariance principle. This reading (messianic_suspension) is distinguished from study_as_performance by denying that study discharges the obligation; from performance_only by denying that non-performance constitutes violation; and from archival_preservation by denying that the obligation has lapsed. The upstream/downstream relationship runs loosely rather than hierarchically here — no single reading is empirically prior to the others, unlike the BGS family — so all four are linked as coexisting siblings rather than as a dependency chain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
