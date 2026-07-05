% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__performance_only_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__performance_only_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: sacrifice_obligation_kernel__performance_only_reading
 *   human_readable: Sacrifice Obligation — Performance-Only Reading (Physical Enactment Required, Study Insufficient)
 *   domain: religious/legal/halakhic
 *
 * SUMMARY:
 *   This story instantiates the performance-only reading of the sacrifice
 *   obligation kernel: the halakhic position that korbanot are discharged
 *   only through literal physical enactment at a functioning Temple altar,
 *   and that study, prayer, or liturgical substitution — however valuable —
 *   do not fulfill the underlying mitzvah. Under this reading, the
 *   destruction of the Second Temple in 70 CE opened a structural gap between
 *   an active, binding commandment and a total absence of the capacity to
 *   perform it, a gap that persists formally to the present. This is not
 *   extraction by an identifiable agent in the ordinary snare sense; rather
 *   it is a doctrinal choice that keeps an obligation perpetually open, which
 *   downstream institutions (restoration advocacy, lineage-status claims)
 *   then draw legitimacy from. The kernel itself
 *   (sacrifice_obligation_kernel) is read differently by
 *   study_as_exercise_reading (intellectual engagement discharges the
 *   mitzvah), messianic_suspension_reading (the obligation is suspended, not
 *   owed, pending restoration), and symbolic_archive_reading (the law is
 *   cultural memory, no halakhic claim survives). This file generates ONLY
 *   the performance-only reading; ε, beneficiaries, and victims here are
 *   stable to this reading and are not averaged against the siblings.
 *
 * KEY AGENTS:
 *   - observant_jewish_community: primary bearer of the unfulfilled obligation across civilizational time horizon, identity-locked exit
 *   - kohanim_descendants: lineage-specific payers whose priestly identity is constituted by an unperformable role
 *   - temple_restoration_advocacy_movements: organized beneficiary drawing institutional purpose from the unfulfilled-performance framing
 *   - priestly_lineage_claimants: moderate-power beneficiary whose status has stakes because restoration is read as pending, not obsolete
 *   - rabbinic_courts_and_halakhic_authorities: institutional agenda-setters who administer which reading is operative
 *   - study_as_exercise_reading: sibling reading, non-agent, excluded from authority under this reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__performance_only_reading, 0.81).
domain_priors:suppression_score(sacrifice_obligation_kernel__performance_only_reading, 0.35).
domain_priors:theater_ratio(sacrifice_obligation_kernel__performance_only_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__performance_only_reading, tangled_rope).
narrative_ontology:human_readable(sacrifice_obligation_kernel__performance_only_reading, "Sacrifice Obligation — Performance-Only Reading (Physical Enactment Required, Study Insufficient)").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__performance_only_reading, "religious/legal/halakhic").

domain_priors:requires_active_enforcement(sacrifice_obligation_kernel__performance_only_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__performance_only_reading, '1e0b251c-e3eb-49c9-a166-024e9c8f6380').
narrative_ontology:cs_kernel_codification('1e0b251c-e3eb-49c9-a166-024e9c8f6380', fixed_text).
narrative_ontology:cs_authority_grounding('1e0b251c-e3eb-49c9-a166-024e9c8f6380', lineage).
narrative_ontology:cs_interpretation_layer_present('1e0b251c-e3eb-49c9-a166-024e9c8f6380').
narrative_ontology:cs_reading_relation('1e0b251c-e3eb-49c9-a166-024e9c8f6380', sacrifice_obligation_kernel__study_as_exercise_reading, forecloses).
narrative_ontology:cs_reading_relation('1e0b251c-e3eb-49c9-a166-024e9c8f6380', sacrifice_obligation_kernel__messianic_suspension_reading, coexists_with).
narrative_ontology:cs_reading_relation('1e0b251c-e3eb-49c9-a166-024e9c8f6380', sacrifice_obligation_kernel__symbolic_archive_reading, forecloses).
narrative_ontology:cs_axiom('1e0b251c-e3eb-49c9-a166-024e9c8f6380', foundational, mitzvah_requires_physical_maaseh).
narrative_ontology:cs_axiom_status(mitzvah_requires_physical_maaseh, holdable).
narrative_ontology:cs_axiom_grounding('1e0b251c-e3eb-49c9-a166-024e9c8f6380', mitzvah_requires_physical_maaseh, conventional).
narrative_ontology:cs_axiom('1e0b251c-e3eb-49c9-a166-024e9c8f6380', foundational, intellectual_engagement_insufficient_for_discharge).
narrative_ontology:cs_axiom_status(intellectual_engagement_insufficient_for_discharge, holdable).
narrative_ontology:cs_axiom_grounding('1e0b251c-e3eb-49c9-a166-024e9c8f6380', intellectual_engagement_insufficient_for_discharge, conventional).
narrative_ontology:cs_axiom('1e0b251c-e3eb-49c9-a166-024e9c8f6380', secondary, obligation_remains_formally_owed_absent_temple).
narrative_ontology:cs_axiom_status(obligation_remains_formally_owed_absent_temple, holdable).
narrative_ontology:cs_axiom_grounding('1e0b251c-e3eb-49c9-a166-024e9c8f6380', obligation_remains_formally_owed_absent_temple, deontological).
narrative_ontology:cs_reference_frame('1e0b251c-e3eb-49c9-a166-024e9c8f6380', temple_era_literal_performance_standard).
narrative_ontology:cs_drift_state('1e0b251c-e3eb-49c9-a166-024e9c8f6380', post_destruction_rabbinic_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('1e0b251c-e3eb-49c9-a166-024e9c8f6380', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__performance_only_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__performance_only_reading, temple_restoration_advocacy_movements).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__performance_only_reading, priestly_lineage_claimants).
narrative_ontology:constraint_victim(sacrifice_obligation_kernel__performance_only_reading, observant_jewish_community).
narrative_ontology:constraint_victim(sacrifice_obligation_kernel__performance_only_reading, kohanim_descendants).
narrative_ontology:constraint_victim(sacrifice_obligation_kernel__performance_only_reading, diaspora_religious_practitioners).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__performance_only_reading, physical_performance_primacy_doctrine).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__performance_only_reading, mitzvah_requires_maaseh).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Held bound by a commandment (korbanot) that this reading treats as unfulfillable by any means available to them since the Temple's destruction in 70 CE. Study, prayer, and liturgical substitution are acknowledged as valuable but explicitly do not discharge the obligation under this reading. They carry an open, unfulfillable mitzvah across roughly 1,900 years, generationally, with no exit from the commandedness itself even as physical performance remains structurally impossible.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, observant_jewish_community, payer,
    powerless, civilizational, identity_locked, global).

% Carry lineage-specific obligations tied to priestly service that this reading holds in permanent abeyance without discharge. Their identity as kohanim is itself constituted by a role they cannot perform; genealogical status persists as a marker of unexercised duty rather than active function.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, kohanim_descendants, payer,
    powerless, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_kernel__performance_only_reading, kohanim_descendants, excluded).

% Organizations and factions (e.g., groups preparing ritual implements, tracing genealogies, lobbying for access to the Temple Mount) derive their institutional purpose and fundraising rationale from the unfulfilled-performance gap this reading insists upon. The more sharply the obligation is read as strictly performative and currently unmet, the more their advocacy and preparatory activity is justified as urgent religious necessity.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, temple_restoration_advocacy_movements, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_kernel__performance_only_reading, temple_restoration_advocacy_movements, agenda_setter).

% Individuals and institutes claiming heightened authority or status through documented kohanic descent gain social and institutional standing specifically because the performance-only reading keeps the priestly function nominally 'pending' rather than obsolete or purely symbolic — their claimed status has stakes precisely because the reading treats restoration as the live discharge condition.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, priestly_lineage_claimants, beneficiary,
    moderate, generational, constrained, national).

% Adjudicate and transmit which reading of the sacrifice-obligation kernel is operative for a given community. This reading is one they can hold, teach, or de-emphasize; they administer the interpretive apparatus (Talmudic, Rambam-derived) that keeps the performance-only standard in force, and they could shift emphasis toward alternative readings without abandoning the kernel itself.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, rabbinic_courts_and_halakhic_authorities, agenda_setter,
    institutional, civilizational, arbitrage, global).

% A sibling reading (not an actor) that holds intellectual engagement with sacrifice law itself discharges the mitzvah. Not consulted as authoritative wherever this performance-only reading dominates; listed here for structural completeness, not as a party bearing costs or benefits.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, study_as_exercise_reading, excluded,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(sacrifice_obligation_kernel__performance_only_reading, study_as_exercise_reading).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_kernel__performance_only_reading, diffuse).
narrative_ontology:fixing_cost_class(sacrifice_obligation_kernel__performance_only_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves a bright-line, unambiguous halakhic standard for what counts as fulfilling a physical commandment — preventing indefinite substitution or dilution of maaseh (deed) requirements by intellectual or symbolic proxies, and keeping the community oriented toward eventual literal restoration rather than treating exile as a permanent settlement.
% TRANSFER_FUNCTION: Moves nothing material between named parties in the ordinary extraction sense; instead it holds open an unresolved obligation across the entire observant population, transferring the psychological, liturgical, and institutional weight of 'commanded but unable to perform' onto every generation, while advocacy movements and lineage-status claimants extract legitimacy, funding, and standing from the persistence of that unresolved gap.
% ABSENT_VOICES: The study_as_exercise_reading and symbolic_archive_reading communities are structurally present in the same textual tradition but not authoritative under this reading; practitioners who find the perpetual-unfulfillment framing psychologically or theologically unsustainable (e.g., those who quietly treat prayer as sufficient discharge) are not represented in the formal halakhic conversation this reading governs.
% DISAPPEARANCE_RATIONALE: If the performance-only standard were abandoned overnight in favor of, say, the study-as-exercise reading, the entire liturgical, legal, and institutional apparatus built around 'awaiting restoration' would lose its rationale: yeshivot devoted to sacrificial law study could reclassify their activity as complete fulfillment rather than preparation, restoration advocacy would lose its distinctive urgency, and centuries of halakhic literature treating the obligation as suspended-but-live would require reinterpretation. The kohanic status system would also shift meaning substantially.
% FOUNDING_PROBLEM: The founding problem was maintaining doctrinal fidelity to the Torah's explicit command structure (korbanot as maaseh, physical acts specified in detail) after the Second Temple's destruction removed all capacity to perform them — the reading was built to prevent the commandment from being redefined out of existence by treating substitutes as equivalent to the literal act.
% FOUNDING_PROBLEM_CORROBORATION: Talmudic authorities (e.g., tractates addressing korbanot post-Destruction) and later halakhic codifiers attest the problem was live at formation and remains formally live within Orthodox legal reasoning. Comparative religious scholars and historians of Jewish liturgy, writing from outside the tradition's own authority structure, corroborate that the physical-performance standard was maintained as a deliberate doctrinal choice (rather than a logical necessity) precisely to preserve restoration-oriented eschatology — but many do not corroborate that the underlying problem remains 'live' in any operational sense, noting that prayer and study have functioned as de facto communal practice for nearly two millennia regardless of the formal doctrine.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__performance_only_reading, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__performance_only_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__performance_only_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_obligation_kernel__performance_only_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__performance_only_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__performance_only_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_obligation_kernel__performance_only_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_obligation_kernel__performance_only_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.81) because the reading imposes an open-ended, unfulfillable formal obligation on an entire population across roughly nineteen centuries, with the burden compounding as generations pass without discharge (rising trajectory in the measurements). Suppression is authored moderate (0.35) rather than high because there is no active coercive apparatus forcing individuals to accept this reading — rabbinic authority operates through doctrinal transmission and communal norm rather than physical coercion, and dissenting or alternative readings (the three siblings) are live elsewhere in the same tradition. Theater ratio rises over the interval (0.20 to 0.42) reflecting the growth of preparatory and advocacy activity (garment weaving, altar-vessel reconstruction, genealogical registries) that performs readiness without altering the underlying structural impossibility — a genuine but increasingly performative preparatory layer. Accessibility collapse is authored high (0.72): once a community accepts the performance-only standard, the practical alternatives (treating study or prayer as sufficient) become doctrinally unavailable to them without switching readings entirely. Resistance is authored moderate-high (0.58): communities and individual poskim have pushed back by emphasizing prayer-as-substitute language in liturgy (the Amidah's korbanot references) even while formally upholding this reading, producing a lived tension the doctrine does not fully resolve.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic-authority seat, the performance-only reading looks like doctrinal fidelity — the coordination function of preserving what the commandment actually requires against dilution. From the observant-community seat under civilizational time horizon and identity-locked exit, the same structure operates as an inescapable, compounding unfulfilled debt: a mitzvah held perpetually open with the entire population as payer and no agent as clean beneficiary. The engine should register this as tangled_rope rather than snare precisely because a genuine coordination function (doctrinal precision, resistance to substitution-creep) coexists with real asymmetric cost-bearing borne by a population that has no exit from the underlying commandedness, and because institutions layered on top (restoration advocacy, lineage-status economies) do capture legitimacy from the persistence of the gap even though the original doctrinal choice was not designed as extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Observant_jewish_community and kohanim_descendants are declared victims: they carry the obligation's weight with no realistic exit (identity-locked — leaving the framework means leaving the religious identity itself) and derive no offsetting institutional benefit from the reading's persistence, pushing their directionality toward the full-target end. Temple_restoration_advocacy_movements and priestly_lineage_claimants are declared beneficiaries: their institutional purpose, fundraising, and status economies are enabled specifically by the reading's insistence that the obligation remains unmet and physically discharge-able only through restoration, pushing their directionality toward the beneficiary end despite their comparatively modest formal power. Rabbinic_courts_and_halakhic_authorities sit as agenda-setters with institutional power and arbitrage-like exit (they can shift interpretive emphasis without losing standing), which is why they are not listed as beneficiaries in the extraction sense — they administer the kernel rather than collecting from its persistence.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as tangled_rope rather than snare (no clean extractive agent) or mountain (this is a human doctrinal choice, not a natural law) prevents two mislabeling errors: it avoids treating a genuine, defensible doctrinal-fidelity function as pure victimhood-generating extraction, while also avoiding treating the compounding 1,900-year burden on the observant population as a costless or purely symbolic matter. The founding_problem fields register status as contested rather than dead or live cleanly: the doctrinal problem (preventing substitution-creep) remains formally live within the tradition's own reasoning, but the practical problem (a population needing some mode of religious fulfillment) has been substantially addressed de facto through prayer and study for centuries — even though this reading does not grant that substitution formal standing. That mismatch (status=contested, disappearance_verdict=world_rearranges) is exactly the kind of signal the R5 genealogy interview is meant to surface rather than resolve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_authority,
    'Which body or process determines that the performance_only_reading, rather than study_as_exercise_reading or messianic_suspension_reading, is the operative reading for a given community, and is that selection itself contestable within Orthodox halakhic method?',
    'Comparative analysis of posek (halakhic decisor) rulings across communities and eras: identify whether communities that emphasize the performance-only standard differ systematically (in institutional structure, geographic proximity to Jerusalem, exposure to restoration movements) from communities that lean toward the study-as-exercise or suspension readings.',
    'If reading selection tracks institutional interest (e.g., communities with active restoration advocacy infrastructure disproportionately hold the performance-only reading) rather than purely textual/logical necessity, this would strengthen the case that the reading''s persistence is partly maintained by the downstream beneficiaries identified here rather than by doctrinal necessity alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_authority, conceptual, 'Whether the choice among sibling readings tracks institutional interest or pure textual necessity.').

omega_variable(
    structural_impossibility_vs_extraction,
    'Is the 1,900-year unfulfilled-obligation gap better modeled as a structural impossibility with no extracting agent (closer to a mountain-like inevitability given the Temple''s destruction) or as a tangled_rope where restoration-advocacy and lineage-status institutions actively extract legitimacy from maintaining the gap rather than resolving it through alternative readings?',
    'Examine whether restoration advocacy organizations and lineage-status institutes have historically supported or opposed halakhic movements toward the study-as-exercise or messianic-suspension readings that would ease the burden on the general population while preserving their own specific institutional roles.',
    'If such institutions have historically resisted softer readings even where those readings were textually available, this supports the tangled_rope classification (active interest in maintaining the gap). If they have been neutral or supportive of softer readings for the general population while maintaining their own specialized preparatory role, the extraction component is weaker and the constraint sits closer to a claimed-but-uncaptured structural condition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(structural_impossibility_vs_extraction, empirical, 'Whether downstream institutions have an active interest in non-resolution of the doctrinal gap.').

omega_variable(
    psychological_burden_measurement,
    'How is the ''extractiveness'' of an unfulfillable religious obligation actually experienced by the observant population — as a source of meaning and eschatological hope (mitigating the burden) or as an unresolved existential debt (compounding it)?',
    'Ethnographic and sociological study of Orthodox communities'' subjective relationship to the unperformed sacrifice obligation, compared across communities with differing emphasis on restoration urgency versus liturgical sufficiency.',
    'If the obligation functions primarily as meaning-generating hope rather than burden, the authored extractiveness score (0.81) may overstate the lived cost even though it accurately describes the formal doctrinal structure; this would not change the classification but would refine the interpretation of what the metric is measuring.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(psychological_burden_measurement, empirical, 'Whether the formal extractiveness score matches the population''s subjective experience of the obligation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__performance_only_reading, 0, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(sacr_tr_t0, observed).
narrative_ontology:measurement(sacr_tr_t300, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 300, 0.28).
narrative_ontology:measurement_basis(sacr_tr_t300, observed).
narrative_ontology:measurement(sacr_tr_t700, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 700, 0.33).
narrative_ontology:measurement_basis(sacr_tr_t700, observed).
narrative_ontology:measurement(sacr_tr_t1100, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 1100, 0.36).
narrative_ontology:measurement_basis(sacr_tr_t1100, observed).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 1500, 0.4).
narrative_ontology:measurement_basis(sacr_tr_t1500, observed).
narrative_ontology:measurement(sacr_tr_t1900, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 1900, 0.42).
narrative_ontology:measurement_basis(sacr_tr_t1900, observed).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(sacr_be_t0, observed).
narrative_ontology:measurement(sacr_be_t300, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 300, 0.62).
narrative_ontology:measurement_basis(sacr_be_t300, observed).
narrative_ontology:measurement(sacr_be_t700, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 700, 0.68).
narrative_ontology:measurement_basis(sacr_be_t700, observed).
narrative_ontology:measurement(sacr_be_t1100, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 1100, 0.73).
narrative_ontology:measurement_basis(sacr_be_t1100, observed).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 1500, 0.78).
narrative_ontology:measurement_basis(sacr_be_t1500, observed).
narrative_ontology:measurement(sacr_be_t1900, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 1900, 0.81).
narrative_ontology:measurement_basis(sacr_be_t1900, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_obligation_kernel__performance_only_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__performance_only_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_kernel__performance_only_reading, 0.08).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__performance_only_reading, study_as_exercise_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__performance_only_reading, messianic_suspension_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__performance_only_reading, symbolic_archive_reading).

% DUAL FORMULATION NOTE:
% This story is one of four members of the sacrifice_obligation_kernel constraint family. performance_only_reading (this story) claims the highest extractiveness of the four because it is the only reading that holds the obligation simultaneously live, undischarged, and undischargeable by any means currently available to practitioners. study_as_exercise_reading resolves the gap by reclassifying intellectual engagement as fulfillment (much lower extractiveness — the obligation is treated as met). messianic_suspension_reading resolves it by treating the obligation as suspended rather than owed (lower extractiveness — no active debt, only readiness-maintenance). symbolic_archive_reading dissolves the halakhic claim entirely (near-zero extractiveness — no live obligation, cultural memory only). Each sibling should be authored as its own file with its own stable ε; this file does not average or blend across them. All four link to each other via affects_constraints to preserve the kernel-family structure for contamination/coupling analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
