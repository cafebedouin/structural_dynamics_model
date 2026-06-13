% ============================================================================
% CONSTRAINT STORY: vatican_ii_magisterial_authority__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_magisterial_authority__continuity_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: vatican_ii_magisterial_authority__continuity_reading
 *   human_readable: Vatican II Magisterial Authority — Continuity Reading
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   The Council Vatican II (1962–1965) convened to modernize the Church while
 *   preserving its essential doctrine. Its conciliar documents are written in
 *   a characteristic style: texts affirm both traditional doctrine and reform
 *   initiatives, often through ambiguous formulations permitting multiple
 *   interpretations. The continuity reading claims that Vatican II represents
 *   organic development within unbroken tradition — no rupture with prior
 *   magisterium. This constraint instantiates that reading as an
 *   institutional authority frame. It declares: conciliar texts must be read
 *   in continuity with pre-conciliar teaching; the 'spirit of Vatican II'
 *   (reform impulses not explicitly textually grounded) is unauthorized;
 *   Latin preservation mandate (SC §36) is binding; religious freedom
 *   doctrine (DH) is reconcilable with the Syllabus of Errors through the
 *   thesis/hypothesis distinction or development-of-doctrine principle. The
 *   constraint's persistence depends on active enforcement of this
 *   interpretive frame by magisterial authority, gatekeeping of approved
 *   exegesis, and suppression of alternative readings as unauthorized. The
 *   measurement series tracks extractiveness (the constraint's ability to
 *   exclude reformist interpretation and benefit the continuity faction),
 *   theater ratio (the proportion of enforcement activity dedicated to
 *   performing continuity rather than substantive textual argument), and
 *   suppression requirement (the intensity of institutional power needed to
 *   hold the frame against accumulating anomalies). This is one reading of a
 *   contested kernel; two sibling readings (rupture_reading,
 *   composite_overdetermination_reading) instantiate fundamentally different
 *   interpretations of the same conciliar texts.
 *
 * KEY AGENTS:
 *   - magisterial_continuity_faction: Sets and administers the interpretive frame; institutional power; identity-locked (their authority depends on continuity being true)
 *   - traditionalist_hierarchy: Institutional beneficiary; identity-locked (legitimacy depends on magisterial coherence across centuries)
 *   - reformist_clergy_theologians: Organized payers; constrained exit (career dependence on magisterial approval); face systematic exclusion of their textual readings as 'unauthorized'
 *   - lay_liberalization_movements: Organized payers; subordinated to magisterial correction; movements for married clergy, contraception reform, women's ordination are delegitimized as 'spirit' rather than 'letter'
 *   - rupture_reading_adherents: Excluded; trapped (their reading is framed as logically incoherent with magisterial infallibility)
 *   - composite_reading_scholars: Excluded; their reading threatens to dissolve the binary the continuity frame depends on
 *   - magisterial_authority_office: Observer seat; can assess whether constraint persists due to hermeneutical truth or institutional power maintaining advantageous frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_magisterial_authority__continuity_reading, 0.62).
domain_priors:suppression_score(vatican_ii_magisterial_authority__continuity_reading, 0.71).
domain_priors:theater_ratio(vatican_ii_magisterial_authority__continuity_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_magisterial_authority__continuity_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_magisterial_authority__continuity_reading, "Vatican II Magisterial Authority — Continuity Reading").
narrative_ontology:topic_domain(vatican_ii_magisterial_authority__continuity_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_magisterial_authority__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority__continuity_reading, 'a4f8e583-e0d4-4b28-bfe1-3b772f720905').
narrative_ontology:cs_kernel_codification('a4f8e583-e0d4-4b28-bfe1-3b772f720905', fixed_text).
narrative_ontology:cs_authority_grounding('a4f8e583-e0d4-4b28-bfe1-3b772f720905', lineage).
narrative_ontology:cs_interpretation_layer_present('a4f8e583-e0d4-4b28-bfe1-3b772f720905').
narrative_ontology:cs_reading_relation('a4f8e583-e0d4-4b28-bfe1-3b772f720905', vatican_ii_magisterial_authority__rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('a4f8e583-e0d4-4b28-bfe1-3b772f720905', vatican_ii_magisterial_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('a4f8e583-e0d4-4b28-bfe1-3b772f720905', foundational, magisterial_doctrinal_non_contradiction).
narrative_ontology:cs_axiom_status(magisterial_doctrinal_non_contradiction, holdable).
narrative_ontology:cs_axiom_grounding('a4f8e583-e0d4-4b28-bfe1-3b772f720905', magisterial_doctrinal_non_contradiction, deontological).
narrative_ontology:cs_axiom('a4f8e583-e0d4-4b28-bfe1-3b772f720905', secondary, conciliar_texts_univocal_continuity).
narrative_ontology:cs_axiom_status(conciliar_texts_univocal_continuity, holdable).
narrative_ontology:cs_axiom_grounding('a4f8e583-e0d4-4b28-bfe1-3b772f720905', conciliar_texts_univocal_continuity, empirically_contingent).
narrative_ontology:cs_reference_frame('a4f8e583-e0d4-4b28-bfe1-3b772f720905', pre_conciliar_magisterial_authority).
narrative_ontology:cs_drift_state('a4f8e583-e0d4-4b28-bfe1-3b772f720905', post_vatican_ii_enforcement_hardening, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a4f8e583-e0d4-4b28-bfe1-3b772f720905', '').
narrative_ontology:cs_kernel_id(vatican_ii_magisterial_authority__continuity_reading, vatican_ii_magisterial_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__continuity_reading, magisterial_continuity_faction).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__continuity_reading, traditionalist_hierarchy).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__continuity_reading, reformist_clergy_theologians).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__continuity_reading, lay_liberalization_movements).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_magisterial_authority__continuity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_magisterial_authority__continuity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_magisterial_authority__continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_magisterial_authority__continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_magisterial_authority__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.35 at Council (t=1962, projected: pre-enforcement baseline) to 0.62 at present (t=2025, observed). This trajectory reflects the constraint's hardening over 63 years: initial ambiguity in conciliar texts allowed multiple readings; magisterial enforcement of the continuity frame became explicit, systematic, and increasingly costly to challenge. Theater ratio climbs sharply from 0.15 to 0.58, indicating growing reliance on performative enforcement: doctrinal corrections, hermeneutical encyclicals (Benedict XVI's 'hermeneutics of continuity,' John Paul II's catechesis), and institutional gatekeeping increasingly dominate over textual argument itself. The rising theater ratio is diagnostic: where extractiveness plateaus (0.62 from 2010 onward) while theater continues climbing, the constraint's primary function has shifted from substantive hermeneutical coordination to maintenance of institutional interpretive authority. Suppression requirement climbs from 0.42 to 0.71, reflecting intensifying institutional force needed to hold the frame: career sanctions on reformist theologians, prohibition of certain exegetical approaches in seminaries, suppression of 'unauthorized' interpretations via Congregation for the Doctrine of the Faith, formal reinterpretation of ambiguous texts to foreclose rupture readings. The constraint is Tangled Rope rather than Rope: it coordinates the Church's hermeneutical self-understanding (genuine coordination function) AND extracts interpretive authority away from reformist theologians and lay movements (asymmetric extraction requiring active enforcement). The measurement grid captures one unified timeline; every metric is authored at every time point so temporal alignment is preserved.
 *
 * PERSPECTIVAL GAP:
 *   The magisterial continuity faction and traditionalist hierarchy perceive this constraint as genuine coordination: preserving doctrinal coherence is a real institutional need, and the continuity reading solves it authoritatively. Their directionality is beneficiary (d ~ 0.15–0.25): they benefit from the constraint, but they do not read themselves as extracting from it — they read themselves as serving a real doctrinal function. Reformist clergy and lay movements perceive the same constraint as enforced extraction: their theological work is systematically excluded, their interpretations are delegitimized as 'unauthorized,' their institutional advancement is contingent on continuity-frame adherence. Their directionality is target (d ~ 0.75–0.85): they bear the cost of interpretive closure without controlling the outcome. The engine computes these divergent directionalities from the beneficiary/victim declarations and exit options (identity_locked for the hierarchy, constrained for the reformists); the divergence is the measurement the framework exists to take. A constraint whose claim and metrics diverge (claimed Rope, computed Tangled Rope) is not an error — it is how institutional extraction hiding behind coordination rhetoric is detected.
 *
 * DIRECTIONALITY LOGIC:
 *   The magisterial_continuity_faction sits as institutional-power beneficiary with identity-locked exit: their authority position depends on the continuity reading being true and enforced. Their directionality d derives from: (1) declared role as beneficiary (they collect interpretive authority and institutional legitimacy), (2) institutional power atom (high structural capacity to shape outcomes), (3) identity-locked exit (the framework's truth is constitutive of their institutional self-concept). This yields d ~ 0.20 (beneficiary-side). The traditionalist_hierarchy sits similarly: d ~ 0.18. The reformist_clergy_theologians sit as organized-power payer with constrained exit: their directionality derives from (1) declared role as payer (they bear the cost of interpretive exclusion and career constraint), (2) organized (not institutional) power (collective but subordinate to magisterial hierarchy), (3) constrained exit (theological careers depend on magisterial employment, publication channels, ordination). This yields d ~ 0.78 (target-side). The lay_liberalization_movements sit similarly: d ~ 0.76. The rupture_reading_adherents sit as organized-power excluded with trapped exit: d ~ 0.88 (full target, trapped, excluded from authority). No directionality overrides are needed; the structural derivation from beneficiary/victim + power + exit produces accurate d values. The directionality divergence across seats (beneficiary faction at 0.20, victim faction at 0.78) is the seat-specific classification divergence the engine will compute — the constraint looks like rope (beneficiary-driven coordination) from the agenda-setter seat and like snare (extraction with cover story) from the payer seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live and real: the Church must maintain doctrinal coherence across institutional change. Vatican II's ambiguous formulations genuinely pose a coherence challenge. The continuity reading is one defensible response to that problem. However, the constraint's persistence depends increasingly on suppression and theater (rising from 0.42 to 0.71, rising from 0.15 to 0.58) rather than on textual evidence or philosophical argument alone. The theater_ratio trajectory is diagnostic: if the constraint persisted because the continuity reading was textually obvious or philosophically unassailable, theater would remain low and suppression would remain proportional to institutional coordination cost. Rising theater indicates that energies have shifted toward enforcement of the reading against accumulating textual anomalies (DH 2 on religious freedom; Unitatis Redintegratio on separated churches; Nostra Aetate on non-Christian religions; Presbyterorum Ordinis on clerical celibacy openness) rather than toward new exegetical argument supporting continuity. The constraint exhibits the pattern of a Tangled Rope with mandatrophy onset: the coordination function (doctrinal coherence) is real, but the extraction (suppression of alternative readings, career gatekeeping) has grown to dominate the functional picture. If the reformist clergy movements organize politically (which they show signs of doing: conservative Vatican II scholarship networks, underground ordination movements, post-Francis momentum for reform) and the magisterial authority cannot suppres them purely institutionally, the constraint will face the challenge of whether its persistence depends on truth (in which case open debate would vindicate it) or on power (in which case suppression's failure would expose it as snare). That test is not yet resolved; the reading remains live rather than conclusively mandatropic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_continuity_vs_institutional_power,
    'Is the continuity reading sustained by the texts'' genuine affirmation of pre-conciliar doctrine, or by institutional enforcement of a hermeneutical frame that suppresses anomalous readings?',
    'Comparative textual analysis by scholars outside the magisterial authority structure; systematic documentation of which readings are excluded from approved seminaries and publication channels; examination of whether textual argument or institutional sanction explains the reading''s persistence.',
    'If textual, the constraint is genuine coordination and the reading deserves status as authentic development. If institutional, the constraint is enforced extraction and the continuity reading is a cover story for magisterial power-preservation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_continuity_vs_institutional_power, empirical, 'Whether continuity reading is grounded in texts or in institutional suppression of alternatives.').

omega_variable(
    hermeneutical_foreclosure_logic,
    'Does the continuity reading logically FORECLOSE the rupture reading (as the magisterial faction claims), or do the two readings COEXIST as genuinely live alternative interpretations of ambiguous texts?',
    'Formal textual analysis showing whether conciliar passages unambiguously support one reading or permit both; examination of whether the foreclosure claim depends on a prior commitment to magisterial infallibility (circular: infallibility requires continuity, therefore rupture is impossible) or on textual evidence itself.',
    'If rupture is logically foreclosed, the continuity reading is justified in treating it as error. If the readings coexist, the Council is overdetermined and the magisterium has endorsed incommensurable ecclesiologies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hermeneutical_foreclosure_logic, conceptual, 'Whether continuity reading''s foreclosure of rupture reading is textually grounded or logically circular.').

omega_variable(
    identity_lock_internalization,
    'To what extent is the magisterial hierarchy''s commitment to the continuity reading internalized (they believe continuity is true independently of institutional interest) versus structurally locked (their institutional self-concept depends on continuity being true)?',
    'Post-enforcement trajectories: if magisterial actors shifted to supporting rupture reading and their institutional legitimacy and self-concept remained intact, the commitment was contingent; if they faced identity crisis and institutional collapse, the commitment was internalized through identity fusion.',
    'High internalization (identity-lock) would indicate genuine conviction and low extractiveness. High structural lock (institutional dependence) would indicate extraction risk: the reading persists not because it is true but because institutional actors cannot abandon it without losing their legitimacy position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_internalization, empirical, 'Whether hierarchy''s continuity commitment is belief-driven or identity-locked.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.71) structural (external barriers: publishing prohibition, career gates, disciplinary action) or internalized (reformist theologians self-censor because they have internalized the continuity frame as the legitimate reading)?',
    'Post-suppression trajectories: document whether reformist scholars who exit magisterial employment or institutional constraints continue to self-censor or openly embrace rupture/composite readings. If they openly adopt alternative readings post-exit, suppression was primarily structural. If they continue self-censoring, suppression was substantially internalized.',
    'Structural suppression indicates the constraint''s force depends on active enforcement and will weaken if enforcement capacity erodes. Internalized suppression indicates the constraint carries with the agent after exit and will persist even if institutional barriers are removed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural institutional enforcement or internalized cognitive frame.').

omega_variable(
    composite_reading_threat_assessment,
    'If the composite-overdetermination reading (Vatican II encodes incompatible visions) were fully developed and academically mainstreamed, would it dissolve the binary the continuity reading depends on and thereby delegitimize the continuity reading as a false resolution of real textual tension?',
    'Academic promotion of composite reading in theology journals and dissertations; magisterial response (prohibition, endorsement, or reinterpretation); empirical tracking of whether composite reading gains scholarly consensus.',
    'If composite reading gains traction, the continuity reading loses its rhetorical force as THE authentic reading — it becomes one interpretive option among three equally valid ones. The constraint''s extractive power depends on foreclosing this outcome.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(composite_reading_threat_assessment, conceptual, 'Whether composite reading threatens the continuity reading''s exclusive interpretive authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_magisterial_authority__continuity_reading, 1962, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1962, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 1962, 0.15).
narrative_ontology:measurement(vati_tr_t1978, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 1978, 0.32).
narrative_ontology:measurement(vati_tr_t1995, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 1995, 0.48).
narrative_ontology:measurement(vati_tr_t2010, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 2010, 0.56).
narrative_ontology:measurement(vati_tr_t2025, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 2025, 0.58).

% Extraction over time
narrative_ontology:measurement(vati_be_t1962, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 1962, 0.35).
narrative_ontology:measurement(vati_be_t1978, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 1978, 0.48).
narrative_ontology:measurement(vati_be_t1995, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 1995, 0.58).
narrative_ontology:measurement(vati_be_t2010, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(vati_be_t2025, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 2025, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1962, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 1962, 0.42).
narrative_ontology:measurement(vati_su_t1978, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 1978, 0.54).
narrative_ontology:measurement(vati_su_t1995, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 1995, 0.64).
narrative_ontology:measurement(vati_su_t2010, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(vati_su_t2025, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 2025, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_magisterial_authority__continuity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vatican_ii_magisterial_authority__continuity_reading, 0.14).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__continuity_reading, vatican_ii_magisterial_authority__rupture_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__continuity_reading, vatican_ii_magisterial_authority__composite_overdetermination_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__continuity_reading, magisterial_infallibility_doctrine).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__continuity_reading, development_of_doctrine_hermeneutic).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__continuity_reading, papal_interpretive_supremacy).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__continuity_reading, post_conciliar_reform_suppression).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested kernel vatican_ii_magisterial_authority. All three readings emerge from the same conciliar texts (Vatican II documents, 1962–1965) but instantiate different constraints because they declare different structural beneficiaries, victims, and interpretive authorities. The continuity_reading (this story) claims Vatican II is development, not rupture, and benefits the magisterial hierarchy by preserving doctrinal coherence. The rupture_reading claims Vatican II is fundamental break and would benefit reformist theologians and lay movements by legitimizing substantial doctrinal change. The composite_overdetermination_reading claims Vatican II encodes incompatible visions and would dissolve both prior readings by showing them as arbitrary resolutions of real textual tension. Each reading instantiates a different ε (extractiveness, suppression) because each reading's persistence depends on different institutional mechanisms and beneficiary arrangements. The three stories are linked by shared kernel identity and by the fact that enforcement of one reading (continuity) requires suppression of the others. The network.affects_constraints array in each story links to its siblings; the engine's contamination-propagation system will track how degradation in one reading's enforcement (for example, if magisterial suppression capacity erodes) will affect the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
