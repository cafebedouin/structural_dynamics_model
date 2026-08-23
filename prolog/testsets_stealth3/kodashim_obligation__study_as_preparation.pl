% ============================================================================
% CONSTRAINT STORY: kodashim_obligation__study_as_preparation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_obligation__study_as_preparation, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: kodashim_obligation__study_as_preparation
 *   human_readable: Kodashim Study Obligation as Preparatory Preservation (Restoration-Pending Reading)
 *   domain: religious/legal/textual_preservation
 *
 * SUMMARY:
 *   This file instantiates ONE reading of the kodashim_obligation kernel, per
 *   the committer frame: the preparation reading, under which sacrificial law
 *   remains fully binding although performance is impossible without the
 *   sanctuary, and the study mandate exists to transmit operative technical
 *   knowledge (ritual sequence, priestly duty, disqualification rules,
 *   sanctuary architecture) to a future generation that will perform it upon
 *   restoration. The epsilon referent is the standing arrangement under
 *   contest: the binding-but-unperformable study mandate as this reading
 *   itself assesses it. The sibling readings are separate constraints in
 *   separate files: study_as_performance (the cosmic function operates
 *   through study now; no deferred victim) and study_as_archive (the corpus
 *   documents a defunct system; no binding force, no restoration dependency).
 *   They are linked via network.affects_constraints and documented here only
 *   to fix the decomposition; no averaging or hedging across readings occurs
 *   in this file. KEY AGENTS (by structural relationship): -
 *   halakhic_authorities: Agenda setter (institutional/constrained) —
 *   administers the binding claim, sets curricula, answers the practical
 *   queries the suspension generates - current_observant_generation: Primary
 *   present target (moderate/constrained) — lives under laws it cannot
 *   execute, funds and defers to study it will not master - torah_scholars:
 *   Specialist intermediary (moderate/identity_locked) — carries the corpus,
 *   collects standing and livelihood, pays opportunity cost -
 *   future_jewish_generations: Designated beneficiary (powerless/trapped) —
 *   receives the corpus only upon restoration; cannot act, speak, or exit -
 *   kohanim_priestly_families: Dual-bound party (organized/identity_locked) —
 *   pays anticipation costs now, uniquely positioned to benefit at resumption
 *   - temple_movement_activists: Activation entrepreneurs (organized/mobile)
 *   — convert preparation into present action - secular_and_reform_jews,
 *   animal_welfare_advocates: Excluded objectors outside the halakhic
 *   conversation - academic_historians_of_religion: Analytical observer —
 *   corroborates the founding genealogy, takes no position on bindingness.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__study_as_preparation, 0.3).
domain_priors:suppression_score(kodashim_obligation__study_as_preparation, 0.2).
domain_priors:theater_ratio(kodashim_obligation__study_as_preparation, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, extractiveness, 0.3).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__study_as_preparation, scaffold).
narrative_ontology:human_readable(kodashim_obligation__study_as_preparation, "Kodashim Study Obligation as Preparatory Preservation (Restoration-Pending Reading)").
narrative_ontology:topic_domain(kodashim_obligation__study_as_preparation, "religious/legal/textual_preservation").

domain_priors:requires_active_enforcement(kodashim_obligation__study_as_preparation).
narrative_ontology:has_sunset_clause(kodashim_obligation__study_as_preparation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_preparation, 'ab58e0bd-960d-42a7-a823-8b945034a50d').
narrative_ontology:cs_kernel_codification('ab58e0bd-960d-42a7-a823-8b945034a50d', fixed_text).
narrative_ontology:cs_authority_grounding('ab58e0bd-960d-42a7-a823-8b945034a50d', lineage).
narrative_ontology:cs_interpretation_layer_present('ab58e0bd-960d-42a7-a823-8b945034a50d').
narrative_ontology:cs_reading_relation('ab58e0bd-960d-42a7-a823-8b945034a50d', kodashim_obligation__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('ab58e0bd-960d-42a7-a823-8b945034a50d', kodashim_obligation__study_as_archive, forecloses).
narrative_ontology:cs_axiom('ab58e0bd-960d-42a7-a823-8b945034a50d', foundational, sacrificial_command_remains_binding_without_temple).
narrative_ontology:cs_axiom_status(sacrificial_command_remains_binding_without_temple, holdable).
narrative_ontology:cs_axiom_grounding('ab58e0bd-960d-42a7-a823-8b945034a50d', sacrificial_command_remains_binding_without_temple, deontological).
narrative_ontology:cs_axiom('ab58e0bd-960d-42a7-a823-8b945034a50d', foundational, study_transmits_operational_competence_to_restoration).
narrative_ontology:cs_axiom_status(study_transmits_operational_competence_to_restoration, holdable).
narrative_ontology:cs_axiom_grounding('ab58e0bd-960d-42a7-a823-8b945034a50d', study_transmits_operational_competence_to_restoration, instrumental).
narrative_ontology:cs_axiom('ab58e0bd-960d-42a7-a823-8b945034a50d', secondary, performance_requires_physical_sanctuary).
narrative_ontology:cs_axiom_status(performance_requires_physical_sanctuary, holdable).
narrative_ontology:cs_axiom_grounding('ab58e0bd-960d-42a7-a823-8b945034a50d', performance_requires_physical_sanctuary, theological).
narrative_ontology:cs_reference_frame('ab58e0bd-960d-42a7-a823-8b945034a50d', binding_suspended_awaiting_restoration).
narrative_ontology:cs_drift_state('ab58e0bd-960d-42a7-a823-8b945034a50d', contemporary_temple_revival_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('ab58e0bd-960d-42a7-a823-8b945034a50d', '').
narrative_ontology:cs_kernel_id(kodashim_obligation__study_as_preparation, kodashim_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_preparation, future_jewish_generations).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_preparation, kohanim_priestly_families).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_preparation, torah_scholars).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_preparation, current_observant_generation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_preparation, temple_movement_activists).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_preparation, torah_scholars).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_preparation, kohanim_priestly_families).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_preparation, temple_restoration_doctrine).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_preparation, masoretic_transmission_principle).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_preparation, talmud_torah_substitution_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Codify and teach that the sacrificial laws remain binding; set the curricula through which Seder Kodshim circulates (yeshiva syllabi, Daf Yomi scheduling); adjudicate the practical questions the suspension generates (what replaces each offering, what purity rules still apply). Their authority rests on the system's continuity: repudiating the binding claim would dissolve the grounds of their own office, so exit from the arrangement is effectively closed to them even though they administer it.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, halakhic_authorities, agenda_setter,
    institutional, generational, constrained, global).

% Live under laws the tradition declares binding but which they cannot execute; rely on prayer substitution for repair the sacrificial system would otherwise provide; fund schools, synagogues, and study programs that carry the corpus; defer to specialists for knowledge they will not themselves master. Leaving the community carries familial, social, and identity costs that keep exit expensive but not impossible.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, current_observant_generation, payer,
    moderate, biographical, constrained, global).

% Dedicate years to mastering the sacrificial tractates; earn livelihood and standing as teachers and decisors within the study economy; bear the opportunity cost of expertise with no present application. Their scholarly self is constituted by engagement with the corpus: exit means leaving the beit midrash world entirely, not merely changing subjects.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, torah_scholars, beneficiary,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(kodashim_obligation__study_as_preparation, torah_scholars, payer).

% Designated recipients of the preserved corpus upon restoration: the cohort for whom the transmitted procedural knowledge would make resumed service possible. They cannot act, speak, consent, or decline; their position exists only as a projection of present arrangements, and their interests are voiced exclusively by proxies who draw standing from the representation.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, future_jewish_generations, beneficiary,
    powerless, civilizational, trapped, global).

% Maintain genealogical records and purity discipline across generations in anticipation of resumed service; carry distinctive present burdens (awareness of impurity contact, redemption obligations, heightened expectations of ritual knowledge). Positioned to officiate uniquely upon restoration, they are bound by the arrangement twice over and promised by it uniquely.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, kohanim_priestly_families, payer,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(kodashim_obligation__study_as_preparation, kohanim_priestly_families, beneficiary).

% Prepare vessels, train candidates for priestly service, publish restoration liturgies, and advocate presence on the Temple Mount. The study mandate supplies their project's legitimacy narrative and personnel pipeline. They chose this vocation and could redirect their energies to other causes, so their position inside the arrangement is voluntary rather than captive.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, temple_movement_activists, beneficiary,
    organized, generational, mobile, national).

% Stand outside the halakhic conversation that sustains the obligation; regard binding sacrificial law variously as historical artifact, ancestral memory, or an ethical problem. They are not addressed by the arrangement's enforcement and would contest its premises if seated, but the conversation's boundaries keep them outside it.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, secular_and_reform_jews, excluded,
    organized, biographical, mobile, global).

% Object prospectively to the resumption of animal slaughter. Because no performance currently occurs, their objection has no present purchase on the arrangement; it activates only at the restoration threshold the preparation reading awaits, which is exactly when the excluded seat's absence from deliberation would matter most.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, animal_welfare_advocates, excluded,
    organized, biographical, mobile, global).

% Document the post-70 textualization of sacrifice and the transmission history of the sacrificial corpus; corroborate the founding genealogy from outside the beneficiary set; take no position on whether the laws bind and bear none of the arrangement's costs.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, academic_historians_of_religion, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_obligation__study_as_preparation, diffuse).
narrative_ontology:fixing_cost_class(kodashim_obligation__study_as_preparation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves a complete procedural corpus of sacrificial law — ritual sequences, priestly duties, disqualification rules, sanctuary architecture — across an indefinite interval in which it has no executable use, so that the capability survives discontinuously and is available if performance resumes.
% TRANSFER_FUNCTION: Moves study-time and educational resources from the present generation into a maintained knowledge corpus designated for a future one; moves the availability of sacrificial atonement out of present reach into deferred restoration; confers scholarly standing and livelihood on the specialists who carry the corpus.
% ABSENT_VOICES: Secular and Reform Jews, and prospective animal-welfare objectors, sit outside the halakhic conversation that sustains the obligation. Most structurally absent is the designated beneficiary itself: future generations, whose interests are voiced only by present proxies — scholars and restoration activists — who draw standing and resources from the act of representation.
% DISAPPEARANCE_RATIONALE: Within the observing community the world rearranges: curricula lose a pillar, priestly anticipation disciplines lapse, restoration projects lose their legitimacy narrative and personnel pipeline, and the claim that the covenant remains fully binding loses its operative expression. Outside that community nothing changes: outsiders neither fund nor obey the arrangement, and several deny that it binds anyone at all — the parties genuinely dispute whether anything depends on it.
% FOUNDING_PROBLEM: After the Second Temple's destruction in 70 CE, a covenant structured around sacrificial service faced commandments it could no longer execute; the arrangement was built to preserve the operative knowledge and the binding claim across exile until restoration made performance possible again.
% FOUNDING_PROBLEM_CORROBORATION: Academic historians of religion and textual scholars outside the beneficiary set corroborate the founding problem: the rabbinic turn to textualizing sacrifice after 70 CE is a well-documented response to cultic loss. Those same external sources attest that the founding condition persists — the sanctuary is still absent and the law still unperformable — while the resolution timeline (restoration) is attested by no one outside the tradition and by no data channel at all.
narrative_ontology:disappearance_verdict(kodashim_obligation__study_as_preparation, contested).
narrative_ontology:founding_problem_status(kodashim_obligation__study_as_preparation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_obligation__study_as_preparation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kodashim_obligation__study_as_preparation, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_obligation__study_as_preparation, 0.3, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_obligation__study_as_preparation_tests).
:- end_tests(kodashim_obligation__study_as_preparation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.30): the arrangement imposes a real study burden and, under the tradition's own account, defers the current generation's access to sacrificial atonement, but the costs are bounded, sanctioned alternatives exist (prayer substitution is internal to the tradition), and the arrangement delivers meaning, continuity, and scholarly standing to its present participants. Suppression is low (0.20) and authored as a RAW STRUCTURAL PROPERTY, unscaled: the mandate compels within the community but forbids no alternative form of service; the engine scales only extractiveness. Theater ratio (0.25) reflects a genuine functional core (knowledge transmission) with a growing commemorative fringe: after nineteen centuries without resumption, a rising share of Kodshim engagement functions as identity performance (mastery honors, siyyum celebrations) rather than operational drill. Accessibility collapse is low (0.30): alternatives do not collapse — the sibling readings themselves remain live options, and prayer substitution is textually sanctioned. Resistance is moderate-low (0.30): Kodshim is famously the least-popular Talmudic order among students, and ethical discomfort with resumed sacrifice simmers below the surface, but no organized internal movement opposes the study mandate itself. The claimed type is scaffold: the arrangement's own justification is the TRANSITION — it declares itself valid only until restoration converts study back into performance, which is a sunset clause in everything but statutory form, hence has_sunset_clause: true paired with requires_active_enforcement: true (curricula, Daf Yomi scheduling, communal expectation actively maintain participation). The claim and the metrics are independent authored facts: I claim scaffold because the transitional justification is structural; I author the metrics as descriptive of actual operation; the engine computes per-seat types and may disagree. Temporal series run on ONE SHARED GRID (1180, 1500, 1800, 1947, 1967, 2000, 2026) with every tracked metric authored at every point. The mild extraction dip at 1947 reflects emancipation-era thinning of participation and institutional capacity; the postwar climb reflects the Daf Yomi maturation, day-school rebuilding, and Temple-movement revival. The suppression_requirement series is authored deliberately (not defaulted): enforcement machinery measurably hardened from the twentieth century onward — scheduled universal participation, standardized curricula, communal pressure — as exit pressure from secularization grew, which is an enforcement-capacity trajectory, not a change in the obligation's intrinsic coerciveness.
 *
 * PERSPECTIVAL GAP:
 *   The seats should classify differently. From the halakhic-authority seat the arrangement is faithful stewardship of a binding covenant: the sunset is promised, the costs are covenantal dues, the type experienced is transitional coordination. From the current-generation payer seat the same structure is a weight carried without its promised relief: laws bind, atonement waits, study serves a future the payer will not see. From the scholar seat the constraint is fused with identity — leaving it means leaving the beit midrash world entire, so even a low-extraction structure is experienced as inescapable. From the excluded seats the constraint is invisible: nothing binds them, nothing extracts from them, and several deny it binds anyone. The divergence is driven by exit asymmetry (mobile activists and outsiders versus identity_locked scholars and kohanim) and by the fact that the designated beneficiary cannot experience anything at all yet.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: future_jewish_generations (designed recipient of the corpus), kohanim_priestly_families (secondary; uniquely positioned officiants), torah_scholars (incidental collectors of standing and livelihood) — all derive low d toward the beneficiary end. Victims: current_observant_generation bears the deferred cosmic repair and the support burden — high d toward the target end, moderated by constrained (not trapped) exit. One override is declared: the future generation is the story's sole powerless atom, and the derivation's trapped-exit modulation would push its d toward the target end; but its trapped-ness is constitutive (it cannot act because it cannot yet act), not suppressive (nothing walls off exits it wants), and its declared beneficiary status should dominate. Hence power_atom powerless is overridden to d = 0.05. Kohanim's dual position is left to the derivation from the declared secondary role rather than overridden.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold claim guards against two mislabels. Against SNARE: the arrangement extracts little, suppresses little, and no seat captures the extraction — the preserved corpus is held in common and designated for the future; scholars' returns are wages for genuine pedagogic labor, not rents skimmed from the payer's burden. Against PITON: the administrator (halakhic authorities) bears real costs of maintaining the arrangement and sincerely holds its function to be live; the theater fringe is rising but subordinate; and the founding problem — law binding but unperformable, sanctuary absent — is still live, so mandatrophy is NOT resolved and mandatrophy_resolved is not declared. The sunset clause is the design, not decay: if restoration occurred, the constraint would dissolve by its own terms into performance. The genuine rot-risk runs the other way — if restoration belief died internally while study continued, preparation would decay into commemoration, which is precisely the archive sibling's claim; the corpus keeps the three readings apart so that this drift, were it to happen, registers as migration BETWEEN constraints rather than noise inside one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer,
    'Which reading of the kodashim_obligation kernel governs evaluation: preparation (instantiated here), performance (study enacts the cosmic function now), or archive (study preserves a defunct system''s memory without binding force)?',
    'Comparative analysis of halakhic discourse: curricular framing of Seder Kodshim, liturgical references to awaited avodah, restoration activism, and whether authorities describe study as counting-as-offering, as remembrance, or as readiness for resumption.',
    'Sibling readings change the victim set and epsilon structurally: the performance reading dissolves the deferred-cosmic-repair deprivation (the function operates now, extraction falls further); the archive reading removes binding force entirely (no obligation, no restoration dependency, extraction collapses toward informational cost).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer, conceptual, 'Committer structure: this story is one reading of a three-way kernel contest; sibling readings are separate constraints with different epsilon and victim sets.').

omega_variable(
    restoration_occurrence_horizon,
    'Will the anticipated restoration occur within any horizon that keeps preparation materially functional rather than asymptotically archival?',
    'No data channel resolves this inside or outside the framework; the story tracks proxy signals instead: restoration-movement growth, political access to the Temple Mount, and curricular emphasis on Kodshim as leading indicators.',
    'Non-occurrence drives theater_ratio upward and converges the constraint toward the archive sibling''s profile; occurrence validates the scaffold''s sunset clause and collapses extraction toward pure transition cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(restoration_occurrence_horizon, empirical, 'Whether the eschatological sunset condition ever fires, determining whether preparation is transitional support or indefinite maintenance.').

omega_variable(
    substitution_completeness,
    'Does prayer fully replace sacrificial atonement (emptying the deferred-victim structure), or is the replacement partial such that the current generation genuinely lacks a repair mechanism the binding law presupposes?',
    'Systematic analysis of the rabbinic reception of Hosea 14:3 and Talmudic statements on prayer replacing avodah, weighed against how decisors balance substitution against restoration longing.',
    'Full substitution empties base_properties.victims and pushes the constraint toward pure coordination with negligible extraction; partial substitution sustains the deferred-repair cost that anchors the payer seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_completeness, conceptual, 'Whether the tradition''s own substitution doctrine neutralizes the current generation''s deprivation.').

omega_variable(
    enforcement_motivation_composition,
    'Is the rising enforcement intensity coordination-maintenance (holding a costly practice against attrition as exit pressure grows) or extraction-defense (protecting institutional rents the study economy generates)?',
    'Motivation surveys across communities and budget analysis of study institutions: whether enforcement spending tracks attrition risk or revenue and status protection.',
    'An extraction-defense composition raises effective extraction for the payer seat and tilts the constraint toward tangled_rope; a coordination-maintenance composition keeps the scaffold reading intact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_motivation_composition, empirical, 'Composition of the enforcement ratchet visible in the suppression_requirement series since the modern era.').

omega_variable(
    restoration_internal_opposition,
    'Would a proximate restoration encounter enough internal ethical opposition (animal-welfare concern within the observing community itself) to contest the sunset condition rather than fulfill it cleanly?',
    'Track intra-Orthodox discourse on renewed sacrifice as restoration politics intensifies; survey rabbinic positions on the ethics of resumption.',
    'Material internal opposition would convert the scaffold''s clean sunset into a contested transition, pushing the constraint toward tangled_rope dynamics at the restoration threshold.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(restoration_internal_opposition, preference, 'Whether the constraint''s own beneficiary community would accept the transition its preparation builds toward.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_preparation, 1180, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kodashim_prep_study_tr_t1180, kodashim_obligation__study_as_preparation, theater_ratio, 1180, 0.08).
narrative_ontology:measurement(kodashim_prep_study_tr_t1500, kodashim_obligation__study_as_preparation, theater_ratio, 1500, 0.11).
narrative_ontology:measurement(kodashim_prep_study_tr_t1800, kodashim_obligation__study_as_preparation, theater_ratio, 1800, 0.15).
narrative_ontology:measurement(kodashim_prep_study_tr_t1947, kodashim_obligation__study_as_preparation, theater_ratio, 1947, 0.14).
narrative_ontology:measurement(kodashim_prep_study_tr_t1967, kodashim_obligation__study_as_preparation, theater_ratio, 1967, 0.19).
narrative_ontology:measurement(kodashim_prep_study_tr_t2000, kodashim_obligation__study_as_preparation, theater_ratio, 2000, 0.22).
narrative_ontology:measurement(kodashim_prep_study_tr_t2026, kodashim_obligation__study_as_preparation, theater_ratio, 2026, 0.25).

% Extraction over time
narrative_ontology:measurement(kodashim_prep_study_be_t1180, kodashim_obligation__study_as_preparation, base_extractiveness, 1180, 0.2).
narrative_ontology:measurement(kodashim_prep_study_be_t1500, kodashim_obligation__study_as_preparation, base_extractiveness, 1500, 0.23).
narrative_ontology:measurement(kodashim_prep_study_be_t1800, kodashim_obligation__study_as_preparation, base_extractiveness, 1800, 0.26).
narrative_ontology:measurement(kodashim_prep_study_be_t1947, kodashim_obligation__study_as_preparation, base_extractiveness, 1947, 0.25).
narrative_ontology:measurement(kodashim_prep_study_be_t1967, kodashim_obligation__study_as_preparation, base_extractiveness, 1967, 0.28).
narrative_ontology:measurement(kodashim_prep_study_be_t2000, kodashim_obligation__study_as_preparation, base_extractiveness, 2000, 0.3).
narrative_ontology:measurement(kodashim_prep_study_be_t2026, kodashim_obligation__study_as_preparation, base_extractiveness, 2026, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(kodashim_prep_study_su_t1180, kodashim_obligation__study_as_preparation, suppression_requirement, 1180, 0.1).
narrative_ontology:measurement(kodashim_prep_study_su_t1500, kodashim_obligation__study_as_preparation, suppression_requirement, 1500, 0.11).
narrative_ontology:measurement(kodashim_prep_study_su_t1800, kodashim_obligation__study_as_preparation, suppression_requirement, 1800, 0.13).
narrative_ontology:measurement(kodashim_prep_study_su_t1947, kodashim_obligation__study_as_preparation, suppression_requirement, 1947, 0.16).
narrative_ontology:measurement(kodashim_prep_study_su_t1967, kodashim_obligation__study_as_preparation, suppression_requirement, 1967, 0.17).
narrative_ontology:measurement(kodashim_prep_study_su_t2000, kodashim_obligation__study_as_preparation, suppression_requirement, 2000, 0.19).
narrative_ontology:measurement(kodashim_prep_study_su_t2026, kodashim_obligation__study_as_preparation, suppression_requirement, 2026, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_obligation__study_as_preparation, information_standard).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_preparation, study_as_performance).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_preparation, study_as_archive).

% DUAL FORMULATION NOTE:
% The colloquial label 'obligation to study the sacrificial laws' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints sharing one kernel. This file is the preparation reading (binding + suspended + resumable; epsilon 0.30; deferred-victim structure; scaffold). study_as_performance holds the function as operating now (epsilon lower; no deferred victim; no restoration dependency). study_as_archive denies binding force outright (epsilon near informational cost; no obligation; no designated beneficiary). The preparation reading sits upstream of the other two: its binding premise is what the performance reading spiritualizes and the archive reading rejects. Family members link mutually through network.affects_constraints; each file authors its own epsilon, beneficiaries, and victims without hedging across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kodashim_obligation__study_as_preparation, powerless, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
