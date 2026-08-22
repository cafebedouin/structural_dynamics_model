% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_obligation__study_as_archiving
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_obligation__study_as_archiving, []).

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
 *   constraint_id: temple_sacrifice_obligation__study_as_archiving
 *   human_readable: Temple Sacrifice Obligation (Study-as-Archiving Reading)
 *   domain: religious/legal
 *
 * SUMMARY:
 *   Rabbinic Judaism, following the Temple's destruction, needed to address
 *   the fact that a substantial portion of biblical commandments (the
 *   sacrificial service) became physically impossible to perform. The
 *   study_as_archiving reading resolves this by treating close textual study
 *   of sacrificial law as a preservation mechanism — keeping the knowledge
 *   intact against a hoped-for future restoration — while explicitly
 *   declining to claim that such study fulfills, suspends, or otherwise
 *   closes the obligation. The obligation stays open, permanently in arrears,
 *   for the entire interval. This produces a durable institutional role for a
 *   scholarly class whose subject matter can never be practiced, and a lay
 *   community that carries the doctrinal weight of unmet divine command
 *   indefinitely.
 *
 * KEY AGENTS:
 *   - halakhic_scholarly_class: institutional authority that rules the archiving doctrine into place and staffs the study apparatus it justifies
 *   - yeshiva_institutions: beneficiary institutions whose curricular and funding structures depend on Kodashim study retaining real doctrinal stakes
 *   - lay_observant_community: bears the liturgical and psychological cost of a permanently unfulfilled obligation
 *   - unfulfilled_divine_command: the non-agent structural victim — the commanded act itself, permanently unperformed under this reading
 *   - reform_and_reconstructionist_movements: excluded voice rejecting the premise of continued bindingness
 *   - comparative_religion_scholars: analytical observer with no stake in the outcome
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__study_as_archiving, 0.47).
domain_priors:suppression_score(temple_sacrifice_obligation__study_as_archiving, 0.58).
domain_priors:theater_ratio(temple_sacrifice_obligation__study_as_archiving, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, extractiveness, 0.47).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__study_as_archiving, tangled_rope).
narrative_ontology:human_readable(temple_sacrifice_obligation__study_as_archiving, "Temple Sacrifice Obligation (Study-as-Archiving Reading)").
narrative_ontology:topic_domain(temple_sacrifice_obligation__study_as_archiving, "religious/legal").

domain_priors:requires_active_enforcement(temple_sacrifice_obligation__study_as_archiving).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__study_as_archiving, '79ad96f5-20b4-4ecc-90b4-fb8e13b98f9a').
narrative_ontology:cs_kernel_codification('79ad96f5-20b4-4ecc-90b4-fb8e13b98f9a', fixed_text).
narrative_ontology:cs_authority_grounding('79ad96f5-20b4-4ecc-90b4-fb8e13b98f9a', lineage).
narrative_ontology:cs_interpretation_layer_present('79ad96f5-20b4-4ecc-90b4-fb8e13b98f9a').
narrative_ontology:cs_reading_relation('79ad96f5-20b4-4ecc-90b4-fb8e13b98f9a', temple_sacrifice_obligation__study_as_occupation, coexists_with).
narrative_ontology:cs_reading_relation('79ad96f5-20b4-4ecc-90b4-fb8e13b98f9a', temple_sacrifice_obligation__messianic_suspension, coexists_with).
narrative_ontology:cs_axiom('79ad96f5-20b4-4ecc-90b4-fb8e13b98f9a', foundational, study_preserves_but_does_not_discharge).
narrative_ontology:cs_axiom_status(study_preserves_but_does_not_discharge, holdable).
narrative_ontology:cs_axiom_grounding('79ad96f5-20b4-4ecc-90b4-fb8e13b98f9a', study_preserves_but_does_not_discharge, conventional).
narrative_ontology:cs_axiom('79ad96f5-20b4-4ecc-90b4-fb8e13b98f9a', foundational, obligation_remains_actively_outstanding_pending_restoration).
narrative_ontology:cs_axiom_status(obligation_remains_actively_outstanding_pending_restoration, holdable).
narrative_ontology:cs_axiom_grounding('79ad96f5-20b4-4ecc-90b4-fb8e13b98f9a', obligation_remains_actively_outstanding_pending_restoration, deontological).
narrative_ontology:cs_reference_frame('79ad96f5-20b4-4ecc-90b4-fb8e13b98f9a', temple_era_sacrificial_praxis).
narrative_ontology:cs_drift_state('79ad96f5-20b4-4ecc-90b4-fb8e13b98f9a', post_second_temple_rabbinic_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('79ad96f5-20b4-4ecc-90b4-fb8e13b98f9a', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__study_as_archiving, temple_sacrifice_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_archiving, yeshiva_institutions).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_archiving, halakhic_scholarly_class).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_archiving, textual_transmission_project).
narrative_ontology:constraint_victim(temple_sacrifice_obligation__study_as_archiving, unfulfilled_divine_command).
narrative_ontology:constraint_victim(temple_sacrifice_obligation__study_as_archiving, lay_observant_community).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_archiving, torah_study_equivalent_to_sacrifice_doctrine).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_archiving, perpetual_bindingness_of_temple_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rules on and transmits the position that studying the laws of sacrifice (seder Kodashim, Zevachim, Menachot, Rambam's Hilchot Beit HaBechirah) archives the obligation for a future restoration without discharging it now. This ruling sustains an entire curriculum, career track, and institutional prestige structure — yeshivot and scholars whose primary output is close textual analysis of a practice none of them will ever perform. Their exit from this framing would mean the deflation of a major branch of study into pure antiquarianism.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, halakhic_scholarly_class, agenda_setter,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_obligation__study_as_archiving, halakhic_scholarly_class, beneficiary).

% Fund and staff advanced study of sacrificial law as a core curricular pillar, justified by the archiving doctrine (learning it counts as preserving it for restoration, and the study itself carries independent merit under 'Torah study equals sacrifice'). Institutional funding, faculty positions, and publication pipelines depend on this branch of study retaining serious stakes rather than being read as historical curiosity.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, yeshiva_institutions, beneficiary,
    institutional, generational, constrained, global).

% Live under a legal system that continues to declare the sacrificial obligation binding, unperformed, and unperformable — mourning practices (Tisha B'Av, the three weeks, absence of certain simchas at full intensity) and daily liturgy (Amidah petitions for Temple restoration) encode the standing debt. They bear the psychological and ritual weight of an obligation they are told is real, active, and permanently unmet, mediated only by the promise that scholarly archiving keeps the account current until restoration.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, lay_observant_community, payer,
    moderate, biographical, constrained, global).

% The commanded act itself — offering as prescribed at the designated site — has gone unperformed for roughly two millennia. Under this reading, no substitute discharges it; it sits as a permanent structural deficit the archiving reading explicitly declines to close, distinguishing it from readings that claim suspension or occupation actually settle the account.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, unfulfilled_divine_command, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(temple_sacrifice_obligation__study_as_archiving, unfulfilled_divine_command).

% The eventual restored Temple and its service is the only event that would convert the archived knowledge into performed obligation. It has no voice in current halakhic deliberation and no timeline; its absence is precisely what the archiving doctrine is built around without ever having to specify.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, messianic_restoration_project, excluded,
    powerless, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(temple_sacrifice_obligation__study_as_archiving, messianic_restoration_project).

% Would argue the sacrificial system is not merely suspended pending restoration but obsolete as a form of worship, and that maintaining its binding status as unperformed debt is itself the problem rather than a fact to be archived around. They have exited the framework entirely rather than contesting it from within, and are not treated as an interlocutor in Orthodox halakhic discourse on this question.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, reform_and_reconstructionist_movements, excluded,
    organized, generational, mobile, global).

% Study how legal systems maintain the bindingness of unperformable commands across centuries, and can compare the archiving doctrine to structurally similar mechanisms in other traditions (dormant canon law, suspended treaty obligations) without a stake in the outcome.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, comparative_religion_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_obligation__study_as_archiving, halakhic_scholarly_class).
narrative_ontology:fixing_cost_class(temple_sacrifice_obligation__study_as_archiving, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves highly detailed technical knowledge of sacrificial procedure (species, procedure, priestly qualification, Temple architecture) across a nearly two-thousand-year gap in practice, so that if restoration ever occurred the knowledge to perform the service correctly would not have been lost.
% TRANSFER_FUNCTION: Moves scholarly attention, institutional funding, and doctrinal significance toward the class that studies sacrificial law, while moving onto the lay community the ongoing ritual and psychological weight of an obligation declared permanently outstanding and not dischargeable by any means available to them.
% ABSENT_VOICES: Reform and Reconstructionist movements, who reject the premise that the obligation remains binding at all, are not treated as parties to the halakhic conversation about how to relate to it. The unperformed command itself has no advocate other than the scholars who study it in the abstract.
% DISAPPEARANCE_RATIONALE: If the archiving doctrine were dropped, the scholarly class would need to either recharacterize Kodashim study as pure historical interest (deflating its institutional stakes) or adopt a sibling reading (occupation or suspension) that changes what the obligation currently requires. Orthodox institutional life would visibly rearrange around whichever replacement doctrine filled the gap; a fully secular observer might see little practical change since the sacrifices would not resume either way — hence contested rather than a clean verdict.
% FOUNDING_PROBLEM: After the Temple's destruction in 70 CE, rabbinic authorities needed a way to keep the sacrificial commandments legally and religiously alive — neither declaring them abrogated (which would concede permanent loss of covenantal practice) nor requiring impossible present performance.
% FOUNDING_PROBLEM_CORROBORATION: Historians of rabbinic Judaism (outside the yeshiva system) corroborate that the archiving move served a real continuity function after 70 CE and again after Bar Kokhba, citing the Mishnah's redaction of Kodashim as a documented response to exactly this problem — this is attested by academic Talmud scholarship, not solely by the beneficiary institutions themselves.
narrative_ontology:disappearance_verdict(temple_sacrifice_obligation__study_as_archiving, contested).
narrative_ontology:founding_problem_status(temple_sacrifice_obligation__study_as_archiving, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__study_as_archiving, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(temple_sacrifice_obligation__study_as_archiving, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_obligation__study_as_archiving, 0.47, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_obligation__study_as_archiving_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(temple_sacrifice_obligation__study_as_archiving, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(temple_sacrifice_obligation__study_as_archiving_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.47) rather than high: the coordination function (knowledge preservation) is genuine and non-trivial — without sustained close study, procedural knowledge of an intricate ritual system would plausibly degrade or be lost across two millennia. But there is a real asymmetric cost: the scholarly class gains stable institutional standing, prestige, and a permanent subject matter, while the lay community carries the doctrinal and liturgical burden of a debt that under this reading is never reduced by their own action, only by an event (restoration) entirely outside anyone's control. Suppression (0.58) reflects the authority structure's active maintenance of the obligation's binding status against readings that would relax it (reform positions, or even the sibling suspension reading, which the archiving reading resists by insisting the obligation is live and outstanding, not dormant). Theater ratio rises modestly over the interval (0.25 to 0.40) as study of Kodashim persists as a curricular category for over a thousand years without any prospect of practical application — the growth reflects the widening gap between the scale of scholarly investment and any near-term possibility of the knowledge being used.
 *
 * PERSPECTIVAL GAP:
 *   From the scholarly agenda-setter seat, this looks like a genuine coordination achievement — an unbroken chain of technical knowledge transmitted across catastrophic discontinuity. From the lay payer seat, the same arrangement can read as an open-ended, non-negotiable debt sustained by an authority structure that benefits from keeping the account open rather than resolving it via either of the sibling readings. The engine computing divergent per-seat types from this same structural data is the expected and correct output, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   The halakhic scholarly class and yeshiva institutions sit near the beneficiary end: they derive standing, funding, and identity from the archiving doctrine remaining authoritative, and their exit options are identity-locked or institutionally constrained rather than freely mobile. The lay observant community sits nearer the target end: they bear the ritual and psychological cost of the standing debt without deriving comparable institutional benefit, and their exit options are constrained by communal and religious identity. The unfulfilled command itself is a non-agent victim, included for structural completeness — declared with agent:false so it is excluded from directionality computation while still marking the point that the reading concedes something remains permanently unclosed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing legal/religious discontinuity after 70 CE) was genuinely live at founding and remains contested rather than clearly dead — corroborated by academic historians independent of the yeshiva system, which forecloses treating this as pure capture. But the doctrine's specific choice to leave the obligation unperformed and unperformable, rather than adopting suspension (which would relieve the standing debt) or occupation (which would treat study as sufficient), is a policy choice with an identifiable asymmetric institutional beneficiary. This is exactly the kind of case the tangled_rope classification exists for: a real coordination function (knowledge preservation) riding alongside a genuine, actively-enforced asymmetric cost structure (permanent unmet obligation borne diffusely by the laity, permanent institutional relevance captured concentratedly by the scholarly class).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    archiving_vs_occupation_boundary,
    'Is there a principled halakhic distinction between ''study preserves knowledge for future use'' (archiving) and ''study constitutes present occupation of the commandment'' (occupation), or is the choice between these readings itself a matter of institutional preference rather than textual compulsion?',
    'Comparative analysis of classical sources (Rambam, Ramban, later poskim) on whether talmud torah be-inyanei kodashim carries independent fulfillment value versus purely preservative value; examine whether any authority explicitly argues the distinction has practical legal consequences (e.g., for the blessing status of the study, or for communal obligation).',
    'If the distinction is textually underdetermined, the choice of archiving over occupation looks more like an institutional framing choice that happens to preserve the standing debt (and the associated liturgical mourning structure) rather than a compelled reading — strengthening the tangled_rope classification''s asymmetric-extraction leg.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(archiving_vs_occupation_boundary, conceptual, 'Whether archiving and occupation are textually distinct positions or institutionally motivated framings of the same underlying practice.').

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading (study_as_archiving) of the temple_sacrifice_obligation kernel. The sibling readings — messianic_suspension (obligation dormant, not outstanding) and study_as_occupation (study itself discharges/occupies the obligation) — would each change the victim structure materially: suspension removes the ''permanently unmet debt'' framing entirely, and occupation would move the scholarly class from beneficiary into a kind of surrogate-fulfiller role rather than pure archivist. Where exactly does the disagreement between archiving and its siblings live — is it a live legal dispute among current poskim, or has one reading become dominant in practice while the others persist only as theoretical alternatives?',
    'Survey contemporary halakhic literature and communal liturgical practice (e.g., whether Tisha B''Av observance and Amidah petitions are explained to congregants in archiving terms, suspension terms, or occupation terms) to locate which reading actually governs lived practice versus which are academic possibilities.',
    'If archiving is in fact the dominant lived reading (as the persistence of full mourning practices around Temple loss suggests, versus a suspension reading which might argue mourning should be muted since the debt isn''t ''active''), this constraint''s ε and victim structure describe the operative system, not merely one live option among equals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Locating where the archiving/suspension/occupation disagreement actually lives in contemporary practice versus theory.').

omega_variable(
    restoration_timeline_indeterminacy,
    'Because the archiving doctrine''s resolution condition (messianic restoration) has no timeline and no falsification condition, is the obligation''s ''binding but archived'' status distinguishable in practice from an obligation that will simply never be fulfilled?',
    'No empirical resolution mechanism exists by construction — this is the theological analog of an unfalsifiable claim. Track instead whether communal practice or halakhic discourse shows any sensitivity to the passage of time (e.g., does the intensity of mourning or the urgency of preservation activity change as centuries pass) that would indicate the community treats the indeterminacy as itself informative.',
    'If no such time-sensitivity appears across nearly two millennia, that supports reading the archiving doctrine as functionally permanent rather than genuinely provisional, which would push the classification toward a more purely extractive characterization of the standing debt.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(restoration_timeline_indeterminacy, conceptual, 'Whether the indefinitely deferred resolution condition renders the archiving/fulfillment distinction practically meaningless.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__study_as_archiving, 0, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(temp_tr_t0, observed).
narrative_ontology:measurement(temp_tr_t300, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 300, 0.28).
narrative_ontology:measurement_basis(temp_tr_t300, observed).
narrative_ontology:measurement(temp_tr_t700, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 700, 0.32).
narrative_ontology:measurement_basis(temp_tr_t700, observed).
narrative_ontology:measurement(temp_tr_t1100, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 1100, 0.35).
narrative_ontology:measurement_basis(temp_tr_t1100, observed).
narrative_ontology:measurement(temp_tr_t1500, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 1500, 0.38).
narrative_ontology:measurement_basis(temp_tr_t1500, observed).
narrative_ontology:measurement(temp_tr_t1950, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 1950, 0.4).
narrative_ontology:measurement_basis(temp_tr_t1950, observed).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(temp_be_t0, observed).
narrative_ontology:measurement(temp_be_t300, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 300, 0.35).
narrative_ontology:measurement_basis(temp_be_t300, observed).
narrative_ontology:measurement(temp_be_t700, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 700, 0.4).
narrative_ontology:measurement_basis(temp_be_t700, observed).
narrative_ontology:measurement(temp_be_t1100, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 1100, 0.43).
narrative_ontology:measurement_basis(temp_be_t1100, observed).
narrative_ontology:measurement(temp_be_t1500, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 1500, 0.45).
narrative_ontology:measurement_basis(temp_be_t1500, observed).
narrative_ontology:measurement(temp_be_t1950, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 1950, 0.47).
narrative_ontology:measurement_basis(temp_be_t1950, observed).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(temp_su_t0, observed).
narrative_ontology:measurement(temp_su_t300, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 300, 0.52).
narrative_ontology:measurement_basis(temp_su_t300, observed).
narrative_ontology:measurement(temp_su_t700, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 700, 0.54).
narrative_ontology:measurement_basis(temp_su_t700, observed).
narrative_ontology:measurement(temp_su_t1100, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 1100, 0.56).
narrative_ontology:measurement_basis(temp_su_t1100, observed).
narrative_ontology:measurement(temp_su_t1500, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 1500, 0.57).
narrative_ontology:measurement_basis(temp_su_t1500, observed).
narrative_ontology:measurement(temp_su_t1950, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 1950, 0.58).
narrative_ontology:measurement_basis(temp_su_t1950, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_obligation__study_as_archiving, identity_coordination).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_obligation__study_as_archiving, 0.1).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_archiving, temple_sacrifice_obligation__study_as_occupation).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_archiving, temple_sacrifice_obligation__messianic_suspension).

% DUAL FORMULATION NOTE:
% This is one of three sibling constraint stories decomposing the natural-language 'temple sacrifice obligation post-70CE' kernel, per the ε-invariance principle: study_as_archiving (this file, tangled_rope, moderate ε), study_as_occupation (higher expected ε if study is claimed to substitute for performance while the scholarly class captures the fulfillment-credit), and messianic_suspension (lower expected ε, since suspension removes the standing-debt framing entirely and the victim set shrinks accordingly). Each carries its own beneficiary/victim structure and its own claimed_type; they are linked here rather than merged because measuring the same underlying kernel by different doctrinal lenses yields materially different ε values — exactly the signal that indicates separate constraints rather than one constraint under different observables.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
