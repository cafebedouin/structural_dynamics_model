% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__prophetic_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eternal_marriage_covenant__prophetic_override_reading, []).

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
 *   constraint_id: eternal_marriage_covenant__prophetic_override_reading
 *   human_readable: Prophetic Override Reading of the Eternal Marriage Covenant (Continuing-Revelation Supersession)
 *   domain: religious/political_theology/commitment_systems
 *
 * SUMMARY:
 *   A canonized text (Doctrine and Covenants 132) commands plural marriage as
 *   an eternal covenant; a sovereign state criminalizes the practice and
 *   begins dissolving the church that transmits the text. In 1890 the living
 *   prophet declares the practice retired, presenting the declaration as
 *   superseding revelation received under existential necessity. This story
 *   instantiates the prophetic_override_reading of that event: prior
 *   revelation is supersable when circumstances require, and the
 *   church-survival constraint takes precedence over the practice mandate.
 *   The claim/metric independence rule is honored: the claimed type is what I
 *   believe structurally true of this arrangement, and the metrics describe
 *   its observed operation; the engine computes each seat's classification
 *   from the structural data. KEY AGENTS (by structural relationship):
 *   living_prophet_office (agenda-setter, institutional/constrained)
 *   administers the override; lds_church_institution (primary beneficiary,
 *   institutional/constrained) collects survival; quorum_of_twelve_apostles
 *   (dual beneficiary/payer, institutional/identity_locked) sustains and
 *   absorbs; rank_and_file_membership (primary target with residual benefit,
 *   organized/identity_locked) bears reversal costs and funds continuity;
 *   plural_marriage_households (concentrated targets, powerless/trapped) bear
 *   dissolution; post_manifesto_continuers (enforcement targets,
 *   powerless/identity_locked) bear discipline;
 *   united_states_federal_authorities (external co-agenda-setter,
 *   institutional/arbitrage) supplies and audits the pressure;
 *   religious_historians (analytical observers) hold no stake.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__prophetic_override_reading, 0.62).
domain_priors:suppression_score(eternal_marriage_covenant__prophetic_override_reading, 0.65).
domain_priors:theater_ratio(eternal_marriage_covenant__prophetic_override_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__prophetic_override_reading, tangled_rope).
narrative_ontology:human_readable(eternal_marriage_covenant__prophetic_override_reading, "Prophetic Override Reading of the Eternal Marriage Covenant (Continuing-Revelation Supersession)").
narrative_ontology:topic_domain(eternal_marriage_covenant__prophetic_override_reading, "religious/political_theology/commitment_systems").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__prophetic_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__prophetic_override_reading, '8eefc279-1b28-4159-b9e1-eb433bbec9ef').
narrative_ontology:cs_kernel_codification('8eefc279-1b28-4159-b9e1-eb433bbec9ef', fixed_text).
narrative_ontology:cs_authority_grounding('8eefc279-1b28-4159-b9e1-eb433bbec9ef', lineage).
narrative_ontology:cs_interpretation_layer_present('8eefc279-1b28-4159-b9e1-eb433bbec9ef').
narrative_ontology:cs_reading_relation('8eefc279-1b28-4159-b9e1-eb433bbec9ef', eternal_marriage_covenant__immutable_commandment_reading, forecloses).
narrative_ontology:cs_reading_relation('8eefc279-1b28-4159-b9e1-eb433bbec9ef', eternal_marriage_covenant__temporal_accommodation_reading, coexists_with).
narrative_ontology:cs_axiom('8eefc279-1b28-4159-b9e1-eb433bbec9ef', foundational, living_prophet_supersedes_prior_command).
narrative_ontology:cs_axiom_status(living_prophet_supersedes_prior_command, holdable).
narrative_ontology:cs_axiom_grounding('8eefc279-1b28-4159-b9e1-eb433bbec9ef', living_prophet_supersedes_prior_command, theological).
narrative_ontology:cs_axiom('8eefc279-1b28-4159-b9e1-eb433bbec9ef', secondary, exigency_activates_oracle_authority).
narrative_ontology:cs_axiom_status(exigency_activates_oracle_authority, holdable).
narrative_ontology:cs_axiom_grounding('8eefc279-1b28-4159-b9e1-eb433bbec9ef', exigency_activates_oracle_authority, instrumental).
narrative_ontology:cs_reference_frame('8eefc279-1b28-4159-b9e1-eb433bbec9ef', living_oracle_supremacy).
narrative_ontology:cs_drift_state('8eefc279-1b28-4159-b9e1-eb433bbec9ef', contemporary_canon_freeze_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8eefc279-1b28-4159-b9e1-eb433bbec9ef', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__prophetic_override_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__prophetic_override_reading, lds_church_institution).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__prophetic_override_reading, quorum_of_twelve_apostles).
narrative_ontology:constraint_victim(eternal_marriage_covenant__prophetic_override_reading, plural_marriage_households).
narrative_ontology:constraint_victim(eternal_marriage_covenant__prophetic_override_reading, rank_and_file_membership).
narrative_ontology:constraint_victim(eternal_marriage_covenant__prophetic_override_reading, post_manifesto_continuers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__prophetic_override_reading, rank_and_file_membership).
narrative_ontology:constraint_victim(eternal_marriage_covenant__prophetic_override_reading, quorum_of_twelve_apostles).
narrative_ontology:constraint_vindicates(eternal_marriage_covenant__prophetic_override_reading, continuing_revelation_doctrine).
narrative_ontology:constraint_vindicates(eternal_marriage_covenant__prophetic_override_reading, circumstantial_prophetic_supersession).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds sole authority to receive and announce binding revelation for the body. In 1890 issues the declaration retiring plural marriage, presenting it as revelation received under existential threat rather than concession. Decides alone what counts as circumstances requiring supersession; no other seat can verify the receipt or contest the judgment except by repudiating his authority outright. Leaving would mean vacating an office whose legitimacy rests on the very authority now being exercised.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, living_prophet_office, agenda_setter,
    institutional, generational, constrained, global).

% The corporate body facing disincorporation, asset forfeiture, and temple seizure under the 1887 federal statute. The declaration halts the confiscation schedule, opens the path to amnesty and Utah statehood, and restores legal footing. The institution keeps its buildings, membership rolls, treasury, and missionary apparatus; its continuity is the arrangement's principal material payoff.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, lds_church_institution, beneficiary,
    institutional, generational, constrained, continental).

% Senior governing council that sustains the declaration. Several members had spent careers defending plural marriage as eternal law; two are later pressured from their positions for continuing to authorize or perform plural sealings afterward. They retain office, standing, and succession rights while individually absorbing discipline for commitments made under the prior command.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, quorum_of_twelve_apostles, beneficiary,
    institutional, biographical, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(eternal_marriage_covenant__prophetic_override_reading, quorum_of_twelve_apostles, payer).

% Ordinary members who financed, sustained, and in thousands of cases practiced plural marriage under the prior commandment. After the declaration they are asked to grant the new instruction equal authority with the old one, with no access to the channel that distinguishes revelation from policy. They keep funding and staffing the institution throughout, absorb the social whiplash of reversed expectation, and inherit a surviving church. Leaving means forfeiting congregation, sealing networks, and the eternal framework their lives are ordered around.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, rank_and_file_membership, payer,
    organized, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(eternal_marriage_covenant__prophetic_override_reading, rank_and_file_membership, beneficiary).

% Families constituted under the earlier commandment, often spanning decades, multiple wives, and pooled economic life. The declaration dissolves their legal and social standing without consulting the wives, without restoring property invested in the practice, and with eternal-family expectations suddenly ungovernable. Wives commonly lack independent income; husbands face a choice between prosecution and household abandonment.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, plural_marriage_households, payer,
    powerless, biographical, trapped, regional).

% Members, including senior leaders, who conclude the earlier commandment remains binding and continue contracting or blessing plural marriages after the declaration. They face ecclesiastical discipline, excommunication after 1904, and in some cases renewed federal prosecution. Remaining in the main body costs them standing and eventually membership; leaving costs them the community and offices that constitute their identity, so they exit only by building rival congregations abroad or underground.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, post_manifesto_continuers, payer,
    powerless, biographical, identity_locked, regional).

% Statutes, courts, and marshals criminalizing plural marriage, dissolving the church's incorporation, and seizing assets. They supply the pressure that activates the declaration, then audit compliance through Senate hearings and prosecutorial discretion. They can amend the coercive environment at will and bear none of the arrangement's internal costs.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, united_states_federal_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Scholars reconstructing the sequence from diaries, council minutes, court records, and sealing registers. They hold no stake in the covenant's validity and publish findings that cut against every party's self-description, including the church's own retrospective accounts.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, religious_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eternal_marriage_covenant__prophetic_override_reading, lds_church_institution).
narrative_ontology:fixing_cost_class(eternal_marriage_covenant__prophetic_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solved an existential collective-action problem: a religious body whose canon commanded a practice its sovereign criminalized needed a single authoritative mechanism to redirect the whole community at once without fragmenting. The living prophet's superseding declaration supplied one decision point binding all members, preserving corporate continuity, temple operations, and communal coherence under terminal external pressure.
% TRANSFER_FUNCTION: Moves compliance and the accumulated value of prior sacrifice (households, property, reputations, careers) from individual members and plural families toward institutional continuity; moves discretionary religious authority upward to the living prophet's office; returns legal existence and property standing to the corporate church.
% ABSENT_VOICES: Plural wives, whose households the declaration dissolved, had no seat in the 1890 deliberations; the announcement issued over their families without consultation, and their objections survive mainly in private letters and later fundamentalist literature. Members who held the earlier commandment as immutable were present only as objects of discipline. Historians reconstruct their positions after the fact.
% DISAPPEARANCE_RATIONALE: Without the override mechanism the body splits between compliance and covenant: the corporation loses incorporation, temples, and treasury on the 1887 statute's schedule; thousands of members scatter into exile colonies or rival congregations; and the tradition's later adaptive episodes lose their instrument. Everything downstream of the institution's survival reorganizes.
% FOUNDING_PROBLEM: How a community whose canon commands a practice its sovereign criminalizes can persist: reconcile a binding eternal-law commitment with overwhelming external coercion without abandoning either the community or the authority structure that issues its commitments.
% FOUNDING_PROBLEM_CORROBORATION: Federal court records and the Edmunds-Tucker proceedings independently attest the coercive circumstance; Wilford Woodruff's contemporaneous diaries and his 1904 congressional-era testimony corroborate the survival rationale from inside; the mechanism's later reuse in the 1978 priesthood-expansion episode is attested by participants and by scholarship from outside the 1890 beneficiary generation, showing the founding problem recurs rather than having been solved once.
narrative_ontology:disappearance_verdict(eternal_marriage_covenant__prophetic_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(eternal_marriage_covenant__prophetic_override_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__prophetic_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(eternal_marriage_covenant__prophetic_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eternal_marriage_covenant__prophetic_override_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eternal_marriage_covenant__prophetic_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(eternal_marriage_covenant__prophetic_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eternal_marriage_covenant__prophetic_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.62: the arrangement produces a double-compliance asymmetry — members obeyed a costly command under full institutional authority, then obeyed its revocation under the same authority, with no access to the channel that distinguishes the two acts and no restitution for sunk sacrifice — while the survival payoff concentrates in the corporate institution. It stays short of snare territory because the coordination payoff is real and large: the community, including every future member who inherits it, persists. Suppression 0.65: enforcement infrastructure matured over the interval (disciplinary councils, the 1904 Second Manifesto with excommunication exposure, hearing-driven scrutiny), riding on identity fusion rather than primarily physical coercion. Theater_ratio 0.40: public compliance performance ran in parallel with continued private plural sealings for roughly a decade and a half, so a meaningful share of observable activity was audience management rather than governance; the ratio peaks mid-interval and recedes as the enforcement line hardens. Accessibility_collapse 0.72: once a committed member understands the override, the operative choice set collapses to accept-or-leave; exits exist and were taken (schisms occurred) but at identity-destruction cost. Resistance 0.55: organized continuation, resignations from the Twelve, and an eventual fundamentalist breakaway meet the arrangement with real but ultimately non-regime-threatening opposition. The measurement series run on one shared time grid (points 0, 5, 10, 15, 19, 25) so every tracked metric carries an authored value at every examined time point; the suppression_requirement series is authored because the story's enforcement picture genuinely changes — ecclesiastical enforcement capacity is built up over the interval while federal pressure relaxes.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the agenda-setter seat the arrangement is faithful stewardship: the same revelatory authority that bound the people releases them from destruction, and the covenant's substance is preserved by bending its practice. From the plural-household seat the same act is the binding authority dissolving what it created, without consultation or restitution — the costs land on those with the least voice. From the continuer seat it is betrayal ratified by discipline. From the historian's seat the sequence is legible as adaptive institutional survival with distributed costs. None of these perceptions is authored as a classification; the engine derives them from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (lds_church_institution, quorum_of_twelve_apostles) sit near the beneficiary end: the arrangement subsidizes their continuity, and the quorum's dual position (office retained, individual discipline absorbed) tempers but does not invert it. Declared victims sit near the target end, amplified by exit conditions: plural_marriage_households are trapped with the highest effective extraction; rank_and_file_membership are identity_locked, which pulls them toward the full-target end despite their residual inheritance benefit; post_manifesto_continuers combine victim status with identity lock and bear enforcement directly. united_states_federal_authorities are structurally subsidized by the arrangement (it delivers the compliance they demanded) but are deliberately NOT declared beneficiaries — they are external to the covenant structure — so their seat relies on the canonical fallback rather than a structural derivation; no directionality override is used because overrides key on power atoms that are shared across seats with genuinely different relationships here.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling immutable-law commitment with sovereign coercion — is live, demonstrated by the mechanism's reuse in 1978; nothing about the arrangement is vestigial, so piton is structurally wrong despite the mid-interval theater spike, which was transitional audience management rather than decay. The tangled_rope claim keeps both truths visible: labeling this pure coordination erases the dissolved households and disciplined continuers; labeling it pure extraction erases the genuine survival function that every subsequent generation inherits. The mandatrophy interview confirms the arrangement has not outlived its mandate — it has changed mandates, from protecting a practice to protecting the body that once practiced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates the prophetic_override_reading of the eternal_marriage_covenant kernel. Is circumstantial supersession — rather than immutable permanence or suspension-without-renunciation — the correct structural reading of the 1890 declaration?',
    'Comparative doctrinal tracing: which reading the institution''s own later invocations implicitly adopt (the 1978 priesthood-expansion episode invoked new revelation, not suspension language); fundamentalist schism literature; First Presidency explanations issued between 1890 and 1910.',
    'If the accommodation reading governs, covenant validity persists and member-side extraction drops materially; if the immutable reading governs, the override itself becomes the violation and the victim set expands to include the covenant''s own integrity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Which of the three readings of the eternal-marriage kernel structures the arrangement.').

omega_variable(
    supersession_scope_ambiguity,
    'Did the 1890 override supersede the practice mandate only, or the covenant commandment itself — were pre-1890 plural sealings retroactively voided, suspended, or left eternally valid?',
    'Post-1890 sealing-register practice, First Presidency rulings on existing plural families, and temple policy toward widows and children of plural marriages.',
    'Full supersession maximizes the double-compliance extraction profile (an authority that demanded sacrifice then revoked its object); practice-only suspension lowers extraction attributable to the override and shifts weight toward the accommodation sibling reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supersession_scope_ambiguity, conceptual, 'Scope of supersession: commandment versus practice.').

omega_variable(
    coercion_revelation_provenance,
    'Was the superseding revelation causally produced by federal coercive pressure, by independent spiritual conviction, or both inseparably?',
    'Chronology cross-reference of Woodruff''s contemporaneous diaries and letters against court decisions and confiscation schedules, plus internal council minutes predating public announcements.',
    'A purely coercion-driven override reads as capitulation under duress, weakening the coordination framing and raising effective suppression attribution; conviction-driven receipt strengthens coordination and lowers effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_revelation_provenance, empirical, 'Causal provenance of the activating revelation.').

omega_variable(
    dual_audience_theater_ambiguity,
    'Was the post-Manifesto period of continued private plural sealings alongside public compliance a managed dual-audience performance intrinsic to running the transition, or evidence that the declaration was not sincerely received as revelation?',
    'Cross-referencing public addresses and congressional testimony against sealed-record evidence of post-1890 marriages and leadership correspondence.',
    'The managed-performance reading raises the functional meaning of the theater ratio (bridge management during enforcement buildup, not decay); the insincerity reading pushes the arrangement toward enforcement-heavy extraction and strengthens the snare-adjacent interpretation for the continuer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_audience_theater_ambiguity, empirical, 'Whether the post-Manifesto dual practice was managed transition or bad faith.').

omega_variable(
    suppression_mechanism_split,
    'Is member-side compliance with the override maintained by structural ecclesiastical discipline, by internalized testimony identity, or both — and in what proportion?',
    'Post-exit trajectories: members who left for fundamentalist groups retained identical covenant commitments, indicating the internalized component persists after exit; disciplinary records quantify the structural component.',
    'If largely internalized, suppression outlasts the enforcement infrastructure and effective suppression exceeds measured disciplinary activity, making payer-seat exit less real than the structural record alone suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_split, empirical, 'Structural versus internalized mechanisms sustaining member compliance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__prophetic_override_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eter_tr_t0, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(eter_tr_t0, observed).
narrative_ontology:measurement(eter_tr_t5, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 5, 0.44).
narrative_ontology:measurement_basis(eter_tr_t5, observed).
narrative_ontology:measurement(eter_tr_t10, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 10, 0.51).
narrative_ontology:measurement_basis(eter_tr_t10, observed).
narrative_ontology:measurement(eter_tr_t15, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 15, 0.52).
narrative_ontology:measurement_basis(eter_tr_t15, observed).
narrative_ontology:measurement(eter_tr_t19, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 19, 0.45).
narrative_ontology:measurement_basis(eter_tr_t19, observed).
narrative_ontology:measurement(eter_tr_t25, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(eter_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(eter_be_t0, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(eter_be_t0, observed).
narrative_ontology:measurement(eter_be_t5, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 5, 0.46).
narrative_ontology:measurement_basis(eter_be_t5, observed).
narrative_ontology:measurement(eter_be_t10, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement_basis(eter_be_t10, observed).
narrative_ontology:measurement(eter_be_t15, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement_basis(eter_be_t15, observed).
narrative_ontology:measurement(eter_be_t19, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 19, 0.61).
narrative_ontology:measurement_basis(eter_be_t19, observed).
narrative_ontology:measurement(eter_be_t25, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement_basis(eter_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(eter_su_t0, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 0, 0.34).
narrative_ontology:measurement_basis(eter_su_t0, observed).
narrative_ontology:measurement(eter_su_t5, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 5, 0.54).
narrative_ontology:measurement_basis(eter_su_t5, observed).
narrative_ontology:measurement(eter_su_t10, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 10, 0.59).
narrative_ontology:measurement_basis(eter_su_t10, observed).
narrative_ontology:measurement(eter_su_t15, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement_basis(eter_su_t15, observed).
narrative_ontology:measurement(eter_su_t19, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 19, 0.66).
narrative_ontology:measurement_basis(eter_su_t19, observed).
narrative_ontology:measurement(eter_su_t25, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 25, 0.65).
narrative_ontology:measurement_basis(eter_su_t25, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=25
narrative_ontology:measurement(eter_grid_01, eternal_marriage_covenant__prophetic_override_reading, accessibility_collapse(class), 0, 0.5).
narrative_ontology:measurement(eter_grid_02, eternal_marriage_covenant__prophetic_override_reading, accessibility_collapse(class), 25, 0.75).
narrative_ontology:measurement(eter_grid_03, eternal_marriage_covenant__prophetic_override_reading, accessibility_collapse(individual), 0, 0.6).
narrative_ontology:measurement(eter_grid_04, eternal_marriage_covenant__prophetic_override_reading, accessibility_collapse(individual), 25, 0.65).
narrative_ontology:measurement(eter_grid_05, eternal_marriage_covenant__prophetic_override_reading, accessibility_collapse(organizational), 0, 0.45).
narrative_ontology:measurement(eter_grid_06, eternal_marriage_covenant__prophetic_override_reading, accessibility_collapse(organizational), 25, 0.65).
narrative_ontology:measurement(eter_grid_07, eternal_marriage_covenant__prophetic_override_reading, accessibility_collapse(structural), 0, 0.55).
narrative_ontology:measurement(eter_grid_08, eternal_marriage_covenant__prophetic_override_reading, accessibility_collapse(structural), 25, 0.7).
narrative_ontology:measurement(eter_grid_09, eternal_marriage_covenant__prophetic_override_reading, resistance(class), 0, 0.55).
narrative_ontology:measurement(eter_grid_10, eternal_marriage_covenant__prophetic_override_reading, resistance(class), 25, 0.4).
narrative_ontology:measurement(eter_grid_11, eternal_marriage_covenant__prophetic_override_reading, resistance(individual), 0, 0.6).
narrative_ontology:measurement(eter_grid_12, eternal_marriage_covenant__prophetic_override_reading, resistance(individual), 25, 0.3).
narrative_ontology:measurement(eter_grid_13, eternal_marriage_covenant__prophetic_override_reading, resistance(organizational), 0, 0.65).
narrative_ontology:measurement(eter_grid_14, eternal_marriage_covenant__prophetic_override_reading, resistance(organizational), 25, 0.2).
narrative_ontology:measurement(eter_grid_15, eternal_marriage_covenant__prophetic_override_reading, resistance(structural), 0, 0.6).
narrative_ontology:measurement(eter_grid_16, eternal_marriage_covenant__prophetic_override_reading, resistance(structural), 25, 0.3).
narrative_ontology:measurement(eter_grid_17, eternal_marriage_covenant__prophetic_override_reading, stakes_inflation(class), 0, 0.7).
narrative_ontology:measurement(eter_grid_18, eternal_marriage_covenant__prophetic_override_reading, stakes_inflation(class), 25, 0.55).
narrative_ontology:measurement(eter_grid_19, eternal_marriage_covenant__prophetic_override_reading, stakes_inflation(individual), 0, 0.75).
narrative_ontology:measurement(eter_grid_20, eternal_marriage_covenant__prophetic_override_reading, stakes_inflation(individual), 25, 0.5).
narrative_ontology:measurement(eter_grid_21, eternal_marriage_covenant__prophetic_override_reading, stakes_inflation(organizational), 0, 0.8).
narrative_ontology:measurement(eter_grid_22, eternal_marriage_covenant__prophetic_override_reading, stakes_inflation(organizational), 25, 0.35).
narrative_ontology:measurement(eter_grid_23, eternal_marriage_covenant__prophetic_override_reading, stakes_inflation(structural), 0, 0.75).
narrative_ontology:measurement(eter_grid_24, eternal_marriage_covenant__prophetic_override_reading, stakes_inflation(structural), 25, 0.4).
narrative_ontology:measurement(eter_grid_25, eternal_marriage_covenant__prophetic_override_reading, suppression(class), 0, 0.45).
narrative_ontology:measurement(eter_grid_26, eternal_marriage_covenant__prophetic_override_reading, suppression(class), 25, 0.55).
narrative_ontology:measurement(eter_grid_27, eternal_marriage_covenant__prophetic_override_reading, suppression(individual), 0, 0.4).
narrative_ontology:measurement(eter_grid_28, eternal_marriage_covenant__prophetic_override_reading, suppression(individual), 25, 0.5).
narrative_ontology:measurement(eter_grid_29, eternal_marriage_covenant__prophetic_override_reading, suppression(organizational), 0, 0.3).
narrative_ontology:measurement(eter_grid_30, eternal_marriage_covenant__prophetic_override_reading, suppression(organizational), 25, 0.65).
narrative_ontology:measurement(eter_grid_31, eternal_marriage_covenant__prophetic_override_reading, suppression(structural), 0, 0.7).
narrative_ontology:measurement(eter_grid_32, eternal_marriage_covenant__prophetic_override_reading, suppression(structural), 25, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__prophetic_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(eternal_marriage_covenant__prophetic_override_reading, eternal_marriage_covenant__immutable_commandment_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__prophetic_override_reading, eternal_marriage_covenant__temporal_accommodation_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Manifesto ended polygamy' decomposes into three structurally distinct readings of one kernel (D&C 132 as fixed canonical text): immutable permanence, circumstantial prophetic supersession (this file), and suspension-without-renunciation. Each gets its own epsilon, victim set, and classification because the referent — what the arrangement does to pre-existing covenant obligations — differs under each reading. The override reading inherits the canonized text's authority (fixed_text kernel, lineage transmission) and exerts structural pressure on the accommodation reading's legitimacy conditions; the immutable sibling is linked as the reading this one forecloses. All family members are linked per the constraint-family rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
