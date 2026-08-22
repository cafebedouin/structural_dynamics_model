% ============================================================================
% CONSTRAINT STORY: quran_9_5_scope__abrogating_universal
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_9_5_scope__abrogating_universal, []).

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
 *   constraint_id: quran_9_5_scope__abrogating_universal
 *   human_readable: Nasikh Reading of 9:5 as Standing Universal-Offensive-Jihad Obligation
 *   domain: religious/political/legal
 *
 * SUMMARY:
 *   This story authors the ABROGATING_UNIVERSAL reading of the contested
 *   quran_9_5_scope kernel: the claim that verse 9:5 (the 'sword verse')
 *   functions as a nasikh (abrogating) instrument that cancels over a hundred
 *   earlier Quranic verses counseling patience, restraint, and defensive-only
 *   warfare, converting the earlier corpus into a dead letter and
 *   establishing an open-ended, standing legal obligation of offensive jihad
 *   against unconverted polytheists. This is one of three structurally
 *   distinct constraints sharing a text; the sibling readings
 *   (contextual_defensive, progressive_synthesis) are NOT part of this story
 *   — they are separate constraint files with their own ε, their own
 *   beneficiary/victim sets, and their own classification, linked via
 *   network.affects_constraints. Under THIS reading's own lights, the
 *   standing arrangement is a legal obligation binding on the Muslim polity
 *   to prosecute war until submission or conversion is achieved; ε is
 *   authored against that arrangement, not against any reformist alternative.
 *
 * KEY AGENTS:
 *   - expansionist_jihadist_movements: Primary beneficiary and enforcer (organized/arbitrage) — invokes doctrine to legitimate territorial and military action
 *   - abrogation_jurist_lineage: Doctrinal agenda-setter (institutional/identity_locked) — professional and interpretive identity bound to defending the abrogation ruling
 *   - non_muslim_polytheist_populations: Primary target (powerless/trapped) — categorically named as legitimate targets absent submission
 *   - comparative_islamic_scholars: Analytical observer (analytical/analytical) — traces doctrinal genealogy without institutional power to adjudicate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_9_5_scope__abrogating_universal, 0.88).
domain_priors:suppression_score(quran_9_5_scope__abrogating_universal, 0.9).
domain_priors:theater_ratio(quran_9_5_scope__abrogating_universal, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, extractiveness, 0.88).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_9_5_scope__abrogating_universal, snare).
narrative_ontology:human_readable(quran_9_5_scope__abrogating_universal, "Nasikh Reading of 9:5 as Standing Universal-Offensive-Jihad Obligation").
narrative_ontology:topic_domain(quran_9_5_scope__abrogating_universal, "religious/political/legal").

domain_priors:requires_active_enforcement(quran_9_5_scope__abrogating_universal).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_9_5_scope__abrogating_universal, '212c4300-83cf-4f55-8bde-048ee019812c').
narrative_ontology:cs_kernel_codification('212c4300-83cf-4f55-8bde-048ee019812c', fixed_text).
narrative_ontology:cs_authority_grounding('212c4300-83cf-4f55-8bde-048ee019812c', lineage).
narrative_ontology:cs_interpretation_layer_present('212c4300-83cf-4f55-8bde-048ee019812c').
narrative_ontology:cs_reading_relation('212c4300-83cf-4f55-8bde-048ee019812c', quran_9_5_scope__contextual_defensive, forecloses).
narrative_ontology:cs_reading_relation('212c4300-83cf-4f55-8bde-048ee019812c', quran_9_5_scope__progressive_synthesis, forecloses).
narrative_ontology:cs_axiom('212c4300-83cf-4f55-8bde-048ee019812c', foundational, nasikh_cancels_prior_peaceful_verses).
narrative_ontology:cs_axiom_status(nasikh_cancels_prior_peaceful_verses, holdable).
narrative_ontology:cs_axiom_grounding('212c4300-83cf-4f55-8bde-048ee019812c', nasikh_cancels_prior_peaceful_verses, conventional).
narrative_ontology:cs_axiom('212c4300-83cf-4f55-8bde-048ee019812c', foundational, sword_verse_establishes_standing_universal_obligation).
narrative_ontology:cs_axiom_status(sword_verse_establishes_standing_universal_obligation, holdable).
narrative_ontology:cs_axiom_grounding('212c4300-83cf-4f55-8bde-048ee019812c', sword_verse_establishes_standing_universal_obligation, conventional).
narrative_ontology:cs_reference_frame('212c4300-83cf-4f55-8bde-048ee019812c', classical_maximalist_abrogation_consensus).
narrative_ontology:cs_drift_state('212c4300-83cf-4f55-8bde-048ee019812c', contemporary_post_colonial_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('212c4300-83cf-4f55-8bde-048ee019812c', '').
narrative_ontology:cs_kernel_id(quran_9_5_scope__abrogating_universal, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__abrogating_universal, expansionist_jihadist_movements).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__abrogating_universal, abrogation_jurist_lineage).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__abrogating_universal, caliphal_conquest_administrators).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, non_muslim_polytheist_populations).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, dhimmi_subject_populations).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, muslim_dissenting_jurists).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, treaty_partner_communities).
narrative_ontology:constraint_vindicates(quran_9_5_scope__abrogating_universal, sword_verse_supremacy_doctrine).
narrative_ontology:constraint_vindicates(quran_9_5_scope__abrogating_universal, abrogation_of_peaceful_verses_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Cites 9:5 as universal, standing legal warrant for offensive military action against non-Muslims who have not submitted or converted, treating it as having abrogated every earlier verse counseling patience, tolerance, or defensive-only engagement. Uses the doctrine to recruit, to legitimate territorial expansion, and to override local jurists who counsel restraint. Can select whichever classical abrogation ruling supports the current campaign and discard the rest.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, expansionist_jihadist_movements, beneficiary,
    organized, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(quran_9_5_scope__abrogating_universal, expansionist_jihadist_movements, agenda_setter).

% A chain of classical exegetes (echoing positions attributed to al-Dahhak, some readings of Ibn Kathir's compilation of earlier authorities, and later Salafi-adjacent scholarship) who ruled that 9:5 abrogates over 100 earlier verses counseling peace, patience, or defensive war only. Their scholarly authority and institutional standing are bound up with defending the abrogation doctrine as correct fiqh; reversing the ruling would cost them standing within their own interpretive tradition.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, abrogation_jurist_lineage, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Historical and revivalist political authorities who convert the abrogation doctrine into state policy — using 'submission, conversion, or the sword' as the operative legal framework for expansion, taxation (jizya as submission marker), and territorial administration. Benefits from a maximalist reading because it removes legal ambiguity that would otherwise require negotiated coexistence with non-Muslim polities.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, caliphal_conquest_administrators, beneficiary,
    institutional, generational, arbitrage, continental).

% Under this reading, categorically named as legitimate military targets absent formal submission or conversion — there is no recognized standing to simply be left alone as an unconverted polytheist community. Their only recognized exits are conversion, formal capitulation, or violent conquest; the doctrine as read here removes 'peaceful coexistence while remaining unconverted' as a legally cognizable option.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, non_muslim_polytheist_populations, payer,
    powerless, biographical, trapped, global).

% Even where formal submission is accepted (as with People of the Book under a broader jurisprudential frame this reading sometimes extends by analogy), the underlying doctrine casts their continued unconverted existence as tolerated only through submission and payment, never as an equal or default-legitimate state. They bear ongoing legal, social, and fiscal subordination justified by reference to the same abrogation logic.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, dhimmi_subject_populations, payer,
    powerless, generational, constrained, regional).

% Scholars who read the classical corpus as supporting the contextual-defensive or progressive-synthesis positions are marginalized within institutions controlled by abrogationist lineages, sometimes accused of diluting doctrine or capitulating to modern sensibilities. Their contextualist arguments are excluded from the operative legal reasoning wherever the maximalist reading holds administrative or educational power.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, muslim_dissenting_jurists, excluded,
    moderate, biographical, constrained, national).

% Communities and polities that hold or seek treaty relationships with Muslim-majority states find that the abrogationist reading treats such treaties as provisional at best — valid only until strength permits abrogation of the truce itself, since the doctrine construes 9:5 as superseding earlier obligations toward peaceful coexistence. Their voice in defending treaty durability is structurally discounted by a framework that reads the sword verse as the final word.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, treaty_partner_communities, excluded,
    moderate, biographical, trapped, regional).

% Historians of tafsir and Islamic law who trace how the abrogation claim for 9:5 developed, which classical authorities endorsed or rejected it, and how it compares with the contextual-defensive and progressive-synthesis readings. They document the doctrine's usage without holding institutional power to adjudicate which reading is authoritative.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, comparative_islamic_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_9_5_scope__abrogating_universal, expansionist_jihadist_movements).
narrative_ontology:fixing_cost_class(quran_9_5_scope__abrogating_universal, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a maximally simple, unambiguous legal rule for organized military and political action — remove interpretive uncertainty about whether peace with unconverted non-Muslims is ever legally required, and coordinate expansionist movements and administrators around a single, non-negotiable standing directive.
% TRANSFER_FUNCTION: Moves physical safety, property, political autonomy, and legal standing away from non-Muslim and dissenting populations toward expansionist movements and administrators who can invoke universal jihad obligation as legal cover for conquest, taxation, and subordination.
% ABSENT_VOICES: Non-Muslim polytheist communities targeted by the doctrine have no standing within the interpretive tradition that produces the ruling. Dissenting Muslim jurists favoring contextual or progressive readings are present in the tradition but excluded from institutional authority wherever abrogationist lineages control religious education and legal administration.
% DISAPPEARANCE_RATIONALE: If this specific reading's authority collapsed, expansionist movements would lose their primary textual warrant for treating unconverted non-Muslims as standing legitimate military targets; treaty relationships would gain durability; dissenting jurists favoring contextual or progressive readings would gain institutional standing currently denied them. The underlying text (9:5) would remain, but its operative legal meaning would shift entirely toward the sibling readings.
% FOUNDING_PROBLEM: Classical jurists faced the interpretive problem of reconciling numerous Quranic verses with different tones toward non-Muslims (patience, defensive war, treaty-keeping, and 9:5's harsher language) into one coherent legal system usable by a state that needed clear rules for war and peace.
% FOUNDING_PROBLEM_CORROBORATION: Abrogationist jurists and expansionist movements attest the founding problem (need for a clear, standing rule) remains live and is correctly solved by universal abrogation. Comparative Islamic scholars and dissenting jurists — outside the beneficiary set — attest that the classical abrogation claim itself rests on contested and often weak isnad chains, that contemporaneous treaty practice by the Prophet's own community contradicts a universal reading, and that the 'standing obligation' framing reflects later imperial administrative convenience rather than an unavoidable textual conclusion.
narrative_ontology:disappearance_verdict(quran_9_5_scope__abrogating_universal, world_rearranges).
narrative_ontology:founding_problem_status(quran_9_5_scope__abrogating_universal, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_9_5_scope__abrogating_universal, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quran_9_5_scope__abrogating_universal, 'none', 1).
narrative_ontology:epsilon_provenance(quran_9_5_scope__abrogating_universal, 0.88, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_9_5_scope__abrogating_universal_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quran_9_5_scope__abrogating_universal, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quran_9_5_scope__abrogating_universal_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.88) and rising over the interval because, under this reading, the doctrine converts an already-contested individual verse into a totalizing standing obligation that removes the legal category of 'peaceful unconverted coexistence' entirely — every unconverted non-Muslim community becomes, in principle, a legitimate object of the obligation. Suppression is authored even higher (0.90) because the reading's persistence depends on actively suppressing the contextual-defensive and progressive-synthesis readings within institutions the abrogationist lineage controls — dissenting jurists are marginalized, not merely disagreed with. Theater ratio is kept comparatively low (0.25) because, as read here, the doctrine is not chiefly performative — where it holds administrative power it produces real military and legal consequences, though some invocation in contemporary rhetoric is more symbolic than operationally followed. Accessibility collapse is moderate-high (0.62), not maximal, because the sibling readings remain textually and historically available and are actively held by other scholarly communities — the collapse is institutional and political, not logical or textual.
 *
 * DIRECTIONALITY LOGIC:
 *   Expansionist movements and the abrogationist jurist lineage sit at the beneficiary end: they set and administer the rule, and the rule removes ambiguity that would otherwise constrain their action, so directionality derives low d for them. Non-Muslim polytheist populations and dhimmi populations sit at the target end: they are named beneficiaries of nothing and bear the costs the rule authorizes, so directionality derives high d, amplified by their trapped/powerless positioning. Dissenting Muslim jurists occupy an excluded rather than beneficiary or straightforward payer role — they are structurally locked out of the interpretive conversation that would let them contest the ruling from within, which is why the exit option is authored as constrained rather than trapped (they are not the ones physically targeted, but their professional standing is at risk).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — 7th-century need for a coherent legal rule reconciling varied Quranic statements on war and peace — is authored as contested rather than flatly dead or live, because the abrogationist lineage insists the problem (need for unambiguous standing rule) remains live in perpetuity, while outside corroborators (comparative scholars, dissenting jurists) argue the specific abrogation solution reflects a particular imperial-era administrative convenience rather than an unavoidable logical entailment of the text. The disappearance_verdict is world_rearranges precisely because concrete arrangements (military doctrine, treaty durability, legal treatment of non-Muslim populations) are built on this specific reading holding institutional authority; removing it would not eliminate Islamic law generally but would collapse this reading's operative claim to be the correct account of 9:5.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    abrogation_isnad_validity,
    'Do the classical chains of transmission (isnad) supporting the claim that 9:5 abrogates 100+ prior verses meet the same evidentiary standard applied elsewhere in usul al-fiqh, or is the abrogation claim itself a later doctrinal accretion with weaker sourcing than the verses it purports to cancel?',
    'Historical-critical isnad analysis comparing the abrogation claim''s sourcing against comparable fiqh rulings; comparative dating of tafsir manuscripts asserting the abrogation versus manuscripts preserving contextualist readings.',
    'If the abrogation claim rests on comparatively weak or late sourcing, this reading''s claim to represent authoritative classical consensus is substantially undermined, strengthening the contextual_defensive and progressive_synthesis siblings as the better-attested readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(abrogation_isnad_validity, empirical, 'Whether the nasikh claim for 9:5 is as well-attested as the verses it claims to cancel.').

omega_variable(
    which_reading_is_operative_where,
    'In any given contemporary or historical Muslim-majority polity, which of the three sibling readings is actually operative in law, education, and military doctrine, and how did that reading come to hold institutional power there?',
    'Comparative institutional analysis of religious curricula, state legal codes, and military doctrine documents across jurisdictions, tracing which reading each institution''s authority structure endorses.',
    'Where the abrogationist reading is not institutionally operative, this constraint''s stakeholders (as authored) do not apply to that context — the reading is a live possibility within the tradition, not a universal description of contemporary Islamic legal practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(which_reading_is_operative_where, empirical, 'Which reading holds actual institutional power in which jurisdiction.').

omega_variable(
    committer_frame_disagreement_location,
    'Where exactly do the three sibling readings of quran_9_5_scope disagree — is it about the historical circumstance of revelation (asbab al-nuzul), the validity and scope of nasikh as a hermeneutic device generally, or the theological status of 7th-century political directives relative to the Quran''s broader ethical arc?',
    'Structured comparison of each reading''s foundational axioms (see cs_structure.axioms across the three sibling files) to isolate whether the disagreement is empirical-historical, methodological (validity of abrogation as a device), or theological-hermeneutic.',
    'Locating the disagreement clarifies whether it is resolvable by historical evidence (asbab al-nuzul), by internal fiqh-methodological argument (nasikh validity), or is irreducibly a matter of theological commitment (ethical trajectory vs. literalist application) — each locates the omega differently for future analysis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_frame_disagreement_location, conceptual, 'Where the abrogating_universal, contextual_defensive, and progressive_synthesis readings actually diverge structurally.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__abrogating_universal, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_9_5_scope__abrogating_universal, theater_ratio, 0, 0.1).
narrative_ontology:measurement(qura_tr_t20, quran_9_5_scope__abrogating_universal, theater_ratio, 20, 0.13).
narrative_ontology:measurement(qura_tr_t40, quran_9_5_scope__abrogating_universal, theater_ratio, 40, 0.17).
narrative_ontology:measurement(qura_tr_t60, quran_9_5_scope__abrogating_universal, theater_ratio, 60, 0.2).
narrative_ontology:measurement(qura_tr_t80, quran_9_5_scope__abrogating_universal, theater_ratio, 80, 0.23).
narrative_ontology:measurement(qura_tr_t100, quran_9_5_scope__abrogating_universal, theater_ratio, 100, 0.25).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_9_5_scope__abrogating_universal, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(qura_be_t20, quran_9_5_scope__abrogating_universal, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(qura_be_t40, quran_9_5_scope__abrogating_universal, base_extractiveness, 40, 0.74).
narrative_ontology:measurement(qura_be_t60, quran_9_5_scope__abrogating_universal, base_extractiveness, 60, 0.79).
narrative_ontology:measurement(qura_be_t80, quran_9_5_scope__abrogating_universal, base_extractiveness, 80, 0.84).
narrative_ontology:measurement(qura_be_t100, quran_9_5_scope__abrogating_universal, base_extractiveness, 100, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_9_5_scope__abrogating_universal, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(qura_su_t20, quran_9_5_scope__abrogating_universal, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(qura_su_t40, quran_9_5_scope__abrogating_universal, suppression_requirement, 40, 0.78).
narrative_ontology:measurement(qura_su_t60, quran_9_5_scope__abrogating_universal, suppression_requirement, 60, 0.83).
narrative_ontology:measurement(qura_su_t80, quran_9_5_scope__abrogating_universal, suppression_requirement, 80, 0.87).
narrative_ontology:measurement(qura_su_t100, quran_9_5_scope__abrogating_universal, suppression_requirement, 100, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, quran_9_5_scope__contextual_defensive).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, quran_9_5_scope__progressive_synthesis).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling stories decomposing the natural-language label 'the meaning of Quran 9:5' per the ε-invariance principle: measuring the verse's legal scope under the abrogationist hermeneutic yields high, rising ε (0.88) with a broad victim set (all unconverted non-Muslims); measuring it under the contextual-defensive hermeneutic yields low ε concentrated on treaty-breaking belligerents only; measuring it under the progressive-synthesis hermeneutic yields near-zero ε as a historically time-bound directive. These are not the same constraint viewed from different angles — they have different victim sets, different beneficiaries, and different persistence mechanisms, so they are authored as three separate files linked here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
