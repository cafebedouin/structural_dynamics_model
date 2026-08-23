% ============================================================================
% CONSTRAINT STORY: first_amendment_speech_protection__harm_limited_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_first_amendment_speech_protection__harm_limited_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: first_amendment_speech_protection__harm_limited_reading
 *   human_readable: First Amendment Harm-Limited Reading
 *   domain: constitutional_law/political_philosophy/speech_regulation
 *
 * SUMMARY:
 *   This constraint story captures the 'harm-limited' reading of First
 *   Amendment speech protection — the view that protection yields when speech
 *   causes demonstrable unconsented-to harm. It is one reading of the
 *   contested kernel 'first_amendment_speech_protection,' alongside
 *   absolutist and categorical-balancing readings. The harm-limited reading
 *   emerged mid-20th century (Chaplinsky, Beauharnais, later refined in RAV
 *   v. St. Paul, Virginia v. Black, and lower-court harassment jurisprudence)
 *   as a response to the perceived failure of absolutism to address
 *   speech-as-violence. The constraint extracts from speakers whose
 *   expression crosses the harm threshold (injunctions, damages, criminal
 *   penalties) and coordinates by channeling regulation through a harm
 *   standard rather than content-based censorship. Beneficiaries are
 *   vulnerable minorities and targeted communities; victims are speakers
 *   found to cause harm. Courts and regulators are agenda-setters. The
 *   metrics reflect a constraint that has grown more extractive and more
 *   actively enforced over eight decades, with rising theater as the harm
 *   standard expands into contested territory (offensive speech,
 *   misinformation, dissent).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__harm_limited_reading, 0.58).
domain_priors:suppression_score(first_amendment_speech_protection__harm_limited_reading, 0.62).
domain_priors:theater_ratio(first_amendment_speech_protection__harm_limited_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__harm_limited_reading, tangled_rope).
narrative_ontology:human_readable(first_amendment_speech_protection__harm_limited_reading, "First Amendment Harm-Limited Reading").
narrative_ontology:topic_domain(first_amendment_speech_protection__harm_limited_reading, "constitutional_law/political_philosophy/speech_regulation").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__harm_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__harm_limited_reading, 'cf7c431f-93c2-4d8d-b245-0249a106ea9f').
narrative_ontology:cs_kernel_codification('cf7c431f-93c2-4d8d-b245-0249a106ea9f', fixed_text).
narrative_ontology:cs_authority_grounding('cf7c431f-93c2-4d8d-b245-0249a106ea9f', lineage).
narrative_ontology:cs_interpretation_layer_present('cf7c431f-93c2-4d8d-b245-0249a106ea9f').
narrative_ontology:cs_reading_relation('cf7c431f-93c2-4d8d-b245-0249a106ea9f', first_amendment_speech_protection__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('cf7c431f-93c2-4d8d-b245-0249a106ea9f', first_amendment_speech_protection__categorical_balancing_reading, coexists_with).
narrative_ontology:cs_axiom('cf7c431f-93c2-4d8d-b245-0249a106ea9f', foundational, harm_triggers_regulation).
narrative_ontology:cs_axiom_status(harm_triggers_regulation, holdable).
narrative_ontology:cs_axiom_grounding('cf7c431f-93c2-4d8d-b245-0249a106ea9f', harm_triggers_regulation, empirically_contingent).
narrative_ontology:cs_axiom('cf7c431f-93c2-4d8d-b245-0249a106ea9f', foundational, unconsented_harm_standard).
narrative_ontology:cs_axiom_status(unconsented_harm_standard, holdable).
narrative_ontology:cs_axiom_grounding('cf7c431f-93c2-4d8d-b245-0249a106ea9f', unconsented_harm_standard, deontological).
narrative_ontology:cs_reference_frame('cf7c431f-93c2-4d8d-b245-0249a106ea9f', harm_limited_framework).
narrative_ontology:cs_drift_state('cf7c431f-93c2-4d8d-b245-0249a106ea9f', contemporary_digital_harm_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('cf7c431f-93c2-4d8d-b245-0249a106ea9f', '').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__harm_limited_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__harm_limited_reading, vulnerable_minorities).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__harm_limited_reading, targeted_communities).
narrative_ontology:constraint_victim(first_amendment_speech_protection__harm_limited_reading, speakers_causing_harm).
narrative_ontology:constraint_victim(first_amendment_speech_protection__harm_limited_reading, controversial_speakers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__harm_limited_reading, controversial_speakers).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__harm_limited_reading, government_regulators).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__harm_limited_reading, harm_principle_in_speech_regulation).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__harm_limited_reading, unconsented_harm_as_regulatory_trigger).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Groups historically targeted by hate speech, harassment, and coordinated defamation campaigns. Gain protection when courts recognize demonstrable unconsented-to harm from speech. Their exit from harm is constrained by identity — they cannot stop being members of targeted groups. They rely on legal enforcement to create safer public discourse.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, vulnerable_minorities, beneficiary,
    moderate, generational, constrained, national).

% Communities (religious, ethnic, gender/sexual minorities) that experience speech-based harm as collective injury. Benefit when the harm standard enables injunctions, damages, or platform obligations. Often excluded from the doctrinal debate about 'what counts as harm' — courts and litigants define the standard without their direct participation.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, targeted_communities, beneficiary,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(first_amendment_speech_protection__harm_limited_reading, targeted_communities, excluded).

% Speakers whose expression is found to cause demonstrable unconsented-to harm — hate speakers, harassers, doxxers, revenge porn distributors. Bear costs: injunctions, damages, criminal penalties, platform bans. Their exit is constrained because the harm finding attaches to the speech act itself; they cannot easily 'speak differently' without changing the message. Some face professional and social consequences beyond legal penalties.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, speakers_causing_harm, payer,
    moderate, biographical, constrained, national).

% Speakers engaging in provocative but not clearly harmful expression — political extremists, offensive satirists, boundary-pushing artists. Risk being swept into the harm category by expansive judicial readings. Their exit is more mobile: they can modulate speech, change platforms, or accept smaller audiences. They also benefit indirectly when the harm standard is narrowly drawn, preserving space for edge cases.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, controversial_speakers, payer,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(first_amendment_speech_protection__harm_limited_reading, controversial_speakers, beneficiary).

% Judicial branch that adjudicates 'demonstrable unconsented-to harm' — defines the evidentiary standard, weighs speech value against harm, crafts remedies. Sets the agenda by choosing which cases to hear and how to frame the harm inquiry. Their institutional legitimacy depends on the standard appearing principled rather than outcome-driven.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, courts, agenda_setter,
    institutional, generational, analytical, national).

% Executive agencies (FCC, FTC, state AGs) that enforce speech regulations justified by the harm standard. Gain regulatory authority and enforcement tools. Can arbitrage across jurisdictions — forum-shop for favorable courts, coordinate with platforms. Their interest is in expanding the actionable harm category to increase regulatory reach.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, government_regulators, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(first_amendment_speech_protection__harm_limited_reading, government_regulators, beneficiary).

% ACLU, FIRE, EFF and similar groups that litigate, file amicus briefs, and mobilize public opinion against harm-based speech restrictions. They do not directly collect or pay — they observe and contest the constraint's operation from a structural-integrity perspective. Their analytical seat is professionalized: they track doctrine, predict expansion, and intervene at inflection points.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, civil_liberties_organizations, observer,
    organized, generational, analytical, national).

% First Amendment absolutists who hold that 'no law' means no law. They would object to any harm-based limitation but are structurally excluded from the harm-limited framework's internal logic — the framework defines them out by treating harm as a trump. Their exit is trapped: they cannot accept the harm standard without abandoning their core commitment, and they cannot escape the framework's legal force.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, absolutist_advocates, excluded,
    organized, generational, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a principled boundary for speech regulation: only speech causing demonstrable unconsented-to harm loses protection. Solves the coordination problem of distinguishing protected expression from actionable harm without granting government open-ended censorship power.
% TRANSFER_FUNCTION: Moves regulatory authority over speech from speakers to courts and regulators when harm is proven. Transfers the cost of harmful speech from targeted communities (who bear psychological, social, economic injury) to speakers (who face legal consequences) and to the state (which bears enforcement cost).
% ABSENT_VOICES: The communities most affected by speech harm are often absent from the doctrinal construction of 'demonstrable harm' — courts and elite litigants define the standard. Future speakers chilled by uncertain boundaries are absent. Platforms that must implement harm-based moderation at scale are absent from the constitutional calculus.
% DISAPPEARANCE_RATIONALE: If the harm-limited reading vanished overnight, courts would revert to either absolutist or categorical-balancing frameworks. Hate speech, harassment, and revenge porn would lose specific doctrinal footholds for regulation. Platforms would lose legal clarity on takedown obligations. Vulnerable communities would lose a recognized legal pathway. The speech-regulation landscape would reorganize around whichever sibling reading filled the vacuum.
% FOUNDING_PROBLEM: The absolutist reading left vulnerable communities with no remedy for speech that functions as violence — hate speech inciting violence, targeted harassment destroying livelihoods, nonconsensual intimate imagery. The categorical balancing reading produced unpredictable, ad hoc outcomes. The harm-limited reading was built to create a principled, evidence-based trigger for regulation that respects speech presumptively but yields when harm is proven.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians (e.g., Geneva Crenshaw, Robert Post) document the mid-20th century shift from absolutist to harm-aware frameworks as responsive to civil rights era harms. Critical race theorists (Matsuda, Lawrence, Delgado, Crenshaw) corroborate that the founding problem was real and pressing — their work is from outside the judicial beneficiaries. However, contemporary originalist scholars contest whether the founding problem was ever the Constitution's concern, arguing the First Amendment was designed precisely to prevent harm-based exceptions.
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__harm_limited_reading, world_rearranges).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__harm_limited_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__harm_limited_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(first_amendment_speech_protection__harm_limited_reading, 'none', 1).
narrative_ontology:epsilon_provenance(first_amendment_speech_protection__harm_limited_reading, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(first_amendment_speech_protection__harm_limited_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(first_amendment_speech_protection__harm_limited_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(first_amendment_speech_protection__harm_limited_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects that speakers losing protection face real consequences — criminal penalties, civil liability, platform removal — and the category of 'harm-causing speech' has expanded from fighting words to hate speech, harassment, true threats, revenge porn, and increasingly to misinformation and extremist rhetoric. Suppression (0.62) is substantial because the constraint depends on active enforcement: courts must find harm, regulators must act, platforms must comply. Theater (0.28) is moderate — the harm inquiry is genuine in core cases (true threats, doxxing) but becomes performative when stretched to cover political dissent or offensive ideas. Accessibility collapse (0.52) is middling: speakers can sometimes avoid harm findings by modifying expression, but identity-locked speakers (extremists, committed ideologues) cannot. Resistance (0.55) is significant: civil liberties groups, originalist judges, and political movements push back against expansion.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat (vulnerable minorities), the constraint is a Rope — genuine coordination solving a collective-action problem (speech-as-violence) with minimal coercive overhead. From the payer seat (speakers causing harm), it is a Snare — the harm standard is cover for suppressing disfavored views, alternatives (counterspeech, private ordering) are suppressed, enforcement is coercive. From the agenda-setter seat (courts), it is a Tangled Rope — real coordination function (principled boundary) but asymmetric extraction (speakers pay, communities gain). The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Vulnerable minorities and targeted communities are structural beneficiaries (d near 0.15) — the constraint subsidizes their protection from speech-as-harm. Speakers causing harm are structural targets (d near 0.85) — the constraint extracts their expressive liberty when harm is proven. Controversial speakers sit near symmetric (d ~0.5) — they risk harm findings but also benefit from a clear standard that preserves non-harmful edge speech. Courts and regulators are agenda-setters with analytical/institutional exit (d ~0.3 for courts as neutral arbiters, ~0.2 for regulators who gain authority). Absolutist advocates are excluded and identity-locked (d ~0.9) — they cannot accept the framework without abandoning their premise.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (speech-as-violence with no remedy) was live in 1942-1990 and remains live for core harms (true threats, harassment, nonconsensual imagery). But the constraint has expanded into territory where the founding problem is contested: political misinformation, offensive ideas, dissent labeled 'harmful.' The mandate has partially outlived its function — the coordination core remains but extraction has accumulated around the edges. This is exactly the mandatrophy signature: a once-principled boundary now serving as a vehicle for regulatory expansion. The 'contested' status of the founding problem captures this tension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_demonstrability_ambiguity,
    'Is ''demonstrable unconsented-to harm'' an empirically verifiable standard or an inherently contestable normative judgment?',
    'Track judicial opinions over time: if harm findings converge on measurable criteria (threats, stalking, doxxing, revenge porn), the standard is empirically grounded; if they diverge into ''dignitary harm,'' ''psychic injury,'' ''epistemic harm,'' it is normatively contestable.',
    'If empirically grounded, the constraint''s extraction is bounded and its coordination function dominates (Tangled Rope). If normatively contestable, expansion is unbounded and extraction dominates (Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_demonstrability_ambiguity, conceptual, 'Whether the harm standard has objective epistemic content or is an elastic normative vessel.').

omega_variable(
    kernel_reading_relationship,
    'Does the harm-limited reading structurally foreclose the absolutist reading, or do they coexist as competing frameworks?',
    'Examine whether any jurisdiction or doctrinal lineage has adopted the harm-limited reading as exclusive — i.e., treated absolutist arguments as legally impermissible rather than merely incorrect. If yes, forecloses; if no, coexists_with.',
    'If forecloses, the kernel has collapsed to a single reading — the contest is over. If coexists_with, the kernel remains contested and the constraint''s legitimacy depends on ongoing doctrinal competition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_relationship, conceptual, 'Structural relationship between this reading and the absolutist sibling.').

omega_variable(
    platform_enforcement_capture,
    'Has the harm standard been captured by platform content-moderation systems that operationalize ''harm'' at scale without judicial oversight?',
    'Compare platform takedown rates for ''harmful content'' vs. court adjudications of harm. If platform enforcement vastly exceeds and precedes judicial findings, the constraint has migrated to private governance.',
    'If captured, the constraint''s suppression and extraction metrics understate reality — private enforcement is less accountable. The constraint type may shift from Tangled Rope (public coordination) to Snare (private extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(platform_enforcement_capture, empirical, 'Whether private platform enforcement has become the primary instantiation of the harm standard.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__harm_limited_reading, 1942, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fa_harm_limited_tr_t1942, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 1942, 0.1).
narrative_ontology:measurement(fa_harm_limited_tr_t1964, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 1964, 0.12).
narrative_ontology:measurement(fa_harm_limited_tr_t1989, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 1989, 0.18).
narrative_ontology:measurement(fa_harm_limited_tr_t2003, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 2003, 0.22).
narrative_ontology:measurement(fa_harm_limited_tr_t2015, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 2015, 0.25).
narrative_ontology:measurement(fa_harm_limited_tr_t2024, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(fa_harm_limited_be_t1942, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 1942, 0.15).
narrative_ontology:measurement(fa_harm_limited_be_t1964, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 1964, 0.22).
narrative_ontology:measurement(fa_harm_limited_be_t1989, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 1989, 0.35).
narrative_ontology:measurement(fa_harm_limited_be_t2003, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 2003, 0.44).
narrative_ontology:measurement(fa_harm_limited_be_t2015, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 2015, 0.51).
narrative_ontology:measurement(fa_harm_limited_be_t2024, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fa_harm_limited_su_t1942, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 1942, 0.2).
narrative_ontology:measurement(fa_harm_limited_su_t1964, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 1964, 0.35).
narrative_ontology:measurement(fa_harm_limited_su_t1989, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 1989, 0.48).
narrative_ontology:measurement(fa_harm_limited_su_t2003, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 2003, 0.55).
narrative_ontology:measurement(fa_harm_limited_su_t2015, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 2015, 0.59).
narrative_ontology:measurement(fa_harm_limited_su_t2024, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__harm_limited_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(first_amendment_speech_protection__harm_limited_reading, 0.12).
narrative_ontology:affects_constraint(first_amendment_speech_protection__harm_limited_reading, first_amendment_speech_protection__absolutist_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__harm_limited_reading, first_amendment_speech_protection__categorical_balancing_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__harm_limited_reading, platform_content_moderation_harm_standard).
narrative_ontology:affects_constraint(first_amendment_speech_protection__harm_limited_reading, hate_speech_regulation_framework).
narrative_ontology:affects_constraint(first_amendment_speech_protection__harm_limited_reading, harassment_law_civil_criminal).

% DUAL FORMULATION NOTE:
% This constraint is the harm_limited_reading of the first_amendment_speech_protection kernel. It decomposes the natural-language concept 'First Amendment protection' into structurally distinct claims. The absolutist_reading has near-zero extraction (Mountain-like); categorical_balancing_reading has moderate extraction with high suppression (Tangled Rope); this harm_limited_reading has rising extraction and active enforcement (Tangled Rope trending toward Snare). The ε values differ because they refer to different constraint structures: one protects speech categorically, one balances case-by-case, one triggers on proven harm.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(first_amendment_speech_protection__harm_limited_reading, institutional, 0.2).
constraint_indexing:directionality_override(first_amendment_speech_protection__harm_limited_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
