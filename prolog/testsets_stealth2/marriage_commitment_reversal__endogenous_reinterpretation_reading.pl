% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_reversal__endogenous_reinterpretation_reading, []).

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
 *   constraint_id: marriage_commitment_reversal__endogenous_reinterpretation_reading
 *   human_readable: Manifesto as Continuing Revelation: Internal Reinterpretation of God's Will Under Changed Circumstances (Endogenous Reading)
 *   domain: religious institutional history / commitment systems / political theology
 *
 * SUMMARY:
 *   On September 24, 1890, Wilford Woodruff issued the announcement
 *   Latter-day Saints know as the Manifesto, advising members to refrain from
 *   contracting any marriage forbidden by the law of the land; he attributed
 *   the decision to a vision received September 23, 1890, in which, by his
 *   account, the Lord showed him what would befall the church and its people
 *   if plural marriage continued. This file generates ONE reading of the
 *   contested kernel marriage_commitment_reversal: the
 *   endogenous_reinterpretation_reading, in which the reversal was authorized
 *   from inside the tradition — divine revelation reinterpreting God's will
 *   under changed circumstances, with Section 132 remaining canon while its
 *   application yields to living revelation. The sibling readings (reversal
 *   as exogenous coercion with doctrine unrevised; reversal as a persistent
 *   doctrine-practice gap) are separate constraints, linked through
 *   network.affects_constraints; nothing about them is averaged into this
 *   file. The epsilon referent is the standing arrangement under contest
 *   assessed by this reading's own lights: the community-wide requirement
 *   that members accept the reversal as revealed will, bear its
 *   reconciliation costs, and extend the prophet's interpretive authority
 *   over prior canon. Claim and metrics are authored independently: the
 *   claimed type states what this reading takes the structure to be; the
 *   metrics describe how the arrangement actually operated. KEY AGENTS (by
 *   structural relationship): - first_presidency_leadership: Agenda-setter
 *   and principal beneficiary (institutional / identity_locked) — issues the
 *   reversal as revelation and collects preserved interpretive authority -
 *   quorum_of_twelve_apostles: Secondary beneficiary (institutional /
 *   constrained) — sustains and administers acceptance across the stakes -
 *   reconciling_rank_and_file: Dual-positioned membership seat (moderate /
 *   constrained) — receives relief and institutional survival, bears the
 *   reconciliation labor - post_manifesto_plural_families: Principal bearing
 *   seat (powerless / trapped) — married after the announcement under quiet
 *   permission, later prosecuted and disciplined -
 *   fundamentalist_excommunicants: Bearing seat (powerless / trapped) —
 *   refuse the reversal on Section 132 grounds, lose membership -
 *   federal_judicial_apparatus: Excluded party (institutional / mobile) —
 *   supplied the changed circumstances, absent from internal meaning-making -
 *   utah_statehood_elites: Incidental beneficiary (powerful / mobile) —
 *   convert compliance into statehood - historians_of_mormonism: Analytical
 *   observer (analytical / analytical) — reconstruct the sequence from
 *   archives outside the sanction system
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.58).
domain_priors:suppression_score(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.68).
domain_priors:theater_ratio(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__endogenous_reinterpretation_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_reversal__endogenous_reinterpretation_reading, "Manifesto as Continuing Revelation: Internal Reinterpretation of God's Will Under Changed Circumstances (Endogenous Reading)").
narrative_ontology:topic_domain(marriage_commitment_reversal__endogenous_reinterpretation_reading, "religious institutional history / commitment systems / political theology").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__endogenous_reinterpretation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__endogenous_reinterpretation_reading, 'b473b166-d904-4f35-8c36-d674fd49fda6').
narrative_ontology:cs_kernel_codification('b473b166-d904-4f35-8c36-d674fd49fda6', fixed_text).
narrative_ontology:cs_authority_grounding('b473b166-d904-4f35-8c36-d674fd49fda6', lineage).
narrative_ontology:cs_interpretation_layer_present('b473b166-d904-4f35-8c36-d674fd49fda6').
narrative_ontology:cs_reading_relation('b473b166-d904-4f35-8c36-d674fd49fda6', marriage_commitment_reversal__exogenous_override_reading, forecloses).
narrative_ontology:cs_reading_relation('b473b166-d904-4f35-8c36-d674fd49fda6', marriage_commitment_reversal__practice_doctrine_gap, influences).
narrative_ontology:cs_axiom('b473b166-d904-4f35-8c36-d674fd49fda6', foundational, living_prophet_reinterprets_conditional_commands).
narrative_ontology:cs_axiom_status(living_prophet_reinterprets_conditional_commands, holdable).
narrative_ontology:cs_axiom_grounding('b473b166-d904-4f35-8c36-d674fd49fda6', living_prophet_reinterprets_conditional_commands, theological).
narrative_ontology:cs_axiom('b473b166-d904-4f35-8c36-d674fd49fda6', secondary, canon_preserved_under_interpretive_supersession).
narrative_ontology:cs_axiom_status(canon_preserved_under_interpretive_supersession, holdable).
narrative_ontology:cs_axiom_grounding('b473b166-d904-4f35-8c36-d674fd49fda6', canon_preserved_under_interpretive_supersession, conventional).
narrative_ontology:cs_reference_frame('b473b166-d904-4f35-8c36-d674fd49fda6', conditional_command_living_oracle_frame).
narrative_ontology:cs_drift_state('b473b166-d904-4f35-8c36-d674fd49fda6', contemporary, gap(axiom_overriding, minor, true)).
narrative_ontology:cs_created_at('b473b166-d904-4f35-8c36-d674fd49fda6', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__endogenous_reinterpretation_reading, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__endogenous_reinterpretation_reading, first_presidency_leadership).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__endogenous_reinterpretation_reading, quorum_of_twelve_apostles).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__endogenous_reinterpretation_reading, utah_statehood_elites).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__endogenous_reinterpretation_reading, reconciling_rank_and_file).
narrative_ontology:constraint_victim(marriage_commitment_reversal__endogenous_reinterpretation_reading, post_manifesto_plural_families).
narrative_ontology:constraint_victim(marriage_commitment_reversal__endogenous_reinterpretation_reading, fundamentalist_excommunicants).
narrative_ontology:constraint_victim(marriage_commitment_reversal__endogenous_reinterpretation_reading, reconciling_rank_and_file).
narrative_ontology:constraint_vindicates(marriage_commitment_reversal__endogenous_reinterpretation_reading, continuing_revelation_doctrine).
narrative_ontology:constraint_vindicates(marriage_commitment_reversal__endogenous_reinterpretation_reading, prophetic_precedence_over_prior_canon).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the office that speaks for God to the church. Received and announced the September 23, 1890 vision directing the cessation of plural marriage, and issued the advice to refrain from marriages forbidden by law. What flows to the office is the continuation of its claim to interpret divine will: the episode demonstrates that current revelation governs past commandments. Leaving the arrangement would mean recasting the announcement as surrender to federal power, which would unsettle the very authority the office exercises over canon — the office and the framing have become the same thing.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, first_presidency_leadership, agenda_setter,
    institutional, generational, identity_locked, global).

% The senior governing quorum below the First Presidency. Several members hesitated to endorse the announcement — at least one objected that endorsing it surrendered principle — before sustaining it and carrying its acceptance through the stakes and missions. They share the preserved interpretive order and staff the councils that later enforce it.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, quorum_of_twelve_apostles, beneficiary,
    institutional, generational, constrained, global).

% Ordinary members taught for a generation that plural marriage was required for the highest degree of glory. They receive relief from raids, arrests, and asset seizure, and a church that survives; they give acceptance — temple recommends, sustaining votes, and the private work of fitting Section 132 to the announcement in personal belief. Departure means losing congregation, family religious life, and expected salvation; staying means performing an assent they cannot publicly qualify.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, reconciling_rank_and_file, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__endogenous_reinterpretation_reading, reconciling_rank_and_file, payer).

% Couples who contracted plural marriages after September 1890, some sealed with the knowledge or participation of senior leaders who treated the announcement as public-facing only. After 1904 they face federal indictment, Senate investigation, and church discipline; some flee to colonies in Mexico and Canada; the standing of their marriages is retroactively destabilized both in civil law and in the community that solemnized them.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, post_manifesto_plural_families, payer,
    powerless, biographical, trapped, national).

% Members and emerging splinter groups who conclude that a proclamation cannot suspend what a canonized revelation commands. Through the 1910s and 1920s they are summoned to disciplinary councils, offered membership on condition of renouncing ongoing plural practice, and on refusal cast out; they keep the conviction and lose the community, forming separate sects on its margins.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, fundamentalist_excommunicants, payer,
    powerless, generational, trapped, regional).

% Congress, the federal courts, and territorial officials: authors of the Edmunds-Tucker regime that disincorporates the church, disenfranchises practitioners, imprisons husbands, and seizes temple property. Having won compliance, they take no position on the announcement's internal meaning; their leverage stops at the edge of the church's self-understanding, and they are not part of the conversation that assigns the reversal its cause.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, federal_judicial_apparatus, excluded,
    institutional, generational, mobile, national).

% Territorial political and commercial leadership whose statehood campaign requires the practice's end. They receive statehood in 1896, restored normal commerce, and a weakened opposition; they neither administer nor enforce the announcement, but their project is what compliance purchases.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, utah_statehood_elites, beneficiary,
    powerful, generational, mobile, regional).

% Academic and independent researchers who reconstruct the sequence from Woodruff's journals and draft manuscripts, cabinet correspondence, court dockets, and press coverage. They stand outside the church's sanction system, publish where no temple recommend is at stake, and document both the pressure timeline and the evolution of the vision account.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, historians_of_mormonism, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_reversal__endogenous_reinterpretation_reading, first_presidency_leadership).
narrative_ontology:fixing_cost_class(marriage_commitment_reversal__endogenous_reinterpretation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives a dispersed community a single authoritative account under which everyone reverses a core marriage practice at the same time: members can comply with federal law, retain confidence that current revelation governs past commandments, and keep Section 132 in canon without treating it as void — solved once, centrally, through the prophet's announcement rather than through per-congregation negotiation or private conscience.
% TRANSFER_FUNCTION: Moves reconciliation labor from the institution to the membership: each believer absorbs the work of harmonizing Section 132 with the announcement in personal belief. Moves compliance costs onto individual families who dissolve or conceal plural households. Moves a stock of legitimacy capital upward to the First Presidency, whose office demonstrates authority over its own prior canon.
% ABSENT_VOICES: Post-announcement plural families had no seat when the Second Manifesto tightened enforcement in 1904 — their testimony existed only as evidence in disciplinary councils and Senate hearings. Fundamentalist-minded members were talked about, not with, until their excommunications. The federal apparatus had secured compliance and declined any interest in how the reversal was narrated. And any member inclined to read the announcement as capitulation rather than revelation had no sanctioned venue; the public sustaining vote was the only channel.
% DISAPPEARANCE_RATIONALE: If the revelation framing vanished overnight — if the announcement stood as naked policy concession — the living-oracle doctrine loses its clearest demonstration, the 1890 crisis likely fractures the church along the lines the fundamentalist schisms later followed but a generation earlier and at scale, Utah's statehood bargain loses its domestic enforcer, and the template later used for the 1978 priesthood reversal disappears. Arrangements across the tradition depend on the framing.
% FOUNDING_PROBLEM: How can a community bound by canonized command (Section 132) cease the commanded practice under legal annihilation — corporate dissolution, imprisoned leadership, seized temples — without surrendering the claim that its head receives binding revelation?
% FOUNDING_PROBLEM_CORROBORATION: Partially corroborated from outside the benefiting parties: academic historians of Mormonism, working from journals, draft manuscripts, court records, and correspondence, attest both the severity of the external pressure and the interpretive function of the revelation framing; the tradition's own later reuse of the mechanism in 1978 evidences that the underlying problem — reconciling living revelation with prior canon — remained live. The institution's own narratives cannot serve as independent attestation, and Woodruff's contemporaneous letters show sincere conviction entangled with full awareness of the consequences, so the strongest internal testimony is mixed rather than clean.
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__endogenous_reinterpretation_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__endogenous_reinterpretation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__endogenous_reinterpretation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_commitment_reversal__endogenous_reinterpretation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_reversal__endogenous_reinterpretation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_reversal__endogenous_reinterpretation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_reversal__endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is moderate (0.58 at interval end): the arrangement takes real things from identifiable parties — reconciliation labor, conformity performance, disciplinary exposure, retroactive destabilization of marriages solemnized in good faith — while returning survival goods (relief from raids and seizure, a functioning church, statehood). It is neither negligible nor total. Suppression (0.68) is higher than extraction because persistence depends on disciplinary machinery rather than participant preference: temple recommends presuppose acceptance, the 1904 Second Manifesto attached penalties, and refusal ended in excommunication. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled downstream. Theater ratio (0.45) reflects a growing share of activity shifting from functional governance to narrative maintenance — retelling and defending the vision account, curating official history — as the practice itself wound down. Accessibility collapse (0.6): once the acceptance requirement is understood, alternatives collapse almost entirely to exit; there is no sanctioned middle position. Resistance (0.45): real but bounded — apostolic hesitation before signing, continued quiet plural marriages through the 1890s, and the fundamentalist counter-movement. The measurement series run on ONE shared eight-point grid (t=0..40, i.e., 1890-1930) so every tracked metric is authored at every examined time point. The suppression_requirement series is authored deliberately: this story's traced dynamic is enforcement-capacity change — an advisory announcement in 1890 (0.35), hardening through the 1904 Second Manifesto (0.66), peaking with the 1910s-1920s disciplinary campaigns (0.73), then easing slightly as dissenters exited and the remainder internalized the narrative (0.68). Base extractiveness peaks in the same window and settles as the bearing population shrinks. Receipt surface: the gains demonstrably accrue to the first_presidency_leadership seat — legitimacy capital is what the framing produces and where it lands — hence gain_flow names that seat rather than asserting diffuseness.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the agenda-setting seat the arrangement is the mechanism that saved the institution and demonstrated living revelation — costs are the price of survival, borne gladly. From the post-announcement families' seat the same structure operates as retroactive betrayal: permission granted quietly, then withdrawn loudly, with the families holding the liability. From the fundamentalist seat it is an enforced falsehood about what God commanded. The rank-and-file seat is genuinely dual — relief received, reconciliation labor given — and computes intermediate. The engine derives these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the leadership and statehood seats toward the subsidized end (low d); victim declarations drive the post-announcement families and fundamentalists toward the full-target end, amplified by trapped exit — they cannot relocate their marriages' legitimacy or their membership elsewhere. The rank-and-file seat is the one place the automatic derivation would err: it is listed primarily under beneficiaries (its dominant flow is relief and survival), which biases the derived d toward subsidy, but its actual ledger — reconciliation labor, compulsory assent, exposure to discipline — nets slightly to the paying side. A directionality override sets the moderate power atom to d=0.55, just past symmetry; reconciling_rank_and_file is the only moderate-power seat in the story, so the override touches exactly that seat. The federal apparatus is role-excluded: it supplied the changed circumstances but is not a party to the interpretive arrangement and contributes no beneficiary/victim declaration.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid classification is what keeps both halves of this arrangement visible. Reading it as pure coordination would erase the consistency costs and the disciplined dissenters; reading it as pure extraction would erase the genuine survival function the framing performed under existential legal pressure. The R5 interview shows no mandatrophy: founding_problem_status is live (the reconcile-living-revelation-with-prior-canon problem recurs, and the 1890 mechanism was reused as the template for the 1978 priesthood reversal) and disappearance_verdict is world_rearranges — status-live crossed with rearrange is the consistent cell, so no capture/zombie flag arises. The theater_ratio trajectory is the early-warning line: if the framing ever atrophied into pure commemoration — retold but administering nothing — the drift toward inertial maintenance would register there.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vision_authenticity_ambiguity,
    'Was the September 23, 1890 experience a singular revelatory event as later narrated, or a decision process reached under pressure and retrospectively framed as revelation (the journal drafts of the vision account evolved across retellings)?',
    'Manuscript archaeology of Woodruff''s journal drafts, contemporaneous letters to George Q. Cannon and other confidants, and comparison with the 1889-1890 decision correspondence and cabinet discussions.',
    'If the account was substantially retrospective, the revelation framing functions as manufactured legitimation and the arrangement''s cost profile shifts toward enforced narrative acceptance; if singular and sincerely reported, the coordination function is stronger and the measured costs read better as the price of institutional survival.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vision_authenticity_ambiguity, empirical, 'Whether the vision account reports an event or constructs one.').

omega_variable(
    kernel_reading_allocation,
    'Does the endogenous reading correctly locate the operative arrangement, or is the revelation framing derivative of the coercion structure (exogenous_override_reading) or of the persistent canon-practice ambiguity (practice_doctrine_gap)?',
    'Compare computed classifications across the three sibling stories; weight member testimony on internal conviction versus external fear; test whether the framing changed member behavior beyond what bare compliance with federal law predicts.',
    'If the exogenous reading dominates, this story''s epsilon falls and the framing becomes narrative dressing over a coercion event; if the gap reading dominates, the operative arrangement sits at the doctrine-practice interface and this story becomes a masking layer over it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_allocation, conceptual, 'Committer-frame allocation of the reversal''s operative arrangement among sibling readings of the marriage_commitment_reversal kernel.').

omega_variable(
    consistency_cost_distribution,
    'Is the theological-consistency cost (why did God''s will change?) a real welfare burden distributed across the membership, or an elite-analytical concern that ordinary believers dissolve cheaply through conditional-command theology?',
    'Disaffiliation accounts, private diaries, and sermons from 1890-1930 referencing doctrinal whiplash; comparative distress rates around the 1890 and 1978 reversals.',
    'If the cost dissolves cheaply, effective extraction drops and the arrangement moves toward the coordination end; if broadly borne, the hybrid reading solidifies and the bearing set widens beyond open dissenters.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consistency_cost_distribution, empirical, 'Who actually bears the why-did-God''s-will-change cost.').

omega_variable(
    prophetic_office_identity_lock,
    'Could the First Presidency have issued the reversal as explicit policy concession rather than revelation, or was the revelation form constitutive of the office such that no other issuance was available?',
    'Counterfactual analysis of institutional statements in adjacent crises (including 1978) and of Woodruff''s private resistance to ''signing away'' the principle; search the tradition for any policy-form doctrinal announcement.',
    'If identity-locked, the framing was not strategic choice and part of the enforcement burden is self-applied; consequences concentrate on the agenda-setting seat''s exit conditions rather than on member-facing costs.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(prophetic_office_identity_lock, conceptual, 'Whether the revelation form was chosen or structurally compelled by the office''s identity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(marr_tr_t0, observed).
narrative_ontology:measurement(marr_tr_t5, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(marr_tr_t5, observed).
narrative_ontology:measurement(marr_tr_t10, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(marr_tr_t10, observed).
narrative_ontology:measurement(marr_tr_t14, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 14, 0.38).
narrative_ontology:measurement_basis(marr_tr_t14, observed).
narrative_ontology:measurement(marr_tr_t20, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(marr_tr_t20, observed).
narrative_ontology:measurement(marr_tr_t26, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 26, 0.43).
narrative_ontology:measurement_basis(marr_tr_t26, observed).
narrative_ontology:measurement(marr_tr_t33, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 33, 0.44).
narrative_ontology:measurement_basis(marr_tr_t33, observed).
narrative_ontology:measurement(marr_tr_t40, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 40, 0.45).
narrative_ontology:measurement_basis(marr_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(marr_be_t0, observed).
narrative_ontology:measurement(marr_be_t5, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement_basis(marr_be_t5, observed).
narrative_ontology:measurement(marr_be_t10, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 10, 0.53).
narrative_ontology:measurement_basis(marr_be_t10, observed).
narrative_ontology:measurement(marr_be_t14, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 14, 0.6).
narrative_ontology:measurement_basis(marr_be_t14, observed).
narrative_ontology:measurement(marr_be_t20, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement_basis(marr_be_t20, observed).
narrative_ontology:measurement(marr_be_t26, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 26, 0.61).
narrative_ontology:measurement_basis(marr_be_t26, observed).
narrative_ontology:measurement(marr_be_t33, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 33, 0.59).
narrative_ontology:measurement_basis(marr_be_t33, observed).
narrative_ontology:measurement(marr_be_t40, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement_basis(marr_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(marr_su_t0, observed).
narrative_ontology:measurement(marr_su_t5, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement_basis(marr_su_t5, observed).
narrative_ontology:measurement(marr_su_t10, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement_basis(marr_su_t10, observed).
narrative_ontology:measurement(marr_su_t14, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 14, 0.66).
narrative_ontology:measurement_basis(marr_su_t14, observed).
narrative_ontology:measurement(marr_su_t20, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement_basis(marr_su_t20, observed).
narrative_ontology:measurement(marr_su_t26, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 26, 0.73).
narrative_ontology:measurement_basis(marr_su_t26, observed).
narrative_ontology:measurement(marr_su_t33, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 33, 0.7).
narrative_ontology:measurement_basis(marr_su_t33, observed).
narrative_ontology:measurement(marr_su_t40, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement_basis(marr_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__endogenous_reinterpretation_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_commitment_reversal__endogenous_reinterpretation_reading, marriage_commitment_reversal__exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__endogenous_reinterpretation_reading, marriage_commitment_reversal__practice_doctrine_gap).

% DUAL FORMULATION NOTE:
% The colloquial label 'the 1890 Manifesto' covers three structurally distinct claims about what reversed plural marriage: internal revelation reinterpreting God's will (this file), external coercion with Section 132 preserved unrevised (exogenous_override_reading), and a persistent canon-practice ambiguity (practice_doctrine_gap). Per the epsilon-invariance principle each is a separate story with its own epsilon, beneficiary/victim structure, and classification; they form a constraint family linked through affects_constraints. Causal texture across the family: the endogenous reading supplies the legitimating narrative that the gap reading describes as obscuring the doctrine-practice interface, and it competes with the exogenous reading for the causal location of the reversal. This story's epsilon is measured against the acceptance-of-revelation arrangement; measuring the compliance arrangement or the gap instead would be measuring the siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_commitment_reversal__endogenous_reinterpretation_reading, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
