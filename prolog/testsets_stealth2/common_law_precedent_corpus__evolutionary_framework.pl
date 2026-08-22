% ============================================================================
% CONSTRAINT STORY: common_law_precedent_corpus__evolutionary_framework
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_law_precedent_corpus__evolutionary_framework, []).

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
 *   constraint_id: common_law_precedent_corpus__evolutionary_framework
 *   human_readable: Common-Law Precedent Corpus — Evolutionary Framework Reading
 *   domain: legal/jurisprudential
 *
 * SUMMARY:
 *   Under this reading, the accumulated corpus of judicial precedent operates
 *   as an adaptive framework rather than a binding wall: past decisions
 *   supply starting points, doctrinal vocabulary, and burden allocations,
 *   while contemporary normative evolution licenses courts to reinterpret or
 *   overrule them. The arrangement is actively administered — appellate
 *   hierarchy, certiorari-style docket control, and citation discipline keep
 *   the corpus coherent — and it genuinely coordinates legal meaning across
 *   millions of disputes and decades. The same channel that carries
 *   correction also carries asymmetric flows: interpretive authority accrues
 *   to the bench that wields the update power, while reliance-holders and
 *   respondents reached by revised readings absorb adaptation costs they did
 *   not choose and cannot veto. Claim/metric independence: claimed_type is
 *   authored as tangled_rope from the structural facts (genuine coordination
 *   function, named payers, active enforcement); the metrics are authored
 *   from descriptive operation and are not tuned to the claim or to any
 *   predicted engine output. Family note: this file instantiates the
 *   evolutionary_framework reading of the common_law_precedent_corpus kernel;
 *   strict_stare_decisis and pluralist_balancing are separate constraint
 *   files with their own epsilon values, linked through
 *   network.affects_constraints. KEY AGENTS (by structural relationship): -
 *   appellate_judiciary: Primary agenda-setter and collector
 *   (institutional/identity_locked) — administers the framework, decides
 *   which precedents live, accrues interpretive authority -
 *   norm_challenge_litigants: Primary beneficiary (organized/mobile) —
 *   receives standing pathways for norm challenge -
 *   doctrinal_scholarship_community: Secondary beneficiary (organized/mobile)
 *   — supplies the normative-evolution arguments the bench takes up -
 *   reliance_interest_holders: Primary payer (powerful/constrained) — bears
 *   repricing when settled readings shift - retroactive_exposure_defendants:
 *   Sharpest payer (powerless/trapped) — past conduct becomes reachable by
 *   new readings' sanctions - elected_legislature: Institutional payer
 *   (institutional/constrained) — sees enacted meaning displaced, retains
 *   composition leverage over the bench - the_public_whose_norms_are_invoked:
 *   Excluded seat (powerless/trapped) — invoked as warrant, present only as
 *   abstraction - jurisprudential_analysts: Analytical observer
 *   (analytical/analytical) — maps the structure, collects nothing
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__evolutionary_framework, 0.52).
domain_priors:suppression_score(common_law_precedent_corpus__evolutionary_framework, 0.35).
domain_priors:theater_ratio(common_law_precedent_corpus__evolutionary_framework, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, extractiveness, 0.52).
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__evolutionary_framework, tangled_rope).
narrative_ontology:human_readable(common_law_precedent_corpus__evolutionary_framework, "Common-Law Precedent Corpus — Evolutionary Framework Reading").
narrative_ontology:topic_domain(common_law_precedent_corpus__evolutionary_framework, "legal/jurisprudential").

domain_priors:requires_active_enforcement(common_law_precedent_corpus__evolutionary_framework).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__evolutionary_framework, 'a8890505-eadd-4009-b2da-92f27a2ec620').
narrative_ontology:cs_kernel_codification('a8890505-eadd-4009-b2da-92f27a2ec620', fixed_text).
narrative_ontology:cs_authority_grounding('a8890505-eadd-4009-b2da-92f27a2ec620', lineage).
narrative_ontology:cs_interpretation_layer_present('a8890505-eadd-4009-b2da-92f27a2ec620').
narrative_ontology:cs_reading_relation('a8890505-eadd-4009-b2da-92f27a2ec620', common_law_precedent_corpus__strict_stare_decisis, forecloses).
narrative_ontology:cs_reading_relation('a8890505-eadd-4009-b2da-92f27a2ec620', common_law_precedent_corpus__pluralist_balancing, coexists_with).
narrative_ontology:cs_axiom('a8890505-eadd-4009-b2da-92f27a2ec620', foundational, normative_evolution_licenses_reinterpretation).
narrative_ontology:cs_axiom_status(normative_evolution_licenses_reinterpretation, holdable).
narrative_ontology:cs_axiom_grounding('a8890505-eadd-4009-b2da-92f27a2ec620', normative_evolution_licenses_reinterpretation, instrumental).
narrative_ontology:cs_axiom('a8890505-eadd-4009-b2da-92f27a2ec620', foundational, precedent_as_starting_point_not_command).
narrative_ontology:cs_axiom_status(precedent_as_starting_point_not_command, holdable).
narrative_ontology:cs_axiom_grounding('a8890505-eadd-4009-b2da-92f27a2ec620', precedent_as_starting_point_not_command, conventional).
narrative_ontology:cs_axiom('a8890505-eadd-4009-b2da-92f27a2ec620', secondary, overruling_is_corrective_not_usurpative).
narrative_ontology:cs_axiom_status(overruling_is_corrective_not_usurpative, holdable).
narrative_ontology:cs_axiom_grounding('a8890505-eadd-4009-b2da-92f27a2ec620', overruling_is_corrective_not_usurpative, deontological).
narrative_ontology:cs_reference_frame('a8890505-eadd-4009-b2da-92f27a2ec620', adaptive_common_law_tradition).
narrative_ontology:cs_drift_state('a8890505-eadd-4009-b2da-92f27a2ec620', contemporary_textualist_ascendancy, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('a8890505-eadd-4009-b2da-92f27a2ec620', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__evolutionary_framework, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, norm_challenge_litigants).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, appellate_judiciary).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, doctrinal_scholarship_community).
narrative_ontology:constraint_victim(common_law_precedent_corpus__evolutionary_framework, reliance_interest_holders).
narrative_ontology:constraint_victim(common_law_precedent_corpus__evolutionary_framework, retroactive_exposure_defendants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(common_law_precedent_corpus__evolutionary_framework, elected_legislature).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The supreme and intermediate appellate benches decide which lines of precedent to reaffirm, distinguish, or overrule, write the opinions that become tomorrow's starting points, and control the docket through certiorari-style screening. Each departure adds to its members' doctrinal legacy; each reaffirmation spends institutional capital. Leaving the role would mean leaving adjudication itself — the bench's professional identity is bound up with authorship of the law's development.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, appellate_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(common_law_precedent_corpus__evolutionary_framework, appellate_judiciary, beneficiary).

% Public-interest firms, civil-rights organizations, and repeat constitutional litigants who bring test cases asking courts to read settled doctrine against changed circumstances. The framework's openness gives them a standing invitation: a favorable panel, a supportive government litigator, or a shifted social fact can reopen a closed question. Their constraint is patience and funding — doctrinal campaigns run decades — but they face no structural bar to trying.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, norm_challenge_litigants, beneficiary,
    organized, generational, mobile, national).

% Law faculties, journals, and treatise writers who produce the normative-evolution arguments, syntheses, and legitimacy analyses that briefs and opinions cite. When courts announce that circumstances have evolved, scholarly work supplies the account of the evolution; uptake converts academic production into influence. Scholars can redirect attention to other questions if uptake fails.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, doctrinal_scholarship_community, beneficiary,
    organized, biographical, mobile, global).

% Contract counterparties, regulated industries, municipalities, and property owners who arranged affairs — pricing, compliance programs, estate plans, licensing — around what courts had said the law was. When a settled reading is revisited, their completed transactions and built compliance do not move with it; they absorb repricing, retrofitting, or litigation risk after the fact. Their recourse runs through lobbying for legislative override or participating as amici in the very case that unsettles them — slow, uncertain channels.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, reliance_interest_holders, payer,
    powerful, biographical, constrained, national).

% Criminal and civil respondents whose conduct was lawful or defensible under the doctrine as it stood when they acted, but becomes sanctionable under a revised reading announced in someone else's case. They did not choose the test case, cannot withdraw their past conduct, and typically learn the new rule only when it is applied to them. Individual defendants hold no collective organization and no forum of their own.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, retroactive_exposure_defendants, payer,
    powerless, immediate, trapped, national).

% Writes statutes and proposes amendments, and periodically finds its enactments' meaning revised by courts applying evolved understandings — or its policy judgments displaced by constitutional updating. Its remedies (statutory override, amendment, jurisdiction stripping, appointment politics) are blunt, slow, and often unavailable against constitutional holdings. It retains real leverage over the bench's composition, exercised on electoral timescales.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, elected_legislature, payer,
    institutional, biographical, constrained, national).

% Citizens whose changing convictions are cited as the warrant for reinterpretation but who appear in the process only as abstractions — surveyed, polled, or imagined by counsel and clerks. They receive whatever protection updated doctrine affords and bear whatever unpredictability it creates, without a seat, a docket entry, or a vote on any particular departure.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, the_public_whose_norms_are_invoked, excluded,
    powerless, generational, trapped, national).

% Academic and comparative scholars who map how different legal systems handle precedent's force, publish legitimacy and compliance studies, and supply the vocabulary in which the bench and bar argue about departure. They collect nothing from the framework's operation and bear none of its costs; their stake is explanatory.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, jurisprudential_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_law_precedent_corpus__evolutionary_framework, appellate_judiciary).
narrative_ontology:fixing_cost_class(common_law_precedent_corpus__evolutionary_framework, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a shared, transmissible body of authoritative legal starting points so that thousands of dispersed courts can decide similar cases similarly without relitigating first principles, and channels legal change through doctrinal argument rather than discontinuous rupture — each departure arrives tethered to the corpus it modifies.
% TRANSFER_FUNCTION: Moves final interpretive authority from enacted text and settled holdings to sitting appellate judges; moves adaptation costs onto parties who relied on prior readings and onto respondents whose past conduct new readings reach; moves doctrinal legacy and agenda control to the bench; and moves litigation opportunity toward well-funded repeat players able to sustain multi-decade norm campaigns.
% ABSENT_VOICES: The citizens whose 'contemporary norms' supply the warrant for reinterpretation have no seat — their convictions enter as elite constructions (briefs, clerk memoranda, scholarship) rather than testimony. Strict-constructionist legislators and voters who would call updating anti-democratic speak only from outside, through appointment politics and amicus filings. Past litigants who settled under now-revisited readings are absent by definition — their cases are closed.
% DISAPPEARANCE_RATIONALE: If courts stopped treating precedent as even a framework — deciding every case from first principles — dockets would lengthen dramatically, lower courts would diverge until appellate correction caught up, contracts and regulatory compliance priced against settled readings would reprice, and the profession's core craft of distinguishing, synthesizing, and citing would lose its object. Political branches would face pressure to codify wholesale. The rearrangement would be total and slow.
% FOUNDING_PROBLEM: How can a legal system decide novel cases consistently without a comprehensive code, preserve continuity with inherited law, and correct its own past errors without each correction appearing as rupture?
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: legal historians trace stare decisis's consolidation to the practical demands of expanding dockets and printed reports rather than to judicial self-interest alone; comparative scholarship shows civil-law systems independently developing jurisprudence constante against the same consistency problem; bar associations and continuing-education bodies treat precedent-handling as basic professional infrastructure. The bench also attests the problem's liveness, but the external attestations stand on their own.
narrative_ontology:disappearance_verdict(common_law_precedent_corpus__evolutionary_framework, world_rearranges).
narrative_ontology:founding_problem_status(common_law_precedent_corpus__evolutionary_framework, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_law_precedent_corpus__evolutionary_framework, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(common_law_precedent_corpus__evolutionary_framework, 'none', 1).
narrative_ontology:epsilon_provenance(common_law_precedent_corpus__evolutionary_framework, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_law_precedent_corpus__evolutionary_framework_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_law_precedent_corpus__evolutionary_framework, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_law_precedent_corpus__evolutionary_framework_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.52 — moderate: the framework's flexibility is genuinely corrective in many episodes, but the same channel that carries correction also carries self-empowerment (the bench decides when norms have 'evolved') and imposes unchosen adaptation costs on reliance-holders and respondents. Suppression is 0.35: challenge pathways are deliberately broad, but exit from the precedent system as such is unavailable — every argument must proceed through the corpus, and legislative override is slow and often structurally blocked. Theater ratio 0.42 reflects the ritualized performance of deference — extended precedent recitation, 'reluctant' departures, stare decisis-factor liturgies — accompanying many reinterpretations; the performance is functional (it manufactures continuity) but a growing share of it defends the updater role rather than deciding anything. Accessibility collapse 0.38: once the framework is understood, the alternatives (strict binding, codification, open disregard) remain visible and partially exercisable — nothing about the adaptive framework forecloses them; they are merely institutionally expensive. Resistance 0.5: organized jurisprudential counter-movements, political criticism, and periodic court-curbing proposals meet the practice continuously without displacing it. The measurement series share one grid (1960/1975/1990/2005/2015/2025): extractiveness rises through the twentieth century's rights expansion and the professionalization of doctrinal campaigning, then plateaus as counter-movements raise the price of further updating; theater rises in step as deference-performance intensifies under scrutiny; suppression_requirement climbs from near-passive maintenance to active defense as strict readings gained seats — the enforcement-capacity trajectory is the story's traced dynamic, which is why the series is authored despite a moderate static suppression value. Suppression is authored as a raw structural property and is not scaled by power or scope; effective extraction is computed by the engine from directionality and spatial scope.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently by construction. From the appellate bench, the framework is its own craft and patrimony: the power to revisit doctrine is the power to author the law, and departures feel like corrections. From reliance-holders, the same openness is unilateral repricing of settled expectations by a counterparty that cannot itself be sued for the loss. From exposed respondents, it is retroactivity without consent. From the legislature, it is displacement of enacted meaning. The engine computes these per-seat classifications from power, exit, and role data; the divergence between the bench's self-understanding and the payers' experience is the measurable quantity, not something the authored claim settles.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries sit toward the subsidized end: the appellate judiciary collects interpretive authority and agenda control (doubly positioned — administrator and collector); norm-challenge litigants receive standing pathways that exist nowhere else in the system; the scholarship community converts uptake into influence. Declared victims sit toward the target end: reliance-holders bear the transfer with only slow, uncertain recourse (constrained exit pushes them toward full-target weighting); retroactively exposed defendants are powerless and trapped — the clearest full-target seat. The legislature bears displaced meaning but retains composition leverage over the bench, moderating its position below the trapped seats. The invoked public is deliberately left undeclared in the beneficiary/victim arrays: it is invoked as justification while receiving diffuse benefit and diffuse unpredictability, and the canonical fallback for its power atom approximates symmetry. No directionality overrides were authored: the derivation from role plus exit already places every seat correctly, and a power-atom-keyed override would collide across the two powerless seats (defendants and public) whose true positions differ.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — consistent decision without a code, continuity with correction — remains live, so this is not a mandatrophy case and mandatrophy_resolved is left undeclared. The tangled_rope classification earns its keep by blocking two symmetrical misreadings: a rope reading would credit the coordination function and miss who pays for the flexibility (reliance-holders, exposed respondents, displaced legislature); a snare reading would fixate on the payers and erase the genuine coordination without which no legal system of this scale operates. The piton path is visible as a future risk rather than a present fact: if codified statutes or uniform automated adjudication ever absorbed precedent's coordinating function, the remaining practice — ceremonial citation, factor-balancing recited but not applied — would be theatrical maintenance of an atrophied core, with the bench as agenda-setter lacking incentive to repair what it no longer profits from and diffuse payers lacking standing to demand repair.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the common_law_precedent_corpus kernel — what would the corpus''s operative structure be under the strict_stare_decisis or pluralist_balancing readings, and which structural element carries the disagreement?',
    'Not resolvable by data alone: the readings are rival normative commitments held simultaneously by different jurists and schools. Resolution would require either doctrinal convergence (one reading capturing the bench) or an explicit meta-doctrine allocating precedent''s force.',
    'Under strict_stare_decisis, rigidity and suppression rise sharply, the beneficiary set shrinks toward stability-seeking institutions, and norm-challenge litigants lose their standing pathways; under pluralist_balancing, epsilon fragments by domain and no single seat-level classification covers the corpus. The disagreement sits at one element: the standard for departure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Kernel contest: three rival readings of precedent''s binding force; this file authors only the evolutionary reading.').

omega_variable(
    norm_perception_channel,
    'Whose convictions count as ''contemporary normative evolution'' — measured public opinion, professional consensus, or the bench''s own perception — and does the channel concentrate interpretive discretion?',
    'Compare departures justified by cited social-science or polling evidence against those justified by unarticulated judicial perception; track correlation between elite-composition shifts and the direction of ''evolved'' norms.',
    'If the channel is elite perception, the flexibility concentrates interpretive rents in the bench and the payer seats'' effective burdens rise; if anchored to measurable opinion, the framework tracks its warrant and the excess falls toward coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(norm_perception_channel, conceptual, 'Source-authority ambiguity inside the reading''s own engine: who reads the evolving norms.').

omega_variable(
    reliance_loss_magnitude,
    'How large are the realized losses borne by reliance-interest holders when settled readings are revisited — transaction repricing, compliance retrofitting, invalidated expectations?',
    'Longitudinal transaction-level studies around landmark departures (contract, property, immunity-type reversals) measuring realized repricing against pre-departure hedging behavior.',
    'Small realized losses support drifting this story toward rope (flexibility as cheap correction); large realized losses push toward snare-flavored asymmetry with the bench as concentrated collector.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reliance_loss_magnitude, empirical, 'Magnitude of reliance-side costs from normalized overruling.').

omega_variable(
    legitimacy_feedback_warrant,
    'Does adaptive updating sustain or erode the courts'' institutional legitimacy — the instrumental warrant beneath this reading''s foundational axiom?',
    'Panel and survey legitimacy studies correlated with salient departure episodes; cross-jurisdiction comparison of courts that update frequently versus rarely.',
    'Sustained erosion would route the foundational axiom toward foreclosure computation (an empirically contingent warrant failing its test) and pressure the reading toward the strict sibling; sustained support stabilizes the axiom''s holdable status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_feedback_warrant, empirical, 'Empirical testability of the adaptive framework''s legitimacy warrant.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__evolutionary_framework, 1960, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clpc_evo_tr_t1960, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 1960, 0.25).
narrative_ontology:measurement_basis(clpc_evo_tr_t1960, observed).
narrative_ontology:measurement(clpc_evo_tr_t1975, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 1975, 0.3).
narrative_ontology:measurement_basis(clpc_evo_tr_t1975, observed).
narrative_ontology:measurement(clpc_evo_tr_t1990, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 1990, 0.36).
narrative_ontology:measurement_basis(clpc_evo_tr_t1990, observed).
narrative_ontology:measurement(clpc_evo_tr_t2005, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 2005, 0.4).
narrative_ontology:measurement_basis(clpc_evo_tr_t2005, observed).
narrative_ontology:measurement(clpc_evo_tr_t2015, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 2015, 0.43).
narrative_ontology:measurement_basis(clpc_evo_tr_t2015, observed).
narrative_ontology:measurement(clpc_evo_tr_t2025, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 2025, 0.42).
narrative_ontology:measurement_basis(clpc_evo_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(clpc_evo_be_t1960, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 1960, 0.4).
narrative_ontology:measurement_basis(clpc_evo_be_t1960, observed).
narrative_ontology:measurement(clpc_evo_be_t1975, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 1975, 0.44).
narrative_ontology:measurement_basis(clpc_evo_be_t1975, observed).
narrative_ontology:measurement(clpc_evo_be_t1990, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 1990, 0.49).
narrative_ontology:measurement_basis(clpc_evo_be_t1990, observed).
narrative_ontology:measurement(clpc_evo_be_t2005, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 2005, 0.53).
narrative_ontology:measurement_basis(clpc_evo_be_t2005, observed).
narrative_ontology:measurement(clpc_evo_be_t2015, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 2015, 0.53).
narrative_ontology:measurement_basis(clpc_evo_be_t2015, observed).
narrative_ontology:measurement(clpc_evo_be_t2025, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 2025, 0.52).
narrative_ontology:measurement_basis(clpc_evo_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(clpc_evo_su_t1960, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 1960, 0.16).
narrative_ontology:measurement_basis(clpc_evo_su_t1960, observed).
narrative_ontology:measurement(clpc_evo_su_t1975, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 1975, 0.21).
narrative_ontology:measurement_basis(clpc_evo_su_t1975, observed).
narrative_ontology:measurement(clpc_evo_su_t1990, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 1990, 0.27).
narrative_ontology:measurement_basis(clpc_evo_su_t1990, observed).
narrative_ontology:measurement(clpc_evo_su_t2005, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 2005, 0.31).
narrative_ontology:measurement_basis(clpc_evo_su_t2005, observed).
narrative_ontology:measurement(clpc_evo_su_t2015, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 2015, 0.34).
narrative_ontology:measurement_basis(clpc_evo_su_t2015, observed).
narrative_ontology:measurement(clpc_evo_su_t2025, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 2025, 0.35).
narrative_ontology:measurement_basis(clpc_evo_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__evolutionary_framework, enforcement_mechanism).
narrative_ontology:affects_constraint(common_law_precedent_corpus__evolutionary_framework, strict_stare_decisis).
narrative_ontology:affects_constraint(common_law_precedent_corpus__evolutionary_framework, pluralist_balancing).

% DUAL FORMULATION NOTE:
% The natural-language label 'stare decisis / precedent' conflates three structurally distinct claims about the corpus's binding force. This file authors the evolutionary_framework claim alone, with its own epsilon (0.52), beneficiary set, and classification; strict_stare_decisis (higher rigidity, higher suppression, stability-seeking beneficiaries) and pluralist_balancing (domain-fragmented epsilon) are separate files. Upstream/downstream: strict_stare_decisis is the historically upstream default whose legitimacy formula the other two cite in order to relax or contextualize it; this reading exerts structural pressure on the strict file by normalizing departure, and the pluralist file mediates between them. All three are linked through network.affects_constraints per the constraint-family rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
