% ============================================================================
% CONSTRAINT STORY: first_amendment_speech_protection__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_first_amendment_speech_protection__absolutist_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: first_amendment_speech_protection__absolutist_reading
 *   human_readable: First Amendment Categorical Speech Protection (Absolutist Reading)
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the First Amendment
 *   speech-protection kernel: the absolutist reading, under which the text
 *   'no law' is an unconditional command and protection is categorical except
 *   for a narrow, historically fixed set of exclusions. The arrangement
 *   solves a real and recurrent problem — government censorship of dissent —
 *   through a rule that removes regulatory discretion entirely. Its costs are
 *   allocated asymmetrically: speakers and media collect protection and
 *   commercial freedom, while the harms of tolerated hostile expression land
 *   on the minorities and individuals those expressions target, who lack
 *   recourse and cannot exit. Per the epsilon-invariance principle, the
 *   colloquial label 'First Amendment protection' decomposes into three
 *   structurally distinct constraints (this absolutist reading, the
 *   harm_limited_reading, and the categorical_balancing_reading), linked
 *   through network.affects_constraints; each carries its own victim set and
 *   its own epsilon. The claim/metric gap is deliberate: the reading is
 *   CLAIMED here as tangled_rope (my structural judgment — genuine
 *   coordination plus asymmetric cost-bearing plus active enforcement), while
 *   the metrics describe the regime's actual operation independently.
 *
 * KEY AGENTS:
 *   - political_dissidents: Primary intended beneficiary (moderate/constrained) — the protected core the rule was built for
 *   - mass_media_publishers: Concentrated commercial beneficiary (institutional/arbitrage) — collects the largest monetizable gains
 *   - majority_culture_speakers: Zero-cost beneficiary (organized/mobile) — formal protection at no personal expense
 *   - systemically_targeted_minorities: Primary target (powerless/trapped) — bears generational costs of protected hostile speech
 *   - repeatedly_harassed_individuals: Secondary target (powerless/constrained) — acute, recurring harassment burdens
 *   - federal_judiciary: Agenda setter and enforcer (institutional/constrained) — maintains the regime by striking down regulation
 *   - state_legislatures: Repeatedly defeated payer (institutional/constrained) — foreclosed regulatory agendas
 *   - critical_race_theorists: Analytical observer (analytical/analytical) — documents the distributive incidence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__absolutist_reading, 0.62).
domain_priors:suppression_score(first_amendment_speech_protection__absolutist_reading, 0.65).
domain_priors:theater_ratio(first_amendment_speech_protection__absolutist_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__absolutist_reading, tangled_rope).
narrative_ontology:human_readable(first_amendment_speech_protection__absolutist_reading, "First Amendment Categorical Speech Protection (Absolutist Reading)").
narrative_ontology:topic_domain(first_amendment_speech_protection__absolutist_reading, "constitutional/political").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__absolutist_reading, 'e89a03b6-a4d0-4b00-9654-656e7cfcb0fd').
narrative_ontology:cs_kernel_codification('e89a03b6-a4d0-4b00-9654-656e7cfcb0fd', fixed_text).
narrative_ontology:cs_authority_grounding('e89a03b6-a4d0-4b00-9654-656e7cfcb0fd', lineage).
narrative_ontology:cs_interpretation_layer_present('e89a03b6-a4d0-4b00-9654-656e7cfcb0fd').
narrative_ontology:cs_reading_relation('e89a03b6-a4d0-4b00-9654-656e7cfcb0fd', first_amendment_speech_protection__harm_limited_reading, forecloses).
narrative_ontology:cs_reading_relation('e89a03b6-a4d0-4b00-9654-656e7cfcb0fd', first_amendment_speech_protection__categorical_balancing_reading, forecloses).
narrative_ontology:cs_axiom('e89a03b6-a4d0-4b00-9654-656e7cfcb0fd', foundational, no_law_admits_no_balancing).
narrative_ontology:cs_axiom_status(no_law_admits_no_balancing, holdable).
narrative_ontology:cs_axiom_grounding('e89a03b6-a4d0-4b00-9654-656e7cfcb0fd', no_law_admits_no_balancing, deontological).
narrative_ontology:cs_axiom('e89a03b6-a4d0-4b00-9654-656e7cfcb0fd', secondary, unprotected_set_frozen_to_historical_exclusions).
narrative_ontology:cs_axiom_status(unprotected_set_frozen_to_historical_exclusions, holdable).
narrative_ontology:cs_axiom_grounding('e89a03b6-a4d0-4b00-9654-656e7cfcb0fd', unprotected_set_frozen_to_historical_exclusions, conventional).
narrative_ontology:cs_reference_frame('e89a03b6-a4d0-4b00-9654-656e7cfcb0fd', unconditional_no_law_textual_command).
narrative_ontology:cs_drift_state('e89a03b6-a4d0-4b00-9654-656e7cfcb0fd', contemporary_carveout_accumulated_doctrine, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e89a03b6-a4d0-4b00-9654-656e7cfcb0fd', '').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__absolutist_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__absolutist_reading, political_dissidents).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__absolutist_reading, mass_media_publishers).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__absolutist_reading, majority_culture_speakers).
narrative_ontology:constraint_victim(first_amendment_speech_protection__absolutist_reading, systemically_targeted_minorities).
narrative_ontology:constraint_victim(first_amendment_speech_protection__absolutist_reading, repeatedly_harassed_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(first_amendment_speech_protection__absolutist_reading, state_legislatures).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__absolutist_reading, marketplace_of_ideas_doctrine).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__absolutist_reading, viewpoint_neutrality_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Publish criticism of officials, organize unpopular causes, and expose official misconduct under the shelter of near-absolute protection; historically the group the provision was built for. Their alternatives — self-censorship or exile — are severe, and they depend on the regime continuing.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, political_dissidents, beneficiary,
    moderate, biographical, constrained, national).

% Operate newspapers, broadcasters, and publishing houses with near-total freedom to disseminate contentious material, largely free of tort exposure for offensive-but-protected content and of regulatory licensing. Their commercial models depend on this freedom; they could relocate operations abroad but have every incentive to stay and collect.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, mass_media_publishers, beneficiary,
    institutional, biographical, arbitrage, national).

% Express mainstream views that no regulator threatens; receive the same formal protection at essentially zero personal cost, and supply the electoral constituency that sustains the regime. Their expression would survive intact under any of the rival readings.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, majority_culture_speakers, beneficiary,
    organized, biographical, mobile, national).

% Live under a speech order in which racist advocacy, extremist recruitment, and group-directed vilification are protected or nearly so; absorb the resulting harassment, intimidation, and civic-withdrawal costs across generations; have no legal recourse against most of it and cannot exit their identity or cheaply leave the jurisdiction.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, systemically_targeted_minorities, payer,
    powerless, generational, trapped, national).

% Endure sustained targeted harassment campaigns that stop short of true threats; available remedies reduce to platform rules, blocking, and narrowed tort theories; each campaign restarts the burden, and changing neighborhoods, jobs, or platforms only partially escapes it.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, repeatedly_harassed_individuals, payer,
    powerless, immediate, constrained, national).

% Life-tenured judges review every speech regulation brought before them, strike down measures that burden protected expression, and maintain the list of recognized exceptions. Their opinions are the operative text of the regime; they cannot decline the docket or opt out of the role, and appointment cycles shape the doctrine for decades.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Pass statutes targeting harassing, hateful, or deceptive speech; see them enjoined and struck down with regularity; bear the litigation costs of repeated defeats. Their policy agendas in this area are foreclosed by the federal doctrine, and they cannot withdraw from its supremacy.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, state_legislatures, payer,
    institutional, biographical, constrained, regional).

% Scholars documenting who bears the costs of categorical protection: compiling harassment-incidence data, tracking which communities absorb dignitary and safety harms, and arguing that the rule's formal neutrality conceals its distributive incidence. They publish, testify, and appear as amici but hold no decision seat.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, critical_race_theorists, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(first_amendment_speech_protection__absolutist_reading, mass_media_publishers).
narrative_ontology:fixing_cost_class(first_amendment_speech_protection__absolutist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the government-censorship collective-action problem: by removing case-by-case discretion over speech, it blocks the historically recurring pattern in which temporary majorities silence dissenters, unpopular religions, labor organizers, and civil-rights advocates. One categorical rule replaces ad hoc judgments that majorities reliably capture.
% TRANSFER_FUNCTION: Moves the costs of tolerated harmful expression — dignitary injury, harassment burdens, intimidation-driven civic withdrawal — from speakers onto the people and groups the harmful expression targets; formally distributes protection to everyone while the practical value concentrates on speakers whose expression is contested and the commercial value on mass media.
% ABSENT_VOICES: The targets of protected harmful speech had no seat when the doctrine formed — enslaved people, women, and the unpropertied were absent at ratification, and modern harassment targets appear in case files only as party names, never as participants. They would object that the neutrality of the rule is purchased with their safety; they stand outside the courtroom, the doctrinal seminar, and the historical record of adoption.
% DISAPPEARANCE_RATIONALE: Legislatures would regulate hateful and harassing speech within a season; prosecutors would charge under new statutes; platform moderation duties would shift; a large volume of currently protected publications, rallies, and campaigns would become actionable. The expressive economy — and the daily speech environment of targeted groups — would reorganize almost immediately.
% FOUNDING_PROBLEM: Government suppression of political dissent: the Alien and Sedition Acts, prior-restraint traditions, and a century of prosecutions of abolitionists, labor organizers, pacifists, and civil-rights advocates showed that left unconstrained, governments criminalize unpopular speech whenever their power feels threatened.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties by prosecutorial records (World War I sedition prosecutions, Espionage Act leak cases), historical scholarship on the Sedition Acts, and ongoing dockets in which journalists and whistleblowers face government process — sources with no stake in speakers' commercial gains. Recurrent censorship attempts across administrations attest the founding problem remains live.
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__absolutist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__absolutist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(first_amendment_speech_protection__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(first_amendment_speech_protection__absolutist_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(first_amendment_speech_protection__absolutist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(first_amendment_speech_protection__absolutist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(first_amendment_speech_protection__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.62 because the categorical rule transfers a large, growing volume of speech-harm cost onto identifiable bearers while its protection value concentrates on speakers; it is not higher because the anti-censorship function is real, corroborated, and exercised daily. Suppression is 0.65 as a RAW structural property — unscaled by power or scope — reflecting that the regime persists by actively striking down the legislative alternatives (harassment codes, hate-speech bans, disinformation rules) that peer democracies maintain; the alternatives remain visible and workable elsewhere, which is why accessibility_collapse is low (0.35) rather than mountain-like. Resistance is 0.6: sustained scholarly, legislative, and litigious opposition meets the rule continuously. Theater_ratio is 0.36: the gap between the absolute rhetoric ('no law') and the operating doctrine (incitement, defamation, obscenity, fraud, true threats, solicitation carve-outs) is real but the enforcement activity underneath is functional, not performative. The three temporal series run on ONE shared grid (points 0/15/30/45/60/75): extractiveness climbs as media reach expands and harm salience grows while the protected set stays fixed; theater climbs as carve-outs accumulate against the absolute frame; suppression_requirement climbs as regulatory appetite (campaign finance, harassment, synthetic media) collides with the doctrinal wall and demands ever-more-active judicial policing. Fixing cost is prohibitive: Article V supermajorities plus entrenched judicial doctrine place removal far beyond any current coalition's reach, independent of where gains flow.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same text. From the dissident seat the arrangement is liberty itself — the difference between publishing and prison. From the targeted-minority seat the identical rule is an imposed risk environment they did not consent to and cannot leave. From the judiciary seat it is doctrinal craft: line-drawing among exceptions. From the state-legislature seat it is a foreclosed policy space paid for in lost litigation. The engine derives these per-seat classifications from the structural data (power, exit, role); this story does not adjudicate between them — the divergence IS the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for political_dissidents (genuine, load-bearing protection), mass_media_publishers (subsidized commercial freedom — and, per the receipt surface, the seat where gains demonstrably accrue in concentrated form), and majority_culture_speakers (nominal beneficiaries whose practical benefit is trivial but whose costs are nil). Victim declarations drive high directionality for systemically_targeted_minorities (trapped exit amplifies toward the full-target end) and repeatedly_harassed_individuals (constrained exit, slightly less amplified). The federal_judiciary sits mid-range: it administers and enforces but collects no rents from the arrangement. State_legislatures derive high directionality as repeat losers whose regulatory authority is the thing being suppressed. No overrides are needed: the beneficiary/victim declarations plus differentiated exit atoms already separate the seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — state censorship of dissent — is live, so mandatrophy does not apply and the arrangement is not a piton-in-waiting: it performs its original function continuously. The classification discipline cuts both ways here. Reading the regime as pure rope would erase the measurable cost-incidence on targeted minorities that the structural delta names; reading it as a snare would erase the corroborated, historically decisive protection it affords dissidents against recurrent state censorship. Tangled_rope holds both facts in one structure: a genuine coordination function (categorical anti-censorship), asymmetric extraction through the same structure (externalized harm), and a hard requirement of active enforcement (judicial review) without which the arrangement collapses within a season.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is one reading of the first_amendment_speech_protection kernel: how would instantiating the harm_limited_reading or the categorical_balancing_reading instead change the victim set and epsilon?',
    'Author and compare the two sibling stories: enumerate each reading''s protected/unprotected boundary, identify who gains legal recourse under each, and recompute cost-incidence per reading.',
    'Under harm_limited_reading the victim set contracts (harassment targets gain recourse) and epsilon falls; under categorical_balancing_reading victims become case-contingent and epsilon varies by case. The absolutist reading carries the widest protected set and therefore the fullest externalization of harm onto targeted groups.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: which reading of the speech-protection kernel is instantiated, and what the siblings would change.').

omega_variable(
    neutrality_vs_incidence,
    'Is the concentration of speech-harm costs on targeted minorities an incidental byproduct of a formally neutral rule, or constitutive of the rule''s design and continued maintenance?',
    'Distributional analysis: compare harm incidence and enforcement patterns across groups; test whether the doctrine''s existing carve-outs (true threats, incitement) track the harms minorities actually face or systematically miss them.',
    'If constitutive, the cost-allocation is intrinsic to the arrangement and resistant to facially neutral reform; if incidental, tailoring existing carve-outs could reduce minority cost-bearing without abandoning categorical protection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neutrality_vs_incidence, empirical, 'Whether the distributive pattern is design or byproduct.').

omega_variable(
    externalized_cost_magnitude,
    'How large are the systemic oppression costs borne by targeted minorities under categorical protection, relative to the coordination value the protection delivers?',
    'Longitudinal studies linking protected hostile speech to mental-health, political-participation, and residential/platform-mobility outcomes in targeted communities, weighed against documented deterrence of government censorship.',
    'Sets epsilon precisely and locates the arrangement on the tangled-rope/snare boundary: a dominant externalized-cost term pushes toward snare; a dominant censorship-deterrence term stabilizes tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externalized_cost_magnitude, empirical, 'Magnitude of the externalized harm term versus the coordination benefit term.').

omega_variable(
    historical_exclusion_boundary,
    'Which exclusions count as ''narrow historical'' is contestable: does the frozen list include true threats, fraud, solicitation — and where do novel categories such as synthetic sexual imagery fall?',
    'Doctrinal tracing: catalog the recognized exclusions, test each against the reading''s own ratification-era criterion, and observe how courts treat newly emerging harm categories.',
    'Each admitted new exclusion converts part of the protected set into balanced territory, drifting the reading in practice toward categorical_balancing_reading and shrinking the victim set; refusal to admit any freezes the externalization in place.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_exclusion_boundary, conceptual, 'Stability of the exclusion list on which the reading''s categorical character depends.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__absolutist_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fa_absolutist_tr_t0, first_amendment_speech_protection__absolutist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(fa_absolutist_tr_t15, first_amendment_speech_protection__absolutist_reading, theater_ratio, 15, 0.24).
narrative_ontology:measurement(fa_absolutist_tr_t30, first_amendment_speech_protection__absolutist_reading, theater_ratio, 30, 0.27).
narrative_ontology:measurement(fa_absolutist_tr_t45, first_amendment_speech_protection__absolutist_reading, theater_ratio, 45, 0.31).
narrative_ontology:measurement(fa_absolutist_tr_t60, first_amendment_speech_protection__absolutist_reading, theater_ratio, 60, 0.34).
narrative_ontology:measurement(fa_absolutist_tr_t75, first_amendment_speech_protection__absolutist_reading, theater_ratio, 75, 0.36).

% Extraction over time
narrative_ontology:measurement(fa_absolutist_be_t0, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement(fa_absolutist_be_t15, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 15, 0.49).
narrative_ontology:measurement(fa_absolutist_be_t30, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 30, 0.53).
narrative_ontology:measurement(fa_absolutist_be_t45, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 45, 0.57).
narrative_ontology:measurement(fa_absolutist_be_t60, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 60, 0.6).
narrative_ontology:measurement(fa_absolutist_be_t75, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 75, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(fa_absolutist_su_t0, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 0, 0.46).
narrative_ontology:measurement(fa_absolutist_su_t15, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 15, 0.51).
narrative_ontology:measurement(fa_absolutist_su_t30, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(fa_absolutist_su_t45, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 45, 0.59).
narrative_ontology:measurement(fa_absolutist_su_t60, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 60, 0.62).
narrative_ontology:measurement(fa_absolutist_su_t75, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 75, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__absolutist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(first_amendment_speech_protection__absolutist_reading, first_amendment_speech_protection__harm_limited_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__absolutist_reading, first_amendment_speech_protection__categorical_balancing_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'First Amendment speech protection' per the epsilon-invariance principle. The single label conflates three structurally distinct claims: the absolutist reading (this file — categorical protection, widest protected set, victims = targeted minorities bearing externalized harm, epsilon ~0.62), the harm_limited_reading (protection yields to demonstrable unconsented harm — smaller victim set, lower epsilon), and the categorical_balancing_reading (case-by-case weighing — case-contingent victim set, unstable epsilon). The absolutist reading is upstream: its precedent raises the evidentiary and doctrinal bar the sibling readings must clear, so its operation structurally influences both siblings' operating environments. Each file links the others through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
