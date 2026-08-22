% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__marketplace_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__marketplace_reading, []).

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
 *   constraint_id: speech_protection_kernel__marketplace_reading
 *   human_readable: Marketplace-of-Ideas Reading of Speech Protection
 *   domain: constitutional law/political philosophy/communication rights
 *
 * SUMMARY:
 *   This story instantiates ONE reading of a contested kernel. The kernel is
 *   speech protection as a constitutional commitment; this file authors the
 *   marketplace_reading: protection justified by collective epistemic benefit
 *   — truth discovered through open contestation — with content-based
 *   restrictions rejected as distortions of that process and counterspeech
 *   designated as the remedy for false and harmful expression. The standing
 *   arrangement under contest (and the fixed epsilon referent) is the
 *   operative doctrine: broad protection on epistemic grounds, a working
 *   presumption against content-based regulation, judicial enforcement
 *   striking down restrictive measures, and narrow carve-outs (incitement,
 *   defamation thresholds, true threats) left regulable. The other four
 *   readings of the kernel — absolutist, harm_threshold, dignity,
 *   democratic_participation — are separate constraints in separate files;
 *   this story does not describe the contest inside the constraint, does not
 *   hedge epsilon across readings, and does not average over them. The claim
 *   and the metrics are independent authored facts: claimed_type is
 *   tangled_rope because the arrangement possesses a genuine coordination
 *   function (it solves the ex ante censorship problem and protects
 *   dissenters who would otherwise be silenced) while simultaneously
 *   extracting asymmetrically (concentrated communicative capital gains
 *   immunity and reach; diffuse targets bear unremedied costs); the metrics
 *   are authored from what the operation descriptively shows. Epsilon is
 *   reading-indexed over that fixed referent: 0.58 is what this reading can
 *   honestly concede about the arrangement given disinformation-era evidence
 *   — a dignity or harm_threshold reading of the same arrangement would
 *   author a higher value because it counts target harms this reading's
 *   neutrality axiom discounts as epistemic process. Interval mapping: T0
 *   corresponds to roughly 1919 (the Abrams-era crystallization of the
 *   epistemic justification), T100 to roughly 2019.
 *
 * KEY AGENTS:
 *   - - constitutional_courts: Agenda setter (institutional/constrained) — administers and enforces the protection standard through doctrinal review; binds itself via precedent
 *   - - mass_media_corporations: Primary beneficiary (institutional/arbitrage) — converts protection into audience reach and liability immunity; commands the largest counterspeech channels
 *   - - digital_platform_operators: Primary beneficiary and gain concentrator (institutional/arbitrage) — hosts and amplifies expression behind the shield of content-neutrality while monetizing engagement
 *   - - political_advocacy_networks: Secondary beneficiary (organized/mobile) — depends on unimpeded distribution for campaign and attack messaging
 *   - - dissident_minority_speakers: Legitimating beneficiary (powerless/constrained) — the arrangement's genuine protection case; without it their expression is regulable out of existence
 *   - - private_figure_defamation_targets: Payer (moderate/trapped) — bear reputational injury with remedies confined to the narrowest doctrinal category
 *   - - targeted_group_members: Payer (powerless/identity_locked) — absorb degrading and exclusionary expression as a cost of the open commons; cannot resign group membership
 *   - - disinformation_exposed_audiences: Payer (moderate/constrained) — inhabit saturated information environments; individual counterspeech is negligible against industrial production
 *   - - content_restriction_legislators: Excluded (institutional/constrained) — hold electoral mandates for content-based measures the standard routinely invalidates
 *   - - comparative_speech_scholars: Analytical observer (analytical/analytical) — study how sibling-reading regimes perform on accuracy, trust, and inclusion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__marketplace_reading, 0.58).
domain_priors:suppression_score(speech_protection_kernel__marketplace_reading, 0.52).
domain_priors:theater_ratio(speech_protection_kernel__marketplace_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__marketplace_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__marketplace_reading, "Marketplace-of-Ideas Reading of Speech Protection").
narrative_ontology:topic_domain(speech_protection_kernel__marketplace_reading, "constitutional law/political philosophy/communication rights").

domain_priors:requires_active_enforcement(speech_protection_kernel__marketplace_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__marketplace_reading, 'dc632cde-8b0f-41b6-8bcb-3e340ebd624d').
narrative_ontology:cs_kernel_codification('dc632cde-8b0f-41b6-8bcb-3e340ebd624d', formalized).
narrative_ontology:cs_authority_grounding('dc632cde-8b0f-41b6-8bcb-3e340ebd624d', lineage).
narrative_ontology:cs_interpretation_layer_present('dc632cde-8b0f-41b6-8bcb-3e340ebd624d').
narrative_ontology:cs_reading_relation('dc632cde-8b0f-41b6-8bcb-3e340ebd624d', speech_protection_kernel__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('dc632cde-8b0f-41b6-8bcb-3e340ebd624d', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_reading_relation('dc632cde-8b0f-41b6-8bcb-3e340ebd624d', speech_protection_kernel__harm_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('dc632cde-8b0f-41b6-8bcb-3e340ebd624d', speech_protection_kernel__dignity_reading, forecloses).
narrative_ontology:cs_axiom('dc632cde-8b0f-41b6-8bcb-3e340ebd624d', foundational, truth_discovery_requires_content_neutrality).
narrative_ontology:cs_axiom_status(truth_discovery_requires_content_neutrality, holdable).
narrative_ontology:cs_axiom_grounding('dc632cde-8b0f-41b6-8bcb-3e340ebd624d', truth_discovery_requires_content_neutrality, empirically_contingent).
narrative_ontology:cs_axiom('dc632cde-8b0f-41b6-8bcb-3e340ebd624d', secondary, counterspeech_remedy_over_restriction).
narrative_ontology:cs_axiom_status(counterspeech_remedy_over_restriction, holdable).
narrative_ontology:cs_axiom_grounding('dc632cde-8b0f-41b6-8bcb-3e340ebd624d', counterspeech_remedy_over_restriction, instrumental).
narrative_ontology:cs_reference_frame('dc632cde-8b0f-41b6-8bcb-3e340ebd624d', millian_open_epistemic_commons).
narrative_ontology:cs_drift_state('dc632cde-8b0f-41b6-8bcb-3e340ebd624d', contemporary_algorithmic_disinformation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('dc632cde-8b0f-41b6-8bcb-3e340ebd624d', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__marketplace_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, mass_media_corporations).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, digital_platform_operators).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, political_advocacy_networks).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, dissident_minority_speakers).
narrative_ontology:constraint_victim(speech_protection_kernel__marketplace_reading, private_figure_defamation_targets).
narrative_ontology:constraint_victim(speech_protection_kernel__marketplace_reading, targeted_group_members).
narrative_ontology:constraint_victim(speech_protection_kernel__marketplace_reading, disinformation_exposed_audiences).
narrative_ontology:constraint_vindicates(speech_protection_kernel__marketplace_reading, content_neutrality_doctrine).
narrative_ontology:constraint_vindicates(speech_protection_kernel__marketplace_reading, counterspeech_sufficiency_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Review legislation and official action against the speech-protective standard, invalidating measures that turn on the content or viewpoint of expression. Precedent defines what protection covers and what stays regulable; the judges bind themselves through stare decisis and can revisit the standard only at high institutional cost. They collect no revenue from the arrangement and bear none of its informational costs; their stake is the maintenance obligation itself.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, constitutional_courts, agenda_setter,
    institutional, generational, constrained, national).

% Operate news and entertainment businesses whose products are expressive acts. They invoke the standard to defeat liability and content-based regulation, reach audiences without gatekeeping, and litigate from deep pockets across jurisdictions. Their exposure to the arrangement's costs is limited because they command the largest counterspeech channels — the designated remedy works best for exactly them.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, mass_media_corporations, beneficiary,
    institutional, generational, arbitrage, global).

% Run the infrastructures through which most public expression now circulates. The standard shields their hosting and amplification decisions from content-based regulation while engagement-driven distribution monetizes whatever draws attention, including falsehood. They can relocate operations and legal exposure across jurisdictions and face no comparably scaled counterparty.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, digital_platform_operators, beneficiary,
    institutional, generational, arbitrage, global).

% Organize campaigns, fundraising appeals, and opposition messaging that depend on unimpeded broadcast and digital distribution. The standard protects their most aggressive material from content-based restriction. They remain subject to ordinary election and finance law but not to rules aimed at what their expression says.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, political_advocacy_networks, beneficiary,
    organized, biographical, mobile, national).

% Speak against governments, majorities, and orthodoxies with few resources and small audiences. The standard is frequently the only thing standing between their expression and official suppression; without it their speech would be regulated out of existence. With it they survive, though their reach stays small and the remedy asymmetry still favors their opponents.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, dissident_minority_speakers, beneficiary,
    powerless, biographical, constrained, national).

% Are private individuals who suffer reputational injury from false publication about them. Doctrinal thresholds confine their remedies to the narrowest category of statements, made with demonstrated disregard for truth, on matters outside public concern. They cannot exit the circulation of a falsehood and cannot buy correction at anything like the scale of the original dissemination.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, private_figure_defamation_targets, payer,
    moderate, biographical, trapped, national).

% Belong to groups subjected to degrading, dehumanizing, or exclusionary expression. The standard treats such expression as contribution to public contestation, and the designated remedy — responding with more speech — presumes resources, standing, and audience that most members lack. Membership in the targeted group is constitutive of who they are; it is not something they can resign, and the harm tracks them across generations.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, targeted_group_members, payer,
    powerless, generational, identity_locked, national).

% Inhabit information environments saturated with coordinated falsehood amplified for engagement. Individual responses — unfollowing, fact-checking, replying — are negligible against industrial-scale production. They can curate personal feeds but cannot exit the shared epistemic environment their markets, institutions, and elections run on.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, disinformation_exposed_audiences, payer,
    moderate, biographical, constrained, global).

% Propose content-based measures — disinformation rules, dignity protections, harm-based standards — carrying electoral mandates, which the courts routinely invalidate. The standard excludes them from regulating this field regardless of majority support. Their remaining instruments are viewpoint-neutral time, place, and manner rules, funding levers, and persuasion.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, content_restriction_legislators, excluded,
    institutional, biographical, constrained, national).

% Study how peer democracies operating harm-conditioned or dignity-conditioned speech regimes perform on accuracy, institutional trust, and inclusion relative to the content-neutral model. They publish trade-off analyses and testify in reform debates without holding, enforcing, or financially depending on the standard.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, comparative_speech_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_kernel__marketplace_reading, digital_platform_operators).
narrative_ontology:fixing_cost_class(speech_protection_kernel__marketplace_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the ex ante censorship problem: no agent — above all the state — is empowered to decide which claims may enter public contestation, so error-correction runs through open rebuttal rather than gatekeeping. It coordinates the shared expectation that unpopular or false claims will be answered rather than suppressed, and it protects the dissent on which epistemic and political correction both depend.
% TRANSFER_FUNCTION: Moves expressive opportunity, audience reach, and immunity from content-based restriction toward holders of communicative capital — media firms, platform operators, organized advocacy, and protected dissenters — and moves the costs of falsehood and demeaning expression onto private-figure targets, members of targeted groups, and audiences who lack effective counterspeech capacity.
% ABSENT_VOICES: Private-figure defamation targets and members of groups subjected to degrading expression appear only as hypothetical litigants behind doctrinal thresholds; legislators with mandates for content-based remedies are excluded by the standard itself; proponents of deliberative-quality or dignitary frameworks sit outside the arrangement's justificatory terms entirely and therefore never appear as counterparties in its administration.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, content-based regulation would proliferate within a single legislative session; platform and media compliance structures would rebuild around per-content clearance; dissent and minority expression would become discretionary objects of majority tolerance; and defamation and group-harm liability would expand sharply. Legal doctrine, media economics, and protest politics all currently depend on the standard's operation.
% FOUNDING_PROBLEM: Early twentieth-century state suppression: sedition prosecutions, prior restraint, and licensing regimes punishing criticism of government and war policy — the problem of official censorship entrenching error and protecting illegitimate power from contestation.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: civil-liberties monitoring organizations and press-freedom indices document continuing censorship pressure in peer democracies (journalist prosecutions, attempted removals of books and curricula, emergency speech controls), confirming the founding problem persists. Simultaneously, comparative-law scholars and disinformation researchers — also outside the beneficiary set — attest that the arrangement now overshoots that founding problem by immunizing industrial-scale falsehood, disputing the adequacy of this reading's solution rather than the liveness of the problem.
narrative_ontology:disappearance_verdict(speech_protection_kernel__marketplace_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__marketplace_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__marketplace_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_protection_kernel__marketplace_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__marketplace_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__marketplace_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_kernel__marketplace_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_kernel__marketplace_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.58: the arrangement coordinates genuinely (no agent decides ex ante which claims may be heard; dissent survives), but the costs of falsehood and demeaning expression fall on seats with no effective remedy while immunity and reach concentrate in communicative capital. Suppression 0.52: persistence depends on active judicial enforcement — courts must continuously strike down content-based measures for the arrangement to hold, so the restriction alternative is coercively foreclosed rather than merely unused; suppression is authored as a raw structural property and is not scaled by power or scope. Theater_ratio 0.35: the truth-discovery function is real, but the designated remedy grows increasingly performative as the resource asymmetry between industrial falsehood-production and individual response widens — 'the answer is more speech' describes an exchange that only the well-resourced can actually conduct. Accessibility_collapse 0.42: alternatives do not fully collapse — time/place/manner rules, the incitement and true-threat carve-outs, and harm-regulated regimes abroad remain visible and partly usable, which is characteristic of a construct that must be defended rather than a natural limit. Resistance 0.6: sustained legislative attempts at content-based regulation, dignity-based movements, and comparative-policy pressure meet the standard continuously. The temporal series run on one shared grid (all three metrics at T0/20/40/60/80/100): base_extractiveness climbs as communicative capital concentrates (extraction accumulation layered onto a protective origin); theater_ratio climbs as the remedy rhetoric detaches from remedy reality; suppression_requirement climbs as enforcement machinery hardens from early protective review into mature strict-scrutiny doctrine — the story specifically traces enforcement-capacity change, which is why suppression_requirement is authored rather than left static.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute materially different types from identical structural data. From the dissident_minority_speakers seat the arrangement is the thing that makes expression possible at all — a lifeline, experienced as near-pure coordination. From the mass_media_corporations and digital_platform_operators seats it is a revenue-bearing immunity — coordination shading into subsidy. From the private_figure_defamation_targets and targeted_group_members seats the same structure operates as enforced exposure: a rule that affirmatively disables their remedies. The constitutional_courts seat experiences maintenance duty — the obligation to invalidate democratically enacted measures — which reads as neutral administration from the bench and as suppression from the legislature's excluded position. The engine computes these divergences from power, exit, and role data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: mass_media_corporations and digital_platform_operators (arbitrage-grade exit, institutional power) sit nearest the beneficiary end — the arrangement subsidizes them; political_advocacy_networks similarly; dissident_minority_speakers are the strongest subsidy case (powerless, constrained exit, wholly dependent on the protection). Victim declarations drive high directionality: private_figure_defamation_targets are trapped (cannot exit the circulation of a falsehood, cannot purchase correction at scale); disinformation_exposed_audiences are constrained (can curate feeds, cannot exit the shared epistemic environment); targeted_group_members are identity_locked — their exposure runs through group membership they cannot resign, fusing the constraint to who they are, which places them nearest the full-target end and makes their effective extraction the highest in the story. Identity-lock here is relational and constitutive: the harmed position IS the identity, so exit is not merely costly but conceptually unavailable; if the identity frame broke (if the expression stopped tracking group membership), these agents would migrate toward the constrained profile of the audiences seat. The constitutional_courts seat derives near-symmetric: it administers without collecting. No directionality overrides are used — the beneficiary/victim plus exit derivations produce accurate values for every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — official censorship entrenching error and illegitimate power — remains live, so no mandatrophy is declared: the arrangement has not outlived its mandate. The classification discipline prevents two opposite mislabelings. Labeling the arrangement pure coordination (rope) would erase the measurable asymmetric extraction: targets whose remedies are doctrinally disabled, audiences paying an epistemic tax, and the concentration of immunity in precisely the actors with the least need of protection. Labeling it pure extraction (snare) would erase the genuine coordination core: dissident and minority speakers are real net beneficiaries, the anti-censorship function operates, and peer democracies without comparable protection show measurably worse dissent survival. Tangled_rope holds both facts in one structure — which is also why the drift signal matters: the theater_ratio trajectory indicates the counterspeech remedy component is progressively atrophying toward performance while the immunity component strengthens, a decomposition the per-seat computation makes visible that a single scalar label would hide.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the speech_protection_kernel — the marketplace_reading, which justifies speech protection by collective epistemic benefit and rejects content-based restrictions as distorting the truth-discovery process. What would each sibling reading change structurally, and where exactly is the disagreement located?',
    'Framework-level analysis of which justificatory bases can coexist inside a single doctrinal commitment, combined with comparative doctrine tracing which grounds of restriction each reading actually admits.',
    'The disagreement is located in two elements: the justificatory basis of protection (epistemic benefit here; autonomy, dignity, harm-avoidance, or self-governance in the siblings) and the permissibility of content-based grounds of restriction. Adopting a sibling reading changes the victim set — the harm_threshold and dignity readings make targets of demonstrable harm and subordination remedy-eligible, which this reading discounts as part of the epistemic process — and changes the restriction space, validating content-based rules this reading forecloses.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: this file instantiates the marketplace_reading of speech_protection_kernel; sibling readings relocate the justificatory basis and the content-restriction boundary.').

omega_variable(
    counterspeech_sufficiency_under_amplification,
    'Is the designated remedy — answering false and harmful speech with more speech — empirically sufficient when falsehood production and amplification operate at industrial scale with engagement-optimized distribution?',
    'Cross-jurisdiction comparison of falsehood prevalence, correction latency, and belief persistence between content-neutral regimes and harm-regulated peer democracies, controlling for platform penetration.',
    'If the remedy is insufficient, the counterspeech component of the arrangement is largely performative and effective extraction rises above the authored value; if sufficient, a larger share of measured extraction is the price of the coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterspeech_sufficiency_under_amplification, empirical, 'Whether the reading''s foundational remedy assumption survives algorithmic amplification and epistemic inequality.').

omega_variable(
    extraction_attribution_across_kernel,
    'How much of the measured extraction belongs to this reading specifically, versus the protection kernel common to all five readings?',
    'Compare remedy availability and target outcomes across jurisdictions whose doctrine operates sibling readings; isolate outcomes that flip with the content-restriction boundary rather than with protection breadth.',
    'All readings protect speech, so baseline anti-censorship benefit is shared; the reading-specific delta is its discounting of target harm via the neutrality axiom. Attribution recalibrates epsilon for the whole constraint family and determines which sibling files inherit contamination when this reading''s purity degrades.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_attribution_across_kernel, conceptual, 'Separating reading-specific extraction from kernel-shared coordination benefit.').

omega_variable(
    beneficiary_concentration_question,
    'Does the arrangement chiefly protect marginal and dissident speakers — its legitimating image — or concentrated communicative capital?',
    'Distributional analysis of who successfully invokes the standard: litigant profiles in speech litigation, platform-policy disputes, and regulatory challenges, weighted by outcome value obtained.',
    'If benefits concentrate in media firms, platforms, and organized advocacy while costs fall on diffuse targets, the coordination function legitimates extraction it does not distribute, supporting the tangled assessment; if benefits are genuinely diffuse, the arrangement sits closer to pure coordination than the authored metrics suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_concentration_question, empirical, 'Whether the epistemic-commons justification maps onto the actual distribution of who gains.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__marketplace_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_kernel__marketplace_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(spee_tr_t0, observed).
narrative_ontology:measurement(spee_tr_t20, speech_protection_kernel__marketplace_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement_basis(spee_tr_t20, observed).
narrative_ontology:measurement(spee_tr_t40, speech_protection_kernel__marketplace_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement_basis(spee_tr_t40, observed).
narrative_ontology:measurement(spee_tr_t60, speech_protection_kernel__marketplace_reading, theater_ratio, 60, 0.28).
narrative_ontology:measurement_basis(spee_tr_t60, observed).
narrative_ontology:measurement(spee_tr_t80, speech_protection_kernel__marketplace_reading, theater_ratio, 80, 0.32).
narrative_ontology:measurement_basis(spee_tr_t80, observed).
narrative_ontology:measurement(spee_tr_t100, speech_protection_kernel__marketplace_reading, theater_ratio, 100, 0.35).
narrative_ontology:measurement_basis(spee_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_kernel__marketplace_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(spee_be_t0, observed).
narrative_ontology:measurement(spee_be_t20, speech_protection_kernel__marketplace_reading, base_extractiveness, 20, 0.36).
narrative_ontology:measurement_basis(spee_be_t20, observed).
narrative_ontology:measurement(spee_be_t40, speech_protection_kernel__marketplace_reading, base_extractiveness, 40, 0.44).
narrative_ontology:measurement_basis(spee_be_t40, observed).
narrative_ontology:measurement(spee_be_t60, speech_protection_kernel__marketplace_reading, base_extractiveness, 60, 0.52).
narrative_ontology:measurement_basis(spee_be_t60, observed).
narrative_ontology:measurement(spee_be_t80, speech_protection_kernel__marketplace_reading, base_extractiveness, 80, 0.56).
narrative_ontology:measurement_basis(spee_be_t80, observed).
narrative_ontology:measurement(spee_be_t100, speech_protection_kernel__marketplace_reading, base_extractiveness, 100, 0.58).
narrative_ontology:measurement_basis(spee_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_kernel__marketplace_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(spee_su_t0, observed).
narrative_ontology:measurement(spee_su_t20, speech_protection_kernel__marketplace_reading, suppression_requirement, 20, 0.3).
narrative_ontology:measurement_basis(spee_su_t20, observed).
narrative_ontology:measurement(spee_su_t40, speech_protection_kernel__marketplace_reading, suppression_requirement, 40, 0.38).
narrative_ontology:measurement_basis(spee_su_t40, observed).
narrative_ontology:measurement(spee_su_t60, speech_protection_kernel__marketplace_reading, suppression_requirement, 60, 0.45).
narrative_ontology:measurement_basis(spee_su_t60, observed).
narrative_ontology:measurement(spee_su_t80, speech_protection_kernel__marketplace_reading, suppression_requirement, 80, 0.5).
narrative_ontology:measurement_basis(spee_su_t80, observed).
narrative_ontology:measurement(spee_su_t100, speech_protection_kernel__marketplace_reading, suppression_requirement, 100, 0.52).
narrative_ontology:measurement_basis(spee_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__marketplace_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__dignity_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__democratic_participation_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'freedom of speech' covers five structurally distinct claims that share one kernel but differ in justificatory basis, victim set, and restriction space. This file instantiates the marketplace_reading alone; each sibling is a separate story with its own epsilon, beneficiaries, and victims. Epsilon differs across the family because each reading counts different costs over the SAME standing arrangement: the harm_threshold and dignity readings count target harms this reading discounts as epistemic process, so their authored extraction exceeds this file's. Genealogically this reading is upstream — its epistemic framing shaped the others' development — and the network edges carry that influence alongside the coexistence and foreclosure relations declared in cs_structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
