% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__democratic_participation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__democratic_participation_reading, []).

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
 *   constraint_id: speech_protection_kernel__democratic_participation_reading
 *   human_readable: Democratic-Participation Hierarchy of Speech Protection
 *   domain: constitutional_law/political_philosophy/communication_rights
 *
 * SUMMARY:
 *   The standing arrangement under contest is a constitutional order that
 *   protects expression unequally by design: expression sorted as
 *   governance-relevant (criticism of officials, opposition organizing,
 *   accountability reporting) sits in a near-unconditionally protected core,
 *   while everything sorted peripheral (art, commerce, intimacy, unclassified
 *   new genres) faces restriction on ordinary statutory terms. This file
 *   instantiates ONE reading of the speech_protection_kernel — the
 *   democratic_participation_reading — and authors epsilon only for this
 *   hierarchical arrangement as this reading assesses it. The kernel
 *   decomposes, per the epsilon-invariance principle, into five structurally
 *   distinct constraints: the absolutist_reading (flat near-categorical
 *   protection, negligible extraction, few victims), the marketplace_reading
 *   (truth-discovery service, low-moderate extraction), the
 *   harm_threshold_reading (fully conditional protection, moderate extraction
 *   with a listener-side victim set), the dignity_reading
 *   (subordination-conditioned protection, high extraction from the
 *   standpoint of targeted groups), and this reading (moderate extraction
 *   concentrated on the periphery). They are separate stories linked through
 *   network.affects_constraints; no averaging across readings occurs here.
 *   The claim/metrics gap is deliberate: the reading presents itself as the
 *   coordination structure that makes self-governance possible, while the
 *   authored metrics describe a moderately extractive, actively enforced
 *   hierarchy — the engine measures that divergence.
 *
 * KEY AGENTS:
 *   - - constitutional_courts: agenda-setting administrator (institutional/constrained) — draws and maintains the political/non-political line through accumulated doctrine
 *   - - legislative_regulatory_bodies: co-administrator (institutional/constrained) — writes and enforces the periphery rules the core's fence makes safe to legislate
 *   - - civic_press_and_political_journalists: primary beneficiary (organized/constrained) — publishes under the strongest shelter in the arrangement
 *   - - opposition_parties_and_activists: primary beneficiary (organized/constrained) — existence depends on the protected category staying broad
 *   - - ordinary_citizen_audience: beneficiary with a diffuse cost share (organized/constrained) — receives the protected information flow, loses the restricted periphery
 *   - - incumbent_officeholders: cost-bearing target with an offsetting legitimacy return (powerful/constrained)
 *   - - nonpolitical_expression_communities: primary cost-bearing target (moderate/constrained) — artists, performers, intimate and hobbyist expression
 *   - - commercial_speech_interests: cost-bearing target (moderate/constrained) — weakest protection tier in the arrangement
 *   - - marginalized_identity_speakers: cost-bearing target deprioritized by the sorting itself (powerless/trapped)
 *   - - emergent_genre_creators: excluded seat — producers of forms the doctrinal categories predate (moderate/mobile)
 *   - - comparative_constitutional_scholars: analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__democratic_participation_reading, 0.46).
domain_priors:suppression_score(speech_protection_kernel__democratic_participation_reading, 0.58).
domain_priors:theater_ratio(speech_protection_kernel__democratic_participation_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__democratic_participation_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__democratic_participation_reading, "Democratic-Participation Hierarchy of Speech Protection").
narrative_ontology:topic_domain(speech_protection_kernel__democratic_participation_reading, "constitutional_law/political_philosophy/communication_rights").

domain_priors:requires_active_enforcement(speech_protection_kernel__democratic_participation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__democratic_participation_reading, '425d90c7-fadf-4e59-919c-2a088cb2b3cf').
narrative_ontology:cs_kernel_codification('425d90c7-fadf-4e59-919c-2a088cb2b3cf', fixed_text).
narrative_ontology:cs_authority_grounding('425d90c7-fadf-4e59-919c-2a088cb2b3cf', lineage).
narrative_ontology:cs_interpretation_layer_present('425d90c7-fadf-4e59-919c-2a088cb2b3cf').
narrative_ontology:cs_reading_relation('425d90c7-fadf-4e59-919c-2a088cb2b3cf', speech_protection_kernel__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('425d90c7-fadf-4e59-919c-2a088cb2b3cf', speech_protection_kernel__harm_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('425d90c7-fadf-4e59-919c-2a088cb2b3cf', speech_protection_kernel__marketplace_reading, influences).
narrative_ontology:cs_reading_relation('425d90c7-fadf-4e59-919c-2a088cb2b3cf', speech_protection_kernel__dignity_reading, coexists_with).
narrative_ontology:cs_axiom('425d90c7-fadf-4e59-919c-2a088cb2b3cf', foundational, political_expression_precondition_of_self_governance).
narrative_ontology:cs_axiom_status(political_expression_precondition_of_self_governance, holdable).
narrative_ontology:cs_axiom_grounding('425d90c7-fadf-4e59-919c-2a088cb2b3cf', political_expression_precondition_of_self_governance, deontological).
narrative_ontology:cs_axiom('425d90c7-fadf-4e59-919c-2a088cb2b3cf', secondary, protection_tracks_governance_relevance).
narrative_ontology:cs_axiom_status(protection_tracks_governance_relevance, holdable).
narrative_ontology:cs_axiom_grounding('425d90c7-fadf-4e59-919c-2a088cb2b3cf', protection_tracks_governance_relevance, instrumental).
narrative_ontology:cs_reference_frame('425d90c7-fadf-4e59-919c-2a088cb2b3cf', guaranteed_civic_forum).
narrative_ontology:cs_drift_state('425d90c7-fadf-4e59-919c-2a088cb2b3cf', contemporary_platform_mediated_practice, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('425d90c7-fadf-4e59-919c-2a088cb2b3cf', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, civic_press_and_political_journalists).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, opposition_parties_and_activists).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, ordinary_citizen_audience).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, incumbent_officeholders).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, nonpolitical_expression_communities).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, commercial_speech_interests).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, marginalized_identity_speakers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, incumbent_officeholders).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, ordinary_citizen_audience).
narrative_ontology:constraint_vindicates(speech_protection_kernel__democratic_participation_reading, meiklejohnian_civic_function_thesis).
narrative_ontology:constraint_vindicates(speech_protection_kernel__democratic_participation_reading, deliberative_democracy_information_precondition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decide which expressions fall inside the protected category and which fall outside it, through doctrine accumulated over generations of cases. Every restriction statute, every challenged publication, and every new expressive genre eventually arrives before them for sorting. Precedent binds their future decisions to past lines, and they cannot resign the sorting function without dissolving their own authority over the speech domain.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, constitutional_courts, agenda_setter,
    institutional, generational, constrained, national).

% Write and enforce the rules governing expression outside the protected core: obscenity statutes, advertising standards, broadcast licensing, assembly permitting. They legislate freely at the edge precisely because the center is fenced off, and their enforcement staffs process the restrictions the courts decline to reach.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, legislative_regulatory_bodies, agenda_setter,
    institutional, generational, constrained, national).

% Publish accountability reporting, opposition commentary, and coverage of official conduct under the strongest legal shelter the arrangement offers; investigations that once drew sedition charges now proceed with little prior-restraint risk. Their dependence on the protected category is existential — a narrowing of what counts as political reporting would expose them directly.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, civic_press_and_political_journalists, beneficiary,
    organized, biographical, constrained, national).

% Organize, campaign, and criticize incumbents relying on the guarantee that governance-directed speech cannot be suppressed. Minority parties gain the most: without the guarantee, incumbent control of state machinery would silence them. Their access to ballots, rallies, and airtime rides on the category staying broad.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, opposition_parties_and_activists, beneficiary,
    organized, biographical, constrained, national).

% Receive the information flow that voting presupposes — debates, investigations, dissent — and supply the electoral legitimacy that makes the protection politically durable. They also consume the restricted periphery's output and absorb its absence: banned genres, priced-out advertising speech, and sanitized cultural space are losses they register dimly and vote on rarely.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, ordinary_citizen_audience, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_kernel__democratic_participation_reading, ordinary_citizen_audience, payer).

% Govern under a guarantee that criticism, satire, and opposition organizing cannot be legally silenced, absorbing hostile coverage and protest as a standing cost of office. The same arrangement returns legitimacy to them: elections conducted over an open field of argument confer a mandate censored politics would not, and the fenced core lets them regulate nuisances at the edge without appearing to attack speech itself.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, incumbent_officeholders, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_kernel__democratic_participation_reading, incumbent_officeholders, beneficiary).

% Make art, performance, comedy, sexual expression, and hobbyist publishing that the sorting places outside the protected category. Their work faces obscenity prosecutions, venue closures, platform demotion, and funding bars that political publications never meet. Some reframe their work as commentary to cross the line; reframing is unreliable and changes the work.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, nonpolitical_expression_communities, payer,
    moderate, biographical, constrained, national).

% Advertise, market, and inform customers under the weakest protection the arrangement offers. Advertising claims face substantiation regimes, product bans, and mandatory disclosures that would be unconstitutional intrusions if applied to editorial content. Trade associations lobby case by case for upgraded protection and occasionally win it.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, commercial_speech_interests, payer,
    moderate, biographical, constrained, national).

% Speak about their own conditions — discrimination, family life, community conflict — in registers the sorting tends to file as private or merely personal rather than political. Speech about subordination from inside the group has repeatedly been restricted or denied shelter while identical subject matter from established commentators counts as public affairs. Their recourse is slow movement-building to force reclassification; they cannot exit their own voice.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, marginalized_identity_speakers, payer,
    powerless, biographical, trapped, national).

% Produce forms the doctrinal categories predate — short-form video, games, memes, interactive fiction. No case law yet sorts their genres; they learn where the lines are only when enforcement arrives. They organize no litigation and hold no seat in the precedent conversations that will eventually classify them.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, emergent_genre_creators, excluded,
    moderate, immediate, mobile, global).

% Compare how different orders sort expression, trace which classifications survive contact with politics, and document the gap between the civic-forum ideal and operating practice. They publish the critiques litigants later weaponize but decide nothing themselves.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_kernel__democratic_participation_reading, constitutional_courts).
narrative_ontology:fixing_cost_class(speech_protection_kernel__democratic_participation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Guarantees that the information environment self-governance presupposes survives pressure: criticism of officials, opposition organizing, and accountability reporting are secured against suppression, solving the collective-action problem in which no single speaker can defend the public sphere alone.
% TRANSFER_FUNCTION: Moves constitutional protection — judicial solicitude, litigation priority, immunity from restriction — toward expression sorted as governance-relevant, and moves restriction discretion toward everything sorted peripheral: from nonpolitical speakers, commercial speakers, and unclassified genres toward legislatures, regulators, and the courts that administer the line.
% ABSENT_VOICES: Emergent-genre creators and the audiences of restricted expression have no seat in the precedent conversations that draw the line; they encounter the boundary only when enforcement arrives. Scholars outside the courts voice the objection that the political/private divide encodes the priorities of the already-included, but that objection enters doctrine slowly and through the very institution it criticizes.
% DISAPPEARANCE_RATIONALE: Overnight removal would not restore a pre-existing silence: periphery statutes would face categorical scrutiny and most would fall or be rewritten, political reporting and opposition organizing would lose their settled shelter and re-litigate from zero, and the courts would lose the sorting jurisdiction that organizes their entire speech docket. The expressive order would reorganize around whichever sibling reading the surviving institutions adopted — the world rearranges rather than reverting.
% FOUNDING_PROBLEM: State suppression of governance-critical speech: governments silencing opposition, criticism, and accountability information, thereby disabling the population's ability to govern itself.
% FOUNDING_PROBLEM_CORROBORATION: Press-freedom monitoring organizations, the historical record of sedition and prior-restraint regimes documented by legal historians, and comparative scholars studying autocratic backsliding all attest from seats outside the beneficiary set that the founding problem remains live. No party outside the state's own enforcement organs attests that the periphery restrictions remain necessary; that half of the genealogy is attested only by the institutions that enforce it.
narrative_ontology:disappearance_verdict(speech_protection_kernel__democratic_participation_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__democratic_participation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__democratic_participation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_protection_kernel__democratic_participation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__democratic_participation_reading, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__democratic_participation_reading_tests).
:- end_tests(speech_protection_kernel__democratic_participation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.46 at interval end) because the arrangement secures a vast domain nearly absolutely while imposing real, bounded restriction exposure on the periphery; the burden is asymmetric but the core's subsidy is broad. Suppression (0.58) reflects the machinery the hierarchy requires in both directions — courts striking down suppression of the core, statutes and enforcement staffing the periphery — and is authored as a raw structural property, unscaled by power or scope; only extractiveness is scaled, modestly upward here by national scope. Theater is low-moderate (0.25): the protection does real work, but ceremonial rights-talk has grown alongside a quietly narrowing periphery. Accessibility collapse is moderate-low (0.4): reframing expression as political, migrating between jurisdictions, and adopting sibling readings remain partly open alternatives. Resistance (0.55) is sustained scholarly and litigious contestation of the sorting itself. The measurement series run on one shared time grid (1964-2026, six points, every tracked metric at every point); trajectories are mildly monotone with no oscillation, so no intermittent-reinforcement mechanism is implicated, and the end-state values match the base_properties scalars. Suppression_requirement is tracked because the story specifically traces enforcement-intensity change: the boundary-policing workload intensified as expressive volume exploded and classification disputes multiplied. Suppression is predominantly structural (precedent, statutes, enforcement) with an internalized component — periphery speakers pre-concede unworthiness of protection — a split carried by the suppression_mechanism_split omega. The administering courts show institutional identity fusion with guardianship of the core (judicial_guardianship_identity_fusion omega); if that frame broke, internal revision would become available.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the press and opposition seats the arrangement computes as the coordination structure that makes their work survivable; from the periphery seats the same sorting computes as a queue in which they stand behind every governance-relevant claimant; from the courts' seat it computes as stewardship of a civic inheritance. Inter-institutionally, courts and legislatures experience one arrangement oppositely: courts defend the core their authority rests on, while legislatures exploit the periphery the fence makes safely regulable. Laterally, the press and nonpolitical creators hold similar nominal standing as speakers, yet the classification alone differentiates their exit options and protection — power diverges despite equal formal standing. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (press, opposition, citizen audience) derive low directionality — the arrangement subsidizes them, damping effective extraction toward or below zero. Declared victims derive high directionality: nonpolitical and commercial speakers bear restriction exposure with constrained exit; marginalized identity speakers bear it with effectively no exit, sitting nearest the full-target end; incumbent officeholders bear forced tolerance of criticism, partially offset by the legitimacy return the open field confers, netting them toward the target end. Administrators (courts, legislatures) derive mid-range directionality; the courts' classification jurisdiction is recorded on the receipt surface rather than inflated into their directionality. National spatial scope scales effective extraction modestly upward for the targets. No directionality overrides were needed: the beneficiary/victim declarations plus exit options produce the correct relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — state suppression of governance-critical speech — remains live, and overnight disappearance would rearrange the expressive order rather than leave it unchanged; founding_problem_status and disappearance_verdict agree, so no zombie or capture flag is warranted. The tangled_rope claim is what keeps both faces visible: the reading describes itself as pure coordination (protecting self-governance), and a rope classification would launder the periphery's asymmetric burden behind that self-description; sibling critiques (harm, dignity) read the whole arrangement as extraction, and a snare classification would discard the load-bearing core that press freedom and opposition survival demonstrably depend on. Mandatrophy is not resolved: the mandate has not outlived its function, and the classification's job is to prevent either face from erasing the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_delta_map,
    'This constraint is the democratic_participation_reading of speech_protection_kernel; which structural elements would each sibling reading replace, and how would epsilon move under each?',
    'Track doctrinal adoption events: a supreme court or constitutional convention embracing a sibling''s axiom (near-categorical protection, demonstrable-harm conditions, truth-discovery tests, group-subordination tests) and re-sorting the protected category.',
    'Absolutist adoption flattens the hierarchy and collapses most periphery restrictions (epsilon falls toward coordination-floor levels); harm_threshold adoption makes protection fully conditional on demonstrated absence of harm (the victim set migrates to harmed listeners and readers); marketplace adoption replaces the political/non-political axis with truth-contestability (protection follows falsifiable claim-making rather than subject matter); dignity adoption adds group-subordination grounds for restriction (the victim set expands to targeted groups while the protected core narrows).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_delta_map, conceptual, 'Committer structure: this story is one reading of a five-reading kernel; records what each sibling would structurally change.').

omega_variable(
    political_boundary_drawing_ambiguity,
    'Where does the political/non-political boundary sit, and who controls its movement?',
    'Accumulated classification cases: track which expressions courts admit to the protected category across successive decades, and which challengers succeed in reframing periphery expression as governance-relevant.',
    'A widening boundary lowers measured extraction (fewer speakers left in the restrictable periphery); a narrowing boundary raises it and pushes the arrangement toward snare-like operation for the excluded genres and speakers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_boundary_drawing_ambiguity, conceptual, 'The hierarchy''s epsilon depends on a contestable classification boundary.').

omega_variable(
    periphery_restriction_necessity,
    'Are the periphery restrictions (obscenity statutes, advertising regulation, assembly permitting) genuine trade-offs the protection of the core requires, or burdens collected under the hierarchy''s cover?',
    'Comparative natural experiments: jurisdictions applying near-categorical protection to the same domains, observing whether self-governance outcomes degrade when the periphery is flattened.',
    'If governance outcomes hold under flat protection, the periphery is extraction riding on the core''s coordination function; if they degrade, part of the measured extraction is the price of the core itself and the tangled_rope reading strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(periphery_restriction_necessity, empirical, 'Whether the asymmetric burden is separable from the coordination function.').

omega_variable(
    judicial_guardianship_identity_fusion,
    'Has the judiciary''s institutional identity fused with guardianship of the protected core such that it can no longer evaluate the sorting function critically?',
    'Observe whether courts ever concede the classification project is incoherent (declining to sort, deferring to flat rules) versus always re-deriving the hierarchy in new cases.',
    'If fused, boundary corrections arrive only through external shocks (amendment, treaty override, generational turnover); if not, internal doctrinal revision remains available and the arrangement can shed periphery extraction without regime change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_guardianship_identity_fusion, conceptual, 'Institutional identity lock on the administering seat.').

omega_variable(
    suppression_mechanism_split,
    'Is the arrangement''s suppression structural (precedent, statutes, enforcement staffing) or internalized (periphery speakers pre-conceding that their expression is not worth protecting)?',
    'Post-repeal expression trajectories: if restricted genres flourish once statutes lapse, suppression was structural; if output stays muted after repeal, internalization carries it.',
    'Internalized suppression raises effective suppression above the structural measure and persists after statutory repeal; structural suppression falls with repeal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_split, empirical, 'Structural versus internalized suppression mechanism split.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__democratic_participation_reading, 1964, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t1964, speech_protection_kernel__democratic_participation_reading, theater_ratio, 1964, 0.16).
narrative_ontology:measurement_basis(spee_tr_t1964, observed).
narrative_ontology:measurement(spee_tr_t1976, speech_protection_kernel__democratic_participation_reading, theater_ratio, 1976, 0.18).
narrative_ontology:measurement_basis(spee_tr_t1976, observed).
narrative_ontology:measurement(spee_tr_t1988, speech_protection_kernel__democratic_participation_reading, theater_ratio, 1988, 0.2).
narrative_ontology:measurement_basis(spee_tr_t1988, observed).
narrative_ontology:measurement(spee_tr_t2000, speech_protection_kernel__democratic_participation_reading, theater_ratio, 2000, 0.22).
narrative_ontology:measurement_basis(spee_tr_t2000, observed).
narrative_ontology:measurement(spee_tr_t2012, speech_protection_kernel__democratic_participation_reading, theater_ratio, 2012, 0.24).
narrative_ontology:measurement_basis(spee_tr_t2012, observed).
narrative_ontology:measurement(spee_tr_t2026, speech_protection_kernel__democratic_participation_reading, theater_ratio, 2026, 0.25).
narrative_ontology:measurement_basis(spee_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(spee_be_t1964, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 1964, 0.38).
narrative_ontology:measurement_basis(spee_be_t1964, observed).
narrative_ontology:measurement(spee_be_t1976, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 1976, 0.4).
narrative_ontology:measurement_basis(spee_be_t1976, observed).
narrative_ontology:measurement(spee_be_t1988, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 1988, 0.42).
narrative_ontology:measurement_basis(spee_be_t1988, observed).
narrative_ontology:measurement(spee_be_t2000, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 2000, 0.44).
narrative_ontology:measurement_basis(spee_be_t2000, observed).
narrative_ontology:measurement(spee_be_t2012, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 2012, 0.45).
narrative_ontology:measurement_basis(spee_be_t2012, observed).
narrative_ontology:measurement(spee_be_t2026, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 2026, 0.46).
narrative_ontology:measurement_basis(spee_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t1964, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 1964, 0.48).
narrative_ontology:measurement_basis(spee_su_t1964, observed).
narrative_ontology:measurement(spee_su_t1976, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 1976, 0.51).
narrative_ontology:measurement_basis(spee_su_t1976, observed).
narrative_ontology:measurement(spee_su_t1988, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 1988, 0.53).
narrative_ontology:measurement_basis(spee_su_t1988, observed).
narrative_ontology:measurement(spee_su_t2000, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement_basis(spee_su_t2000, observed).
narrative_ontology:measurement(spee_su_t2012, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 2012, 0.57).
narrative_ontology:measurement_basis(spee_su_t2012, observed).
narrative_ontology:measurement(spee_su_t2026, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 2026, 0.58).
narrative_ontology:measurement_basis(spee_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__democratic_participation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, dignity_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'free speech protection' decomposes into five structurally distinct constraints (one per kernel reading), each with its own epsilon, beneficiary/victim structure, and classification. Historical flow runs upstream from the marketplace and absolutist readings (which informed the mid-century democratic turn) into this reading; this reading exerts downstream structural pressure on the marketplace sibling by displacing truth-discovery justifications in operative doctrine. Epsilon differences across the family: absolutist negligible (flat protection, minimal victim set), marketplace low-moderate, harm_threshold moderate (listener-side victims), dignity high from the targeted-group standpoint, democratic_participation moderate with the burden concentrated on the periphery. All five files link one another through affects_constraints; no story averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
