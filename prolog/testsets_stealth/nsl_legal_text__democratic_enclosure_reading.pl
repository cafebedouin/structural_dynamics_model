% ============================================================================
% CONSTRAINT STORY: nsl_legal_text__democratic_enclosure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nsl_legal_text__democratic_enclosure_reading, []).

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
 *   constraint_id: nsl_legal_text__democratic_enclosure_reading
 *   human_readable: National Security Law (Hong Kong) — Democratic Enclosure Reading
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the National Security Law kernel:
 *   the democratic_enclosure_reading, under which the law operates as a
 *   mechanism for permanent closure of democratic space and criminalization
 *   of dissent. The referent of epsilon is the standing arrangement — the NSL
 *   as enacted and operated since June 2020 — assessed by this reading's own
 *   lights; it is NOT the arrangement this reading would prefer. Under that
 *   referent, the reading sees the security framing as cover for the
 *   suppression of an entire democratic infrastructure: opposition parties
 *   disqualified and prosecuted, the largest independent newspaper destroyed,
 *   unions and civil society organizations dissolved under officer liability,
 *   and expression criminalized down to slogans and social media posts.
 *   Beneficiaries are the central authorities that authored and impose the
 *   text, the local establishment that executes it, and the loyalist elite
 *   strata that collect the rents of eliminated competition. Victims are the
 *   political opposition, the press, civil society, and ordinary
 *   participants. Sibling readings of the same text
 *   (sovereignty_restoration_reading, jurisdictional_capture_reading) are
 *   separate constraint files with their own epsilon values and stakeholder
 *   structures; they are not averaged into this one.
 *
 * KEY AGENTS:
 *   - - beijing_central_authorities: Primary agenda-setter and principal beneficiary (institutional/arbitrage) — authors, interprets, and is insulated from the arrangement it imposes
 *   - - hk_government_establishment: Secondary agenda-setter and partial beneficiary (institutional/constrained) — operates enforcement locally while absorbing autonomy and reputation costs
 *   - - pro_establishment_elites: Beneficiary (powerful/mobile) — collects offices and market calm yielded by eliminated electoral competition
 *   - - democratic_opposition_politicians: Primary target (powerless/trapped) — disqualified, prosecuted, imprisoned; exit means abandoning constituents
 *   - - independent_press_outlets: Primary target (moderate/trapped) — closed by asset freeze or self-censoring under editor liability
 *   - - civil_society_organizations: Primary target (organized/trapped) — dissolved under personal officer liability
 *   - - protest_movement_participants: Primary target (powerless/trapped) — expression criminalized, assembly channel removed
 *   - - hk_nsl_designated_judges: Administering intermediary (institutional/constrained) — staffs security dockets under subordinate interpretation
 *   - - foreign_democracies: Excluded critic (powerful/arbitrage) — sanctions and treaty suspensions applied from outside
 *   - - hongkong_diaspora_activists: Excluded continuator (organized/mobile) — advocacy from exile under warrant and bounty
 *   - - international_human_rights_monitors: Analytical observer (institutional/analytical) — compiles the record with no enforcement lever
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__democratic_enclosure_reading, 0.87).
domain_priors:suppression_score(nsl_legal_text__democratic_enclosure_reading, 0.9).
domain_priors:theater_ratio(nsl_legal_text__democratic_enclosure_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, extractiveness, 0.87).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__democratic_enclosure_reading, snare).
narrative_ontology:human_readable(nsl_legal_text__democratic_enclosure_reading, "National Security Law (Hong Kong) — Democratic Enclosure Reading").
narrative_ontology:topic_domain(nsl_legal_text__democratic_enclosure_reading, "constitutional/political").

domain_priors:requires_active_enforcement(nsl_legal_text__democratic_enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__democratic_enclosure_reading, 'a10a5997-a9c2-4eb1-a8b4-a5c48ad53e3d').
narrative_ontology:cs_kernel_codification('a10a5997-a9c2-4eb1-a8b4-a5c48ad53e3d', fixed_text).
narrative_ontology:cs_authority_grounding('a10a5997-a9c2-4eb1-a8b4-a5c48ad53e3d', extraction).
narrative_ontology:cs_interpretation_layer_present('a10a5997-a9c2-4eb1-a8b4-a5c48ad53e3d').
narrative_ontology:cs_reading_relation('a10a5997-a9c2-4eb1-a8b4-a5c48ad53e3d', nsl_legal_text__sovereignty_restoration_reading, coexists_with).
narrative_ontology:cs_reading_relation('a10a5997-a9c2-4eb1-a8b4-a5c48ad53e3d', nsl_legal_text__jurisdictional_capture_reading, influences).
narrative_ontology:cs_axiom('a10a5997-a9c2-4eb1-a8b4-a5c48ad53e3d', foundational, democratic_pluralism_nonnegotiable).
narrative_ontology:cs_axiom_status(democratic_pluralism_nonnegotiable, holdable).
narrative_ontology:cs_axiom_grounding('a10a5997-a9c2-4eb1-a8b4-a5c48ad53e3d', democratic_pluralism_nonnegotiable, deontological).
narrative_ontology:cs_axiom('a10a5997-a9c2-4eb1-a8b4-a5c48ad53e3d', foundational, nsl_prosecutions_target_dissent_not_violence).
narrative_ontology:cs_axiom_status(nsl_prosecutions_target_dissent_not_violence, holdable).
narrative_ontology:cs_axiom_grounding('a10a5997-a9c2-4eb1-a8b4-a5c48ad53e3d', nsl_prosecutions_target_dissent_not_violence, empirically_contingent).
narrative_ontology:cs_reference_frame('a10a5997-a9c2-4eb1-a8b4-a5c48ad53e3d', common_law_liberal_autonomy_baseline).
narrative_ontology:cs_drift_state('a10a5997-a9c2-4eb1-a8b4-a5c48ad53e3d', post_enforcement_consolidation, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('a10a5997-a9c2-4eb1-a8b4-a5c48ad53e3d', '').
narrative_ontology:cs_kernel_id(nsl_legal_text__democratic_enclosure_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__democratic_enclosure_reading, beijing_central_authorities).
narrative_ontology:constraint_beneficiary(nsl_legal_text__democratic_enclosure_reading, hk_government_establishment).
narrative_ontology:constraint_beneficiary(nsl_legal_text__democratic_enclosure_reading, pro_establishment_elites).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, democratic_opposition_politicians).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, independent_press_outlets).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, civil_society_organizations).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, protest_movement_participants).
narrative_ontology:constraint_vindicates(nsl_legal_text__democratic_enclosure_reading, comprehensive_national_security_concept).
narrative_ontology:constraint_vindicates(nsl_legal_text__democratic_enclosure_reading, patriots_administering_hong_kong).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted the law in closed session and imposed it by annexing it to the Basic Law, bypassing Hong Kong's legislature entirely. Holds sole interpretation power through the Standing Committee, decides which cases move to mainland jurisdiction, and chairs the new security committee. It is not subject to the law's constraints in any forum it does not itself control; its exposure to the arrangement runs through international reputation and sanctions, not domestic enforcement.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, beijing_central_authorities, agenda_setter,
    institutional, generational, arbitrage, global).

% Operates the law day to day: the Security Bureau runs a dedicated police unit, the Department of Justice brings prosecutions, and the Chief Executive designates which judges may hear security cases and disqualifies legislators under oath vetting. Officeholders retain their positions and patronage flows conditioned on demonstrating enforcement, while absorbing costs they do not control: erosion of judicial-independence reputation, professional emigration, and sanctions directed at named officials.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, hk_government_establishment, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__democratic_enclosure_reading, hk_government_establishment, beneficiary).

% Loyalist legislators, business figures, and district leaders who collect the offices, contracts, and market calm that follow from the removal of electoral competition and protest disruption. Many hold foreign residency or assets abroad, so physical exit is available, but taking it would forfeit the positions and access the arrangement secures.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, pro_establishment_elites, beneficiary,
    powerful, biographical, mobile, national).

% Former legislators, district councillors, and party organizers disqualified under oath requirements, prosecuted under subversion and collusion charges, or held in pretrial detention for years. Remaining means possible imprisonment; leaving means abandoning constituents and organizations built over decades. Most who faced charges stayed and stood trial rather than flee.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, democratic_opposition_politicians, payer,
    powerless, biographical, trapped, national).

% Newsrooms operating under licensing dependence, advertiser flight, and personal criminal liability of editors and publishers for printed content. The largest pro-democracy newspaper was closed by asset freeze and its executives charged under collusion provisions; surviving outlets avoid the topics that previously defined their coverage. Exit means ceasing to report on Hong Kong from inside Hong Kong.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, independent_press_outlets, payer,
    moderate, biographical, trapped, regional).

% Trade unions, professional associations, and advocacy groups that once mobilized members and issued public statements. Facing personal liability for officers under the law's organization provisions, most large pro-democracy organizations voted to dissolve themselves; registration, banking, and venue dependence make continued operation conditional on political quiet.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, civil_society_organizations, payer,
    organized, biographical, trapped, local).

% Ordinary residents who attended marches, chanted slogans, posted online, or donated to protest bail funds now classified as collusion with foreign forces. Arrests have reached teenagers and retirees; a slogan on a placard or a social media post can constitute evidence. Their historical leverage was mass numbers, which the assembly-authorization regime removes.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, protest_movement_participants, payer,
    powerless, immediate, trapped, local).

% Judices selected by the Chief Executive to hear national security cases, sitting without juries. They retain common-law craft in procedure but operate under an interpretation clause subordinating their readings to Standing Committee interpretations, and several overseas judges have resigned from the Court of Final Appeal rather than serve in this configuration. Individual resignation remains available; collectively the bench continues staffing the dockets.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, hk_nsl_designated_judges, agenda_setter,
    institutional, biographical, constrained, national).

% Governments that responded with sanctions on officials, suspension of extradition treaties, and expanded visa schemes for Hong Kongers. They object from entirely outside the arrangement and hold no vote in its operation; their instruments are reputational and economic, applied at a distance, and the law's extraterritorial provisions reach their territories' residents rather than vice versa.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, foreign_democracies, excluded,
    powerful, generational, arbitrage, continental).

% Exiled former legislators, journalists, and organizers continuing advocacy from London, Taipei, Toronto, and elsewhere. They publish, brief foreign parliaments, and operate under outstanding arrest warrants and announced bounties that make return impossible. Their physical exclusion from the territory is, on this reading, the arrangement functioning as intended.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, hongkong_diaspora_activists, excluded,
    organized, generational, mobile, global).

% UN treaty bodies, special rapporteurs, bar associations, and academic observers documenting prosecution patterns, trial fairness, and detention conditions. They compile the evidentiary record that the enforcing seats dispute and the targeted seats supply testimony to, and they possess no enforcement lever inside the jurisdiction.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, international_human_rights_monitors, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nsl_legal_text__democratic_enclosure_reading, beijing_central_authorities).
narrative_ontology:fixing_cost_class(nsl_legal_text__democratic_enclosure_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Consolidates national-security jurisdiction over Hong Kong into a single framework: four defined offense categories, a dedicated police and prosecution apparatus, designated judges, and a channel to mainland jurisdiction for specified cases — replacing the pre-2020 condition in which no national security legislation operated and Article 23 implementation had lapsed.
% TRANSFER_FUNCTION: Moves political liberty, organizational capacity, and personal freedom from civil society, press, and opposition politics to the central state; moves prosecutorial discretion and case-removal authority upward from local courts to centrally controlled bodies; moves legislative seats from contested election to a vetted candidate pool.
% ABSENT_VOICES: Hong Kong's electorate — which had returned pan-democratic candidates in every openly contested election — was never consulted; the draft was prepared behind closed doors and imposed by annexation without local legislative passage. Detained defendants, shuttered newsrooms, and dissolved societies had no seat in the drafting. Signatories and parties to the Joint Declaration were notified of a completed text, not negotiated with.
% DISAPPEARANCE_RATIONALE: If the law vanished overnight, prosecuted defendants would seek release, dissolved unions and parties would re-register, the closed newspaper's successors would resume publication, disqualified candidates would stand again, and the emigration wave would partially reverse — the entire democratic infrastructure the arrangement suppresses would begin rebuilding within months, which is precisely why its maintenance requires continuous enforcement.
% FOUNDING_PROBLEM: End the 2019 extradition-bill unrest and close the gap left by the abandoned 2003 Article 23 legislation: mass protests had persisted for months, and the central authorities judged Hong Kong's own institutions unwilling and unable either to restore order or to pass security legislation themselves.
% FOUNDING_PROBLEM_CORROBORATION: UN Human Rights Committee review records, special-procedure communications, foreign governments party to the Joint Declaration, and academic legal scholarship outside the beneficiary set attest both that large-scale street unrest had subsided before enactment and that subsequent prosecutions concentrate on speech, publishing, and organizing rather than violence. No corroborating source outside the beneficiary set attests that the founding problem remains live in its original form; the enforcing seats assert continuation by pointing to foreign 'collusion,' which the excluded and targeted seats characterize as ordinary international contact.
narrative_ontology:disappearance_verdict(nsl_legal_text__democratic_enclosure_reading, world_rearranges).
narrative_ontology:founding_problem_status(nsl_legal_text__democratic_enclosure_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nsl_legal_text__democratic_enclosure_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nsl_legal_text__democratic_enclosure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nsl_legal_text__democratic_enclosure_reading, 0.87, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nsl_legal_text__democratic_enclosure_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nsl_legal_text__democratic_enclosure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nsl_legal_text__democratic_enclosure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.87 at interval end) because the arrangement transfers the entire stock of organized political capacity — parties, press, unions, electoral competitiveness — from one side of the polity to the other, and the transfer rate increased as enforcement machinery matured. Suppression is higher still (0.90) and is authored as a raw structural property, unscaled: the arrangement's persistence depends on continuous active enforcement (dedicated police unit, prosecution office, designated judges, assembly authorization), not on participant preference. Theater is low (0.22) and rises only slightly: unlike degraded arrangements, the enforcement here is overwhelmingly functional for its purpose — arrests, trials, and dissolutions do the work the enclosure requires — with a modest performative layer (national security education campaigns, pledge ceremonies) growing as overt resistance disappears. Accessibility collapse is 0.72: domestic alternatives have largely collapsed, but exile media, international advocacy, and quiet noncompliance persist, so collapse is deep but incomplete. Resistance is 0.55: mass domestic resistance was broken within the first two years, while external resistance (sanctions, diaspora organizing, UN processes) continues without territorial leverage. Coalition potential among the multiple victim classes is deliberately foreclosed by design — the assembly-authorization regime removes exactly the mechanism (mass simultaneous presence) that previously coordinated dispersed victims, and officer-liability provisions convert each organization's survival calculus against collective action; the diaspora coalition exists but lacks jurisdictional purchase. The measurement series run on one shared time grid (months 0-72, annual points) with every tracked metric authored at every point. The trajectories are monotonic ratchets, not cycles: earlier Hong Kong contention followed a tension-release cycle (2014, 2019), and the removal of the release phase is itself the arrangement's central achievement — the absence of oscillation is the signal, documented here rather than treated as missing data.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute radically different types from identical structural data. From the central authorities' seat, the arrangement is a security framework they authored, control interpretation of, and bear no domestic cost of — coordination with negligible personal extraction. From the opposition, press, and civil society seats, the same structure operates as enforced extraction with trapped exit. The designated judges occupy an intermediate position: administering the machinery while absorbing its reputational costs. The local establishment is the hardest seat — it both collects patronage conditioned on enforcement and bears costs (sanctions, professional flight, judicial-independence erosion) it does not control, which is why its dual position is flagged in the establishment_capture_depth omega rather than resolved by a blunt override. The engine computes these divergences from the structural data; the authored snare claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: the central authorities sit nearest the full-beneficiary pole (they wrote the text, hold interpretation, and face no domestic enforcement exposure — arbitrage-grade insulation), the establishment sits low but not minimal (dual-positioned collector and bearer of costs), and loyalist elites sit low with mobile-but-forfeiting exit. Victim declarations drive targets toward the full-target pole, amplified by trapped exit: opposition figures who chose trial over exile, newsrooms that cannot report from elsewhere, organizations whose officers face personal liability, participants whose only prior leverage was assembly. Scope amplifies effective extraction for targets: the law's extraterritorial provisions extend its reach globally, making verification of compliance harder and raising the effective burden on diaspora and foreign-resident targets, while the central authorities' own exposure runs through the same global channel only as reputation. No directionality_overrides were authored: the one seat where the derivation strains (the establishment's partial capture) cannot be corrected without distorting the central authorities' derived value, because the override surface is keyed to power atom rather than agent; the ambiguity is carried by omega instead.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — ending the 2019 unrest and closing the Article 23 gap — was substantially accomplished before or shortly after enactment, yet the arrangement has intensified rather than relaxed: founding_problem_status is contested, disappearance_verdict is world_rearranges, and the status-by-verdict mismatch is exactly the capture/zombie signature the R5 consumer checks. Classifying this as snare rather than rope prevents the security-coordination cover story from laundering extraction as collective benefit; classifying it as snare rather than piton matters because the extraction is concentrated, not diffuse — a named seat (the central authorities) demonstrably receives the gains, and fixing is prohibitive because no domestic repeal mechanism exists (Standing Committee interpretation supremacy forecloses local correction) while external pressure lacks jurisdictional purchase. The absence of any sunset clause is authored as a structural fact, not an oversight: the arrangement presents itself as permanent, and the scaffold category is unavailable to it by its own terms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of kernel nsl_legal_text — the democratic_enclosure_reading. Sibling readings (sovereignty_restoration_reading, jurisdictional_capture_reading) instantiate different constraints from the same text with different victim and beneficiary sets and different epsilon values. Which reading governs a given classification?',
    'Cross-reading comparison across the linked family files: the same statutory text yields a snare-shaped structure under this reading, a coordination/restoration structure under the sovereignty reading, and a hybrid legal-transplant structure under the jurisdictional capture reading. Disagreement is located in the victim set (who counts as protected vs. suppressed) and in the referent of the security interest.',
    'Classification is reading-indexed, not topic-indexed: a corpus consumer averaging across readings would fabricate a middle epsilon that corresponds to no party''s actual position. Any verdict about ''the NSL'' must name the reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: one kernel, three readings, classification valid only per reading.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression structural (liability, licensing, detention) or internalized (self-censorship that persists independent of enforcement probability)?',
    'Post-exit trajectory test: survey editorial and organizational behavior among actors who have physically exited the jurisdiction. If avoidance of sensitive topics persists among exiled press and civil society at rates above baseline caution, a substantial share of suppression is internalized and would survive formal repeal.',
    'If internalized, effective suppression exceeds the structural measure — repeal alone would not restore the democratic infrastructure, and the constraint''s shadow would outlast its text.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural vs. internalized share of the chilling effect.').

omega_variable(
    security_function_separability,
    'Does the law contain a genuine counterterrorism or espionage function structurally separable from the criminalization of dissent, such that a narrower statute could deliver it?',
    'Compare prosecution distribution across the four offense categories: if terrorism and sabotage prosecutions are a negligible fraction while subversion and collusion prosecutions of speech and organization dominate, the security frame functions as cover and the separable core is empty in practice.',
    'If separable, the arrangement is pure extraction riding a nominal security label; if inseparable, a small fraction of measured extraction is the price of any security framework, and the sovereignty_restoration_reading gains footing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(security_function_separability, conceptual, 'Whether coordination content and extraction content are structurally separable.').

omega_variable(
    permanence_of_enclosure,
    'Is the closure of democratic space a permanent structural feature or contingent on the central authorities'' threat assessment and leadership generation?',
    'Observe enforcement intensity across a sustained period of civic quiet and across a leadership transition: decriminalization of low-level expression or release of long-detained defendants under quiet conditions would indicate contingency; unchanged prosecution rates would indicate structural lock-in via the patriots-vetting apparatus.',
    'If contingent, the constraint resembles an enforced equilibrium that could relax; if locked, the vetting machinery itself reproduces the enclosure regardless of threat level, and the snare classification hardens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(permanence_of_enclosure, empirical, 'Contingent equilibrium vs. structurally locked enclosure.').

omega_variable(
    establishment_capture_depth,
    'How far is the Hong Kong government a principal with independent preferences versus an agent executing central directives — i.e., does it retain any latent preference for moderation that enforcement costs could activate?',
    'Track divergence between centrally signaled policy and local implementation (charging thresholds, sentencing submissions, designation choices) across the interval; systematic local leniency would indicate residual principal-hood.',
    'The directionality derivation reads the establishment as a beneficiary (low d) because it collects office and patronage; if it is substantially a captured agent also bearing autonomy and talent-flight costs, its true d sits higher, and the extraction it mediates should be attributed upward rather than shared. No per-seat override was authored because the available override surface is keyed to power atom and would simultaneously distort the central authorities'' derived near-full-beneficiary value; this omega carries the residual ambiguity instead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(establishment_capture_depth, empirical, 'Principal vs. captured-agent status of the local enforcing seat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__democratic_enclosure_reading, 0, 72).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl_enclosure_rd_tr_t0, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(nsl_enclosure_rd_tr_t12, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 12, 0.14).
narrative_ontology:measurement(nsl_enclosure_rd_tr_t24, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 24, 0.16).
narrative_ontology:measurement(nsl_enclosure_rd_tr_t36, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 36, 0.18).
narrative_ontology:measurement(nsl_enclosure_rd_tr_t48, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 48, 0.19).
narrative_ontology:measurement(nsl_enclosure_rd_tr_t60, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 60, 0.21).
narrative_ontology:measurement(nsl_enclosure_rd_tr_t72, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 72, 0.22).

% Extraction over time
narrative_ontology:measurement(nsl_enclosure_rd_be_t0, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(nsl_enclosure_rd_be_t12, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 12, 0.65).
narrative_ontology:measurement(nsl_enclosure_rd_be_t24, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 24, 0.71).
narrative_ontology:measurement(nsl_enclosure_rd_be_t36, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 36, 0.77).
narrative_ontology:measurement(nsl_enclosure_rd_be_t48, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 48, 0.82).
narrative_ontology:measurement(nsl_enclosure_rd_be_t60, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 60, 0.85).
narrative_ontology:measurement(nsl_enclosure_rd_be_t72, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 72, 0.87).

% Suppression requirement over time
narrative_ontology:measurement(nsl_enclosure_rd_su_t0, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(nsl_enclosure_rd_su_t12, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 12, 0.7).
narrative_ontology:measurement(nsl_enclosure_rd_su_t24, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 24, 0.76).
narrative_ontology:measurement(nsl_enclosure_rd_su_t36, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 36, 0.81).
narrative_ontology:measurement(nsl_enclosure_rd_su_t48, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 48, 0.85).
narrative_ontology:measurement(nsl_enclosure_rd_su_t60, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 60, 0.88).
narrative_ontology:measurement(nsl_enclosure_rd_su_t72, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 72, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_legal_text__democratic_enclosure_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nsl_legal_text__democratic_enclosure_reading, nsl_legal_text__sovereignty_restoration_reading).
narrative_ontology:affects_constraint(nsl_legal_text__democratic_enclosure_reading, nsl_legal_text__jurisdictional_capture_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the NSL' decomposes into three structurally distinct constraints — one per reading of the kernel text — per the epsilon-invariance principle. This file (democratic_enclosure_reading) carries the highest epsilon of the family because its victim set is the entire democratic infrastructure; the sovereignty_restoration_reading shares the referent text but authors a coordination-forward structure with a different victim set; the jurisdictional_capture_reading isolates the legal-transplant component centered on judicial autonomy. Upstream/downstream: the enclosure outcome (this file) supplies the political fact that the jurisdictional capture reading explains mechanistically, hence the influences edge; the sovereignty reading is held by a disjoint faction and merely coexists. Each file links the others via network.affects_constraints; no file averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
