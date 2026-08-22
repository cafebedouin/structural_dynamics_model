% ============================================================================
% CONSTRAINT STORY: nsl_legal_text__sovereignty_restoration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nsl_legal_text__sovereignty_restoration_reading, []).

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
 *   constraint_id: nsl_legal_text__sovereignty_restoration_reading
 *   human_readable: National Security Law (Hong Kong) — Sovereignty Restoration Reading
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   This story instantiates ONE reading of a contested kernel. The kernel is
 *   the National Security Law for Hong Kong, enacted by the NPC Standing
 *   Committee on 30 June 2020 following the 2019 unrest. This file generates
 *   the sovereignty_restoration_reading: the arrangement as a legitimate
 *   sovereign security instrument that restored constitutional order and
 *   public calm after eighteen months of escalating confrontation. From this
 *   seat, the arrangement solves a real coordination problem — the
 *   sovereign's security prerogatives over Hong Kong had no operational
 *   enforcement locally — while concentrating its costs on identifiable
 *   classes (protest participants, disqualified opposition politicians,
 *   independent press practitioners) whom this reading classifies as security
 *   threats rather than as wrongfully burdened parties. The structural facts
 *   are authored honestly: those classes bear the costs, and they are
 *   declared as such; the reading's valuation of that bearing is what differs
 *   from the sibling files. CONSTRAINT FAMILY: the colloquial label 'the NSL'
 *   decomposes, per the epsilon-invariance principle, into three structurally
 *   distinct readings — this file,
 *   nsl_legal_text__democratic_enclosure_reading, and
 *   nsl_legal_text__jurisdictional_capture_reading. All three take the SAME
 *   standing arrangement as their epsilon referent; the values differ because
 *   epsilon is reading-indexed (OQ-26), not because the referent varies. Each
 *   file links the other two via network.affects_constraints. KEY AGENTS (by
 *   structural relationship): - cpg_sovereign_authority: Primary beneficiary
 *   and agenda-setter (institutional/arbitrage) — drafts, solely interprets,
 *   and collects the arrangement's principal returns -
 *   hong_kong_sar_establishment: Delegated administrator and secondary
 *   beneficiary (institutional/constrained) — enforces locally within lines
 *   drawn centrally - protest_movement_participants: Primary target class
 *   (powerless/trapped) — arrests, designated-track trials, multi-year
 *   sentences, dissolved organizations - disqualified_opposition_legislators:
 *   Secondary target (powerless/identity_locked) — careers and liberty bound
 *   to a proscribed political identity - independent_press_practitioners:
 *   Secondary target (moderate/constrained) — outlet closure by raid,
 *   prosecution precedent, licensing anxiety - hong_kong_general_public:
 *   Diffuse beneficiary carrying indirect costs (moderate/mobile) — calm
 *   regained, civic space narrowed, emigration valve open -
 *   international_business_community: Beneficiary with arbitrage exit
 *   (powerful/arbitrage) — endorses predictability, can and partly does
 *   relocate - foreign_critical_governments: Excluded objectors
 *   (institutional/constrained) — sanctions and treaty suspensions from
 *   outside the arrangement - un_human_rights_monitors: Analytical observer
 *   (institutional/analytical) — treaty-body review without enforcement
 *   leverage
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__sovereignty_restoration_reading, 0.56).
domain_priors:suppression_score(nsl_legal_text__sovereignty_restoration_reading, 0.67).
domain_priors:theater_ratio(nsl_legal_text__sovereignty_restoration_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, extractiveness, 0.56).
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 0.67).
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, resistance, 0.34).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__sovereignty_restoration_reading, tangled_rope).
narrative_ontology:human_readable(nsl_legal_text__sovereignty_restoration_reading, "National Security Law (Hong Kong) — Sovereignty Restoration Reading").
narrative_ontology:topic_domain(nsl_legal_text__sovereignty_restoration_reading, "constitutional/political").

domain_priors:requires_active_enforcement(nsl_legal_text__sovereignty_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__sovereignty_restoration_reading, 'e965a095-76ed-4940-b5f7-d1782fd14fa6').
narrative_ontology:cs_kernel_codification('e965a095-76ed-4940-b5f7-d1782fd14fa6', fixed_text).
narrative_ontology:cs_authority_grounding('e965a095-76ed-4940-b5f7-d1782fd14fa6', lineage).
narrative_ontology:cs_interpretation_layer_present('e965a095-76ed-4940-b5f7-d1782fd14fa6').
narrative_ontology:cs_reading_relation('e965a095-76ed-4940-b5f7-d1782fd14fa6', nsl_legal_text__democratic_enclosure_reading, coexists_with).
narrative_ontology:cs_reading_relation('e965a095-76ed-4940-b5f7-d1782fd14fa6', nsl_legal_text__jurisdictional_capture_reading, influences).
narrative_ontology:cs_axiom('e965a095-76ed-4940-b5f7-d1782fd14fa6', foundational, sovereign_security_prerogative_supreme).
narrative_ontology:cs_axiom_status(sovereign_security_prerogative_supreme, holdable).
narrative_ontology:cs_axiom_grounding('e965a095-76ed-4940-b5f7-d1782fd14fa6', sovereign_security_prerogative_supreme, conventional).
narrative_ontology:cs_axiom('e965a095-76ed-4940-b5f7-d1782fd14fa6', foundational, order_as_liberty_precondition).
narrative_ontology:cs_axiom_status(order_as_liberty_precondition, holdable).
narrative_ontology:cs_axiom_grounding('e965a095-76ed-4940-b5f7-d1782fd14fa6', order_as_liberty_precondition, instrumental).
narrative_ontology:cs_reference_frame('e965a095-76ed-4940-b5f7-d1782fd14fa6', restored_sovereign_public_order).
narrative_ontology:cs_drift_state('e965a095-76ed-4940-b5f7-d1782fd14fa6', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e965a095-76ed-4940-b5f7-d1782fd14fa6', '').
narrative_ontology:cs_kernel_id(nsl_legal_text__sovereignty_restoration_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, cpg_sovereign_authority).
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, hong_kong_sar_establishment).
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, international_business_community).
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, hong_kong_general_public).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, protest_movement_participants).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, disqualified_opposition_legislators).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, independent_press_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, hong_kong_general_public).
narrative_ontology:constraint_vindicates(nsl_legal_text__sovereignty_restoration_reading, sovereign_national_security_indivisibility_doctrine).
narrative_ontology:constraint_vindicates(nsl_legal_text__sovereignty_restoration_reading, basiclaw_article_23_fulfillment_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted and enacted the law through the NPC Standing Committee in June 2020 after the 2019 unrest, retains sole power to interpret it, and directs application through the national security committees. Collects the arrangement's principal returns: uncontested authority over Hong Kong, elimination of the street challenge, and a demonstration of sovereign resolve legible well beyond the territory. Can amend or reinterpret the text at will; nothing external binds it to the arrangement except its own judgment.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, cpg_sovereign_authority, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__sovereignty_restoration_reading, cpg_sovereign_authority, beneficiary).

% Administers the arrangement day to day: the Chief Executive chairs the Committee for Safeguarding National Security, the police field a national security department, and designated judges hear the cases. Regained governability it had lost by 2019 — budgets pass, ordinances move, the streets are quiet. Operates within lines drawn in Beijing and cannot alter the text or its interpretation; its own credibility is staked on the arrangement's continuation.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, hong_kong_sar_establishment, beneficiary,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__sovereignty_restoration_reading, hong_kong_sar_establishment, agenda_setter).

% Headquarters regional operations, lists companies, and routes capital through Hong Kong on the expectation of predictable order, and publicly welcomed the restoration of calm after 2019. Retains full freedom to relocate listings, treasury functions, or staff to Singapore or elsewhere if predictability erodes, and has partially exercised that option since 2020 without legal exposure.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, international_business_community, beneficiary,
    powerful, immediate, arbitrage, global).

% Commutes, trades, and raises families in a city where confrontations no longer close roads or campuses; survey majorities during the unrest reported exhaustion with disruption. Carries the arrangement's diffuse indirect costs: a patriotism-focused civic curriculum for children, habitual self-censorship in ordinary expression, and an emigration wave of several hundred thousand residents — disproportionately young and professional — exercising British National (Overseas) and other visa routes.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, hong_kong_general_public, beneficiary,
    moderate, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__sovereignty_restoration_reading, hong_kong_general_public, payer).

% Mobilized the 2019 demonstrations and now bear the arrangement's direct costs: arrests under the four offense categories, trials in the designated judge track, sentences running to years, and organized formations dissolved under the societies ordinance. Those with warrants outstanding cannot return from abroad without arrest; those remaining organize nothing, since assembling itself now carries legal exposure.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, protest_movement_participants, payer,
    powerless, biographical, trapped, regional).

% Elected lawmakers and district councillors removed from office or barred from rerunning after oath-taking vetoes and the electoral restructuring, with dozens of the pan-democratic camp's core prosecuted collectively in the case of the forty-seven. Their careers, reputations, and in many cases their liberty are bound to a political identity the arrangement treats as a security category; leaving means renouncing that identity or living in permanent exile from the constituency that elected them.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, disqualified_opposition_legislators, payer,
    powerless, biographical, identity_locked, regional).

% Reporters and editors of the city's outspoken outlets: one flagship closed after police raids with its founder and executives charged under the collusion category, the remainder publishing under arrest precedent and licensing uncertainty. Professional identity runs through publication; the live choices are muted coverage, relocating outlets abroad, or prosecution.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, independent_press_practitioners, payer,
    moderate, biographical, constrained, regional).

% Governments of the United States, the United Kingdom, EU members, and allied states that sanctioned officials, suspended extradition treaties, and opened visa lanes for residents. Were never party to the drafting, object from outside the arrangement, and command no lever inside it short of measures that would also damage their own nationals' interests in the city.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, foreign_critical_governments, excluded,
    institutional, generational, constrained, global).

% Treaty-body experts and special procedures reviewing the arrangement against the ICCPR obligations extended to Hong Kong, publishing findings on the forty-seven case, press freedom, and the electoral restructuring, and receiving written replies but no compliance.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, un_human_rights_monitors, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nsl_legal_text__sovereignty_restoration_reading, cpg_sovereign_authority).
narrative_ontology:fixing_cost_class(nsl_legal_text__sovereignty_restoration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Assigns operational competence for national security in Hong Kong to the sovereign center: defines four offense categories (secession, subversion, terrorist activity, collusion with foreign forces), creates dedicated enforcement institutions (committee, police unit, designated judge track), and fills the gap left by the never-enacted local Article 23 legislation — problems previously handled nowhere are now handled in one place, by one command structure.
% TRANSFER_FUNCTION: Moves political freedom of action, organizational capacity, and — for those in custody — personal liberty from protest participants, opposition politicians, and independent journalists to the central authorities; moves adjudication of politically defined cases into a dedicated track insulated from jury trial; moves curricular and oath content into schools and offices as standardized loyalty formularies.
% ABSENT_VOICES: The law was drafted and enacted by the NPC Standing Committee without a Hong Kong legislative process or public consultation. The defendants now prosecuted under it, the staff of the closed newspaper, and the voters whose electoral map was redrawn afterward had no seat in drafting. Foreign governments object from outside; UN treaty bodies comment without leverage. Unanimity behind the arrangement's justification arises in forums from which every paying seat was absent.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, the governance equilibrium it maintains would dissolve: the institutions built around it (committees, designated track, prosecution pipeline) would lose their organizing mandate, the proscribed formations would reconstitute, and — on this reading's account — the street confrontation of 2019 would resume from the point of paralysis at which the arrangement intervened. Every seated party's position depends on its continuation or repeal; nothing about the status quo survives its removal unchanged.
% FOUNDING_PROBLEM: The 2019 unrest: months of escalating demonstrations and confrontations that paralyzed government, blocked the legislature, and culminated in levels of violence the local institutions could not contain — compounded by the decades-long failure to enact local national security legislation under Basic Law Article 23, leaving the sovereign's security prerogatives over the territory constitutionally assigned but practically unenforced.
% FOUNDING_PROBLEM_CORROBORATION: The existence and severity of the 2019 unrest is corroborated by sources outside the beneficiary coalition: contemporaneous press archives, arrest and casualty records, and academic studies of the movement. The claim that the problem REMAINS live — that threats of equivalent magnitude persist and justify continued intensive enforcement — rests almost entirely on statements by the central and SAR governments themselves; no independent non-beneficiary source attests the current magnitude of the threat. That corroboration gap is recorded plainly as signal, and omega restoration_endpoint_permanence tracks its consequences.
narrative_ontology:disappearance_verdict(nsl_legal_text__sovereignty_restoration_reading, world_rearranges).
narrative_ontology:founding_problem_status(nsl_legal_text__sovereignty_restoration_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nsl_legal_text__sovereignty_restoration_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nsl_legal_text__sovereignty_restoration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nsl_legal_text__sovereignty_restoration_reading, 0.56, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nsl_legal_text__sovereignty_restoration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nsl_legal_text__sovereignty_restoration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nsl_legal_text__sovereignty_restoration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   CLAIM/METRIC INDEPENDENCE: the claimed type is authored from this reading's own lights and the metrics from this reading's own observation, without tuning either to the other or to a predicted engine output. From this seat the arrangement is a tangled_rope: the reading sincerely holds the security-coordination function as real (the 2019 paralysis was the founding emergency; the offense categories and institutions address it), while acknowledging that identifiable classes pay through the same structure — that acknowledgment is exactly what separates this claim from a rope claim, which would require denying the victim structure the expected structural delta specifies. Metrics: extractiveness 0.56 is moderate because the costs concentrate on political opponents rather than the general population, per this reading's own account of its operation; suppression 0.67 is high because persistence depends on continuously operated machinery (national security police, designated judges, prosecution track, organizing offenses that raise the cost of coalition formation itself) — suppression is authored as a raw structural property and is NOT scaled by power or scope; only extractiveness is scaled, by directionality and scope, in the engine's computation. Theater 0.20: enforcement is predominantly functional (real dockets, real custodial sentences), with a growing but minority performative layer (security education days, exhibitions, oath ceremonies) as the acute phase recedes. Accessibility_collapse 0.62 blends two populations: for the target class, alternatives (assembly, organization, outspoken publication) have collapsed almost completely; for the general population, ordinary economic and private life continues with narrowed civic margins. Resistance 0.34 reflects the end state: the 2019-scale mobilization demonstrated the target class's latent coalition power, but the arrangement's organizing-offense provisions price coalition formation itself, so measured resistance decays across the series rather than re-concentrating. The measurement series run on ONE shared time grid (2020-2026, annual): every tracked metric is authored at every point, so no end-state scalar substitutes into earlier rows. The extractiveness trajectory peaks at consolidation (2021-2022: mass arrests, the forty-seven case, the flagship newspaper's closure, the electoral restructuring) and declines gently thereafter as the domestic target class exhausts — a decline partially offset by extraterritorial extension (overseas bounties), which keeps the floor from collapsing. Suppression_requirement is tracked because this story specifically traces enforcement-capacity change: rapid build-up through 2021, then slow normalization decay as deterrence substitutes for active suppression.
 *
 * PERSPECTIVAL GAP:
 *   The engine computes per-seat classifications from the structural data, and the seats should diverge sharply. From the cpg_sovereign_authority seat the arrangement is a successfully executed restoration it authored and solely controls; from the payer seats the same structure operates as continuous legal exposure. Inter-institutional dynamics: the NPCSC-level authority holds arbitrage over the text (sole interpretation power, amendment at will), the SAR establishment administers under it with no exit (its governability is staked on the arrangement's continuation), and foreign critical governments stand wholly outside with only escalatory levers that damage their own nationals' interests. Same-level lateral dynamics: three seats share nominal Hong Kong residency and radically different constraint-specific exits — the business community arbitrages freely across jurisdictions (listings, treasury functions, staff relocatable to Singapore), the general public holds a costly-but-open emigration valve (several hundred thousand departures via BN(O) and other routes), and protest participants with warrants outstanding are trapped, since returning means arrest. Equal global standing, unequal exits: the differentiation is produced entirely by each seat's position relative to THIS arrangement, which is what the directionality computation reads.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. cpg_sovereign_authority sits nearest the beneficiary pole (d near 0.0): it wrote the text, solely interprets it, and collects the returns — restored uncontested authority, eliminated street challenge, readable deterrence abroad. hong_kong_sar_establishment derives low d as beneficiary-administrator, damped slightly by its constrained position inside lines it did not draw. international_business_community derives very low d: it pays nothing into the arrangement and holds arbitrage-grade exit, placing it at the extreme beneficiary end. hong_kong_general_public derives near-symmetric d: genuine stability benefit against diffuse indirect costs, with mobile exit damping its effective extraction. The three victim classes derive high d: protest_movement_participants (trapped, powerless) sit nearest the full-target pole; disqualified_opposition_legislators add identity-lock (career, reputation, and in many cases liberty fused to a political identity the arrangement criminalizes), pushing them to the trapped-or-worse end despite nominal mobility via exile; independent_press_practitioners sit slightly lower (constrained: muted coverage or relocation remain technically available). No directionality overrides are authored: the derivation chain produces the correct d for every seat from the declarations and exit atoms alone.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite mislabelings. Against pure-extraction mislabeling: the arrangement has a real, currently functioning coordination core — the founding problem (2019-scale paralysis plus the decades-long Article 23 enforcement gap) is authored as LIVE from this reading's lights, and the low theater ratio (0.20, well under the 0.5 substitution threshold) records that enforcement activity is predominantly doing what it says. Against coordination whitewash: the victim declarations and the peaked-then-elevated extractiveness series record that the same structure transfers liberty and political capacity from identifiable classes to the center, so the arrangement cannot certify as a rope regardless of the sincerity of the security narrative. The forward risk this reading must price is mandatrophy-by-success: if the threat recedes and the apparatus is maintained at constant scale, the founding problem's liveness becomes self-asserted rather than evidenced — omega restoration_endpoint_permanence tracks exactly this, and the slowly rising theater series (0.10 to 0.20) is the early indicator to watch. The R5 mismatch consumer reads founding_problem_status (live) against disappearance_verdict (world_rearranges): no mismatch fires today, but a future flip of status to dead while the verdict holds would flag capture/zombie dynamics through the computed theater path.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_delta,
    'This story instantiates the sovereignty_restoration_reading of the nsl_legal_text kernel; how would the classification shift if the same standing arrangement were authored under the democratic_enclosure_reading or the jurisdictional_capture_reading?',
    'Cross-read the sibling files (nsl_legal_text__democratic_enclosure_reading, nsl_legal_text__jurisdictional_capture_reading): compare their authored epsilon over the identical referent, their victim sets, and their computed per-seat types.',
    'The enclosure reading should widen the victim set toward general civil society and raise epsilon substantially; the capture reading should shift victims toward the judiciary and common-law institutions. Divergence across the family is the measurement; convergence would indicate the readings are not structurally distinct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_delta, conceptual, 'Committer structure: one kernel, three readings, reading-indexed epsilon over a fixed referent.').

omega_variable(
    threat_magnitude_proportionality,
    'Is the security threat the arrangement targets — secession, subversion, terrorist activity, foreign collusion — of a magnitude that bears the weight the restoration narrative places on it?',
    'Declassified intelligence assessments, comparative counterfactuals (did large-scale unrest recur in comparable jurisdictions without equivalent statutes), and proportionality analysis of offense definitions against documented conduct.',
    'If documented threat magnitude is materially below the asserted level, the coordination half of the tangled_rope claim weakens and a larger share of measured extraction reads as rent rather than security cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threat_magnitude_proportionality, empirical, 'Whether the threat assessment underlying the restoration narrative is empirically warranted.').

omega_variable(
    victim_boundary_stability,
    'Is the bounded victim set — political opposition, independent press, protest participants — a stable design feature of the arrangement, or a stage that widens once the initial target class is exhausted?',
    'Longitudinal tracking of arrest and prosecution demographics, offense-category drift, and the occupational profile of defendants across the interval and beyond.',
    'Boundary expansion toward professionals, clergy, educators, or ordinary expressive conduct would push effective extraction upward across previously near-symmetric seats and destabilize the moderate-extraction profile this reading authors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_boundary_stability, empirical, 'Whether the target class stays bounded as the reading''s moderate-extraction claim presupposes.').

omega_variable(
    restoration_endpoint_permanence,
    'Does the restoration narrative imply a terminal state at which the arrangement''s intensive enforcement becomes redundant, or does it constitute permanent security infrastructure with no completion condition?',
    'Observe whether enforcement intensity, prosecution rates, and institutional staffing decay as the domestic threat recedes, or whether the apparatus is maintained at constant scale regardless of threat indicators.',
    'Decay would vindicate the reading''s transitional logic and pull the arrangement toward a completing-support profile; constancy would indicate the restoration framing functions as justification for a standing regime, with mandatrophy risk accumulating behind a live-problem declaration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restoration_endpoint_permanence, conceptual, 'Whether the reading''s own logic predicts an endpoint, and whether practice tracks it.').

omega_variable(
    extraterritorial_scope_drift,
    'How far does the arrangement''s practical reach extend beyond Hong Kong through Article 38 extraterritoriality, overseas bounty notices, and pressure on diaspora communities?',
    'Count and classify extraterritorial enforcement actions (warrants, bounties, notices to overseas entities) over time; measure chilling effects reported by diaspora organizations.',
    'Scope growth amplifies effective extraction at larger spatial scales and converts formerly mobile exits (emigration) into constrained ones for wanted persons, raising directionalities for seats currently scored as exited.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraterritorial_scope_drift, empirical, 'Trajectory of the arrangement''s effective spatial scope.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__sovereignty_restoration_reading, 2020, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl__tr_t2020, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(nsl__tr_t2021, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 2021, 0.12).
narrative_ontology:measurement(nsl__tr_t2022, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 2022, 0.15).
narrative_ontology:measurement(nsl__tr_t2023, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 2023, 0.16).
narrative_ontology:measurement(nsl__tr_t2024, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 2024, 0.18).
narrative_ontology:measurement(nsl__tr_t2025, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 2025, 0.19).
narrative_ontology:measurement(nsl__tr_t2026, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 2026, 0.2).

% Extraction over time
narrative_ontology:measurement(nsl__be_t2020, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 2020, 0.42).
narrative_ontology:measurement(nsl__be_t2021, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 2021, 0.58).
narrative_ontology:measurement(nsl__be_t2022, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 2022, 0.63).
narrative_ontology:measurement(nsl__be_t2023, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 2023, 0.62).
narrative_ontology:measurement(nsl__be_t2024, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 2024, 0.6).
narrative_ontology:measurement(nsl__be_t2025, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 2025, 0.58).
narrative_ontology:measurement(nsl__be_t2026, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 2026, 0.56).

% Suppression requirement over time
narrative_ontology:measurement(nsl__su_t2020, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 2020, 0.55).
narrative_ontology:measurement(nsl__su_t2021, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 2021, 0.72).
narrative_ontology:measurement(nsl__su_t2022, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 2022, 0.71).
narrative_ontology:measurement(nsl__su_t2023, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 2023, 0.7).
narrative_ontology:measurement(nsl__su_t2024, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 2024, 0.69).
narrative_ontology:measurement(nsl__su_t2025, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 2025, 0.68).
narrative_ontology:measurement(nsl__su_t2026, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 2026, 0.67).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_legal_text__sovereignty_restoration_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nsl_legal_text__sovereignty_restoration_reading, nsl_legal_text__democratic_enclosure_reading).
narrative_ontology:affects_constraint(nsl_legal_text__sovereignty_restoration_reading, nsl_legal_text__jurisdictional_capture_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'the NSL' conflates three structurally distinct claims that measure differently and must not share one story. This file (sovereignty_restoration_reading) authors moderate epsilon with a bounded victim set centered on political opposition; democratic_enclosure_reading authors high epsilon with the victim set widened to general civil society and democratic participation; jurisdictional_capture_reading authors epsilon concentrated on the judiciary and common-law institutional autonomy. The upstream/downstream structure runs from this reading outward: each successful invocation of sovereign-necessity justification under this reading alters the legitimacy conditions and institutional channel availability within which the sibling readings operate (declared as 'influences' toward the capture reading; 'coexists_with' toward the enclosure reading, which competes as a live opposed evaluation without logical foreclosure in either direction). All three files link one another via network.affects_constraints; orphaning any member would break contamination-propagation analysis across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
