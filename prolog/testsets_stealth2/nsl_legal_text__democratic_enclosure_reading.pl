% ============================================================================
% CONSTRAINT STORY: nsl_legal_text__democratic_enclosure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Hong Kong National Security Law — Democratic Enclosure Reading
 *   domain: constitutional law/political sociology/international relations
 *
 * SUMMARY:
 *   The National Security Law, imposed on Hong Kong in June 2020 by direct
 *   annexation to the Basic Law, operates — on this reading — as a machine
 *   for converting the city's democratic infrastructure into prosecutable
 *   exposure. Offense categories drawn broadly enough to reach speech,
 *   organization, journalism, and electioneering are enforced by a dedicated
 *   police unit, a designated judiciary, and a prosecutor corps answerable to
 *   the central authorities; the electoral system is restructured so that
 *   only screened candidates may stand; the largest independent newspaper is
 *   closed and its editors charged; unions, churches, and civic groups
 *   dissolve under liability exposure; and the law reaches extraterritorially
 *   through warrants and bounties on exiles. The referent of the authored
 *   metrics is the standing arrangement as it operates over Hong Kong civic
 *   life, assessed by this reading's own lights: the arrangement takes the
 *   political liberties and autonomous institutions of the resident
 *   population and delivers consolidated control to the center and
 *   distributed offices, budgets, and powers to its local agents. KEY AGENTS
 *   (by structural relationship): beijing_central_authorities — agenda setter
 *   (institutional/arbitrage), drafts, interprets, and enforces, collects
 *   consolidated control; hk_pro_establishment_political_class — local
 *   beneficiary-administrator (powerful/constrained), staffs the reconfigured
 *   institutions; hk_security_apparatus — enforcement beneficiary
 *   (powerful/constrained), budget and careers grow with prosecutions;
 *   prodemocracy_politicians_and_activists,
 *   independent_journalists_and_outlets, civil_society_organizations —
 *   primary targets (powerless-to-moderate/trapped), bear imprisonment,
 *   closure, and dissolution; general_hk_public — diffuse target
 *   (powerless/constrained), bears the chilled commons and the emigration
 *   split; hk_business_community — dual-positioned (powerful/mobile),
 *   collects stability and carries sanctions and talent-flight costs;
 *   exiled_dissident_networks — excluded (organized/mobile), barred and
 *   warranted; foreign_governments — external observer with direct
 *   extraterritorial exposure (institutional/analytical).
 *
 * KEY AGENTS:
 *   - - beijing_central_authorities: Agenda setter (institutional/arbitrage) — sole interpreter and amender; collects consolidated political control
 *   - - hk_pro_establishment_political_class: Beneficiary-administrator (powerful/constrained) — collects offices and passes implementing law
 *   - - hk_security_apparatus: Enforcement beneficiary (powerful/constrained) — expanding budget, powers, careers
 *   - - prodemocracy_politicians_and_activists: Primary target (powerless/trapped) — imprisoned, disqualified, or exiled
 *   - - independent_journalists_and_outlets: Primary target (moderate/trapped) — outlet closures and prosecutions
 *   - - civil_society_organizations: Primary target (powerless/trapped) — compelled dissolution
 *   - - general_hk_public: Diffuse target (powerless/constrained) — chilled speech, monitored online life, emigration valve
 *   - - hk_business_community: Dual-positioned (powerful/mobile) — collects stability, bears sanctions and talent-flight costs
 *   - - exiled_dissident_networks: Excluded (organized/mobile) — warranted and bountied abroad
 *   - - foreign_governments: External observer with extraterritorial exposure (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__democratic_enclosure_reading, 0.88).
domain_priors:suppression_score(nsl_legal_text__democratic_enclosure_reading, 0.9).
domain_priors:theater_ratio(nsl_legal_text__democratic_enclosure_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__democratic_enclosure_reading, snare).
narrative_ontology:human_readable(nsl_legal_text__democratic_enclosure_reading, "Hong Kong National Security Law — Democratic Enclosure Reading").
narrative_ontology:topic_domain(nsl_legal_text__democratic_enclosure_reading, "constitutional law/political sociology/international relations").

domain_priors:requires_active_enforcement(nsl_legal_text__democratic_enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__democratic_enclosure_reading, '11629950-e899-44ac-bf83-eec33e5fb4ac').
narrative_ontology:cs_kernel_codification('11629950-e899-44ac-bf83-eec33e5fb4ac', fixed_text).
narrative_ontology:cs_authority_grounding('11629950-e899-44ac-bf83-eec33e5fb4ac', extraction).
narrative_ontology:cs_interpretation_layer_present('11629950-e899-44ac-bf83-eec33e5fb4ac').
narrative_ontology:cs_reading_relation('11629950-e899-44ac-bf83-eec33e5fb4ac', nsl_legal_text__sovereignty_restoration_reading, coexists_with).
narrative_ontology:cs_reading_relation('11629950-e899-44ac-bf83-eec33e5fb4ac', nsl_legal_text__jurisdictional_capture_reading, coexists_with).
narrative_ontology:cs_axiom('11629950-e899-44ac-bf83-eec33e5fb4ac', foundational, nsl_offenses_capture_ordinary_dissent).
narrative_ontology:cs_axiom_status(nsl_offenses_capture_ordinary_dissent, holdable).
narrative_ontology:cs_axiom_grounding('11629950-e899-44ac-bf83-eec33e5fb4ac', nsl_offenses_capture_ordinary_dissent, empirically_contingent).
narrative_ontology:cs_axiom('11629950-e899-44ac-bf83-eec33e5fb4ac', foundational, democratic_closure_is_permanent_by_design).
narrative_ontology:cs_axiom_status(democratic_closure_is_permanent_by_design, holdable).
narrative_ontology:cs_axiom_grounding('11629950-e899-44ac-bf83-eec33e5fb4ac', democratic_closure_is_permanent_by_design, empirically_contingent).
narrative_ontology:cs_reference_frame('11629950-e899-44ac-bf83-eec33e5fb4ac', two_systems_civil_liberties_baseline).
narrative_ontology:cs_drift_state('11629950-e899-44ac-bf83-eec33e5fb4ac', contemporary_post_article23_era, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('11629950-e899-44ac-bf83-eec33e5fb4ac', '').
narrative_ontology:cs_kernel_id(nsl_legal_text__democratic_enclosure_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__democratic_enclosure_reading, beijing_central_authorities).
narrative_ontology:constraint_beneficiary(nsl_legal_text__democratic_enclosure_reading, hk_pro_establishment_political_class).
narrative_ontology:constraint_beneficiary(nsl_legal_text__democratic_enclosure_reading, hk_security_apparatus).
narrative_ontology:constraint_beneficiary(nsl_legal_text__democratic_enclosure_reading, hk_business_community).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, prodemocracy_politicians_and_activists).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, independent_journalists_and_outlets).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, civil_society_organizations).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, general_hk_public).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, hk_business_community).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, foreign_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted and imposed the law in June 2020 by annexing it directly to the Basic Law, bypassing the local legislature. Retains sole power of interpretation and amendment through the Standing Committee, operates an office in Hong Kong with its own enforcement personnel, and applies the law extraterritorially, including bounty notices for people living abroad. Sets the definitions of the security offenses and the pace of implementation.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, beijing_central_authorities, agenda_setter,
    institutional, generational, arbitrage, global).

% Staffs the reconfigured institutions: after the 2021 electoral overhaul only candidates screened as loyal may stand, so the legislature and district councils are filled from this class. It passes implementing legislation such as the 2024 safeguarding ordinance, defends the arrangement publicly, and collects the offices, salaries, and committee chairs previously held by the opposition. Its position depends on continued loyalty; defection means losing office.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, hk_pro_establishment_political_class, beneficiary,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__democratic_enclosure_reading, hk_pro_establishment_political_class, agenda_setter).

% Runs day-to-day enforcement: a dedicated police national security department with expanded arrest and surveillance powers, a designated list of judges authorized to hear security cases, and prosecutors selecting charges from the law's broad offense categories. Budget and headcount have grown steadily since 2020, and careers advance through successful prosecutions.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, hk_security_apparatus, beneficiary,
    powerful, biographical, constrained, regional).

% Former legislators, district councillors, union leaders, and organizers. Dozens are detained awaiting trial or serving multi-year sentences under subversion and collusion charges; others are disqualified from office, barred from elections, or have fled abroad. Party structures have dissolved. Remaining in Hong Kong political life carries prosecution risk; leaving means abandoning constituents, and some who left face warrants issued at home.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, prodemocracy_politicians_and_activists, payer,
    powerless, biographical, trapped, regional).

% Worked under accreditation and advertising markets now shaped by the security apparatus. The city's largest pro-democracy newspaper was raided, its assets frozen, and it closed in 2021; its founder and several editors face collusion and sedition charges. Remaining outlets exercise caution on political coverage; reporters describe narrowing editorial space, source attrition, and the departure of colleagues.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, independent_journalists_and_outlets, payer,
    moderate, biographical, trapped, regional).

% Unions, professional bodies, churches, NGOs, and mutual-aid groups that formed the city's associational life. Under registration scrutiny and member-liability exposure, most large groups voted to dissolve between 2020 and 2023 rather than continue operating; the rest restrict themselves to service provision. Dissolution was voluntary in form and compelled in substance.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, civil_society_organizations, payer,
    powerless, biographical, trapped, regional).

% Experiences the contraction indirectly and personally: mass protest is effectively unavailable, school curricula emphasize national security education, library books have been removed, film censorship has tightened, and online speech is monitored. Emigration pathways, notably the UK BN(O) visa, opened an exit taken by well over a hundred thousand residents, but eligibility and resources limit who can leave, and families are split across jurisdictions.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, general_hk_public, payer,
    powerless, generational, constrained, regional).

% Chambers of commerce, banks, and multinationals welcomed restored predictability after 2019 and continue to cite stability as the city's core asset. They now also carry costs: sanctions exposure for dealings with listed entities, senior-staff departures, talent outflow, and uncertainty about where commercial data or ordinary speech crosses a security line. Capital remains mobile and several regional headquarters have relocated.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, hk_business_community, beneficiary,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__democratic_enclosure_reading, hk_business_community, payer).

% Legislators, activists, and journalists operating from the UK, US, Australia, Taiwan, and Canada. They run advocacy, media, and mutual-support organizations abroad and testify to foreign parliaments. Authorities have issued arrest warrants and bounties for prominent figures, contacted their relatives, and warned foreign governments against engagement. They participate in the international conversation but are barred from any Hong Kong forum.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, exiled_dissident_networks, excluded,
    organized, biographical, mobile, global).

% Assess and respond through sanctions on Hong Kong and mainland officials, asylum and visa pathways, parliamentary inquiries, and statements in UN reviews. Their nationals and companies fall within the law's extraterritorial reach, giving them a direct stake. They hold no seat in the arrangement's decision structure, and their objections are dismissed as interference in internal affairs.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, foreign_governments, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__democratic_enclosure_reading, foreign_governments, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nsl_legal_text__democratic_enclosure_reading, beijing_central_authorities).
narrative_ontology:fixing_cost_class(nsl_legal_text__democratic_enclosure_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves, for the sovereign, the problem of securing political control over a territory whose population demonstrated mass-mobilization capacity in 2019: it standardizes offense definitions, unifies police, prosecution, and a designated judiciary under central oversight, screens candidacy for public office, and aligns schooling and public messaging with the security framework.
% TRANSFER_FUNCTION: Moves political liberties (speech, assembly, press, electoral choice) and institutional autonomy from Hong Kong residents and civil society to the central authorities and their local agents; moves offices, budgets, and enforcement powers to the pro-establishment class and the security apparatus; and moves several hundred thousand residents themselves out of the territory through emigration.
% ABSENT_VOICES: Imprisoned opposition figures, closed newsrooms, dissolved unions and congregations, and the general public whose consent the arrangement governs are absent from every forum where it is reviewed; exiled activists are barred and criminally warranted. The legislature now contains no opposition by design, so no seated voice articulates the position of those who bear the costs.
% DISAPPEARANCE_RATIONALE: The enforcement architecture, the electoral system, school curricula, the media market's structure, and the career structures of the security and establishment classes all depend on the arrangement. Overnight removal would reopen prosecutorial questions, invite the return of exiled organizations, and force reconstitution of the legislature; the city's political order would rearrange around its absence.
% FOUNDING_PROBLEM: Months of mass unrest in 2019 against an extradition bill, which the central authorities read as a threat to sovereignty exploiting gaps in Hong Kong's legal armor — Article 23 of the Basic Law had never been locally legislated since 2003.
% FOUNDING_PROBLEM_CORROBORATION: Beijing and the Hong Kong government attest a continuing threat, but both speak from the beneficiary set. Outside it: UN human rights treaty-body reviews, foreign parliamentary inquiries, international legal scholars, and former common-law judges attest that the 2019 unrest had subsided before imposition and characterize the subsequent expansion (the 2024 ordinance, extraterritorial warrants years after any disorder) as serving consolidation rather than responding to renewed violence. Exiled civil society corroborates from the target side. No neutral domestic forum exists to attest either way.
narrative_ontology:disappearance_verdict(nsl_legal_text__democratic_enclosure_reading, world_rearranges).
narrative_ontology:founding_problem_status(nsl_legal_text__democratic_enclosure_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nsl_legal_text__democratic_enclosure_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nsl_legal_text__democratic_enclosure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nsl_legal_text__democratic_enclosure_reading, 0.88, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored high (0.88) because the arrangement converts the city's entire democratic infrastructure — parties, press, unions, elections, assembly — into legal jeopardy and transfers control of it to the center; the transfer is the arrangement's operating output, not a side effect. Suppression is higher still (0.90) because persistence depends on continuously applied coercion: designated judges, an expanding security police, asset freezes, and extraterritorial warrants — not on participant preference. Theater is moderate-low and declining (0.34 to 0.20): the imposition period carried a heavy legitimation campaign (education rollouts, ceremony, boilerplate defenses), which gave way to routinized enforcement requiring less performance; the remaining share is anniversaries, security-education days, and official rhetoric. Accessibility collapse is 0.75: domestic alternatives to compliance (independent media, opposition parties, protest, advocacy NGOs) have nearly vanished, but the emigration valve — taken by hundreds of thousands — keeps the collapse short of total. Resistance is 0.45: intense organized resistance in 2019-2021, now largely exiled, underground, or reduced to memory-keeping and international advocacy, plus sustained foreign-government pushback. The measurement series run on ONE shared time grid (months since July 2020, points at 0/10/20/30/40/50/60) with every tracked metric authored at every point; the trajectory is a monotonic ratchet, not a cycle — no oscillation mechanism is present, so no cyclical commentary applies. suppression_requirement is authored deliberately: the story's traced dynamic is enforcement-capacity build-up (designated judge list in month ~1, the 47-democrats prosecution and electoral overhaul by month ~10, Apple Daily closure by month ~12, first convictions and oath purges by month ~20, bounty rounds at months ~36 and ~44, the 2024 implementing ordinance and its first prosecutions by month ~50). End-state measurement values equal the base_properties scalars; the base_properties describe the constraint at interval end.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and the divergence is the finding. From the agenda-setter seat (beijing_central_authorities) and the beneficiary seats (establishment class, security apparatus), the arrangement presents as restored order, fulfilled constitutional duty, and rewarded loyalty — a functioning governance structure. From the payer seats (politicians, journalists, civil society, the public), the identical structure presents as criminalization of ordinary democratic conduct and the dismantling of promised liberties. The business seat computes mixed: stability collected, mobility preserved, but real costs borne. The engine derives these per-seat classifications from the authored power, exit, and directionality data; nothing in the claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. beijing_central_authorities sit at the beneficiary pole (d near 0.0): the arrangement subsidizes them with control, and their arbitrage-grade exit — they wrote the rules and alone can amend or interpret them — pins them there. The establishment class and security apparatus derive low d as beneficiaries, with constrained exit keeping them dependent on the arrangement's continuation. The four victim groups derive high d toward the target pole; trapped exit options (imprisonment, closed outlets, compelled dissolution) place the activist, journalist, and civil-society seats at the extreme target end, where effective extraction is amplified, while the general public's constrained-but-real emigration exit moderates its d slightly below the trapped seats. The business community's dual declaration (beneficiary with payer secondary role) places it mid-low rather than at the pole — it collects stability and pays in sanctions exposure and talent flight. No directionality_overrides are authored: the override surface is keyed per power atom, and this story's atoms collide across opposed seats (the institutional atom contains both the agenda setter and the external observer; the powerful atom contains pure beneficiaries and the dual-positioned business seat), so any per-atom override would misdirect more seats than it corrects. Roles plus exit options carry the differentiation instead. Spatial scope is global (extraterritorial warrants and bounties), which the engine reflects as harder verification and a modest amplification of effective extraction; suppression, by contrast, is a raw structural property and enters unscaled.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the 2019 unrest — had subsided before imposition, and the arrangement has since expanded rather than wound down: a second implementing statute in 2024, bounty rounds against exiles years after any disorder, and a permanent screening apparatus. On this reading the mandate has outlived its founding function, hence mandatrophy_resolved: true. The classification discipline matters here in both directions: labeling the arrangement a rope (pure coordination for security) would erase the identifiable victims and launder enclosure as collective benefit; labeling it a piton would misread an actively intensified machine as inertial residue — the enforcement trajectory is rising, not decaying. The snare claim keeps the victim set visible and the enforcement dependency explicit. The R5 interview adds the audit trail: founding_problem_status is contested rather than dead because the beneficiary seats attest a live threat, but the corroboration field names attestations from outside the beneficiary set (UN treaty bodies, foreign inquiries, former common-law judges) supporting the spent-mandate reading — the asymmetry between self-attestation and external corroboration is itself the signal a reviewer should weigh.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading (democratic_enclosure_reading) of kernel nsl_legal_text. What structural facts would the sibling readings (sovereignty_restoration_reading, jurisdictional_capture_reading) change?',
    'Cross-reading corpus comparison: the sovereignty reading authors low epsilon with a victim set limited to violent actors and treats the security warrant as genuine; the capture reading concentrates epsilon on legal-institutional autonomy rather than civic space. The readings remain separate files per the epsilon-invariance principle.',
    'Classification is reading-indexed: under the sovereignty reading the same text computes toward rope or tangled_rope; under this reading it computes as a snare. Cross-reading divergence is the measurement the corpus exists to take, not noise to be reconciled.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: this story is one of three readings of the NSL text; records what sibling readings would change structurally.').

omega_variable(
    residual_coordination_or_cover,
    'Does the arrangement retain any genuine order-maintenance function that the pre-existing Public Order Ordinance, sedition law, and police powers could not have performed, or is the security framing wholly cover for political enclosure?',
    'Counterfactual legal analysis: compare the conduct actually prosecuted against what existing ordinances already reached; identify any charged conduct that required the new offense categories rather than existing ones.',
    'A wholly-cover finding confirms the snare claim; a demonstrable irreducible security function would mark a coordination residue and push the boundary of this reading''s classification toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_coordination_or_cover, conceptual, 'Whether any coordination function survives once the enclosure function is subtracted.').

omega_variable(
    chilling_internalization_split,
    'How much of the observed contraction in speech, publishing, and association is produced by direct enforcement versus anticipatory self-censorship that would persist even if enforcement relaxed?',
    'Longitudinal behavioral and survey data: publication rates, event notifications sought, self-censorship indices, and post-acquittal behavior of defendants; compare cohorts exposed to different enforcement intensity.',
    'An internalized component means effective suppression exceeds the structural measure and would outlast any formal relaxation — decisive for any reversibility or transition assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chilling_internalization_split, empirical, 'Structural versus internalized share of the measured suppression.').

omega_variable(
    permanence_design_question,
    'Is the closure permanent by design — no sunset, interpretive monopoly, extraterritorial reach, layered implementing statute — or contingent on conditions a future renegotiation could alter?',
    'Institutional-design analysis of the amendment path (Standing Committee monopoly, Annex III procedure) combined with historical baselines of central-government policy reversals toward Hong Kong.',
    'If reversibility is structurally real, a transitional characterization becomes coherent for some seats; if the design locks the arrangement in, the permanence premise of this reading holds and the snare classification stabilizes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(permanence_design_question, empirical, 'Designed permanence versus contingent reversibility of the enclosure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__democratic_enclosure_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl_dem_enc_tr_t0, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 0, 0.34).
narrative_ontology:measurement(nsl_dem_enc_tr_t10, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement(nsl_dem_enc_tr_t20, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 20, 0.29).
narrative_ontology:measurement(nsl_dem_enc_tr_t30, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 30, 0.27).
narrative_ontology:measurement(nsl_dem_enc_tr_t40, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement(nsl_dem_enc_tr_t50, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 50, 0.22).
narrative_ontology:measurement(nsl_dem_enc_tr_t60, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 60, 0.2).

% Extraction over time
narrative_ontology:measurement(nsl_dem_enc_be_t0, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(nsl_dem_enc_be_t10, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 10, 0.63).
narrative_ontology:measurement(nsl_dem_enc_be_t20, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 20, 0.71).
narrative_ontology:measurement(nsl_dem_enc_be_t30, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 30, 0.77).
narrative_ontology:measurement(nsl_dem_enc_be_t40, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 40, 0.82).
narrative_ontology:measurement(nsl_dem_enc_be_t50, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 50, 0.86).
narrative_ontology:measurement(nsl_dem_enc_be_t60, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 60, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(nsl_dem_enc_su_t0, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(nsl_dem_enc_su_t10, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(nsl_dem_enc_su_t20, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 20, 0.74).
narrative_ontology:measurement(nsl_dem_enc_su_t30, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 30, 0.8).
narrative_ontology:measurement(nsl_dem_enc_su_t40, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 40, 0.85).
narrative_ontology:measurement(nsl_dem_enc_su_t50, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 50, 0.88).
narrative_ontology:measurement(nsl_dem_enc_su_t60, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 60, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_legal_text__democratic_enclosure_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nsl_legal_text__democratic_enclosure_reading, nsl_legal_text__sovereignty_restoration_reading).
narrative_ontology:affects_constraint(nsl_legal_text__democratic_enclosure_reading, nsl_legal_text__jurisdictional_capture_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the NSL' decomposes into three structurally distinct readings of one kernel text, per the epsilon-invariance principle. This file (democratic_enclosure_reading) authors epsilon 0.88 with civil society, press, and opposition in the victim set. The sovereignty_restoration_reading authors low epsilon over the same referent with a victim set confined to violent actors, and is the reading the beneficiary seats cite as warrant — upstream legitimacy claim feeding this arrangement's defense. The jurisdictional_capture_reading shares this reading's critical stance but locates the harm in legal-system transplantation and common-law erosion rather than civic enclosure; its victim set centers the bench, bar, and contractual order. Each story links the other two via affects_constraints; divergence in their computed classifications is the cross-reading measurement, not an inconsistency to repair.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
