% ============================================================================
% CONSTRAINT STORY: nsl_legal_text__democratic_enclosure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   constraint_id: nsl_legal_text__democratic_enclosure_reading
 *   human_readable: Hong Kong National Security Law — Democratic Enclosure Reading
 *   domain: constitutional/political/international
 *
 * SUMMARY:
 *   The National Security Law, enacted by NPCSC decision on 30 June 2020 and
 *   inserted directly into Annex III of the Basic Law, established a parallel
 *   security architecture in Hong Kong: a security committee chaired by the
 *   Chief Executive, a mainland-staffed investigative office operating
 *   outside local supervision, designated national-security judges, and four
 *   offense categories reaching secession, subversion, terrorism, and
 *   collusion with foreign forces. This story instantiates the
 *   democratic_enclosure_reading of that contested text (kernel
 *   nsl_legal_text): it treats criminalization of dissent and permanent
 *   closure of democratic space as the arrangement's operative function, with
 *   the security rationale supplying legitimation rather than limitation.
 *   Across the interval the enforcement record shows a monotonic ratchet:
 *   mass arrest of opposition primary organizers, forced closure of the
 *   flagship critical newspaper, dissolution of most civil-society
 *   federations, restructuring of electoral rules to exclude unvetted
 *   candidacies, widening of the offense surface through the 2024
 *   Safeguarding National Security Ordinance, and a bounty regime reaching
 *   exiled figures abroad. FAMILY NOTE: sibling stories
 *   nsl_legal_text__sovereignty_restoration_reading (which authors low
 *   epsilon over the same referent, reading the same record as legitimate
 *   restoration) and nsl_legal_text__jurisdictional_capture_reading
 *   (intermediate epsilon, focused on common-law institutional erosion)
 *   decompose the colloquial label; this file carries the reading-indexed
 *   high-epsilon instantiation and links both siblings through
 *   network.affects_constraints. KEY AGENTS (by structural relationship): -
 *   beijing_central_authorities: Primary beneficiary and agenda-setter
 *   (institutional/arbitrage) — authors, interprets, and collects the
 *   arrangement's durable gains - hk_government_establishment: Administrator
 *   and secondary beneficiary (institutional/identity_locked) — enforces
 *   locally and absorbs the career payoff - pro_establishment_political_camp:
 *   Conditional beneficiary (organized/constrained) -
 *   hk_opposition_politicians: Primary target (powerless/trapped) -
 *   independent_press_sector: Primary target (powerless/trapped) -
 *   civil_society_organizations: Primary target (powerless/trapped) -
 *   pro_democracy_activists: Primary target (powerless/trapped) -
 *   hk_electorate: Diffuse payer (moderate/constrained) -
 *   overseas_exiled_dissenters: Extraterritorial target, structurally
 *   excluded (powerless/constrained) - un_human_rights_mechanisms: Analytical
 *   observer (institutional/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__democratic_enclosure_reading, 0.88).
domain_priors:suppression_score(nsl_legal_text__democratic_enclosure_reading, 0.92).
domain_priors:theater_ratio(nsl_legal_text__democratic_enclosure_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__democratic_enclosure_reading, snare).
narrative_ontology:human_readable(nsl_legal_text__democratic_enclosure_reading, "Hong Kong National Security Law — Democratic Enclosure Reading").
narrative_ontology:topic_domain(nsl_legal_text__democratic_enclosure_reading, "constitutional/political/international").

domain_priors:requires_active_enforcement(nsl_legal_text__democratic_enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__democratic_enclosure_reading, 'bc94d2f3-135b-43b2-8bb5-8c156052b287').
narrative_ontology:cs_kernel_codification('bc94d2f3-135b-43b2-8bb5-8c156052b287', fixed_text).
narrative_ontology:cs_authority_grounding('bc94d2f3-135b-43b2-8bb5-8c156052b287', extraction).
narrative_ontology:cs_interpretation_layer_present('bc94d2f3-135b-43b2-8bb5-8c156052b287').
narrative_ontology:cs_reading_relation('bc94d2f3-135b-43b2-8bb5-8c156052b287', nsl_legal_text__sovereignty_restoration_reading, coexists_with).
narrative_ontology:cs_reading_relation('bc94d2f3-135b-43b2-8bb5-8c156052b287', nsl_legal_text__jurisdictional_capture_reading, coexists_with).
narrative_ontology:cs_axiom('bc94d2f3-135b-43b2-8bb5-8c156052b287', foundational, dissent_is_primary_enforcement_object).
narrative_ontology:cs_axiom_status(dissent_is_primary_enforcement_object, holdable).
narrative_ontology:cs_axiom_grounding('bc94d2f3-135b-43b2-8bb5-8c156052b287', dissent_is_primary_enforcement_object, empirically_contingent).
narrative_ontology:cs_axiom('bc94d2f3-135b-43b2-8bb5-8c156052b287', foundational, democratic_closure_is_irreversible_by_design).
narrative_ontology:cs_axiom_status(democratic_closure_is_irreversible_by_design, holdable).
narrative_ontology:cs_axiom_grounding('bc94d2f3-135b-43b2-8bb5-8c156052b287', democratic_closure_is_irreversible_by_design, empirically_contingent).
narrative_ontology:cs_reference_frame('bc94d2f3-135b-43b2-8bb5-8c156052b287', democratic_enclosure_apparatus).
narrative_ontology:cs_drift_state('bc94d2f3-135b-43b2-8bb5-8c156052b287', post_article23_codification, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('bc94d2f3-135b-43b2-8bb5-8c156052b287', '2026-08-05T00:00:00Z').
narrative_ontology:cs_kernel_id(nsl_legal_text__democratic_enclosure_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__democratic_enclosure_reading, beijing_central_authorities).
narrative_ontology:constraint_beneficiary(nsl_legal_text__democratic_enclosure_reading, hk_government_establishment).
narrative_ontology:constraint_beneficiary(nsl_legal_text__democratic_enclosure_reading, pro_establishment_political_camp).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, hk_opposition_politicians).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, independent_press_sector).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, civil_society_organizations).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, pro_democracy_activists).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, hk_electorate).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, overseas_exiled_dissenters).
narrative_ontology:constraint_vindicates(nsl_legal_text__democratic_enclosure_reading, holistic_national_security_concept).
narrative_ontology:constraint_vindicates(nsl_legal_text__democratic_enclosure_reading, executive_led_governance_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authored the law by NPCSC decision and inserted it directly into Annex III of the Basic Law, bypassing local legislative process. Sets interpretation policy through the NPCSC, directs the Office for Safeguarding National Security, which reports to Beijing rather than to Hong Kong institutions, and holds transfer authority over cases it designates complex or foreign-involved. Collects durable political control over a territory whose institutions it does not staff. Bears no enforcement costs locally, and nothing in the arrangement constrains its other policy instruments.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, beijing_central_authorities, agenda_setter,
    institutional, generational, arbitrage, global).

% Administers day-to-day enforcement: the Chief Executive chairs the Committee for Safeguarding National Security and designates the judges who hear security cases; the Department of Justice brings prosecutions; the disciplined services execute arrests and surveillance. Officials receive career continuity, expanded budgets, and governing legitimacy from the arrangement. Individual officials cannot defect without ending their careers and exposing family members employed within the same system; the bureaucracy's standing is fused with administering the arrangement competently.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, hk_government_establishment, agenda_setter,
    institutional, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__democratic_enclosure_reading, hk_government_establishment, beneficiary).

% Occupies the legislative and district-governance seats opened by the restructuring of election rules. Receives office, patronage, and policy access conditional on demonstrated reliability. Its members now compete only against vetted co-partisans; losing that protected position would mean facing open electoral competition they have historically lost.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, pro_establishment_political_camp, beneficiary,
    organized, biographical, constrained, regional).

% Former legislators, district councillors, and primary-election organizers. Most are disqualified from office, awaiting trial, serving sentences, or in exile; party registration and fundraising channels are closed. Re-entry into formal politics requires passing vetting that excludes their platform by construction. Those already abroad face bounty announcements and passport invalidation.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, hk_opposition_politicians, payer,
    powerless, biographical, trapped, regional).

% Ran Hong Kong's last mass-circulation critical newsrooms. The flagship outlet closed after advertiser withdrawal, asset freezes, and editorial arrests; remaining newsroom leaders operate under sedition statutes running parallel to the security law, licensing dependence, and reporter detention. Individual journalists can emigrate, and many have; the institutions they worked for cannot relocate their audience or archives without ceasing operations.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, independent_press_sector, payer,
    powerless, biographical, trapped, regional).

% Trade-union federations, humanitarian monitors, professional associations, and rights groups that organized marches, provided legal aid, and documented policing. Most dissolved preemptively after leadership arrests made continued existence a personal-liability decision for board members. Bank accounts were frozen, venues declined bookings, and membership drained as affiliation became prosecutable association.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, civil_society_organizations, payer,
    powerless, biographical, trapped, regional).

% Student leaders and movement organizers from the 2014 and 2019 cycles, currently distributed across remand centers, prisons, and exile. Their organizing repertoire — petitions, rallies, primaries — is now charged conduct. Personal futures turn on outcomes they cannot influence from custody or from abroad.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, pro_democracy_activists, payer,
    powerless, biographical, trapped, regional).

% Several million registered voters, turnout-capable and historically decisive in competitive cycles, now offered ballots containing only vetted candidates. Individual members retain ordinary economic life and may emigrate; the collective channel through which their preferences reached government is closed. Holding a political preference is not itself illegal, but expressing it organizationally is.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, hk_electorate, payer,
    moderate, biographical, constrained, regional).

% Activists, former legislators, and commentators operating from the UK, US, Australia, Taiwan, and elsewhere. Announced under bounty warrants; passports invalidated; relatives in Hong Kong visited and questioned. They continue advocacy abroad while remaining formally unreachable by the territory's institutions except through the warrant regime directed at them.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, overseas_exiled_dissenters, payer,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__democratic_enclosure_reading, overseas_exiled_dissenters, excluded).

% The Human Rights Committee reviewing ICCPR compliance as applied to Hong Kong, plus special procedures receiving submissions. Produces findings and recommendations addressed to the signatory state; holds no enforcement instruments; documents the gap between statutory assurances and prosecution records.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, un_human_rights_mechanisms, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nsl_legal_text__democratic_enclosure_reading, beijing_central_authorities).
narrative_ontology:fixing_cost_class(nsl_legal_text__democratic_enclosure_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Centralizes national-security governance for Hong Kong: creates a National Security Committee chaired by the Chief Executive, a dedicated police unit with covert-policing powers, designated judges, and four statutory offense categories (secession, subversion, terrorism, collusion with foreign forces), replacing the prior absence of any national-security statute after the 2003 Article 23 bill failed.
% TRANSFER_FUNCTION: Moves prosecutorial discretion, organizational existence, publication capacity, candidacy eligibility, and personal liberty away from opposition politicians, independent outlets, civil-society groups, and activists, toward the central authorities and the governing establishment; moves political risk downward onto anyone who speaks or organizes against the arrangement, and upward concentration of interpretive authority to the NPCSC.
% ABSENT_VOICES: Detained defendants whose cases define the law's interpretation cannot contest it; exiled activists are outside the jurisdiction under active warrants; the general population was never consulted — the law was enacted by NPCSC decision with no Hong Kong ratification vote; pan-democratic voters lost representation when electoral rules were restructured to exclude unvetted candidates. None of these seats is reachable by the institutions that administer the arrangement.
% DISAPPEARANCE_RATIONALE: If the law vanished overnight: dissolved organizations would re-form, shuttered outlets would reopen or successors would launch, exiled figures would return or regain standing, electoral competition would resume over genuinely contested boundaries, pending prosecutions would collapse, and the political field would reorganize around the reopened space within months. Every named seat's situation depends on the arrangement's continuance.
% FOUNDING_PROBLEM: The central government possessed no legal instrument to prosecute secession, subversion, terrorism, or foreign collusion in Hong Kong after the 2003 Article 23 bill was withdrawn amid mass protest; the 2019 cycle — at its peak involving citywide road blockades, airport disruption, and violent clashes — presented that legislative gap as an acute control problem.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: the UN Human Rights Committee's 2022 periodic-review findings and successive special-procedure communications attest that enforcement overwhelmingly targets peaceful expression far beyond any violent-threat threshold, supporting a shifted-function reading; foreign-government assessments and independent academic coding of prosecution patterns corroborate. The benefiting parties attest that security threats remain live and cite continuing foreign interference. No attestation from within the affected victim classes reaches the official record — detained and warrant-listed voices are structurally unable to corroborate anything, which is itself the strongest available signal about where dissent sits in the arrangement.
narrative_ontology:disappearance_verdict(nsl_legal_text__democratic_enclosure_reading, world_rearranges).
narrative_ontology:founding_problem_status(nsl_legal_text__democratic_enclosure_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nsl_legal_text__democratic_enclosure_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
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
 *   Extractiveness 0.88 reflects enforcement reaching every node of the democratic infrastructure rather than a narrow offender class: parties, press, unions, professional bodies, publishers, schools. Suppression 0.92 reflects machinery rather than atmosphere — a dedicated police unit with covert-policing powers, designated judges, no-jury trials for security cases, routine denial of bail, passport-surrender orders, and the 2024 ordinance's widened offense surface; suppression is authored as a raw structural property and is NOT scaled by power or scope in the engine's arithmetic, unlike extractiveness. Theater 0.18 is deliberately low: the apparatus was operational within weeks of promulgation, and performative activity (patriotic curricula, staged consultation exercises preceding Article 23) accretes late and modestly against continuous functional enforcement. Accessibility collapse 0.78: once the veto-points were mapped, alternatives collapsed nearly completely for organized dissent — open candidacies, independent mass-circulation journalism, street mobilization, and formal NGO operation all closed; residual channels (individual expression, emigration, litigation of narrowing scope) keep it short of the near-total collapse characteristic of natural limits. Resistance 0.5: sustained external resistance (sanctions, parliamentary inquiries, UN review) against largely extinguished domestic overt resistance. All temporal series share one grid (t=0..6, annual); trajectories are monotonic ratchets rather than cycles — no relaxation phase exists because no accountability loop remains that could force one. CLAIM/METRIC INDEPENDENCE: claimed_type snare is asserted from this reading's structural seat (identifiable victim classes, a coordination claim functioning as cover, persistence through coercion and exit-suppression); the engine computes per-seat classifications independently, and divergence is the measurement the corpus exists to take.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat the identical text presents as constitutional completion — order restored, a governance gap closed; that experience is the sovereignty_restoration_reading's home terrain, and the engine will compute it from the beneficiary-side structural data. From the payer seats the same clauses operate as criminalization: candidacy vetting, seditious-publication prosecution, organizational dissolution. Between the two institutional beneficiaries the divergence is subtler: Beijing holds an arbitrage-grade position (collects durable control, bears no local enforcement burden), while the Hong Kong establishment is identity_locked — its administrative standing, career lattice, and institutional self-conception are fused with administering the arrangement, so it cannot price exit even though it shares nominal power. Same-level institutional seats therefore compute different directionalities and different effective extraction from identical formal authority.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive the derivation: beijing_central_authorities, hk_government_establishment, and pro_establishment_political_camp enter through beneficiaries[], placing them near the beneficiary pole, with exit profiles grading them (arbitrage lowest; identity_locked and constrained progressively nearer symmetry). Six victim classes enter through victims[] with payer roles: trapped exits (opposition politicians, press, civil society, activists) sit near the full-target pole; the electorate's constrained-but-individually-mobile profile sits somewhat lower; exiled dissenters remain high-d targets because physical exit did not terminate exposure — bounty warrants, invalidated passports, and family pressure reach across borders. Global spatial scope on the enforcing side amplifies effective extraction modestly in the engine's arithmetic; suppression enters unscaled. No directionality_overrides were needed: the derivation from declared structure reproduces this reading's structural picture without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem interview returns a contested status against a world_rearranges verdict: the parties dispute whether the security problem remains live, but no party disputes that arrangements now depend on the text. The mandatrophy lens guards the reverse of the usual error here — not coordination mislabeled as extraction, but extraction wearing a coordination charter. Because the charter (response to the unresolved 2003 Article 23 gap and the 2019 unrest) is partially defensible as history, a naive reading could certify steady-state security coordination; the victim declarations, enforcement-dependence, and exit-suppression data prevent that certification. Had the founding problem been cleanly dead, the status-times-verdict mismatch would flag zombie maintenance directly; the contested status routes the live/dead question to the omegas instead (threat-category expansiveness, suppression internalization), where it belongs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This story is one reading of kernel nsl_legal_text — the democratic_enclosure_reading. Which structural elements of the text do the three readings treat as load-bearing, and how do the sibling readings redistribute the beneficiary and victim sets and epsilon over the same referent?',
    'Compile the sibling stories nsl_legal_text__sovereignty_restoration_reading and nsl_legal_text__jurisdictional_capture_reading and compare per-seat classifications, epsilon, and victim declarations across the triplet; locate the disagreement in the two load-bearing elements: the genuineness of the security rationale, and the operative mechanism (political criminalization vs legal-system substitution vs sovereign restoration).',
    'The sovereignty reading would move civil society, press, and opposition out of the victim set entirely and author low epsilon (a legitimacy-restoration profile over the same referent); the jurisdictional_capture reading would shift victims toward common-law institutions and author intermediate epsilon; this reading maximizes the victim set across the whole democratic infrastructure and authors high epsilon.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer-frame indexicality: which reading of the NSL text a story instantiates determines its victim set and epsilon; siblings are separate constraints linked by network edges.').

omega_variable(
    suppression_internalization_chilling,
    'Is the measured suppression structural (statutes, policing, prosecution machinery) or internalized (self-censorship and anticipatory obedience that persists independent of current enforcement probability)?',
    'Post-intensity-change trajectory analysis: track rates of speech, publication, and organizational formation following any sharp drop in enforcement intensity (amnesty, non-enforcement period, repeal); persistent depression after barrier removal indicates internalized carryover.',
    'If a substantial share is internalized, effective closure outlasts the text itself — repeal alone would not reopen the space quickly, raising persistence estimates and weakening any transitional reading of the arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_chilling, empirical, 'Structural vs internalized components of suppression in the closure mechanism.').

omega_variable(
    threat_category_expansiveness,
    'Are the offense categories (especially collusion with foreign forces and the adjacent sedition surface) defined so broadly that any advocacy of autonomy or criticism of the arrangement qualifies, making the founding security problem unfalsifiable and live-by-construction?',
    'Systematic coding of prosecuted cases against statutory thresholds: the distribution of defendants whose conduct involved violence, genuine foreign direction, or neither; doctrinal analysis of element breadth by independent legal scholars.',
    'If most prosecutions involve neither violence nor foreign direction, the security rationale functions as universal legitimation for dissent-targeting, reinforcing the enclosure reading against the restoration reading and confirming that the founding problem cannot die while defined this way.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(threat_category_expansiveness, empirical, 'Whether offense breadth converts the security mandate into a dissent mandate.').

omega_variable(
    exile_coalition_capacity,
    'Can the fragmented victim classes — exiled politicians, diaspora journalists, surviving mutual-aid networks, and the domestic electorate''s residual associational life — reconstruct coalition capacity sufficient to raise resistance above the level enforcement can absorb?',
    'Track exiled network formation, cross-border media reach, sanction coordination among foreign governments, and the elasticity of enforcement response (new bounty waves, ordinance amendments, expanded offense categories) to coalition milestones.',
    'Coalition revival would push resistance upward and enforcement cost upward, potentially dating a transition from stable closure toward contested maintenance; continued fragmentation keeps the monotonic ratchet intact and the current classification stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exile_coalition_capacity, empirical, 'Feasibility of victim-coalition reconstruction under extraterritorial pressure.').

omega_variable(
    extraterritorial_enforcement_reach,
    'How far does effective enforcement actually extend beyond Hong Kong''s jurisdiction — bounty-regime reach, notice service, family-pressure leverage, financial-channel denial — and does that effective reach exceed the territorial scope encoded in the stakeholders'' spatial_scope atoms?',
    'Enumerate enforcement actions against persons resident outside Hong Kong over the interval (warrants announced, assets frozen, visa denials, relatives questioned) and compare against the declared scope atoms of the targeted seats.',
    'If effective reach is substantially supraterritorial, scope-amplified effective extraction for diaspora targets is understated unless their scope atoms are revised upward; per-seat classifications for those seats shift accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraterritorial_enforcement_reach, empirical, 'Effective geographic reach of the enforcement apparatus versus authored scope atoms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__democratic_enclosure_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl__tr_t0, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(nsl__tr_t0, observed).
narrative_ontology:measurement(nsl__tr_t1, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 1, 0.11).
narrative_ontology:measurement_basis(nsl__tr_t1, observed).
narrative_ontology:measurement(nsl__tr_t2, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 2, 0.13).
narrative_ontology:measurement_basis(nsl__tr_t2, observed).
narrative_ontology:measurement(nsl__tr_t3, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 3, 0.14).
narrative_ontology:measurement_basis(nsl__tr_t3, observed).
narrative_ontology:measurement(nsl__tr_t4, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 4, 0.16).
narrative_ontology:measurement_basis(nsl__tr_t4, observed).
narrative_ontology:measurement(nsl__tr_t5, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 5, 0.17).
narrative_ontology:measurement_basis(nsl__tr_t5, observed).
narrative_ontology:measurement(nsl__tr_t6, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 6, 0.18).
narrative_ontology:measurement_basis(nsl__tr_t6, observed).

% Extraction over time
narrative_ontology:measurement(nsl__be_t0, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement_basis(nsl__be_t0, observed).
narrative_ontology:measurement(nsl__be_t1, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 1, 0.71).
narrative_ontology:measurement_basis(nsl__be_t1, observed).
narrative_ontology:measurement(nsl__be_t2, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 2, 0.78).
narrative_ontology:measurement_basis(nsl__be_t2, observed).
narrative_ontology:measurement(nsl__be_t3, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 3, 0.82).
narrative_ontology:measurement_basis(nsl__be_t3, observed).
narrative_ontology:measurement(nsl__be_t4, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 4, 0.85).
narrative_ontology:measurement_basis(nsl__be_t4, observed).
narrative_ontology:measurement(nsl__be_t5, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 5, 0.87).
narrative_ontology:measurement_basis(nsl__be_t5, observed).
narrative_ontology:measurement(nsl__be_t6, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 6, 0.88).
narrative_ontology:measurement_basis(nsl__be_t6, observed).

% Suppression requirement over time
narrative_ontology:measurement(nsl__su_t0, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 0, 0.66).
narrative_ontology:measurement_basis(nsl__su_t0, observed).
narrative_ontology:measurement(nsl__su_t1, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 1, 0.75).
narrative_ontology:measurement_basis(nsl__su_t1, observed).
narrative_ontology:measurement(nsl__su_t2, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 2, 0.8).
narrative_ontology:measurement_basis(nsl__su_t2, observed).
narrative_ontology:measurement(nsl__su_t3, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 3, 0.84).
narrative_ontology:measurement_basis(nsl__su_t3, observed).
narrative_ontology:measurement(nsl__su_t4, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 4, 0.89).
narrative_ontology:measurement_basis(nsl__su_t4, observed).
narrative_ontology:measurement(nsl__su_t5, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 5, 0.91).
narrative_ontology:measurement_basis(nsl__su_t5, observed).
narrative_ontology:measurement(nsl__su_t6, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 6, 0.92).
narrative_ontology:measurement_basis(nsl__su_t6, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_legal_text__democratic_enclosure_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nsl_legal_text__democratic_enclosure_reading, nsl_legal_text__sovereignty_restoration_reading).
narrative_ontology:affects_constraint(nsl_legal_text__democratic_enclosure_reading, nsl_legal_text__jurisdictional_capture_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the NSL' covers at least three structurally distinct claims about one enacted text. Per the epsilon-invariance principle these are separate constraint stories sharing one kernel: this file instantiates democratic_enclosure_reading (epsilon 0.88, victims span the democratic infrastructure); the sovereignty_restoration sibling instantiates a low-epsilon legitimacy profile over the same referent; the jurisdictional_capture sibling instantiates intermediate epsilon with victims concentrated in common-law institutions. The upstream/downstream structure runs through the shared enforcement record: each reading cites the same prosecutions and ordinances as evidence for its own mechanism, so the stories contaminate each other's evidentiary base and are linked bidirectionally through network edges rather than averaged into one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
