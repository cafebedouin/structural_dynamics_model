% ============================================================================
% CONSTRAINT STORY: nsl_legal_text__democratic_enclosure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: nsl_legal_text__democratic_enclosure_reading
 *   human_readable: Hong Kong National Security Law — Democratic Enclosure Reading
 *   domain: constitutional_law/political_sociology/international_relations
 *
 * SUMMARY:
 *   Enacted by the NPC Standing Committee in June 2020 and imposed on Hong
 *   Kong without local legislative process, the National Security Law
 *   criminalizes secession, subversion, terrorism, and collusion with foreign
 *   forces in broad, elastic terms. Under this reading, the law's central
 *   function is not suppressing violence — that problem had substantially
 *   receded by the time of enactment — but the systematic disqualification of
 *   opposition candidates, mass resignation and arrest of pro-democracy
 *   legislators, forced closure of independent media (Apple Daily, Stand
 *   News), and the mass self-dissolution of over 60 civil society
 *   organizations including unions and professional bodies rather than risk
 *   prosecution. The chilling effect operates largely without individual
 *   prosecution: the credible threat of the apparatus is sufficient to
 *   produce closure.
 *
 * KEY AGENTS:
 *   - beijing_central_authorities: sets and retains override authority over the law (institutional/analytical) — collects governance control
 *   - national_security_police_apparatus: administers day-to-day enforcement (institutional) — grows institutional footprint with each case
 *   - pro_democracy_opposition: prosecuted for ordinary political organizing (moderate/trapped) — bears imprisonment and disqualification
 *   - independent_press: forced closure via asset freezes and arrests (moderate/trapped) — loses ability to operate at all
 *   - civil_society_organizations: dissolve preemptively under chilling effect (powerless/trapped) — loses organizing capacity without individual prosecution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__democratic_enclosure_reading, 0.88).
domain_priors:suppression_score(nsl_legal_text__democratic_enclosure_reading, 0.93).
domain_priors:theater_ratio(nsl_legal_text__democratic_enclosure_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 0.93).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__democratic_enclosure_reading, snare).
narrative_ontology:human_readable(nsl_legal_text__democratic_enclosure_reading, "Hong Kong National Security Law — Democratic Enclosure Reading").
narrative_ontology:topic_domain(nsl_legal_text__democratic_enclosure_reading, "constitutional_law/political_sociology/international_relations").

domain_priors:requires_active_enforcement(nsl_legal_text__democratic_enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__democratic_enclosure_reading, '35433a96-3f5a-4d91-9686-407a356c1052').
narrative_ontology:cs_kernel_codification('35433a96-3f5a-4d91-9686-407a356c1052', formalized).
narrative_ontology:cs_authority_grounding('35433a96-3f5a-4d91-9686-407a356c1052', extraction).
narrative_ontology:cs_interpretation_layer_present('35433a96-3f5a-4d91-9686-407a356c1052').
narrative_ontology:cs_reading_relation('35433a96-3f5a-4d91-9686-407a356c1052', nsl_legal_text__sovereignty_restoration_reading, coexists_with).
narrative_ontology:cs_reading_relation('35433a96-3f5a-4d91-9686-407a356c1052', nsl_legal_text__jurisdictional_capture_reading, influences).
narrative_ontology:cs_axiom('35433a96-3f5a-4d91-9686-407a356c1052', foundational, security_framing_as_political_closure_instrument).
narrative_ontology:cs_axiom_status(security_framing_as_political_closure_instrument, holdable).
narrative_ontology:cs_axiom_grounding('35433a96-3f5a-4d91-9686-407a356c1052', security_framing_as_political_closure_instrument, empirically_contingent).
narrative_ontology:cs_axiom('35433a96-3f5a-4d91-9686-407a356c1052', foundational, electoral_and_associational_pluralism_as_precondition_for_legitimate_order).
narrative_ontology:cs_axiom_status(electoral_and_associational_pluralism_as_precondition_for_legitimate_order, holdable).
narrative_ontology:cs_axiom_grounding('35433a96-3f5a-4d91-9686-407a356c1052', electoral_and_associational_pluralism_as_precondition_for_legitimate_order, deontological).
narrative_ontology:cs_reference_frame('35433a96-3f5a-4d91-9686-407a356c1052', one_country_two_systems_pluralist_baseline).
narrative_ontology:cs_drift_state('35433a96-3f5a-4d91-9686-407a356c1052', post_2020_enforcement_era, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('35433a96-3f5a-4d91-9686-407a356c1052', '').
narrative_ontology:cs_kernel_id(nsl_legal_text__democratic_enclosure_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__democratic_enclosure_reading, beijing_central_authorities).
narrative_ontology:constraint_beneficiary(nsl_legal_text__democratic_enclosure_reading, hk_establishment_bloc).
narrative_ontology:constraint_beneficiary(nsl_legal_text__democratic_enclosure_reading, national_security_police_apparatus).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, pro_democracy_opposition).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, independent_press).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, civil_society_organizations).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, student_activists).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, trade_unionists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, hk_judiciary).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted and imposed the law directly via the National People's Congress Standing Committee, bypassing the Hong Kong legislature entirely. Retains override authority over interpretation, can claim jurisdiction over 'complex' cases, and stations a new security office in Hong Kong outside local judicial oversight. Bears no cost from the arrangement; every discretionary lever runs in its favor.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, beijing_central_authorities, agenda_setter,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__democratic_enclosure_reading, beijing_central_authorities, beneficiary).

% Pro-Beijing legislators, business elites, and appointed officials who gain a legislature and civil service cleared of opposition voices via disqualification and loyalty oaths, and a public sphere no longer contesting their governance. They face none of the law's coercive machinery and can exit to the mainland market or elsewhere at will.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, hk_establishment_bloc, beneficiary,
    powerful, generational, mobile, national).

% New specialized police unit and prosecutors' office empowered with expanded surveillance, warrantless-adjacent search powers in security cases, and the ability to freeze assets and deny bail as a near-default. Administers the law's day-to-day enforcement and grows its institutional footprint and budget with each new case.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, national_security_police_apparatus, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__democratic_enclosure_reading, national_security_police_apparatus, beneficiary).

% Elected legislators, primary-election organizers, and party figures prosecuted for 'subversion' for acts as ordinary as organizing an unofficial primary or chanting a protest slogan. Face indefinite pretrial detention, denial of bail as the default posture under the law, and trial without jury for security offenses. Exit means exile and abandonment of political life; remaining means prosecution.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, pro_democracy_opposition, payer,
    moderate, biographical, trapped, national).

% Newsrooms whose editors and executives have been arrested, whose assets have been frozen making continued publication financially impossible, and whose parent companies have been forced to liquidate. Coverage of protests or criticism of security enforcement is read as potential collusion with foreign forces. Exit means shutting down or relocating staff abroad under threat of extraterritorial pursuit.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, independent_press, payer,
    moderate, biographical, trapped, national).

% Unions, churches, professional associations, and advocacy groups have dissolved en masse rather than risk prosecution for prior advocacy now recast as subversive. Their institutional memory and organizing capacity are destroyed by self-dissolution, which the law's chilling effect achieves without requiring individual prosecution of every member.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, civil_society_organizations, payer,
    powerless, biographical, trapped, national).

% Young people who participated in 2019 protests or who continue small acts of symbolic dissent (banners, chants, social media posts) face prosecution under secession and incitement provisions applied retroactively in effect through ongoing investigation of past conduct. Many face a choice between silence, emigration, or prosecution with sentences up to life imprisonment.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, student_activists, payer,
    powerless, biographical, trapped, local).

% Organizers of the general strikes during the 2019 unrest and subsequent labor organizing are treated as coordinating collusion with foreign forces when union activity intersects with political demands. Independent unions have deregistered rather than face this exposure.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, trade_unionists, payer,
    powerless, biographical, trapped, local).

% Foreign states, international NGOs, and diaspora advocacy groups are named as potential 'external forces' whose contact with local actors can itself constitute an offense (Article 29). They are excluded from any formal role in the law's application yet are structurally implicated as the law's named threat category, which forecloses ordinary transnational civil society linkage.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, foreign_governments_and_ngos, excluded,
    powerful, biographical, constrained, global).

% Common-law judges operate within a parallel security-case track where the Chief Executive designates which judges may hear national security cases, and mainland authorities can assert jurisdiction over cases at will. Judicial independence in this domain is structurally circumscribed even where individual judges attempt principled rulings.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, hk_judiciary, observer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__democratic_enclosure_reading, hk_judiciary, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nsl_legal_text__democratic_enclosure_reading, beijing_central_authorities).
narrative_ontology:fixing_cost_class(nsl_legal_text__democratic_enclosure_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The law claims to solve a genuine coordination problem: preventing violent unrest, secessionist organizing, and foreign interference from destabilizing governance. Under this reading, that claim is a cover — the actual operative function is the systematic disqualification of electoral competition, dissolution of independent civil society, and closure of independent press, none of which required a security framework to address as ordinary public order matters.
% TRANSFER_FUNCTION: Moves political voice, associational capacity, and press freedom from the pro-democracy camp, independent media, and civil society to Beijing and the aligned HK establishment, who gain an uncontested governing field. It also moves prosecutorial and surveillance capacity permanently into a specialized apparatus with self-reinforcing institutional incentives to keep finding cases.
% ABSENT_VOICES: The disqualified legislators, detained activists, and dissolved organizations who would contest the law's necessity are, by the law's own operation, removed from the fora (legislature, press, courts of public opinion) where that contest would occur — the silencing is the mechanism, not merely a side effect the law forgot to prevent.
% DISAPPEARANCE_RATIONALE: If the law were repealed overnight, disqualified legislators would seek to re-register for office, dissolved unions and civil society groups would attempt to reconstitute, shuttered newsrooms would attempt to relaunch, and detained activists facing pending or served sentences would have a path to release or appeal — the current governing arrangement in Hong Kong depends materially on the law's continued operation to exclude these actors from public life.
% FOUNDING_PROBLEM: Officially: to prevent recurrence of the 2019 mass unrest, secessionist advocacy, and alleged foreign interference that Beijing characterized as an existential threat to sovereignty and public order.
% FOUNDING_PROBLEM_CORROBORATION: Beijing and the HK establishment attest the problem (unrest, foreign interference) remains live and requires ongoing vigilance. Independent corroboration from outside the beneficiary set is scarce by design — international human rights bodies (UN Human Rights Committee), foreign bar associations, and exiled former legislators attest instead that the disorder the law was framed around had already subsided by mid-2020 through ordinary policing and that the law's post-2020 caseload has overwhelmingly targeted peaceful political and associational activity rather than violence, supporting a 'founding problem substantially resolved, arrangement repurposed' reading; no source outside Beijing/HK establishment corroborates continued necessity at the scale the law's enforcement record shows.
narrative_ontology:disappearance_verdict(nsl_legal_text__democratic_enclosure_reading, world_rearranges).
narrative_ontology:founding_problem_status(nsl_legal_text__democratic_enclosure_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nsl_legal_text__democratic_enclosure_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nsl_legal_text__democratic_enclosure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nsl_legal_text__democratic_enclosure_reading, 0.88, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored high (0.88) because, under this reading, the arrangement transfers political voice, associational capacity, and press freedom from a broad civil society base to a narrow governing bloc with no compensating benefit to the payers. Suppression is authored even higher (0.93) because persistence depends on continuously credible coercive threat (indefinite detention, asset freezing, extraterritorial reach) rather than voluntary participation — alternatives (opposition politics, independent press, independent unions) have been affirmatively closed, not merely disfavored. Theater ratio starts moderate (0.6) reflecting genuine post-unrest security anxiety in the earliest enforcement period, then falls as the apparatus's caseload reveals itself as overwhelmingly targeting peaceful political activity rather than violence — the coordination cover thins as the extractive function becomes the visible pattern. Accessibility collapse is high (0.82): once a legislator is disqualified or a paper is shuttered, there is no legal path back within the jurisdiction. Resistance is moderate (0.55) reflecting the visible exile movement, international litigation, and diaspora advocacy, tempered against the reality that domestic resistance capacity has been substantially degraded by the law's own operation.
 *
 * PERSPECTIVAL GAP:
 *   From Beijing's seat, the arrangement is legitimate exercise of sovereign authority solving a real security problem (this is exactly the content of the sibling sovereignty_restoration_reading, generated as a separate constraint). From the payer seats authored here, the identical text operates as a closure mechanism whose coordination story does not survive contact with its actual caseload. The engine computes each seat's type from the structural data; this story's job is to state, from the enclosure reading's own lights, why the metrics land where they do — not to adjudicate between readings.
 *
 * DIRECTIONALITY LOGIC:
 *   Beijing central authorities and the national security police apparatus sit at the extreme beneficiary/agenda-setter end: they hold analytical exit (no personal exposure to the law's provisions) and institutional power, and every discretionary lever (case designation, bail default, judge assignment) runs in their favor. The HK establishment bloc benefits from a cleared political field without bearing enforcement costs and retains mobility (assets, residency options) the general population lacks. Pro-democracy opposition, independent press, and civil society sit at the extreme target end: trapped exit (leaving means exile and loss of political/professional life), and the law's elastic definitions of subversion and collusion apply disproportionately to their ordinary activities. Foreign governments and NGOs are excluded from any formal role yet are structurally named as the threat category the law polices contact with — their exclusion is not neutral but forecloses transnational civil society linkage that would otherwise support the domestic victim groups.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy question here is genealogical: was there ever a live founding problem (2019-style mass unrest) that has since become moot, with the arrangement now persisting as pure political control? The founding_problem_status is authored as contested rather than dead because Beijing and HK establishment corroborate ongoing necessity, while external corroboration (UN bodies, foreign bar associations, exiled legislators) supports a 'problem resolved by ordinary means before the law reached full force, arrangement repurposed' account. The classification does not resolve this by fiat — it is carried as an omega and in the founding_problem_corroboration field, cross-checked by the engine against the disappearance_verdict (world_rearranges) for the capture/zombie mismatch signal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection,
    'The NSL text is read as three structurally distinct constraints (democratic_enclosure, jurisdictional_capture, sovereignty_restoration) from the same kernel. Which reading a given analytical seat adopts is not determined by the text alone but by prior commitments about sovereignty, legitimacy of the 2020 imposition process, and evidentiary weight given to the 2019 unrest as ongoing threat versus resolved event.',
    'No empirical resolution mechanism fully closes this — it is a genuinely contested framing question. Partial evidence: caseload composition (violent offenses vs. peaceful political/associational activity), timing of unrest de-escalation relative to enactment, and comparative analysis of pre-2020 versus post-2020 prosecution patterns for equivalent conduct.',
    'If the caseload evidence strongly favors peaceful-activity prosecution over violence prevention, this reading''s structural claims (extraction dominant, coordination cover) are strengthened relative to sovereignty_restoration_reading. If violent/foreign-interference caseload proves substantial, the readings converge more than this story assumes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection, conceptual, 'Which of three sibling readings of the NSL kernel best fits the operative evidence is itself contested and not resolvable by this story alone.').

omega_variable(
    chilling_effect_causal_attribution,
    'How much of the mass civil-society dissolution and press closure is directly caused by the law''s operation versus a broader climate of political pressure (including extralegal pressure, funding withdrawal, and landlord/employer pressure) that predates or runs parallel to the law?',
    'Comparative timeline analysis of organizational dissolutions against specific enforcement actions (arrests, asset freezes, public statements by security officials) versus other pressure vectors; interviews with dissolved organizations'' leadership on stated reasons.',
    'If dissolutions were substantially independent of the law itself, the law''s authored extractiveness and accessibility_collapse in this story would be overstated relative to what the text and its direct enforcement produced versus a broader coercive environment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chilling_effect_causal_attribution, empirical, 'Whether the chilling effect is attributable to the NSL specifically or to a wider coercive climate the NSL is one instrument within.').

omega_variable(
    beneficiary_versus_vindicated_proposition,
    'Is ''restored sovereign order'' properly a vindicated proposition (a doctrine the arrangement''s operation supports) rather than something the HK establishment bloc ''benefits'' from as an actor collecting rents?',
    'Distinguish material benefit flows (political office, business continuity, absence of prosecution) from doctrinal vindication (the proposition that Beijing''s sovereignty claim over Hong Kong''s internal security affairs is legitimate) — the former belongs in beneficiaries, the latter would belong in vindicated_propositions if authored.',
    'This story places material actors (beijing_central_authorities, hk_establishment_bloc) in beneficiaries because they collect concrete governance and economic benefits, not merely doctrinal vindication; the sovereignty_restoration_reading sibling may authors the doctrine itself as a vindicated_proposition rather than a beneficiary, which is a structural difference between the two readings worth tracking.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_versus_vindicated_proposition, conceptual, 'Clarifying why material beneficiaries, not the sovereignty doctrine itself, are named as beneficiaries in this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__democratic_enclosure_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl__tr_t0, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 0, 0.6).
narrative_ontology:measurement(nsl__tr_t6, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 6, 0.5).
narrative_ontology:measurement(nsl__tr_t12, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 12, 0.45).
narrative_ontology:measurement(nsl__tr_t24, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 24, 0.42).
narrative_ontology:measurement(nsl__tr_t36, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 36, 0.4).
narrative_ontology:measurement(nsl__tr_t48, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 48, 0.4).
narrative_ontology:measurement(nsl__tr_t60, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 60, 0.4).

% Extraction over time
narrative_ontology:measurement(nsl__be_t0, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(nsl__be_t6, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 6, 0.68).
narrative_ontology:measurement(nsl__be_t12, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 12, 0.76).
narrative_ontology:measurement(nsl__be_t24, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 24, 0.82).
narrative_ontology:measurement(nsl__be_t36, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 36, 0.85).
narrative_ontology:measurement(nsl__be_t48, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 48, 0.87).
narrative_ontology:measurement(nsl__be_t60, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 60, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(nsl__su_t0, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(nsl__su_t6, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 6, 0.83).
narrative_ontology:measurement(nsl__su_t12, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 12, 0.88).
narrative_ontology:measurement(nsl__su_t24, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 24, 0.91).
narrative_ontology:measurement(nsl__su_t36, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 36, 0.92).
narrative_ontology:measurement(nsl__su_t48, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 48, 0.93).
narrative_ontology:measurement(nsl__su_t60, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 60, 0.93).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_legal_text__democratic_enclosure_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(nsl_legal_text__democratic_enclosure_reading, 0.1).
narrative_ontology:affects_constraint(nsl_legal_text__democratic_enclosure_reading, nsl_legal_text__sovereignty_restoration_reading).
narrative_ontology:affects_constraint(nsl_legal_text__democratic_enclosure_reading, nsl_legal_text__jurisdictional_capture_reading).
narrative_ontology:affects_constraint(nsl_legal_text__democratic_enclosure_reading, hk_basic_law_interpretation_authority).
narrative_ontology:affects_constraint(nsl_legal_text__democratic_enclosure_reading, hk_judicial_independence_common_law_autonomy).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposed from the single NSL legal text kernel (nsl_legal_text), per the ε-invariance principle: the same text produces materially different ε and beneficiary/victim structures depending on which reading is applied, so it cannot be represented as one constraint with a measurement parameter. democratic_enclosure_reading (this story): ε=0.88, victims=civil society/press/opposition, claimed_type=snare. jurisdictional_capture_reading (sibling): centers legal-system erosion, victims=legal profession/judiciary, different ε and beneficiary structure. sovereignty_restoration_reading (sibling): ε assessed low from that reading's own lights, treats the arrangement as legitimate coordination restoring order, beneficiaries framed as the public order function itself. All three link to each other and to the upstream hk_basic_law_interpretation_authority constraint which the NSL's imposition mechanism structurally depends on.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nsl_legal_text__democratic_enclosure_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
