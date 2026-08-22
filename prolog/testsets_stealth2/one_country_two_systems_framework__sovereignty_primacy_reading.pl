% ============================================================================
% CONSTRAINT STORY: one_country_two_systems_framework__sovereignty_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_one_country_two_systems_framework__sovereignty_primacy_reading, []).

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
 *   constraint_id: one_country_two_systems_framework__sovereignty_primacy_reading
 *   human_readable: One Country Two Systems - Sovereignty Primacy Reading (NSL-era Override)
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   This story instantiates the sovereignty_primacy_reading of the
 *   one_country_two_systems_framework kernel: the Basic Law and Joint
 *   Declaration settlement read as Hong Kong autonomy delegated by, and
 *   revocable through, PRC sovereign authority, with national security and
 *   territorial integrity overriding local autonomy on conflict. The standing
 *   arrangement under contest - the epsilon referent, assessed by this
 *   reading's own lights - is the post-2020 sovereignty-primacy operation:
 *   the National Security Law enacted by NPC decision without a local vote,
 *   mainland security organs operating inside Hong Kong, a patriots-only
 *   electoral redesign, and a judiciary trying security cases without juries
 *   under designated-judge panels. The expected structural delta for this
 *   reading (state coercion entering the HK legal system, mainland
 *   enforcement presence, political speech and assembly heavily constrained,
 *   judicial independence curtailed on security dockets) is the arrangement's
 *   observed operation since 2020. Under the epsilon-invariance rule this is
 *   one reading of the kernel, not the kernel itself: the autonomy-primacy
 *   and balanced-coexistence siblings are separate constraints with their own
 *   epsilon values and victim sets, linked through
 *   network.affects_constraints. The claimed type and the metrics are
 *   authored independently: the reading frames the arrangement as legitimate
 *   sovereign coordination, while the metrics record the asymmetric transfer
 *   it actually operates.
 *
 * KEY AGENTS:
 *   - prc_central_authorities: agenda-setter and primary beneficiary (institutional/arbitrage) - holds final interpretive authority, enacts the override, collects jurisdictional supremacy
 *   - prc_national_security_apparatus: beneficiary (institutional/arbitrage) - collects enforcement jurisdiction inside Hong Kong
 *   - hk_pro_beijing_establishment: beneficiary and local administrator (organized/identity_locked) - holds office under patriots-only screening, staffs the arrangement
 *   - hk_pro_democracy_politicians: primary target (organized/trapped) - disqualified, prosecuted, exiled; organizational base dismantled
 *   - hk_political_speech_community: diffuse target (powerless/constrained) - bears the speech and assembly constraint; exit is emigration
 *   - hk_independent_press: target (moderate/trapped) - largest outlet closed by force, editors jailed, remaining newsrooms self-censor
 *   - hk_civil_society_organizations: target (moderate/trapped) - associations dissolved under security-law exposure
 *   - hk_judiciary: administering seat paying in independence (institutional/constrained) - sits on designated security panels under override power
 *   - uk_and_international_signatories: excluded co-signatory (institutional/constrained) - asserts treaty standing the arrangement does not recognize
 *   - comparative_constitutional_scholars: analytical observer (analytical/analytical) - sees the full reading structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__sovereignty_primacy_reading, 0.65).
domain_priors:suppression_score(one_country_two_systems_framework__sovereignty_primacy_reading, 0.78).
domain_priors:theater_ratio(one_country_two_systems_framework__sovereignty_primacy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__sovereignty_primacy_reading, tangled_rope).
narrative_ontology:human_readable(one_country_two_systems_framework__sovereignty_primacy_reading, "One Country Two Systems - Sovereignty Primacy Reading (NSL-era Override)").
narrative_ontology:topic_domain(one_country_two_systems_framework__sovereignty_primacy_reading, "constitutional/political").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__sovereignty_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__sovereignty_primacy_reading, '6b433a8a-a10f-42c3-8c70-abb50160ff59').
narrative_ontology:cs_kernel_codification('6b433a8a-a10f-42c3-8c70-abb50160ff59', fixed_text).
narrative_ontology:cs_authority_grounding('6b433a8a-a10f-42c3-8c70-abb50160ff59', extraction).
narrative_ontology:cs_interpretation_layer_present('6b433a8a-a10f-42c3-8c70-abb50160ff59').
narrative_ontology:cs_reading_relation('6b433a8a-a10f-42c3-8c70-abb50160ff59', one_country_two_systems_framework__autonomy_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('6b433a8a-a10f-42c3-8c70-abb50160ff59', one_country_two_systems_framework__balanced_coexistence_reading, influences).
narrative_ontology:cs_axiom('6b433a8a-a10f-42c3-8c70-abb50160ff59', foundational, autonomy_delegated_and_revocable_by_sovereign).
narrative_ontology:cs_axiom_status(autonomy_delegated_and_revocable_by_sovereign, holdable).
narrative_ontology:cs_axiom_grounding('6b433a8a-a10f-42c3-8c70-abb50160ff59', autonomy_delegated_and_revocable_by_sovereign, conventional).
narrative_ontology:cs_axiom('6b433a8a-a10f-42c3-8c70-abb50160ff59', foundational, national_security_overrides_local_autonomy).
narrative_ontology:cs_axiom_status(national_security_overrides_local_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('6b433a8a-a10f-42c3-8c70-abb50160ff59', national_security_overrides_local_autonomy, instrumental).
narrative_ontology:cs_reference_frame('6b433a8a-a10f-42c3-8c70-abb50160ff59', sovereign_delegated_autonomy_framework).
narrative_ontology:cs_drift_state('6b433a8a-a10f-42c3-8c70-abb50160ff59', post_nsl_consolidation_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('6b433a8a-a10f-42c3-8c70-abb50160ff59', '').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__sovereignty_primacy_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, prc_central_authorities).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, prc_national_security_apparatus).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, hk_pro_beijing_establishment).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hk_pro_democracy_politicians).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hk_political_speech_community).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hk_independent_press).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hk_civil_society_organizations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hk_judiciary).
narrative_ontology:constraint_vindicates(one_country_two_systems_framework__sovereignty_primacy_reading, sovereign_supremacy_doctrine).
narrative_ontology:constraint_vindicates(one_country_two_systems_framework__sovereignty_primacy_reading, comprehensive_jurisdiction_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds final interpretive authority over Hong Kong's constitutional text through the NPC Standing Committee, enacted the National Security Law by NPC decision without a local vote, and defines what counts as national security. When it judges local autonomy to conflict with sovereignty or territorial integrity, it overrides. It gains final decision authority and a working precedent for further territorial integration; its exit is unrestricted because it writes the rules it operates under.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, prc_central_authorities, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Gained a new enforcement jurisdiction inside Hong Kong under the National Security Law: an office in the city with evidence-gathering and case-taking powers, jurisdiction over specified serious cases, and an expanded mandate and staffing. It neither set the framework nor bears its costs; it collects enforcement domain.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, prc_national_security_apparatus, beneficiary,
    institutional, generational, arbitrage, continental).

% The loyalist political and business network that staffs the chief executive's office, the reformed election committee, and the legislature. Under patriots-only screening it holds political office without competition from the opposition, and it administers the arrangement locally on the center's behalf. Its position is constituted by loyalty to the framework; abandoning it would mean political extinction.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hk_pro_beijing_establishment, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__sovereignty_primacy_reading, hk_pro_beijing_establishment, agenda_setter).

% The organized opposition: legislators disqualified en masse after the 2020 oath controversy, party leaders arrested under the National Security Law, primary organizers convicted of subversion, and exiled figures facing warrants. What remains of the class inside the city cannot contest elections under the redesigned rules; leaving means forfeiting home, and staying means prosecution risk. Its organizational base inside the city has been dismantled.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hk_pro_democracy_politicians, payer,
    organized, biographical, trapped, national).

% Ordinary residents, activists, online speakers, and protesters who bear the speech and assembly constraint directly: slogans and posts have been prosecuted, vigils banned, unions and associations dissolved. Individually each has little power; the practical exit is emigration, taken by large numbers at the cost of family, property, and livelihood.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hk_political_speech_community, payer,
    powerless, biographical, constrained, national).

% The city's independent outlets: the largest was closed by force and its editors jailed under the National Security Law; remaining newsrooms operate under arrest risk and self-censor. Exit is closure or relocation abroad; the enforcement machinery is aimed squarely at this seat.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hk_independent_press, payer,
    moderate, immediate, trapped, national).

% Associations, unions, and NGOs that dissolved themselves under National Security Law exposure after their leaders were arrested. The civil society infrastructure that organized protest support, election monitoring, and mutual aid has largely been dismantled.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hk_civil_society_organizations, payer,
    moderate, biographical, trapped, national).

% Administers the arrangement locally: judges sit on designated national security panels, deny bail under a reversed presumption, and try serious cases without juries, while the Standing Committee holds power to override interpretations. The bench retains its common-law forms, salaries, and international standing, and pays for that retention with curtailed independence on the defining docket; some foreign judges have resigned rather than sit. Leaving the bench is the only exit short of administering outcomes it does not control.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hk_judiciary, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__sovereignty_primacy_reading, hk_judiciary, payer).

% The co-signatory of the Joint Declaration and the foreign governments that lodged protests, offered visa schemes, and imposed sanctions. Under the arrangement's operative logic they hold no standing: the center classifies the declaration as a historical document exhausted at handover and treats their objections as external interference. They would object from inside if the conversation admitted them.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, uk_and_international_signatories, excluded,
    institutional, generational, constrained, global).

% Constitutional lawyers and political scientists who track the framework's competing readings, the Standing Committee's interpretation practice, and the treaty's status. They hold no stake in the arrangement's operation and see the full reading structure from outside.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, comparative_constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(one_country_two_systems_framework__sovereignty_primacy_reading, prc_central_authorities).
narrative_ontology:fixing_cost_class(one_country_two_systems_framework__sovereignty_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single sovereign state encompassing two economic-legal systems: unified diplomacy, defense, and territorial boundary, while preserving Hong Kong's common-law market system. The sovereignty-primacy reading resolves the design's central ambiguity - who decides when the systems conflict - by locating final authority in the center.
% TRANSFER_FUNCTION: Moves final decision authority over Hong Kong governance from local institutions to the PRC center whenever national security is invoked; moves political liberties (speech, assembly, press, electoral participation) from Hong Kong residents into state-security control; moves enforcement jurisdiction to mainland-linked organs operating inside the city.
% ABSENT_VOICES: Disqualified legislators, imprisoned opposition leaders, dissolved civil society organizations, closed newsrooms, and the Joint Declaration's co-signatory would all object to the sovereignty-primacy operation; none holds standing in the operative conversation. The electoral redesign guarantees that only screened candidates hold office, and the center classifies external objection as interference - the unanimity of the arrangement's internal conversation reflects who was removed from it.
% DISAPPEARANCE_RATIONALE: Overnight removal of the override machinery - the National Security Law, final Standing Committee interpretation, patriots-only screening - would collapse pending national-security prosecutions, reopen who may stand for office, allow dissolved organizations and closed outlets to re-form, and strip the center of its working demonstration case for further territorial integration. The world rearranges around the constraint's removal because active enforcement, not inertia, holds the current configuration in place.
% FOUNDING_PROBLEM: The 1980s reintegration problem: how a socialist sovereign state could resume jurisdiction over a capitalist enclave acquired by treaty without destroying its economic function or triggering mass exit. The negotiated answer deliberately left unresolved who decides when the two systems conflict; the sovereignty-primacy reading is the center's answer to that deliberately deferred question.
% FOUNDING_PROBLEM_CORROBORATION: The Joint Declaration text and the Basic Law's autonomy guarantees, with their drafting records, corroborate that substantive, guaranteed autonomy was central to the founding terms; the UK government, UN treaty-body reviews, and international legal commentary - all outside the benefiting parties - attest that the current override departs from those terms. The PRC government, a benefiting party, attests the opposite. Corroboration splits along the beneficiary/victim line: no source outside the benefiting parties attests that the founding problem required the post-2020 override machinery, which is itself signal about the reading's provenance.
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__sovereignty_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__sovereignty_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__sovereignty_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(one_country_two_systems_framework__sovereignty_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(one_country_two_systems_framework__sovereignty_primacy_reading, 0.65, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(one_country_two_systems_framework__sovereignty_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(one_country_two_systems_framework__sovereignty_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(one_country_two_systems_framework__sovereignty_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The interval maps 0 = 1997 (handover) to 27 = 2024 (local Article 23 enactment), with all three tracked series authored on one shared grid. Extractiveness (0.65 at interval end) is reading-indexed: this reading deems the security override a legitimate exercise of reserved sovereignty, which damps its extraction assessment relative to hostile sibling readings over the same referent, but it cannot deny the transfer itself - identifiable groups lose liberty and jurisdiction moves to the center - so the value stays substantial. Suppression (0.78) records the structural enforcement machinery: arrests, no-jury designated panels, a reversed bail presumption, organization dissolutions, newsroom closures. Theater (0.42) records the growing share of performative maintenance: common-law forms retained around pre-determined security outcomes, electoral ritual without contest, oath and patriotic-education ceremony. Accessibility collapse (0.62): opposition alternatives have largely collapsed inside the city, but emigration exit and residual formal channels keep alternatives from full collapse. Resistance (0.55): the 2019 wave was the largest in the arrangement's life; current resistance is diaspora and diplomatic. Claim and metrics are independent: the tangled_rope claim states what the structure is (genuine one-country coordination plus asymmetric extraction, actively enforced); the metrics state how it operates. Receipt surface: the extraction demonstrably accrues to prc_central_authorities (gain_flow), which also faces prohibitive fixing costs - retrenchment would carry existential legitimacy costs for the only seat with power to fix it (fixing_cost) - so the receipt and cost cells point the same direction.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the center's seat the arrangement is a coordination framework it built and reserves final authority over - coordination-like. From the opposition, press, and civil society seats the same structure operates as enforced removal of their liberty - extraction-like. The judiciary's seat is the sharpest divergence: it administers the arrangement (agenda-setter position) while paying for its retention in curtailed independence on the defining docket. The pro-Beijing establishment benefits but is identity-locked into the framework it staffs. The engine computes these per-seat classifications from the structural data; the divergence between the center's experience and the constrained seats' experience is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: the center holds the override and collects jurisdictional supremacy (d near the beneficiary end, reinforced by arbitrage-grade exit); the security apparatus collects enforcement domain with arbitrage exit; the pro-Beijing establishment collects office and political monopoly but is identity-locked, so its cushion is thinner than its role suggests. Targets: opposition politicians (trapped), the speech community (constrained - emigration is real but costly), the press (trapped), and civil society organizations (trapped) all sit near the full-target end. The judiciary is dual-positioned: administering the arrangement pulls its derived directionality toward the agenda side while its independence loss pushes toward target; it should compute mid-range. The excluded co-signatory holds no operative directionality - the arrangement does not register it. gain_flow names the center because the gains (final authority, jurisdiction) demonstrably land there; the apparatus and establishment receive shares, but the seat the extraction accrues to is the center.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - peaceful reintegration of the enclave - was solved at handover; what persists is override machinery whose justification has migrated from reintegration to security management and precedent-maintenance for further territorial integration. Status is contested rather than plainly dead because the center genuinely asserts live security grounds. The classification prevents mislabeling in both directions: reading the arrangement as the center frames it (pure coordination) erases the identifiable victims whose liberty the machinery removes; reading it as pure extraction erases the coordination function (single sovereignty, unified external representation) that even most of the constrained do not contest. The tangled_rope claim holds both components; the founding-problem mismatch (status contested, verdict world_rearranges) flags the zombie risk - machinery outliving the problem it was built for.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment_structure,
    'This constraint is one reading of the one_country_two_systems_framework kernel - what would the sibling readings change structurally, and where exactly is the disagreement located?',
    'Comparative authoring of the sibling readings (autonomy_primacy_reading, balanced_coexistence_reading) over the same referent. The disagreement locates in one structural element: whether the autonomy grants are revocable sovereign delegations or enforceable guarantees.',
    'The autonomy-primacy sibling would author substantially higher extraction over the same referent and a different victim set (all residents under mainland-jurisdiction exposure rather than the politically active); the balanced-coexistence sibling would deny any seat final authority and classify the override as a boundary violation rather than an exercise of reserved power.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment_structure, conceptual, 'Committer structure: which kernel reading this is and what siblings would change.').

omega_variable(
    joint_declaration_enforceability,
    'Is the Sino-British Joint Declaration a continuing treaty obligation whose autonomy guarantees bind the center, or a historical statement of policy exhausted at handover?',
    'State practice and international legal analysis: how comparable framework treaties governing territorial handovers are treated, and whether any forum will hear the question.',
    'If enforceable, the sovereignty-primacy override is a breach and the autonomy reading''s checks are live - the arrangement''s legitimacy floor collapses; if not, the reading''s conventional grounding stands and the override is intra-sovereign.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(joint_declaration_enforceability, empirical, 'Legal status of the treaty guarantee the override operates against.').

omega_variable(
    national_security_threat_proportionality,
    'Does the security threat the override responds to (2019 unrest, secession advocacy) plausibly require the breadth of the machinery built - prosecution of slogans and posts, no-jury trials, extraterritorial reach - or does national security function as an all-purpose override?',
    'Compare the documented threat profile to the offense definitions and enforcement pattern: the share of prosecutions involving any violent act, conviction rates for nonviolent speech, and the volume of extraterritorial warrants.',
    'If the machinery is broader than any plausible threat, the extraction component dominates the coordination component and the reading''s own instrumental justification fails on its own terms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(national_security_threat_proportionality, empirical, 'Whether the security override tracks a genuine threat or is an unbounded override.').

omega_variable(
    residual_judicial_independence,
    'How much judicial independence survives on national-security dockets, given the reversed bail presumption, no juries, designated judge lists, and Standing Committee override power?',
    'Track national-security case outcomes: conviction rates, bail denial rates, acquittal frequency, appeal success, and the foreign-judge resignation pattern.',
    'If outcomes are effectively pre-determined, the judiciary seat computes as theatrical maintenance of common-law forms and the theater ratio is understated; if genuine adjudication survives, the judiciary''s mid-range structural position holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_judicial_independence, empirical, 'Whether the courts adjudicate or ratify on security matters.').

omega_variable(
    authority_grounding_framing_ambiguity,
    'Is the center''s authority under this reading grounded in lineage (continuous sovereign claim resumed at handover) or in extraction (benefit from monopolizing interpretation and preventing kernel revision)?',
    'Examine the center''s own legitimacy discourse: whether it argues from continuity of sovereign title or from the necessity of interpretive control; test whether it would accept any interpretation mechanism it did not control.',
    'A lineage framing would make the authority claim one of conventional continuity rather than benefit-driven control, softening the extraction-grounding classification; the extraction framing is what the structural record - interpretation monopoly plus enforcement of the monopoly - actually shows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_framing_ambiguity, conceptual, 'CS-framing under-determination: lineage versus extraction as the authority''s ground.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__sovereignty_primacy_reading, 0, 27).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(oc2s_sov_primacy_tr_t0, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(oc2s_sov_primacy_tr_t6, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 6, 0.1).
narrative_ontology:measurement(oc2s_sov_primacy_tr_t12, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 12, 0.12).
narrative_ontology:measurement(oc2s_sov_primacy_tr_t17, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 17, 0.18).
narrative_ontology:measurement(oc2s_sov_primacy_tr_t22, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 22, 0.24).
narrative_ontology:measurement(oc2s_sov_primacy_tr_t24, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement(oc2s_sov_primacy_tr_t27, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 27, 0.42).

% Extraction over time
narrative_ontology:measurement(oc2s_sov_primacy_be_t0, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(oc2s_sov_primacy_be_t6, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 6, 0.26).
narrative_ontology:measurement(oc2s_sov_primacy_be_t12, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 12, 0.28).
narrative_ontology:measurement(oc2s_sov_primacy_be_t17, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 17, 0.36).
narrative_ontology:measurement(oc2s_sov_primacy_be_t22, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 22, 0.44).
narrative_ontology:measurement(oc2s_sov_primacy_be_t24, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement(oc2s_sov_primacy_be_t27, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 27, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(oc2s_sov_primacy_su_t0, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(oc2s_sov_primacy_su_t6, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 6, 0.24).
narrative_ontology:measurement(oc2s_sov_primacy_su_t12, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 12, 0.22).
narrative_ontology:measurement(oc2s_sov_primacy_su_t17, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 17, 0.34).
narrative_ontology:measurement(oc2s_sov_primacy_su_t22, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 22, 0.58).
narrative_ontology:measurement(oc2s_sov_primacy_su_t24, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 24, 0.82).
narrative_ontology:measurement(oc2s_sov_primacy_su_t27, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 27, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__sovereignty_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, one_country_two_systems_framework__autonomy_primacy_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, one_country_two_systems_framework__balanced_coexistence_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'One Country, Two Systems' covers three structurally distinct constraints - one per reading of the shared kernel. This story is the sovereignty-primacy reading: delegated, revocable autonomy with the override as reserved power; its epsilon is authored from the reading's own lights over the post-2020 arrangement. The autonomy-primacy sibling treats the same arrangement as treaty breach and authors far higher epsilon with a broader victim set; the balanced-coexistence sibling denies finality to any seat and classifies the override as a boundary violation. In influence terms this reading is upstream: its enforcement (NSL, Standing Committee interpretations, patriots-only screening) squeezes the balanced reading's accommodation space and, within any single constitutional framework, rules out the autonomy reading's enforceable-checks premise, since a framework cannot both bind the center and leave the center's override revocable at its own discretion. All three stories link via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
