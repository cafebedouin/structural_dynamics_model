% ============================================================================
% CONSTRAINT STORY: one_country_two_systems_framework__sovereignty_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: One Country Two Systems (Sovereignty Primacy Reading) — National Security Supremacy Framework
 *   domain: constitutional_law/state_sovereignty/political_systems
 *
 * SUMMARY:
 *   The One Country Two Systems framework (signed 1984, implemented 1997)
 *   promised Hong Kong a high degree of autonomy with independent judiciary,
 *   protected civil liberties, and separate legal system. The
 *   sovereignty_primacy_reading instantiates one interpretation of that
 *   framework: PRC sovereignty is the supreme principle; autonomy is
 *   delegated and revocable; when national security and territorial integrity
 *   conflict with local autonomy, the mainland legal order and authority
 *   prevail. This reading was institutionalized through the 2020 National
 *   Security Law, which introduced mainland security apparatus and expanded
 *   definitions of subversion into Hong Kong. From this reading's frame, the
 *   constraint solves a fundamental ambiguity — it clarifies what One Country
 *   Two Systems means when the two systems come into irreconcilable conflict.
 *   The competing readings (autonomy_primacy_reading and
 *   balanced_coexistence_reading) reject this interpretation: they hold the
 *   framework promised either substantive protected autonomy
 *   (internationalized, legally entrenched) or negotiated coexistence without
 *   supremacy hierarchy. This story authors the sovereignty_primacy_reading
 *   as a clean ε-invariant constraint; its high extractiveness and
 *   suppression reflect the reading's structural consequences, not a judgment
 *   on its legitimacy.
 *
 * KEY AGENTS:
 *   - PRC Central Authority (Beijing): Sets doctrine; enforces via National Security Law; operates security apparatus in HK; defines threats and sovereignty scope
 *   - Hong Kong Judiciary: Once the guardian of local autonomy; now subordinated on national security matters; subject to removal threats for unfavorable rulings
 *   - Hong Kong Civil Society & Opposition: Primary targets of enforcement; face arrest and prosecution under expanded sedition/subversion definitions; constrained or identity-locked exit
 *   - Mainland Security Apparatus: Direct enforcement arm; operates in HK with exemption from local law; defines and prosecutes threats
 *   - Hong Kong Business Community: Beneficiaries of stability and reduced political friction; pay compliance costs; maintain highest exit options
 *   - International Observers: Document drift; lack enforcement capacity beyond diplomatic/sanctions pressure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__sovereignty_primacy_reading, 0.81).
domain_priors:suppression_score(one_country_two_systems_framework__sovereignty_primacy_reading, 0.88).
domain_priors:theater_ratio(one_country_two_systems_framework__sovereignty_primacy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__sovereignty_primacy_reading, tangled_rope).
narrative_ontology:human_readable(one_country_two_systems_framework__sovereignty_primacy_reading, "One Country Two Systems (Sovereignty Primacy Reading) — National Security Supremacy Framework").
narrative_ontology:topic_domain(one_country_two_systems_framework__sovereignty_primacy_reading, "constitutional_law/state_sovereignty/political_systems").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__sovereignty_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__sovereignty_primacy_reading, 'a8de1456-a23a-43ff-bed2-1104d0100f74').
narrative_ontology:cs_kernel_codification('a8de1456-a23a-43ff-bed2-1104d0100f74', fixed_text).
narrative_ontology:cs_authority_grounding('a8de1456-a23a-43ff-bed2-1104d0100f74', extraction).
narrative_ontology:cs_interpretation_layer_present('a8de1456-a23a-43ff-bed2-1104d0100f74').
narrative_ontology:cs_reading_relation('a8de1456-a23a-43ff-bed2-1104d0100f74', one_country_two_systems_framework__autonomy_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('a8de1456-a23a-43ff-bed2-1104d0100f74', one_country_two_systems_framework__balanced_coexistence_reading, forecloses).
narrative_ontology:cs_axiom('a8de1456-a23a-43ff-bed2-1104d0100f74', foundational, prc_sovereignty_supreme).
narrative_ontology:cs_axiom_status(prc_sovereignty_supreme, holdable).
narrative_ontology:cs_axiom_grounding('a8de1456-a23a-43ff-bed2-1104d0100f74', prc_sovereignty_supreme, deontological).
narrative_ontology:cs_axiom('a8de1456-a23a-43ff-bed2-1104d0100f74', foundational, autonomy_is_delegated_revocable).
narrative_ontology:cs_axiom_status(autonomy_is_delegated_revocable, holdable).
narrative_ontology:cs_axiom_grounding('a8de1456-a23a-43ff-bed2-1104d0100f74', autonomy_is_delegated_revocable, deontological).
narrative_ontology:cs_reference_frame('a8de1456-a23a-43ff-bed2-1104d0100f74', prc_sovereign_supremacy_doctrine).
narrative_ontology:cs_drift_state('a8de1456-a23a-43ff-bed2-1104d0100f74', post_national_security_law_institutionalization, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('a8de1456-a23a-43ff-bed2-1104d0100f74', '').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__sovereignty_primacy_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, prc_central_authority).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_civil_society).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_judiciary).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, political_opposition_parties).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_business_community).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, mainland_security_apparatus).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_ordinary_citizens).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_business_community).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_ordinary_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The PRC central government (National People's Congress Standing Committee, State Council, and central security apparatus) sets the framework's interpretation, determines what constitutes national security threats, and enforces the doctrine. Operates the National Security Law as the key instrument of this reading. Defines Hong Kong's autonomy as delegated and revocable. Maintains direct enforcement agents (Ministry of State Security, Public Security Bureau) operating in Hong Kong outside local legal constraints.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, prc_central_authority, agenda_setter,
    institutional, civilizational, analytical, national).

% Hong Kong's court system, historically independent and guardian of common-law rights, now operates under the National Security Law with explicit carve-outs from independent review. Judges handling national security cases face threat of removal by mainland authorities; the Court of Final Appeal has lost jurisdiction over cases the mainland considers national security matters. The judiciary retains local authority in non-security cases but has become subordinated to mainland doctrine on the largest domain the constraint governs.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_judiciary, payer,
    powerful, biographical, constrained, local).

% NGOs, unions, professional associations, and community organizations operate under the National Security Law with expanded definitions of subversion, sedition, and endangering national security. Mainland enforcement agents conduct operations in Hong Kong; civil society actors face arrest, prosecution, and imprisonment. The constraint narrows the legal space for assembly, advocacy, and organizing. Many organizations have dissolved or relocated; those remaining operate with pre-emptive self-censoring.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_civil_society, payer,
    organized, biographical, constrained, local).

% Democratic opposition parties, pro-independence groups, and localist movements are primary targets of the National Security Law. Founding members imprisoned; candidates disqualified from elections; party platforms declared subversive. Exit means abandoning political identity and commitment; staying means operating under prosecution and removal threat. The constraint has effectively eliminated opposition electoral politics in Hong Kong.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, political_opposition_parties, payer,
    moderate, biographical, identity_locked, local).

% Large Hong Kong conglomerates (finance, real estate, trading, shipping) benefit from the stability and predictability the constraint provides; mainland integration reduces political friction. They pay compliance costs and adapt business models to mainland security requirements. Their highest exit options (arbitrage) mean most have hedged through Shanghai operations, offshore registration, and capital diversification. This group experiences the constraint as manageable rebalancing rather than trap.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_business_community, beneficiary,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_business_community, payer).

% PRC Ministry of State Security, Public Security Bureau, and armed police units operate directly in Hong Kong under the National Security Law. They conduct investigations, arrests, and prosecutions with exemption from Hong Kong legal oversight. They benefit from expanded territorial authority, elimination of a potential governance challenge, and the integration of Hong Kong into the mainland security perimeter.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, mainland_security_apparatus, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__sovereignty_primacy_reading, mainland_security_apparatus, beneficiary).

% General population benefits from order, economic continuity, and absence of protest disruption. Also faces narrowed domain of permissible speech, political assembly, and press freedom. Exit (emigration) requires significant capital and professional credentials; over 1 million have left since 2020; many remain trapped by family ties, property ownership, professional licenses, or dependence on Hong Kong employment.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_ordinary_citizens, beneficiary,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_ordinary_citizens, payer).

% US, UK, EU, Canada, Australia, Japan, and international human rights organizations document the constraint's operation, produce reports, issue diplomatic protests, and impose targeted sanctions. Their enforcement capacity is limited to economic and diplomatic pressure; the reading's architecture is designed to absorb such pressure. Their role is observational and performative; they lack institutional leverage over the constraint's operation.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, international_governments_and_ngos, observer,
    institutional, generational, analytical, global).

% The UK government, as co-signatory of the Joint Declaration, is formally excluded from adjudicating the meaning of the agreement. The PRC has explicitly rejected UK input, declaring the agreement an historical document with no ongoing treaty force. The UK retains formal legal standing but no institutional mechanism to enforce it; the exclusion is structural and permanent.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, british_government_historical, excluded,
    institutional, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(one_country_two_systems_framework__sovereignty_primacy_reading, prc_central_authority).
narrative_ontology:fixing_cost_class(one_country_two_systems_framework__sovereignty_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes unified framework that resolves the structural ambiguity in One Country Two Systems: when does Hong Kong autonomy apply and when does PRC sovereignty override? By declaring sovereignty supremacy and national security as the override criterion, the reading provides clarity and eliminates a source of institutional conflict between Beijing and Hong Kong governance. The coordination solves the problem of what happens when the two systems come into irreconcilable conflict.
% TRANSFER_FUNCTION: Transfers political authority, enforcement discretion, and legal scope from Hong Kong institutions (judiciary, legislature, civil service) to mainland authorities (National People's Congress Standing Committee, State Council, security apparatus). Transfers prosecution power over political speech and assembly from local courts to mainland security apparatus operating under mainland law. Transfers the definition of national security threats from local legal tradition to mainland doctrine. Transfers citizens' legal protections from local courts to mainland enforcement procedures.
% ABSENT_VOICES: International signatories to the Joint Declaration (UK, UN observers) are formally excluded. Hong Kong citizens who favor substantive autonomy are not absent — they are present and defeated; they constitute the opposition and civil society stakeholders with `excluded` role. The absent voices are the legitimacy frameworks those citizens appeal to: international law, treaty enforcement, constitutional protection of local autonomy. Those frameworks are excluded by this reading's assertion that they have no standing over PRC territorial matters.
% DISAPPEARANCE_RATIONALE: If the sovereignty supremacy reading and its enforcement apparatus vanished overnight, Hong Kong would revert to pre-2020 constitutional arrangement: judges would recover independence on all cases, opposition parties would resume electoral politics, civil society would return to protected assembly and speech, mainland security agents would withdraw, and local law would govern local matters. The constraint is not a natural feature of political reality — it is an actively maintained architecture that depends on continuous mainland enforcement and local subordination to persist.
% FOUNDING_PROBLEM: The need to clarify and institutionalize PRC sovereign control over Hong Kong, resolving the ambiguity about whether One Country Two Systems meant substantive local autonomy or delegated, revocable authority. This reading answers: it always meant delegated and revocable authority.
% FOUNDING_PROBLEM_CORROBORATION: The PRC central government, Hong Kong security establishment, and pro-Beijing Hong Kong officials attest the founding problem is live and the constraint solves it. The international community (US, UK, EU, UN bodies, International Court), Hong Kong opposition parties and civil society, and constitutional scholars from non-party jurisdictions contest the reading: they attest that the Joint Declaration solved the founding problem by establishing substantive protected autonomy, and that this constraint violates rather than clarifies that solution. No neutral corroboration exists; the very nature of the reading — its legitimacy grounding — is the disputed matter.
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__sovereignty_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__sovereignty_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__sovereignty_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(one_country_two_systems_framework__sovereignty_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(one_country_two_systems_framework__sovereignty_primacy_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.81 at endpoint) and rising because the constraint transfers political authority, prosecution power, and legal scope from local to mainland institutions, and defines the transfer as irreversible. Suppression is highest metric (0.88) because persistence depends on active enforcement: mainland agents conducting operations, judges constrained by removal threats, opposition prosecuted, civil society reorganized toward compliance. Theater ratio (0.42) is moderate-rising: the National Security Law carries a genuine security function (counterterrorism, prevention of separatism), but a growing share of enforcement targets political speech and judicial independence rather than criminal threats. The measurement series track extractiveness rising as enforcement machinery hardened (2020–2023), suppression rising as courts accepted mainland directives, and theater rising as security justifications became the cover story for political control. One shared time grid across all three metrics ensures alignment; every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The PRC central authority and HK judiciary compute this constraint entirely differently. From Beijing's position, the reading clarifies a constitutional principle (supremacy of national sovereignty) that was always latent in the framework; the constraint is coordination that eliminates ambiguity. From the HK judiciary's position, the constraint is coercive restructuring — judges lose independence on the largest domain of cases, operate under threat of removal, and become organs of mainland policy. Ordinary HK citizens sit near payers (constrained speech, organized opposition becomes illegal) but with some benefit from stability. Business community straddles beneficiary-payer: stability is real, compliance costs are real, but their exit options mean they experience the constraint as a trade-off they can arbitrage, not a trap. The engine computes this divergence from power atoms and exit options: institutional actors with analytical exit (observers, security apparatus) compute differently from biographical-horizon citizens with constrained exit.
 *
 * DIRECTIONALITY LOGIC:
 *   PRC Central Authority is the structural beneficiary (d ≈ 0.1): the constraint transfers authority to them, vindicates their sovereignty doctrine, removes a source of challenge. Hong Kong civil society and opposition parties are targets (d ≈ 0.9): the constraint narrows their legal space, prosecutes their organizing, subordinates their courts. HK judiciary is a complex case (d ≈ 0.6): they retain local jurisdiction and prestige (modest beneficiary component) but lose independence and face removal threats (strong target component); power atom institutional + exit constrained + secondary role payer makes d near-target but pulled up by the residual authority they retain. HK business community (d ≈ 0.3): powerful + arbitrage exit + secondary beneficiary role makes them low-target; they can hedge through capital flight and mainland relocation. International observers (d ≈ 0.5, near symmetric): they have analytical exit and institutional power but no decision-making role in the constraint's operation; cost to them is reputational/political, benefit is... minimal, so truly symmetric.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem ('what does One Country Two Systems mean when the two systems conflict?') was nominally solved by the National Security Law. The mandate was to clarify sovereignty supremacy and institutionalize mainland enforcement capability. The constraint does deliver on that mandate — the mandate has not outlived its function. However, the founding problem itself is contested: the other readings deny that the ambiguity existed in the way this reading claims, or deny that the National Security Law solves it rather than violating it. Mandatrophy in the strict sense (function atrophied, architecture persists) does not apply here — the constraint is actively functional within its own framing. But the contested nature of the founding problem statement is itself the signal: this reading's mandate is rejected by a substantial constituency as a false framing of the original problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_legitimacy_grounding,
    'Does the 1984 Sino-British Joint Declaration and the Basic Law genuinely permit the interpretation that PRC sovereignty is supreme and HK autonomy is revocable, or does the textual commitment to autonomy preclude this reading?',
    'International Court of Justice advisory opinion (if the ICJ had jurisdiction) or comparative constitutional law analysis by scholars from non-party jurisdictions. The dispute is fundamentally textual-interpretive, not empirical.',
    'If the reading is textually defensible, it is a legitimate (if contested) instantiation of the kernel; if the reading is textually foreclosed, it constitutes a unilateral rewriting of the kernel rather than an interpretation of it. This distinction maps to whether the constraint is a reading of the kernel or a rejection of it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_legitimacy_grounding, conceptual, 'Whether the sovereignty supremacy reading is a legitimate textual interpretation of the original agreement or a breaking of it.').

omega_variable(
    suppression_mechanism_internalization,
    'To what extent is the measured suppression (0.88) structural (mainland enforcement machinery, legal penalties, removal threats) versus internalized (HK civil society and judiciary self-censoring in anticipation of enforcement)?',
    'Post-exit observation: if civil society actors who emigrate report that self-censoring persists after relocation, suppression is partially internalized and carries a higher effective burden. If suppression drops after exit, it was primarily structural.',
    'Internalized suppression is harder to reverse and indicates deeper regime capture; structural suppression is more brittle and can be reversed by institutional change. High internalization indicates the constraint has already begun to reshape identity and norms, not just behavior.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Internalized versus structural components of suppression.').

omega_variable(
    beneficiary_capture_of_judiciary,
    'Are Hong Kong judges primarily targets (d ≈ 0.6) or captured agents of the beneficiary (d closer to 0.3)?',
    'Detailed analysis of individual judicial decisions in national security cases: do judges show independent reasoning or mechanical application of CCP doctrine? Do some judges resist and face removal? Do they exhibit signs of normative capture or mere coercion?',
    'If judges are primarily coerced targets, the judiciary remains a partly-autonomous institution under pressure. If they are captured, they have become organs of the beneficiary and the institutional asymmetry (local judiciary vs. mainland security) has eroded completely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_of_judiciary, empirical, 'Whether the judiciary is a coerced stakeholder or a captured agent of mainland authority.').

omega_variable(
    international_enforcement_capacity,
    'Can international mechanisms (sanctions, ICJ referral, treaty enforcement) actually constrain this reading''s operation, or are they purely performative relative to PRC capacity to absorb pressure?',
    'Historical analysis of sanction effectiveness on PRC behavior (precedent: low); analysis of PRC''s economic and political alternatives to international engagement (precedent: many). The empirical track record suggests international pressure has minimal effect on domestic security doctrine.',
    'If international capacity is truly performative, observers lack real enforcement power and the constraint operates in an environment where external pressure is costless to ignore. This changes the nature of international contestation from meaningful check to theater.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(international_enforcement_capacity, empirical, 'Whether international actors can enforce meaningful constraints on this reading or can only document it.').

omega_variable(
    kernel_foreclosure_or_reinterpretation,
    'Does the sovereignty_primacy_reading foreclose the other two readings (autonomy_primacy and balanced_coexistence) within a single commitment framework, or do the readings coexist as different parties'' incompatible positions?',
    'Analyze whether Beijing has formally declared the other readings null and void, or whether they persist as live contestation within different constituencies. If Beijing claims exclusive right to kernel interpretation and prohibits the other readings, that is foreclosure-attempt by fiat (not logical foreclosure, but institutional). If other readings persist despite repression, the readings coexist rather than foreclose.',
    'Foreclosure indicates total institutional capture and resolution of the kernel dispute through suppression. Coexistence indicates ongoing contestation and institutional pluralism (even if unevenly distributed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_foreclosure_or_reinterpretation, empirical, 'Whether this reading forecloses others or coexists with them as contested readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__sovereignty_primacy_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(one__tr_t0, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(one__tr_t3, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 3, 0.32).
narrative_ontology:measurement(one__tr_t6, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 6, 0.36).
narrative_ontology:measurement(one__tr_t9, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 9, 0.38).
narrative_ontology:measurement(one__tr_t15, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement(one__tr_t25, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 25, 0.42).

% Extraction over time
narrative_ontology:measurement(one__be_t0, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(one__be_t3, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 3, 0.67).
narrative_ontology:measurement(one__be_t6, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 6, 0.74).
narrative_ontology:measurement(one__be_t9, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 9, 0.77).
narrative_ontology:measurement(one__be_t15, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 15, 0.79).
narrative_ontology:measurement(one__be_t25, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 25, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(one__su_t0, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(one__su_t3, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 3, 0.76).
narrative_ontology:measurement(one__su_t6, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 6, 0.81).
narrative_ontology:measurement(one__su_t9, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 9, 0.84).
narrative_ontology:measurement(one__su_t15, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 15, 0.86).
narrative_ontology:measurement(one__su_t25, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 25, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__sovereignty_primacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(one_country_two_systems_framework__sovereignty_primacy_reading, 0.25).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, one_country_two_systems_framework__autonomy_primacy_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, one_country_two_systems_framework__balanced_coexistence_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_national_security_law_enforcement).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_judicial_independence_erosion).

% DUAL FORMULATION NOTE:
% This constraint is part of the one_country_two_systems_framework constraint family, which decomposes three semantically distinct readings of the 1984 agreement into separate constraint stories. The sovereignty_primacy_reading (this story) claims PRC sovereignty is supreme; the autonomy_primacy_reading claims substantive protected autonomy; the balanced_coexistence_reading claims negotiated coexistence without supremacy. The three stories share a kernel (the agreement) but instantiate different ε values and structural relationships: sovereignty_primacy has high extractiveness (0.81) and suppression (0.88) because it privileges mainland authority; autonomy_primacy would have lower extractiveness and suppression because it protects local institutions; balanced_coexistence would show ε between the two because it institutionalizes contestation rather than hierarchy. Network links connect each reading to its siblings and to downstream institutional constraints (National Security Law enforcement, judicial independence erosion) that implement the reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
