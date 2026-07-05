% ============================================================================
% CONSTRAINT STORY: one_country_two_systems_framework__sovereignty_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: One Country, Two Systems — Sovereignty Primacy Reading
 *   domain: constitutional_law/political_systems/state_sovereignty
 *
 * SUMMARY:
 *   This story instantiates the sovereignty-primacy reading of the One
 *   Country, Two Systems kernel: Hong Kong's autonomy is understood as a
 *   delegated grant from an undivided PRC sovereign authority, revocable and
 *   overridable whenever the central government determines that national
 *   security or territorial integrity is at stake. Under this reading, the
 *   2020 National Security Law, the mainland security office operating in
 *   Hong Kong, candidate disqualification, and NPCSC interpretive overrides
 *   of Basic Law provisions are not departures from the original arrangement
 *   but its logical unfolding — sovereignty was never shared, only
 *   administration was delegated, and delegation is revocable by definition.
 *   This is a distinct constraint from the autonomy_primacy_reading
 *   (treaty-guaranteed, internationally enforceable autonomy) and the
 *   balanced_coexistence_reading (ongoing negotiated division of powers) —
 *   each of those readings has its own epsilon, its own beneficiary/victim
 *   structure, and its own story file; they are not alternative measurements
 *   of this one.
 *
 * KEY AGENTS:
 *   - central_peoples_government: agenda_setter/institutional — holds and exercises revocation authority
 *   - mainland_state_security_apparatus: beneficiary/agenda_setter/institutional — operates enforcement inside Hong Kong
 *   - hong_kong_pro_beijing_establishment: beneficiary/powerful — gains political dominance
 *   - hong_kong_pro_democracy_activists: payer/powerless/trapped — bears prosecution and exile
 *   - hong_kong_independent_judiciary: payer/moderate/constrained — loses independence on national security matters
 *   - hong_kong_press_and_civil_society: payer/moderate/trapped — dissolves or self-censors
 *   - hong_kong_general_population: payer+beneficiary/powerless — mixed continuity and contraction
 *   - international_treaty_partners: excluded/powerful/analytical — object without enforcement standing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__sovereignty_primacy_reading, 0.78).
domain_priors:suppression_score(one_country_two_systems_framework__sovereignty_primacy_reading, 0.87).
domain_priors:theater_ratio(one_country_two_systems_framework__sovereignty_primacy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 0.87).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__sovereignty_primacy_reading, tangled_rope).
narrative_ontology:human_readable(one_country_two_systems_framework__sovereignty_primacy_reading, "One Country, Two Systems — Sovereignty Primacy Reading").
narrative_ontology:topic_domain(one_country_two_systems_framework__sovereignty_primacy_reading, "constitutional_law/political_systems/state_sovereignty").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__sovereignty_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__sovereignty_primacy_reading, '69481727-0801-473b-ba03-f6fc1f642259').
narrative_ontology:cs_kernel_codification('69481727-0801-473b-ba03-f6fc1f642259', formalized).
narrative_ontology:cs_authority_grounding('69481727-0801-473b-ba03-f6fc1f642259', extraction).
narrative_ontology:cs_interpretation_layer_present('69481727-0801-473b-ba03-f6fc1f642259').
narrative_ontology:cs_reading_relation('69481727-0801-473b-ba03-f6fc1f642259', one_country_two_systems_framework__autonomy_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('69481727-0801-473b-ba03-f6fc1f642259', one_country_two_systems_framework__balanced_coexistence_reading, influences).
narrative_ontology:cs_axiom('69481727-0801-473b-ba03-f6fc1f642259', foundational, sovereignty_is_undivided_and_indelegable_in_substance).
narrative_ontology:cs_axiom_status(sovereignty_is_undivided_and_indelegable_in_substance, holdable).
narrative_ontology:cs_axiom_grounding('69481727-0801-473b-ba03-f6fc1f642259', sovereignty_is_undivided_and_indelegable_in_substance, conventional).
narrative_ontology:cs_axiom('69481727-0801-473b-ba03-f6fc1f642259', foundational, national_security_necessity_overrides_prior_autonomy_grants).
narrative_ontology:cs_axiom_status(national_security_necessity_overrides_prior_autonomy_grants, holdable).
narrative_ontology:cs_axiom_grounding('69481727-0801-473b-ba03-f6fc1f642259', national_security_necessity_overrides_prior_autonomy_grants, instrumental).
narrative_ontology:cs_reference_frame('69481727-0801-473b-ba03-f6fc1f642259', unitary_sovereign_delegation_model).
narrative_ontology:cs_drift_state('69481727-0801-473b-ba03-f6fc1f642259', post_2020_national_security_law_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('69481727-0801-473b-ba03-f6fc1f642259', '').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__sovereignty_primacy_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, central_peoples_government).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_pro_beijing_establishment).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, mainland_state_security_apparatus).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_pro_democracy_activists).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_independent_judiciary).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_press_and_civil_society).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_general_population).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_general_population).
narrative_ontology:constraint_vindicates(one_country_two_systems_framework__sovereignty_primacy_reading, unitary_sovereign_state_doctrine).
narrative_ontology:constraint_vindicates(one_country_two_systems_framework__sovereignty_primacy_reading, national_security_primacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds and exercises the constitutional authority (via the National People's Congress Standing Committee) to interpret the Basic Law, enact the National Security Law directly into Hong Kong's Annex III, establish a mainland national security office operating in Hong Kong, and disqualify legislators and candidates on loyalty grounds. Frames all of this as the exercise of sovereignty that was never actually transferred, only administrative autonomy. Bears essentially no cost from exercising this authority; enforcement capacity is backed by the full apparatus of the mainland state.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, central_peoples_government, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Local political and business elites whose access to office, contracts, and mainland markets depends on demonstrated loyalty to the sovereignty-primacy framing. They gain enhanced political power as electoral and legislative rules are reshaped to guarantee their dominance (e.g. the 2021 electoral overhaul), and they can exit to the mainland or abroad if local conditions sour.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_pro_beijing_establishment, beneficiary,
    powerful, generational, mobile, regional).

% Operates the Office for Safeguarding National Security directly in Hong Kong, with mainland personnel who can in defined cases assert jurisdiction over cases removing them from the Hong Kong judicial system entirely. Gains a forward-operating base for national security enforcement inside a jurisdiction previously walled off from mainland criminal procedure.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, mainland_state_security_apparatus, beneficiary,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__sovereignty_primacy_reading, mainland_state_security_apparatus, agenda_setter).

% Face prosecution, disqualification from office, and indefinite detention under national security charges for acts (protest organizing, political publishing, seeking foreign sanctions) that were lawful before 2020. Many have fled abroad, been imprisoned, or gone silent; remaining in Hong Kong and continuing activity now carries existential legal risk. Emigration is the only meaningful exit, and it forecloses political participation entirely.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_pro_democracy_activists, payer,
    powerless, biographical, trapped, regional).

% Common-law judges historically insulated from mainland procedure now operate national security cases under a designated-judges system, without jury trial in key cases, and subject to NPCSC interpretations that can retroactively override judicial rulings. Judges can resign but cannot reform the structure from within; those who remain administer a system whose sovereignty-primacy override clause they did not design and cannot check.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_independent_judiciary, payer,
    moderate, biographical, constrained, regional).

% Independent outlets (Apple Daily, Stand News) have been raided, had assets frozen, and shut down under national-security-linked prosecutions of editors and executives. Civil society organizations have dissolved rather than risk prosecution for foreign collusion. Remaining in operation requires self-censorship on an expanding and ill-defined list of prohibited topics.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_press_and_civil_society, payer,
    moderate, biographical, trapped, regional).

% Most residents experience continuity in commercial law, property rights, and daily administration — the beneficiary side of the coordination claim — while bearing a contracted space for political expression, uncertain legal exposure for previously ordinary speech, and the psychological cost of living under a widened, vaguely bounded national security net. Emigration (via BNO or other pathways) is available to those with resources; those without are more constrained.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_general_population, payer,
    powerless, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_general_population, beneficiary).

% The United Kingdom (as co-signatory of the Sino-British Joint Declaration) and other states have objected that the sovereignty-primacy reading violates the treaty's 50-year autonomy guarantee, but hold no enforcement mechanism inside Hong Kong or China — China treats the Declaration as a historical document with no continuing legal effect post-handover. Their objections register as diplomatic protest and sanctions, not structural leverage.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, international_treaty_partners, excluded,
    powerful, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(one_country_two_systems_framework__sovereignty_primacy_reading, central_peoples_government).
narrative_ontology:fixing_cost_class(one_country_two_systems_framework__sovereignty_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, stable, ultimately-decisive locus of authority (Beijing) that resolves jurisdictional disputes between the mainland legal system and Hong Kong's common-law system without requiring case-by-case renegotiation, and gives international capital a predictable answer to 'who is actually in charge' during crises.
% TRANSFER_FUNCTION: Moves effective political and legal authority from Hong Kong's autonomous institutions (Legislative Council, judiciary, civil society) to central state organs and their local allies; moves personal safety and legal certainty away from dissenting or independent actors toward the state and its designated loyalists.
% ABSENT_VOICES: The Hong Kong electorate that would have voted for full universal suffrage under the original Basic Law roadmap has no channel to object within the current structure — pro-democracy candidates are vetted out before reaching a ballot. The UK, as co-signatory of the Joint Declaration, objects from outside but has no domestic enforcement standing.
% DISAPPEARANCE_RATIONALE: If sovereignty-primacy enforcement (NSL, disqualification mechanisms, the mainland security office) were withdrawn, Hong Kong's judiciary, press, and electoral competition would substantially reopen to pre-2020 configurations within a single electoral cycle — the pro-democracy movement, civil society organizations, and independent press have organizational memory and diaspora networks ready to re-engage. The current political settlement depends entirely on continued enforcement, not on a settled equilibrium that participants would recreate voluntarily.
% FOUNDING_PROBLEM: Reunifying Hong Kong with the mainland in 1997 without immediately imposing socialist economic and legal structures, preserving Hong Kong's function as a stable international financial center, while formally reasserting Chinese sovereignty after a century of colonial administration.
% FOUNDING_PROBLEM_CORROBORATION: Beijing attests that the 2019 protests and perceived foreign interference activated a genuine and still-live national security threat justifying the current framework. Independent assessments — UN human rights bodies, the UK Foreign Office's six-monthly Hong Kong reports, and international bar associations — attest that the invoked security threat is used to resolve a different, older problem (managing political competition and dissent), and that the founding 1997 problem of preserving Hong Kong's distinct legal and economic system is being actively dismantled rather than served by the current enforcement regime.
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__sovereignty_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__sovereignty_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__sovereignty_primacy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(one_country_two_systems_framework__sovereignty_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(one_country_two_systems_framework__sovereignty_primacy_reading, 0.78, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises sharply from a low baseline (0.18 at handover) to 0.78 by 2024, tracking the 2020 NSL enactment as the clear inflection point — this is not gradual drift but a discrete institutional rupture where a coordination arrangement (managed transition, preserved commercial law) acquired a large, concentrated, actively-enforced extraction of political rights layered on top. Suppression is authored higher than extraction (0.87 vs 0.78) because the sovereignty-primacy reading's defining feature is that alternatives (independent courts, an open press, electoral competition) have been actively and deliberately foreclosed, not merely made costly — accessibility_collapse is authored at 0.72 to reflect that meaningful alternatives (appeal to international law, domestic electoral change) have substantially but not completely closed off (diaspora and international advocacy channels remain, hence not higher). Theater ratio is moderate (0.42): the 'rule of law continuity' framing performs real legal process — trials, appeals, published judgments — but an increasing share of that process is under designated national-security judges operating without jury and under interpretive constraints that pre-determine outcomes.
 *
 * PERSPECTIVAL GAP:
 *   From the central government's seat, this is sovereignty being lawfully exercised over a delegated administrative region — no different in kind from Beijing's authority over any province, just implemented gradually per treaty commitments. From the pro-democracy activist or independent judge's seat, this is the abrogation of specific guarantees they organized their lives, careers, and legal expectations around. The engine computes these as structurally different seat classifications from the same underlying data; the divergence is the finding, not an inconsistency to resolve.
 *
 * DIRECTIONALITY LOGIC:
 *   The central government and the mainland security apparatus sit at the pure beneficiary end: they set the rules, capture the political stability and territorial-integrity assurance the framework produces, and bear essentially none of its costs (arbitrage-grade exit, institutional power). Pro-democracy activists, independent judges, and civil society sit at the target end: trapped or constrained exit, powerless-to-moderate power, and the constraint's coercive apparatus is aimed specifically at their prior activity. The general population is genuinely mixed — real continuity in commercial and property law is a real coordination benefit, but the contraction of speech and associational space is a real, if more diffuse, cost — this is why they hold both beneficiary and payer roles rather than being forced into one bucket.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (peaceful reunification while preserving Hong Kong's distinct system to protect its financial-center function) is genuinely contested as to whether it is live or dead. Beijing's account treats an ongoing security threat as the live problem the framework addresses; independent observers read the current enforcement apparatus as solving a different problem entirely — managing domestic political competition — using tools nominally justified by the original 1997 settlement. Classifying this as tangled_rope rather than snare preserves the genuine coordination function still operating for the general population (continuity of commercial law, currency, and administration) while still naming the concentrated, enforced extraction from specific political and civil-society targets — a pure snare classification would erase the real coordination benefit that a large share of the population still experiences, while a rope classification would erase the coercion documented in the NSL enforcement record.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_delegation_vs_treaty_constraint,
    'Is Hong Kong autonomy correctly modeled as a revocable administrative delegation from an undivided PRC sovereign, or as a treaty-constrained grant that binds Beijing under international law regardless of its domestic constitutional framing?',
    'This is the core committer-axis disagreement among the three kernel readings and is not resolvable by evidence internal to any one reading — it depends on which authority (PRC domestic constitutional law vs. international treaty law with UK/UN standing) is treated as dispositive. The sovereignty_primacy_reading takes the domestic constitutional framing as dispositive; the autonomy_primacy_reading takes the Joint Declaration as dispositive; the balanced_coexistence_reading treats the boundary itself as perpetually open to negotiation.',
    'If the treaty-constraint reading were adopted instead, the same enforcement actions (NSL, disqualifications) would register as breach of an external, non-revocable obligation rather than lawful exercise of retained sovereignty, converting significant portions of the extraction here into internationally wrongful acts rather than legitimate domestic authority — this would not change ε within this story, but it is the reason the sibling reading is a separate file with a separate ε rather than a different observable of this one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_delegation_vs_treaty_constraint, conceptual, 'Which authority — PRC domestic constitutional framing or international treaty law — is dispositive over the nature of Hong Kong autonomy.').

omega_variable(
    national_security_threat_genuineness,
    'To what extent does the invoked national security threat (foreign interference, secession risk during 2019 protests) reflect a genuine, proportionate security concern versus a pretext for suppressing domestic political competition?',
    'Comparative analysis of NSL prosecution targets (predominantly opposition politicians, journalists, and protest organizers rather than actors with documented foreign-intelligence links) against the stated justification; independent judicial and human-rights body assessments (UN Human Rights Committee reviews, foreign bar association reports) versus PRC/HKSAR government white papers.',
    'If the threat is substantially genuine, greater weight attaches to the coordination function (protecting territorial integrity) offsetting the extraction; if substantially pretextual, the classification moves further toward snare as the coordination story becomes cover rather than function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(national_security_threat_genuineness, empirical, 'Whether the national security justification is proportionate or serves as cover for suppressing domestic political competition.').

omega_variable(
    beneficiary_status_of_general_population,
    'Does the general Hong Kong population net-benefit from continuity of commercial law and administrative stability, or net-lose from contracted civil liberties and long-run erosion of the institutional distinctiveness that made Hong Kong economically valuable?',
    'Longitudinal tracking of capital flight, professional emigration rates, foreign direct investment trends, and Hong Kong''s ranking on rule-of-law and press-freedom indices relative to the pre-2020 baseline, weighed against continuity metrics (contract enforcement, currency stability, commercial litigation volume).',
    'If net-loss dominates over the medium term (2020s-2030s), the general population''s dual beneficiary/payer role should shift further toward payer, increasing effective extraction for that seat and strengthening the case that even the coordination-function population bears net costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_status_of_general_population, empirical, 'Whether the general population is a net beneficiary of continuity or a net payer of civil-liberty contraction over time.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__sovereignty_primacy_reading, 1997, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(one__tr_t1997, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 1997, 0.2).
narrative_ontology:measurement(one__tr_t2003, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 2003, 0.22).
narrative_ontology:measurement(one__tr_t2014, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 2014, 0.28).
narrative_ontology:measurement(one__tr_t2019, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 2019, 0.3).
narrative_ontology:measurement(one__tr_t2020, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 2020, 0.38).
narrative_ontology:measurement(one__tr_t2021, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 2021, 0.4).
narrative_ontology:measurement(one__tr_t2024, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(one__be_t1997, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 1997, 0.18).
narrative_ontology:measurement(one__be_t2003, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 2003, 0.22).
narrative_ontology:measurement(one__be_t2014, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 2014, 0.35).
narrative_ontology:measurement(one__be_t2019, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 2019, 0.48).
narrative_ontology:measurement(one__be_t2020, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 2020, 0.71).
narrative_ontology:measurement(one__be_t2021, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 2021, 0.76).
narrative_ontology:measurement(one__be_t2024, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(one__su_t1997, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 1997, 0.25).
narrative_ontology:measurement(one__su_t2003, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 2003, 0.3).
narrative_ontology:measurement(one__su_t2014, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 2014, 0.45).
narrative_ontology:measurement(one__su_t2019, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 2019, 0.58).
narrative_ontology:measurement(one__su_t2020, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 2020, 0.82).
narrative_ontology:measurement(one__su_t2021, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 2021, 0.86).
narrative_ontology:measurement(one__su_t2024, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 2024, 0.87).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__sovereignty_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, autonomy_primacy_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, balanced_coexistence_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_national_security_law_enforcement).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_judicial_independence_erosion).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the one_country_two_systems_framework kernel: sovereignty_primacy_reading (this file), autonomy_primacy_reading, and balanced_coexistence_reading. Each reading instantiates a structurally distinct constraint with its own epsilon, beneficiary/victim set, and classification, per the epsilon-invariance principle — they are not the same constraint measured three ways. This reading shows the sharpest extraction trajectory (0.18 to 0.78) because it treats the 2020 enforcement discontinuity as the framework operating as designed rather than as a departure from it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
