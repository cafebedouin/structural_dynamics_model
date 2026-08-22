% ============================================================================
% CONSTRAINT STORY: one_country_two_systems_framework__sovereignty_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: One Country, Two Systems — Sovereignty Primacy Reading
 *   domain: constitutional_law/political_systems/state_sovereignty
 *
 * SUMMARY:
 *   This story authors the sovereignty-primacy reading of the One Country,
 *   Two Systems kernel: the 1997 handover arrangement, the Basic Law, and the
 *   2020 National Security Law are read as instruments through which PRC
 *   sovereign authority delegates and can revoke Hong Kong's autonomy
 *   whenever national security or territorial integrity is implicated. Under
 *   this reading, the 2019 protests and their suppression, the
 *   disqualification of opposition legislators, the NSL's direct operation in
 *   Hong Kong courts, and the presence of mainland security organs on Hong
 *   Kong soil are not departures from the framework but its correct operation
 *   — autonomy was always conditional and always subject to override. This is
 *   a distinct constraint from the autonomy_primacy_reading (which treats the
 *   same 1997–2020 guarantees as internationally enforceable treaty
 *   commitments) and from the balanced_coexistence_reading (which treats the
 *   boundary as continuously negotiated rather than legally settled in either
 *   direction). The epsilon authored here is high because, BY THIS READING'S
 *   OWN LIGHTS, the override is a legitimate and correctly exercised
 *   sovereign prerogative that nonetheless imposes severe, concentrated costs
 *   on a specific set of local actors — the reading does not deny the
 *   extraction, it denies that the extraction is illegitimate.
 *
 * KEY AGENTS:
 *   - prc_central_government: primary agenda_setter and beneficiary (institutional/arbitrage) — holds and exercises override authority
 *   - hong_kong_pro_democracy_movement: primary target (powerless/trapped) — bears reclassification of prior lawful activity as subversion
 *   - hong_kong_independent_judiciary: secondary target (moderate/constrained) — loses interpretive finality
 *   - mainland_security_and_liaison_agencies: administering agent (institutional/analytical) — operates the override mechanism directly on Hong Kong soil
 *   - foreign_governments_and_international_observers: excluded observer (powerful/analytical) — asserts interest, denied standing by the reading itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__sovereignty_primacy_reading, 0.78).
domain_priors:suppression_score(one_country_two_systems_framework__sovereignty_primacy_reading, 0.86).
domain_priors:theater_ratio(one_country_two_systems_framework__sovereignty_primacy_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 0.86).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__sovereignty_primacy_reading, tangled_rope).
narrative_ontology:human_readable(one_country_two_systems_framework__sovereignty_primacy_reading, "One Country, Two Systems — Sovereignty Primacy Reading").
narrative_ontology:topic_domain(one_country_two_systems_framework__sovereignty_primacy_reading, "constitutional_law/political_systems/state_sovereignty").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__sovereignty_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__sovereignty_primacy_reading, 'e1caa79d-ae3c-41a9-b87c-a4e430347488').
narrative_ontology:cs_kernel_codification('e1caa79d-ae3c-41a9-b87c-a4e430347488', formalized).
narrative_ontology:cs_authority_grounding('e1caa79d-ae3c-41a9-b87c-a4e430347488', extraction).
narrative_ontology:cs_interpretation_layer_present('e1caa79d-ae3c-41a9-b87c-a4e430347488').
narrative_ontology:cs_reading_relation('e1caa79d-ae3c-41a9-b87c-a4e430347488', one_country_two_systems_framework__autonomy_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('e1caa79d-ae3c-41a9-b87c-a4e430347488', one_country_two_systems_framework__balanced_coexistence_reading, influences).
narrative_ontology:cs_axiom('e1caa79d-ae3c-41a9-b87c-a4e430347488', foundational, sovereign_authority_is_indivisible_and_ultimately_revocable).
narrative_ontology:cs_axiom_status(sovereign_authority_is_indivisible_and_ultimately_revocable, holdable).
narrative_ontology:cs_axiom_grounding('e1caa79d-ae3c-41a9-b87c-a4e430347488', sovereign_authority_is_indivisible_and_ultimately_revocable, conventional).
narrative_ontology:cs_axiom('e1caa79d-ae3c-41a9-b87c-a4e430347488', foundational, national_security_determination_is_a_central_government_prerogative_not_subject_to_local_judicial_review).
narrative_ontology:cs_axiom_status(national_security_determination_is_a_central_government_prerogative_not_subject_to_local_judicial_review, holdable).
narrative_ontology:cs_axiom_grounding('e1caa79d-ae3c-41a9-b87c-a4e430347488', national_security_determination_is_a_central_government_prerogative_not_subject_to_local_judicial_review, conventional).
narrative_ontology:cs_reference_frame('e1caa79d-ae3c-41a9-b87c-a4e430347488', unitary_state_sovereignty_with_delegated_autonomy).
narrative_ontology:cs_drift_state('e1caa79d-ae3c-41a9-b87c-a4e430347488', post_2020_national_security_law_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e1caa79d-ae3c-41a9-b87c-a4e430347488', '').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__sovereignty_primacy_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, prc_central_government).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_pro_beijing_establishment).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_pro_democracy_movement).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_independent_judiciary).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_civil_society_organizations).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_press_and_journalists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_business_and_financial_sector).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_business_and_financial_sector).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds and exercises the constitutional power to interpret the Basic Law through the National People's Congress Standing Committee, to impose the National Security Law directly, and to station security personnel in Hong Kong. Frames autonomy as a grant that can be calibrated or withdrawn whenever it is read as threatening territorial integrity or Party rule. Bears essentially no cost from exercising this authority and faces no domestic legal check on it.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, prc_central_government, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__sovereignty_primacy_reading, prc_central_government, beneficiary).

% Local political, business, and administrative figures whose positions, contracts, and legitimacy depend on alignment with Beijing's reading of the framework. They gain enforcement backing, political appointments, and protection from electoral or judicial checks that would otherwise constrain them; their exit option is upward mobility into mainland-aligned structures, not resistance.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_pro_beijing_establishment, beneficiary,
    organized, generational, mobile, regional).

% Elected legislators, organizers, and activists who treated Basic Law autonomy guarantees as enforceable. Under this reading their activity is reclassified as subversion, collusion with foreign forces, or secession; many face disqualification, prosecution under the National Security Law, or exile. Emigration is the only real exit and it forfeits their political project entirely.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_pro_democracy_movement, payer,
    powerless, biographical, trapped, regional).

% Common-law judges who previously exercised interpretive independence over the Basic Law now operate under a framework where NPCSC interpretation is final and binding, and national-security cases can be tried by hand-picked judges or removed to mainland jurisdiction. Judges can resign but cannot rule their way out of the override; the institution's independence is the thing being consumed.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_independent_judiciary, payer,
    moderate, biographical, constrained, regional).

% Unions, student groups, and advocacy NGOs that operated under assumed rule-of-law protections now face deregistration, asset freezes, or dissolution when their activity is read as endangering national security. Formal dissolution and self-censorship are the available responses; the space for coordinated dissent is systematically closing.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_civil_society_organizations, payer,
    powerless, biographical, trapped, regional).

% Independent outlets and reporters whose critical coverage of the central government or local establishment is increasingly prosecutable as national-security offense. Some outlets have closed or relocated operations abroad; those who stay operate under credible threat of raid or arrest, which shapes editorial decisions well beyond formally prohibited speech.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_press_and_journalists, payer,
    moderate, biographical, constrained, regional).

% The Office for Safeguarding National Security and liaison bodies now operate directly within Hong Kong, investigating and in defined cases prosecuting cases outside local police and judicial chains entirely. They administer the override on the ground and answer to Beijing rather than to Hong Kong institutions.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, mainland_security_and_liaison_agencies, agenda_setter,
    institutional, civilizational, analytical, national).

% Treaty co-signatories (notably the UK, under the Sino-British Joint Declaration) and international bodies assert an interest in Hong Kong's promised autonomy but hold no enforcement mechanism recognized by this reading; PRC sovereignty doctrine treats foreign objection as interference in internal affairs and excludes it from the adjudicating conversation entirely.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, foreign_governments_and_international_observers, excluded,
    powerful, generational, analytical, global).

% Multinational firms and the financial sector generally benefit from stability and predictable enforcement once political contestation is suppressed, but bear reputational and operational risk when international perception of rule-of-law erosion affects capital flows and expatriate confidence. Large firms can relocate regional headquarters; smaller local firms cannot.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_business_and_financial_sector, beneficiary,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_business_and_financial_sector, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(one_country_two_systems_framework__sovereignty_primacy_reading, prc_central_government).
narrative_ontology:fixing_cost_class(one_country_two_systems_framework__sovereignty_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, final locus of authority to resolve jurisdictional conflicts between a socialist unitary state and a common-law capitalist enclave, avoiding the need to renegotiate sovereignty questions case by case and giving the central government a mechanism to respond to what it perceives as existential threats to territorial integrity.
% TRANSFER_FUNCTION: Moves effective governing authority — over legislative interpretation, judicial finality, security enforcement, and permissible political speech — from Hong Kong's local institutions and civil society to PRC central organs, on the central government's own determination of when national security is implicated.
% ABSENT_VOICES: The 1997 generation of Hong Kong residents who were promised fifty years of unchanged systems were never party to the NSL's imposition; foreign treaty co-signatories are formally excluded from adjudicating compliance; ordinary residents who neither support nor oppose the establishment but experience shrinking associational space have no forum in which their preferences register.
% DISAPPEARANCE_RATIONALE: If sovereignty-override authority vanished overnight, Hong Kong's legislature, courts, and civil society would revert to contesting the Basic Law's boundaries through local institutions rather than through Beijing's final say; disqualified legislators, dissolved organizations, and exiled activists would have a basis to resume political activity; mainland security agencies operating on Hong Kong soil would lose their jurisdictional basis entirely.
% FOUNDING_PROBLEM: How to reincorporate a territory with a distinct legal, economic, and civic system into a unitary socialist state without immediately destroying the economic value and international confidence that system generated — while ensuring the territory could never become a base for challenging central authority or territorial integrity.
% FOUNDING_PROBLEM_CORROBORATION: The PRC central government and aligned establishment attest the founding problem (safeguarding sovereignty and territorial integrity) remains live and cite 2019 unrest as vindication. Independent legal scholars, former Hong Kong judges who have resigned citing rule-of-law concerns, and foreign parliamentary inquiries (UK, EU, and US congressional committees) attest that the original economic-continuity rationale has been substantially supplanted by open-ended political control unconnected to any live security threat — corroboration from outside the beneficiary set exists but is itself excluded from the adjudicating framework this reading recognizes.
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__sovereignty_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__sovereignty_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__sovereignty_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extraction (0.78) is high because the reading concentrates political, legal, and associational costs onto identifiable local actors — legislators, judges, journalists, civil society — while the central government and aligned establishment capture nearly all of the governing authority transferred. Suppression (0.86) exceeds extraction because persistence of the arrangement depends on active enforcement: NSL prosecutions, disqualifications, deregistrations, and direct mainland agency presence, not on voluntary local acceptance. Theater ratio is moderate (0.40) because enforcement is substantially functional (prosecutions produce real convictions and real institutional change) rather than symbolic, though public messaging about 'restoring stability' does perform a legitimating function beyond what the enforcement itself requires. Accessibility collapse (0.72) reflects how thoroughly alternative avenues for contesting the override — independent courts, an unconstrained legislature, international appeal — have closed once the NSL and NPCSC interpretation authority are in place. Resistance (0.58) remains substantial because a large minority of Hong Kong residents and much of the international community continue to contest the framework's legitimacy even as the mechanism to contest it domestically has been foreclosed.
 *
 * DIRECTIONALITY LOGIC:
 *   The PRC central government sits at the extreme beneficiary end: it authored the override, exercises it without domestic check, and bears essentially no structural cost. The pro-Beijing establishment sits near it, deriving position and protection from alignment. The pro-democracy movement, judiciary, civil society, and press sit at the target end: their prior legal and institutional standing is precisely what the override consumes, and their exit options range from trapped to merely constrained. The business sector is genuinely mixed — it benefits from enforced stability but bears reputational and capital-flight risk, which is why it carries a dual role rather than a clean beneficiary or payer designation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reincorporating Hong Kong without destroying its economic value while foreclosing any base for challenging central authority — has plausibly shifted from a live territorial-integrity concern (relevant amid 2019-era unrest) toward an open-ended political-control mandate exercised well past the point where an acute security threat could be demonstrated. The founding_problem_status is authored as contested rather than resolved: this reading's own proponents insist the problem remains live and self-evidently justifies continued override, while corroboration from outside the beneficiary set (resigned judges, foreign parliamentary inquiries, independent legal scholarship) reads the arrangement as having outrun its stated justification. The tangled_rope classification is chosen deliberately over snare because a genuine coordination function is present and structurally necessary — some mechanism for resolving sovereignty conflicts between a unitary state and a semi-autonomous territory is not obviously illegitimate on its face — but it operates alongside concentrated, enforced extraction from identifiable victim groups, which is exactly the hybrid tangled_rope is built to flag rather than let either 'it's just governance' or 'it's pure oppression' framings absorb the whole picture uncontested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_primacy_kernel_reading,
    'Is the sovereignty-primacy reading of One Country, Two Systems the correct account of what the 1984 Joint Declaration and 1990 Basic Law actually committed the PRC to, or is it a post-hoc reinterpretation that the 2020 National Security Law imposed on a framework originally understood (by its drafters and co-signatories) as guaranteeing substantive, judicially enforceable autonomy?',
    'Comparative textual and negotiating-history analysis of the Joint Declaration, the Basic Law drafting record, and NPCSC interpretation practice pre- and post-2020, cross-referenced against contemporaneous statements by drafters and UK negotiators at the time of handover.',
    'If the sovereignty-primacy reading is the historically accurate account of the original bargain, the override is a consistent exercise of always-latent authority. If the autonomy-primacy reading is historically accurate, the current enforcement regime is better modeled as a repudiation of the founding commitment rather than its fulfillment — which would reclassify much of what this story treats as legitimate coordination as closer to pure extraction dressed in continuity language.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_primacy_kernel_reading, conceptual, 'Whether sovereignty-primacy is the kernel''s original meaning or a later reinterpretation — the central committer-frame ambiguity this reading takes a position on.').

omega_variable(
    national_security_threshold_ambiguity,
    'Is there a knowable, non-arbitrary threshold at which local activity actually threatens PRC territorial integrity or national security, or is the threshold self-defined by the central government such that the override can be triggered by any activity it chooses to characterize as threatening?',
    'Comparative case analysis of NSL prosecutions against a plausible external security-threat baseline (e.g., analogous national-security statutes in other jurisdictions and their invocation patterns) to assess whether Hong Kong prosecutions track an identifiable security threshold or track political dissent generally.',
    'If the threshold is genuinely security-tracking, the override functions closer to a bounded coordination mechanism (tangled_rope with a narrower extraction footprint). If the threshold is self-defining and elastic, the override is functionally unbounded and the classification would drift toward snare, since the coordination story would no longer constrain what counts as a legitimate trigger.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(national_security_threshold_ambiguity, empirical, 'Whether the national-security trigger for override is bounded or self-defining without limit.').

omega_variable(
    cs_framing_kernel_vs_legitimacy_narrative,
    'Should the commitment-system kernel for this reading be the Basic Law text itself (a fixed_text framing) or the higher-order narrative of PRC territorial and political continuity that the Basic Law is read as serving (a formalized doctrine framing)? These produce different authority_grounding pictures — fixed_text+extraction (the text wielded institutionally) versus a formalized sovereignty doctrine that treats the text as merely one implementing instrument among several (NSL, NPCSC interpretations, Article 23 legislation).',
    'Track which framing PRC constitutional scholarship and NPCSC practice itself privileges when justifying interpretive departures from Basic Law text — if departures are justified by appeal to sovereignty doctrine rather than textual argument, the doctrine framing is operative.',
    'Under the fixed_text framing, drift analysis would track departures from Basic Law text specifically. Under the formalized-doctrine framing, the Basic Law itself is downstream of the sovereignty doctrine and drift should be tracked against the doctrine''s own stated scope, which is more elastic. This story adopts the formalized-doctrine framing (kernel_codification: formalized, authority_grounding: extraction) because NPCSC practice has repeatedly justified departures from Basic Law text by direct appeal to sovereignty and security doctrine rather than textual construction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_kernel_vs_legitimacy_narrative, conceptual, 'Alternative CS framings (fixed_text vs formalized doctrine) that would classify drift differently.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__sovereignty_primacy_reading, 0, 28).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(one__tr_t0, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(one__tr_t5, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(one__tr_t12, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 12, 0.25).
narrative_ontology:measurement(one__tr_t17, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 17, 0.3).
narrative_ontology:measurement(one__tr_t22, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 22, 0.36).
narrative_ontology:measurement(one__tr_t25, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 25, 0.39).
narrative_ontology:measurement(one__tr_t28, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 28, 0.4).

% Extraction over time
narrative_ontology:measurement(one__be_t0, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(one__be_t5, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 5, 0.36).
narrative_ontology:measurement(one__be_t12, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 12, 0.42).
narrative_ontology:measurement(one__be_t17, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 17, 0.52).
narrative_ontology:measurement(one__be_t22, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 22, 0.71).
narrative_ontology:measurement(one__be_t25, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 25, 0.76).
narrative_ontology:measurement(one__be_t28, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 28, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(one__su_t0, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(one__su_t5, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 5, 0.38).
narrative_ontology:measurement(one__su_t12, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 12, 0.44).
narrative_ontology:measurement(one__su_t17, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 17, 0.58).
narrative_ontology:measurement(one__su_t22, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 22, 0.79).
narrative_ontology:measurement(one__su_t25, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 25, 0.84).
narrative_ontology:measurement(one__su_t28, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 28, 0.86).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__sovereignty_primacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(one_country_two_systems_framework__sovereignty_primacy_reading, 0.12).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, autonomy_primacy_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, balanced_coexistence_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_national_security_law_enforcement).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_judicial_independence_erosion).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language label 'One Country, Two Systems.' autonomy_primacy_reading authors the same 1997-2020 arrangement as an internationally enforceable treaty guarantee (low epsilon under that reading's own lights, high epsilon assigned only to breach events); balanced_coexistence_reading authors the arrangement as an ongoing, legitimately contested negotiation with no settled legal supremacy (moderate epsilon, contested classification). This story authors the sovereignty-primacy reading with epsilon = 0.78 because, by this reading's own lights, the override is legitimate sovereign prerogative that nonetheless imposes severe concentrated costs. Per the epsilon-invariance principle, these are three distinct constraints sharing a contested kernel, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
