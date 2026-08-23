% ============================================================================
% CONSTRAINT STORY: npt_article_iv_vi_pairing__abolitionist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_article_iv_vi_pairing__abolitionist, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: npt_article_iv_vi_pairing__abolitionist
 *   human_readable: NPT Article IV/VI Pairing — Abolitionist Reading
 *   domain: international_law/nuclear_governance/treaty_interpretation
 *
 * SUMMARY:
 *   The abolitionist reading of the NPT Article IV/VI pairing treats the
 *   treaty not as a balanced bargain but as a structural snare: it extracts
 *   permanent nonproliferation compliance from 186 states while the five NWS
 *   retain and modernize their arsenals indefinitely. Authority for this
 *   reading derives from humanitarian law (ICJ 1996, ICRC positions) and the
 *   TPNW (2017), which together establish a prohibition norm that renders the
 *   NPT's discriminatory framework illegitimate. Article IV's 'inalienable
 *   right' to peaceful nuclear technology is read as a dual-use proliferation
 *   enabler that the prohibition norm must constrain. Weapon possession is
 *   categorically illegal — no distinction between peaceful and military
 *   programs can be verified or sustained. The NPT itself is delegitimized as
 *   insufficient; its review conferences are theater masking the extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__abolitionist, 0.82).
domain_priors:suppression_score(npt_article_iv_vi_pairing__abolitionist, 0.78).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__abolitionist, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, extractiveness, 0.82).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__abolitionist, snare).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__abolitionist, "NPT Article IV/VI Pairing — Abolitionist Reading").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__abolitionist, "international_law/nuclear_governance/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__abolitionist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__abolitionist, '9c5ee6f6-288d-406e-b0f0-a789f8a76316').
narrative_ontology:cs_kernel_codification('9c5ee6f6-288d-406e-b0f0-a789f8a76316', formalized).
narrative_ontology:cs_authority_grounding('9c5ee6f6-288d-406e-b0f0-a789f8a76316', extraction).
narrative_ontology:cs_interpretation_layer_present('9c5ee6f6-288d-406e-b0f0-a789f8a76316').
narrative_ontology:cs_reading_relation('9c5ee6f6-288d-406e-b0f0-a789f8a76316', npt_article_iv_vi_pairing__nonproliferation_primary, forecloses).
narrative_ontology:cs_reading_relation('9c5ee6f6-288d-406e-b0f0-a789f8a76316', npt_article_iv_vi_pairing__grand_bargain, coexists_with).
narrative_ontology:cs_axiom('9c5ee6f6-288d-406e-b0f0-a789f8a76316', foundational, nuclear_weapon_possession_categorically_illegal).
narrative_ontology:cs_axiom_status(nuclear_weapon_possession_categorically_illegal, holdable).
narrative_ontology:cs_axiom_grounding('9c5ee6f6-288d-406e-b0f0-a789f8a76316', nuclear_weapon_possession_categorically_illegal, deontological).
narrative_ontology:cs_axiom('9c5ee6f6-288d-406e-b0f0-a789f8a76316', foundational, humanitarian_law_supersedes_npt).
narrative_ontology:cs_axiom_status(humanitarian_law_supersedes_npt, holdable).
narrative_ontology:cs_axiom_grounding('9c5ee6f6-288d-406e-b0f0-a789f8a76316', humanitarian_law_supersedes_npt, deontological).
narrative_ontology:cs_reference_frame('9c5ee6f6-288d-406e-b0f0-a789f8a76316', npt_entry_into_force_1970).
narrative_ontology:cs_drift_state('9c5ee6f6-288d-406e-b0f0-a789f8a76316', contemporary_tpnw_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9c5ee6f6-288d-406e-b0f0-a789f8a76316', '2026-08-05T14:30:00Z').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__abolitionist, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__abolitionist, nuclear_umbrella_allies).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, non_nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, global_majority_populations).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, future_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__abolitionist, tpnw_states_parties).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__abolitionist, humanitarian_law_prohibits_nuclear_weapons).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__abolitionist, tpnw_customary_law_status).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__abolitionist, complete_disarmament_obligation_erga_omnes).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess nuclear arsenals while administering the NPT regime. Set the agenda for review conferences, control verification priorities through IAEA Board of Governors, and define what counts as 'progress' on Article VI. Benefit from the regime's constraint on horizontal proliferation while deferring their own disarmament obligations indefinitely. Exit is arbitrage-grade: they can withdraw (Article X) with minimal cost and retain their arsenals.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, nuclear_weapon_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__abolitionist, nuclear_weapon_states, beneficiary).

% Non-nuclear weapon states under extended deterrence commitments (NATO, US allies in Asia, Australia). Benefit from NPT's nonproliferation constraint on adversaries while relying on patron nuclear weapons for security. Their exit is constrained by alliance structures and security dependencies; leaving the umbrella would require alternative security arrangements.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, nuclear_umbrella_allies, beneficiary,
    organized, generational, constrained, regional).

% The 186 NNWS parties to the NPT. Forswear nuclear weapons permanently, accept comprehensive safeguards, and transfer technology under Article IV — but receive no binding disarmament timeline from NWS. Their exit is constrained: withdrawal (Article X) triggers severe diplomatic, economic, and security consequences; the regime's verification machinery makes clandestine programs nearly impossible. They bear the cost of compliance while the bargain's other half goes unfulfilled.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, non_nuclear_weapon_states, payer,
    moderate, generational, constrained, global).

% Populations in the Global South and non-aligned states who bear the humanitarian risk of nuclear detonation (climate effects, famine, radiation) without any say in the regime. They cannot exit the risk — nuclear winter is planetary. Their interests are represented only indirectly through NNWS governments, many of which are aid-dependent on NWS. The TPNW emerged from their civil society mobilization but they remain excluded from NPT decision-making.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, global_majority_populations, payer,
    powerless, civilizational, trapped, universal).

% Abstract but structurally real stakeholder: all humans not yet born who inherit the nuclear risk without consent. The NPT regime perpetuates a permanent condition of existential threat. No exit exists. Listed as non-agent (agent=false) because they cannot act, but their structural position as ultimate victims of the constraint's extraction (perpetuated nuclear risk) is analytically necessary.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, future_generations, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(npt_article_iv_vi_pairing__abolitionist, future_generations).

% The 70+ states parties to the Treaty on the Prohibition of Nuclear Weapons (TPNW). They operate an alternative legal framework that categorically prohibits nuclear weapons. They benefit from the TPNW's normative clarity and stigma-generation against nuclear possession. Their exit from the NPT frame is mobile — they participate in NPT review conferences as observers but center their diplomacy on TPNW implementation. They are not bound by NPT Article IV/VI pairing.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, tpnw_states_parties, observer,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__abolitionist, tpnw_states_parties, beneficiary).

% The International Atomic Energy Agency secretariat administers the verification machinery (Article III safeguards). It is the regime's operational backbone. As an observer seat, it neither collects the regime's political rents nor bears its humanitarian costs, but its institutional survival depends on the NPT's continued legitimacy. Its analytical position sees the full structure: the verification asymmetry (NNWS comprehensively verified, NWS minimally) and the Article VI compliance gap.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, iaea_secretariat, observer,
    institutional, generational, analytical, global).

% The ICJ's 1996 Advisory Opinion concluded that 'there exists an obligation to pursue in good faith and bring to a conclusion negotiations leading to nuclear disarmament in all its aspects under strict and effective international control.' This reading treats that obligation as legally binding and justiciable. The Court sits outside the regime but its pronouncement structurally conditions the regime's legitimacy. It does not enforce; it clarifies the law.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, international_court_of_justice, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The NPT presents itself as solving a collective-action problem: preventing nuclear war by limiting weapons to five states while they disarm. The abolitionist reading holds this coordination is a cover — the real function is managing proliferation to preserve NWS monopoly.
% TRANSFER_FUNCTION: Moves the legal right to nuclear weapons and the technology to acquire them from NNWS to NWS permanently; moves the burden of verification, compliance costs, and existential risk from NWS to NNWS and global populations; moves diplomatic legitimacy from prohibition norms to the NPT's discriminatory framework.
% ABSENT_VOICES: Hibakusha (atomic bomb survivors) and affected communities from nuclear testing (Marshall Islands, Kazakhstan, Algeria, French Polynesia, Indigenous lands in US/Australia) are structurally excluded from NPT decision-making. Their testimony on humanitarian consequences is admitted only as 'civil society input' with no formal standing. The TPNW was built precisely to center these voices.
% DISAPPEARANCE_RATIONALE: If the NPT Article IV/VI pairing vanished overnight, the legal architecture constraining NNWS would collapse — but so would the legal cover for NWS retention. The TPNW would become the sole treaty framework; NNWS would face no legal barrier to acquiring nuclear technology (though political/technical barriers remain); NWS would lose the 'grand bargain' narrative that legitimizes their arsenals. The world would rearrange toward either rapid proliferation or rapid prohibition, depending on which coalition mobilizes faster.
% FOUNDING_PROBLEM: The 1968 NPT was built to solve the 'N+1 problem' — preventing a cascade of new nuclear weapon states after China's 1964 test — while offering a vague disarmament promise to secure NNWS signatures. The founders (US, USSR, UK) understood Article VI as a political gesture, not a binding timeline.
% FOUNDING_PROBLEM_CORROBORATION: Declassified US/USSR negotiating records (SIPRI, UNIDIR archives) show Article VI was drafted as 'pursue negotiations in good faith' precisely to avoid a binding deadline. NNWS negotiators (India, Mexico, Sweden) objected at the time — their dissent is on the record. The TPNW preamble cites the 'catastrophic humanitarian consequences' as the problem the NPT failed to solve. No independent legal scholar today argues the founding problem (preventing proliferation via a credible disarmament bargain) is still live.
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__abolitionist, world_rearranges).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__abolitionist, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__abolitionist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(npt_article_iv_vi_pairing__abolitionist, 'none', 1).
narrative_ontology:epsilon_provenance(npt_article_iv_vi_pairing__abolitionist, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_article_iv_vi_pairing__abolitionist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_article_iv_vi_pairing__abolitionist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_article_iv_vi_pairing__abolitionist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.82) because NWS extract permanent security compliance from NNWS (no weapons, full verification) while delivering zero disarmament. Suppression is high (0.78) because the regime actively suppresses the prohibition alternative (TPNW boycott, pressure on signatories) and makes withdrawal existentially costly. Theater ratio is moderate (0.45) — NPT review conferences, 'step-by-step' disarmament rhetoric, and action plans perform coordination while the structural extraction continues. Accessibility collapse is very high (0.85): once a state joins the NPT, the legal, technical, and political barriers to acquiring nuclear weapons or exiting the regime are nearly total. Resistance is moderate (0.55) — TPNW, NNWS coalitions (NAM, NPT PrepCom walkouts), and civil society mount real resistance but remain structurally marginalized.
 *
 * PERSPECTIVAL GAP:
 *   From the NWS seat, the NPT is a successful coordination mechanism that prevented the predicted 20-30 nuclear states. From the NNWS seat, it is a discriminatory trap that froze inequality. From the global majority seat, it is a suicide pact they never signed. The engine computes these divergent effective extractions from the single structural dataset — the abolitionist claim (snare) reflects the NNWS/global majority seat; the NWS seat would compute as rope or mountain. The divergence IS the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   NWS and umbrella allies are structural beneficiaries (d ≈ 0.1-0.2): they collect the security benefits of nonproliferation without paying the disarmament cost. NNWS are primary targets (d ≈ 0.9): they pay the full compliance cost with no reciprocal delivery. Global majority populations and future generations are trapped targets (d ≈ 1.0): they bear the existential risk with zero exit. TPNW states and ICJ are analytical observers (d ≈ 0.5): they see the structure but are not directly caught in the extraction flow. IAEA is an institutional observer whose mandate depends on the regime's continuity.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing N+1 proliferation via a credible disarmament bargain) is dead — NWS have not disarmed, and the bargain's reciprocity is a fiction. Yet the arrangement persists and deepens (modernization programs, AUKUS, extended deterrence expansion). This is classic mandatrophy: the mandate (Article VI) has outlived its function (disarmament), but the constraint (Article III/IV compliance) extracts more than ever. The regime survives by suppressing the prohibition alternative and performing disarmament theater. Classification as snare (not piton) is correct because NWS actively benefit and actively maintain the extraction — it is not inertial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_justiciability,
    'Is NPT Article VI a legally binding obligation of result (disarmament) or merely an obligation of conduct (pursue negotiations)?',
    'ICJ advisory opinion or contentious case clarifying the legal character of Article VI; state practice and opinio juris since 1996 Advisory Opinion; TPNW''s explicit prohibition as subsequent agreement modifying NPT interpretation under VCLT Article 31(3)(b).',
    'If binding obligation of result, NWS are in material breach — the regime''s legitimacy collapses and NNWS may have grounds for countermeasures or withdrawal without Article X penalties. If mere conduct obligation, the extraction continues legally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_justiciability, conceptual, 'Legal character of Article VI — the core interpretive dispute between abolitionist and nonproliferation_primary readings.').

omega_variable(
    dual_use_inherent_proliferation_risk,
    'Does civilian nuclear technology transfer under Article IV inherently create proliferation risk that cannot be verified away, or can safeguards reliably separate peaceful from military programs?',
    'Historical analysis of proliferation pathways (India 1974, Iraq 1990s, Iran 2000s, DPRK) — every proliferator used Article IV-covered technology/assistance. Technical assessment of verification limits: enrichment/reprocessing dual-use, latency, breakout time. IAEA''s own ''state-level concept'' acknowledges fundamental verification gaps.',
    'If inherent, Article IV is structurally incompatible with nonproliferation and the abolitionist reading''s prohibition-norm constraint on Article IV is validated. If separable, the NPT''s coordination function has technical credibility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dual_use_inherent_proliferation_risk, empirical, 'Whether the coordination function (Article IV technology transfer) is structurally separable from the extraction (proliferation latency).').

omega_variable(
    tpnw_customary_law_status,
    'Has the TPNW''s prohibition norm crystallized into customary international law binding on non-parties (including NWS)?',
    'State practice and opinio juris analysis: 70+ ratifications, 93 signatories, UNGA resolutions with overwhelming majorities, ICJ 1996 ''comprehensive prohibition'' language, ICRC/ICJ statements on customary status. NWS persistent objection doctrine assessment.',
    'If customary, NWS are bound by prohibition regardless of NPT/TPNW party status — the abolitionist reading''s authority claim is legally secured. If not, the reading remains a normative aspiration without formal legal force against NWS.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tpnw_customary_law_status, conceptual, 'Whether the TPNW prohibition norm has customary law force — the abolitionist reading''s authority anchor.').

omega_variable(
    suppression_mechanism_npt_review_conferences,
    'Is the suppression of the prohibition alternative (TPNW) at NPT review conferences structural (procedural exclusion, consensus rules) or internalized (NNWS self-censorship, dependency on NWS aid)?',
    'Post-exit suppression trajectory: track NNWS voting/alignment on TPNW before/after aid dependence metrics; analyze NPT RevCon procedural records for systematic exclusion patterns; compare with other treaty regimes'' treatment of competing instruments.',
    'If structural, the regime''s suppression is externally enforced and reversible by procedural reform. If internalized, the constraint''s effective suppression persists even if formal barriers drop — NNWS carry the suppression with them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_npt_review_conferences, empirical, 'Structural vs. internalized suppression of the prohibition alternative in NPT forums.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__abolitionist, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt_art_iv_vi_abol_tr_t1970, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 1970, 0.25).
narrative_ontology:measurement(npt_art_iv_vi_abol_tr_t1985, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 1985, 0.3).
narrative_ontology:measurement(npt_art_iv_vi_abol_tr_t1995, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 1995, 0.35).
narrative_ontology:measurement(npt_art_iv_vi_abol_tr_t2000, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2000, 0.38).
narrative_ontology:measurement(npt_art_iv_vi_abol_tr_t2010, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2010, 0.42).
narrative_ontology:measurement(npt_art_iv_vi_abol_tr_t2017, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2017, 0.44).
narrative_ontology:measurement(npt_art_iv_vi_abol_tr_t2024, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(npt_art_iv_vi_abol_be_t1970, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 1970, 0.45).
narrative_ontology:measurement(npt_art_iv_vi_abol_be_t1985, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 1985, 0.55).
narrative_ontology:measurement(npt_art_iv_vi_abol_be_t1995, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 1995, 0.62).
narrative_ontology:measurement(npt_art_iv_vi_abol_be_t2000, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement(npt_art_iv_vi_abol_be_t2010, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2010, 0.73).
narrative_ontology:measurement(npt_art_iv_vi_abol_be_t2017, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2017, 0.79).
narrative_ontology:measurement(npt_art_iv_vi_abol_be_t2024, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2024, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(npt_art_iv_vi_abol_su_t1970, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 1970, 0.5).
narrative_ontology:measurement(npt_art_iv_vi_abol_su_t1985, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 1985, 0.58).
narrative_ontology:measurement(npt_art_iv_vi_abol_su_t1995, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 1995, 0.65).
narrative_ontology:measurement(npt_art_iv_vi_abol_su_t2000, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(npt_art_iv_vi_abol_su_t2010, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 2010, 0.73).
narrative_ontology:measurement(npt_art_iv_vi_abol_su_t2017, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 2017, 0.76).
narrative_ontology:measurement(npt_art_iv_vi_abol_su_t2024, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__abolitionist, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_article_iv_vi_pairing__abolitionist, 0.12).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing__nonproliferation_primary).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing__grand_bargain).

% DUAL FORMULATION NOTE:
% This constraint family (npt_article_iv_vi_pairing) decomposes the single NPT label into three structurally distinct readings with divergent ε values. The abolitionist reading (this story) has high extractiveness (0.82) because it treats the NPT as a discriminatory extraction regime. The nonproliferation_primary reading has low extractiveness (~0.2) because it treats Article VI as aspirational and the regime as genuine coordination. The grand_bargain reading sits between (~0.5) as a contested reciprocal bargain. They are linked because the NPT text is the shared kernel, but each reading instantiates a different constraint with different beneficiaries, victims, and type.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npt_article_iv_vi_pairing__abolitionist, institutional, 0.15).
constraint_indexing:directionality_override(npt_article_iv_vi_pairing__abolitionist, organized, 0.35).
constraint_indexing:directionality_override(npt_article_iv_vi_pairing__abolitionist, moderate, 0.85).
constraint_indexing:directionality_override(npt_article_iv_vi_pairing__abolitionist, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
