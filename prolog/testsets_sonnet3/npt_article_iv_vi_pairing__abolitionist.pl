% ============================================================================
% CONSTRAINT STORY: npt_article_iv_vi_pairing__abolitionist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
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
 *   constraint_id: npt_article_iv_vi_pairing__abolitionist
 *   human_readable: NPT Article IV/VI Pairing — Abolitionist Reading (Disarmament-Primacy)
 *   domain: international_law/nuclear_governance/treaty_interpretation
 *
 * SUMMARY:
 *   This story authors the abolitionist reading of the NPT's Article IV/VI
 *   pairing: the claim that Article VI's disarmament obligation is not
 *   aspirational but mandatory, that Article IV's peaceful-technology
 *   guarantee is illegitimate insofar as it sustains dual-use proliferation
 *   pathways, and that legal authority for this reading derives from
 *   international humanitarian law and weapons-prohibition treaty precedent
 *   (landmines, cluster munitions, and most directly the 2017 Treaty on the
 *   Prohibition of Nuclear Weapons). Under this reading the NPT's
 *   diplomatic-bargain architecture is itself delegitimized as an
 *   insufficient response to a category of weapon that humanitarian law
 *   renders categorically impermissible — there is no meaningful distinction
 *   between 'peaceful' and 'military' nuclear programs once the prohibition
 *   norm is taken as authoritative, because the underlying fissile and
 *   delivery infrastructure is dual-use by construction. This is one of three
 *   linked readings of the same kernel (npt_article_iv_vi_pairing); the
 *   nonproliferation_primary reading and the grand_bargain reading are
 *   separate constraint stories with their own ε and stakeholder structures,
 *   not alternative framings folded into this one.
 *
 * KEY AGENTS:
 *   - nuclear_weapon_states: agenda_setter/beneficiary (institutional/arbitrage) — administer verification architecture, retain arsenals under indefinite 'good faith' language
 *   - npt_administering_bodies: beneficiary/agenda_setter (institutional/arbitrage) — institutional relevance depends on the pairing remaining unresolved
 *   - non_nuclear_weapon_states: payer (moderate/constrained) — bear compliance costs without corresponding disarmament delivery
 *   - atomic_bomb_survivor_communities: payer (powerless/trapped) — bear the embodied humanitarian cost the norm exists to prevent, no NPT seat
 *   - tpnw_signatory_states: payer/excluded (moderate/constrained) — operate under the prohibition norm but lack NPT institutional leverage
 *   - civil_society_disarmament_coalitions: excluded (powerless/constrained) — authored the humanitarian-consequences case this reading treats as authoritative, no negotiating standing
 *   - international_legal_scholars: observer (analytical/analytical) — assess competing authority claims across all three kernel readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__abolitionist, 0.81).
domain_priors:suppression_score(npt_article_iv_vi_pairing__abolitionist, 0.72).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__abolitionist, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, extractiveness, 0.81).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__abolitionist, tangled_rope).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__abolitionist, "NPT Article IV/VI Pairing — Abolitionist Reading (Disarmament-Primacy)").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__abolitionist, "international_law/nuclear_governance/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__abolitionist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__abolitionist, 'cfb04da1-b448-4af8-91f3-a7b12e24da2a').
narrative_ontology:cs_kernel_codification('cfb04da1-b448-4af8-91f3-a7b12e24da2a', fixed_text).
narrative_ontology:cs_authority_grounding('cfb04da1-b448-4af8-91f3-a7b12e24da2a', extraction).
narrative_ontology:cs_interpretation_layer_present('cfb04da1-b448-4af8-91f3-a7b12e24da2a').
narrative_ontology:cs_reading_relation('cfb04da1-b448-4af8-91f3-a7b12e24da2a', npt_article_iv_vi_pairing__nonproliferation_primary, forecloses).
narrative_ontology:cs_reading_relation('cfb04da1-b448-4af8-91f3-a7b12e24da2a', npt_article_iv_vi_pairing__grand_bargain, influences).
narrative_ontology:cs_axiom('cfb04da1-b448-4af8-91f3-a7b12e24da2a', foundational, possession_categorically_impermissible).
narrative_ontology:cs_axiom_status(possession_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('cfb04da1-b448-4af8-91f3-a7b12e24da2a', possession_categorically_impermissible, deontological).
narrative_ontology:cs_axiom('cfb04da1-b448-4af8-91f3-a7b12e24da2a', foundational, dual_use_risk_voids_peaceful_use_guarantee).
narrative_ontology:cs_axiom_status(dual_use_risk_voids_peaceful_use_guarantee, holdable).
narrative_ontology:cs_axiom_grounding('cfb04da1-b448-4af8-91f3-a7b12e24da2a', dual_use_risk_voids_peaceful_use_guarantee, empirically_contingent).
narrative_ontology:cs_reference_frame('cfb04da1-b448-4af8-91f3-a7b12e24da2a', npt_grand_bargain_1968_framework).
narrative_ontology:cs_drift_state('cfb04da1-b448-4af8-91f3-a7b12e24da2a', post_tpnw_ratification_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('cfb04da1-b448-4af8-91f3-a7b12e24da2a', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__abolitionist, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__abolitionist, npt_administering_bodies).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, non_nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, atomic_bomb_survivor_communities).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, tpnw_signatory_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The five NPT-recognized weapon states administer the treaty's verification and export-control architecture, retain their arsenals under Article VI's non-justiciable 'good faith negotiation' language, and use continued NPT membership to claim legal legitimacy for possession while facing no binding disarmament timeline. From the abolitionist reading, their persistence in the regime perpetuates the very dual-use proliferation risk Article IV was meant to bound, and their veto over verification and enforcement bodies lets them treat disarmament as aspirational indefinitely.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, nuclear_weapon_states, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__abolitionist, nuclear_weapon_states, beneficiary).

% The IAEA and Review Conference apparatus derive their institutional mandate, funding, and diplomatic relevance from administering the Article IV/VI trade precisely because it remains unresolved; a completed disarmament regime under TPNW-style prohibition would displace much of their verification function onto a different treaty architecture. They benefit from the pairing's continued ambiguity.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, npt_administering_bodies, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__abolitionist, npt_administering_bodies, agenda_setter).

% Accepted permanent non-acquisition obligations under Article II in exchange for a promised path to disarmament (Article VI) and peaceful nuclear technology access (Article IV). Under the abolitionist reading, they have been paying the compliance cost of Article II for five decades while Article VI performance has not materialized and Article IV technology transfer has been slow-walked on proliferation-risk grounds — the same dual-use logic the abolitionist reading rejects as insufficient justification for continued possession.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, non_nuclear_weapon_states, payer,
    moderate, generational, constrained, global).

% Hibakusha and other survivor and downwinder communities bear the embodied humanitarian cost the entire prohibition norm is built to prevent recurring. They have no formal seat in NPT review architecture; their testimony and advocacy fed directly into the TPNW's humanitarian-consequences framing, which this reading treats as the superior source of legal authority over NPT's diplomatic bargain structure. They cannot exit the harm already suffered and have no institutional lever inside the NPT process itself.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, atomic_bomb_survivor_communities, payer,
    powerless, civilizational, trapped, global).

% States that have ratified the Treaty on the Prohibition of Nuclear Weapons operate under the categorical-illegality norm this reading treats as authoritative, but remain formally outside NPT's institutional levers (Review Conferences, IAEA safeguards architecture) and are treated by the nuclear weapon states as a parallel, non-binding regime. They pay the diplomatic cost of pursuing prohibition while lacking power to compel weapon-state compliance through either treaty.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, tpnw_signatory_states, payer,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__abolitionist, tpnw_signatory_states, excluded).

% Organizations like ICAN that built the humanitarian-consequences case underlying TPNW and this reading's authority claim have no formal negotiating status inside NPT Review Conferences; they can observe and lobby but cannot bind outcomes. Their argument — that Article IV's dual-use technology transfers keep proliferation risk structurally alive regardless of Article III verification — is precisely the premise this reading adopts, yet the forum where it would be adjudicated excludes them.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, civil_society_disarmament_coalitions, excluded,
    powerless, generational, constrained, global).

% Assess whether humanitarian law and weapons-prohibition treaty precedent (landmines, cluster munitions, chemical/biological weapons conventions) supply a legitimate interpretive authority that supersedes or merely coexists alongside the NPT's diplomatic-bargain structure. Their scholarship is cited by all three kernel readings but adjudicates none of them authoritatively.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, international_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_article_iv_vi_pairing__abolitionist, nuclear_weapon_states).
narrative_ontology:fixing_cost_class(npt_article_iv_vi_pairing__abolitionist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Read charitably, the NPT was meant to coordinate a phased transition: non-weapon states forgo acquisition, weapon states disarm over time, and all parties share peaceful nuclear technology under safeguards. The abolitionist reading holds that this coordination function has been irreparably captured by weapon-state non-performance on Article VI, so the only remaining legitimate coordination mechanism is the categorical prohibition norm embodied in TPNW.
% TRANSFER_FUNCTION: Under this reading, the arrangement transfers legal cover and diplomatic legitimacy to nuclear weapon states (who retain arsenals while claiming treaty compliance) at the expense of non-weapon states' foregone options and, ultimately, at the expense of anyone exposed to the humanitarian consequences the weapons exist to threaten. Article IV's peaceful-technology sharing is treated as insufficient compensation because it perpetuates dual-use proliferation risk rather than closing it.
% ABSENT_VOICES: Hibakusha and survivor communities, and the civil society coalitions that built the humanitarian-consequences case, are structurally absent from NPT Review Conference decision-making despite their testimony being the normative foundation this reading invokes. TPNW signatory states are present in their own treaty forum but excluded from NPT's institutional levers.
% DISAPPEARANCE_RATIONALE: If the NPT Article IV/VI pairing as currently interpreted disappeared and were replaced by unqualified adherence to the TPNW's categorical prohibition, nuclear weapon states would lose their primary claim to legal legitimacy for continued possession, the IAEA's safeguards-and-technology-transfer function would be restructured or subordinated to a prohibition-verification regime, and non-weapon states would no longer need to accept dual-use technology transfer as an acceptable trade for non-acquisition. The diplomatic architecture built around the 'grand bargain' framing would need to be replaced entirely.
% FOUNDING_PROBLEM: The NPT was built to solve horizontal proliferation while preserving peaceful nuclear energy access, on the promise that vertical disarmament would eventually eliminate the underlying weapons risk entirely. The abolitionist reading holds that the founding problem was mischaracterized from the outset: managing proliferation without eliminating possession leaves the dual-use pathway open indefinitely, so the actual problem — the existence of nuclear weapons as a class of weapon incompatible with humanitarian law — was never addressed.
% FOUNDING_PROBLEM_CORROBORATION: The International Committee of the Red Cross and a substantial body of international humanitarian law scholarship, external to both NPT weapon-state administrators and TPNW advocacy organizations, corroborate that nuclear weapons' foreseeable humanitarian consequences are incompatible with the laws of armed conflict — this is cited as independent support for treating the founding problem as still live and mischaracterized by the NPT's bargain structure. Nuclear weapon states themselves dispute this characterization and maintain the founding problem (horizontal proliferation) has been substantially solved by NPT membership levels.
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__abolitionist, world_rearranges).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__abolitionist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__abolitionist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(npt_article_iv_vi_pairing__abolitionist, 'none', 1).
narrative_ontology:epsilon_provenance(npt_article_iv_vi_pairing__abolitionist, 0.81, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored high (0.81 by 2024) and rising because, under this reading's own premises, every year of continued weapon-state possession under NPT cover without categorical prohibition is itself the extraction — the treaty's diplomatic legitimacy is being drawn down by non-weapon states' patience and compliance while the promised disarmament good is never delivered. Suppression is authored high (0.72) because the NPT Review Conference process, IAEA safeguards architecture, and P5 veto dynamics actively exclude the TPNW's categorical framing from formal adjudication — this is not passive neglect but active institutional structuring. Theater ratio is authored rising sharply (0.25 to 0.58) because Review Conference 'consensus final documents' on disarmament have increasingly become performative language exercises disconnected from any verification or enforcement mechanism, a pattern especially visible after the 1995, 2000, and 2010 Review Conferences produced disarmament commitments with no follow-through machinery. Accessibility collapse is moderate (0.45), reflecting that TPNW itself demonstrates alternatives are NOT fully collapsed — a parallel prohibition treaty exists and has been ratified by dozens of states, so exit from the NPT framing is structurally available even if institutionally marginalized. Resistance is authored high (0.78): TPNW ratification, ICAN's Nobel Prize, and sustained humanitarian-consequences advocacy represent substantial active resistance to the NPT bargain's legitimacy claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear weapon states sit at the beneficiary end: they extract continued legal legitimacy for possession from NPT membership while facing no binding disarmament enforcement, and their arbitrage-grade exit options (they can selectively invoke or ignore Article VI language as convenient) place them structurally near full beneficiary. NPT administering bodies benefit secondarily through institutional persistence tied to the pairing's ambiguity. Non-weapon states, survivor communities, and TPNW signatories are targets: they bear compliance costs, embodied harms, or diplomatic marginalization respectively, with trapped-to-constrained exit options reflecting that leaving the NPT framework (for non-weapon states) risks losing whatever peaceful-technology access Article IV does provide, while survivor communities have no exit from harm already done.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two mirror-image errors. First, treating the NPT bargain as still-functioning coordination (the grand_bargain reading's premise) would understate that, from this reading's vantage, the coordination function was structurally incapable of producing disarmament because weapon states control both the enforcement mechanism and the definition of 'good faith' compliance — a mandate that cannot be held accountable to its own terms has effectively expired even though the institution persists. Second, treating the entire NPT as pure extraction with no coordination residue would ignore that Article IV's peaceful-technology-sharing function, however entangled with dual-use risk, has produced real transfers (medical isotopes, power generation cooperation) that non-weapon states have used — hence tangled_rope rather than snare: there is a genuine coordination function (technology sharing, verification infrastructure) bundled with asymmetric extraction (weapon states extracting legitimacy without delivering the reciprocal obligation), and it requires active enforcement (Review Conference gatekeeping, export control regimes) to hold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    external_authority_supersession,
    'Does humanitarian law and TPNW treaty precedent constitute a legitimate external authority capable of overriding NPT''s own bargain-structure interpretation, or is TPNW merely a parallel regime with no binding force over NPT parties who have not ratified it?',
    'International Court of Justice advisory opinion, or accumulated state practice showing NPT weapon states modifying behavior in response to TPNW''s normative pressure (e.g., changes in doctrine, arsenal posture, or Review Conference concessions attributable to TPNW''s existence).',
    'If TPNW is found to carry genuine interpretive authority over NPT obligations, this reading''s claim that Article IV is conditionally illegitimate gains binding legal force rather than remaining a normative/advocacy position; if TPNW remains a non-binding parallel regime, this reading persists as a contested interpretation rather than settled law.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(external_authority_supersession, conceptual, 'Whether TPNW/humanitarian law authority can bind or merely persuade NPT interpretation.').

omega_variable(
    dual_use_inseparability,
    'Is the dual-use proliferation risk inherent to any peaceful nuclear technology transfer (making Article IV structurally inseparable from weapons risk), or is the risk a function of specific technology types (enrichment, reprocessing) that could be excluded from Article IV sharing while preserving genuinely peaceful applications?',
    'Technical assessment of which nuclear fuel-cycle technologies possess genuine weapons-applicable dual-use characteristics versus those (e.g., medical isotope production, most power-reactor designs with adequate safeguards) that do not.',
    'If dual-use risk is separable by technology type, the abolitionist claim that Article IV is categorically illegitimate weakens to a claim about specific technology categories, closer to the grand_bargain reading''s conditional framing; if inseparable, the categorical illegitimacy claim is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_use_inseparability, empirical, 'Whether Article IV''s dual-use risk is technology-specific or structurally universal.').

omega_variable(
    kernel_framing_alternative_authority_source,
    'This reading grounds authority in humanitarian law and TPNW precedent. An alternative framing could ground the SAME abolitionist substantive position (complete disarmament, Article IV conditionality) in customary international law obligations independent of TPNW ratification status — a framing that would not depend on TPNW''s contested binding force at all.',
    'Legal scholarship tracing whether disarmament-as-customary-law claims predate and are independent of TPNW, versus whether the abolitionist position is definitionally tied to TPNW''s specific prohibition text.',
    'If customary law grounds the same position independently of TPNW, this reading''s authority claim becomes more robust (two independent legal bases rather than one contested treaty); if the position is definitionally TPNW-dependent, the external_authority_supersession omega above becomes the single load-bearing uncertainty for this entire reading''s legitimacy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_alternative_authority_source, conceptual, 'Alternative framing: customary international law vs. TPNW-specific treaty authority as the grounding source.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__abolitionist, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 1970, 0.25).
narrative_ontology:measurement(npt__tr_t1985, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 1985, 0.32).
narrative_ontology:measurement(npt__tr_t1995, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 1995, 0.4).
narrative_ontology:measurement(npt__tr_t2010, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2010, 0.48).
narrative_ontology:measurement(npt__tr_t2017, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2017, 0.54).
narrative_ontology:measurement(npt__tr_t2024, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2024, 0.58).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 1970, 0.55).
narrative_ontology:measurement(npt__be_t1985, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 1985, 0.62).
narrative_ontology:measurement(npt__be_t1995, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 1995, 0.68).
narrative_ontology:measurement(npt__be_t2010, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2010, 0.74).
narrative_ontology:measurement(npt__be_t2017, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2017, 0.78).
narrative_ontology:measurement(npt__be_t2024, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2024, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 1970, 0.5).
narrative_ontology:measurement(npt__su_t1985, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 1985, 0.55).
narrative_ontology:measurement(npt__su_t1995, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 1995, 0.6).
narrative_ontology:measurement(npt__su_t2010, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(npt__su_t2017, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 2017, 0.69).
narrative_ontology:measurement(npt__su_t2024, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__abolitionist, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_article_iv_vi_pairing__abolitionist, 0.1).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing__nonproliferation_primary).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing__grand_bargain).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the npt_article_iv_vi_pairing kernel. nonproliferation_primary treats Article VI as aspirational and grounds authority in weapon-state security interests (lower authored extractiveness, rope/scaffold-leaning). grand_bargain treats Articles IV and VI as reciprocal conditional obligations within NPT's own text (moderate extractiveness, tangled_rope-leaning but with a live coordination story). This abolitionist reading authors the highest extractiveness of the three, because it treats the entire diplomatic-bargain premise as insufficient and grounds authority in an external prohibition norm that delegitimizes NPT's own terms. All three share the same underlying treaty text and institutional actors but diverge on where legitimate authority sits and what performance would satisfy the obligations — per the ε-invariance principle, these are three constraints, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
