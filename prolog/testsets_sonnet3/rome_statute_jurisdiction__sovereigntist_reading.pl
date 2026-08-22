% ============================================================================
% CONSTRAINT STORY: rome_statute_jurisdiction__sovereigntist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rome_statute_jurisdiction__sovereigntist_reading, []).

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
 *   constraint_id: rome_statute_jurisdiction__sovereigntist_reading
 *   human_readable: Rome Statute Jurisdiction — Sovereigntist (Consent-Bounded) Reading
 *   domain: international_law/treaty_interpretation/institutional_authority
 *
 * SUMMARY:
 *   This story instantiates the sovereigntist reading of the Rome Statute
 *   jurisdiction kernel: the treaty is read as a strictly bounded consent
 *   instrument, where ICC jurisdiction attaches only to states that ratify
 *   (or specially accept jurisdiction), non-party nationals are categorically
 *   immune except through a UNSC referral that the permanent members can
 *   themselves veto, and complementarity operates as deference to national
 *   courts rather than a standard the Court itself may override. This is a
 *   distinct constraint from the universalist reading (which treats the
 *   Statute as establishing jurisdiction transcending consent, e.g. over
 *   non-party nationals for territorial-state crimes) and from the hybrid
 *   complementarity reading (which treats complementarity as an active
 *   balancing mechanism, not pure deference). Each reading has its own
 *   epsilon, its own beneficiary/victim structure, and its own
 *   classification; this file addresses only the sovereigntist reading.
 *
 * KEY AGENTS:
 *   - unsc_permanent_members: agenda_setter/beneficiary (institutional/arbitrage) — control the referral gate that is the sole path to non-party nationals
 *   - non_party_permanent_powers: beneficiary (institutional/arbitrage) — categorically shielded absent referral
 *   - ratifying_states_national_judiciaries: beneficiary/agenda_setter (institutional/constrained) — retain primary jurisdiction under deferential complementarity
 *   - atrocity_victims_in_non_referred_situations: payer (powerless/trapped) — bear the cost of the jurisdictional gap
 *   - weak_state_judiciaries_facing_selective_referral: payer (moderate/constrained) — asymmetrically exposed relative to non-party shielding
 *   - icc_prosecutor_and_chambers: excluded (institutional/analytical) — institutional voice discounted under this reading's scope limits
 *   - human_rights_ngos_and_victim_advocates: excluded (organized/mobile) — advocate for broader reach, no formal vote
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__sovereigntist_reading, 0.42).
domain_priors:suppression_score(rome_statute_jurisdiction__sovereigntist_reading, 0.38).
domain_priors:theater_ratio(rome_statute_jurisdiction__sovereigntist_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__sovereigntist_reading, tangled_rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__sovereigntist_reading, "Rome Statute Jurisdiction — Sovereigntist (Consent-Bounded) Reading").
narrative_ontology:topic_domain(rome_statute_jurisdiction__sovereigntist_reading, "international_law/treaty_interpretation/institutional_authority").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__sovereigntist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__sovereigntist_reading, '590b4966-72a3-429e-a236-951f95aca7fc').
narrative_ontology:cs_kernel_codification('590b4966-72a3-429e-a236-951f95aca7fc', fixed_text).
narrative_ontology:cs_authority_grounding('590b4966-72a3-429e-a236-951f95aca7fc', lineage).
narrative_ontology:cs_interpretation_layer_present('590b4966-72a3-429e-a236-951f95aca7fc').
narrative_ontology:cs_reading_relation('590b4966-72a3-429e-a236-951f95aca7fc', rome_statute_jurisdiction__universalist_reading, forecloses).
narrative_ontology:cs_reading_relation('590b4966-72a3-429e-a236-951f95aca7fc', rome_statute_jurisdiction__hybrid_complementarity_reading, influences).
narrative_ontology:cs_axiom('590b4966-72a3-429e-a236-951f95aca7fc', foundational, treaty_jurisdiction_requires_state_consent).
narrative_ontology:cs_axiom_status(treaty_jurisdiction_requires_state_consent, holdable).
narrative_ontology:cs_axiom_grounding('590b4966-72a3-429e-a236-951f95aca7fc', treaty_jurisdiction_requires_state_consent, conventional).
narrative_ontology:cs_axiom('590b4966-72a3-429e-a236-951f95aca7fc', foundational, complementarity_is_deference_not_override).
narrative_ontology:cs_axiom_status(complementarity_is_deference_not_override, holdable).
narrative_ontology:cs_axiom_grounding('590b4966-72a3-429e-a236-951f95aca7fc', complementarity_is_deference_not_override, conventional).
narrative_ontology:cs_reference_frame('590b4966-72a3-429e-a236-951f95aca7fc', westphalian_consent_based_treaty_regime).
narrative_ontology:cs_drift_state('590b4966-72a3-429e-a236-951f95aca7fc', post_afghanistan_palestine_jurisprudence_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('590b4966-72a3-429e-a236-951f95aca7fc', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__sovereigntist_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__sovereigntist_reading, non_party_permanent_powers).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__sovereigntist_reading, ratifying_states_national_judiciaries).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__sovereigntist_reading, unsc_permanent_members).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__sovereigntist_reading, atrocity_victims_in_non_referred_situations).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__sovereigntist_reading, weak_state_judiciaries_facing_selective_referral).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__sovereigntist_reading, consent_based_treaty_jurisdiction_doctrine).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__sovereigntist_reading, pacta_tertiis_nec_nocent_nec_prosunt).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold veto power over Security Council referrals, the sole mechanism by which the Court can reach non-party nationals. Several are themselves non-parties to the Statute. They control which atrocity situations reach the Court and which are shielded, and they bear no jurisdiction exposure themselves regardless of referral activity.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, unsc_permanent_members, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__sovereigntist_reading, unsc_permanent_members, beneficiary).

% Never ratified the Statute and treat non-consent as dispositive: their nationals are categorically outside jurisdiction absent a referral they can veto. They can conduct extraterritorial military and security operations without exposure, and they cite the consent principle as settled treaty law rather than a policy choice.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, non_party_permanent_powers, beneficiary,
    institutional, civilizational, arbitrage, global).

% Retain primary jurisdiction over their own nationals under complementarity, which this reading treats as deference to sovereign courts rather than a backstop the Court may override. They can forestall ICC jurisdiction entirely by conducting or claiming to conduct genuine domestic proceedings, and they benefit from the presumption that domestic process satisfies the Statute absent unwillingness or inability.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, ratifying_states_national_judiciaries, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__sovereigntist_reading, ratifying_states_national_judiciaries, agenda_setter).

% Suffer crimes committed by nationals of non-party states outside any referred situation. Under this reading they have no forum: the Court's jurisdiction ends at the consent boundary, the UNSC referral that could reach the perpetrators is blocked by veto, and no domestic prosecution is forthcoming from an unwilling or complicit state of nationality.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, atrocity_victims_in_non_referred_situations, payer,
    powerless, biographical, trapped, local).

% Ratifying states whose situations are the ones actually referred or self-triggered, since their nationals lack the shield non-parties enjoy and their domestic judiciaries are more readily found unwilling or unable. They experience the same consent framework asymmetrically: their consent binds them fully while non-party consent-withholding exempts others.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, weak_state_judiciaries_facing_selective_referral, payer,
    moderate, biographical, constrained, regional).

% Would read complementarity as a standard the Court itself assesses and, in some formulations, as extending to nationals of non-parties where territorial states have consented. Under the sovereigntist reading their assessments of unwillingness or inability, and any theory of jurisdiction over non-party nationals absent referral, are treated as beyond the Statute's conditional bargain — their institutional voice on the scope question is structurally discounted.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, icc_prosecutor_and_chambers, excluded,
    institutional, generational, analytical, global).

% Argue that consent-gating leaves the gravest crimes committed by powerful non-parties permanently unreachable and that this defeats the Statute's stated object and purpose. They lobby, file amicus submissions, and document unaddressed atrocities, but hold no vote in either ratification or UNSC referral.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, human_rights_ngos_and_victim_advocates, excluded,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rome_statute_jurisdiction__sovereigntist_reading, non_party_permanent_powers).
narrative_ontology:fixing_cost_class(rome_statute_jurisdiction__sovereigntist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a treaty-based court that states can join on known and bounded terms — states get a shared forum for prosecuting the gravest crimes without exposing their nationals, or any non-consenting state's nationals, to jurisdiction they never agreed to accept.
% TRANSFER_FUNCTION: Moves prosecutorial exposure onto ratifying states (and, selectively, weaker or already-scrutinized parties) while shielding non-ratifying powers' nationals from any exposure except through a UNSC referral those same powers can veto; the practical cost of unreachable jurisdiction is transferred to victims of crimes committed by shielded nationals.
% ABSENT_VOICES: Victims of crimes committed by non-party nationals in non-referred situations have no seat at the ratification table and no vote at the Security Council; the ICC's own Prosecutor and Chambers, whose institutional interest runs toward broader jurisdictional reach, are treated under this reading as interpreting past the bargain's edge rather than as an authoritative voice on its scope.
% DISAPPEARANCE_RATIONALE: If the strict-consent framework were abandoned tomorrow in favor of the universalist reading, non-party states would face a materially different calculus about extraterritorial operations, ratification incentives would shift (why join if jurisdiction attaches regardless), and the UNSC veto would lose its function as the sole gate to non-party nationals — the entire architecture of who can be reached, and by whom, would reorganize.
% FOUNDING_PROBLEM: States negotiating the Rome Statute in the 1990s needed a jurisdictional formula that could secure enough ratifications to bring the Court into existence at all, given that major military powers would not accept a court with jurisdiction over their nationals absent their consent; strict consent-gating (plus the UNSC referral valve) was the price of getting the treaty adopted rather than stillborn.
% FOUNDING_PROBLEM_CORROBORATION: Delegations from consent-insistent negotiating states and their legal advisers attest the founding problem (treaty viability requiring a consent floor) remains live and structurally necessary. Independent international law scholars outside any state delegation, and the Court's own jurisprudence in cases addressing jurisdiction over non-party nationals via territorial consent, attest that the founding problem was substantially a negotiating compromise rather than a legal necessity, and that the consent-gating now functions primarily to shield powerful non-parties rather than to solve any remaining coordination problem.
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__sovereigntist_reading, world_rearranges).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__sovereigntist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__sovereigntist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rome_statute_jurisdiction__sovereigntist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rome_statute_jurisdiction__sovereigntist_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rome_statute_jurisdiction__sovereigntist_reading_tests).
:- end_tests(rome_statute_jurisdiction__sovereigntist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.42) rather than high because the sovereigntist reading is not itself a coercive extraction machine toward ratifying states — it is a genuine consent-bounded coordination device for the parties who join it. The extraction that exists is asymmetric and structural: it falls on victims in situations the framework structurally cannot reach and on weaker ratifying states who lack the shielding that non-ratification affords powerful states. Suppression (0.38) reflects the active diplomatic and legal work required to hold the consent line against universalist jurisprudential drift — this is not passive; it requires continued advocacy, treaty-interpretation argument, and UNSC veto exercise. Theater ratio (0.30) captures that some of the framework's stated justification (equal sovereign treatment) coexists with a real asymmetry the theater partially obscures: consent-gating is presented as principled uniformity but operates unevenly between powers that can and cannot afford non-ratification.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of a non-party permanent power, this reading is simply accurate treaty law — pacta tertiis, no obligations without consent, full stop; the arrangement looks like a rope, a clean coordination device states may or may not join. From the seat of victims in situations shielded by veto, or from weak ratifying states exposed while powerful non-parties are not, the same consent architecture looks like an asymmetric extraction structure requiring the enforcement work (diplomatic, jurisprudential, veto-exercising) to keep the powerful outside its reach. The engine's per-seat computation should reflect that the identical jurisdictional rule reads as near-rope for arbitrage-exit institutional beneficiaries and as tangled_rope-to-snare-adjacent for trapped, powerless payers.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-party permanent powers and their nationals sit at the clearest beneficiary end: they hold full exit (never bound, jurisdiction only via a referral they can veto) with no offsetting exposure. Ratifying states' national judiciaries benefit from deferential complementarity, gaining the coordination benefit of a functioning court while retaining primary control over their own nationals' exposure. Atrocity victims in non-referred situations sit at the full-target end: they bear the practical cost of the jurisdictional gap with no legal remedy the framework provides — the constraint's operation does not extract from them procedurally, but its boundary leaves them exactly where the harm occurs, uncompensated. Weak ratifying states occupy an intermediate position: nominally symmetric consent, but asymmetric practical exposure relative to non-parties.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — securing enough ratifications to bring a court into existence at all — is contested as to whether it is still live. If the Court is now institutionally stable and widely enough ratified that the original viability concern no longer requires a hard consent floor, the strict-consent architecture may be inertially maintained past its founding function, primarily serving the powers that benefit from remaining outside its reach rather than solving any present coordination problem. Classifying this as tangled_rope rather than snare or rope preserves both halves: there is a genuine coordination function for states that ratify (a real court, real shared standards, real domestic-primacy benefit), and there is a real, identifiable asymmetric extraction (victims in non-referred situations, weaker states' disproportionate exposure) sustained by active enforcement (veto exercise, jurisprudential defense of the consent line). Neither pure-coordination (rope) nor pure-extraction (snare) framing would capture both halves; mandatrophy is not fully resolved because whether the consent floor is still doing founding work, versus now purely shielding non-parties, remains genuinely contested rather than settled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_as_law_vs_negotiating_compromise,
    'Is the strict-consent jurisdictional boundary a settled principle of treaty law that the Rome Statute simply applies, or was it a negotiating compromise specific to 1998 ratification politics that has since been treated as though it were doctrinally compelled?',
    'Comparative analysis of the Rome Statute''s negotiating history (travaux préparatoires) against post-ratification ICC jurisprudence (e.g., Pre-Trial Chamber decisions on jurisdiction over non-party nationals via territorial consent in Afghanistan and Palestine situations) to determine whether the consent floor was treated by drafters as legally mandatory versus politically necessary for adoption.',
    'If it was a negotiating compromise rather than doctrinal necessity, the sovereigntist reading''s claim to represent settled law weakens considerably, and the persistence of strict consent-gating looks more like continued political leverage by powerful non-parties than legal fidelity to the text.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_as_law_vs_negotiating_compromise, conceptual, 'Whether the consent floor is legal necessity or negotiated compromise now treated as doctrine.').

omega_variable(
    founding_problem_still_live,
    'Does the original viability problem (needing a hard consent floor to secure enough ratifications for the Court to exist) still hold now that the Court has 120+ states parties and institutional permanence, or has the founding problem been solved while the consent architecture persists as shield rather than bridge?',
    'Track ratification trends and withdrawal threats over time: if ratification is stable or growing without further consent concessions, the viability problem is likely resolved and the consent floor''s continued strictness is better explained by non-party shielding interest than by founding necessity.',
    'If the founding problem is dead, the sovereigntist reading''s coordination justification weakens relative to its extraction effect, pushing the computed classification toward snare; if genuinely still live (e.g., further erosion would trigger mass withdrawal), the tangled_rope classification''s coordination half is better supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_still_live, empirical, 'Whether the ratification-viability problem the consent floor solved is still active.').

omega_variable(
    kernel_framing_under_determination,
    'Is the more structurally significant framing of this kernel the jurisdictional consent rule itself (the obvious framing, addressed here), or the deeper legitimacy claim layered above it — that a court whose reach depends on great-power veto tolerance can nonetheless claim universal moral authority over ''the gravest crimes of concern to the international community as a whole'' (Rome Statute preamble)? Under the second framing, the sovereigntist reading and the universalist preamble language are in direct tension within the same text.',
    'Textual analysis contrasting the Statute''s operative jurisdictional articles (consent-bounded) against its preambular and Article 1 language (universal concern, ending impunity) to assess whether the instrument''s own self-description forecloses a purely consent-bounded reading of its legitimacy claim, even where its jurisdictional mechanics remain consent-bounded.',
    'If the deeper legitimacy-claim framing is adopted, this constraint''s classification might shift toward including the preamble''s universalist aspiration as a vindicated-proposition tension internal to the sovereigntist reading itself, rather than treating universalism as purely an external sibling reading; this would not change ε for this reading but would sharpen the documented internal tension.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_under_determination, conceptual, 'Whether the operative jurisdictional text or the preambular legitimacy claim is the more decision-relevant framing of the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__sovereigntist_reading, 1998, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_tr_t1998, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 1998, 0.15).
narrative_ontology:measurement(rome_tr_t2002, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2002, 0.18).
narrative_ontology:measurement(rome_tr_t2008, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2008, 0.22).
narrative_ontology:measurement(rome_tr_t2014, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2014, 0.26).
narrative_ontology:measurement(rome_tr_t2020, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2020, 0.28).
narrative_ontology:measurement(rome_tr_t2024, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(rome_be_t1998, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 1998, 0.28).
narrative_ontology:measurement(rome_be_t2002, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2002, 0.32).
narrative_ontology:measurement(rome_be_t2008, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2008, 0.36).
narrative_ontology:measurement(rome_be_t2014, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2014, 0.39).
narrative_ontology:measurement(rome_be_t2020, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2020, 0.41).
narrative_ontology:measurement(rome_be_t2024, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(rome_su_t1998, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 1998, 0.25).
narrative_ontology:measurement(rome_su_t2002, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2002, 0.28).
narrative_ontology:measurement(rome_su_t2008, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2008, 0.31).
narrative_ontology:measurement(rome_su_t2014, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2014, 0.34).
narrative_ontology:measurement(rome_su_t2020, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2020, 0.36).
narrative_ontology:measurement(rome_su_t2024, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2024, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(rome_statute_jurisdiction__sovereigntist_reading, rome_statute_jurisdiction__universalist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__sovereigntist_reading, rome_statute_jurisdiction__hybrid_complementarity_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__sovereigntist_reading, unsc_referral_veto_mechanism).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the rome_statute_jurisdiction kernel. The universalist_reading claims jurisdiction transcending consent (particularly for non-party nationals via territorial-state referral) and would author a lower epsilon for the coordination gain of universal reach but a different victim set (non-party states losing sovereignty control rather than uncompensated atrocity victims). The hybrid_complementarity_reading treats complementarity as an active Court-assessed balance rather than pure deference, producing an intermediate epsilon between the two poles. All three share the same underlying treaty text but diverge in what the text is read to establish; each carries its own beneficiary/victim structure and must not be averaged into a single classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
