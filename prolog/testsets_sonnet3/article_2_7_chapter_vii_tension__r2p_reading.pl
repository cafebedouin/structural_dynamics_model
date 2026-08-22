% ============================================================================
% CONSTRAINT STORY: article_2_7_chapter_vii_tension__r2p_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_2_7_chapter_vii_tension__r2p_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: article_2_7_chapter_vii_tension__r2p_reading
 *   human_readable: Responsibility to Protect (R2P) Reading of Sovereignty-Intervention Tension
 *   domain: international_law/political_philosophy/security_studies
 *
 * SUMMARY:
 *   This constraint instantiates the R2P (Responsibility to Protect) reading
 *   of the Article 2(7)/Chapter VII tension: sovereignty is treated as
 *   conditional on a state's protection of its own population, and systematic
 *   atrocity — genocide, war crimes, ethnic cleansing, crimes against
 *   humanity — triggers a residual international responsibility that can
 *   override the target state's claim to exclusive domestic jurisdiction.
 *   This is one reading of a genuinely contested kernel; the sibling reading
 *   (sovereignty_first_reading, authored separately) holds that sovereignty
 *   is foundational and that intervention requires either consent or a
 *   Chapter VII finding narrowly tied to inter-state aggression, not to
 *   internal conduct. The two readings share the same textual kernel (the UN
 *   Charter's Article 2(7) domestic jurisdiction clause set against Chapter
 *   VII enforcement powers) but produce structurally distinct constraints
 *   with different ε, different beneficiaries, and different victims — they
 *   are not the same claim viewed from two angles.
 *
 * KEY AGENTS:
 *   - persecuted_civilian_populations: Primary beneficiary (powerless/trapped) — dependent on external intervention materializing
 *   - intervening_coalition_states: Agenda-setter and secondary beneficiary (institutional/arbitrage) — selects when and where to invoke the doctrine
 *   - targeted_state_governments: Primary payer (moderate/trapped) — sovereignty claim suspended once atrocity threshold is alleged
 *   - un_security_council: Agenda-setter with veto-based self-exclusion capacity (institutional/constrained)
 *   - international_law_scholars: Analytical observer tracking the gap between doctrine and selective application
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__r2p_reading, 0.68).
domain_priors:suppression_score(article_2_7_chapter_vii_tension__r2p_reading, 0.61).
domain_priors:theater_ratio(article_2_7_chapter_vii_tension__r2p_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_2_7_chapter_vii_tension__r2p_reading, tangled_rope).
narrative_ontology:human_readable(article_2_7_chapter_vii_tension__r2p_reading, "Responsibility to Protect (R2P) Reading of Sovereignty-Intervention Tension").
narrative_ontology:topic_domain(article_2_7_chapter_vii_tension__r2p_reading, "international_law/political_philosophy/security_studies").

domain_priors:requires_active_enforcement(article_2_7_chapter_vii_tension__r2p_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_2_7_chapter_vii_tension__r2p_reading, '6f3365b8-1f0d-4692-9340-c5fa587dedbb').
narrative_ontology:cs_kernel_codification('6f3365b8-1f0d-4692-9340-c5fa587dedbb', fixed_text).
narrative_ontology:cs_authority_grounding('6f3365b8-1f0d-4692-9340-c5fa587dedbb', distributed).
narrative_ontology:cs_reading_relation('6f3365b8-1f0d-4692-9340-c5fa587dedbb', article_2_7_chapter_vii_tension__sovereignty_first_reading, coexists_with).
narrative_ontology:cs_axiom('6f3365b8-1f0d-4692-9340-c5fa587dedbb', foundational, protection_obligation_overrides_domestic_jurisdiction).
narrative_ontology:cs_axiom_status(protection_obligation_overrides_domestic_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('6f3365b8-1f0d-4692-9340-c5fa587dedbb', protection_obligation_overrides_domestic_jurisdiction, deontological).
narrative_ontology:cs_axiom('6f3365b8-1f0d-4692-9340-c5fa587dedbb', secondary, systematic_atrocity_as_objective_intervention_trigger).
narrative_ontology:cs_axiom_status(systematic_atrocity_as_objective_intervention_trigger, holdable).
narrative_ontology:cs_axiom_grounding('6f3365b8-1f0d-4692-9340-c5fa587dedbb', systematic_atrocity_as_objective_intervention_trigger, empirically_contingent).
narrative_ontology:cs_reference_frame('6f3365b8-1f0d-4692-9340-c5fa587dedbb', post_charter_absolute_domestic_jurisdiction).
narrative_ontology:cs_drift_state('6f3365b8-1f0d-4692-9340-c5fa587dedbb', post_2005_world_summit_endorsement, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('6f3365b8-1f0d-4692-9340-c5fa587dedbb', '').
narrative_ontology:cs_kernel_id(article_2_7_chapter_vii_tension__r2p_reading, article_2_7_chapter_vii_tension).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__r2p_reading, persecuted_civilian_populations).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__r2p_reading, intervening_coalition_states).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__r2p_reading, targeted_state_governments).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__r2p_reading, westphalian_sovereignty_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face systematic atrocity — genocide, ethnic cleansing, war crimes, crimes against humanity — from their own state or from a state's collapse into non-protection. Under this reading, their exposure activates a residual international responsibility that overrides the target state's exclusive jurisdiction claim. They have no independent capacity to compel intervention and depend entirely on the political will of external powers materializing in time.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, persecuted_civilian_populations, beneficiary,
    powerless, immediate, trapped, national).

% Invoke the R2P doctrine to authorize or conduct intervention — military, diplomatic, or through Security Council authorization — against a state committing or failing to prevent atrocity. They select when the threshold is deemed crossed, bear no binding obligation to intervene uniformly, and gain strategic, reputational, or security benefits from selective enforcement. Their exit option is effectively arbitrage: they invoke the norm where interests align and decline where they do not.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, intervening_coalition_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article_2_7_chapter_vii_tension__r2p_reading, intervening_coalition_states, beneficiary).

% Have their claim to exclusive domestic jurisdiction under Article 2(7) suspended once systematic atrocity is alleged. They cannot exit the constraint by asserting sovereignty alone — under this reading sovereignty is conditional and forfeitable. Weaker states with fewer great-power patrons are far more exposed to this suspension than states with Security Council protectors, regardless of the severity of underlying conduct.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, targeted_state_governments, payer,
    moderate, immediate, trapped, national).

% Functions as the background legal-order default that R2P narrows. Each invocation of the R2P reading treats sovereignty as instrumentally conditional rather than foundational, eroding the norm's presumptive strength as a doctrine even where a given intervention is not itself contested. Not an actor; carried here to register that the doctrine itself bears structural cost from repeated conditional carve-outs.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, westphalian_sovereignty_norm, payer,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(article_2_7_chapter_vii_tension__r2p_reading, westphalian_sovereignty_norm).

% Formally the body meant to authorize Chapter VII enforcement action underlying R2P's ‘pillar three.’ Permanent members with veto power can block authorization even where atrocity is well-documented, meaning the Council both administers the doctrine and can exclude itself from acting on it. Non-permanent members and most UN member states have voice in debate but no capacity to force action past a veto.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, un_security_council, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(article_2_7_chapter_vii_tension__r2p_reading, un_security_council, excluded).

% Support R2P rhetorically in the General Assembly but have no veto and no independent enforcement capacity. They bear the precedential risk that R2P's conditional-sovereignty logic could someday be turned on them, without holding any power to shape when and against whom it is invoked.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, weaker_un_member_states, excluded,
    powerless, generational, constrained, global).

% Study the doctrine's application record, documenting the gap between R2P's universalist normative claim and its politically selective invocation (Libya versus Syria versus Myanmar), and assess whether the doctrine is developing into customary international law or remaining a contested political framework.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, international_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_2_7_chapter_vii_tension__r2p_reading, diffuse).
narrative_ontology:fixing_cost_class(article_2_7_chapter_vii_tension__r2p_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a normative framework for the international community to act collectively against mass atrocity when a state either perpetrates it or manifestly fails to prevent it, replacing ad hoc, legally uncertain intervention with an articulated, three-pillar responsibility (state responsibility, international assistance, timely collective response).
% TRANSFER_FUNCTION: Moves the presumptive claim to non-interference away from the targeted state government and toward the intervening coalition and Security Council, and — where intervention succeeds in protecting people — moves physical safety toward the threatened population; where intervention is selective or absent, no such transfer occurs despite equivalent atrocity elsewhere.
% ABSENT_VOICES: Populations in states without great-power patrons or geopolitical salience (e.g., historically under-intervened cases) would object to the doctrine's uneven application but have no forum with binding authority; the targeted government's own domestic constituencies who might oppose an incumbent regime but do not wish for external military intervention are rarely canvassed directly.
% DISAPPEARANCE_RATIONALE: If the R2P reading vanished, some analysts hold that atrocity response would collapse into ad hoc, sovereignty-first inaction (world_rearranges from the perspective of at-risk populations); others hold that R2P has rarely produced binding action beyond what great-power interest already independently generated, so removing the doctrinal label would leave actual state behavior largely unchanged (world_unchanged from the realist critique). The parties to this kernel dispute which world we are actually in.
% FOUNDING_PROBLEM: The Rwandan and Srebrenica failures of the 1990s revealed a normative and institutional gap: the international community had no articulated doctrine authorizing action against a sovereign state actively perpetrating or permitting mass atrocity against its own population, and Article 2(7)'s domestic jurisdiction clause was invoked to justify inaction.
% FOUNDING_PROBLEM_CORROBORATION: UN-commissioned inquiries into the Rwanda and Srebrenica failures (independent of any state benefiting from R2P's later invocation) corroborate that the founding problem — normative and institutional paralysis in the face of documented atrocity — was real. Independent international law scholars and human rights monitors outside both the intervening-coalition and targeted-state camps corroborate that the doctrine has since been invoked selectively (Libya 2011) and withheld in comparably severe cases (Syria, Myanmar), supporting the contested-status reading rather than a clean live/dead determination.
narrative_ontology:disappearance_verdict(article_2_7_chapter_vii_tension__r2p_reading, contested).
narrative_ontology:founding_problem_status(article_2_7_chapter_vii_tension__r2p_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_2_7_chapter_vii_tension__r2p_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_2_7_chapter_vii_tension__r2p_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_2_7_chapter_vii_tension__r2p_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_2_7_chapter_vii_tension__r2p_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__r2p_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_2_7_chapter_vii_tension__r2p_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.68 by 2024) because, under this reading's own lights, the doctrine's selective invocation functions as a mechanism that extracts sovereignty-claim value from weaker states disproportionately — the doctrine is invoked against states without great-power patrons far more readily than against those with them, meaning the 'conditional sovereignty' logic operates asymmetrically even though its normative claim is universal. Suppression (0.61) reflects the coercive apparatus behind actual interventions once invoked — military force, sanctions regimes, occupation authority — which is a raw structural property of the enforcement mechanism and is not scaled by scope in this account (scope amplification is handled separately, downstream, in the engine's χ computation). Theater ratio rises sharply around 2011 (Libya) reflecting the gap between the doctrine's protective rhetoric and its use as post-hoc legitimation for regime-change intervention, then partially recedes as post-Libya backlash constrained subsequent invocations (contributing to the Syria non-intervention outcome). Accessibility collapse is moderate (0.4) — alternative doctrinal framings (strict non-intervention, ad hoc Security Council authorization without R2P vocabulary) remain live and contested, so alternatives have not fully collapsed. Resistance is high (0.78) because targeted states, non-aligned blocs, and sovereignty-first scholars actively contest the doctrine's legal status as customary law.
 *
 * PERSPECTIVAL GAP:
 *   From the intervening-coalition seat, this reading is coordination succeeding where the prior regime failed catastrophically. From the targeted-state seat, the identical structure is an externally imposed suspension of a legal protection they would otherwise hold, applied selectively based on the target's geopolitical alignment rather than the severity of conduct. The engine should compute these as diverging per-seat classifications from the same structural data — the claimed_type (tangled_rope) reflects the analytical synthesis of both readings existing simultaneously within the doctrine's actual operation.
 *
 * DIRECTIONALITY LOGIC:
 *   Persecuted populations sit at the extreme beneficiary end structurally (the doctrine exists in their name) but have no agency over its invocation, which is why their power atom is 'powerless' despite nominal beneficiary status — the benefit is conditional on others' willingness to act. Intervening coalition states carry the actual directional benefit: they set the agenda, select cases, and bear minimal binding obligation, giving them arbitrage-grade exit. Targeted state governments are the clear structural target — their sovereignty claim, which would otherwise be dispositive under a sovereignty-first reading, is suspended by this reading's own logic once the atrocity threshold is alleged, and weaker targeted states (Libya, Syria at various points) have far less capacity to resist that suspension than would a P5-aligned state facing comparable allegations. The sovereignty norm itself is listed as a non-agent payer to register that repeated conditional carve-outs erode the norm's general strength as a legal default, independent of any single case's merits.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — Rwanda/Srebrenica-era paralysis in the face of documented atrocity — was real and is independently corroborated by UN inquiry bodies outside the doctrine's later beneficiaries. Whether that problem remains live or has been supplanted by the doctrine's use as selective political cover is exactly what the founding_problem_status='contested' and disappearance_verdict='contested' fields are built to hold open rather than resolve by authorial fiat. Classifying this as tangled_rope (rather than snare) is the mandatrophy-preventing move: there IS a genuine coordination function (a documented gap in atrocity response was filled), which blocks the story from being mislabeled as pure extraction, while the required beneficiary/victim/enforcement triad captures that the coordination rides alongside asymmetric extraction from weaker targeted states and from the sovereignty norm's general strength.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    customary_law_crystallization_status,
    'Has the R2P reading crystallized into binding customary international law, or does it remain a contested political doctrine invoked selectively at the discretion of powerful states?',
    'State practice and opinio juris analysis across the full population of atrocity situations since 2005 (not just the invoked cases), cross-referenced against ICJ pronouncements and consistent General Assembly voting patterns; a genuinely crystallized customary norm would show consistent invocation criteria independent of the target state''s geopolitical alignment.',
    'If crystallized as binding law, the doctrine''s extraction from weaker states would be better characterized as the application of a genuine, if imperfect, legal norm (moving this reading closer to tangled_rope with the coordination function weighted more heavily). If it remains discretionary political doctrine, the extraction from targeted states is closer to unconstrained major-power extraction dressed in normative vocabulary (moving toward snare).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(customary_law_crystallization_status, empirical, 'Whether R2P has become binding customary law or remains discretionary doctrine.').

omega_variable(
    kernel_framing_locus_of_disagreement,
    'Where exactly does the r2p_reading and sovereignty_first_reading disagreement live: is it a disagreement about the correct interpretation of the Charter text (interpretive), or a disagreement about which normative value (human protection vs. non-interference) should govern when the text is genuinely silent (normative)?',
    'Close textual and travaux-préparatoires analysis of Article 2(7) and Chapter VII drafting history would resolve the interpretive question; it would not resolve the normative question, which is a matter of which value ordering states and scholars choose to prioritize.',
    'If the disagreement is purely interpretive, one reading could in principle be shown determinately correct by exhaustive legal-historical analysis, and the kernel would be less genuinely contested than modeled. If normative, the two readings are irreducibly and permanently coexisting, which supports classifying both readings as `coexists_with` rather than treating one as a transitional error correcting toward the other.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_locus_of_disagreement, conceptual, 'Whether the r2p/sovereignty-first split is interpretive (resolvable) or normative (permanent).').

omega_variable(
    selective_invocation_as_intrinsic_or_contingent,
    'Is the doctrine''s selective, power-correlated invocation pattern (Libya yes, Syria no, Myanmar no) an intrinsic feature of any atrocity-response doctrine operating within a Security Council structure with veto powers, or a contingent, correctable defect of the current institutional design?',
    'Comparative institutional analysis: would a reformed authorization mechanism (e.g., a veto-restraint pledge, or a General Assembly-based Uniting for Peace pathway) produce materially less selective outcomes in modeled or historical counterfactual cases?',
    'If intrinsic to any veto-gated system, the extraction from weaker targeted states is a structural feature of the doctrine as such, supporting a stable tangled_rope classification. If contingent and correctable, the current high extractiveness score reflects a fixable institutional defect rather than the doctrine''s essential character, and a reformed doctrine might score substantially lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_invocation_as_intrinsic_or_contingent, empirical, 'Whether selective enforcement is intrinsic to Security Council structure or a correctable design flaw.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_2_7_chapter_vii_tension__r2p_reading, 2001, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t2001, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2001, 0.25).
narrative_ontology:measurement(arti_tr_t2005, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2005, 0.3).
narrative_ontology:measurement(arti_tr_t2011, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2011, 0.35).
narrative_ontology:measurement(arti_tr_t2015, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2015, 0.48).
narrative_ontology:measurement(arti_tr_t2020, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2020, 0.45).
narrative_ontology:measurement(arti_tr_t2024, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(arti_be_t2001, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2001, 0.35).
narrative_ontology:measurement(arti_be_t2005, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2005, 0.42).
narrative_ontology:measurement(arti_be_t2011, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2011, 0.66).
narrative_ontology:measurement(arti_be_t2015, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2015, 0.6).
narrative_ontology:measurement(arti_be_t2020, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2020, 0.64).
narrative_ontology:measurement(arti_be_t2024, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t2001, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2001, 0.4).
narrative_ontology:measurement(arti_su_t2005, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2005, 0.45).
narrative_ontology:measurement(arti_su_t2011, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2011, 0.58).
narrative_ontology:measurement(arti_su_t2015, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2015, 0.62).
narrative_ontology:measurement(arti_su_t2020, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2020, 0.6).
narrative_ontology:measurement(arti_su_t2024, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2024, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_2_7_chapter_vii_tension__r2p_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__r2p_reading, sovereignty_first_reading).

% DUAL FORMULATION NOTE:
% This story and sovereignty_first_reading are sibling readings of the article_2_7_chapter_vii_tension kernel, decomposed per the ε-invariance principle rather than modeled as one constraint with a measurement parameter. The r2p_reading authors high ε (0.68) with beneficiaries = persecuted populations and victims = targeted states and the sovereignty norm; the sovereignty_first_reading authors a substantially different ε and inverted beneficiary/victim structure (sovereignty-holding states as beneficiaries, would-be intervention targets' populations as the group left unprotected). Each story stands as an independently coherent, ε-invariant constraint from its own reading's structural premises; neither story averages over or references the other's internal metrics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
