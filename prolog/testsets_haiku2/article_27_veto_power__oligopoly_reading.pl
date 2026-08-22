% ============================================================================
% CONSTRAINT STORY: article_27_veto_power__oligopoly_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_27_veto_power__oligopoly_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: article_27_veto_power__oligopoly_reading
 *   human_readable: P5 Veto Power as Geopolitical Oligopoly Entrenchment
 *   domain: international_relations/institutional_design/constitutional_law
 *
 * SUMMARY:
 *   The UN Security Council Article 27 veto grants permanent members absolute
 *   power to block any substantive resolution, and the UN Charter's amendment
 *   procedure requires unanimity—giving each P5 member a veto over removal of
 *   its own veto. This story reads the veto as a Snare: a mechanism that
 *   collects authority rents for the P5 by preventing the emergence of any
 *   institutional arrangement that would redistribute power, and suppresses
 *   non-P5 agency by making reform impossible. The founding problem
 *   (preventing forced great-power war) is structurally live in the minds of
 *   veto holders but is substantially obsolete in practice; the arrangement
 *   persists because the P5 benefit from entrenchment, not because the
 *   original rationale is still compelling. This is one reading of a
 *   contested kernel (article_27_veto_power); sibling readings frame the veto
 *   as coordination mechanism (sovereignty_reading, coordination_reading)
 *   rather than extraction.
 *
 * KEY AGENTS:
 *   - permanent_five_powers: institutional agenda-setters; control the veto mechanism and the reform barrier; collect authority rents and entrenchment benefits
 *   - non_p5_states: organized payers; constrained by veto, forced to negotiate, blocked from meaningful reform participation
 *   - global_majority: powerless victims; bear consequences of veto-blocked humanitarian action, arms control, accountability
 *   - reform_coalitions: excluded from any effective voice; structurally barred by the veto-of-veto-removal design
 *   - secretary_general: observer; dependent on veto coalition consent, cannot independently reform the system
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__oligopoly_reading, 0.81).
domain_priors:suppression_score(article_27_veto_power__oligopoly_reading, 0.88).
domain_priors:theater_ratio(article_27_veto_power__oligopoly_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__oligopoly_reading, snare).
narrative_ontology:human_readable(article_27_veto_power__oligopoly_reading, "P5 Veto Power as Geopolitical Oligopoly Entrenchment").
narrative_ontology:topic_domain(article_27_veto_power__oligopoly_reading, "international_relations/institutional_design/constitutional_law").

domain_priors:requires_active_enforcement(article_27_veto_power__oligopoly_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__oligopoly_reading, 'b8515543-c5e6-446d-a6cd-0bae6737e5f0').
narrative_ontology:cs_kernel_codification('b8515543-c5e6-446d-a6cd-0bae6737e5f0', formalized).
narrative_ontology:cs_authority_grounding('b8515543-c5e6-446d-a6cd-0bae6737e5f0', extraction).
narrative_ontology:cs_interpretation_layer_present('b8515543-c5e6-446d-a6cd-0bae6737e5f0').
narrative_ontology:cs_reading_relation('b8515543-c5e6-446d-a6cd-0bae6737e5f0', article_27_veto_power__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('b8515543-c5e6-446d-a6cd-0bae6737e5f0', article_27_veto_power__sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('b8515543-c5e6-446d-a6cd-0bae6737e5f0', foundational, veto_as_power_entrenchment_mechanism).
narrative_ontology:cs_axiom_status(veto_as_power_entrenchment_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('b8515543-c5e6-446d-a6cd-0bae6737e5f0', veto_as_power_entrenchment_mechanism, empirically_contingent).
narrative_ontology:cs_axiom('b8515543-c5e6-446d-a6cd-0bae6737e5f0', secondary, amendment_procedure_blocks_institutional_evolution).
narrative_ontology:cs_axiom_status(amendment_procedure_blocks_institutional_evolution, holdable).
narrative_ontology:cs_axiom_grounding('b8515543-c5e6-446d-a6cd-0bae6737e5f0', amendment_procedure_blocks_institutional_evolution, conventional).
narrative_ontology:cs_reference_frame('b8515543-c5e6-446d-a6cd-0bae6737e5f0', veto_as_geopolitical_entrenchment).
narrative_ontology:cs_drift_state('b8515543-c5e6-446d-a6cd-0bae6737e5f0', contemporary_2025, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b8515543-c5e6-446d-a6cd-0bae6737e5f0', '').
narrative_ontology:cs_kernel_id(article_27_veto_power__oligopoly_reading, article_27_veto_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_27_veto_power__oligopoly_reading, permanent_five_powers).
narrative_ontology:constraint_victim(article_27_veto_power__oligopoly_reading, non_p5_states).
narrative_ontology:constraint_victim(article_27_veto_power__oligopoly_reading, global_majority).
narrative_ontology:constraint_victim(article_27_veto_power__oligopoly_reading, reform_coalitions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess formal veto power over all substantive Security Council resolutions; can block any action on any issue; control the body's institutional direction and change-resistance. Justify the veto as protection against forced military engagement; operate it as a tool for blocking resolutions that constrain their regional interests, enable competitors, or would trigger accountability mechanisms. Collect ongoing authority rents: ability to set the agenda, extract concessions from non-P5 states seeking Council action, and entrench their geopolitical status through institutional immutability. Have absolute veto power over any Charter amendment that would remove or modify the veto.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, permanent_five_powers, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Possess one vote each in the General Assembly, zero veto power in the Security Council. Face a standing arrangement where any of the five can unilaterally block action on issues affecting their interests, regardless of majority support or the strength of the case. Must negotiate with P5 for any Council action; pay political and material costs for concessions. Have no formal path to reform the veto system itself—amendment requires unanimity including all five powers, giving each an absolute veto over their own veto removal. Their organized power at the General Assembly level carries no enforcement authority.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, non_p5_states, payer,
    organized, biographical, constrained, global).

% States and populations outside the P5 and their aligned blocs bear the consequences of veto-blocked action: humanitarian crises unaddressed because a veto protects the aggressor, arms control proposals blocked by a permanent member seeking military advantage, accountability mechanisms circumvented, institutional reforms indefinitely frozen because veto holders have no incentive to enable change that would dilute their power. Have no institutional voice and no exit path; are trapped by the constraint's global scope.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, global_majority, payer,
    powerless, generational, trapped, global).

% Groups of non-P5 states, civil society organizations, and international law scholars have proposed reforms (expansion of P5, conditional veto, weighted voting, circumvention via General Assembly) for 80 years. None have succeeded because any reform capable of redistributing power requires Charter amendment, which requires all five permanent members' consent—each has an absolute veto over any change to the system itself. This is structural exclusion: the rule that defines the problem also defines the insurmountable procedural barrier to solving it. Their proposals enter the discourse but never reach the amendment stage.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, reform_coalitions, excluded,
    organized, biographical, trapped, global).

% Oversees the UN bureaucracy and can initiate or highlight issues for Council attention, but has no independent enforcement power and depends on Council votes to authorize actions. Cannot reform the Charter and must operate within veto constraints; has become primarily a diplomatic facilitator among the powers rather than an independent agenda-setter. Dependent on P5 consent for any meaningful Council action.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, secretary_general, observer,
    institutional, biographical, constrained, global).

% The International Court of Justice, ICC, human rights courts, and accountability bodies operate in the gaps the Security Council leaves open. P5 members can block Council referrals, investigations, enforcement actions; can shield allies from prosecution; can veto peacekeeping mandates that would back court judgments. Their institutional autonomy is ultimately subordinate to Council veto. Excluded from any effective voice in whether Council action supports or undermines accountability.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, international_courts_accountability_mechanisms, excluded,
    analytical, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_27_veto_power__oligopoly_reading, permanent_five_powers).
narrative_ontology:fixing_cost_class(article_27_veto_power__oligopoly_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that the Security Council does not compel military action against any nuclear-armed great power; prevents the Council from becoming a tool of an ad hoc majority to impose its will on a permanent member; by requiring consensus among the five, maintains the principle that great powers cannot be bound to collective action they fundamentally reject.
% TRANSFER_FUNCTION: Transfers authority-setting power to the P5: the ability to veto any substantive Council action; the ability to initiate Council sessions and set agenda priorities; the ability to extract political and material concessions from non-P5 states seeking Council support for their interests; the ability to entrench their own institutional status through the reform-blocking logic (veto of veto removal). The transfer is paid by non-P5 states in the form of blocked or delayed action, forced negotiation, reduced institutional voice, and the permanent absence of a path to institutional reform.
% ABSENT_VOICES: The reform coalitions (non-P5 states, civil society, international law scholars proposing Charter amendments) are structurally excluded because any amendment they might succeed in drafting requires the consent of the five powers whose interests it would constrain. The exclusion is total and procedural: the rule that empowers the few simultaneously defines the barrier that locks out reform. Rival frameworks (rotating membership, weighted voting, conditional veto) are proposed in scholarly work and diplomatic negotiations but never reach the amendment stage because the veto coalition blocks them preventatively.
% DISAPPEARANCE_RATIONALE: If the veto disappeared overnight, the Security Council's operational capacity would transform: a two-thirds majority could authorize humanitarian interventions, sanctions regimes, peacekeeping operations, and enforcement actions that any single power had previously blocked. The global distribution of authority would shift markedly—the P5 would lose their standing ability to shape outcomes in their favor and block institutional evolution. Some regional conflicts that have persisted under P5 protection would become subject to international action. The Council would cease to function as a mechanism for great-power consensus and would become a majoritarian body. Non-P5 states and reform coalitions would gain institutional voice; the P5's geopolitical entrenchment would erode.
% FOUNDING_PROBLEM: In 1945, the permanent five powers sought to prevent a recurrence of the League of Nations' collapse by ensuring that any enforcement action undertaken by the global governance body would have the consent of all major powers capable of global military reach. The veto was intended as a safeguard against the UN being used to coerce a great power into a war it rejected, thereby preserving great-power participation in the system itself.
% FOUNDING_PROBLEM_CORROBORATION: The P5 (especially Russia and China, and historically the USSR) attest the founding problem remains live—the veto is still necessary to prevent the Council from becoming a tool against them. Non-P5 states, reform coalitions, scholars of international law, and humanitarian organizations attest that the founding problem is substantially obsolete: the threat of great-power war through forced UN action is minimal compared to the reality that regional conflicts, humanitarian crises, and aggressor protection persist because action is blocked by veto. Independent analyses from outside the benefiting parties (academic consensus in IR, documented positions of non-aligned movements, General Assembly resolutions calling for reform) support the 'founding problem is dead but the arrangement persists' reading.
narrative_ontology:disappearance_verdict(article_27_veto_power__oligopoly_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_27_veto_power__oligopoly_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__oligopoly_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_27_veto_power__oligopoly_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_27_veto_power__oligopoly_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_27_veto_power__oligopoly_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_27_veto_power__oligopoly_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_27_veto_power__oligopoly_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.81 at interval end) and accumulating: the veto's value as an authority-entrenchment tool has increased as geopolitical contexts have shifted. In 1945, the founding problem was acute; by 2025, most security threats are not great-power war but regional conflict, humanitarian crisis, and accountability avoidance—contexts where the veto is deployed less to prevent forced military engagement and more to block action that constrains a P5 member's interests or enables a competitor. Suppression is high (0.88) because the mechanism is actively enforced: veto threats are issued, vetoes are cast, and the amendment procedure is explicitly referenced as the insurmountable barrier to reform. Theater has risen markedly (from 0.12 to 0.42): the P5 increasingly justify vetos by invoking sovereignty and great-power consent principles while the actual function is power entrenchment. The measurement series track the constraint's drift over 80 years: extraction and theater rising as the founding rationale becomes less credible; suppression rising as reform pressure intensifies and the P5 must defend the mechanism more actively.
 *
 * PERSPECTIVAL GAP:
 *   From the P5 seats, the veto is a necessary safeguard against being coerced; from the payer seats (non-P5 organized states), it is a tool of blockade and forced concession-extraction. From the powerless-majority seat, it is structural victimhood without recourse. From the reform-coalition seat, it is total exclusion by design—the rule that defines the problem also defines why solving it is impossible. The engine's per-seat computation should show the P5 computing as beneficiary/low-extraction-cost (d near 0), non-P5 states computing as payers/high-extraction-cost (d near 1), and powerless agents computing as trapped targets. The veto holder's framing (coordination/sovereignty) and the oligopoly reading's framing (extraction/entrenchment) are structurally incompatible within a single framework; they are sibling readings of the same kernel, not observations of the same constraint viewed from different angles.
 *
 * DIRECTIONALITY LOGIC:
 *   The permanent five are the beneficiaries: they collect the ability to shape global outcomes in their favor, extract concessions from non-P5 states seeking Council action, and most critically, entrench their institutional status indefinitely (because any reform requires their unanimous consent). Their directionality is near the beneficiary end (d ~ 0.1-0.2). Non-P5 states are the primary payers: they constrained by the veto and forced to negotiate from a position of structural weakness. Their directionality is near the target end (d ~ 0.85-0.95). The global majority (powerless) are trapped victims with zero effective exit (d ~ 1.0). Reform coalitions are excluded not by force but by procedure—they are structurally locked out of the conversation because the reform mechanism itself is controlled by the very powers reform would constrain. No directionality override is necessary: the structural data (beneficiary/victim, power atoms, exit options) correctly derive the directionalities.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint's founding problem (prevent forced great-power war via UN action) is clearly dead: contemporary security threats are regional conflict, humanitarian crisis, state fragility, and accountability gaps—not great-power military confrontation initiated by UN mandate. Yet the constraint persists and its extractiveness has accumulated. This is the classical mandatrophy case: the arrangement was built to solve a specific problem, the problem has faded, but the beneficiaries have no incentive to remove the arrangement and instead have layered additional extraction onto its structure. The veto now functions primarily as an institutional entrenchment mechanism rather than a great-power safeguard. The theater-ratio rise (0.12 → 0.42) reflects the increasing gap between the justification the P5 offer (sovereignty, great-power consent) and the actual function (power preservation, reform blockade). The constraint classification should flag mandatrophy: P5 benefit from an arrangement whose founding rationale no longer obtains, and the mechanism itself prevents the institutional reform that would address the obsolescence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_temporal_status,
    'Is the founding problem (preventing forced great-power military engagement via UN mandate) genuinely obsolete, or does it retain latent salience despite the shift toward regional and humanitarian crises?',
    'Counterfactual analysis: if the veto mechanism were removed, would great-power military confrontation via UN mandate become significantly more likely? Survey of strategic analyses from military establishments and security studies outside the benefiting parties; historical comparison of great-power behavior under consensus vs. majoritarian governance.',
    'If the founding problem is dead (confirmed obsolete), mandatrophy classification strengthens and extraction characterization hardens. If latent salience is established, the coordination reading gains credibility and extraction may be recharacterized as price of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_temporal_status, empirical, 'Whether the veto''s original justification remains live or has become a cover story.').

omega_variable(
    reform_blockade_intentionality,
    'Is the veto-of-veto-removal deliberately designed as a reform-blocking mechanism, or is it a structural accident of requiring unanimous consent for amendment?',
    'Historical analysis of Charter drafting records and subsequent P5 statements on amendment; examination of whether P5 members have ever seriously entertained reform proposals or treated amendment as an open question.',
    'If deliberately designed for entrenchment, the oligopoly reading is vindicated and extraction characterization is strongly supported. If accidental, the mechanism may be reframed as a side effect of unanimous-consent logic rather than as intentional extraction architecture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_blockade_intentionality, empirical, 'Whether the amendment barrier is a deliberate feature or a structural byproduct.').

omega_variable(
    exit_options_for_non_p5_alternative_institutions,
    'Do non-P5 states have meaningful exit options through alternative institutions (regional organizations, ad hoc coalitions, General Assembly committees), or are these alternatives functionally subordinate to the P5''s Security Council control?',
    'Empirical analysis of non-P5 institutional alternatives'' capacity to authorize and implement enforcement action without Security Council sanction; case studies of humanitarian, peacekeeping, and accountability initiatives pursued outside the Council.',
    'If alternatives are genuinely constraining but viable, exit_options for non-P5 states should be reclassified upward (constrained rather than trapped); directionality would shift downward and extraction would partially decompress. If alternatives are merely theatrical (rubber stamps without enforcement capacity), the trapped classification is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_options_for_non_p5_alternative_institutions, empirical, 'Whether non-P5 agents genuinely have alternative institutional paths or are functionally confined to the Council.').

omega_variable(
    reading_kernel_contest_boundary,
    'Are the three readings (oligopoly, coordination, sovereignty) genuinely distinct constraints instantiated by the same kernel, or do they represent different observer positions on a single constraint?',
    'Structural analysis: do the three readings produce different ε values, different beneficiary/victim sets, and different type classifications? If yes, they are distinct constraints and should be authored as separate stories. If they differ only in justification (same ε, same beneficiaries/victims, same type), they are observer positions on one constraint and should be unified.',
    'If distinct constraints, the kernel framework is correctly applied and each reading should be authored as a separate story with cs_structure.reading_relations. If observer positions on one constraint, the kernel frame is inappropriate and the story should be reframed as a single constraint with perspectival divergence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_contest_boundary, conceptual, 'Whether the three readings are distinct constraints or observer positions on one constraint.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.88) structurally external (P5 actively threatens veto, blocks amendments, enforces procedural barriers) or internalized (non-P5 states have internalized their powerlessness and no longer seriously pursue reform)?',
    'Historical trajectory analysis: did reform efforts decline because the barriers were reinforced, or because non-P5 states stopped trying? Survey of recent reform proposals and P5 responses; analysis of General Assembly rhetoric over time.',
    'If structural, suppression is wielded by the P5 and should persist post-exit (if non-P5 states left the Council, the suppression mechanism would persist for those who remained). If internalized, non-P5 states carry the suppression with them and would need post-exit consciousness-raising to reactivate reform motivation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression is structural coercion or internalized learned helplessness.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__oligopoly_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_27_veto_power__oligopoly_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(arti_tr_t0, observed).
narrative_ontology:measurement(arti_tr_t10, article_27_veto_power__oligopoly_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement_basis(arti_tr_t10, observed).
narrative_ontology:measurement(arti_tr_t20, article_27_veto_power__oligopoly_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement_basis(arti_tr_t20, observed).
narrative_ontology:measurement(arti_tr_t40, article_27_veto_power__oligopoly_reading, theater_ratio, 40, 0.34).
narrative_ontology:measurement_basis(arti_tr_t40, observed).
narrative_ontology:measurement(arti_tr_t60, article_27_veto_power__oligopoly_reading, theater_ratio, 60, 0.38).
narrative_ontology:measurement_basis(arti_tr_t60, observed).
narrative_ontology:measurement(arti_tr_t80, article_27_veto_power__oligopoly_reading, theater_ratio, 80, 0.42).
narrative_ontology:measurement_basis(arti_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_27_veto_power__oligopoly_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(arti_be_t0, observed).
narrative_ontology:measurement(arti_be_t10, article_27_veto_power__oligopoly_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement_basis(arti_be_t10, observed).
narrative_ontology:measurement(arti_be_t20, article_27_veto_power__oligopoly_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement_basis(arti_be_t20, observed).
narrative_ontology:measurement(arti_be_t40, article_27_veto_power__oligopoly_reading, base_extractiveness, 40, 0.72).
narrative_ontology:measurement_basis(arti_be_t40, observed).
narrative_ontology:measurement(arti_be_t60, article_27_veto_power__oligopoly_reading, base_extractiveness, 60, 0.77).
narrative_ontology:measurement_basis(arti_be_t60, observed).
narrative_ontology:measurement(arti_be_t80, article_27_veto_power__oligopoly_reading, base_extractiveness, 80, 0.81).
narrative_ontology:measurement_basis(arti_be_t80, observed).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_27_veto_power__oligopoly_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement_basis(arti_su_t0, observed).
narrative_ontology:measurement(arti_su_t10, article_27_veto_power__oligopoly_reading, suppression_requirement, 10, 0.76).
narrative_ontology:measurement_basis(arti_su_t10, observed).
narrative_ontology:measurement(arti_su_t20, article_27_veto_power__oligopoly_reading, suppression_requirement, 20, 0.8).
narrative_ontology:measurement_basis(arti_su_t20, observed).
narrative_ontology:measurement(arti_su_t40, article_27_veto_power__oligopoly_reading, suppression_requirement, 40, 0.84).
narrative_ontology:measurement_basis(arti_su_t40, observed).
narrative_ontology:measurement(arti_su_t60, article_27_veto_power__oligopoly_reading, suppression_requirement, 60, 0.86).
narrative_ontology:measurement_basis(arti_su_t60, observed).
narrative_ontology:measurement(arti_su_t80, article_27_veto_power__oligopoly_reading, suppression_requirement, 80, 0.88).
narrative_ontology:measurement_basis(arti_su_t80, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_27_veto_power__oligopoly_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_27_veto_power__oligopoly_reading, 0.15).
narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, article_27_veto_power__coordination_reading).
narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, article_27_veto_power__sovereignty_reading).
narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, un_charter_amendment_procedure__veto_lock).
narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, p5_humanitarian_exemption_asymmetry).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the article_27_veto_power kernel. The coordination_reading and sovereignty_reading are sibling constraints instantiated from the same kernel; they are not alternative framings of this constraint but structurally distinct constraints with different ε values, beneficiary/victim structures, and types. The oligopoly_reading treats the veto as extraction-focused (Snare); the coordination_reading treats it as coordination-focused (Rope/Tangled Rope); the sovereignty_reading treats it as a natural-law instantiation (Mountain/Rope). Each reading has its own constraint_id, its own base_properties, and its own classification. The network edge indicates structural influence: the oligopoly reading's ε value depends on the amendment-procedure constraint's design (veto lock makes reform impossible, which is what allows extraction to persist). See audits/2026-06-19_article27_kernel_decomposition for the full decomposition rationale.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
