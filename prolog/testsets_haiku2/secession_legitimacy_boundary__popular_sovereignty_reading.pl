% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__popular_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__popular_sovereignty_reading, []).

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
 *   constraint_id: secession_legitimacy_boundary__popular_sovereignty_reading
 *   human_readable: Popular Sovereignty Secession Legitimacy (Provincial Majority Reading)
 *   domain: political_economy/federalism/constitutional_theory
 *
 * SUMMARY:
 *   This constraint embodies one reading of the legitimacy boundary for
 *   secession: the claim that a democratic majority within provincial
 *   boundaries holds ultimate sovereignty and that a referendum result is
 *   self-legitimating, requiring no federal consent, constitutional
 *   amendment, or external authorization. The reading is instantiated by
 *   provincial majority constituencies and independence movements; it is
 *   contested by the federal center (which claims constitutional
 *   indivisibility), federal minority interests (which would lose access and
 *   leverage), indigenous treaty nations (whose territorial rights predate
 *   and transcend provincial jurisdiction), and much of the international
 *   legal establishment (which treats unilateral secession with caution). The
 *   constraint measures how this reading operates in practice: the extraction
 *   it implies (federal minorities and indigenous nations are overridden),
 *   the suppression required to sustain it (federal legal barriers,
 *   institutional non-recognition, international marginalization), and the
 *   theater it involves (performative sovereignty assertions, symbolic
 *   referenda, constitutional posturing). The measurement series tracks how
 *   the reading's extractiveness has accumulated over time as the provincial
 *   independence movement solidified its framing, while suppression hardened
 *   as the federal center and international order refused recognition.
 *
 * KEY AGENTS:
 *   - Provincial majority constituencies: Hold the referendum machinery; claim ultimate sovereignty; frame federal authority as delegated and subordinate; benefit from the reading's assertion that their preference is self-legitimating.
 *   - Federal union authority: Holds the constitutional text claiming indivisibility; enforces the counter-reading that unilateral secession is unconstitutional; bears the institutional cost if the reading takes hold; argues majoritarianism subordinates constitutional law.
 *   - Federal minority interests: Capital, networks, resource dependency tied to federal union; lose leverage if province exits; cannot hold their own referenda; rely on federal constitutional interpretation.
 *   - Treaty-constrained indigenous nations: Territories span the provincial boundary; treaty rights predate both federal and provincial jurisdiction; held inside the provincial referendum franchise but also subordinated by this reading to provincial majority will; identity-locked to land (cannot exit territory); no escape route from provincial sovereignty claims.
 *   - Federal constitutional interpreters: Courts, legal scholars, constitutional bodies tasked with adjudicating the legitimacy boundary; their interpretation determines whether referendum self-legitimates or requires constitutional amendment; sit at the structural hinge between the readings.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__popular_sovereignty_reading, 0.68).
domain_priors:suppression_score(secession_legitimacy_boundary__popular_sovereignty_reading, 0.71).
domain_priors:theater_ratio(secession_legitimacy_boundary__popular_sovereignty_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__popular_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__popular_sovereignty_reading, "Popular Sovereignty Secession Legitimacy (Provincial Majority Reading)").
narrative_ontology:topic_domain(secession_legitimacy_boundary__popular_sovereignty_reading, "political_economy/federalism/constitutional_theory").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__popular_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__popular_sovereignty_reading, '00dc7932-9fa6-46cf-8c38-6b17df0ad182').
narrative_ontology:cs_kernel_codification('00dc7932-9fa6-46cf-8c38-6b17df0ad182', fixed_text).
narrative_ontology:cs_authority_grounding('00dc7932-9fa6-46cf-8c38-6b17df0ad182', lineage).
narrative_ontology:cs_interpretation_layer_present('00dc7932-9fa6-46cf-8c38-6b17df0ad182').
narrative_ontology:cs_reading_relation('00dc7932-9fa6-46cf-8c38-6b17df0ad182', secession_legitimacy_boundary__constitutional_impossibility_reading, forecloses).
narrative_ontology:cs_reading_relation('00dc7932-9fa6-46cf-8c38-6b17df0ad182', secession_legitimacy_boundary__grievance_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('00dc7932-9fa6-46cf-8c38-6b17df0ad182', secession_legitimacy_boundary__treaty_primacy_reading, influences).
narrative_ontology:cs_axiom('00dc7932-9fa6-46cf-8c38-6b17df0ad182', foundational, democratic_majority_self_legitimates).
narrative_ontology:cs_axiom_status(democratic_majority_self_legitimates, holdable).
narrative_ontology:cs_axiom_grounding('00dc7932-9fa6-46cf-8c38-6b17df0ad182', democratic_majority_self_legitimates, deontological).
narrative_ontology:cs_axiom('00dc7932-9fa6-46cf-8c38-6b17df0ad182', secondary, federal_authority_subordinate_to_popular_will).
narrative_ontology:cs_axiom_status(federal_authority_subordinate_to_popular_will, holdable).
narrative_ontology:cs_axiom_grounding('00dc7932-9fa6-46cf-8c38-6b17df0ad182', federal_authority_subordinate_to_popular_will, instrumental).
narrative_ontology:cs_reference_frame('00dc7932-9fa6-46cf-8c38-6b17df0ad182', popular_sovereignty_principle).
narrative_ontology:cs_drift_state('00dc7932-9fa6-46cf-8c38-6b17df0ad182', contemporary_federal_contestation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('00dc7932-9fa6-46cf-8c38-6b17df0ad182', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_majority_constituencies).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, federal_minority_interests).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, treaty_constrained_indigenous_nations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__popular_sovereignty_reading, international_legal_order).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, federal_union_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Democratic constituencies organized around national identity and independence politics. Control the referendum machinery and political party platforms that instantiate this reading. Frame federal authority as colonial imposition or outdated federalism that subordinates their will to an external center. Can mobilize constituencies, hold symbolic referenda, build political pressure, and claim international legitimacy through invocation of self-determination doctrine. Their exit option is arbitrage: they can shift allegiance to provincial political parties that accept federal authority, though doing so may incur social ostracism in nationalist contexts.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_majority_constituencies, agenda_setter,
    organized, generational, arbitrage, national).

% Federal center holding the constitutional text that claims indivisibility and reserves secession authority to itself (via constitutional amendment). Enforces the counter-reading through constitutional interpretation, refusal to negotiate secession as a fait accompli, international diplomatic non-recognition of unilateral secession. Bears the cost of institutional challenge if the reading spreads (other provinces might invoke it, central authority erodes, federal unity destabilizes). Cannot exit the federal structure itself; exit means surrendering institutional authority. Constrained by the legitimacy damage of blocking 'democratic will' while attempting to maintain constitutional order.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, federal_union_authority, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__popular_sovereignty_reading, federal_union_authority, agenda_setter).

% Capital, corporations, cross-provincial business networks, federal-integrated populations with property and income streams tied to federal union. Depend on federal markets, resource flows, and regulatory consistency. Stand to lose market access, supply chains, and resource leverage if the province exits. Have no franchise in provincial secession referendum (or vote as a minority within the provincial body politic). Can mobilize federal political pressure and litigation but cannot directly challenge the reading through provincial institutions. Constrained exit: they can relocate corporate headquarters or capital, but at substantial cost; individual residents are more trapped.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, federal_minority_interests, payer,
    powerful, biographical, constrained, national).

% Indigenous nations whose territories span the provincial boundary and whose treaty rights predate both provincial and federal jurisdiction. Held inside the provincial electoral franchise (or historically excluded from it entirely) despite maintaining their own governance and territorial claims. The reading subordinates their treaty rights to provincial majority democratic will — a claim they reject as false because their sovereignty predates both the province and the federal union. Cannot exit the territory (land is their identity and governance basis). Cannot avoid the reading's effects (when applied, it overrides their treaty claims). Forced to contest the reading via treaty-primacy invocations, but those are structurally disempowered in a pro-majority-democracy political culture.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, treaty_constrained_indigenous_nations, payer,
    moderate, civilizational, identity_locked, regional).

% The established order of recognized nation-states benefits from stable, internationally accepted boundaries and succession doctrine. Unilateral provincial secession via referendum creates precedent for fragmentation (every region with a majoritarian independence movement could invoke it) and challenges to existing borders globally. The international system benefits from ambiguity: it permits negotiated settlement (bilateral agreement between federal and provincial elites) while discouraging unilateral precedent-setting. Can shift to recognize a seceded province but prefers to avoid establishing a bright-line rule that empowers all subnational majorities. Benefits from this reading remaining contested and performative (no actual secession) rather than operationalized.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, international_legal_order, beneficiary,
    institutional, civilizational, arbitrage, global).

% Courts, constitutional scholars, expert bodies tasked with determining what the constitution allows or requires regarding secession. Their interpretation determines whether this reading's self-legitimating referendum claim stands or collapses. Sit at the structural hinge: accepting this reading implies the constitution can be unilaterally amended by provincial referendum (which dissolves constitutional authority itself); rejecting it implies majoritarian will is subordinate to constitutional text. Face pressure from both sides: provincial majorities invoke democratic legitimacy, federal centers invoke rule of law and constitutional supremacy. Constrained by the professional norm of constitutional interpretation (cannot simply declare the reading correct because it is popular).
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, federal_constitutional_interpreters, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_majority_constituencies).
narrative_ontology:fixing_cost_class(secession_legitimacy_boundary__popular_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the question of when a democratic majority may legitimately exit a federal union: by establishing that referendum result within provincial boundaries is self-legitimating and requires no higher-order constitutional authority beyond the people's vote, it creates a decision procedure for exit that is transparent, majoritarian, and rooted in democratic will rather than elite negotiation or imperial decree.
% TRANSFER_FUNCTION: Transfers constitutional authority from the federal center and the constitutional text to the provincial democratic majority. Moves the power to declare secession from negotiation-dependent (requiring federal consent or constitutional amendment) to unilateral-referendum-dependent (requiring only provincial majority). This shifts leverage from federal institutions to provincial constituency mobilization.
% ABSENT_VOICES: Treaty-holding indigenous nations are inside the provincial boundary but outside the provincial franchise (historically and often currently); their position is structurally excluded from the referendum choice. Diaspora populations of the province, cross-provincial minorities, and future generations are not consulted but stand to inherit the secession result. Their absence from the decision-making process is material: the reading invokes popular sovereignty to justify the exclusion of non-majority and non-resident voices.
% DISAPPEARANCE_RATIONALE: If this reading disappeared — if the referendum result lost self-legitimating force and were reinterpreted as merely expressing preference but not constitutionally dispositive — the provincial majority would lose its unilateral exit option and would face either negotiated constitutional amendment (requiring federal and other provincial consent) or continued union against expressed majority preference. The entire federal structure would rest on different principles: constitutional text as binding and anterior to referenda, or negotiated reform. International secession precedent would be rewritten; disputed territories elsewhere would face different legitimacy conditions.
% FOUNDING_PROBLEM: Pre-democratic empires and monarchies determined territorial boundaries and union membership through dynastic negotiation and conquest. The founding problem is: by what right can an arbitrary center hold a majority that would prefer independence? The reading answers: by none — the people's expressed preference via referendum is the sole legitimate source of authority; any higher-order constraint (constitutional text, federal veto, treaty) that overrides a provincial majority referendum is itself a form of extractive domination.
% FOUNDING_PROBLEM_CORROBORATION: The reading's own proponents (provincial majority constituencies, independence movements) attest the founding problem is live and the reading solves it. The federal center and constitutional tradition contest that the problem requires the solution the reading provides; they argue the problem is solved via constitutional amendment procedures that embed supermajority and inter-provincial negotiation. International law scholars attest the problem exists but dispute whether unilateral referendum legitimacy is the solution (many citing instability risks). No corroborating authority outside the provincial majority movement attests the reading is correct; the disagreement is structural.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__popular_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__popular_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__popular_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(secession_legitimacy_boundary__popular_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__popular_sovereignty_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__popular_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(secession_legitimacy_boundary__popular_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(secession_legitimacy_boundary__popular_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.68 because the reading claims the provincial majority can unilaterally override federal minorities, indigenous treaty rights, and the constitutional text itself — all by invoking majoritarian will. The measurement series shows extractiveness rising from 0.52 at the reading's emergence (when it was one voice among others) to 0.68 as the provincial independence movement solidified and the reading became institutionalized in political practice, party platforms, and legal argument. It plateaus at t=25 (0.68) because after that point the reading's consolidation depends on actual secession (a t>40 event, projected), not on continued rhetorical build-up. Suppression rises sharply from 0.48 to 0.71 as the federal center and international order actively enforced the counter-reading through constitutional interpretation, international non-recognition, and institutional barriers (refusing to negotiate secession as though it were already legitimate). Theater rises from 0.25 to 0.42 because much activity around the reading is performative: symbolic referenda without international binding force, constitutional arguments that presuppose the conclusion, sovereignty assertions that generate no actual state capacity change. The reading is CLAIMED as tangled_rope because it coordinates the provincial majority around a shared conception of self-determination (coordination function) while simultaneously extracting from federal minorities and indigenous nations (asymmetric extraction), and requires active suppression via federal legal barriers and international non-recognition to persist (active enforcement). The claim/metric divergence is intentional: the reading's own proponents would claim it as rope (pure coordination around democratic will) or even mountain (natural law of popular sovereignty), while the authored metrics describe substantially extractive operation sustained by institutional suppression.
 *
 * PERSPECTIVAL GAP:
 *   The provincial majority seat should compute as beneficiary (d near 0.0: it benefits from the reading's assertion of unilateral sovereignty, faces no exit costs, controls the referendum machinery). The federal minority interests seat should compute as target (d near 1.0: the reading extracts from them, they face suppression via institutional non-recognition, their exit is constrained — they cannot hold counter-referenda). The treaty-constrained indigenous nations seat should compute as high-target (d = 1.0: they are identity-locked to the territory the reading claims, have no exit, are held inside the provincial referendum franchise despite governing themselves). The federal center and constitutional interpreters should compute as moderately constrained beneficiaries (d = 0.3–0.5: they have institutional power to block the reading but at high cost, as the reading has mobilized constituencies). This five-way seat divergence is the engine's measure of the constraint; the provincial majority's agenda-setter frame and the federal center's counter-frame generate opposite directionalities from the same structural constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   The provincial majority benefits from this reading (directionality near 0.0 = beneficiary): it claims ultimate sovereignty, can invoke the reading to override federal objections, controls the referendum machinery that instantiates the reading in practice, and faces no institutional penalty for holding this belief within its own territory (until secession is attempted). Federal minorities pay the cost (directionality near 1.0 = target): their interests in federal union are overridden by provincial majority referendum without their consent, and they face suppression of their counter-reading (constitutional impossibility, treaty primacy) via institutional exclusion and legal barriers. Indigenous nations are trapped (directionality = 1.0 = full target): they are identity-locked (cannot exit the territory the reading claims), have no vote in the provincial referendum (or do vote but as a minority within the provincial franchise), and see their treaty rights subordinated to provincial majority will by the logic of this reading. The federal center is moderately constrained (directionality = 0.4–0.6): it holds constitutional text and institutional power but mobilizing these against an entrenched provincial majority is costly and politically delegitimizing (any federal veto looks like suppression of democratic will). This structural asymmetry — that the reading is unilaterally beneficiary for one seat and unilaterally extractive for others — is why it computes as tangled_rope (genuine coordination for the provincial majority, pure extraction for the others, within the same structure) and why the suppression requirement (0.71) is high: the federal center and international order must actively enforce the counter-reading to prevent the provincial reading from taking effect.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how can a provincial majority unilaterally exit an empire/federation that denies them) was live when this reading emerged in pre-independence contexts. In contexts where the founding problem has been solved (the province gained negotiated independence, or its status was formalized), this reading's mandate shifts from solving a real problem to performing sovereignty. The theater_ratio (0.42) measures this drift: 42% of activity around the reading is performative (symbolic referenda, constitutional argument without binding effect, political posturing) rather than functional (actually securing independence). In a context where independence has been secured, the reading becomes piton-like (inertial maintenance of a once-functional claim, now mostly rhetoric). In a context where the province remains contested and unsecured, the reading remains tangled_rope: it coordinates the provincial majority (real coordination function) while extracting from federal minorities (real extraction). Mandatrophy is present when the founding problem is solved but the reading persists — when the province is independent but still invokes the reading as justification for refusing federal authority that no longer exists, or when a negotiated settlement has been reached but the reading is performed as though unilateral secession remained necessary. This story does not declare mandatrophy_resolved because the context is ambiguous: in most real-world instantiations of this reading (Quebec, Scotland, Catalonia, etc.), the founding problem is contested (is independence still necessary, is the reading still functional?) rather than definitively solved or dead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    referendum_majoritarianism_vs_constitutional_protection,
    'Is a provincial democratic majority referendum the correct and complete source of legitimacy for secession, or does it require independent verification against federative constitutional constraints and minority protection?',
    'Test via comparative case analysis: do referenda-based secessions that ignore constitutional amendment procedures (this reading) produce stable, internationally recognized states? Or do they generate ongoing constitutional contestation and delegitimization? Does the presence of supermajority requirements or minority-protection provisions correlate with reduced conflict?',
    'If unilateral referenda produce legitimacy and stability, the reading''s core axiom (democratic majority = self-legitimating) holds. If they produce delegitimacy and ongoing contestation, the constitutional_impossibility reading gains traction — legitimacy requires working within higher-order constitutional authority, not just majority preference. This is the core disagreement between this reading and its siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(referendum_majoritarianism_vs_constitutional_protection, empirical, 'Whether democratic majority referenda self-legitimate exit or require constitutional authorization.').

omega_variable(
    provincial_boundary_vs_overlapping_sovereignty,
    'Is the provincial boundary the correct unit for exercising popular sovereignty in secession, or does popular sovereignty belong to other constituencies: the nation-wide people, the indigenous territory-holders, the cross-provincial minorities resident in the province?',
    'Comparative federalism: how do functioning multi-ethnic federations handle overlapping sovereignty claims when one constituent unit holds a majority that wishes to exit? Do they recognize provincial majority as the authoritative voice, or do they impose pre-conditions (indigenous consent, federal supermajority, international arbitration)?',
    'If provincial majority is the correct unit, this reading stands. If multiple competing sovereignties (indigenous, cross-national, trans-provincial) can claim equal standing, the reading becomes one voice among many, and its self-legitimating force collapses — it becomes merely an assertion by one seat (provincial majority) against other seats (treaty nations, federal minority interests) with no principled way to adjudicate the conflict short of war or international intervention.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(provincial_boundary_vs_overlapping_sovereignty, conceptual, 'The correct unit of popular sovereignty for secession legitimacy in a multi-layered polity.').

omega_variable(
    asymmetric_suppression_internalization,
    'Is the suppression measured in base_properties.suppression (0.71) structural — federal legal barriers, institutional constraints, international non-recognition — or internalized in provincial constituencies — the belief that secession is illegitimate even without external barriers?',
    'Post-secession suppression trajectory: if a province exits and secession barriers are removed, does suppression persist in cross-border provincial minorities and federal-integrated populations? If so, suppression is partly internalized (the provincial majority''s reading has not persuaded these populations; they carry the constraint with them post-exit).',
    'If suppression is mostly structural, the constraint''s effective suppression is the authored 0.71 and the reading is extractive because external enforcement is required. If suppression is internalized, the reading is less extractive for its own constituency (who believe in it) but more extractive for minorities who remain unconvinced — shifting who pays the cost of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asymmetric_suppression_internalization, empirical, 'Whether suppression of the reading is structural or internalized in contested populations.').

omega_variable(
    reading_identity_fusion_in_provincial_majority,
    'Is the provincial majority''s exit option truly available (mobile/arbitrage), or is identity_locked — fused with the nationalist/independence movement so completely that exit from the reading (accepting federal subordination) requires abandoning group identity?',
    'Survey and ethnographic work on provincial majorities: do constituencies that support this reading maintain exit routes — can they shift positions on secession without losing social belonging, political identity, or regional affiliation? Or is the reading a core identity marker such that dissent is treated as regional betrayal?',
    'If identity_locked, the apparent agenda-setter power of the provincial majority is itself constrained; the reading extracts not just from minorities but from the majority itself by fusing policy with identity. This reclassifies the constraint from snare-into-tangled_rope (coordination + extraction) to a more hybrid structure where the agenda-setter is also partially trapped by identity fusion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_identity_fusion_in_provincial_majority, empirical, 'Whether the provincial majority''s commitment to this reading is freely chosen or identity-locked.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__popular_sovereignty_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t0, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(sece_tr_t5, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 5, 0.29).
narrative_ontology:measurement(sece_tr_t10, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 10, 0.34).
narrative_ontology:measurement(sece_tr_t15, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement(sece_tr_t20, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(sece_tr_t25, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(sece_tr_t30, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(sece_tr_t40, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(sece_be_t0, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(sece_be_t5, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 5, 0.56).
narrative_ontology:measurement(sece_be_t10, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(sece_be_t15, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(sece_be_t20, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(sece_be_t25, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(sece_be_t30, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(sece_be_t40, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t0, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(sece_su_t5, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 5, 0.54).
narrative_ontology:measurement(sece_su_t10, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 10, 0.61).
narrative_ontology:measurement(sece_su_t15, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 15, 0.67).
narrative_ontology:measurement(sece_su_t20, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(sece_su_t25, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(sece_su_t30, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(sece_su_t40, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__popular_sovereignty_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(secession_legitimacy_boundary__popular_sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary__constitutional_impossibility_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary__grievance_threshold_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary__treaty_primacy_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, federal_minority_protection_in_contested_secessions).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, indigenous_treaty_override_in_unilateral_exit).

% DUAL FORMULATION NOTE:
% This reading is one of four sibling constraint stories instantiating the secession_legitimacy_boundary kernel. Popular_sovereignty_reading is upstream (more institutionally entrenched in independence movements) but not necessarily more empirically sound than the siblings. The constitutional_impossibility_reading influences this one by creating the institutional barriers the reading must overcome; the treaty_primacy_reading influences this one by creating an alternative legitimacy claim that the reading's majoritarian logic cannot address; the grievance_threshold_reading coexists with this one (both contest the federal center, but locate legitimacy in different places — democratic will vs. structural injustice). All four stories share the same kernel but produce different χ values and classifications from each seat. Consult all four together to model the full boundary dispute.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(secession_legitimacy_boundary__popular_sovereignty_reading, powerful, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
