% ============================================================================
% CONSTRAINT STORY: jcpoa_treaty_bindingness__binding_multilateral_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jcpoa_treaty_bindingness__binding_multilateral_reading, []).

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
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jcpoa_treaty_bindingness__binding_multilateral_reading
 *   human_readable: JCPOA as Binding Multilateral Treaty (Consensus Modification Reading)
 *   domain: international_law/nuclear_non_proliferation/treaty_compliance
 *
 * SUMMARY:
 *   The JCPOA (Joint Comprehensive Plan of Action) is a multilateral treaty
 *   between Iran and the P5+1 (US, UK, France, Germany, Russia, China) signed
 *   in 2015, capping Iran's nuclear enrichment program in exchange for
 *   sanctions relief. This constraint examines ONE SPECIFIC READING of the
 *   JCPOA's bindingness: the binding multilateral reading, which interprets
 *   the agreement as a lawful, irreversible treaty obligation under
 *   international law (Vienna Convention pacta sunt servanda), enforceable
 *   through automatic sanctions snapback (UNSC Resolution 2231) if Iran
 *   violates enrichment limits. This reading requires consensus-based
 *   modification or dissolution — no single signatory (including the US) can
 *   unilaterally withdraw without violating binding legal obligations. The
 *   binding multilateral reading creates structural asymmetry: Iran's
 *   enrichment flexibility is severely constrained by binding obligation,
 *   while the US and other signatories face binding constraints on unilateral
 *   sanctions reimposition (must maintain consensus). The US 2018 withdrawal
 *   is treated by this reading as illegal breach of binding obligation, not
 *   lawful treaty exit. Alternative readings (transactional_provisional,
 *   graduated_compliance) interpret the JCPOA differently — as a provisional
 *   agreement pending improved conditions, or as a graduated compliance
 *   regime where violations trigger calibrated responses rather than
 *   automatic sanctions. This constraint instantiates only the binding
 *   multilateral reading.
 *
 * KEY AGENTS:
 *   - Iran: Primary victim (powerless/trapped) under binding multilateral reading — enrichment program structurally locked, unilateral exit forbidden by binding obligation, maximum extraction from sanctions and restrictions
 *   - E3+2 Signatories (UK, France, Germany, Russia, China): Organized beneficiaries (organized/constrained) — share verification burden and sanctions leverage, benefit from collective action preventing arms race, but constrained by consensus requirement preventing unilateral defection
 *   - United States: Institutional actor (institutional/mobile) — before 2018 withdrawal, perceived as both beneficiary (verification certainty, alliance credibility) and constrained by consensus requirement; after 2018, treated by binding reading as illegal defector from binding obligation
 *   - IAEA & Joint Commission: Institutional intermediary (institutional/arbitrage) — benefit from enforcement mandate and legitimacy, but recognize framework as potentially temporary/sunset structure
 *   - Iran Enrichment Program: Victim (abstract, powerless) — structurally locked by binding constraint, cannot unilaterally suspend without regime illegitimacy cost
 *   - Non-Proliferation Regime: Abstract beneficiary (powerless) — the global NPT/IAEA system benefits from binding JCPOA enforcement but depends on sustained signatory compliance; any defection (US 2018) degrades regime credibility
 *   - Analytical Observer: Detached position (analytical/analytical) — risks naturalizing contingent institutional reading (binding multilateral) as immutable treaty law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.48).
domain_priors:suppression_score(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.62).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__binding_multilateral_reading, tangled_rope).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__binding_multilateral_reading, "JCPOA as Binding Multilateral Treaty (Consensus Modification Reading)").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__binding_multilateral_reading, "international_law/nuclear_non_proliferation/treaty_compliance").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__binding_multilateral_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__binding_multilateral_reading, 'ea157e9b-5622-4eef-abeb-cf1e0bb9cbcf').
narrative_ontology:cs_kernel_codification('ea157e9b-5622-4eef-abeb-cf1e0bb9cbcf', formalized).
narrative_ontology:cs_authority_grounding('ea157e9b-5622-4eef-abeb-cf1e0bb9cbcf', extraction).
narrative_ontology:cs_interpretation_layer_present('ea157e9b-5622-4eef-abeb-cf1e0bb9cbcf').
narrative_ontology:cs_reading_relation('ea157e9b-5622-4eef-abeb-cf1e0bb9cbcf', jcpoa_treaty_bindingness__transactional_provisional_reading, forecloses).
narrative_ontology:cs_reading_relation('ea157e9b-5622-4eef-abeb-cf1e0bb9cbcf', jcpoa_treaty_bindingness__graduated_compliance_reading, coexists_with).
narrative_ontology:cs_axiom('ea157e9b-5622-4eef-abeb-cf1e0bb9cbcf', foundational, pacta_sunt_servanda_binding).
narrative_ontology:cs_axiom_status(pacta_sunt_servanda_binding, holdable).
narrative_ontology:cs_axiom_grounding('ea157e9b-5622-4eef-abeb-cf1e0bb9cbcf', pacta_sunt_servanda_binding, deontological).
narrative_ontology:cs_axiom('ea157e9b-5622-4eef-abeb-cf1e0bb9cbcf', foundational, unilateral_withdrawal_violates_binding).
narrative_ontology:cs_axiom_status(unilateral_withdrawal_violates_binding, holdable).
narrative_ontology:cs_axiom_grounding('ea157e9b-5622-4eef-abeb-cf1e0bb9cbcf', unilateral_withdrawal_violates_binding, deontological).
narrative_ontology:cs_reference_frame('ea157e9b-5622-4eef-abeb-cf1e0bb9cbcf', treaty_law_pacta_sunt_servanda).
narrative_ontology:cs_drift_state('ea157e9b-5622-4eef-abeb-cf1e0bb9cbcf', post_us_withdrawal_2018, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ea157e9b-5622-4eef-abeb-cf1e0bb9cbcf', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__binding_multilateral_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, multilateral_non_proliferation_regime).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, signatories_committed_compliance).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, iran_enrichment_flexibility).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, us_withdrawal_capability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IRAN TRAPPED BY BINDING MULTILATERAL COMMITMENT (SNARE) — Under this reading, Iran's enrichment program is structurally locked: unilateral suspension or violation triggers automatic multilateral snapback sanctions without requiring individual UNSC member consensus. Iran bears full extraction cost (sanctions, program restrictions) with no unilateral exit path. Maximum suppression because exit via withdrawal would destroy regime legitimacy and isolate the state regionally. The binding multilateral reading structurally maximizes Iran's experienced extraction.
constraint_indexing:constraint_classification(jcpoa_treaty_bindingness__binding_multilateral_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: IRAN AS TREATY PARTICIPANT WITH CONSTRAINED OPTIONS (TANGLED ROPE) — Iran benefits from sanctions relief and normalized trade (coordination function) while bearing ongoing inspections and enrichment caps (extraction). Can formally remain in treaty indefinitely but faces reputational and security costs to withdrawal. Extraction runs asymmetrically toward Iran but coordination is real — both Iran and non-proliferation regime benefit from continued Iranian compliance.
constraint_indexing:constraint_classification(jcpoa_treaty_bindingness__binding_multilateral_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: E3+2 SIGNATORIES (ROPE) — UK, France, Germany, Russia, China perceive the JCPOA as a coordination mechanism solving the collective action problem of Iran verification without requiring military containment. Suppression from Iran is necessary (intrusive inspections, monitoring), but binding multilateral structure prevents unilateral defection by any signatory. Low effective extraction because no single signatory extracts asymmetric benefit — all share verification burden and sanctions leverage. Exit cost is moderate (treaty withdrawal damages credibility on other regimes) but symmetrical.
constraint_indexing:constraint_classification(jcpoa_treaty_bindingness__binding_multilateral_reading, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: IAEA & MULTILATERAL DISPUTE RESOLUTION (SCAFFOLD) — IAEA and multilateral joint commission see the JCPOA as a temporary coordination architecture with sunset possibilities: the treaty contains review gates (every 8 years) and can be superseded by a more comprehensive agreement or rendered moot by regional normalization. Institutional actors benefit from the framework's existence (legitimacy, enforcement mandate) but recognize it as a transitional structure pending broader Middle East arrangements. Theater is moderate — the joint commission's deliberations have genuine verification function, not purely performative.
constraint_indexing:constraint_classification(jcpoa_treaty_bindingness__binding_multilateral_reading, scaffold,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: US INSTITUTIONAL ACTOR IN BINDING COMMITMENT (TANGLED ROPE) — Before 2018 withdrawal, US perceived binding multilateral structure as constraining unilateral action (extraction cost to US power) while delivering verification certainty and alliance credibility (coordination benefit). Benefited from Iran sanctions leverage shared with E3+2. But binding consensus requirement meant US could not unilaterally trigger snapback or withdraw without coalition fracture. This reading assigns US mobile exit options (ability to withdraw politically and diplomatically) but recognizes that withdrawal cost is asymmetric — US bears reputational damage disproportionate to other signatories because US is treated as obligation-breaker.
constraint_indexing:constraint_classification(jcpoa_treaty_bindingness__binding_multilateral_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: NON-PROLIFERATION REGIME (ABSTRACT BENEFICIARY) (SNARE) — The multilateral non-proliferation system itself (NPT, IAEA authority, sanctions consensus) structurally benefits from binding JCPOA interpretation. Treat the regime as a 'powerless' abstract agent: it has no veto, no seat at the table, but bears the cost of defection by any signatory (precedent for unilateral treaty withdrawal, sanctions evasion, verification dodging). The binding multilateral reading locks the regime into dependency on continuous signatory compliance. If any signatory withdraws and re-engages Iran, the regime's enforcement credibility collapses. Suppression is high: no mechanism exists to prevent powerful states from defecting (US 2018 demonstration). Theater is low — the regime's core function (preventing nuclear weapons spread) is genuine, not performative.
constraint_indexing:constraint_classification(jcpoa_treaty_bindingness__binding_multilateral_reading, snare,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: TREATY ARCHITECTURE AS DEGRADED INSTITUTION (PITON) — From a civilizational horizon, the binding multilateral JCPOA reading inherits institutional scaffolding from the Cold War era: UNSC vetoes, consensus voting, permanent member privileges, inspection regimes modeled on arms control treaties between superpowers. This architecture is substantially performative in the post-Cold War multipolar context — UNSC consensus is rarely achievable, permanent members pursue divergent interests, and the framework's legitimacy rests on a legitimacy claim ("consensus" in a Security Council where Russia and China routinely block resolutions) that no longer holds. Theater ratio is high. The binding multilateral reading keeps this degraded ritual alive through inertia rather than function.
constraint_indexing:constraint_classification(jcpoa_treaty_bindingness__binding_multilateral_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — International law theory (pacta sunt servanda, pacta tertiis) holds treaties as binding natural law-equivalents at the civilizational scale: once ratified, treaties create irreversible legal obligations that constrain signatory behavior at civilizational time horizons. This perspective sees the JCPOA binding multilateral structure not as a contingent institutional choice but as an immutable principle of treaty law itself. However, the structural data contradicts this mountain classification: the US 2018 withdrawal demonstrated that binding does not mean immutable — powerful states can unilaterally defect, and the framework's enforcement mechanism (snapback) requires consensus that major powers can block.
constraint_indexing:constraint_classification(jcpoa_treaty_bindingness__binding_multilateral_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jcpoa_treaty_bindingness__binding_multilateral_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(jcpoa_treaty_bindingness__binding_multilateral_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(jcpoa_treaty_bindingness__binding_multilateral_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(jcpoa_treaty_bindingness__binding_multilateral_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(jcpoa_treaty_bindingness__binding_multilateral_reading, TR),
    TR >= 0.70.

:- end_tests(jcpoa_treaty_bindingness__binding_multilateral_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The binding multilateral reading assigns Iran the structural role of enrichment victim — caps, inspections, and sanctions represent extraction. But the reading also acknowledges genuine coordination (sanctions relief, normalized trade). The extractiveness metric reflects asymmetric distribution: Iran bears continuous compliance costs, while signatories bear inspection costs symmetrically. Extraction rises over the interval (0.35 → 0.48) because US 2018 withdrawal shifted the constraint from symmetric (all bound equally) to asymmetric (Iran bound by law, US exited unilaterally). Suppression (0.62): High. Iran's exit options are severely constrained: unilateral withdrawal triggers sanctions snapback (automatic under binding reading), enrichment acceleration triggers snapback, even formal complaints at UNSC face veto by Russia/China. Suppression rises over the interval (0.50 → 0.62) as US withdrawal demonstrated that even attempted consensus (Trump administration's argument for snapback) can fail, leaving Iran with no path to removal of binding constraint short of regime change. Theater ratio (0.55): Moderate. The joint commission's verification and dispute resolution procedures have genuine content (IAEA inspections actually occur, enrichment levels are monitored), but theater rises over interval as UNSC snapback mechanism becomes less credible post-US withdrawal (UNSC consensus increasingly unachievable). The binding multilateral reading's theater derives from the increasingly performative nature of the 'consensus' requirement when permanent members routinely block each other's resolutions.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits extreme perspectival divergence. Iran experiences snare (powerless/trapped) with maximum extraction. E3+2 experience rope (organized/constrained) with genuine coordination. US (before 2018) experienced tangled rope (institutional/mobile) with mixed coordination and constrained extraction. Multilateral institutions experience scaffold (institutional/arbitrage) with sunset logic. The non-proliferation regime experiences snare (abstract powerless victim). Cold War architecture experiences piton (institutional/arbitrage) with high theater. The analytical observer risks mountain (naturalizing contingent institutional choice). The gap between Iran's snare and E3's rope is 2.5+ classification steps — the same 'binding multilateral' constraint is experienced as maximum extraction by one party and as legitimate coordination by another. This gap reveals the binding reading's fundamental asymmetry: it redistributes power from unilateral actors (US) toward multilateral consensus (E3), but the consensus is unstable because permanent members (Russia, China) can block enforcement, making the binding claim vulnerable to great-power defection.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values for each perspective are derived from beneficiary/victim status and exit options. Iran (victim + trapped) = high d (0.85+), experiences high f(d) → high χ. E3+2 (beneficiaries + constrained exit) = low-moderate d (0.40), experiences moderate f(d) → moderate χ. US before 2018 (beneficiary + mobile exit) = low d (0.15), experiences negative f(d) → negative/coordination χ. Non-proliferation regime (victim + trapped) = maximum d (0.95), experiences maximum f(d) → maximum χ but classified as snare because abstract agent cannot organize. IAEA (beneficiary + arbitrage) = very low d (0.05), experiences slightly negative f(d) → near-zero χ. Cold War architecture (institutional + arbitrage) = low d (0.10), but piton classification derives from theater gate, not from high χ. Analytical observer (analytical perspective) = canonical d (0.73), experiences moderate χ but risks misclassifying contingent institutional arrangement as mountain.
 *
 * MANDATROPHY ANALYSIS:
 *   The binding multilateral reading creates a mandatrophy (is this a coordination mechanism that requires extraction, or an extraction mechanism dressed up as coordination?). The Iran perspective resolves it toward snare — Iran sees only extraction, minimal coordination benefit (sanctions relief is conditional and reversible). The E3 perspective resolves it toward rope — genuine coordination of verification burden and sanctions leverage. The analytical observer perspective risks resolving it toward mountain — naturalizing the binding commitment as an immutable principle of treaty law, when the constraint is actually a contingent choice to interpret the JCPOA as binding (rather than provisional or graduated). The US 2018 withdrawal demonstrates the instability: the binding reading cannot actually prevent withdrawal by a powerful signatory, which suggests the 'binding' label masks political accommodation rather than legal constraint. Resolving the mandatrophy requires distinguishing: (1) What does the JCPOA text actually say? (2) How do major signatories actually behave? (3) What is the distribution of power to change the constraint? The binding multilateral reading answers: (1) binding commitment, (2) Iran complies more than signatories do, (3) consensus-based but US can unilaterally exit (as demonstrated). This reveals mandatrophy: the constraint is simultaneously binding (Iran treats it as such) and provisional (US can exit). The reading resolves this by classifying Iran as snare (bound), E3 as rope (coordinating), and US post-2018 as having escaped via illegal defection. Alternative readings (transactional, graduated) would resolve by classifying the entire structure as tangled rope or scaffold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    binding_vs_provisional_interpretation,
    'Does the JCPOA constitute a binding treaty under international law pacta sunt servanda principles, or a provisional agreement contingent on continuous consensus?',
    'Legal analysis of the JCPOA text, ratification instruments, and Vienna Convention Article 26 interpretation. Empirical evidence: US 2018 withdrawal and its international legal response (UNSC resolution attempts, ICJ cases, treaty language invocations). Whether the withdrawal succeeded in its stated legal form (termination) or merely de facto suspended US obligations.',
    'If binding (pacta sunt servanda applies): US 2018 withdrawal was illegal under international law, and JCPOA remains binding on the US. Current snapback sanctions are invalid. Negotiations must restore US participation to restore legitimacy. If provisional (agreement-pending-consensus): US withdrawal was legally valid, and snapback sanctions lack multilateral foundation — they require fresh UNSC consensus. This reading (binding multilateral) assumes the binding interpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(binding_vs_provisional_interpretation, conceptual, 'Distinction between binding treaty and provisional agreement under international law').

omega_variable(
    snapback_mechanism_enforcement,
    'Does the JCPOA snapback sanctions mechanism operate automatically upon Iranian enrichment violation, or does it require fresh UNSC consensus to authorize reimposition?',
    'Legal text analysis: UNSC Resolution 2231 and its snapback clause language. Historical precedent: Did any Iranian violation trigger automatic snapback, or did disputes require fresh UNSC votes? Post-2019 evidence: How have E3 countries invoked snapback authority against Iranian violations?',
    'If automatic: binding multilateral reading is structurally correct — Iran cannot escape consequences. If requiring consensus: binding interpretation collapses because Russia and China can block snapback authorization, making Iran''s extraction risks asymmetric to power dynamics. This reading assumes automatic snapback.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(snapback_mechanism_enforcement, empirical, 'Whether JCPOA snapback operates automatically or requires fresh consensus').

omega_variable(
    unilateral_withdrawal_legal_status,
    'Can a major power lawfully withdraw from the JCPOA unilaterally, or does withdrawal violate its binding obligations under international law?',
    'ICJ advisory opinions or case law on treaty withdrawal. Precedent from US 2018 withdrawal: did international law scholars and courts characterize the withdrawal as lawful, unlawful, or ambiguous? Whether other signatories filed UNSC complaints or ICJ cases against US withdrawal.',
    'If unilateral withdrawal is lawful: binding multilateral reading is weakened — any signatory can exit, and the ''binding'' constraint is really a ''binding unless you withdraw'' constraint. If unilateral withdrawal is illegal: binding reading is strengthened — powerful states cannot unilaterally defect. This reading assumes binding interpretation but acknowledges the legal ambiguity as a key omega.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unilateral_withdrawal_legal_status, conceptual, 'Legal status of unilateral treaty withdrawal under international law').

omega_variable(
    non_proliferation_regime_enforcement_credibility,
    'Does the JCPOA''s binding multilateral structure actually enhance the global non-proliferation regime''s credibility, or does it obscure a hollowed-out consensus that major powers do not uniformly enforce?',
    'Comparative analysis: proliferation violations by non-JCPOA states (North Korea, Syria) and responses vs. JCPOA violations by Iran and responses. Whether sanctions against JCPOA violators are uniformly imposed vs. blocked by particular UNSC members. Whether the binding multilateral label masks asymmetric enforcement.',
    'If binding credibly enforces: non-proliferation regime is genuinely strengthened. If binding obscures weak enforcement: the constraint''s extractiveness is mislabeled — Iran experiences extraction not because the binding is real but because Iran is isolated regardless of JCPOA status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_proliferation_regime_enforcement_credibility, empirical, 'Whether binding multilateral structure credibly enforces non-proliferation').

omega_variable(
    kernel_reading_committer_frame,
    'Is the JCPOA binding because treaty law requires it, or because major powers have chosen to treat it as binding pending regime change or comprehensive Middle East agreement?',
    'Committer-axis framing: analyze statements by US, E3, Russia, China, and Iran on the JCPOA''s legal status. Does each party invoke ''binding international obligation'' or ''agreement-in-effect-until-renegotiated''? Historical analysis: how did each party frame the JCPOA at signature, ratification, and in subsequent disputes?',
    'If binding via law: the binding multilateral reading is correct. If binding via political choice: alternative readings (transactional_provisional, graduated_compliance) are equally defensible. This omega documents that the kernel itself (JCPOA''s binding status) admits multiple readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_committer_frame, conceptual, 'Whether JCPOA bindingness derives from law or political choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__binding_multilateral_reading, 2015, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpoa_bind_theater_2015, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(jcpoa_bind_theater_2018, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 3, 0.52).
narrative_ontology:measurement(jcpoa_bind_theater_2021, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(jcpoa_bind_extract_2015, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(jcpoa_bind_extract_2018, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(jcpoa_bind_extract_2021, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 6, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(jcpoa_bind_supp_2015, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(jcpoa_bind_supp_2018, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 3, 0.58).
narrative_ontology:measurement(jcpoa_bind_supp_2021, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 6, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__binding_multilateral_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, jcpoa_treaty_bindingness__transactional_provisional_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, jcpoa_treaty_bindingness__graduated_compliance_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, iran_sanctions_regime).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, unsc_consensus_enforcement).

% DUAL FORMULATION NOTE:
% The JCPOA's bindingness is contested across three structurally distinct readings: binding_multilateral_reading (this story), transactional_provisional_reading (treats JCPOA as provisional agreement), and graduated_compliance_reading (treats violations as calibrated escalation). These are not perspectives on a single constraint — they are three different constraints sharing the same natural language label ('JCPOA bindingness'). Each has a different epsilon value reflecting different empirical claims about how binding the agreement actually is. The binding_multilateral reading assumes ε=0.48 (moderate extraction from Iran under binding interpretation). The transactional_provisional reading would assume higher ε (more extraction because Iran expects agreement could be abandoned). The graduated_compliance reading would assume different epsilon reflecting proportional response, not automatic snapback. Do not collapse these into perspectives — each is a separate constraint story. The three stories link via network.affects_constraints to show the constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jcpoa_treaty_bindingness__binding_multilateral_reading, institutional, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
