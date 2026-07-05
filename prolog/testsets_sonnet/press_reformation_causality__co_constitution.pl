% ============================================================================
% CONSTRAINT STORY: press_reformation_causality__co_constitution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causality__co_constitution, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: press_reformation_causality__co_constitution
 *   human_readable: Print-Economy/Reformation Feedback Infrastructure (Co-Constitution Reading)
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   Between roughly 1450 and 1600, the mechanical press, the rapidly
 *   professionalizing print trade, and an escalating religious controversy
 *   over Church authority co-evolved: printers' commercial incentives shaped
 *   which theological arguments got amplified into which formats, and the
 *   resulting religious mobilization shaped what the print trade found
 *   profitable to produce, in a loop that outran the design intentions of any
 *   single party.
 *
 * KEY AGENTS:
 *   - printer_publishers: commercial beneficiary and co-agenda-setter (organized/mobile) — shapes and is shaped by controversy content
 *   - reforming_clergy: theological beneficiary and co-agenda-setter (moderate/constrained) — adapts rhetoric to print form
 *   - territorial_princes: political beneficiary (institutional/arbitrage) — redirects controversy to consolidate power
 *   - literate_lay_readers: diffuse beneficiary (moderate/constrained) — recruited into print-mediated religious identity
 *   - unlicensed_printers: primary payer (powerless/trapped) — bears enforcement risk without protection
 *   - catholic_dioceses_losing_tithe_revenue: institutional payer (institutional/constrained) — loses revenue and jurisdiction
 *   - religious_minorities_targeted_by_new_print_polemics: excluded payer (powerless/trapped) — collateral target of intensified polemic
 *   - historians_of_technology_and_religion: analytical observer — reconstructs the feedback structure retrospectively
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causality__co_constitution, 0.42).
domain_priors:suppression_score(press_reformation_causality__co_constitution, 0.38).
domain_priors:theater_ratio(press_reformation_causality__co_constitution, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, extractiveness, 0.42).
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causality__co_constitution, scaffold).
narrative_ontology:human_readable(press_reformation_causality__co_constitution, "Print-Economy/Reformation Feedback Infrastructure (Co-Constitution Reading)").
narrative_ontology:topic_domain(press_reformation_causality__co_constitution, "history_of_technology/religious_history/media_studies").

narrative_ontology:has_sunset_clause(press_reformation_causality__co_constitution).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causality__co_constitution, 'e7c2857b-7ab7-49eb-8d77-47f249d7d38f').
narrative_ontology:cs_kernel_codification('e7c2857b-7ab7-49eb-8d77-47f249d7d38f', distributed).
narrative_ontology:cs_authority_grounding('e7c2857b-7ab7-49eb-8d77-47f249d7d38f', distributed).
narrative_ontology:cs_reading_relation('e7c2857b-7ab7-49eb-8d77-47f249d7d38f', press_reformation_causality__technological_determinism, coexists_with).
narrative_ontology:cs_reading_relation('e7c2857b-7ab7-49eb-8d77-47f249d7d38f', press_reformation_causality__strategic_deployment, coexists_with).
narrative_ontology:cs_axiom('e7c2857b-7ab7-49eb-8d77-47f249d7d38f', foundational, causal_weight_is_bidirectionally_distributed).
narrative_ontology:cs_axiom_status(causal_weight_is_bidirectionally_distributed, holdable).
narrative_ontology:cs_axiom_grounding('e7c2857b-7ab7-49eb-8d77-47f249d7d38f', causal_weight_is_bidirectionally_distributed, empirically_contingent).
narrative_ontology:cs_axiom('e7c2857b-7ab7-49eb-8d77-47f249d7d38f', secondary, no_single_actor_class_holds_originating_intent).
narrative_ontology:cs_axiom_status(no_single_actor_class_holds_originating_intent, holdable).
narrative_ontology:cs_axiom_grounding('e7c2857b-7ab7-49eb-8d77-47f249d7d38f', no_single_actor_class_holds_originating_intent, empirically_contingent).
narrative_ontology:cs_reference_frame('e7c2857b-7ab7-49eb-8d77-47f249d7d38f', medieval_church_textual_monopoly).
narrative_ontology:cs_drift_state('e7c2857b-7ab7-49eb-8d77-47f249d7d38f', post_reformation_confessional_settlement, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('e7c2857b-7ab7-49eb-8d77-47f249d7d38f', '').
narrative_ontology:cs_kernel_id(press_reformation_causality__co_constitution, press_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, printer_publishers).
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, reforming_clergy).
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, territorial_princes).
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, literate_lay_readers).
narrative_ontology:constraint_victim(press_reformation_causality__co_constitution, unlicensed_printers).
narrative_ontology:constraint_victim(press_reformation_causality__co_constitution, catholic_dioceses_losing_tithe_revenue).
narrative_ontology:constraint_victim(press_reformation_causality__co_constitution, religious_minorities_targeted_by_new_print_polemics).
narrative_ontology:constraint_vindicates(press_reformation_causality__co_constitution, co_constitution_of_technology_and_agency_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Printing houses in cities like Wittenberg, Basel, and Strasbourg respond to demand for pamphlets and vernacular tracts, but also actively shape what gets written by commissioning cheap, fast-turnaround polemics and choosing which authors to back. They profit from religious controversy as a product category and can relocate operations across jurisdictions when local authorities crack down, but their commercial choices also feed back into which theological arguments circulate widely.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, printer_publishers, beneficiary,
    organized, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__co_constitution, printer_publishers, agenda_setter).

% Figures like Luther adapt their rhetorical style, pamphlet length, and argument structure to what the print economy rewards — short, quotable, reproducible texts — while also using the resulting reach to build a movement that reshapes what printers find profitable to produce. Their theological commitments are not simply amplified by the press; the press's formal demands (brevity, vernacular idiom, serializability) shape how the theology gets expressed. Exit from print dependence is limited once a movement's identity is built on pamphlet circulation.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, reforming_clergy, beneficiary,
    moderate, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__co_constitution, reforming_clergy, agenda_setter).

% Regional rulers leverage the print-fueled religious controversy to assert independence from Rome, redirect church revenues, and consolidate territorial authority. They can selectively license or suppress presses within their domains, giving them more room to maneuver than either printers or clergy, and they benefit from a controversy whose terms they did not fully originate but which they redirect to political ends.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, territorial_princes, beneficiary,
    institutional, generational, arbitrage, regional).

% Urban artisans, merchants, and lower clergy gain direct access to vernacular scripture and polemic for the first time, and their appetite for it (measured in what sells) further shapes print output. They are recruited into a controversy whose print-mediated form partly determines how they understand their own religious choices; they cannot easily step outside the pamphlet culture once literacy and access have reoriented their religious practice around reading.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, literate_lay_readers, beneficiary,
    moderate, biographical, constrained, regional).

% Small print operations without protection from a sympathetic prince or guild face confiscation, fines, or worse when their output angers whichever authority currently holds power in their locality. They bear the costs of a controversy escalating faster than any single jurisdiction's licensing regime can absorb, with little capacity to relocate or diversify their output.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, unlicensed_printers, payer,
    powerless, immediate, trapped, local).

% Ecclesiastical institutions see real revenue and jurisdictional authority erode as territories convert or as princes seize church lands under Reformation pretexts. They can respond with counter-print campaigns and eventually the printing-savvy Counter-Reformation, but the initial cost is borne before that adaptation matures.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, catholic_dioceses_losing_tithe_revenue, payer,
    institutional, generational, constrained, continental).

% Anabaptists, Jews, and other groups become targets of newly circulating printed polemic that both reforming and Catholic authorities produce in competition for legitimacy. The controversy's print-amplified intensity increases the stakes and violence directed at groups outside the two main contending camps, without their having any voice in the controversy's terms.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, religious_minorities_targeted_by_new_print_polemics, payer,
    powerless, immediate, trapped, local).

% Scholars reconstruct the feedback dynamics between print economics, clerical rhetoric, and political opportunism after the fact, weighing this co-constitution reading against determinist and strategic-deployment accounts of the same events.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, historians_of_technology_and_religion, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(press_reformation_causality__co_constitution, diffuse).
narrative_ontology:fixing_cost_class(press_reformation_causality__co_constitution, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The print economy and the reforming movement jointly solve a mutual discovery problem: printers need reliably sellable content and reformers need scalable reach, and the two needs shape each other iteratively — print formats select for certain theological arguments, and theological demand selects for certain print business models.
% TRANSFER_FUNCTION: Attention, literacy-driven purchasing power, and eventually tithe revenue and land move away from the established Church and toward printers, reforming clergy, and the princes who redirect ecclesiastical assets, mediated by a rapidly scaling pamphlet market that no single party fully controls.
% ABSENT_VOICES: Religious minorities caught in the crossfire of escalating print polemic, and unlicensed or itinerant printers destroyed by jurisdictions moving faster than any coordinated regulation of the press, have no seat in either the theological or commercial negotiations that produce the controversy's terms.
% DISAPPEARANCE_RATIONALE: Remove either half of the feedback loop — the print economy's capacity for rapid vernacular reproduction, or the pre-existing theological grievances driving demand for it — and the Reformation's speed, geography, and institutional outcomes change substantially; this reading treats the outcome as jointly produced rather than as the release of a technology's latent potential or as the execution of a strategic plan by any single set of actors.
% FOUNDING_PROBLEM: Neither printers nor reformers set out to co-produce a continental religious realignment: printers were solving a commercial problem (what sells in a new, capital-intensive medium) and reformers/clergy were solving a theological-institutional problem (contesting the Church's authority structure); the co-constitution reading holds that the interaction of these two independent problem-solving processes, not the deliberate design of either, generated the scale and shape of the Reformation.
% FOUNDING_PROBLEM_CORROBORATION: Book-historians (e.g. print-run and format analyses of Wittenberg and Basel output) and comparative historians of failed reform movements without comparable print infrastructure (e.g. earlier Hussite and Wycliffite movements) corroborate that print format shaped argument style and reach independent of any single reformer's intent; this sits outside the self-reports of either reformers (who describe their own agency) or printers (who describe market response), which is why the corroboration is treated as coming from outside the benefiting parties, though the historiographical debate about how much weight to give each side remains genuinely open.
narrative_ontology:disappearance_verdict(press_reformation_causality__co_constitution, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causality__co_constitution, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causality__co_constitution, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(press_reformation_causality__co_constitution, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causality__co_constitution, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causality__co_constitution_tests).
:- end_tests(press_reformation_causality__co_constitution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises sharply through the early controversy period (1450-1525, peaking near the height of the pamphlet war) as the feedback loop intensifies revenue transfer from Church institutions and risk exposure for marginal printers and minorities, then moderates as confessional institutions stabilize and absorb the print economy into routine (rather than crisis) operation by 1600. Theater ratio rises more gradually and persists higher than at the outset — some of what looks like ongoing doctrinal urgency in later decades is institutional performance (confessional identity maintenance) rather than live controversy, consistent with a scaffold whose original transitional function is partially completing while residual performative activity continues. Suppression and resistance are moderate rather than extreme because the feedback structure, unlike either sibling reading's cleaner causal story, distributes coercive capacity across multiple actors (princes, dioceses, guilds) none of whom fully controls the outcome.
 *
 * PERSPECTIVAL GAP:
 *   From the reforming-clergy seat, the press looks like a providential tool serving a theological end already fully formed; from the printer seat, theological content looks like inventory responding to market signals; from the prince seat, the whole controversy looks like an opportunity structure for jurisdictional consolidation. The co-constitution reading is precisely the claim that none of these single-actor perspectives captures the causal structure — the engine's per-seat computation should show meaningfully different classifications from each seat, and that divergence IS the finding, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   No single seat plays pure beneficiary or pure victim across the whole interval — this is the structural signature of co-constitution rather than determinism or strategic deployment. Printers and reforming clergy are beneficiaries who are also partially constrained by the very format demands they exploit (their agency is real but bounded by what the medium rewards). Princes hold the most freedom of maneuver (arbitrage exit) because they can license, tax, or suppress presses within their own territory while remaining outside the commercial/theological feedback loop itself. Unlicensed printers and targeted religious minorities are the clearest victims — trapped, powerless, bearing costs generated by a controversy escalating faster than any single actor designed it to.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold classification with an (implicit rather than legislated) sunset clause captures that the print/controversy feedback loop was a transitional coordination structure: the acute phase of mutual shaping (roughly 1517-1555) gave way to institutionalized confessional print cultures (Lutheran, Catholic Counter-Reformation, Reformed) that no longer required the same intensity of feedback-driven adaptation. Treating this as a permanent extractive structure (snare) would miss that most of the acute dynamics resolved into stable institutions; treating it as pure coordination (rope) would miss the real victims — unlicensed printers and targeted minorities — who bore costs the coordination story does not account for.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    co_constitution_vs_determinism_framing,
    'Is the observed feedback loop genuinely bidirectional co-shaping, or does the causal weight actually run predominantly from the press''s fixed technical properties (movable type''s replicability, portability, cost curve) to the religious outcome, with reformers merely adapting to a technology whose trajectory was already set?',
    'Comparative analysis of pre-print heretical movements (Hussite, Wycliffite) with comparable theological content but without print infrastructure: if those movements'' failure to scale is fully explained by absence of the press alone, that favors technological_determinism; if their failure also depended on different institutional/political receptivity independent of media technology, that favors co_constitution.',
    'If determinism holds, this story''s scaffold classification (transitional co-produced infrastructure) would be wrong and the press should instead be classified as a mountain or rope with the press''s technical properties as the primary independent variable, not codetermined.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(co_constitution_vs_determinism_framing, conceptual, 'Whether causal weight runs bidirectionally (this reading) or predominantly from technology to outcome (sibling reading).').

omega_variable(
    co_constitution_vs_strategic_deployment_framing,
    'How much of the print/controversy escalation reflects genuinely emergent, unplanned feedback versus deliberate strategic coordination by reformers and printers who understood and exploited the medium''s properties from early on (Luther''s own comments on the press suggest considerable strategic awareness)?',
    'Textual analysis of reformers'' and printers'' own correspondence and marginalia for evidence of deliberate media strategy versus post-hoc adaptation; degree of advance planning in pamphlet campaign timing and format standardization.',
    'If strategic awareness was pervasive and effective from early on, the distributed-extraction, no-single-beneficiary structure this story claims would collapse toward a more concentrated tangled_rope or snare with reformers/printers as clear agenda-setters, closer to the strategic_deployment sibling reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(co_constitution_vs_strategic_deployment_framing, empirical, 'Whether the escalation was emergent/unplanned or substantially strategically coordinated by named actors.').

omega_variable(
    sunset_clause_naturalness,
    'Was the co-constitutive feedback loop''s decline into stable confessional print cultures by 1600 an inherent ''sunset'' built into the structure, or was it itself an extractive endpoint (confessional institutions capturing and closing down the earlier, more open feedback dynamic for their own consolidation)?',
    'Trace whether print output diversity and format experimentation declined (consistent with natural sunset/maturation) or whether it was actively suppressed by newly stabilized confessional censorship regimes (consistent with capture, which would argue against calling this scaffold''s sunset benign).',
    'If the decline was itself an extractive closing-down by consolidating institutions, the has_sunset_clause framing understates continuing extraction and the classification should drift toward tangled_rope or snare in the later portion of the interval rather than scaffold throughout.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sunset_clause_naturalness, empirical, 'Whether the apparent resolution of the feedback loop was benign maturation or institutional capture disguised as sunset.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causality__co_constitution, 1450, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1450, press_reformation_causality__co_constitution, theater_ratio, 1450, 0.05).
narrative_ontology:measurement_basis(pres_tr_t1450, observed).
narrative_ontology:measurement(pres_tr_t1480, press_reformation_causality__co_constitution, theater_ratio, 1480, 0.08).
narrative_ontology:measurement_basis(pres_tr_t1480, observed).
narrative_ontology:measurement(pres_tr_t1510, press_reformation_causality__co_constitution, theater_ratio, 1510, 0.12).
narrative_ontology:measurement_basis(pres_tr_t1510, observed).
narrative_ontology:measurement(pres_tr_t1525, press_reformation_causality__co_constitution, theater_ratio, 1525, 0.2).
narrative_ontology:measurement_basis(pres_tr_t1525, observed).
narrative_ontology:measurement(pres_tr_t1550, press_reformation_causality__co_constitution, theater_ratio, 1550, 0.25).
narrative_ontology:measurement_basis(pres_tr_t1550, observed).
narrative_ontology:measurement(pres_tr_t1580, press_reformation_causality__co_constitution, theater_ratio, 1580, 0.24).
narrative_ontology:measurement_basis(pres_tr_t1580, observed).
narrative_ontology:measurement(pres_tr_t1600, press_reformation_causality__co_constitution, theater_ratio, 1600, 0.22).
narrative_ontology:measurement_basis(pres_tr_t1600, observed).

% Extraction over time
narrative_ontology:measurement(pres_be_t1450, press_reformation_causality__co_constitution, base_extractiveness, 1450, 0.12).
narrative_ontology:measurement_basis(pres_be_t1450, observed).
narrative_ontology:measurement(pres_be_t1480, press_reformation_causality__co_constitution, base_extractiveness, 1480, 0.18).
narrative_ontology:measurement_basis(pres_be_t1480, observed).
narrative_ontology:measurement(pres_be_t1510, press_reformation_causality__co_constitution, base_extractiveness, 1510, 0.28).
narrative_ontology:measurement_basis(pres_be_t1510, observed).
narrative_ontology:measurement(pres_be_t1525, press_reformation_causality__co_constitution, base_extractiveness, 1525, 0.45).
narrative_ontology:measurement_basis(pres_be_t1525, observed).
narrative_ontology:measurement(pres_be_t1550, press_reformation_causality__co_constitution, base_extractiveness, 1550, 0.4).
narrative_ontology:measurement_basis(pres_be_t1550, observed).
narrative_ontology:measurement(pres_be_t1580, press_reformation_causality__co_constitution, base_extractiveness, 1580, 0.35).
narrative_ontology:measurement_basis(pres_be_t1580, observed).
narrative_ontology:measurement(pres_be_t1600, press_reformation_causality__co_constitution, base_extractiveness, 1600, 0.32).
narrative_ontology:measurement_basis(pres_be_t1600, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(press_reformation_causality__co_constitution, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causality__co_constitution, information_standard).
narrative_ontology:boltzmann_floor_override(press_reformation_causality__co_constitution, 0.05).
narrative_ontology:affects_constraint(press_reformation_causality__co_constitution, press_reformation_causality__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causality__co_constitution, press_reformation_causality__strategic_deployment).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the press_reformation_causality kernel. co_constitution (this story) classifies the press as a transitional scaffold within multiple simultaneous tangled_rope relationships with distributed extraction and no single beneficiary. technological_determinism classifies the press's spread-enabling properties as closer to fixed/mountain-like infrastructure with the religious outcome as a downstream consequence. strategic_deployment classifies the same events as deliberate tangled_rope or snare-like exploitation by identifiable reformer/printer coalitions with clearer beneficiary concentration. All three share the same historical events but decompose the causal-structural claim into three distinct constraints per the ε-invariance principle: each has a different distribution of extraction across actors and a different account of where agency/inevitability is located.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
