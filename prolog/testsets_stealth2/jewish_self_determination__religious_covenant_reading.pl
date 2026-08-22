% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__religious_covenant_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__religious_covenant_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: jewish_self_determination__religious_covenant_reading
 *   human_readable: Divine-Covenant Reading of Jewish Territorial Sovereignty
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   The religious covenant reading holds that Jewish claim to the land
 *   derives from divine grant, making territorial sovereignty a religious
 *   obligation that stands independent of secular political frameworks: no
 *   negotiated agreement, international ruling, or domestic majority can
 *   alienate what God granted. Activated operationally after 1967, the
 *   reading is instantiated through the settlement enterprise, rabbinical
 *   adjudication of the land's status, and progressive entrenchment in
 *   governing coalitions. The claim/metric gap is deliberate and is the point
 *   of the story: the reading SELF-PRESENTS as immutable divine law (a
 *   mountain-shaped self-description with zero degrees of freedom), while the
 *   authored metrics describe its actual operation — genuine coordination for
 *   the believing community combined with asymmetric cost-bearing imposed on
 *   those who never accepted the premise, sustained by active enforcement.
 *   The engine measures that divergence; nothing here reconciles the claim to
 *   the metrics.
 *
 * KEY AGENTS:
 *   - religious_zionist_settlement_movement: Primary beneficiary (organized/identity_locked) — collects land allocation, state budget, and military protection; exit means abandoning both home and worldview
 *   - national_religious_rabbinical_establishment: Agenda setter (institutional/identity_locked) — issues the rulings that define the land's status; authority depends on the kernel staying unrevised
 *   - israeli_government_coalitions: Enforcing administrator with dual position (institutional/constrained) — administers and funds the arrangement while absorbing its diplomatic and security costs
 *   - palestinian_residents_of_occupied_territories: Primary target (powerless/trapped) — bear the arrangement's territorial consequences; their consent has no standing inside the frame
 *   - secular_israeli_two_state_advocates: Secondary target (moderate/constrained) — their negotiating framework is defined as subordinate to the covenant premise
 *   - palestinian_national_authority: Excluded voice (organized/trapped) — nominally the negotiating counterpart; the frame renders its consent inadmissible
 *   - world_jewish_diaspora_institutions: Diffuse cost-bearer (organized/mobile) — carry reputational and political costs of policies attributed to the Jewish collectivity
 *   - international_legal_institutions: Analytical observer (institutional/analytical) — adjudicate against secular legal frameworks the frame declares itself independent of
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__religious_covenant_reading, 0.74).
domain_priors:suppression_score(jewish_self_determination__religious_covenant_reading, 0.72).
domain_priors:theater_ratio(jewish_self_determination__religious_covenant_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, resistance, 0.57).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__religious_covenant_reading, tangled_rope).
narrative_ontology:human_readable(jewish_self_determination__religious_covenant_reading, "Divine-Covenant Reading of Jewish Territorial Sovereignty").
narrative_ontology:topic_domain(jewish_self_determination__religious_covenant_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_self_determination__religious_covenant_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__religious_covenant_reading, '7b48846c-8dfb-4c16-93e9-1fd94a6017e4').
narrative_ontology:cs_kernel_codification('7b48846c-8dfb-4c16-93e9-1fd94a6017e4', fixed_text).
narrative_ontology:cs_authority_grounding('7b48846c-8dfb-4c16-93e9-1fd94a6017e4', lineage).
narrative_ontology:cs_interpretation_layer_present('7b48846c-8dfb-4c16-93e9-1fd94a6017e4').
narrative_ontology:cs_reading_relation('7b48846c-8dfb-4c16-93e9-1fd94a6017e4', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('7b48846c-8dfb-4c16-93e9-1fd94a6017e4', jewish_self_determination__indigenous_return_reading, coexists_with).
narrative_ontology:cs_reading_relation('7b48846c-8dfb-4c16-93e9-1fd94a6017e4', jewish_self_determination__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('7b48846c-8dfb-4c16-93e9-1fd94a6017e4', jewish_self_determination__diasporist_reading, forecloses).
narrative_ontology:cs_axiom('7b48846c-8dfb-4c16-93e9-1fd94a6017e4', foundational, land_grant_eternal_and_unconditional).
narrative_ontology:cs_axiom_status(land_grant_eternal_and_unconditional, holdable).
narrative_ontology:cs_axiom_grounding('7b48846c-8dfb-4c16-93e9-1fd94a6017e4', land_grant_eternal_and_unconditional, theological).
narrative_ontology:cs_axiom('7b48846c-8dfb-4c16-93e9-1fd94a6017e4', foundational, sovereignty_independent_of_secular_consent).
narrative_ontology:cs_axiom_status(sovereignty_independent_of_secular_consent, holdable).
narrative_ontology:cs_axiom_grounding('7b48846c-8dfb-4c16-93e9-1fd94a6017e4', sovereignty_independent_of_secular_consent, theological).
narrative_ontology:cs_axiom('7b48846c-8dfb-4c16-93e9-1fd94a6017e4', secondary, halakhic_supremacy_over_territorial_question).
narrative_ontology:cs_axiom_status(halakhic_supremacy_over_territorial_question, holdable).
narrative_ontology:cs_axiom_grounding('7b48846c-8dfb-4c16-93e9-1fd94a6017e4', halakhic_supremacy_over_territorial_question, conventional).
narrative_ontology:cs_reference_frame('7b48846c-8dfb-4c16-93e9-1fd94a6017e4', eternal_divine_land_grant).
narrative_ontology:cs_drift_state('7b48846c-8dfb-4c16-93e9-1fd94a6017e4', contemporary_annexationist_mainstreaming, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('7b48846c-8dfb-4c16-93e9-1fd94a6017e4', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__religious_covenant_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__religious_covenant_reading, religious_zionist_settlement_movement).
narrative_ontology:constraint_beneficiary(jewish_self_determination__religious_covenant_reading, national_religious_rabbinical_establishment).
narrative_ontology:constraint_victim(jewish_self_determination__religious_covenant_reading, palestinian_residents_of_occupied_territories).
narrative_ontology:constraint_victim(jewish_self_determination__religious_covenant_reading, secular_israeli_two_state_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jewish_self_determination__religious_covenant_reading, israeli_government_coalitions).
narrative_ontology:constraint_victim(jewish_self_determination__religious_covenant_reading, world_jewish_diaspora_institutions).
narrative_ontology:constraint_vindicates(jewish_self_determination__religious_covenant_reading, covenantal_land_grant_doctrine).
narrative_ontology:constraint_vindicates(jewish_self_determination__religious_covenant_reading, inalienability_of_promised_land).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Builds and inhabits communities across the claimed heartland, receiving land allocation, housing support, infrastructure investment, and military protection. Organizes political pressure against any territorial concession and frames residence itself as fulfillment of obligation. Leaving would mean abandoning both home and the theological account that gives the project its meaning, so departure is not experienced as an available option.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, religious_zionist_settlement_movement, beneficiary,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__religious_covenant_reading, religious_zionist_settlement_movement, agenda_setter).

% Issues rulings that define the land's status, trains the movement's leadership through yeshiva networks, and supplies the religious authorization that settlement policy invokes. Its standing depends on the covenant kernel remaining unrevised: a revised kernel would dissolve its adjudicative role over territorial questions.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, national_religious_rabbinical_establishment, agenda_setter,
    institutional, generational, identity_locked, national).

% Administers the territories, funds and protects the settlement communities, and operates under coalition arithmetic that gives covenant-aligned parties leverage over territorial policy. Simultaneously absorbs the arrangement's external costs: diplomatic isolation, exposure in international legal fora, and the security deployments the settlement map requires. Relinquishing the administrative role would collapse the governing coalition, so the enforcing position is locked in by electoral structure.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, israeli_government_coalitions, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__religious_covenant_reading, israeli_government_coalitions, payer).

% Live under the territorial claim's operational consequences: land appropriation, movement restrictions, expansion of neighboring communities onto or adjacent to their localities, and unequal legal regimes. Their consent plays no role inside the covenant frame, and neither relocation nor local political participation removes the claim's reach. Collective organization exists but has repeatedly failed to alter the arrangement's course.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, palestinian_residents_of_occupied_territories, payer,
    powerless, generational, trapped, regional).

% Pursue negotiated territorial compromise through electoral and diplomatic channels. The covenant premise, held by coalition partners and increasingly embedded in state organs, defines compromise as transgression regardless of its secular merits, which removes their framework's subject matter from the table. Their options narrow to emigration, political realignment, or persistent minority status.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, secular_israeli_two_state_advocates, payer,
    moderate, biographical, constrained, national).

% Nominally the counterpart for territorial negotiation. Inside the covenant frame its consent is structurally inadmissible — agreements signed with it cannot bind what was granted by higher authority — so it participates in diplomacy while having no standing in the operative arrangement. Its objections are audible outside the frame and weightless inside it.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, palestinian_national_authority, excluded,
    organized, generational, trapped, regional).

% Carry reputational and political costs as covenant-maximalist policy is attributed to the Jewish collectivity broadly. Many hold other readings of the shared kernel and object to the covenant frame's operation, but distancing carries communal-rupture costs, so most institutions absorb the association while contesting the theology behind it.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, world_jewish_diaspora_institutions, payer,
    organized, generational, mobile, global).

% Adjudicate the settlement regime against treaty law and Security Council resolutions. Their findings register the arrangement's costs with precision but possess no enforcement channel into a frame that declares itself independent of secular legal authority — the exact boundary the covenant claim draws is the boundary of their jurisdiction.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, international_legal_institutions, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Within religious Zionist communities, the covenant premise solves a real coordination problem: it provides a single authoritative account of why the land matters, what may be done with it, and who decides — replacing ad hoc political judgment with halakhic adjudication, and aligning settlement, education, and political activity around one shared obligation.
% TRANSFER_FUNCTION: Moves territorial control, state budget, and legal protection from the general Israeli public sphere and from Palestinian residents toward settlement institutions and their constituency; and moves decision authority over territorial questions from secular-democratic deliberation to religious adjudication insulated from electoral revision.
% ABSENT_VOICES: Palestinian residents and their representatives would object and do object, but the frame assigns them no standing — their consent is inadmissible by construction, since the claim is held independent of secular agreement. Secular Israeli negotiators are present in the polity but their framework is defined as subordinate. Non-Zionist religious communities and diaspora Jews holding rival readings of the same kernel are talked about more than with. The unanimity of the frame is real inside it and manufactured at its boundary: dissent was never admitted to the room the frame recognizes.
% DISAPPEARANCE_RATIONALE: If the covenant claim vanished overnight, settlement justification would collapse to security and utilitarian arguments that respond to ordinary political revision; coalition arithmetic would lose its religious veto points; annexation momentum would lose its absolute, non-negotiable character; and territorial compromise would become thinkable for the governing system rather than merely for its opposition. The believing community would face a meaning-crisis of the first order. Nearly every arrangement documented in this story depends on the claim's non-revisability.
% FOUNDING_PROBLEM: After 1967, the religious Zionist community confronted an unexpected territorial windfall with no settled doctrine for it: did sovereignty over the heartland fulfill the covenant, violate the traditional deference (the three-oaths posture), or await divine initiative? The covenant reading resolved the crisis by converting territory into immediate religious obligation — and, at depth, answered the older problem of how a halakhic community could relate to modern statehood without surrendering religious authority over collective questions.
% FOUNDING_PROBLEM_CORROBORATION: Independent academic historiography of religious Zionism (accounts of the post-1967 doctrinal shift and the Gush Emunin generation), contemporaneous Knesset records and press archives documenting the sequence from hesitancy to activation, and Palestinian and international legal documentation of the arrangement's effects all attest the founding problem and its transformation. None of these sources belongs to the benefiting parties; the benefiting parties' own attestation (the claim is eternal and self-evidently binding) is precisely the testimony this corroboration check does not count.
narrative_ontology:disappearance_verdict(jewish_self_determination__religious_covenant_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__religious_covenant_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__religious_covenant_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_self_determination__religious_covenant_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__religious_covenant_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__religious_covenant_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__religious_covenant_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_self_determination__religious_covenant_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.74 at interval end) because the arrangement moves territory, budget, and legal protection toward the settlement constituency while removing territorial questions from ordinary democratic revision — the claim's 'independence of secular frameworks' is precisely a removal of decision authority from those affected. Suppression is high (0.72) because persistence depends on active machinery: coalition veto points, state enforcement of the settlement regime, and intra-communal sanction against dissent, not on voluntary assent of the governed. Accessibility collapse is substantial but not total (0.68): for anyone who accepts the covenant premise, alternatives (partition, compromise) collapse almost completely — you cannot trade away a divine grant — while for those outside the premise alternatives remain thinkable, which is why the number sits below mountain-range values. Resistance is real (0.57): the Oslo process, the 2005 disengagement, Palestinian resistance, and international legal consensus all pushed against the arrangement, yet it persisted and later advanced. Theater is moderate-low (0.28): prayer, study, and settlement as religious practice are functionally genuine for participants; the performative share rises when enforcement is blocked (symbolic marches, reenactment politics after 2005) and falls when real power returns. The temporal series show one visible cycle: steady intensification 1967–2000, a disengagement-driven dip at 2005, then renewed ascent. The oscillation is partly the mechanism itself — each setback mobilizes the movement, and the post-crisis consolidation lands higher than the pre-crisis baseline (intermittent reinforcement), which is why the 2025 endpoint exceeds the 2000 peak despite the 2005 defeat. All three metric series share one eight-point grid so no metric row is backfilled from another's endpoints.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and should. From the believing seat (identity_locked, beneficiary-directionality), the arrangement presents as obligation with zero perceived cost — the closest thing to a mountain experience available to any participant, since divine command admits no degrees of freedom. From the payer seats, the same structure operates as enforced asymmetry: costs imposed without consent, alternatives foreclosed by a premise they never accepted. From the state seat, the arrangement is a governing bargain — enforce the frame, absorb the external costs, keep the coalition. The engine computes these divergences from power, exit, and directionality data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   The settlement movement and rabbinical establishment are declared beneficiaries: land, budget, legal protection, and adjudicative authority flow to them, placing their derived directionality near the beneficiary pole — amplified further by identity_locked exit, since an agent who cannot leave cannot arbitrage away the arrangement's terms. Palestinian residents and secular two-state advocates are declared targets: the first bear the arrangement's territorial consequences with trapped exit, the second bear the foreclosure of their negotiating framework with constrained exit; both derive high directionality, and the trapped seat sits nearest the full-target end. The governing coalitions are dual-positioned (agenda_setter with payer secondary role): they administer the arrangement but also absorb its diplomatic isolation, legal exposure, and security burden, pulling their derived directionality toward symmetric rather than beneficiary. Diaspora institutions bear diffuse reputational costs with mobile exit — the mobile option dampens their effective extraction well below what their cost-bearing alone would suggest.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how a religious community relates to modern sovereignty without surrendering halakhic authority, sharpened into crisis by the post-1967 territorial windfall — remains live for its holders: the theological question the arrangement answers is unresolved until, in the frame's own terms, redemption. Mandatrophy is therefore not resolved, and the classification guards against the two symmetrical mislabels: reading the arrangement as pure coordination (rope) would erase the identifiable populations bearing costs they never consented to; reading it as pure extraction (snare) would erase the genuine identity- and meaning-coordination the frame performs for its community, which is why adherence survives costs that would collapse a purely coercive structure. The live-function-plus-asymmetric-cost profile is the tangled_rope signature. Note also what the arrangement is not: it is not a piton, because the function has not atrophied and the beneficiaries actively maintain enforcement; and the founding-problem status (live) paired with the disappearance verdict (world_rearranges) produces no zombie mismatch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_command_vs_constructed_doctrine,
    'Is the covenant claim a framework-independent divine obligation (immutable, immune to political revision, mountain-like in form) or a theological-political doctrine constructed and activated by identifiable actors under identifiable conditions?',
    'Doctrinal-history comparison across shifts in political fortune: pre-1967 religious Zionism largely deferred territorial activism (three-oaths posture, waiting-for-redemption theology); post-1967 the same tradition activated immediate territorial obligation. If the operative content of the claim tracks political opportunity rather than textual discovery, the immutability is enacted rather than found.',
    'A constructed-doctrine verdict fails any mountain certification outright and consolidates tangled_rope classification with snare-side drift risk; a framework-independent verdict would license partial mountain treatment within the believing seat only, leaving the cross-framework classification unchanged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_command_vs_constructed_doctrine, empirical, 'Whether the constraint''s claimed immutability is discovered or historically enacted.').

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is one reading of the kernel jewish_self_determination; what would each sibling reading change structurally if instantiated instead?',
    'Generate the sibling stories and compare beneficiary/victim sets, revisability, and enforcement profiles across the family.',
    'The settler_colonial_reading authors the same territory question with Palestinians as indigenous rights-holders and the sovereignty project itself as the extraction; the liberal_nationalist_reading preserves the nation but relocates the claim''s source to universal principle, restoring negotiability; the indigenous_return_reading keeps the return but grounds it in historical continuity rather than decree; the diasporist_reading negates territorial sovereignty altogether. The disagreement is located in the claim''s SOURCE, which determines whether compromise is thinkable at all.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: reading-indexing of the self-determination kernel.').

omega_variable(
    believer_seat_extraction_opacity,
    'Adherents experience the arrangement as obligation rather than cost (effective extraction near zero from their seat); is the authored epsilon an artifact of the analytical seat or a fact about the arrangement?',
    'Per-seat computation from the structural data: compare computed effective extraction at the identity_locked beneficiary seats against the payer seats. The divergence is the finding, not an error to reconcile.',
    'If believer-seat extraction is genuinely near zero, the arrangement''s costs are invisible from inside the framework and any renegotiation coalition must run through non-believer seats; classification remains tangled_rope either way, but enforcement prognosis and reform pathways change materially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(believer_seat_extraction_opacity, conceptual, 'Seat-indexed perception versus structural cost-bearing.').

omega_variable(
    capture_direction_state_vs_movement,
    'Does state power serve the covenant movement, or does the movement serve state-expansion interests that would pursue the same territory without the theology?',
    'Legislative-initiative tracing and budget-flow analysis: who originates settlement-expansion instruments, who consumes their outputs, and whether the instruments would exist unchanged if the covenant premise were removed.',
    'Determines whether the arrangement''s gains accrue primarily to religious authority (theological capture of state institutions) or to secular expansionism operating through covenant framing (the movement as instrument). Changes which seat is the capturer for receipt-surface purposes and which counter-coalition could plausibly unwind the arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capture_direction_state_vs_movement, empirical, 'Direction of entanglement between religious authority and state power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__religious_covenant_reading, 1967, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jsd_religious_covenant_tr_t1967, jewish_self_determination__religious_covenant_reading, theater_ratio, 1967, 0.14).
narrative_ontology:measurement_basis(jsd_religious_covenant_tr_t1967, observed).
narrative_ontology:measurement(jsd_religious_covenant_tr_t1977, jewish_self_determination__religious_covenant_reading, theater_ratio, 1977, 0.18).
narrative_ontology:measurement_basis(jsd_religious_covenant_tr_t1977, observed).
narrative_ontology:measurement(jsd_religious_covenant_tr_t1988, jewish_self_determination__religious_covenant_reading, theater_ratio, 1988, 0.23).
narrative_ontology:measurement_basis(jsd_religious_covenant_tr_t1988, observed).
narrative_ontology:measurement(jsd_religious_covenant_tr_t1995, jewish_self_determination__religious_covenant_reading, theater_ratio, 1995, 0.27).
narrative_ontology:measurement_basis(jsd_religious_covenant_tr_t1995, observed).
narrative_ontology:measurement(jsd_religious_covenant_tr_t2000, jewish_self_determination__religious_covenant_reading, theater_ratio, 2000, 0.31).
narrative_ontology:measurement_basis(jsd_religious_covenant_tr_t2000, observed).
narrative_ontology:measurement(jsd_religious_covenant_tr_t2005, jewish_self_determination__religious_covenant_reading, theater_ratio, 2005, 0.36).
narrative_ontology:measurement_basis(jsd_religious_covenant_tr_t2005, observed).
narrative_ontology:measurement(jsd_religious_covenant_tr_t2012, jewish_self_determination__religious_covenant_reading, theater_ratio, 2012, 0.32).
narrative_ontology:measurement_basis(jsd_religious_covenant_tr_t2012, observed).
narrative_ontology:measurement(jsd_religious_covenant_tr_t2025, jewish_self_determination__religious_covenant_reading, theater_ratio, 2025, 0.28).
narrative_ontology:measurement_basis(jsd_religious_covenant_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(jsd_religious_covenant_be_t1967, jewish_self_determination__religious_covenant_reading, base_extractiveness, 1967, 0.34).
narrative_ontology:measurement_basis(jsd_religious_covenant_be_t1967, observed).
narrative_ontology:measurement(jsd_religious_covenant_be_t1977, jewish_self_determination__religious_covenant_reading, base_extractiveness, 1977, 0.46).
narrative_ontology:measurement_basis(jsd_religious_covenant_be_t1977, observed).
narrative_ontology:measurement(jsd_religious_covenant_be_t1988, jewish_self_determination__religious_covenant_reading, base_extractiveness, 1988, 0.56).
narrative_ontology:measurement_basis(jsd_religious_covenant_be_t1988, observed).
narrative_ontology:measurement(jsd_religious_covenant_be_t1995, jewish_self_determination__religious_covenant_reading, base_extractiveness, 1995, 0.61).
narrative_ontology:measurement_basis(jsd_religious_covenant_be_t1995, observed).
narrative_ontology:measurement(jsd_religious_covenant_be_t2000, jewish_self_determination__religious_covenant_reading, base_extractiveness, 2000, 0.64).
narrative_ontology:measurement_basis(jsd_religious_covenant_be_t2000, observed).
narrative_ontology:measurement(jsd_religious_covenant_be_t2005, jewish_self_determination__religious_covenant_reading, base_extractiveness, 2005, 0.59).
narrative_ontology:measurement_basis(jsd_religious_covenant_be_t2005, observed).
narrative_ontology:measurement(jsd_religious_covenant_be_t2012, jewish_self_determination__religious_covenant_reading, base_extractiveness, 2012, 0.68).
narrative_ontology:measurement_basis(jsd_religious_covenant_be_t2012, observed).
narrative_ontology:measurement(jsd_religious_covenant_be_t2025, jewish_self_determination__religious_covenant_reading, base_extractiveness, 2025, 0.74).
narrative_ontology:measurement_basis(jsd_religious_covenant_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(jsd_religious_covenant_su_t1967, jewish_self_determination__religious_covenant_reading, suppression_requirement, 1967, 0.31).
narrative_ontology:measurement_basis(jsd_religious_covenant_su_t1967, observed).
narrative_ontology:measurement(jsd_religious_covenant_su_t1977, jewish_self_determination__religious_covenant_reading, suppression_requirement, 1977, 0.41).
narrative_ontology:measurement_basis(jsd_religious_covenant_su_t1977, observed).
narrative_ontology:measurement(jsd_religious_covenant_su_t1988, jewish_self_determination__religious_covenant_reading, suppression_requirement, 1988, 0.53).
narrative_ontology:measurement_basis(jsd_religious_covenant_su_t1988, observed).
narrative_ontology:measurement(jsd_religious_covenant_su_t1995, jewish_self_determination__religious_covenant_reading, suppression_requirement, 1995, 0.59).
narrative_ontology:measurement_basis(jsd_religious_covenant_su_t1995, observed).
narrative_ontology:measurement(jsd_religious_covenant_su_t2000, jewish_self_determination__religious_covenant_reading, suppression_requirement, 2000, 0.63).
narrative_ontology:measurement_basis(jsd_religious_covenant_su_t2000, observed).
narrative_ontology:measurement(jsd_religious_covenant_su_t2005, jewish_self_determination__religious_covenant_reading, suppression_requirement, 2005, 0.55).
narrative_ontology:measurement_basis(jsd_religious_covenant_su_t2005, observed).
narrative_ontology:measurement(jsd_religious_covenant_su_t2012, jewish_self_determination__religious_covenant_reading, suppression_requirement, 2012, 0.66).
narrative_ontology:measurement_basis(jsd_religious_covenant_su_t2012, observed).
narrative_ontology:measurement(jsd_religious_covenant_su_t2025, jewish_self_determination__religious_covenant_reading, suppression_requirement, 2025, 0.72).
narrative_ontology:measurement_basis(jsd_religious_covenant_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__religious_covenant_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__diasporist_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the kernel jewish_self_determination per the epsilon-invariance principle: the colloquial label 'the Jewish claim to the land' conflates five structurally distinct constraints (five readings), each with its own stable epsilon, beneficiary/victim structure, and revisability profile. This file is the religious_covenant_reading member. The upstream/downstream structure runs through the claim's source: readings that ground the claim in revisable sources (liberal-nationalist, indigenous-return) inherit pressure from this reading whenever covenant-framed policy hardens, because it demonstrates what the claim becomes when its source is held non-negotiable; the settler_colonial and diasporist readings consume this reading's operations as their central evidence. Epsilon differences across the family reflect the different arrangements each reading instantiates, not different observables applied to one arrangement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
