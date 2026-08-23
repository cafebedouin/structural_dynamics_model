% ============================================================================
% CONSTRAINT STORY: total_war_winnability_post1945__normative_reading_drop
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_winnability_post1945__normative_reading_drop, []).

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
 *   constraint_id: total_war_winnability_post1945__normative_reading_drop
 *   human_readable: Post-1945 Normative Prohibition on Total War (Normative Reading)
 *   domain: international-relations/strategic-studies/commitment-systems
 *
 * SUMMARY:
 *   This story instantiates the normative_reading_drop reading of the
 *   total_war_winnability_post1945 kernel: the claim that total war remains
 *   physically available to states but became normatively illegitimate
 *   through the UN Charter's Article 2(4) prohibition on aggressive force and
 *   the postwar development of humanitarian law (Geneva Conventions 1949,
 *   Additional Protocols 1977, Rome Statute 1998). The constraint under
 *   classification is that legal-normative prohibition regime itself: a
 *   constructed, treaty-grounded arrangement, not a natural limit. Its
 *   coordination function is real and enormous: states jointly renounce
 *   unlimited war, converting a mutual-catastrophe free-for-all into bounded
 *   use with shared legitimacy costs for violation. Its extraction is also
 *   real: the same arrangement freezes the 1945 territorial settlement to the
 *   victors' advantage, embeds permanent-member enforcement exemption, and
 *   delivers protection selectively, so revisionist powers and unprotected
 *   civilian populations bear costs the steward class does not. The claimed
 *   type (tangled_rope) is authored from that structural judgment; the
 *   metrics are authored descriptively from the regime's actual operation;
 *   the two are independent authored facts. Sibling readings (structural
 *   contraction, strategic-culture drift) are separate stories linked in the
 *   network section; the attribution contest lives in the omega variables,
 *   not inside this constraint.
 *
 * KEY AGENTS:
 *   - global_civilian_populations: primary beneficiary (moderate/trapped) — the protected base whose survival the prohibition insures
 *   - small_and_middle_powers: secondary beneficiary (moderate/constrained) — the normative shield against larger neighbors
 *   - charter_status_quo_powers: agenda-setter and collector (institutional/arbitrage) — stewards who police an order that locks in their own advantages
 *   - revisionist_powers: primary target (powerful/trapped) — bears enforcement costs and foreclosed options
 *   - civilians_in_enforcement_gap_conflicts: cost-bearing residual (powerless/trapped) — intended protectees beyond enforcement's reach
 *   - secessionist_and_liberation_movements: excluded voice (organized/trapped) — border-change by force criminalized, no seat at the table
 *   - international_legal_community: analytical observer (institutional/analytical) — the interpretive layer that certifies what the text means
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_winnability_post1945__normative_reading_drop, 0.46).
domain_priors:suppression_score(total_war_winnability_post1945__normative_reading_drop, 0.65).
domain_priors:theater_ratio(total_war_winnability_post1945__normative_reading_drop, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, extractiveness, 0.46).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__normative_reading_drop, tangled_rope).
narrative_ontology:human_readable(total_war_winnability_post1945__normative_reading_drop, "Post-1945 Normative Prohibition on Total War (Normative Reading)").
narrative_ontology:topic_domain(total_war_winnability_post1945__normative_reading_drop, "international-relations/strategic-studies/commitment-systems").

domain_priors:requires_active_enforcement(total_war_winnability_post1945__normative_reading_drop).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__normative_reading_drop, '86761569-caa8-4893-96eb-cc750f20cfed').
narrative_ontology:cs_kernel_codification('86761569-caa8-4893-96eb-cc750f20cfed', fixed_text).
narrative_ontology:cs_authority_grounding('86761569-caa8-4893-96eb-cc750f20cfed', lineage).
narrative_ontology:cs_interpretation_layer_present('86761569-caa8-4893-96eb-cc750f20cfed').
narrative_ontology:cs_reading_relation('86761569-caa8-4893-96eb-cc750f20cfed', total_war_winnability_post1945__structural_contraction_reading, forecloses).
narrative_ontology:cs_reading_relation('86761569-caa8-4893-96eb-cc750f20cfed', total_war_winnability_post1945__strategic_culture_drift, coexists_with).
narrative_ontology:cs_axiom('86761569-caa8-4893-96eb-cc750f20cfed', foundational, total_war_physically_possible_yet_normatively_prohibited).
narrative_ontology:cs_axiom_status(total_war_physically_possible_yet_normatively_prohibited, holdable).
narrative_ontology:cs_axiom_grounding('86761569-caa8-4893-96eb-cc750f20cfed', total_war_physically_possible_yet_normatively_prohibited, empirically_contingent).
narrative_ontology:cs_axiom('86761569-caa8-4893-96eb-cc750f20cfed', secondary, civilian_immunity_distinct_from_military_necessity).
narrative_ontology:cs_axiom_status(civilian_immunity_distinct_from_military_necessity, holdable).
narrative_ontology:cs_axiom_grounding('86761569-caa8-4893-96eb-cc750f20cfed', civilian_immunity_distinct_from_military_necessity, deontological).
narrative_ontology:cs_reference_frame('86761569-caa8-4893-96eb-cc750f20cfed', post1945_normative_prohibition_order).
narrative_ontology:cs_drift_state('86761569-caa8-4893-96eb-cc750f20cfed', contemporary_revisionist_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('86761569-caa8-4893-96eb-cc750f20cfed', '').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__normative_reading_drop, total_war_winnability_post1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__normative_reading_drop, global_civilian_populations).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__normative_reading_drop, small_and_middle_powers).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__normative_reading_drop, charter_status_quo_powers).
narrative_ontology:constraint_victim(total_war_winnability_post1945__normative_reading_drop, revisionist_powers).
narrative_ontology:constraint_victim(total_war_winnability_post1945__normative_reading_drop, civilians_in_enforcement_gap_conflicts).
narrative_ontology:constraint_vindicates(total_war_winnability_post1945__normative_reading_drop, constructivist_norm_compliance_thesis).
narrative_ontology:constraint_vindicates(total_war_winnability_post1945__normative_reading_drop, civilian_immunity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live under the legal promise that wars between states will not be fought without limit and that civilians will not be targeted as such. Receive the protection when it holds; bury the dead when enforcement fails. Cannot exit the state system or the planet; their protection arrives only through institutions they do not control.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, global_civilian_populations, beneficiary,
    moderate, generational, trapped, global).

% Depend on the prohibition as their principal shield against larger neighbors: open conquest now carries legitimacy costs, sanctions risk, and probable coalition response that would otherwise fall on them alone. Cannot opt out of the system their survival depends on; they can bandwagon or balance but not leave.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, small_and_middle_powers, beneficiary,
    moderate, generational, constrained, global).

% Hold permanent Council seats and veto rights written in 1945; administer enforcement, authorize coalitions, and decide which violations reach the machinery. Their borders and institutional privileges are frozen by the same rules they police; they can reinterpret, delay, or deflect enforcement aimed at themselves or their clients, and they bear real costs of system maintenance: defense burdens, retaliation exposure, and the obligation to justify their own uses of force in legal terms.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, charter_status_quo_powers, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(total_war_winnability_post1945__normative_reading_drop, charter_status_quo_powers, beneficiary).

% States whose strategy prizes territorial or coercive revision. The prohibition forecloses open conquest as legitimate policy; violation triggers sanctions, isolation, tribunals, and coalition arming of their opponents. Exit is unavailable: leaving the treaty system forfeits the legitimacy every other instrument of statecraft runs on. Some sit inside the steward class, shielded by the veto; most face the machinery bare.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, revisionist_powers, payer,
    powerful, generational, trapped, global).

% Live where the prohibition's enforcement does not reach: sieges, urban bombardment, and mass-atrocity campaigns proceed while the machinery deliberates. They were the intended protectees; the protection is delivered selectively, and from where they stand its absence is indistinguishable from abandonment.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, civilians_in_enforcement_gap_conflicts, payer,
    powerless, biographical, trapped, regional).

% Peoples seeking border change by force. The prohibition protects the territorial integrity of the states they fight, criminalizes their method, and offers them no seat at the table where legitimate force is defined. Their claim enters the system only through the very states that suppress them.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, secessionist_and_liberation_movements, excluded,
    organized, biographical, trapped, regional).

% Jurists, tribunals, and humanitarian bodies that interpret the prohibition's text, adjudicate violations, and certify compliance. They command no armies and collect no revenues; their product is the ruling that determines what the words mean in any given decade.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, international_legal_community, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_winnability_post1945__normative_reading_drop, charter_status_quo_powers).
narrative_ontology:fixing_cost_class(total_war_winnability_post1945__normative_reading_drop, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the interstate-security coordination problem that produced two world wars: by jointly renouncing unlimited war and codifying civilian-immunity limits, states convert a mutual-catastrophe free-for-all into bounded use, establish shared legitimacy costs for violation, and create the common legal language that lets coalitions form against violators quickly.
% TRANSFER_FUNCTION: Moves strategic freedom and legitimacy. The right of unrestricted war-making moves from all states to no one; enforcement costs (sanctions, isolation, tribunals) move onto violators; institutional privilege (permanent seats, veto rights, frozen favorable borders) accrues to the arrangement's stewards; protection flows to civilian populations where enforcement reaches.
% ABSENT_VOICES: Secessionist and liberation movements would object that the prohibition protects their opponents' territorial integrity while criminalizing their only method; they are not seated, because the arrangement's membership is states and non-state peoples enter only through states that typically suppress them. Civilians in enforcement-gap conflicts would object that the promise is delivered selectively; they appear mainly as statistics in the machinery's own reports.
% DISAPPEARANCE_RATIONALE: If Article 2(4) and the humanitarian-law regime vanished overnight, conquest would again be legitimate policy: annexation incentives return immediately, every state with a grievance or a weakened neighbor reprices, alliance systems shift from war-prevention to war-winning postures, and the legal vocabulary that lets coalitions assemble against violators disappears. Even readers skeptical of the norm's causal weight concede the overnight landscape changes; the kernel's readings argue about how much of the long-run restraint the law explains, not about whether its removal would rearrange the world.
% FOUNDING_PROBLEM: After two world wars that killed tens of millions and destroyed cities wholesale, build a legal order in which aggressive war between states is unlawful and civilian populations are not legitimate targets, making total war not merely costly but illegitimate.
% FOUNDING_PROBLEM_CORROBORATION: The steward states and UN organs attest the problem is live (perpetual-vigilance rhetoric); revisionist powers attest it is dead or transformed (the order as obsolete lock-in). Corroboration from outside the benefiting parties: documentary historiography of the World Wars establishes the founding catastrophe as real; operational records from humanitarian organizations (casualty counts, access denials) attest that civilian-protection gaps persist; strategic-studies scholarship outside the diplomatic establishment attests the problem has partially transformed rather than disappeared. No single external source settles liveness, hence contested.
narrative_ontology:disappearance_verdict(total_war_winnability_post1945__normative_reading_drop, world_rearranges).
narrative_ontology:founding_problem_status(total_war_winnability_post1945__normative_reading_drop, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_winnability_post1945__normative_reading_drop, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_winnability_post1945__normative_reading_drop, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_winnability_post1945__normative_reading_drop, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_winnability_post1945__normative_reading_drop_tests).
:- end_tests(total_war_winnability_post1945__normative_reading_drop_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.46: the prohibition regime delivers its coordination benefit broadly, but the benefit is distributed unevenly and the costs concentrate — revisionist powers forfeit valued options and face the enforcement machinery, while the steward class polices an order that locks in its own advantages. Suppression 0.65: the arrangement holds only through active machinery — sanctions regimes, ad hoc coalitions, tribunals, conditionality, sustained legitimacy pressure; nothing about it is self-enforcing. Theater 0.42: a growing share of regime activity is commemorative and resolutionary performance (anniversary declarations, emergency sessions, investigative reports without consequence) while enforcement gaps widen; the core function — making unlimited war illegitimate — is still performed, but the performative share has climbed past two-fifths. Accessibility collapse 0.60: open total-war aims have collapsed out of legitimate discourse — no government can declare annihilation as policy — yet the physical option persists and gray-zone substitutes (siege warfare, urban bombardment below the total-war threshold) flourish. Resistance 0.55: recurring violations, doctrinal challenges, and enforcement evasion are constant features, not edge cases. Temporal shape: the series traces a crisis-reform-relaxation-accumulation cycle — enforcement surges after atrocities (1990s tribunals, post-Cold War sanctions architecture), relaxes, and extraction accumulates in the relaxed phases; the end-state values are measured in a strained, high-theater phase. Coalition note: the payer class is not resourceless — revisionist powers align (sanctions-evading trade, parallel institutions), and that coalition potential is part of why the suppression requirement keeps rising.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same arrangement. From the steward seat, the regime is order-management: a legitimate framework the steward administers, profits from, and occasionally bends. From the revisionist seat, the same framework is a cage built by rivals: its tribunals read as selective punishment, its rules-based-order rhetoric as lock-in of 1945 gains. From the civilian seats, it is a promise whose delivery is geographically arbitrary — protection where enforcement reaches, abandonment where it does not. From the legal community's seat, it is a text to be interpreted, and the interpretation is the operative thing. The engine computes these divergences from the structural data; the authored claim adjudicates nothing.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: global_civilian_populations and small_and_middle_powers derive near the beneficiary pole (low d, subsidized or lightly loaded). Victim declarations drive the opposite pole: revisionist_powers derive near the full-target end (high d, amplified by trapped exit — no legitimate outside option exists for a state that abandons the treaty system). Two overrides correct derivations the arrays cannot see. First, charter_status_quo_powers appear in the beneficiaries array, which would derive a near-pure-beneficiary d; structurally they are steward-beneficiaries who also carry real system costs (defense burdens, retaliation exposure, the obligation to legalize their own force), so the override sets institutional d to 0.25. Second, civilians_in_enforcement_gap_conflicts appear in the victims array, which would derive near-full-target d; but they are the regime's intended protectees harmed by its selective operation rather than targets of its extraction proper, so the override sets powerless d to 0.60. Secessionist and liberation movements carry no array declaration; their position (their method criminalized by the arrangement that protects their opponents) is recorded qualitatively and left to the engine's fallback.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — ending industrial-scale total war after two world wars — is contested rather than dead: capabilities persist, large-scale interstate war has returned to Europe, and the problem's defenders and critics dispute whether the original object still needs guarding. No mandatrophy resolution is declared. The classification matters because the regime invites both mislabels: triumphalism reads it as pure coordination achieved (rope), cynicism reads the enforcement gaps as proof the coordination story is cover (snare). The tangled-rope structure holds both truths: the coordination function is real and still performed, and the extraction — steward exemption, frozen-settlement rents, selective protection — rides on the same machinery. The rising theater ratio is the drift signal to watch: if commemoration fully substitutes for enforcement, the arrangement degrades toward performative maintenance with the founding problem unaddressed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_attribution,
    'This story instantiates the normative_reading_drop reading of kernel total_war_winnability_post1945: it attributes the post-1945 absence of great-power total war to the legal-normative prohibition (Article 2(4) plus humanitarian law). Which causal locus actually binds?',
    'Comparative analysis across the kernel''s readings: examine total-war-adjacent conflicts between non-nuclear states under the same prohibition, and violation episodes where nuclear factors are absent; convergence of restraint where norms bind without nuclear cover supports this reading.',
    'If physical removal dominates, this constraint''s authored structure overstates the norm''s work and its classification drifts toward vestigial maintenance; if normative binding dominates, the story stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_attribution, conceptual, 'Kernel-level attribution: normative versus physical versus cultural cause of the post-1945 total-war absence.').

omega_variable(
    sibling_structural_contraction_delta,
    'What would the structural_contraction_reading sibling change in this story''s structure if adopted as the true reading of the kernel?',
    'Adopt the sibling''s premise (nuclear weapons physically removed total war from the reachable space, not socially abandoned) and re-derive: the prohibition regime becomes codification layered over a physical fact; epsilon falls toward zero and the arrangement persists as inertial record-keeping.',
    'Under the sibling reading this story''s hybrid coordination-plus-asymmetry structure dissolves toward a degraded, mostly performative remnant; the beneficiary/victim asymmetry becomes irrelevant because the constrained behavior is physically unavailable regardless.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_structural_contraction_delta, conceptual, 'Structural delta if the physical-impossibility sibling reading is correct.').

omega_variable(
    sibling_strategic_culture_delta,
    'What would the strategic_culture_drift sibling change if the drop from elite discourse, not codified law, did the binding work?',
    'Compare restraint across states with identical treaty commitments but different strategic-cultural formation; if restraint tracks discourse formation rather than ratification status, the treaty layer is downstream crystallization of a cultural shift.',
    'The enforcement measurements in this story would be measuring culture through a legal proxy; classification shifts toward a coordination arrangement with diffuse beneficiaries and the interpretive-layer facts become epiphenomenal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_strategic_culture_delta, conceptual, 'Structural delta if ideational drift, not legal codification, is the operative mechanism.').

omega_variable(
    enforcement_selectivity_intrinsicity,
    'Is the enforcement asymmetry (permanent members exempting themselves and their clients from the machinery they administer) intrinsic to the arrangement''s constitution or contingent on current politics?',
    'Examine the amendment procedure (permanent-member concurrence required for any Charter change) and historical bypass attempts (the Uniting for Peace resolution, ad hoc coalitions); if every bypass route still routes through great-power consent, the asymmetry is constitutional.',
    'Intrinsic asymmetry confirms the extraction component as structural; contingent asymmetry would allow recovery toward a purer coordination arrangement if politics shifted.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_selectivity_intrinsicity, empirical, 'Whether steward exemption is baked into the arrangement''s constitution.').

omega_variable(
    practice_drift_reversibility,
    'Will the contemporary practice drift (annexations absorbed without reversal, humanitarian-law erosion in urban warfare) reverse through enforcement renewal, or consolidate into a new normal?',
    'Track violator cost trajectories and enforcement responses over the coming decade; measure whether the cost of violation is rising (renewal) or falling (consolidation).',
    'Consolidation pushes the arrangement toward performative maintenance with a rising theater share; renewal restores functional enforcement and lowers it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practice_drift_reversibility, empirical, 'Durability of the current enforcement-gap regime.').

omega_variable(
    aggression_ban_vs_total_war_scope,
    'The colloquial label ''Article 2(4)'' covers both the general prohibition on interstate force and the narrower normative drop in total war''s legitimacy; this story authors the latter. Does the referent boundary hold?',
    'Decomposition test: author a sibling story for the general-aggression prohibition with its own epsilon and victim set; if the two stories'' epsilons diverge materially, the boundary is confirmed and both files stand separately.',
    'If forced into one story, epsilon becomes observable-dependent and unstable; decomposed, each claim keeps a single stable epsilon. This story''s epsilon refers only to the total-war-specific normative structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(aggression_ban_vs_total_war_scope, conceptual, 'Referent-scope boundary between the general aggression ban and the total-war-specific normative drop.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__normative_reading_drop, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 1945, 0.2).
narrative_ontology:measurement_basis(tota_tr_t1945, observed).
narrative_ontology:measurement(tota_tr_t1955, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 1955, 0.23).
narrative_ontology:measurement_basis(tota_tr_t1955, observed).
narrative_ontology:measurement(tota_tr_t1965, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 1965, 0.29).
narrative_ontology:measurement_basis(tota_tr_t1965, observed).
narrative_ontology:measurement(tota_tr_t1975, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 1975, 0.32).
narrative_ontology:measurement_basis(tota_tr_t1975, observed).
narrative_ontology:measurement(tota_tr_t1985, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 1985, 0.29).
narrative_ontology:measurement_basis(tota_tr_t1985, observed).
narrative_ontology:measurement(tota_tr_t1995, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 1995, 0.17).
narrative_ontology:measurement_basis(tota_tr_t1995, observed).
narrative_ontology:measurement(tota_tr_t2005, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 2005, 0.23).
narrative_ontology:measurement_basis(tota_tr_t2005, observed).
narrative_ontology:measurement(tota_tr_t2015, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 2015, 0.35).
narrative_ontology:measurement_basis(tota_tr_t2015, observed).
narrative_ontology:measurement(tota_tr_t2025, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 2025, 0.42).
narrative_ontology:measurement_basis(tota_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 1945, 0.3).
narrative_ontology:measurement_basis(tota_be_t1945, observed).
narrative_ontology:measurement(tota_be_t1955, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 1955, 0.35).
narrative_ontology:measurement_basis(tota_be_t1955, observed).
narrative_ontology:measurement(tota_be_t1965, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 1965, 0.39).
narrative_ontology:measurement_basis(tota_be_t1965, observed).
narrative_ontology:measurement(tota_be_t1975, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 1975, 0.41).
narrative_ontology:measurement_basis(tota_be_t1975, observed).
narrative_ontology:measurement(tota_be_t1985, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 1985, 0.37).
narrative_ontology:measurement_basis(tota_be_t1985, observed).
narrative_ontology:measurement(tota_be_t1995, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 1995, 0.29).
narrative_ontology:measurement_basis(tota_be_t1995, observed).
narrative_ontology:measurement(tota_be_t2005, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 2005, 0.33).
narrative_ontology:measurement_basis(tota_be_t2005, observed).
narrative_ontology:measurement(tota_be_t2015, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 2015, 0.41).
narrative_ontology:measurement_basis(tota_be_t2015, observed).
narrative_ontology:measurement(tota_be_t2025, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 2025, 0.46).
narrative_ontology:measurement_basis(tota_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 1945, 0.25).
narrative_ontology:measurement_basis(tota_su_t1945, observed).
narrative_ontology:measurement(tota_su_t1955, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 1955, 0.34).
narrative_ontology:measurement_basis(tota_su_t1955, observed).
narrative_ontology:measurement(tota_su_t1965, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 1965, 0.43).
narrative_ontology:measurement_basis(tota_su_t1965, observed).
narrative_ontology:measurement(tota_su_t1975, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 1975, 0.49).
narrative_ontology:measurement_basis(tota_su_t1975, observed).
narrative_ontology:measurement(tota_su_t1985, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 1985, 0.51).
narrative_ontology:measurement_basis(tota_su_t1985, observed).
narrative_ontology:measurement(tota_su_t1995, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 1995, 0.56).
narrative_ontology:measurement_basis(tota_su_t1995, observed).
narrative_ontology:measurement(tota_su_t2005, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 2005, 0.6).
narrative_ontology:measurement_basis(tota_su_t2005, observed).
narrative_ontology:measurement(tota_su_t2015, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 2015, 0.63).
narrative_ontology:measurement_basis(tota_su_t2015, observed).
narrative_ontology:measurement(tota_su_t2025, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 2025, 0.65).
narrative_ontology:measurement_basis(tota_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_winnability_post1945__normative_reading_drop, enforcement_mechanism).
narrative_ontology:affects_constraint(total_war_winnability_post1945__normative_reading_drop, total_war_winnability_post1945__structural_contraction_reading).
narrative_ontology:affects_constraint(total_war_winnability_post1945__normative_reading_drop, total_war_winnability_post1945__strategic_culture_drift).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial claim that 'total war became impossible or unthinkable after 1945' decomposes into three readings of one kernel. This file instantiates the normative_reading_drop reading (the binding is legal-normative; physical capability persists). The structural_contraction_reading sibling authors the physical-removal claim with its own epsilon (near-zero extraction, mountain-adjacent profile); the strategic_culture_drift sibling authors the ideational-drift claim. Each reading carries its own epsilon, beneficiaries, and classification; this file links both siblings via affects_constraints. Family contest concentrates on attribution: if the structural claim is true it drains this reading's causal content, which is why the attribution omega is the family's pivot.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(total_war_winnability_post1945__normative_reading_drop, institutional, 0.25).
constraint_indexing:directionality_override(total_war_winnability_post1945__normative_reading_drop, powerless, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
