% ============================================================================
% CONSTRAINT STORY: modern_judicialization__eu_membership_and_exit
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_modern_judicialization__eu_membership_and_exit, []).

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
 *   constraint_id: modern_judicialization__eu_membership_and_exit
 *   human_readable: EU Membership and Exit: Pooled Sovereignty and Renationalization
 *   domain: political/legal/constitutional
 *
 * SUMMARY:
 *   EU membership and its reversal form a 40-year constitutional arc that
 *   tests the binding force of supranational law against renationalization.
 *   The constraint emerges from the contradiction between Westminster's claim
 *   to absolute parliamentary sovereignty (a foundational axiom of English
 *   constitutional law) and membership in a supranational legal order where
 *   EU law overrides domestic statute. The constraint's lifecycle shows
 *   rising extractiveness and suppression: early membership (1973–early
 *   1990s) balanced coordination benefits (single market, environmental
 *   standards, labor rights) against pooled sovereignty (experienced as
 *   manageable cost). By the 1990s, extractiveness rose as directives
 *   proliferated, judicial review of EU compliance increased, and immigration
 *   from EU member states accelerated. The suppression requirement climbed as
 *   resistance movements mobilized (UKIP, Eurosceptic backbenchers) and had
 *   to be suppressed through media marginalizing, institutional gatekeeping,
 *   and elite consensus-maintenance. By 2016, the referendum itself became
 *   the enforcing mechanism — a populist override of elite consensus. The
 *   constraint exhibits the tangled_rope signature: genuine coordination
 *   function (trade, rights, scientific cooperation) coexisting with
 *   asymmetric extraction (sovereignty subordination, immigration flows that
 *   benefit employers but suppressed wages in some sectors). Theater ratio
 *   rose throughout, peaking post-referendum as government and parliament
 *   performed the 'will of the people' while managing institutional-level
 *   contradictions (devolution incompatibility, rights framework
 *   entanglement, Northern Ireland border impossibilities). The constraint is
 *   one reading of the modern_judicialization kernel: the UK's distributed
 *   authority system (Westminster, devolved legislatures, Human Rights Act
 *   courts, Supreme Court separation) all crystallized in the same 40-year
 *   period as EU membership and exit. This reading focuses on the
 *   supranational dimension; sibling readings (devolution_settlements,
 *   human_rights_act_1998, uk_supreme_court_creation) address the sub-state
 *   and rights dimensions of the same phenomenon. The core contested axiom:
 *   can parliamentary sovereignty be absolute in a modern interdependent
 *   democracy?
 *
 * KEY AGENTS:
 *   - Single Market Beneficiaries (institutional/arbitrage): Financial services, multinational corporations, agribusiness — benefit from frictionless trade and unified standards; have capacity to relocate but profit from membership coordination
 *   - Sovereignty Restorationists (powerful/mobile → powerless post-referendum): UKIP, Leave campaigners, nationalist MPs — perceive membership as extraction, mobilize politically, succeed in shifting power through referendum
 *   - Cross-Border Citizens (powerless/trapped): EU nationals resident in UK, UK nationals in EU — constrained by territory and rights status; experience extraction in both membership (pooled governance they don't control) and exit (rights revocation)
 *   - Subordinated Legislatures (powerful/constrained): Scottish Parliament, Welsh Senedd, Northern Ireland Assembly — face triple layering of constraints: Westminster supremacy, EU supremacy, then attempted renationalization
 *   - Westminster Parliament (institutional/constrained): Holds pooled sovereignty during membership, attempts to reclaim it post-exit, but faces entanglement with devolution and rights framework
 *   - Remain Coalition (organized/mobile): Pro-integration actors arguing for managed relationship; possess agency but lose referendum power struggle
 *   - Analytical Observer (analytical/analytical): Risks naturalizing the constraint as immutable law of modern governance rather than contingent political construction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(modern_judicialization__eu_membership_and_exit, 0.58).
domain_priors:suppression_score(modern_judicialization__eu_membership_and_exit, 0.62).
domain_priors:theater_ratio(modern_judicialization__eu_membership_and_exit, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(modern_judicialization__eu_membership_and_exit, extractiveness, 0.58).
narrative_ontology:constraint_metric(modern_judicialization__eu_membership_and_exit, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(modern_judicialization__eu_membership_and_exit, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(modern_judicialization__eu_membership_and_exit, tangled_rope).
narrative_ontology:human_readable(modern_judicialization__eu_membership_and_exit, "EU Membership and Exit: Pooled Sovereignty and Renationalization").
narrative_ontology:topic_domain(modern_judicialization__eu_membership_and_exit, "political/legal/constitutional").

domain_priors:requires_active_enforcement(modern_judicialization__eu_membership_and_exit).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(modern_judicialization__eu_membership_and_exit, '5c782984-0150-444f-92db-beeb40f1ffb8').
narrative_ontology:cs_kernel_codification('5c782984-0150-444f-92db-beeb40f1ffb8', formalized).
narrative_ontology:cs_authority_grounding('5c782984-0150-444f-92db-beeb40f1ffb8', extraction).
narrative_ontology:cs_interpretation_layer_present('5c782984-0150-444f-92db-beeb40f1ffb8').
narrative_ontology:cs_reading_relation('5c782984-0150-444f-92db-beeb40f1ffb8', modern_judicialization__devolution_settlements, coexists_with).
narrative_ontology:cs_reading_relation('5c782984-0150-444f-92db-beeb40f1ffb8', modern_judicialization__human_rights_act_1998, coexists_with).
narrative_ontology:cs_reading_relation('5c782984-0150-444f-92db-beeb40f1ffb8', modern_judicialization__uk_supreme_court_creation, influences).
narrative_ontology:cs_axiom('5c782984-0150-444f-92db-beeb40f1ffb8', foundational, parliamentary_sovereignty_absolute).
narrative_ontology:cs_axiom_status(parliamentary_sovereignty_absolute, overridden).
narrative_ontology:cs_axiom_grounding('5c782984-0150-444f-92db-beeb40f1ffb8', parliamentary_sovereignty_absolute, conventional).
narrative_ontology:cs_axiom('5c782984-0150-444f-92db-beeb40f1ffb8', foundational, supranational_law_overrides_domestic_statute).
narrative_ontology:cs_axiom_status(supranational_law_overrides_domestic_statute, holdable).
narrative_ontology:cs_axiom_grounding('5c782984-0150-444f-92db-beeb40f1ffb8', supranational_law_overrides_domestic_statute, deontological).
narrative_ontology:cs_reference_frame('5c782984-0150-444f-92db-beeb40f1ffb8', westminster_supremacy_reasserted).
narrative_ontology:cs_drift_state('5c782984-0150-444f-92db-beeb40f1ffb8', contemporary_post_referendum_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('5c782984-0150-444f-92db-beeb40f1ffb8', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(modern_judicialization__eu_membership_and_exit, modern_judicialization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(modern_judicialization__eu_membership_and_exit, single_market_integrationists).
narrative_ontology:constraint_beneficiary(modern_judicialization__eu_membership_and_exit, sovereignty_restorationists).
narrative_ontology:constraint_victim(modern_judicialization__eu_membership_and_exit, subordinated_legislatures).
narrative_ontology:constraint_victim(modern_judicialization__eu_membership_and_exit, european_rights_framework).
narrative_ontology:constraint_victim(modern_judicialization__eu_membership_and_exit, cross_border_citizens).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CROSS-BORDER CITIZEN (TANGLED ROPE) — Citizens with settled status under EU rights face extraction during the membership phase (pooled rights, supranational dispute resolution) and renationalization phase (visa requirements, lost residence guarantees, deportation risk). Trapped by the phase transition; coordination function (rights coordination) collapses into pure extraction (sovereignty reclamation). Experiences both suppression (cannot exit the territory) and coordination (original single-market rules enabled mobility). Maximum extraction at exit-transition.
constraint_indexing:constraint_classification(modern_judicialization__eu_membership_and_exit, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: SUBORDINATED LEGISLATURE (SNARE) — Scottish Parliament, Welsh Senedd, Northern Ireland Assembly: constrained by Westminster supremacy before devolution, further constrained by EU supremacy during membership, then liberated-yet-trapped by renationalization (UK Parliamentary sovereignty reasserted, but with heightened constitutional brittleness). Experiences suppression throughout (Westminster never ceded true legislative supremacy). Extraction shifts across phases but does not disappear.
constraint_indexing:constraint_classification(modern_judicialization__eu_membership_and_exit, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: SINGLE MARKET BENEFICIARY (ROPE) — Financial services, agribusiness, multinational supply chains: membership provided coordination function (frictionless trade, unified standards, labor mobility). Extraction is coordination cost (regulatory harmonization, dispute resolution fees, some loss of competitive advantage to continental firms). But genuine coordination benefit sustained the constraint. Beneficiary with high exit capacity (can relocate, diversify supply chains) but chooses not to exit during membership.
constraint_indexing:constraint_classification(modern_judicialization__eu_membership_and_exit, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: REMAIN CAMPAIGN COALITION (SCAFFOLD) — Organized coalition arguing for continued membership and managed integration. Sees the constraint as temporary coordination problem with a sunset clause: the Brexit referendum itself is the sunset mechanism (even though the outcome was exit, the campaign embodied scaffold logic — the view that the membership extraction could be renegotiated and resolved through reformed integration). Low effective extraction because this perspective has agency and visibility.
constraint_indexing:constraint_classification(modern_judicialization__eu_membership_and_exit, scaffold,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: SOVEREIGNTY RESTORATIONIST (SNARE) — Political actors (UKIP, Leave campaigners, constitutional nationalists) perceive EU membership as extraction pure: loss of parliamentary sovereignty, foreign courts overriding domestic law, immigrant labor suppressing wages. See supranational rules as suppressive mechanism with no legitimate coordination function. High extraction because the beneficiary (financial/corporate interests) controls the terms of debate. Experienced extractiveness is high because the restorationists have less capacity to renegotiate or exit (they are politically mobilized but institutionally marginal until the referendum shifts power).
constraint_indexing:constraint_classification(modern_judicialization__eu_membership_and_exit, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: WESTMINSTER PARLIAMENT (TANGLED ROPE) — Pool sovereignty during membership (genuine coordination of cross-border trade, rights, standards) while accepting supranational override of parliamentary supremacy. Then reclaim supremacy through exit vote, but face entanglement in devolved legislatures and Human Rights Act adjudication. Experiences both coordination benefit (EU rules simplified commerce, law) and extraction (cannot override EU standards, faces judicial review). Constrained exit because leaving membership requires managing devolution and rights framework simultaneously — cannot simply reclaim sovereignty.
constraint_indexing:constraint_classification(modern_judicialization__eu_membership_and_exit, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / HISTORICAL INEVITABILITY (MOUNTAIN) — From civilizational view, supranational integration is a structural requirement of post-imperial European states managing shared markets and rights. Exit is historically impossible — the attempt to restore absolute parliamentary sovereignty is doomed by economic interdependence, democratic rights entrenchment, and sub-state nationalism. This view naturalizes the constraint as immutable law of modern governance. However, the structural data contradicts this — the constraint has a clear lifecycle (membership phase, exit phase) and contested beneficiaries, indicating this is not a natural law but a political construction. False summit candidate.
constraint_indexing:constraint_classification(modern_judicialization__eu_membership_and_exit, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(modern_judicialization__eu_membership_and_exit_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(modern_judicialization__eu_membership_and_exit, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(modern_judicialization__eu_membership_and_exit, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(modern_judicialization__eu_membership_and_exit, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(modern_judicialization__eu_membership_and_exit_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint exhibits genuine coordination function (single market reduced transaction costs, unified standards simplified commerce, rights framework enabled cross-border life). But extraction is also real: sovereignty pooling meant parliamentary inability to override EU law; immigration flows benefited employers but suppressed wages in some sectors; regulatory harmonization imposed costs on non-integrated industries. The measurement trajectory (0.35 → 0.52 → 0.58) shows extractiveness rising as integration deepened and alternative arrangements became less visible. By exit, the accumulated extraction became salient enough to mobilize a majority against membership. Suppression (0.62): High. Early membership faced cultural and institutional resistance (conservative establishment, nationalism, church authority). This resistance had to be suppressed through: media consensus maintenance (elite newspapers, BBC balance rules that excluded radical exit talk as beyond reasonable debate), institutional gatekeeping (Conservative and Labour leadership excluding Eurosceptics), and reframing exit as impossible ('there is no alternative'). By the 1990s, suppression machinery intensified (Maastricht crisis, forced silence on devolution before referendum, delegitimization of UKIP). The referendum itself was a suppression-rupture: the previously marginal exit position became hegemonic. Theater ratio (0.65): Moderate-high. Early membership had genuine functional content: legal supremacy was real, rights protections worked, trade barriers fell. By the 2010s, theater increased: European citizenship became symbolic rather than operational for most citizens; devolution was treated as settled even while Westminster's relationship to it remained contradictory; the Human Rights Act became theatrical (courts declared incompatibilities they couldn't fix). Post-referendum theater peaked as government performed the 'will of the people' while managing impossibilities (Northern Ireland protocol, devolution incompatibility, citizens' rights unpacking). Claimed type (tangled_rope): Fits the signature: extractiveness 0.58 (within 0.40–0.90 range), suppression 0.62 (≥ 0.40), requires_active_enforcement true (suppression machinery persisted throughout). Beneficiaries and victims shift between phases but both remain present throughout. Coordination function (trade, rights, standards) and asymmetric extraction (sovereignty subordination) coexist.
 *
 * PERSPECTIVAL GAP:
 *   Cross-border citizens experience the constraint as snare during membership (trapped in a supranational order), then as suppression-without-exit during renationalization (trapped again, but in newly restricted domestic order). Single-market beneficiaries experience rope during membership (genuine coordination benefit outweighs sovereignty cost) but transition to constrained actors post-exit (extract benefits disappear, must renegotiate trade). Sovereignty restorationists experience snare during membership (extraction masked by elite consensus) and attempted escape during exit (resort to referendum to override suppression), but then face the theatrical realization that exit solves nothing (devolution remains, rights framework persists, cross-border citizens remain). Westminster Parliament experiences tangled_rope throughout: genuine coordination function (trade, standards) alongside sovereignty suppression, then attempted reclamation (exit vote) alongside new constraints (devolved legislatures won't resubmit). Subordinated legislatures experience triple extraction: from Westminster before devolution, from Westminster+EU during membership, from Westminster again (newly brittle) post-exit. The analytical observer risks seeing the constraint as a natural law of modern governance (mountain) when it is actually a contingent political construction (tangled_rope) with a definite lifecycle and contested beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from beneficiary/victim declarations and exit options. Single-market beneficiaries (institutional/arbitrage) experience low directionality (d ≈ 0.15–0.20): they benefit and can exit (relocate supply chains, diversify markets), so effective extraction is dampened. Sovereignty restorationists (powerful/mobile → powerless-organized post-referendum): directionality shifts across phases. During membership, they are marginalized (constrained or trapped exit from politics) and experience high extraction (d ≈ 0.80). Post-referendum, they capture state power but discover exit doesn't solve the constraint — directionality flips paradoxically because they now benefit from the outcome but realize the constraint structure persists. Cross-border citizens (powerless/trapped): maximum directionality (d ≈ 0.95) throughout both phases — they bear costs in membership (supranational governance they don't control) and post-exit (rights loss, residence loss, identity loss). Subordinated legislatures: constrained exit throughout (cannot simply abandon devolved powers), directionality increases from (d ≈ 0.60) during membership to (d ≈ 0.75) post-exit as Westminster attempts to reassert supremacy. Westminster Parliament: constrained exit (cannot simply exit from devolution or rights framework), directionality shifts between phases as extraction source changes (supranational → sub-state).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolves through phase distinction and beneficiary identification. In the membership phase, the constraint is genuinely tangled_rope: coordination function (single market, rights) is real and enables cross-border activity; extraction (sovereignty pooling, immigration dynamics) is also real but experienced differently by different agents. The suppression (media consensus, institutional gatekeeping) works to maintain the elite consensus that exit is impossible. In the renationalization phase, the constraint structure persists but its manifestation changes: Westminster attempts to reclaim sovereignty (fails, due to devolution and rights entrenchment); sovereignty restorationists mobilize (succeed in referendum); exit occurs (extraction continues in new form). The critical insight: renationalization is not resolution but reconfiguration. The constraint does not disappear when membership exits because the underlying tension (absolute parliamentary sovereignty vs. modern interdependence + distributed authority) persists. This is why both single-market beneficiaries and sovereignty restorationists can claim victory — they are benefiting from different phases of the same tangled constraint. The mandatrophy is resolved by recognizing that tangled_rope can have a lifecycle with phase-dependent beneficiaries, not by proving one side was always right.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    phase_extraction_asymmetry,
    'Does extractiveness genuinely differ between the membership phase and the renationalization phase, or is the same extraction mechanism simply reframed?',
    'Comparative analysis of institutional constraint patterns: regulatory burden metrics before/after exit; parliamentary override capacity before/after; citizen mobility restrictions before/after; judicial supremacy before/after',
    'If phases are structurally distinct (extraction + coordination in membership vs. suppression + reclamation in renationalization): two different constraints should be written separately. If extraction is constant and only the framing changes: one constraint with phase-dependent beneficiaries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(phase_extraction_asymmetry, empirical, 'Whether extractiveness genuinely differs between membership and exit phases or is reframed').

omega_variable(
    parliamentary_supremacy_restoration_thesis,
    'Can absolute parliamentary sovereignty be restored, or has the Human Rights Act and devolution permanently fragmented Westminster authority at the institutional level?',
    'Test cases: attempt to override Human Rights Act protections (Parliament vs. courts); attempt to revoke devolved powers (Westminster vs. legislatures); attempt to restore blanket executive prerogative (Parliament vs. judicial review). Structural outcome: does Westminster actually regain unilateral authority or does it remain constrained by distributed sovereignty?',
    'If sovereignty is restored: renationalization succeeded and constraint moves toward rope or piton. If sovereignty remains fragmented: renationalization is performative and constraint remains tangled_rope or snare throughout both phases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(parliamentary_supremacy_restoration_thesis, empirical, 'Whether parliamentary sovereignty can be functionally restored post-exit').

omega_variable(
    beneficiary_identity_switch,
    'Are single-market beneficiaries and sovereignty-restorationists genuinely distinct beneficiary groups, or do they represent different frames for the same elite interests?',
    'Network analysis of campaign funding, corporate lobbying, and institutional affiliations for both camps; tracking of actor positions across multiple constraint stories (this story, devolution_settlements, human_rights_act); correlation of asset-ownership with exit position',
    'If distinct: beneficiary group switches between phases, confirming tangled_rope with phase-dependent asymmetry. If same elite with reframed interests: constraint is snare throughout, with beneficiary rhetorical shift masking constant extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identity_switch, empirical, 'Whether beneficiary identity genuinely changes between phases or reframes constant elite interests').

omega_variable(
    devolution_independence_vector,
    'Does Scottish, Welsh, and Northern Irish legislature constrained exit create a secondary independence constraint that forecloses the Westminster supremacy restoration narrative?',
    'Doctrinal analysis: can Westminster truly reclaim supremacy while devolved legislatures possess entrenched powers? If devolution is irreversible (as the Scotland Act 1998 text claims), does exit-phase Westminster face the same supranational constraint from sub-state legislatures that it faced from EU?',
    'If devolution forecloses Westminster restoration: the renationalization is theatrical and the constraint remains tangled_rope. If Westminster can override devolution: restoration is genuine and the constraint may move toward rope (if benefits reaccrue to single-market actors) or piton (if sovereignty restoration is performative).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(devolution_independence_vector, conceptual, 'Whether devolution constraints foreclose Westminster supremacy restoration').

omega_variable(
    reading_contest_kernel_stability,
    'Which kernel holds: modern judicialization as constitutional fact (requiring all four readings: devolution, EU exit, rights entrenchment, judicial separation)? Or is each reading a distinct kernel with no necessary relationship to others?',
    'Historical-institutional analysis: trace whether devolution, EU membership, Human Rights Act, and Supreme Court creation are causally linked or independently motivated; identify actors who appealed to all four as a unified constitutional strategy vs. those who treat each as separate reform.',
    'If unified kernel: all four readings are manifestations of one constraint (modern judicialization through distributed authority). If separate: four independent constraints linked only by temporal coincidence. This determines whether network.affects_constraints should list all siblings or none.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_kernel_stability, conceptual, 'Whether the four readings instantiate a single kernel or separate kernels').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(modern_judicialization__eu_membership_and_exit, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eumex_theater_t0_genuine_integration, modern_judicialization__eu_membership_and_exit, theater_ratio, 0, 0.35).
narrative_ontology:measurement(eumex_theater_t20_euroscepticism_rise, modern_judicialization__eu_membership_and_exit, theater_ratio, 20, 0.58).
narrative_ontology:measurement(eumex_theater_t40_post_ref_negotiation, modern_judicialization__eu_membership_and_exit, theater_ratio, 40, 0.65).

% Extraction over time
narrative_ontology:measurement(eumex_extract_t0_accession, modern_judicialization__eu_membership_and_exit, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(eumex_extract_t20_deepening, modern_judicialization__eu_membership_and_exit, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(eumex_extract_t40_referendum, modern_judicialization__eu_membership_and_exit, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(eumex_suppress_t0_early_resistance, modern_judicialization__eu_membership_and_exit, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(eumex_suppress_t20_mobilization, modern_judicialization__eu_membership_and_exit, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(eumex_suppress_t40_exit_politics, modern_judicialization__eu_membership_and_exit, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(modern_judicialization__eu_membership_and_exit, enforcement_mechanism).
narrative_ontology:affects_constraint(modern_judicialization__eu_membership_and_exit, devolution_settlements).
narrative_ontology:affects_constraint(modern_judicialization__eu_membership_and_exit, human_rights_act_1998).
narrative_ontology:affects_constraint(modern_judicialization__eu_membership_and_exit, uk_supreme_court_creation).

% DUAL FORMULATION NOTE:
% This story decomposes modern_judicialization kernel's supranational dimension. The kernel's four sibling readings (devolution, rights, Supreme Court, EU exit) all represent the same underlying constitutional transition from Westminster supremacy to distributed authority. ε-invariance: EU membership and exit can be evaluated via 'degree of sovereignty pooling' observable (ε ≈ 0.58); devolution can be evaluated via 'degree of sub-state override' observable (different ε); rights via 'degree of judicial override'; Supreme Court via 'degree of structural separation'. Each observable yields a different constraint story with its own ε. They are linked by network.affects_constraints to show the distributed authority pattern they collectively instantiate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(modern_judicialization__eu_membership_and_exit, powerful, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
