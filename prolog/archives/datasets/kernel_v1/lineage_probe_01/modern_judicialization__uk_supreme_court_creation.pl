% ============================================================================
% CONSTRAINT STORY: modern_judicialization__uk_supreme_court_creation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_modern_judicialization__uk_supreme_court_creation, []).

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
 *   constraint_id: modern_judicialization__uk_supreme_court_creation
 *   human_readable: UK Supreme Court Creation and Judicial Separation (2005 Reforms)
 *   domain: political/legal/constitutional
 *
 * SUMMARY:
 *   The Constitutional Reform Act 2005 physically and institutionally
 *   separated the UK's highest court from Parliament. The Law Lords departed
 *   Westminster; the new Supreme Court took its seat on Parliament's opposite
 *   flank. This reading of modern judicialization focuses on the judicial
 *   independence axis: the constraint that emerges from the 2005 reforms is
 *   the asymmetry between the judiciary's structural dependence on
 *   parliamentary statute-making and the judiciary's newfound institutional
 *   autonomy claims. The reform suppressed the fused judicial-legislative
 *   identity of the old Lord Chancellor's office — where a single figure
 *   embodied legislative, executive, and judicial authority — in favor of
 *   explicit separation. The beneficiary is the clarity of institutional
 *   roles; the victim is the informal coordination mechanism that the old
 *   fused office enabled. This is a scaffold constraint because the
 *   separation-of-powers doctrine is being installed as a temporary
 *   coordination mechanism that itself becomes obsolete as norms of
 *   parliamentary deference mature and the extraction mechanism of judicial
 *   dependence on Parliament loses force.
 *
 * KEY AGENTS:
 *   - The Judiciary (now separated): Primary beneficiary (institutional/arbitrage) — gains autonomy, clearer institutional identity, explicit independence claims.
 *   - Parliament / Executive: Constrained institutional actor (institutional/constrained) — loses direct oversight mechanism but gains legitimacy from separation-of-powers form; must now influence judiciary through statute and appointment rather than direct presence.
 *   - Constitutional Litigants: Secondary beneficiary (powerless/constrained) — gain access to a court that claims independence from the legislature; face reduced but persistent institutional friction.
 *   - Lord Chancellor's Office (residual): Victim of suppressed fused identity (institutional/constrained) — the tri-cornered office persists ceremonially but lacks substantive power; extraction mechanism now is its continued existence as performative theater.
 *   - Analytical Observer: Perceives this as natural-law separation-of-powers necessity; at risk of naturalizing what is a contingent institutional choice.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(modern_judicialization__uk_supreme_court_creation, 0.35).
domain_priors:suppression_score(modern_judicialization__uk_supreme_court_creation, 0.28).
domain_priors:theater_ratio(modern_judicialization__uk_supreme_court_creation, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(modern_judicialization__uk_supreme_court_creation, extractiveness, 0.35).
narrative_ontology:constraint_metric(modern_judicialization__uk_supreme_court_creation, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(modern_judicialization__uk_supreme_court_creation, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(modern_judicialization__uk_supreme_court_creation, scaffold).
narrative_ontology:human_readable(modern_judicialization__uk_supreme_court_creation, "UK Supreme Court Creation and Judicial Separation (2005 Reforms)").
narrative_ontology:topic_domain(modern_judicialization__uk_supreme_court_creation, "political/legal/constitutional").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(modern_judicialization__uk_supreme_court_creation, '8bc61d64-517f-4fa5-841e-4e0b68ff5d51').
narrative_ontology:cs_kernel_codification('8bc61d64-517f-4fa5-841e-4e0b68ff5d51', formalized).
narrative_ontology:cs_authority_grounding('8bc61d64-517f-4fa5-841e-4e0b68ff5d51', lineage).
narrative_ontology:cs_interpretation_layer_present('8bc61d64-517f-4fa5-841e-4e0b68ff5d51').
narrative_ontology:cs_reading_relation('8bc61d64-517f-4fa5-841e-4e0b68ff5d51', modern_judicialization__devolution_settlements, coexists_with).
narrative_ontology:cs_reading_relation('8bc61d64-517f-4fa5-841e-4e0b68ff5d51', modern_judicialization__eu_membership_and_exit, influences).
narrative_ontology:cs_reading_relation('8bc61d64-517f-4fa5-841e-4e0b68ff5d51', modern_judicialization__human_rights_act_1998, influences).
narrative_ontology:cs_axiom('8bc61d64-517f-4fa5-841e-4e0b68ff5d51', foundational, judicial_institutional_autonomy_desirable).
narrative_ontology:cs_axiom_status(judicial_institutional_autonomy_desirable, holdable).
narrative_ontology:cs_axiom_grounding('8bc61d64-517f-4fa5-841e-4e0b68ff5d51', judicial_institutional_autonomy_desirable, deontological).
narrative_ontology:cs_axiom('8bc61d64-517f-4fa5-841e-4e0b68ff5d51', foundational, separation_of_powers_institutional_necessity).
narrative_ontology:cs_axiom_status(separation_of_powers_institutional_necessity, holdable).
narrative_ontology:cs_axiom_grounding('8bc61d64-517f-4fa5-841e-4e0b68ff5d51', separation_of_powers_institutional_necessity, conventional).
narrative_ontology:cs_reference_frame('8bc61d64-517f-4fa5-841e-4e0b68ff5d51', westminster_parliamentary_supremacy).
narrative_ontology:cs_drift_state('8bc61d64-517f-4fa5-841e-4e0b68ff5d51', post_2005_separated_judiciary, gap(codification_collapse, substantial, true)).
narrative_ontology:cs_created_at('8bc61d64-517f-4fa5-841e-4e0b68ff5d51', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(modern_judicialization__uk_supreme_court_creation, modern_judicialization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(modern_judicialization__uk_supreme_court_creation, judicial_independence_narrative).
narrative_ontology:constraint_beneficiary(modern_judicialization__uk_supreme_court_creation, institutional_clarity).
narrative_ontology:constraint_victim(modern_judicialization__uk_supreme_court_creation, lord_chancellor_office).
narrative_ontology:constraint_victim(modern_judicialization__uk_supreme_court_creation, informal_legislative_judicial_coordination).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE CONSTITUTIONAL LITIGANT (SCAFFOLD) — Benefits from clearer separation of powers (appeals to independent judiciary rather than Parliament); constrained by remaining institutional friction and the incomplete exit from Commons shadow-casting. The litigant perceives a temporary coordination structure that improves their position relative to the old fused model while retaining vestiges of the old framework. Sunset logic: as the Supreme Court's independence matures and parliamentary deference becomes established convention, the extractive friction of dual legitimacy claims dissolves.
constraint_indexing:constraint_classification(modern_judicialization__uk_supreme_court_creation, scaffold,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: THE SEPARATED JUDICIARY / SUPREME COURT (ROPE) — Institutional beneficiary of the reform. Experiences the constraint as coordination: clear separation of venue and identity enable functional independence. The judiciary now occupies a distinct institutional space with its own legitimacy claim. The extraction flow runs toward the judiciary in the form of enhanced autonomy. Net perception: pure coordination mechanism enabling the judiciary to function without legislative interference.
constraint_indexing:constraint_classification(modern_judicialization__uk_supreme_court_creation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: PARLIAMENT / EXECUTIVE BRANCH (TANGLED ROPE) — Constrained by loss of direct oversight over the highest court but benefits from the legitimacy boost of explicit separation-of-powers form. Parliament retains indirect influence through statute-writing and appointment protocols. Genuine coordination function (Parliament legislates, courts interpret) coexists with asymmetric extraction: the judiciary gains autonomy that constrains Parliament's ability to reverse unfavorable rulings through simple legislative override. Mixed experience: benefit from constitutional clarity, cost of reduced appellate control.
constraint_indexing:constraint_classification(modern_judicialization__uk_supreme_court_creation, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE LORD CHANCELLOR'S OFFICE / RESIDUAL FUSED IDENTITY (PITON) — Once a tri-cornered office holding legislative, judicial, and executive authority simultaneously, the role now exists as a performative residuum. The Lord Chancellor retains nominal connection to the judiciary but lacks functional authority (Supreme Court is now independent). The continued existence of the office is theater: it maintains the appearance of continuity while the actual power has departed. Piton classification derives from theater_ratio (0.42) — the office's continued ceremonial existence masks the structural separation that has already occurred.
constraint_indexing:constraint_classification(modern_judicialization__uk_supreme_court_creation, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (SEPARATION-OF-POWERS NECESSITY) (MOUNTAIN) — From a civilizational/universal perspective, the separation of judicial from legislative power is treated as an immutable principle of political organization — a natural law derived from Montesquieu and embedded in democratic theory. The 2005 reform appears as the expression of an unchangeable structural necessity rather than a contingent institutional choice. However, this perspective risks false summitry: the separation was not natural law but a political choice that could have been deferred, compromised, or rejected. The constraint shows apparent mountain properties (high accessibility_collapse, low resistance) only from the post-hoc perspective of a reform already deemed necessary.
constraint_indexing:constraint_classification(modern_judicialization__uk_supreme_court_creation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(modern_judicialization__uk_supreme_court_creation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(modern_judicialization__uk_supreme_court_creation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(modern_judicialization__uk_supreme_court_creation, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(modern_judicialization__uk_supreme_court_creation, TR),
    TR >= 0.70.

:- end_tests(modern_judicialization__uk_supreme_court_creation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Low-to-moderate. The constraint extracts in the form of asymmetric judicial dependence on parliamentary statute-writing — the judiciary gained independence in form (institutional autonomy, separate venue) but remains substantively dependent on Parliament's legislative choices for jurisdiction, remedies, and resource allocation. The extraction is not maximal because the judiciary does exercise real autonomy (ability to reverse or reinterpret parliamentary intent within judicial reasoning), and the separation benefited multiple parties (clearer roles, enhanced legitimacy). The historical trajectory shows declining extractiveness: as institutional norms mature (t=0→t=15), the friction of dual legitimacy claims decreases and the coordination function strengthens. Suppression (0.28): Low-to-moderate. The old fused identity was suppressed — the Lord Chancellor's tri-cornered office was effectively split into separate roles, preventing any future occupation of simultaneous judicial-legislative-executive authority. This suppression is structural (institutional redesign) not coercive (no forced exit, no penalties). Barriers to reverting to the old model are institutional rather than violent. Theater ratio (0.42): Moderate. The continued ceremonial existence of the Lord Chancellor's office (now purely judicial oversight) is performative — the office maintains the appearance of historical continuity while the actual power has departed to the separated Supreme Court. The institutional redesign itself was partly theater: the separation was framed as restoring a natural constitutional principle, but it was a choice about institutional form that could have been postponed or compromised.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary (judiciary) perceives pure coordination — clarity of roles and institutional autonomy serve their function. Parliament perceives tangled rope — coordination is genuine (Parliament legislates, courts interpret) but the loss of direct appellate oversight is extraction. Litigants perceive scaffolding — improved access to an independent court is a temporary coordination improvement with a sunset (as parliamentary deference norms establish, the friction dissolves). The residual Lord Chancellor's office appears as piton — ceremonial performance masking the substantive departure of authority. The analytical observer risks seeing mountain — treating the separation-of-powers principle as an immutable law of political organization — but this naturalizes what was a contingent choice about institutional redesign. The perspectival gap reveals the tension between institutional form (separate space, separate personnel) and substantive dependence (Parliament still controls statute, remedies, jurisdiction).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (judiciary, institutional clarity) derive low directionality (d ≈ 0.15-0.25) because the extraction flow runs toward them — they gain autonomy and legitimacy. Victims (Lord Chancellor's office as fused institution, informal coordination) derive high directionality (d ≈ 0.75-0.85) because the constraint suppresses their previous role and extracts the option to maintain tri-cornered authority. Parliament as constrained institutional actor derives moderate directionality (d ≈ 0.55) because it both benefits (legitimacy gain, constitutional clarity) and bears cost (loss of appellate oversight). The powerless litigant derives moderate-high directionality (d ≈ 0.65) because they benefit in form (access to independent court) but remain substantively dependent on Parliament's institutional choices (d reduction reflects the genuine coordination function). The analytical observer derives high directionality for the naturalization extraction (d ≈ 0.70) — the observer is a victim of the mountain classification's false summit trap.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy through perspectival differentiation. The judiciary experiences the constraint as rope (coordination enabling their function), Parliament as tangled rope (coordination coexisting with asymmetric extraction), litigants as scaffold (temporary improvement with sunset), and the analytical observer as mountain (at risk of naturalizing contingency). No single type is correct — the reading's job is to show HOW the constraint appears from each structural position. The constraint resolves into clear perspectival differences because the beneficiary set (judiciary, institutional clarity) and victim set (fused authority, informal coordination) are distinct and occupy different structural positions. The scaffold classification itself is the mandatrophy resolution: it acknowledges that this is a real institutional improvement (genuine coordination function) while also marking it as a transition state with a sunset — the extraction mechanism (judicial dependence on Parliament) will diminish as norms mature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    separation_form_vs_substance,
    'Does the spatial and institutional separation of the Supreme Court from Parliament constitute genuine independence, or is it primarily a change in form that preserves substantive parliamentary dominance through statute-writing and appointment control?',
    'Empirical analysis of post-2005 rulings: frequency of Supreme Court reversals of implied parliamentary intent; patterns of statutory rewriting following unfavorable rulings; appointment pipeline analysis showing parliamentary influence persistence.',
    'If separation is substantive: the constraint is a genuine structural shift toward judicial independence (scaffold with real sunset, Rope from judiciary perspective). If form only: the constraint is performative (theater_ratio should rise to ≥0.65, reclassifying toward Piton).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(separation_form_vs_substance, empirical, 'Whether Supreme Court separation is substantive or formal').

omega_variable(
    lord_chancellor_residual_extraction,
    'Does the continued existence of the Lord Chancellor office as a performative position extract real costs from clarity of constitutional roles, or has it become sufficiently vestigial that theatrical maintenance no longer matters?',
    'Historical tracking of Lord Chancellor interventions in judicial matters post-2005; confusion rates among citizens and legal practitioners about the Lord Chancellor''s residual authority; frequency of public statements about the tri-cornered role''s meaning.',
    'If extractive: theater_ratio should rise and the piton classification is warranted. If vestigial: theater_ratio should fall and the constraint should reclassify toward pure separation (rope or mountain from some perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lord_chancellor_residual_extraction, empirical, 'Whether Lord Chancellor residual status extracts costs or is sufficiently vestigial').

omega_variable(
    reading_alternative_framing_constitutionalism,
    'This reading instantiates one interpretation of modern judicialization: the UK adopted Anglo-American separation-of-powers doctrine as a constitutive reform. Is this framing the only defensible reading of the 2005 reforms, or can they be read as purely administrative reorganization that did not fundamentally alter the Westminster constitution''s executive-dominant structure?',
    'Interpretive analysis of official reform documents; examination of whether the reforms were framed as constitutional principle or institutional housekeeping; subsequent jurisprudence checking whether courts have claimed autonomy greater than Westminster tradition permitted.',
    'If the administrative-reorganization reading is credible: this reading does not foreclose the alternatives (coexists_with rather than forecloses). If the separation-of-powers reading is uniquely compelling: stronger foreclosure claims against readings that deny judicial independence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_alternative_framing_constitutionalism, conceptual, 'Whether this reading is the only defensible constitutional interpretation or one of multiple coherent framings').

omega_variable(
    scaffold_sunset_temporal_reality,
    'Is the ''sunset'' logic of the scaffold classification real? Will institutional independence norms genuinely mature such that the coordination friction between judiciary and Parliament decreases, or does the extraction mechanism persist indefinitely as a structural feature of judicial dependence on parliamentary statute?',
    'Longitudinal measurement of coordination friction (frequency and magnitude of judicial reversals, statutory rewrites, appointment conflicts) over decades; assessment of whether norms of parliamentary deference are actually establishing or whether each generation of Parliament tests the boundaries again.',
    'If sunset is real: the scaffold classification is warranted. If extraction persists indefinitely: reclassify toward tangled_rope with high suppression and indefinite extractiveness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scaffold_sunset_temporal_reality, empirical, 'Whether the scaffold''s sunset logic will materialize or the constraint persists indefinitely').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(modern_judicialization__uk_supreme_court_creation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uksc_tr_t0, modern_judicialization__uk_supreme_court_creation, theater_ratio, 0, 0.55).
narrative_ontology:measurement(uksc_tr_t7, modern_judicialization__uk_supreme_court_creation, theater_ratio, 7, 0.42).
narrative_ontology:measurement(uksc_tr_t15, modern_judicialization__uk_supreme_court_creation, theater_ratio, 15, 0.35).

% Extraction over time
narrative_ontology:measurement(uksc_be_t0, modern_judicialization__uk_supreme_court_creation, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(uksc_be_t7, modern_judicialization__uk_supreme_court_creation, base_extractiveness, 7, 0.38).
narrative_ontology:measurement(uksc_be_t15, modern_judicialization__uk_supreme_court_creation, base_extractiveness, 15, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(uksc_su_t0, modern_judicialization__uk_supreme_court_creation, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(uksc_su_t7, modern_judicialization__uk_supreme_court_creation, suppression_requirement, 7, 0.32).
narrative_ontology:measurement(uksc_su_t15, modern_judicialization__uk_supreme_court_creation, suppression_requirement, 15, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(modern_judicialization__uk_supreme_court_creation, enforcement_mechanism).
narrative_ontology:affects_constraint(modern_judicialization__uk_supreme_court_creation, human_rights_act_1998).
narrative_ontology:affects_constraint(modern_judicialization__uk_supreme_court_creation, devolution_settlements).
narrative_ontology:affects_constraint(modern_judicialization__uk_supreme_court_creation, eu_membership_and_exit).

% DUAL FORMULATION NOTE:
% UK Supreme Court creation is one reading of the modern_judicialization kernel. All four readings (uk_supreme_court_creation, devolution_settlements, eu_membership_and_exit, human_rights_act_1998) share the same fixed-text kernel (the 2005-era constitutional reforms) but instantiate different structural changes. Each has its own constraint_id, its own ε value, and its own beneficiary/victim structure. The affects_constraints edges point to the sibling readings because they are entangled in the same historical moment and reinforce or cross-pressure each other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(modern_judicialization__uk_supreme_court_creation, institutional, 0.2).
constraint_indexing:directionality_override(modern_judicialization__uk_supreme_court_creation, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
