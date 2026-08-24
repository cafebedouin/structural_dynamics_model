% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_doctrinal_authority__composite_overdetermination_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: vatican_ii_doctrinal_authority__composite_overdetermination_reading
 *   human_readable: Vatican II Unitary Reform Narrative as Overdetermined Packaging
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   Vatican II (1962-1965) produced sixteen documents addressing liturgy,
 *   ecumenism, religious freedom, revelation, the Church in the modern world,
 *   and episcopal collegiality. The composite overdetermination reading
 *   argues these are four structurally independent changes — liturgical
 *   vernacularization (Sacrosanctum Concilium), ecumenical openness (Unitatis
 *   Redintegratio), religious freedom as right (Dignitatis Humanae), and
 *   collegial ecclesiology (Lumen Gentium) — packaged as a single 'reform' to
 *   prevent any component from being rejected in isolation. The unitary
 *   narrative functions as a constraint: it forces interpreters to accept or
 *   reject the whole package, suppressing disaggregated reception. This
 *   reading rejects both the continuity thesis (organic development) and both
 *   rupture theses (progressive/traditionalist) as category errors that treat
 *   the package as a single move.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.62).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.55).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__composite_overdetermination_reading, "Vatican II Unitary Reform Narrative as Overdetermined Packaging").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__composite_overdetermination_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 'dd2ed805-5a02-4e17-95e0-289f051d6238').
narrative_ontology:cs_kernel_codification('dd2ed805-5a02-4e17-95e0-289f051d6238', formalized).
narrative_ontology:cs_authority_grounding('dd2ed805-5a02-4e17-95e0-289f051d6238', lineage).
narrative_ontology:cs_interpretation_layer_present('dd2ed805-5a02-4e17-95e0-289f051d6238').
narrative_ontology:cs_reading_relation('dd2ed805-5a02-4e17-95e0-289f051d6238', vatican_ii_doctrinal_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('dd2ed805-5a02-4e17-95e0-289f051d6238', vatican_ii_doctrinal_authority__rupture_progressive_reading, coexists_with).
narrative_ontology:cs_reading_relation('dd2ed805-5a02-4e17-95e0-289f051d6238', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, coexists_with).
narrative_ontology:cs_axiom('dd2ed805-5a02-4e17-95e0-289f051d6238', foundational, vatican_ii_components_independently_variable).
narrative_ontology:cs_axiom_status(vatican_ii_components_independently_variable, holdable).
narrative_ontology:cs_axiom_grounding('dd2ed805-5a02-4e17-95e0-289f051d6238', vatican_ii_components_independently_variable, empirically_contingent).
narrative_ontology:cs_axiom('dd2ed805-5a02-4e17-95e0-289f051d6238', foundational, unitary_reform_narrative_is_constructed).
narrative_ontology:cs_axiom_status(unitary_reform_narrative_is_constructed, holdable).
narrative_ontology:cs_axiom_grounding('dd2ed805-5a02-4e17-95e0-289f051d6238', unitary_reform_narrative_is_constructed, conventional).
narrative_ontology:cs_reference_frame('dd2ed805-5a02-4e17-95e0-289f051d6238', conciliar_unity_narrative).
narrative_ontology:cs_drift_state('dd2ed805-5a02-4e17-95e0-289f051d6238', post_conciliar_reception, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('dd2ed805-5a02-4e17-95e0-289f051d6238', '2026-08-03T14:30:00Z').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, progressive_reformers).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, ecumenical_partners).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, liturgical_reformers).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, religious_freedom_advocates).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, traditionalist_resistors).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, lay_faithful_confused).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, local_ordinaries_caught_between).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__composite_overdetermination_reading, conciliar_hermeneutic_of_reform).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__composite_overdetermination_reading, pastoral_adaptation_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Use the ambiguity of the unitary reform narrative to advance liturgical, ecumenical, and governance changes beyond what texts explicitly authorize. Their exit is constrained because the magisterium remains the only authorization body; they work within the system to stretch its interpretive boundaries.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, progressive_reformers, beneficiary,
    organized, generational, constrained, global).

% Experience the overdetermined packaging as rupture disguised as continuity. They bear the cost of liturgical displacement, ecumenical compromise, and perceived doctrinal ambiguity. Their identity is fused to pre-conciliar forms; exit means schism or internal exile. They are excluded from authoritative interpretation by the same magisterium that claims to represent them.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, traditionalist_resistors, payer,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__composite_overdetermination_reading, traditionalist_resistors, excluded).

% Receive the packaged reform as a single authoritative shift but experience its components as incoherent — vernacular liturgy without doctrinal clarity, religious freedom without clear limits, collegiality without structural change. They pay in cognitive dissonance and diminished trust. Exit options are constrained by parish geography, family ties, and sacramental need.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, lay_faithful_confused, payer,
    powerless, biographical, constrained, global).

% Must implement the unitary narrative locally while managing contradictory pressures from Rome, progressive priests, traditionalist laity, and ecumenical partners. They set the agenda for diocesan reception but bear the cost of enforcement incoherence. Their exit is constrained by episcopal vow and canonical structure.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, local_ordinaries_caught_between, agenda_setter,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__composite_overdetermination_reading, local_ordinaries_caught_between, payer).

% Benefit from the ecumenism component (Unitatis Redintegratio, Nostra Aetate) packaged within the unitary reform. They gain dialogue structures and recognition without conceding their own doctrinal positions. Their exit is mobile — they engage the Catholic Church as a partner, not a subject.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, ecumenical_partners, beneficiary,
    moderate, generational, mobile, global).

% Use Sacrosanctum Concilium's mandate for 'active participation' as license for extensive ritual restructuring beyond the text. The unitary narrative shields them from charges of rupture. Exit constrained by need for Roman approval of liturgical books.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, liturgical_reformers, beneficiary,
    organized, generational, constrained, global).

% Deploy Dignitatis Humanae as a universal principle against confessional state models. The unitary packaging gives it conciliar weight exceeding its textual specificity. They arbitrage between Catholic social teaching and secular human rights frameworks. Exit is arbitrage-grade — they operate across multiple institutional fields.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, religious_freedom_advocates, beneficiary,
    moderate, civilizational, arbitrage, global).

% Maintains the unitary reform narrative as essential to conciliar authority. Interprets texts to manage tensions between components. Could change the packaging but doing so would undermine the legitimacy structure it administers. Analytical exit: can observe the structure but cannot abandon the kernel without dissolving its own authority.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, magisterial_authority, agenda_setter,
    institutional, civilizational, analytical, universal).

% Produce the hermeneutical frameworks (continuity, rupture, reform) that constitute the reception debate. They neither collect rents nor bear enforcement costs directly, but their frameworks shape which component-readings gain traction. Analytical exit: they can change frameworks without personal cost.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, theologians_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Packages four distinct structural changes (liturgical vernacularization, ecumenical openness, religious freedom doctrine, collegial ecclesiology) into a single authoritative act so that each component gains the others' legitimacy and the Council as a whole resists piecemeal rejection.
% TRANSFER_FUNCTION: Moves interpretive authority from textual specificity to pastoral application — from the conciliar texts as fixed meaning to the 'spirit of the Council' as ongoing authorization. Transfers implementation risk from Rome to local ordinaries. Transfers doctrinal stability from the faithful to the theologians.
% ABSENT_VOICES: Pre-conciliar theological traditions (neo-scholastic manualists, liturgical movement pioneers who opposed vernacularization), Orthodox observers excluded from voting, Catholic laity in mission territories who received implemented reforms without participatory reception. They would object to the packaging but were not in the conciliar aula.
% DISAPPEARANCE_RATIONALE: If the unitary narrative vanished, each component would stand or fall on its own textual and reception history. Liturgical reform would face textualist challenge; religious freedom would revert to pre-conciliar error-theory; ecumenism would lose its conciliar mandate; collegiality would lack structural implementation. The magisterium would lose its primary post-conciliar legitimacy anchor.
% FOUNDING_PROBLEM: How to modernize the Church's self-presentation and external relations without triggering schism or doctrinal relativism — a single Council that could satisfy liturgical reformers, ecumenists, political liberals, and curial conservatives simultaneously.
% FOUNDING_PROBLEM_CORROBORATION: The founding coalition dissolved by 1968 (Humanae Vitae crisis). Progressive reformers acknowledge the unity was tactical (Congar, Rahner private correspondence). Traditionalists attest the problem was misdiagnosed — there was no single problem but four distinct ones. No living participant claims the original packaging problem persists; the magisterium maintains the narrative for legitimacy, not because the founding problem remains.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__composite_overdetermination_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__composite_overdetermination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_doctrinal_authority__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.62: the packaging extracts interpretive labor from local ordinaries and lay faithful who must implement incoherent components as a coherent whole. It enables progressive capture of ambiguity (liturgy, ecumenism) while traditionalists bear the cost of perceived rupture. Suppression 0.55: alternative hermeneutics (disaggregated reading, component-specific rejection) are marginalized by magisterial insistence on 'hermeneutic of reform.' Theater 0.42: the unity performance is real — curial documents (Catechism, Compendium, papal audiences) continuously reassert conciliar unity — but the underlying components drift independently. Accessibility collapse 0.48: disaggregated readings exist (Ratzinger's 'hermeneutic of discontinuity,' Ladaria's component analyses) but remain academically marginal. Resistance 0.68: high from traditionalists (SSPX, sedevacantists, conservative episcopate) and progressive critics (women's ordination, LGBT inclusion) who find the packaging blocks their claims.
 *
 * PERSPECTIVAL GAP:
 *   The magisterial seat experiences the constraint as genuine coordination (solving the founding problem). Progressive reformers experience it as enabling scaffold (temporary unity now obsolete). Traditionalists experience it as snare (ruinous packaging they cannot exit). Lay faithful experience it as piton (inertial performance they endure). The engine computes this divergence from the structural data — the same constraint is mountain to the magisterium (emerges_naturally: true from its seat), rope to progressives, snare to traditionalists, piton to laity.
 *
 * DIRECTIONALITY LOGIC:
 *   Magisterial authority: d ≈ 0.15 (beneficiary — the packaging generates its legitimacy). Progressive reformers: d ≈ 0.25 (beneficiary — ambiguity enables advance). Traditionalist resistors: d ≈ 0.9 (full target — identity-locked, bears extraction). Lay faithful: d ≈ 0.65 (payer — constrained exit, bears cognitive cost). Ecumenical partners: d ≈ 0.1 (beneficiary — mobile, gains without cost). Liturgical reformers: d ≈ 0.2 (beneficiary — constrained but net positive). Religious freedom advocates: d ≈ 0.05 (beneficiary — arbitrage-grade exit). Local ordinaries: d ≈ 0.55 (near symmetric — agenda setters who also pay enforcement costs). The derivation follows beneficiary/victim declarations and exit modulation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (single Council satisfying four factions) died in 1968. The constraint persists because the magisterium extracts legitimacy from the unitary narrative — admitting disaggregation would require acknowledging that some components failed or succeeded independently. This is mandatrophy: the coordination function (preventing schism via package deal) is dead; the extraction function (legitimacy maintenance via unity performance) is live. The constraint is not a snare because the magisterium does not primarily benefit materially — it benefits legitimationally. It is a tangled rope because the coordination residue (common liturgical calendar, shared ecumenical framework) still operates alongside the extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    packaging_intentionality,
    'Was the overdetermined packaging a deliberate curial strategy (Rahners'' ''superdogma'' maneuver) or an emergent property of conciliar compromise?',
    'Archival research on conciliar commissions'' voting records, periti correspondence, and papal intervention logs. Comparative analysis with Trent and Vatican I packaging strategies.',
    'If deliberate, the constraint is designed extraction (snare-adjacent). If emergent, it is structural accident (tangled_rope with lower culpability). Affects suppression attribution: designed suppression vs. inertial suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(packaging_intentionality, empirical, 'Whether the unitary narrative was engineered or emerged.').

omega_variable(
    disaggregation_extraction_delta,
    'Would disaggregated reception (each component judged on its own textual and reception merits) reduce total extractiveness across all seats?',
    'Counterfactual modeling: compare current extraction (packaged) with hypothetical extraction if each document had been received, implemented, and contested separately. Measure at each stakeholder seat.',
    'If disaggregation reduces extraction at traditionalist and lay seats without increasing it at progressive seats, the packaging is net extractive overhead. If it increases conflict extraction (schism risk), the packaging has residual coordination value.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(disaggregation_extraction_delta, conceptual, 'Whether the packaging''s coordination value exceeds its extraction cost.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of disaggregated readings structural (curial censorship, appointment control) or internalized (theologians self-censor to remain in communion, laity internalize ''hermeneutic of continuity'' as piety)?',
    'Post-exit suppression trajectory: track theologians who left or were silenced (Küng, Boff, Schillebeeckx) — did their suppression persist after institutional sanctions? Survey laity on whether they feel free to question conciliar unity privately vs. publicly.',
    'If internalized, effective suppression is higher than structural measure suggests — the constraint travels with the agent. If structural, suppression is removable by institutional reform. Affects omega on identity_lock for traditionalists and laity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression of alternative hermeneutics.').

omega_variable(
    component_independence_degree,
    'Are the four components (liturgy, ecumenism, religious freedom, collegiality) genuinely independent in their textual logic and reception history, or do they share a deep conceptual grammar that makes disaggregation artificial?',
    'Formal theological analysis of cross-document references (e.g., Sacrosanctum Concilium''s ''active participation'' → Lumen Gentium''s ''royal priesthood'' → Gaudium et Spes''s ''autonomy of earthly realities''). Reception history correlation: do dioceses that implemented liturgical reform also implement ecumenism at correlated rates?',
    'If components share deep grammar, the composite reading overstates independence — the unitary narrative reflects real coherence. If independent, the packaging is genuinely overdetermined. Affects claimed_type: if coherent, continuity_reading gains ground; if independent, composite reading is validated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(component_independence_degree, conceptual, 'Whether the four components are structurally independent or conceptually unified.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 1962, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vatican_ii_composite_tr_t1962, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 1962, 0.15).
narrative_ontology:measurement(vatican_ii_composite_tr_t1965, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 1965, 0.22).
narrative_ontology:measurement(vatican_ii_composite_tr_t1968, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 1968, 0.35).
narrative_ontology:measurement(vatican_ii_composite_tr_t1978, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 1978, 0.38).
narrative_ontology:measurement(vatican_ii_composite_tr_t1992, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 1992, 0.4).
narrative_ontology:measurement(vatican_ii_composite_tr_t2005, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 2005, 0.42).
narrative_ontology:measurement(vatican_ii_composite_tr_t2013, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 2013, 0.43).
narrative_ontology:measurement(vatican_ii_composite_tr_t2024, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(vatican_ii_composite_be_t1962, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 1962, 0.35).
narrative_ontology:measurement(vatican_ii_composite_be_t1965, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 1965, 0.42).
narrative_ontology:measurement(vatican_ii_composite_be_t1968, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 1968, 0.55).
narrative_ontology:measurement(vatican_ii_composite_be_t1978, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 1978, 0.58).
narrative_ontology:measurement(vatican_ii_composite_be_t1992, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 1992, 0.6).
narrative_ontology:measurement(vatican_ii_composite_be_t2005, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 2005, 0.62).
narrative_ontology:measurement(vatican_ii_composite_be_t2013, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 2013, 0.63).
narrative_ontology:measurement(vatican_ii_composite_be_t2024, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 2024, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(vatican_ii_composite_su_t1962, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 1962, 0.25).
narrative_ontology:measurement(vatican_ii_composite_su_t1965, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 1965, 0.35).
narrative_ontology:measurement(vatican_ii_composite_su_t1968, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 1968, 0.52).
narrative_ontology:measurement(vatican_ii_composite_su_t1978, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 1978, 0.55).
narrative_ontology:measurement(vatican_ii_composite_su_t1992, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 1992, 0.53).
narrative_ontology:measurement(vatican_ii_composite_su_t2005, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 2005, 0.55).
narrative_ontology:measurement(vatican_ii_composite_su_t2013, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 2013, 0.56).
narrative_ontology:measurement(vatican_ii_composite_su_t2024, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__composite_overdetermination_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.08).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_ii_doctrinal_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_ii_doctrinal_authority__rupture_progressive_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_ii_doctrinal_authority__rupture_traditionalist_reading).

% DUAL FORMULATION NOTE:
% Kernel vatican_ii_doctrinal_authority decomposes into four readings. This reading (composite_overdetermination) treats the kernel as the constraint itself — the unitary packaging. The sibling readings treat the kernel as the content to be interpreted. This reading's ε measures the packaging's extraction; siblings' ε measure their interpretive frameworks' extraction. They are linked because the packaging constrains which interpretations are admissible.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_doctrinal_authority__composite_overdetermination_reading, institutional, 0.15).
constraint_indexing:directionality_override(vatican_ii_doctrinal_authority__composite_overdetermination_reading, organized, 0.25).
constraint_indexing:directionality_override(vatican_ii_doctrinal_authority__composite_overdetermination_reading, powerless, 0.65).
constraint_indexing:directionality_override(vatican_ii_doctrinal_authority__composite_overdetermination_reading, moderate, 0.1).
constraint_indexing:directionality_override(vatican_ii_doctrinal_authority__composite_overdetermination_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
