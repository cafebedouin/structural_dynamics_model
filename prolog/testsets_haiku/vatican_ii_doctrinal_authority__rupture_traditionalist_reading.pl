% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__rupture_traditionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_rupture_traditionalist, []).

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
 *   constraint_id: vatican_ii_doctrinal_authority__rupture_traditionalist_reading
 *   human_readable: Vatican II Doctrinal Authority — Rupture/Traditionalist Reading
 *   domain: ecclesiology/institutional history/hermeneutics
 *
 * SUMMARY:
 *   Vatican II (1962–1965) convened to modernize the Catholic Church and
 *   clarify doctrine in light of contemporary challenges. Under the
 *   rupture/traditionalist reading instantiated here, the Council represents
 *   a fundamental break with pre-conciliar teaching and practice, enabled by
 *   textual ambiguities that permitted divergent interpretation and
 *   post-conciliar implementation by reformers who exploited those
 *   ambiguities. The Council's documents contain errors and compromises that
 *   enabled heterodox theological development. The victims are traditional
 *   doctrine, the Latin liturgy, and those whose faith was rooted in
 *   pre-conciliar forms. This reading contests the official narrative of
 *   organic continuity and the progressive reading that treats the Council as
 *   insufficient and demands ongoing rupture. The measurement trajectory
 *   models increasing extractiveness and theater as post-conciliar
 *   implementation moved further from the Council's actual texts into
 *   interpretive overreach (1965–1990), plateauing as institutional
 *   resistance and the hermeneutic-of-continuity response hardened the
 *   constraint.
 *
 * KEY AGENTS:
 *   - reform_coalition: Bishops and theologians who engineered the Council and control post-conciliar interpretation (institutional power, arbitrary exit)
 *   - progressive_reformist_bishops: Regional hierarchs implementing Vatican II as doctrinal rupture (institutional, constrained exit)
 *   - doctrinal_traditionalists: Bishops, theologians, laypeople maintaining pre-conciliar standards (organized, identity-locked)
 *   - traditional_latin_mass_communities: Priests and laypeople devoted to pre-conciliar liturgy (moderate power, identity-locked, liturgically displaced)
 *   - vatican_doctrinal_offices: Curia defending doctrinal boundaries against heterodoxy (institutional, constrained, losing enforcement power)
 *   - progressive_theologians: Academics and pastors operating in ambiguity space and framing it as Council's spirit (powerful, arbitrage exit)
 *   - faithful_presuming_continuity: Lay Catholics taught pre-conciliar doctrine, then encountered radical reinterpretation (powerless, trapped, excluded from contest)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.68).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.72).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, "Vatican II Doctrinal Authority — Rupture/Traditionalist Reading").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, "ecclesiology/institutional history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__rupture_traditionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 'a76db021-d96f-4fa3-8073-7bfe9e1658fa').
narrative_ontology:cs_kernel_codification('a76db021-d96f-4fa3-8073-7bfe9e1658fa', formalized).
narrative_ontology:cs_authority_grounding('a76db021-d96f-4fa3-8073-7bfe9e1658fa', lineage).
narrative_ontology:cs_interpretation_layer_present('a76db021-d96f-4fa3-8073-7bfe9e1658fa').
narrative_ontology:cs_reading_relation('a76db021-d96f-4fa3-8073-7bfe9e1658fa', vatican_ii_doctrinal_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('a76db021-d96f-4fa3-8073-7bfe9e1658fa', vatican_ii_doctrinal_authority__rupture_progressive_reading, coexists_with).
narrative_ontology:cs_reading_relation('a76db021-d96f-4fa3-8073-7bfe9e1658fa', vatican_ii_doctrinal_authority__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('a76db021-d96f-4fa3-8073-7bfe9e1658fa', foundational, doctrinal_identity_essential_to_apostolic_transmission).
narrative_ontology:cs_axiom_status(doctrinal_identity_essential_to_apostolic_transmission, holdable).
narrative_ontology:cs_axiom_grounding('a76db021-d96f-4fa3-8073-7bfe9e1658fa', doctrinal_identity_essential_to_apostolic_transmission, deontological).
narrative_ontology:cs_axiom('a76db021-d96f-4fa3-8073-7bfe9e1658fa', foundational, conciliar_ambiguity_indicates_error_not_theological_depth).
narrative_ontology:cs_axiom_status(conciliar_ambiguity_indicates_error_not_theological_depth, holdable).
narrative_ontology:cs_axiom_grounding('a76db021-d96f-4fa3-8073-7bfe9e1658fa', conciliar_ambiguity_indicates_error_not_theological_depth, empirically_contingent).
narrative_ontology:cs_reference_frame('a76db021-d96f-4fa3-8073-7bfe9e1658fa', pre_conciliar_doctrinal_settlement).
narrative_ontology:cs_drift_state('a76db021-d96f-4fa3-8073-7bfe9e1658fa', post_1965_implementation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a76db021-d96f-4fa3-8073-7bfe9e1658fa', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, progressive_reformist_bishops).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, post_conciliar_institutional_authority).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditional_liturgy_advocates).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, doctrinal_traditionalists).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, episcopal_opponents_of_reform).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts low at the Council itself (0.35) because the conciliar event is ambiguous and its interpretation is still contested. It rises steeply through the 1960s–1970s (implementation phase under Paul VI) as reformers exploit ambiguities to implement doctrinal changes, reaching 0.59 by mid-1980s. By 1990 (John Paul II's papacy solidifying the hermeneutic of continuity), extractiveness plateaus at 0.68 as institutional resistance hardens — the constraint's growth slows not because the extraction ends but because its narrative frame shifts (claims of continuity rather than openness to ongoing reform). Theater ratio climbs faster and higher, moving from 0.25 (conciliar ambiguity treated as necessary) to 0.58 (Vatican II invoked as authority for reforms the texts do not clearly warrant). Suppression_requirement rises similarly, modeling the institutional enforcement needed to maintain the constraint as resistance from traditionalists and institutional skeptics grows. All three metrics share the same time grid (t=0, 5, 10, 15, 20, 25 years post-Council), ensuring temporal alignment. The claimed type is tangled_rope: there is genuine coordination benefit (the Council did clarify doctrine and opened dialogue), but it is packaged with asymmetric extraction (authority and interpretive power accrue to reformers; traditionalists bear the cost of doctrinal displacement and liturgical loss) and requires active enforcement (doctrinal offices struggling to police heterodoxy, suppression of traditional liturgy, institutional isolation of traditionalist bishops and communities).
 *
 * PERSPECTIVAL GAP:
 *   The reform coalition (agenda_setter seat) experiences this constraint as legitimate authority development and necessary modernization — they see themselves as defending the Council's true meaning against both pre-conciliar rigidity and against traditionalist misreadings. From their seat, extractiveness is low because they frame the constraint as coordination, not extraction. Traditionalist bishops and communities experience the same structure as enforced heterodoxy and stolen authority — extractiveness is very high because they perceive continuous displacement and institutional suppression. The engine will compute these divergent directionalities from the structural data: reformers sit near the beneficiary end (they control interpretation, collect institutional power), traditionalists sit near the target end (they bear costs, face suppression, are identity-locked). A faithful person taught pre-conciliar doctrine and then experiencing radical reinterpretation without transparency faces the highest directionality toward the target end: they were not invited to assess the changes and cannot easily exit. The measured extractiveness (0.68) reflects the traditionalist/victim seat's experience; the reformist seat would author far lower extractiveness and the engine should detect this divergence as seat-level type difference.
 *
 * DIRECTIONALITY LOGIC:
 *   Reform coalition: beneficiary, institutional power, arbitrary exit → d near 0.0 (full beneficiary, subsidized by the constraint). Progressive bishops: beneficiary, institutional power, constrained but not trapped → d ~ 0.15–0.25 (still beneficiary, less arbitrage than reform coalition). Traditionalist bishops: payer, institutional power, constrained exit → d ~ 0.65–0.75 (target, still institutional, but constrained by their vows and office). Doctrinal traditionalists (organized, moderate power): payer, identity-locked → d ~ 0.75–0.85 (high target, because identity-lock means exit is unthinkable). Traditional Latin Mass communities: victim, moderate power, identity-locked → d ~ 0.85–0.95 (highest target, because both direct liturgical displacement and identity fusion). Vatican doctrinal offices: payer but constrained in enforcement → d ~ 0.55–0.65 (symmetric to conflicted, because they have institutional voice but no veto). Faithful taught continuity but experiencing rupture: powerless, trapped, excluded → d ~ 0.90+ (effectively maximum target, because powerlessness + trap + exclusion from contestation). No directionality overrides are needed; the structural derivation from beneficiary/victim + exit + power should produce the right d values directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is modernist pressure on a defensive pre-conciliar Church. That founding problem is contested: reformers claim Vatican II solved it through opening and dialogue; traditionalists claim it created worse problems than it solved. The disappearance_verdict is world_rearranges: if Vatican II were dissolved, the pre-conciliar doctrinal settlement would re-establish, traditionalist liturgy would resume normative status, and the faithful would not experience the gap between taught continuity and implemented rupture. This suggests the constraint carries real mandate (it is not merely performative). However, the theater_ratio rises to 0.58 by interval end, indicating the constraint is increasingly maintained by invoking Vatican II as authority without textual warrant — the theater component is growing. The 'hermeneutic of continuity' (John Paul II and Benedict XVI's framing) is a narrative device asserting continuity while accepting many substantive changes; this is theater in the sense of performative assertion of what the texts do not straightforwardly support. The mandatrophy question is whether Vatican II's founding mandate can sustain this level of theater, or whether the constraint is approaching a point where institutional authority alone (rather than textual and pastoral consensus) carries it. Traditionalist calls for conciliar 'reform of the reform' suggest institutional actors are recognizing the mandatrophy risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_ambiguity_intentionality,
    'Were the ambiguities in Vatican II''s documents a deliberate compromise strategy enabling later interpretation, or a failure of conciliar pedagogy and theological precision?',
    'Archival evidence from conciliar commissions: preserved debate records, redaction history, and explicit statements by conciliar fathers about intentionality. Comparison of pre-conciliar doctrinal texts (which are precise) with conciliar texts to assess whether ambiguity is stylistic drift or deliberate framing.',
    'If intentional compromise: the Council itself bears responsibility for the rupture, and progressives were operating as the Council designed. If unintentional failure: the Council was betrayed, and traditionalists are right that post-conciliar heterodoxy reflects conciliar error rather than conciliar intent. This shifts locus of causation and culpability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_ambiguity_intentionality, empirical, 'Whether doctrinal ambiguity in Vatican II texts was deliberate compromise or failure of precision.').

omega_variable(
    continuity_rupture_definition,
    'What constitutes ''continuity with tradition'' at the doctrinal level? Is reinterpretation of core concepts (e.g., infallibility, mariology, salvation outside the Church) within continuity or a break with it?',
    'Systematic doctrinal comparison: pre-conciliar Magisterium statements on contested topics vs. Vatican II documents vs. post-conciliar development. Philosophical analysis of whether the reinterpretations preserve identity or alter essential content. This is partly empirical (what was taught before/after) and partly conceptual (what counts as continuity in doctrine).',
    'If the reinterpretations preserve core meaning: Vatican II is continuous development and traditionalists mistake evolution for rupture. If core meaning is altered: Vatican II is a rupture and traditionalists are right that it represents doctrinal change. This is the fundamental interpretive divide.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(continuity_rupture_definition, conceptual, 'The definition of doctrinal continuity versus rupture in theological frameworks.').

omega_variable(
    implementation_fidelity,
    'How much of the post-conciliar divergence from pre-conciliar practice flows from Vatican II''s actual texts versus from interpretive overreach by reformers acting in the Council''s name without textual warrant?',
    'Detailed textual analysis of Vatican II documents against actual reforms implemented (liturgy, ecclesiology, ecumenism, religious liberty). Testimony from reformist bishops and theologians about what the Council ''meant'' vs. what it explicitly stated. Identification of specific reforms with explicit vs. disputed textual grounding.',
    'If most reforms have textual warrant: Vatican II itself is more radical than traditionalists assert, and the Council bears responsibility for the rupture. If many reforms lack warrant: traditionalists are right that the Council has been betrayed, and blame should focus on post-conciliar interpreters rather than the Council itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(implementation_fidelity, empirical, 'The degree of fidelity between Vatican II''s explicit texts and post-conciliar implementation.').

omega_variable(
    identity_locked_exit_feasibility,
    'For traditionalist bishops and priests, is the identity-lock to pre-conciliar doctrine structurally imposed or is it psychologically sustained? Would removal of institutional suppression change their commitment, or does their vocation identity make exit unthinkable regardless?',
    'Post-exit trajectory analysis: traditionalists who did leave the institutional Church or accepted the post-conciliar settlement, examining whether they maintained traditional conviction or shifted. Testimony from traditionalists under various institutional conditions (tolerated vs. suppressed). Institutional experiments removing suppression and observing whether traditionalist commitment persists.',
    'If identity-locked, the constraint''s suppression component is partially internalized and would persist even if institutional enforcement were removed — the constraint extracts from something beyond coercive reach. If psychologically sustained but not identity-locked, removing enforcement might shift positions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_exit_feasibility, empirical, 'Whether traditionalist doctrinal commitment is identity-fused or circumstantially maintained.').

omega_variable(
    kernel_reading_committer_frame,
    'This constraint instantiates the rupture_traditionalist_reading of the vatican_ii_doctrinal_authority kernel. Are the other readings (continuity, rupture_progressive, composite_overdetermination) genuinely incommensurable, or do they converge on shared empirical facts while disagreeing on valence and interpretation?',
    'Cross-reading comparison: identify propositions each reading asserts and their logical relationships. Test whether readings are coexistent positions (different parties holding different normative frames) or whether they make falsifiable empirical claims that conflict (only one can be true). Map the disagreement onto the ambiguity_intentionality and continuity_rupture_definition omegas.',
    'If readings are incommensurable: each is a valid framework and the kernel dispute is conceptual/preference-level. If they share empirical ground: resolving the empirical questions (ambiguity intentionality, implementation fidelity) would adjudicate between readings and potentially foreclose some. This affects whether the engine should compute coexists_with or influences relationships in reading_relations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_committer_frame, conceptual, 'Whether Vatican II kernel readings are incommensurable frameworks or empirically resolvable disagreements.').

omega_variable(
    suppression_mechanism_structural_internalized,
    'Is the measured suppression of traditional liturgy and doctrine structurally enforced (institutional prohibition, resource denial, legal barriers) or internalized (traditionalists have absorbed the authority of reform and treat their own practice as illicit without external coercion)?',
    'Post-removal suppression trajectory: in jurisdictions or time periods where institutional prohibition was relaxed (e.g., Benedict XVI''s Summorum Pontificum), did suppression persist at the same level or did it decline? Did traditionalists experience the relaxation as permission or as insufficient? Do traditionalists continue to treat their own practice as illegitimate even when not formally suppressed?',
    'If suppression is structural: removing institutional barriers should liberate traditionalist practice. If internalized: traditionalists carry the suppression internally and removing barriers does not restore their authority; the constraint''s effective suppression remains high. This affects both the measured suppression value and the classification''s stability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_internalized, empirical, 'Whether suppression of traditionalist liturgy and doctrine is structural or internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t0, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(vati_tr_t5, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(vati_tr_t10, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 10, 0.39).
narrative_ontology:measurement(vati_tr_t15, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 15, 0.48).
narrative_ontology:measurement(vati_tr_t20, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 20, 0.54).
narrative_ontology:measurement(vati_tr_t25, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 25, 0.58).

% Extraction over time
narrative_ontology:measurement(vati_be_t0, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(vati_be_t5, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(vati_be_t10, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 10, 0.59).
narrative_ontology:measurement(vati_be_t15, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(vati_be_t20, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(vati_be_t25, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t0, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(vati_su_t5, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 5, 0.51).
narrative_ontology:measurement(vati_su_t10, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(vati_su_t15, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(vati_su_t20, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(vati_su_t25, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority__rupture_progressive_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% Vatican II doctrinal authority is a contested kernel with four structurally distinct readings. Each reading instantiates a different constraint with different ε values and victim/beneficiary sets. This file presents the rupture_traditionalist reading, which reads Vatican II as containing ambiguities and errors enabling heterodox implementation and as rupture (not continuity) with prior doctrine. The ε is high (0.68) because the reading asserts substantial doctrinal change; the valence is negative (change is read as loss/extraction rather than beneficial development). The continuity_reading would author similar doctrinal changes but value them as organic development; the rupture_progressive_reading would agree on rupture but celebrate it as liberating; the composite_overdetermination_reading would decompose the 'one Council' into multiple distinct structural shifts. All four readings coexist as live institutional positions; none logically forecloses the others, though each creates structural pressure on the others through interpretive authority and resource control.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
