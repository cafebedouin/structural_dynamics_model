% ============================================================================
% CONSTRAINT STORY: reformation_composite__theological_fragmentation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_composite__theological_fragmentation_reading, []).

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
 *   constraint_id: reformation_composite__theological_fragmentation_reading
 *   human_readable: Reformation: Theological Fragmentation Reading
 *   domain: historical_epistemology/religious_history/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates one reading of the Reformation kernel:
 *   the claim that the Reformation is fundamentally a theological event
 *   driven by competing soteriological and ecclesiological commitments that
 *   generate structurally incompatible denominations. Under this reading, the
 *   primary observable is doctrinal pluralism — the irreducible disagreement
 *   between Reformed and Catholic Christians on the nature of salvation, the
 *   role of works, the meaning of the Eucharist, and the structure of
 *   ecclesiastical authority. Confessional documents (Augsburg Confession
 *   1530, Heidelberg Catechism 1563, Formula of Concord 1577) serve as
 *   constraint artifacts: they codify the incompatibilities and make them
 *   institutional. Denominational leadership — reformed bishops, synodal
 *   assemblies, confessional hierarchies — emerges as the beneficiary of this
 *   fragmentation, gaining authority through doctrinal boundary-setting. This
 *   reading is distinct from the political_realignment_reading (which centers
 *   state sovereignty against papal/imperial authority) and the
 *   technological_mediation_reading (which centers printing press
 *   dissemination). All three readings refer to the same historical event but
 *   foreground different causal mechanisms and identify different structural
 *   winners. This reading generates a tangled_rope classification: genuine
 *   theological coordination (coherent doctrine within a church) coexists
 *   with asymmetric extraction (doctrinal conformity demanded from laity and
 *   lower clergy) and institutional enforcement (heresy trials,
 *   excommunication, doctrinal surveillance).
 *
 * KEY AGENTS:
 *   - Reformed Denominational Leadership (institutional/arbitrage): Primary beneficiary — gains institutional authority and territorial sovereignty through doctrinal boundary-enforcement against Rome and other Reformed competitors
 *   - Catholic Ecclesiastical Hierarchy (institutional/constrained): Primary victim AND secondary beneficiary — loses institutional monopoly on Christian doctrine but retains substantial territorial and temporal power through Counter-Reformation confessionalization
 *   - Doctrine-Bound Parishioner (powerless/trapped): Primary victim — faces doctrinal identity-lock; cannot exit without severing kinship, community, and spiritual identity
 *   - Competing Clergy (moderate/constrained): Mixed victim-beneficiary — gains pastoral authority within their own confession but faces severe suppression if they attempt doctrinal innovation or crossing confessional boundaries
 *   - Theological Specialists (institutional/arbitrage): Secondary beneficiary — confessional fragmentation creates demand for trained theologians, catechists, and exegetes; institutional investment in theological education increases
 *   - Unified Ecclesiastical Authority (institutional/trapped): Structural victim — the reading's observable (doctrinal pluralism) directly eliminates the possibility of a single coordinating church authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__theological_fragmentation_reading, 0.52).
domain_priors:suppression_score(reformation_composite__theological_fragmentation_reading, 0.58).
domain_priors:theater_ratio(reformation_composite__theological_fragmentation_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__theological_fragmentation_reading, tangled_rope).
narrative_ontology:human_readable(reformation_composite__theological_fragmentation_reading, "Reformation: Theological Fragmentation Reading").
narrative_ontology:topic_domain(reformation_composite__theological_fragmentation_reading, "historical_epistemology/religious_history/political_economy").

domain_priors:requires_active_enforcement(reformation_composite__theological_fragmentation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__theological_fragmentation_reading, 'dc357f32-e489-4ac7-a0ce-8b2eb4d35c31').
narrative_ontology:cs_kernel_codification('dc357f32-e489-4ac7-a0ce-8b2eb4d35c31', fixed_text).
narrative_ontology:cs_authority_grounding('dc357f32-e489-4ac7-a0ce-8b2eb4d35c31', lineage).
narrative_ontology:cs_interpretation_layer_present('dc357f32-e489-4ac7-a0ce-8b2eb4d35c31').
narrative_ontology:cs_reading_relation('dc357f32-e489-4ac7-a0ce-8b2eb4d35c31', reformation_composite__political_realignment_reading, coexists_with).
narrative_ontology:cs_reading_relation('dc357f32-e489-4ac7-a0ce-8b2eb4d35c31', reformation_composite__technological_mediation_reading, influences).
narrative_ontology:cs_axiom('dc357f32-e489-4ac7-a0ce-8b2eb4d35c31', foundational, sola_scriptura_as_theological_foundation).
narrative_ontology:cs_axiom_status(sola_scriptura_as_theological_foundation, holdable).
narrative_ontology:cs_axiom_grounding('dc357f32-e489-4ac7-a0ce-8b2eb4d35c31', sola_scriptura_as_theological_foundation, deontological).
narrative_ontology:cs_axiom('dc357f32-e489-4ac7-a0ce-8b2eb4d35c31', foundational, doctrinal_identity_constitutive_of_church_membership).
narrative_ontology:cs_axiom_status(doctrinal_identity_constitutive_of_church_membership, holdable).
narrative_ontology:cs_axiom_grounding('dc357f32-e489-4ac7-a0ce-8b2eb4d35c31', doctrinal_identity_constitutive_of_church_membership, conventional).
narrative_ontology:cs_reference_frame('dc357f32-e489-4ac7-a0ce-8b2eb4d35c31', unified_christendom_doctrine_mediated_by_magisterium).
narrative_ontology:cs_drift_state('dc357f32-e489-4ac7-a0ce-8b2eb4d35c31', post_confessional_fragmentation_era, gap(codification_collapse, substantial, true)).
narrative_ontology:cs_created_at('dc357f32-e489-4ac7-a0ce-8b2eb4d35c31', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(reformation_composite__theological_fragmentation_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, reformed_denominational_leadership).
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, theological_specialists).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, unified_ecclesiastical_authority).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, doctrinal_consensus_episteme).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DOCTRINE-BOUND PARISHIONER (SNARE) — The theological fragmentation constraint operates as pure extraction for those who inhabit doctrinally incompatible parishes. A parishioner cannot simultaneously hold Reformed soteriology and Catholic sacramental mediation; cannot exit without abandoning identity (family, community, faith cohort); suppression is maximal through spiritual authority claims ('true faith'). The constraint extracts conformity through doctrinal identity-locking. No coordination function visible from this perspective — only the cost of choosing between fidelity to doctrine and fidelity to kin.
constraint_indexing:constraint_classification(reformation_composite__theological_fragmentation_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: COMPETING CLERGY MEMBER (TANGLED ROPE) — A pastor in a fractured Christendom faces mixed extraction and coordination. The theological constraint coordinates their preaching community (shared doctrine, liturgical coherence) and legitimizes their pastoral authority against Rome; but it also extracts conformity from them, binds them to doctrinal positions that may shift, and suppresses alternative theologies through ex cathedra claims. They can exit (become Catholic, become secular) but face severe cost (defrocking, exile, apostasy stigma). Genuine coordination function (doctrinal coherence within the parish) coexists with asymmetric extraction (clergy must perform doctrinal authority).
constraint_indexing:constraint_classification(reformation_composite__theological_fragmentation_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: REFORMED DENOMINATIONAL LEADERSHIP (ROPE) — Lutheran, Reformed, and other emerging denominational hierarchies experience the theological fragmentation as a coordination mechanism. Confessional documents (Augsburg Confession, Heidelberg Catechism) coordinate their churches' doctrinal identity, legitimize their authority against Rome and each other, and enable expansion into new territories. Leadership has exit options (capitulation to Rome, absorption into secular governance, dissolution) but chooses to enforce theological boundaries that are simultaneously coordination tools and institutional assets. Net beneficiary — extraction flows toward this group through doctrinal authority claims.
constraint_indexing:constraint_classification(reformation_composite__theological_fragmentation_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: THEOLOGICAL NECESSITY / NATURAL LAW (MOUNTAIN) — From the civilizational analytical frame, some theological diversification appears inevitable: once sola scriptura is endorsed, different readers will interpret Scripture differently; once private judgment is legitimized, soteriological and ecclesiological commitments will diverge across communities. This perspective risks naturalizing what is actually a contingent institutional choice (the legitimacy of theological pluralism in a supposedly universal church). The engine's false summit detector will flag this as a naturalization of a constructed constraint.
constraint_indexing:constraint_classification(reformation_composite__theological_fragmentation_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: ECUMENICAL RECONCILIATION PROTOCOLS (PITON) — Modern ecumenical movements (Joint Declaration on the Doctrine of Justification, World Council of Churches) treat the theological fragmentation as a partially degraded constraint. Contemporary clerical leadership sees the denominational divisions as historically contingent and potentially reconcilable through doctrinal reinterpretation and hermeneutical charity. The constraint persists through institutional inertia (separate hierarchies, accumulated doctrinal investments, congregational identity fusion) rather than genuine theological incompatibility. Theater ratio is high — ecumenical dialogue performs unity while jurisdictional fragmentation remains intact.
constraint_indexing:constraint_classification(reformation_composite__theological_fragmentation_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_composite__theological_fragmentation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(reformation_composite__theological_fragmentation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(reformation_composite__theological_fragmentation_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(reformation_composite__theological_fragmentation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(reformation_composite__theological_fragmentation_reading, TR),
    TR >= 0.70.

:- end_tests(reformation_composite__theological_fragmentation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The theological fragmentation constraint extracts conformity through doctrinal identity-fusion and institutional enforcement (heresy trials, excommunication). However, extractiveness is not maximal because genuine coordination functions exist: Reformed churches do coordinate liturgy, doctrine, and community life through confessional documents. The extraction is embedded in coordination, not pure. The measurement trajectory (0.38 → 0.52 over 80 years) reflects that early Reformation allowed more theological fluidity; as confessional documents were formalized and denominational hierarchies solidified, extraction mechanisms intensified. Suppression (0.58): Moderate-high. The constraint suppresses alternative theologies through: (a) doctrinal authority claims ('true faith' framed as divinely revealed, not contingent); (b) institutional enforcement (heresy trials, excommunication, loss of livelihood); (c) identity-fusion (theological position becomes inseparable from personal and communal identity). However, suppression is not total — some theological diversity persists within confessions, heretics sometimes escaped, and some individuals moved between confessions. The trajectory (0.45 → 0.58) shows suppression intensifying as denominational structures consolidated. Theater ratio (0.48): Moderate. Theological fragmentation is not primarily performative — real doctrinal disagreements exist and are genuinely believed by participants. However, some performative elements exist: doctrinal disputes sometimes serve as proxies for power struggles; confessional rituals (signing confessions, reciting creeds) perform unity more than they generate it; later ecumenical movements reveal that some denominational boundaries are institutionally maintained despite doctrinal convergence. The measurement trajectory (0.35 → 0.48) reflects increasing institutional theater as confessions became established authorities.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival divergence is fundamentally a gap between beneficiary and victim positions within a constraint that exhibits genuine coordination function (denominations do coherently coordinate doctrine and practice) alongside genuine extraction (conformity is demanded and enforced). The analytical observer risks collapsing this into a mountain (inevitable consequence of sola scriptura) but the beneficiary structure reveals it as constructed: if sola scriptura and private judgment necessarily produced denominational fragmentation, there would be no identifiable beneficiaries — the fragmentation would be natural law, not contingent extraction. But Reformed leadership clearly benefits from institutional authority over doctrine, theological specialists benefit from confessional demand for trained interpreters, and this benefits-structure proves the fragmentation is maintained by institutional choice, not doctrinal necessity. The false summit detection signature fires: mountain classification meets beneficiary declaration, revealing naturalization of construction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from beneficiary/victim status and exit options. Reformed denominational leadership: beneficiary + arbitrage exit (can capitulate to Rome or secularize) → d ≈ 0.10 → low effective extraction toward this group. Doctrine-bound parishioner: victim + trapped exit (cannot exit without severing identity and kinship) → d ≈ 0.95 → high effective extraction from this group. Competing clergy: mixed victim-beneficiary status + constrained exit (can defect but faces severe cost) → d ≈ 0.65 → moderate extraction. The sigmoid function f(d) amplifies this differentiation: f(0.10) ≈ -0.01 (beneficiary experiences negative chi), f(0.95) ≈ 1.42 (trapped victim experiences maximum chi). Scope modifier σ(S) scales continental-level theological fragmentation upward: σ(continental) ≈ 1.1. This drives the measured chi values: institutional beneficiary experiences χ ≈ 0.52 × (-0.01) × 1.1 ≈ negative (net benefit), while powerless trapped parishioner experiences χ ≈ 0.52 × 1.42 × 1.1 ≈ 0.81 (severe extraction). Identity-locked exit option applies to the doctrine-bound parishioner: they are structurally mobile (could physically relocate, legally convert) but identity-locked to their confession (cannot psychologically/socially exit without becoming a different person). This produces the distinctive rope classification at biographical horizon for identity_locked agents (per the immutability table) — the parishioner perceives the constraint as potentially changeable in principle, but only at the cost of identity dissolution.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrinal_incompatibility_scope,
    'Are the soteriological and ecclesiological differences between denominations genuine logical incompatibilities or merely different emphases compatible within a single theological frame?',
    'Formal logical analysis of confessional documents; identification of shared axioms vs. fundamental contradictions; test whether a coherent synthesis framework can accommodate all positions simultaneously without self-contradiction',
    'If genuine incompatibilities (e.g., transubstantiation vs. symbolic presence): theological constraint operates as claimed, high suppression justified. If emphasis differences: constraint is weaker than claimed, suppression reflects institutional choice rather than doctrinal necessity, theater_ratio rises.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(doctrinal_incompatibility_scope, conceptual, 'Whether denominational theologies are logically incompatible or merely different emphases').

omega_variable(
    theological_pluralism_inevitability,
    'Does endorsing sola scriptura and private judgment inevitably produce denominational fragmentation, or is fragmentation a contingent institutional choice enabled but not necessitated by those principles?',
    'Historical counterfactual: did the Reformed churches require institutional hierarchy and boundary enforcement to maintain coherence? Or could plural theologies have coexisted within a single institutional structure (as occurs in some Anglican and Presbyterian global communions)? Identify which elements are doctrinal and which are institutional choice.',
    'If inevitable: theological constraint is closer to mountain (structural property of reformed theology). If contingent: the constraint is a tangled_rope maintained by institutional choice, not doctrinal necessity; authority grounding shifts from lineage (doctrine as authority) to extraction (institutional preservation through doctrinal boundary-setting).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_pluralism_inevitability, conceptual, 'Whether theological pluralism is inevitable consequence of sola scriptura or contingent institutional choice').

omega_variable(
    reformation_reading_contest,
    'Which reading of the Reformation kernel is structurally primary: theological differentiation, political sovereignty assertion, or printing press technological mediation? Can these readings coexist as equally valid descriptions of the same event, or does one reading foreclose the others?',
    'Chronological and causal analysis: which dynamics appear first in the historical record? Which dynamics explain variance in reformation success across regions? Does theological logic appear before or after political assertion of sovereignty? Does printing press adoption precede or follow doctrinal differentiation?',
    'If readings are causally nested (one causes another): one reading forecloses the others (primary cause eliminates alternatives). If readings describe independent processes: readings coexist_with each other. If one reading creates structural pressure on others without eliminating them: influences relation applies. Terminal classification of the reformation_composite kernel depends on resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reformation_reading_contest, empirical, 'Which reading of the Reformation kernel is primary; whether readings foreclose or coexist').

omega_variable(
    confessional_document_function,
    'Do confessional documents (Augsburg Confession, Heidelberg Catechism, etc.) primarily coordinate doctrine within a church community, or do they primarily extract conformity and suppress alternative theologies?',
    'Analysis of confessional document usage in sermons, catechesis, dispute resolution, and excommunication proceedings. Measure the ratio of positive coordination (shared liturgy, coherent preaching) to negative enforcement (heresy trials, membership policing). Historical comparison: same theological positions with and without formal confessional documents.',
    'If coordination-primary: constraint is closer to rope. If enforcement-primary: constraint is closer to snare. The theater_ratio interpretation hinges on this: performative enforcement vs. functional coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(confessional_document_function, empirical, 'Whether confessional documents primarily coordinate or enforce conformity').

omega_variable(
    sola_scriptura_hermeneutical_commons,
    'Does sola scriptura enable a shared hermeneutical commons for interpreting Scripture, or does it inherently fragment interpretation by legitimizing private judgment?',
    'Analysis of Reformed hermeneutical practice: did early Reformed churches establish shared interpretive methods and exegetical standards? Did denominational variation in biblical interpretation arise before or after institutional separation?',
    'If shared commons: constraint is weaker (coordination function dominates). If inherent fragmentation: constraint is stronger (doctrinal divergence is built into the theological framework). Feeds back to omega_theological_pluralism_inevitability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sola_scriptura_hermeneutical_commons, empirical, 'Whether sola scriptura enables shared hermeneutical commons or inherent fragmentation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__theological_fragmentation_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ref_theo_tr_t0, reformation_composite__theological_fragmentation_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ref_theo_tr_t40, reformation_composite__theological_fragmentation_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(ref_theo_tr_t80, reformation_composite__theological_fragmentation_reading, theater_ratio, 80, 0.48).

% Extraction over time
narrative_ontology:measurement(ref_theo_be_t0, reformation_composite__theological_fragmentation_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ref_theo_be_t40, reformation_composite__theological_fragmentation_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement(ref_theo_be_t80, reformation_composite__theological_fragmentation_reading, base_extractiveness, 80, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(ref_theo_su_t0, reformation_composite__theological_fragmentation_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(ref_theo_su_t40, reformation_composite__theological_fragmentation_reading, suppression_requirement, 40, 0.54).
narrative_ontology:measurement(ref_theo_su_t80, reformation_composite__theological_fragmentation_reading, suppression_requirement, 80, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_composite__theological_fragmentation_reading, identity_coordination).
narrative_ontology:affects_constraint(reformation_composite__theological_fragmentation_reading, reformation_composite__political_realignment_reading).
narrative_ontology:affects_constraint(reformation_composite__theological_fragmentation_reading, reformation_composite__technological_mediation_reading).

% DUAL FORMULATION NOTE:
% The Reformation kernel decomposes into three structurally distinct readings, each with different observables and different ε values. This reading (theological_fragmentation, ε=0.52, Tangled Rope) emphasizes doctrinal incompatibility and denominational boundary-enforcement. The political reading would center sovereignty assertions and territorial consolidation (different observable, likely higher ε, closer to Snare). The technological reading would center printing press adoption and dissemination speed (different observable, likely different ε trajectory). All three stories are linked via network.affects_constraints to indicate they are sibling readings of a single kernel, not independent constraints. The engine's constraint family analysis will recognize them as alternate formulations of the same historical event.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reformation_composite__theological_fragmentation_reading, institutional, 0.08).
constraint_indexing:directionality_override(reformation_composite__theological_fragmentation_reading, powerless, 0.93).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
