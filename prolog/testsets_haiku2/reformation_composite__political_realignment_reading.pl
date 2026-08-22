% ============================================================================
% CONSTRAINT STORY: reformation_composite__political_realignment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_composite__political_realignment_reading, []).

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
 *   constraint_id: reformation_composite__political_realignment_reading
 *   human_readable: Reformation as Political Realignment: Territorial Sovereignty via Religious Differentiation
 *   domain: political_economy/religious_history/state_formation
 *
 * SUMMARY:
 *   This constraint story instantiates the POLITICAL REALIGNMENT reading of
 *   the Reformation kernel. The constraint is the mechanism by which emerging
 *   territorial nation-states assert exclusive sovereignty by adopting or
 *   rejecting reformed theology as a jurisdictional tool. The observable is
 *   cuius regio eius religio — the principle that a territory's ruler
 *   determines its official religion — which consolidates the reading:
 *   religious differentiation becomes the mechanism for political
 *   differentiation. The beneficiaries are territorial rulers and nascent
 *   nation-states; the victims are papal authority and imperial jurisdiction.
 *   The constraint operates as enforced extraction: rulers break papal
 *   revenue flows (indulgences, tithes, dispensations), seize church
 *   property, and establish themselves as ultimate religious authority within
 *   their borders. This reading does not deny that theological disagreement
 *   exists; it asserts that the Reformation's historical _force_ derives from
 *   its political utility as a sovereignty lever, not from the truth or
 *   appeal of reformed theology per se. The constraint's persistence depends
 *   on active enforcement: rulers must expel papal legates, replace bishops
 *   with state clergy, suppress Catholic institutional presence, and suppress
 *   dissent (both from papal authorities and from peasant movements that
 *   sought different religious outcomes).
 *
 * KEY AGENTS:
 *   - Territorial rulers (agenda-setters): Luther, Calvin, Henry VIII — leaders who sponsored reformed theology to break imperial/papal jurisdiction and consolidate state power
 *   - Reformed theologians and clergy: the intellectual and institutional apparatus that articulates the theological grounds for the break; dependent on territorial rulers' protection
 *   - Papal authority and imperial apparatus (victims): facing loss of jurisdictional control, revenue streams, and doctrinal monopoly in reformed territories
 *   - Peasantry and urban commons (excluded): bear enforced religious conformity and fiscal extraction (redirection of tithes) without participating in the choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__political_realignment_reading, 0.68).
domain_priors:suppression_score(reformation_composite__political_realignment_reading, 0.71).
domain_priors:theater_ratio(reformation_composite__political_realignment_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__political_realignment_reading, tangled_rope).
narrative_ontology:human_readable(reformation_composite__political_realignment_reading, "Reformation as Political Realignment: Territorial Sovereignty via Religious Differentiation").
narrative_ontology:topic_domain(reformation_composite__political_realignment_reading, "political_economy/religious_history/state_formation").

domain_priors:requires_active_enforcement(reformation_composite__political_realignment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__political_realignment_reading, 'fe0de558-2eaf-4452-9495-cb194f3a571a').
narrative_ontology:cs_kernel_codification('fe0de558-2eaf-4452-9495-cb194f3a571a', fixed_text).
narrative_ontology:cs_authority_grounding('fe0de558-2eaf-4452-9495-cb194f3a571a', extraction).
narrative_ontology:cs_interpretation_layer_present('fe0de558-2eaf-4452-9495-cb194f3a571a').
narrative_ontology:cs_reading_relation('fe0de558-2eaf-4452-9495-cb194f3a571a', reformation_composite__theological_fragmentation_reading, coexists_with).
narrative_ontology:cs_reading_relation('fe0de558-2eaf-4452-9495-cb194f3a571a', reformation_composite__technological_mediation_reading, influences).
narrative_ontology:cs_axiom('fe0de558-2eaf-4452-9495-cb194f3a571a', foundational, sovereignty_through_religious_differentiation).
narrative_ontology:cs_axiom_status(sovereignty_through_religious_differentiation, holdable).
narrative_ontology:cs_axiom_grounding('fe0de558-2eaf-4452-9495-cb194f3a571a', sovereignty_through_religious_differentiation, instrumental).
narrative_ontology:cs_axiom('fe0de558-2eaf-4452-9495-cb194f3a571a', foundational, territorial_jurisdiction_over_doctrine).
narrative_ontology:cs_axiom_status(territorial_jurisdiction_over_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('fe0de558-2eaf-4452-9495-cb194f3a571a', territorial_jurisdiction_over_doctrine, conventional).
narrative_ontology:cs_reference_frame('fe0de558-2eaf-4452-9495-cb194f3a571a', imperial_papal_universal_authority).
narrative_ontology:cs_drift_state('fe0de558-2eaf-4452-9495-cb194f3a571a', post_peace_of_westphalia, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('fe0de558-2eaf-4452-9495-cb194f3a571a', '').
narrative_ontology:cs_kernel_id(reformation_composite__political_realignment_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__political_realignment_reading, territorial_rulers).
narrative_ontology:constraint_beneficiary(reformation_composite__political_realignment_reading, emerging_nation_states).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, papal_authority).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, holy_roman_empire_imperial_apparatus).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reformation_composite__political_realignment_reading, reformed_theologians_and_clergy).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, catholic_institutional_hierarchy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seize control of religious doctrine, property, and institutional loyalty within their territories by sponsoring or protecting reformed theology. They break papal revenue streams (indulgences, dispensations), redirect church lands and tithes to state coffers, and establish themselves as the ultimate religious authority within their borders. The constraint of papal jurisdiction over doctrine and property is precisely what they move to overturn through religious differentiation.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, territorial_rulers, agenda_setter,
    institutional, generational, mobile, national).

% Gain ideological and material independence from empire and papacy. Religious reformation operates as the legitimacy apparatus for national consolidation: 'we are Swedish because we are Lutheran'; 'we are English because we rejected papal supremacy.' The constraint enables state-building by providing theological grounds for rejecting external authority.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, emerging_nation_states, beneficiary,
    organized, generational, mobile, national).

% Loses territorial jurisdiction, revenue streams (tithes, indulgences, dispensations), and doctrinal control in territories that adopt reformed theology. The constraint operates as enforced jurisdictional collapse: papal legates are expelled, bishops are replaced with state-appointed or reformed clergy, and the pope's claim to universal doctrinal authority is explicitly rejected by territorial legislation.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, papal_authority, payer,
    institutional, generational, constrained, continental).

% Loses its claim to jurisdictional universality based on religious unity ('one faith, one empire'). The fragmentation of the empire into reformed and Catholic territories (formalized at Peace of Augsburg, 1555) reflects the constraint's enforcement: the empire can no longer claim religious coherence as grounds for centralized authority, and its power becomes diffused into territorial constituents.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, holy_roman_empire_imperial_apparatus, payer,
    institutional, generational, constrained, continental).

% Gain institutional protection, property holdings, and doctrinal authority within territories that adopt their theology. They coordinate the ideological machinery that justifies state sovereignty; in return, they become state-sponsored clergy dependent on territorial rulers' continued protection. Their role is dual: they are both beneficiaries of the constraint (it protects them from papal prosecution and provides state resources) and agenda-setters (they articulate the theological grounds that legitimize the political break).
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, reformed_theologians_and_clergy, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(reformation_composite__political_realignment_reading, reformed_theologians_and_clergy, agenda_setter).

% Loses institutional presence, property holdings, and doctrinal monopoly in reformed territories. The constraint operates as organizational fragmentation: Catholic bishops are removed, monasteries are secularized, and the Catholic hierarchy's claim to universal spiritual authority is territorially circumscribed. They bear costs they cannot negotiate away (they cannot compete with state-backed reformed theology in reformed territories).
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, catholic_institutional_hierarchy, payer,
    institutional, generational, trapped, continental).

% Are excluded from the decision-making apparatus that determines which theology their territory will adopt. Religious affiliation is decided by territorial rulers; peasants and urban commons bear the constraint (enforced religious conformity, redirection of tithes) without participating in the choice. Their exclusion is structural: the constraint is fundamentally a ruler-to-ruler negotiation, not a mass conversion movement.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, peasantry_and_urban_commons, excluded,
    powerless, biographical, trapped, local).

% Watch and participate in the constraint's deployment as a strategic tool for asserting sovereignty. They see that religious differentiation works as a jurisdictional lever and adopt the strategy (or resist it) based on their own territorial interests. The constraint's success creates imitation and competition: rulers race to establish independent religious authority as a marker of sovereignty.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, competing_territorial_rulers, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_composite__political_realignment_reading, territorial_rulers).
narrative_ontology:fixing_cost_class(reformation_composite__political_realignment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides territorial rulers with a coordinated theological and institutional apparatus for breaking imperial/papal jurisdiction and establishing exclusive internal religious authority. The constraint solves the coordination problem of how multiple rulers can simultaneously reject a universal religious authority without fragmenting into chaos — they do so by adopting territorially-coherent reformed theology and enforcing doctrinal uniformity within their borders.
% TRANSFER_FUNCTION: Moves church property, tithes, dispensation revenues, and doctrinal authority from the papal/imperial apparatus to territorial rulers and state-backed clergy. The transfer is bidirectional: rulers gain material resources and legitimacy; reformed clergy gain protection and property; papacy and empire lose jurisdictional control and revenue streams.
% ABSENT_VOICES: Peasantry, urban commons, and the mass of believers who did not participate in the decision to adopt or reject reformed theology. They are structurally excluded from the agenda-setting layer where territorial rulers negotiate religious affiliation. A peasant revolt (German Peasants' War, 1524–1526) objected to the constraint's operation, but was suppressed by the very rulers who benefited from the religious break — showing that peasant consent was never required.
% DISAPPEARANCE_RATIONALE: If the constraint — the political use of religious differentiation to assert territorial sovereignty — disappeared overnight, the imperial and papal authority structures would reassert jurisdiction over their former territories; the ideological grounds for national consolidation would collapse; and the territorial fragmentation of Europe that grounds modern nation-states would be jeopardized. The world depends on this constraint's persistence.
% FOUNDING_PROBLEM: The universal authority claims of papacy and empire created a legitimacy crisis for emerging territorial rulers: how can a ruler claim exclusive authority within a territory when an external religious institution claims universal jurisdiction? Religious differentiation solves this by grounding sovereignty in local doctrinal choice rather than in submission to universal authority.
% FOUNDING_PROBLEM_CORROBORATION: Territorial rulers and reformed theologians attest the founding problem — that papal/imperial overreach threatened their sovereignty — and cite it as grounds for the break. Modern historians and political scientists (outside the benefiting parties) attest that the Reformation operated as a sovereignty assertion tool: it provided ideological cover for what was fundamentally a jurisdictional grab. However, theological historians attest that the founding problem was genuinely theological (corruption in the Church, soteriological dispute); they contest that politics was primary.
narrative_ontology:disappearance_verdict(reformation_composite__political_realignment_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_composite__political_realignment_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__political_realignment_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reformation_composite__political_realignment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_composite__political_realignment_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_composite__political_realignment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reformation_composite__political_realignment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reformation_composite__political_realignment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and rising (0.35 → 0.68 over 131 years) because the constraint progressively transfers control of religious institutions and their revenues from universal authorities (papacy, empire) to territorial rulers. The suppression requirement is also high (0.42 → 0.71) because the constraint's persistence depends on actively expelling papal authorities, replacing Catholic clergy, secularizing monastic property, and enforcing doctrinal conformity at the local level — all requiring coercive state apparatus. Theater ratio is lower (0.08 → 0.28) because the constraint's function is genuinely extractive (resource transfer) and the coordination function it performs (enabling multiple rulers to break universal authority simultaneously) is real but secondary to the extraction. Resistance is high (0.73) because papal and imperial authorities actively resist the constraint's enforcement — the Reformation is contested, not a smooth transition. The measurement grid uses a single shared time grid (start=1517, end=1648) with all three metrics authored at all six time points, tracing the constraint's maturation from Luther's 95 Theses through the Peace of Westphalia (1648), which formally recognizes the territorial fragmentation the constraint enabled.
 *
 * PERSPECTIVAL GAP:
 *   The territorial-ruler seat and the papal-victim seat should compute radically different classifications. From the ruler's seat: the constraint coordinates a solution to the problem of how to assert sovereignty against universal authority; reformed theology provides the ideological apparatus; the transaction is voluntary among rulers (they choose to adopt the constraint). From the papal seat: the same structure operates as enforced extraction of jurisdictional control and revenue; the papacy has no choice but to accept the loss of territorial authority in reformed regions. The engine computes this divergence from the structural data: rulers have high exit options (mobile) and are beneficiaries (d near 0.0); the papacy has constrained exit and is a victim (d near 1.0). The authored claim (tangled_rope) reflects this asymmetry: genuine coordination among rulers, asymmetric extraction from papacy/empire.
 *
 * DIRECTIONALITY LOGIC:
 *   Territorial rulers exhibit low directionality (d ≈ 0.1–0.2): they are the beneficiaries, they have mobile exit options (they can choose to adopt or reject reformed theology; adoption is strategic, not coerced), and they control the apparatus that enforces the constraint. Papal authority exhibits high directionality (d ≈ 0.8–0.9): it is a victim, its exit options are constrained (it cannot prevent territorial rulers from breaking papal jurisdiction), and it bears costs it did not choose. Emerging nation-states exhibit beneficiary directionality; reformed clergy exhibit asymmetric beneficiary-agenda-setter positioning (they gain institutional protection and property from rulers, making them dependent beneficiaries). Peasantry are excluded from the directionality computation entirely — they do not participate in the choice; the constraint is imposed on them. The override lever is not needed here because the structural derivation (beneficiary/victim + exit options + power) produces the right d values without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is not mandatrophic at the interval's end (1648). The founding problem (need for territorial rulers to assert sovereignty against universal authority) remains live in 1648; the constraint persists because rulers continue to benefit from it and continue to enforce it. The Peace of Westphalia (1648) does not resolve the founding problem; it institutionalizes it, enshrining the principle that sovereignty is territorial and that religion is within the ruler's domain. The constraint does not become theatrical or inert — its function of enabling sovereignty assertion persists. Mandatrophy might become relevant in later intervals (after 1648) if territorial sovereignty itself becomes less contested and the constraint begins to operate more as a vestigial inheritance than as an active sovereignty lever, but within this interval (1517–1648) the constraint remains functional and enforced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theology_vs_politics_primacy,
    'Is the Reformation primarily a religious event (theological dispute drives political consequence) or a political event (political interest drives theological justification)?',
    'Historical counterfactual: if reformed theology had been equally compelling but no territorial ruler had adopted it, would the Reformation have fragmented Europe? If not, politics was primary; if yes, theology was primary.',
    'If theology was primary, the constraint''s ε should be lower (the extraction is incidental to genuine theological dispute). If politics was primary, ε remains high (the extraction is the point; theology is the cover story).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theology_vs_politics_primacy, conceptual, 'Whether political interest or theological truth grounds the constraint''s persistence.').

omega_variable(
    voluntary_adoption_vs_forced_imposition,
    'Did territorial rulers voluntarily adopt reformed theology as a strategic choice, or did reformed theology impose itself on rulers as an ideological momentum they had to accommodate?',
    'Counter-historical narrative: rulers who initially resisted the reform (Charles V, the Catholic Habsburgs) — did they resist because reform threatened their authority, or because they genuinely preferred Catholic theology? Their subsequent Tridentine reforms suggest political rather than theological preference.',
    'If adoption was voluntary-strategic, the constraint is tangled_rope (coordination among rulers, extraction from victims). If adoption was forced-accommodative, the constraint might be better classified as piton (inertial, maintained by momentum rather than by active benefit to rulers).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_adoption_vs_forced_imposition, empirical, 'Whether rulers controlled the adoption of reformed theology or were swept along by it.').

omega_variable(
    enforcement_machinery_vs_organic_spread,
    'Is the constraint''s persistence due to active state enforcement of reformed theology, or to organic spread of reformed belief among the population?',
    'Regional analysis: in regions where reformed theology spread against ruler preference (southern Germany, Bohemia) versus regions where rulers actively imposed reform (Scandinavia, northern Germany), did the constraint''s persistence differ? Did suppression_requirement measurements correlate with the degree of ruler enthusiasm?',
    'If organic spread drove persistence, suppression would be low and theater_ratio would be high. If enforcement drove persistence, suppression would be high and theater_ratio low, as observed. The measurement series (suppression_requirement rising from 0.42 to 0.71) supports the enforcement reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_machinery_vs_organic_spread, empirical, 'Whether the constraint persists because it is actively enforced or because it is organically popular.').

omega_variable(
    reading_foreclosure_theological_fragmentation,
    'Does the political_realignment reading logically foreclose the theological_fragmentation reading within any single framework, or do both readings coexist as live positions?',
    'Logical test: can a party simultaneously assert that the Reformation was politically motivated AND that it produced genuine theological incompatibility? (Answer: yes — politics and theology are not mutually exclusive causes.) Can a party deny that politics was primary while holding that genuine theological fragmentation occurred? (Answer: yes — theology can be primary even if politics was also consequential.)',
    'The two readings coexist; neither forecloses the other. A historical actor can hold both: that rulers strategically adopted reformed theology AND that the theology is genuinely true and incompatible with Catholicism. Foreclosure would require that asserting political motivation logically entails denying theological validity, which it does not.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_theological_fragmentation, conceptual, 'Whether this reading''s core premise (politics was primary) logically excludes the theological reading''s core premise (theology is substantively important).').

omega_variable(
    reading_relation_to_technological_mediation,
    'Does the political_realignment reading influence the technological_mediation reading, or do they coexist independently?',
    'Causal sequence: the printing press enabled the spread of reformed theology across territories, but did politics determine whether the theology spread or was suppressed? Did rulers who adopted reform do so because the printing press made suppression impossible, or did they strategically allow/sponsor the technology''s use to break papal authority?',
    'If rulers could have easily suppressed printed reforms but chose not to (because suppression was politically disadvantageous), then politics influences technology: the printing press was a tool rulers permitted because it served their sovereignty strategy. If rulers were unable to suppress the printing press and had to accommodate its effects, then technology influences politics. The measurement of rising enforcement (suppression_requirement) suggests rulers increasingly worked to control the technology''s effects, implying politics was attempting to harness rather than be swept along.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_relation_to_technological_mediation, empirical, 'Whether political strategy influenced the deployment of printing technology or was determined by it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__political_realignment_reading, 1517, 1648).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1517, reformation_composite__political_realignment_reading, theater_ratio, 1517, 0.08).
narrative_ontology:measurement(refo_tr_t1530, reformation_composite__political_realignment_reading, theater_ratio, 1530, 0.14).
narrative_ontology:measurement(refo_tr_t1555, reformation_composite__political_realignment_reading, theater_ratio, 1555, 0.22).
narrative_ontology:measurement(refo_tr_t1600, reformation_composite__political_realignment_reading, theater_ratio, 1600, 0.27).
narrative_ontology:measurement(refo_tr_t1630, reformation_composite__political_realignment_reading, theater_ratio, 1630, 0.29).
narrative_ontology:measurement(refo_tr_t1648, reformation_composite__political_realignment_reading, theater_ratio, 1648, 0.28).

% Extraction over time
narrative_ontology:measurement(refo_be_t1517, reformation_composite__political_realignment_reading, base_extractiveness, 1517, 0.35).
narrative_ontology:measurement(refo_be_t1530, reformation_composite__political_realignment_reading, base_extractiveness, 1530, 0.48).
narrative_ontology:measurement(refo_be_t1555, reformation_composite__political_realignment_reading, base_extractiveness, 1555, 0.62).
narrative_ontology:measurement(refo_be_t1600, reformation_composite__political_realignment_reading, base_extractiveness, 1600, 0.67).
narrative_ontology:measurement(refo_be_t1630, reformation_composite__political_realignment_reading, base_extractiveness, 1630, 0.68).
narrative_ontology:measurement(refo_be_t1648, reformation_composite__political_realignment_reading, base_extractiveness, 1648, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1517, reformation_composite__political_realignment_reading, suppression_requirement, 1517, 0.42).
narrative_ontology:measurement(refo_su_t1530, reformation_composite__political_realignment_reading, suppression_requirement, 1530, 0.55).
narrative_ontology:measurement(refo_su_t1555, reformation_composite__political_realignment_reading, suppression_requirement, 1555, 0.68).
narrative_ontology:measurement(refo_su_t1600, reformation_composite__political_realignment_reading, suppression_requirement, 1600, 0.71).
narrative_ontology:measurement(refo_su_t1630, reformation_composite__political_realignment_reading, suppression_requirement, 1630, 0.72).
narrative_ontology:measurement(refo_su_t1648, reformation_composite__political_realignment_reading, suppression_requirement, 1648, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_composite__political_realignment_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(reformation_composite__political_realignment_reading, 0.12).
narrative_ontology:affects_constraint(reformation_composite__political_realignment_reading, reformation_composite__theological_fragmentation_reading).
narrative_ontology:affects_constraint(reformation_composite__political_realignment_reading, reformation_composite__technological_mediation_reading).
narrative_ontology:affects_constraint(reformation_composite__political_realignment_reading, territorial_sovereignty_doctrine).
narrative_ontology:affects_constraint(reformation_composite__political_realignment_reading, peace_of_westphalia_recognition).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the reformation_composite kernel. The political_realignment_reading focuses on how territorial rulers USED religious differentiation as a sovereignty lever. The theological_fragmentation_reading (sibling) focuses on genuine doctrinal incompatibilities generated by the Reformation. The technological_mediation_reading (sibling) focuses on the printing press's role in amplifying local dissent into continental movement. All three are constraints on the same historical event; they have different ε values, different beneficiary/victim structures, and different observable mechanisms. The three readings coexist as live analytical positions. Decomposition follows the ε-invariance principle: the core observable (cuius regio eius religio vs. theological consistency vs. information diffusion) is fundamentally different across readings, so they are structurally distinct constraints, not one constraint viewed from three angles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
