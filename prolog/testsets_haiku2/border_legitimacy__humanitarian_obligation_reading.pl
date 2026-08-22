% ============================================================================
% CONSTRAINT STORY: border_legitimacy__humanitarian_obligation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_legitimacy__humanitarian_obligation_reading, []).

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
 *   constraint_id: border_legitimacy__humanitarian_obligation_reading
 *   human_readable: Humanitarian Obligation / Economic Migrant Exclusion Reading
 *   domain: political_philosophy/international_law/migration
 *
 * SUMMARY:
 *   The humanitarian obligation reading of border legitimacy holds that
 *   states have a moral and legal obligation to admit persons fleeing
 *   systematic persecution or immediate survival threats (refugees, asylum
 *   seekers) but retain legitimate authority to exclude those seeking
 *   economic improvement (economic migrants). This reading generates a
 *   bifurcated victim set: those successfully categorized as refugees benefit
 *   (gain protected status), while economic migrants and those misclassified
 *   or unrecognized (internally displaced without persecution claims,
 *   climate-displaced before recognition) bear the cost of exclusion. The
 *   reading presents itself as achieving a principled balance between
 *   humanitarian obligation and state sovereignty. However, the distinction
 *   operates extractively: receiving states gain legitimacy (can claim
 *   humanitarianism while controlling immigration), humanitarian norm-setters
 *   gain authority to adjudicate who counts, border enforcement labor gains
 *   institutional roles, and the boundary-drawing power itself is
 *   concentrated in receiving-state institutions. The constraint is CLAIMED
 *   as tangled_rope (genuine coordination function: admitting refugees does
 *   solve a real problem of state-created persecution and displacement; the
 *   obligation is real) but the metrics suggest substantial extractive
 *   overlay (high suppression of the boundary, rising theater ratio
 *   indicating increasing disconnection between humanitarian rhetoric and
 *   exclusionary outcomes). This gap is the measurement the system takes.
 *
 * KEY AGENTS:
 *   - Receiving states: Set and enforce the refugee/economic migrant boundary; benefit from legitimacy it provides while maintaining selective immigration control
 *   - Refugees and asylum seekers: Powerless agents who gain protected status when categorized correctly; trapped if miscategorized
 *   - Economic migrants: Excluded by the reading; powerless, identity-locked to the category that bars them, bearing the cost of the boundary
 *   - Internally displaced persons unrecognized as refugees: Trapped in a gap; origin states cannot protect them, receiving states deny obligation because no international border was crossed
 *   - Border enforcement labor: Moderate power but constrained exit; implement the distinction daily despite ambiguity and moral difficulty
 *   - Humanitarian norm adjudicators (UNHCR, courts): Set what counts as persecution; benefit from the clarity and authority the distinction provides
 *   - Sovereignty absolutists and freedom-of-movement advocates: Excluded from the conversation the reading permits; their foundational premises are foreclosed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__humanitarian_obligation_reading, 0.58).
domain_priors:suppression_score(border_legitimacy__humanitarian_obligation_reading, 0.62).
domain_priors:theater_ratio(border_legitimacy__humanitarian_obligation_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__humanitarian_obligation_reading, tangled_rope).
narrative_ontology:human_readable(border_legitimacy__humanitarian_obligation_reading, "Humanitarian Obligation / Economic Migrant Exclusion Reading").
narrative_ontology:topic_domain(border_legitimacy__humanitarian_obligation_reading, "political_philosophy/international_law/migration").

domain_priors:requires_active_enforcement(border_legitimacy__humanitarian_obligation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__humanitarian_obligation_reading, '3edfceab-d0ca-4b79-a886-bed96539479d').
narrative_ontology:cs_kernel_codification('3edfceab-d0ca-4b79-a886-bed96539479d', formalized).
narrative_ontology:cs_authority_grounding('3edfceab-d0ca-4b79-a886-bed96539479d', lineage).
narrative_ontology:cs_interpretation_layer_present('3edfceab-d0ca-4b79-a886-bed96539479d').
narrative_ontology:cs_reading_relation('3edfceab-d0ca-4b79-a886-bed96539479d', border_legitimacy__sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('3edfceab-d0ca-4b79-a886-bed96539479d', border_legitimacy__freedom_of_movement_reading, coexists_with).
narrative_ontology:cs_axiom('3edfceab-d0ca-4b79-a886-bed96539479d', foundational, persecution_creates_obligation).
narrative_ontology:cs_axiom_status(persecution_creates_obligation, holdable).
narrative_ontology:cs_axiom_grounding('3edfceab-d0ca-4b79-a886-bed96539479d', persecution_creates_obligation, deontological).
narrative_ontology:cs_axiom('3edfceab-d0ca-4b79-a886-bed96539479d', foundational, state_sovereignty_legitimate).
narrative_ontology:cs_axiom_status(state_sovereignty_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('3edfceab-d0ca-4b79-a886-bed96539479d', state_sovereignty_legitimate, conventional).
narrative_ontology:cs_reference_frame('3edfceab-d0ca-4b79-a886-bed96539479d', post_wwii_persecution_asylum_framework).
narrative_ontology:cs_drift_state('3edfceab-d0ca-4b79-a886-bed96539479d', climate_displacement_era_2020s, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3edfceab-d0ca-4b79-a886-bed96539479d', '').
narrative_ontology:cs_kernel_id(border_legitimacy__humanitarian_obligation_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, receiving_states).
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, humanitarian_norm_adjudicators).
narrative_ontology:constraint_victim(border_legitimacy__humanitarian_obligation_reading, economic_migrants).
narrative_ontology:constraint_victim(border_legitimacy__humanitarian_obligation_reading, internally_displaced_persons_unrecognized).
narrative_ontology:constraint_victim(border_legitimacy__humanitarian_obligation_reading, border_enforcement_labor).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, refugees_asylum_seekers).
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, origin_state_governments).
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, receiving_state_publics).
narrative_ontology:constraint_victim(border_legitimacy__humanitarian_obligation_reading, internally_displaced_unrecognized).
narrative_ontology:constraint_victim(border_legitimacy__humanitarian_obligation_reading, receiving_state_publics).
narrative_ontology:constraint_vindicates(border_legitimacy__humanitarian_obligation_reading, humanitarian_obligation_doctrine).
narrative_ontology:constraint_vindicates(border_legitimacy__humanitarian_obligation_reading, refugee_distinction_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and enforce the refugee/economic migrant boundary through border control, visa policy, asylum adjudication. Control who counts as persecuted and therefore entitled to asylum. Benefit from the reading's legitimacy: can claim humanitarian credentials while excluding migrants. Implement suppression machinery to maintain boundary.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, receiving_states, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(border_legitimacy__humanitarian_obligation_reading, receiving_states, beneficiary).

% Fleeing persecution, war, systematic threats. Protected by the reading when successfully categorized as refugees. Powerless to contest categorization; trapped without international protection.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, refugees_asylum_seekers, beneficiary,
    powerless, immediate, trapped, universal).

% Seeking economic improvement, employment, opportunity. Excluded by the reading because classified as non-refugees. Bear the cost of exclusion without ability to contest the categorization. Trapped in the identity category that bars them.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, economic_migrants, payer,
    powerless, biographical, identity_locked, universal).

% Displaced by conflict or state failure within their origin country but not crossing international border or not meeting persecution criteria. Fall into gap: origin state cannot protect, receiving states deny obligation. Trapped both geographically and categorically.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, internally_displaced_unrecognized, payer,
    powerless, biographical, trapped, regional).

% Guards, adjudicators, detention officers implementing the boundary daily. Implement extraction through enforcement labor. Constrained by career dependency and institutional rules. Bear moral and psychological cost of categorical exclusion.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, border_enforcement_labor, payer,
    moderate, biographical, constrained, national).

% UNHCR, courts, human rights bodies. Set what counts as persecution. Benefit from authority and resources the reading provides. Defend the distinction against both sovereignty and freedom-of-movement challenges.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, humanitarian_norm_adjudicators, agenda_setter,
    institutional, generational, analytical, global).

% Benefit when citizens are excluded as economic migrants rather than admitted as refugees. Excluded from boundary-setting that occurs in receiving-state institutions.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, origin_state_governments, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(border_legitimacy__humanitarian_obligation_reading, origin_state_governments, excluded).

% Argue border restrictions violate human rights. Excluded from the humanitarian reading's conversation because it accepts state border authority.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, freedom_movement_advocates, excluded,
    organized, generational, analytical, global).

% Argue states have absolute right to exclude anyone. Excluded because the humanitarian reading posits obligation rather than discretion.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, sovereignty_absolutists, excluded,
    institutional, generational, analytical, national).

% Human smugglers and trafficking networks profit from the gap created by the boundary. Excluded from formal conversation but materially benefit from the distinction's exclusions.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, irregular_migration_networks, excluded,
    organized, biographical, trapped, global).

% Benefit from legitimacy narrative allowing selective immigration. Bear diffuse costs through public resources and community change.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, receiving_state_publics, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(border_legitimacy__humanitarian_obligation_reading, receiving_state_publics, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_legitimacy__humanitarian_obligation_reading, receiving_states).
narrative_ontology:fixing_cost_class(border_legitimacy__humanitarian_obligation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes who states have humanitarian obligation to admit versus who they may legitimately exclude. Permits states to balance humanitarian claims against border control. Solves the post-WWII problem: how can states respect the human dignity of persecuted people while maintaining immigration authority?
% TRANSFER_FUNCTION: Transfers protected status and asylum access to those categorized as refugees fleeing persecution or disaster. Transfers exclusion and deportation risk to those classified as economic migrants. Transfers legitimacy and humanitarian authority to receiving states and humanitarian adjudicators. Transfers enforcement labor to border workers.
% ABSENT_VOICES: Economic migrants cannot contest their own categorization; internally displaced persons whose displacement does not cross borders or meet persecution criteria are excluded from the conversation; climate-displaced people are largely unrecognized in the current framework; origin state governments have minimal voice in defining what counts as persecution; freedom-of-movement advocates are excluded because the reading presupposes state border authority; sovereignty absolutists are excluded because the reading presupposes humanitarian obligation.
% DISAPPEARANCE_RATIONALE: If the humanitarian obligation reading and its refugee/economic migrant distinction vanished, receiving states would need a new framework: either shift to sovereignty-reading (no obligation to admit anyone), freedom-of-movement reading (presume right to move), or invent entirely new categories. The reading currently structures asylum law, humanitarian funding, international norm-setting, and public legitimacy narratives. Removal would require institutional reconstruction.
% FOUNDING_PROBLEM: Post-WWII systematic persecution and refugee crises required states to protect people fleeing genocide, fascism, and Cold War repression. The 1951 Refugee Convention created the persecution-based distinction to permit states to respond to legitimate survival threats while maintaining border control.
% FOUNDING_PROBLEM_CORROBORATION: Humanitarian organizations (UNHCR, Amnesty, Human Rights Watch) affirm persecution remains a live threat requiring protection. States and receiving-state governments acknowledge the problem persists. However, climate scientists, migration economists, and development scholars (World Bank, IOM, academic migration research) increasingly argue the founding problem has shifted: climate displacement, state collapse, and economic desperation now drive most migration; the persecution-based distinction no longer describes the actual drivers. Economic migrants themselves argue that survival-level economic need should trigger equivalent humanitarian obligation. The corroboration from outside benefiting parties (scholars, scientists) points toward the problem having changed.
narrative_ontology:disappearance_verdict(border_legitimacy__humanitarian_obligation_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__humanitarian_obligation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__humanitarian_obligation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(border_legitimacy__humanitarian_obligation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(border_legitimacy__humanitarian_obligation_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_legitimacy__humanitarian_obligation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_legitimacy__humanitarian_obligation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_legitimacy__humanitarian_obligation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness measurement (0.58 at interval end) reflects that the humanitarian obligation reading operates as a legitimation mechanism: it permits states to appear humanitarian while excluding the majority of migrants who seek entry. The extraction is not crude coercion but structural: the categorical distinction itself is what enables the arrangement. Suppression (0.62) is high because maintaining the boundary requires active enforcement (border control, visa adjudication, detention of those awaiting deportation, exclusion of smuggling networks). Theater is moderate and rising (0.25→0.41 over the interval): as climate displacement and state collapse have grown, the humanitarian reading's founding justification (persecution-based asylum for Holocaust/Cold War refugees) has become increasingly disconnected from actual displacement drivers. The constraint now spends more energy performing humanitarianism (admitting some refugees, international refugee law frameworks) while excluding the people actually displaced (economic desperation, climate, state failure). The measurement series traces this: extractiveness and suppression initially rise as the constraint tightens in response to migration pressure, theater ratio then rises as the disconnect between founding justification and current operation becomes visible. The agent divergence is sharp: receiving-state agenda setters experience this as legitimate coordination; economic migrants experience it as exclusion justified by category-drawing they did not consent to; humanitarian adjudicators experience it as principled obligation; border workers experience it as categorical boundary-enforcement with ambiguous moral status.
 *
 * PERSPECTIVAL GAP:
 *   The engine should compute five distinct type classifications from this structural data. From the receiving-state seat: rope or tangled_rope (genuine humanitarian coordination function, legitimate enforcement, net benefit to the state). From the economic migrant seat: snare (pure exclusion justified by an arbitrary categorical distinction the receiving state controls, no coordination benefit visible, no exit). From the refugee/asylum seeker seat: tangled_rope (partial coordination benefit — they are protected if categorized correctly — but asymmetric: the receiving state controls categorization and can change the rules, creating extraction risk). From the border enforcement seat: tangled_rope or piton (coordinating legitimate asylum while also extracting exclusion labor; unclear if the function is still primary or if theater has taken over). From the humanitarian adjudicator seat: rope (genuine coordination on who counts as persecuted; no personal extraction; analytical position). The reading's legitimacy depends on these diverging perspectives being compatible — that receiving states can be genuinely humanitarian while economically motivated migrants are legitimately excluded. But the measurement data suggests the compatibility is increasingly performative (rising theater ratio). The claim and metrics are intentionally divergent: the constraint claims to be tangled_rope (coordination + asymmetric extraction balanced by humanitarian principle) but the metrics suggest rising theater (the humanitarian principle is increasingly a cover story).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality diverges sharply by seat. Receiving states: d ≈ 0.15 (full beneficiary, low target). They set the rules, collect legitimacy, control the enforcement machinery, and face no consequence if they apply the distinction narrowly. They can exit the humanitarian obligation entirely by shifting to sovereignty reading (which they control rhetorically). Refugees/asylum seekers: d ≈ 0.95 (full target). Powerless, trapped, dependent on the categorization mechanism receiving states operate. Their exit is unavailable; their alternative is return to persecution. Economic migrants: d ≈ 1.0 (full target). The reading creates the category that excludes them; they bear the entire cost. Identity-locked (the category is their social position); trapped (excluded precisely because they are classified as non-refugees). Internally displaced unrecognized: d ≈ 0.98 (full target). Excluded from the obligation framework; trapped both in origin territory and in category. Border enforcement labor: d ≈ 0.72 (substantial target). Constrained exit (career dependent on the system); moderate power (some discretion in adjudication but bounded by rules); implement the extraction through their daily work. Humanitarian norm-setters: d ≈ 0.05 (full beneficiary). They gain authority and resources from the arrangement; they have analytical exit (can shift interpretations); they face no cost if their readings produce harmful exclusions. Receiving-state publics: d ≈ 0.55 (near symmetric). They benefit from the legitimacy narrative and may receive economic benefits from selective migration; they also bear diffuse costs (public resources, community frictions). These directionalities are structurally stable across the interval because the reading's power distribution does not shift.
 *
 * MANDATROPHY ANALYSIS:
 *   The humanitarian obligation reading avoids misclassification as pure rope by naming the victims (economic migrants, internally displaced, misclassified persons) and the extraction mechanism (categorical exclusion). It avoids misclassification as snare by pointing to the genuine coordination function: admitting refugees does solve a real problem, and the obligation is genuinely recognized in law and practice. The reading clarifies that this is not pure coordination (rope) because the bifurcation itself — who counts as a refugee — is controlled by receiving states and operated to their advantage. The constraint is tangled_rope: real coordination in the refugee pathway, real extraction in the boundary-drawing and economic-migrant exclusion. The mandate itself (humanitarian obligation post-WWII persecution) has not become obsolete — persecution remains real — but the constraint's effectiveness at addressing what people actually need has degraded. Economic desperation, climate displacement, and state collapse now drive more migration than persecution, yet the reading's categories treat these as non-obligatory. The founding problem status (contested) reflects this: humanitarian organizations insist the problem persists; scholars argue the problem has shifted and the reading is no longer adequate. The theater ratio's rise suggests the constraint is increasingly performing humanitarianism rather than delivering it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    persecution_vs_desperation_boundary,
    'Is the distinction between systematic persecution (recognized, obligating admission) and economic desperation (not recognized, permitting exclusion) a real categorical difference or a continuum that the reading artificially partitions?',
    'Empirical study: compare outcomes (mortality, poverty, unfreedom) for those classified as refugees vs. those classified as economic migrants post-admission. If long-term outcomes are similar (both groups faced survival-level need), the distinction is arguably post-hoc narrative rather than pre-existing category.',
    'If the boundary is continuous, then the humanitarian obligation reading conceals that it is excluding people facing survival-level need; if the boundary is real, the reading correctly distinguishes obligation types. If continuous, the reading is operating as legitimation of exclusion rather than a genuine obligation framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persecution_vs_desperation_boundary, empirical, 'Whether persecution and desperation are categorically distinct or points on a survival-need continuum.').

omega_variable(
    climate_refugee_boundary_shift,
    'Does the humanitarian obligation reading''s founding problem (persecution-based asylum) apply to climate-driven displacement and state collapse, or does the emergence of climate refugees show the reading was built for a different era and the founding problem has changed?',
    'Climate science data on displacement from climate change; analysis of how many current asylum seekers cite climate factors vs. persecution; legal developments recognizing climate displacement as triggering humanitarian obligation.',
    'If climate displacement becomes recognized as persecution-equivalent, the reading''s obligation expands. If the distinction holds (climate displacement is economic desperation, not persecution), the reading permits exclusion of the majority of future displacement drivers. The reading''s legitimacy depends on its applicability to the actual drivers of displacement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(climate_refugee_boundary_shift, conceptual, 'Whether the humanitarian obligation reading''s categories remain applicable as the causes of displacement shift.').

omega_variable(
    categorical_harm_of_misclassification,
    'What is the measured harm from false negatives (people needing protection classified as economic migrants) vs. false positives (people not needing protection classified as refugees) in the boundary-drawing process?',
    'Audit of asylum adjudication outcomes: track harm (deportation to persecution, trafficking, death) for those rejected as economic migrants vs. harm from those accepted as refugees who were not facing persecution.',
    'If false negatives cause greater harm (death, persecution of the rejected), the obligation reading should error on the inclusive side; if false positives are more costly, states can justify stricter lines. The current reading''s legitimacy assumes the harm from exclusion is lower than the harm from inclusion, but this is empirically contestable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_harm_of_misclassification, empirical, 'Whether the categorical boundary''s errors distribute in ways that justify its strictness.').

omega_variable(
    kernel_reading_contest_structure,
    'This reading is one of three competing interpretations of the border legitimacy kernel: which reading''s foundational axioms are logically compatible, and which are genuinely incompatible within a single framework?',
    'Conceptual analysis: can a state simultaneously hold (1) humanitarian obligation to admit persecuted people, (2) freedom of movement as a human right, and (3) territorial sovereignty as legitimate authority? Or do the axioms force choice?',
    'If the readings are genuinely incompatible (forecloses relation), the kernel contest is a choice between incommensurable frameworks. If they coexist (coexist_with), the kernel hosts multiple legitimate readings. This determines whether the constraint''s validity is universal or reading-dependent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_structure, conceptual, 'The logical relationships among the humanitarian, sovereignty, and freedom-of-movement readings of border legitimacy.').

omega_variable(
    internal_vs_international_boundary_asymmetry,
    'Why does the humanitarian obligation reading recognize persecution within national borders (triggering asylum obligation when crossed internationally) but not movement within borders (internally displaced persons don''t trigger the same obligation absent international crossing)?',
    'Analysis of whether the international-border requirement is structurally necessary to the humanitarian obligation or is a contingent feature that could be reformed.',
    'If the international-border requirement is contingent, the reading could be extended to cover internally displaced persons equally; if it is structural to how state sovereignty interacts with humanitarian obligation, the reading is correct to distinguish them. The current narrowness may conceal that internally displaced persons are excluded not because they face lower threat, but because they have not crossed a border the international community monitors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internal_vs_international_boundary_asymmetry, conceptual, 'Whether the humanitarian obligation reading''s reliance on international borders is a necessary feature or a reformable choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__humanitarian_obligation_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(border_humanitarian_tr_t0, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(border_humanitarian_tr_t0, observed).
narrative_ontology:measurement(border_humanitarian_tr_t6, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 6, 0.29).
narrative_ontology:measurement_basis(border_humanitarian_tr_t6, observed).
narrative_ontology:measurement(border_humanitarian_tr_t12, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement_basis(border_humanitarian_tr_t12, observed).
narrative_ontology:measurement(border_humanitarian_tr_t18, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 18, 0.39).
narrative_ontology:measurement_basis(border_humanitarian_tr_t18, observed).
narrative_ontology:measurement(border_humanitarian_tr_t24, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 24, 0.41).
narrative_ontology:measurement_basis(border_humanitarian_tr_t24, observed).
narrative_ontology:measurement(border_humanitarian_tr_t30, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(border_humanitarian_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(border_humanitarian_be_t0, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(border_humanitarian_be_t0, observed).
narrative_ontology:measurement(border_humanitarian_be_t6, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 6, 0.52).
narrative_ontology:measurement_basis(border_humanitarian_be_t6, observed).
narrative_ontology:measurement(border_humanitarian_be_t12, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement_basis(border_humanitarian_be_t12, observed).
narrative_ontology:measurement(border_humanitarian_be_t18, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 18, 0.57).
narrative_ontology:measurement_basis(border_humanitarian_be_t18, observed).
narrative_ontology:measurement(border_humanitarian_be_t24, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 24, 0.59).
narrative_ontology:measurement_basis(border_humanitarian_be_t24, observed).
narrative_ontology:measurement(border_humanitarian_be_t30, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(border_humanitarian_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(border_humanitarian_su_t0, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(border_humanitarian_su_t0, observed).
narrative_ontology:measurement(border_humanitarian_su_t6, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 6, 0.57).
narrative_ontology:measurement_basis(border_humanitarian_su_t6, observed).
narrative_ontology:measurement(border_humanitarian_su_t12, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 12, 0.6).
narrative_ontology:measurement_basis(border_humanitarian_su_t12, observed).
narrative_ontology:measurement(border_humanitarian_su_t18, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 18, 0.62).
narrative_ontology:measurement_basis(border_humanitarian_su_t18, observed).
narrative_ontology:measurement(border_humanitarian_su_t24, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 24, 0.63).
narrative_ontology:measurement_basis(border_humanitarian_su_t24, observed).
narrative_ontology:measurement(border_humanitarian_su_t30, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement_basis(border_humanitarian_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_legitimacy__humanitarian_obligation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(border_legitimacy__humanitarian_obligation_reading, 0.12).
narrative_ontology:affects_constraint(border_legitimacy__humanitarian_obligation_reading, border_legitimacy__sovereignty_reading).
narrative_ontology:affects_constraint(border_legitimacy__humanitarian_obligation_reading, border_legitimacy__freedom_of_movement_reading).

% DUAL FORMULATION NOTE:
% Part of border_legitimacy constraint family (three readings of one kernel). The humanitarian obligation reading distinguishes itself by accepting state border authority as legitimate while positing moral obligation to admit persecuted people. The sovereignty reading rejects the obligation entirely; the freedom-of-movement reading rejects the border authority itself. All three are readings of the same kernel (state authority over borders + migration rights). Each has distinct ε, distinct beneficiary/victim structure, distinct type classification. Siblings linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
