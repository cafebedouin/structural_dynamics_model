% ============================================================================
% CONSTRAINT STORY: article_2_7_chapter_vii_tension__sovereignty_first_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_2_7_chapter_vii_tension__sovereignty_first_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: article_2_7_chapter_vii_tension__sovereignty_first_reading
 *   human_readable: Article 2(7) / Chapter VII Sovereignty-First Reading: Intervention Barrier
 *   domain: international_law/political_philosophy/security
 *
 * SUMMARY:
 *   The sovereignty-first reading of the Article 2(7) / Chapter VII tension
 *   holds that state sovereignty is foundational to international law, and
 *   intervention is strictly prohibited except where: (1) the target state
 *   consents, or (2) the UN Security Council authorizes under Chapter VII,
 *   limited to inter-state aggression. Under this reading, systematic
 *   atrocity committed by a state against its own population is an internal
 *   matter, shielded from external coercive intervention by the consent
 *   requirement and the veto. This reading benefits post-colonial and
 *   authoritarian states (who use sovereignty as an absolute shield) and
 *   great powers (who weaponize veto power over exceptions). It harms
 *   populations under atrocity (who have no mechanism to invoke intervention
 *   without their torturer's permission). The constraint operates as a snare:
 *   high extraction from atrocity victims, active enforcement through
 *   great-power veto and institutional norm-reinforcement, and suppression of
 *   alternative interpretations (humanitarian advocates and R2P advocates are
 *   systematically excluded from Security Council decision-making).
 *
 * KEY AGENTS:
 *   - Post-colonial states: primary beneficiary, invoke sovereignty as shield against re-colonization
 *   - Authoritarian regimes: primary beneficiary, use constraint to guarantee immunity from intervention on internal atrocities
 *   - Populations under atrocity: primary victim, trapped by consent requirement and veto power
 *   - Great powers: agenda-setters and enforcers, control Security Council veto and interpretation of Chapter VII
 *   - Humanitarian advocates: excluded, their moral and legal arguments are heard but carry no binding weight
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.78).
domain_priors:suppression_score(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.71).
domain_priors:theater_ratio(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_2_7_chapter_vii_tension__sovereignty_first_reading, snare).
narrative_ontology:human_readable(article_2_7_chapter_vii_tension__sovereignty_first_reading, "Article 2(7) / Chapter VII Sovereignty-First Reading: Intervention Barrier").
narrative_ontology:topic_domain(article_2_7_chapter_vii_tension__sovereignty_first_reading, "international_law/political_philosophy/security").

domain_priors:requires_active_enforcement(article_2_7_chapter_vii_tension__sovereignty_first_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_2_7_chapter_vii_tension__sovereignty_first_reading, 'ecc6b32b-16b5-4c86-b91e-9ca9829ee261').
narrative_ontology:cs_kernel_codification('ecc6b32b-16b5-4c86-b91e-9ca9829ee261', formalized).
narrative_ontology:cs_authority_grounding('ecc6b32b-16b5-4c86-b91e-9ca9829ee261', extraction).
narrative_ontology:cs_interpretation_layer_present('ecc6b32b-16b5-4c86-b91e-9ca9829ee261').
narrative_ontology:cs_reading_relation('ecc6b32b-16b5-4c86-b91e-9ca9829ee261', article_2_7_chapter_vii_tension__r2p_reading, coexists_with).
narrative_ontology:cs_axiom('ecc6b32b-16b5-4c86-b91e-9ca9829ee261', foundational, sovereignty_foundational_non_negotiable).
narrative_ontology:cs_axiom_status(sovereignty_foundational_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('ecc6b32b-16b5-4c86-b91e-9ca9829ee261', sovereignty_foundational_non_negotiable, deontological).
narrative_ontology:cs_axiom('ecc6b32b-16b5-4c86-b91e-9ca9829ee261', foundational, intervention_requires_explicit_authorization).
narrative_ontology:cs_axiom_status(intervention_requires_explicit_authorization, holdable).
narrative_ontology:cs_axiom_grounding('ecc6b32b-16b5-4c86-b91e-9ca9829ee261', intervention_requires_explicit_authorization, conventional).
narrative_ontology:cs_reference_frame('ecc6b32b-16b5-4c86-b91e-9ca9829ee261', state_sovereignty_foundational).
narrative_ontology:cs_drift_state('ecc6b32b-16b5-4c86-b91e-9ca9829ee261', contemporary_atrocity_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ecc6b32b-16b5-4c86-b91e-9ca9829ee261', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(article_2_7_chapter_vii_tension__sovereignty_first_reading, article_2_7_chapter_vii_tension).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__sovereignty_first_reading, post_colonial_states).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__sovereignty_first_reading, authoritarian_regimes).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__sovereignty_first_reading, populations_under_atrocity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercise absolute territorial control and veto over internal affairs under the sovereignty-first reading. This constraint shields them from external pressure on governance, human rights, or resource allocation. Their security depends on the prohibition against unilateral intervention being robust; they drafted Article 2(7) as hard protection against former colonial powers re-intervening under humanitarian pretext. They benefit from the constraint's enforcement by institutional majorities in the UN General Assembly and by great powers respecting sovereignty norms.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, post_colonial_states, beneficiary,
    institutional, generational, mobile, global).

% Operate without external constraint on internal repression, mass detention, genocide, or ethnic cleansing, provided they do not cross into inter-state aggression. The sovereignty-first reading grants them immunity from Chapter VII action and from unilateral intervention. Their regimes persist by exploiting this constraint's enforcement: international condemnation is permitted under the reading, but coercive intervention is barred absent Chapter VII authorization (which requires Security Council consensus, routinely blocked by great-power veto). The constraint's extraction comes from populations they govern.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, authoritarian_regimes, beneficiary,
    institutional, generational, trapped, global).

% Bear the costs of the sovereignty-first reading when their own governments commit systematic atrocity. Under this reading, international law forbids intervention without the consent of the very regime committing the atrocity or Security Council authorization (which great powers block for strategic partners). Populations have no exit: they cannot migrate without external assistance, cannot arm themselves without state support, and cannot invoke international protection without their tormentor's permission. The constraint locks them into the power of the regime extracting from them.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, populations_under_atrocity, payer,
    powerless, immediate, trapped, local).

% Control the interpretation and enforcement of Chapter VII through Security Council veto power. They weaponize the sovereignty-first reading selectively: blocking intervention against allies and client states (Russia on Syria, China on Myanmar, US on Saudi Arabia) while advocating intervention against rivals (US on Iraq, France on Libya). The constraint simultaneously benefits them (they invoke sovereignty against intervention in their sphere) and serves their strategic interests (they can block others' interventions). Their directionality is complex: they are both beneficiaries (sovereignty protection) and primary enforcers (veto power over exceptions).
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, great_powers, agenda_setter,
    institutional, generational, arbitrage, global).

% Would invoke the right to protect (R2P) and argue for intervention in cases of genocide, ethnic cleansing, and crimes against humanity. They are systematically excluded from the decision calculus under the sovereignty-first reading: their moral claims carry no legal weight, their expertise is heard but not determinative, and their advocacy is framed as neo-colonial interference. They have no seat at the Security Council and no veto power; they can mount diplomatic pressure but cannot override the constraint.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, humanitarian_advocates, excluded,
    organized, biographical, constrained, global).

% Navigate the sovereignty-first reading with ambivalence. They invoke it against intervention in their own sphere but also face pressure from domestic constituencies to intervene against atrocities. Some (UK, France) are permanent Security Council members and can enforce the constraint; others (Canada, Germany) are constrained by it. They generate the bulk of public ambivalence about the reading but cannot change it unilaterally without great-power consensus.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, liberal_democracies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_2_7_chapter_vii_tension__sovereignty_first_reading, great_powers).
narrative_ontology:fixing_cost_class(article_2_7_chapter_vii_tension__sovereignty_first_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a bright-line rule for the international system: territorial sovereignty is foundational, and intervention is an exception requiring explicit authorization (state consent or Security Council Chapter VII action against inter-state aggression). This avoids a war-of-all-against-all where any powerful state could invade any weaker one under humanitarian justification.
% TRANSFER_FUNCTION: Transfers decision authority over internal affairs to the state holding sovereignty, and transfers immunity from coercive intervention to regimes that respect the inter-state boundary. In practice, it moves the capacity to address internal atrocity from the international community to the perpetrating state, which has zero incentive to constrain itself.
% ABSENT_VOICES: Populations under atrocity are structurally excluded: they cannot petition for intervention, cannot invoke Chapter VII (only states can), and cannot withdraw consent from a regime that never obtained it. Civil society organizations, humanitarian advocates, and transitional justice experts would argue for conditional sovereignty (revoked when atrocity reaches a threshold) but are not seated at the Security Council or in the formal consent mechanism.
% DISAPPEARANCE_RATIONALE: If the sovereignty-first reading disappeared overnight and R2P or conditional-sovereignty reading took its place, intervention thresholds would drop, atrocity-response regimes would proliferate, great-power veto on humanitarian grounds would be circumvented, and the strategic calculus for authoritarian regimes would shift dramatically. Atrocities would face faster international response, though the question of who decides when the threshold is met would become itself contested. The constraint's persistence depends on great-power consensus to maintain it; absent that consensus, the reading would transform.
% FOUNDING_PROBLEM: Post-WWII order needed to prevent great powers from using 'intervention' as cover for re-colonization and conquest. The sovereignty-first reading emerged as protection for newly-independent states against the colonial powers that had recently ruled them. Article 2(7) was drafted as an absolute bar to intervention in internal affairs except through the United Nations and its Charter framework.
% FOUNDING_PROBLEM_CORROBORATION: Post-colonial states and great powers both affirm that the founding problem remains live: they argue re-colonization is a persistent threat and sovereignty protection is necessary. Humanitarian advocates and atrocity victims argue the founding problem is substantially solved (re-colonization is no longer plausible under 21st-century international law) but the constraint persists as rent collection protecting authoritarian regimes. The UN Office on Genocide Prevention and Amnesty International have produced reports stating that the founding problem has shifted: contemporary threats are internal atrocity, not external conquest, but the constraint structure remains optimized for the 1945 context.
narrative_ontology:disappearance_verdict(article_2_7_chapter_vii_tension__sovereignty_first_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_2_7_chapter_vii_tension__sovereignty_first_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_2_7_chapter_vii_tension__sovereignty_first_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_2_7_chapter_vii_tension__sovereignty_first_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_2_7_chapter_vii_tension__sovereignty_first_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__sovereignty_first_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_2_7_chapter_vii_tension__sovereignty_first_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) and rises slowly over the interval because the constraint's primary function (blocking intervention against allied authoritarian regimes) has become more valuable as atrocities have proliferated. The founding problem (preventing re-colonization) is solved, but the constraint persists because great powers benefit from selective enforcement. Suppression (0.71) is high and stable because enforcing the constraint requires actively blocking humanitarian intervention initiatives, suppressing R2P advocacy, and maintaining consensus among great powers on the veto. Theater ratio (0.42) is moderate because the constraint does maintain a real coordination function (prevents unilateral great-power conquest) alongside extractive immunity for atrocities. The reading is classified as snare rather than tangled_rope because the coordination benefit is now clearly secondary to the extraction: the founding problem (preventing great-power re-conquest) has been solved by other mechanisms (nuclear deterrence, integrated economies, institutional norms), and the constraint persists primarily to shield atrocity. Accessibility collapse (0.62) is moderate because alternative interpretations (R2P, conditional sovereignty) are intellectually available but institutionally blocked by veto power. Resistance (0.58) is moderate because humanitarian advocates and some great powers mount real resistance to the constraint, but lack the veto power to override it.
 *
 * PERSPECTIVAL GAP:
 *   The sovereignty-first reading is authored from the seat of post-colonial states and their institutional successors (the Non-Aligned Movement, the African Union, the ASEAN). From this seat, the constraint solves a real founding problem (preventing re-colonization) and remains necessary protection against great-power interference. The R2P reading (the sibling) is authored from the seat of humanitarian advocates, liberal democracies concerned with atrocity prevention, and populations under systematic violence. From these seats, the constraint is a dead founding problem (re-colonization is not a plausible threat in the 21st century) sustained to protect strategic interests. The two readings cannot both be true within a single legal framework: either sovereignty is foundational (this reading) or it is conditional on non-atrocity (R2P reading). Great powers maintain strategic ambiguity, invoking whichever reading serves their interest in a given case.
 *
 * DIRECTIONALITY LOGIC:
 *   Post-colonial states hold moderate power (institutional but without great-power veto) and benefit from the constraint (sovereignty shield). Their directionality is moderate toward the beneficiary end (d ≈ 0.15–0.25) because they benefit and have reasonable exit (they can opt into interventions they want or negotiate with great powers), but they are also constrained by the great-power veto and cannot unilaterally change the constraint. Great powers hold institutional power and control the veto; they benefit from selective enforcement and have maximum exit (they can override the constraint through Security Council consensus). Their directionality is complex: they are beneficiaries of the sovereignty shield for themselves and their allies (d ≈ 0.1–0.2) but also agenda-setters who enforce it for others (they extract from the enforcement power itself). Humanitarian advocates hold organized power but lack Security Council seats; they perceive the constraint as extractive but cannot exit (excluded), so directionality is high toward the target end (d ≈ 0.85). Atrocity victims hold powerless status and face identity lock (they cannot exit their state without external assistance); the constraint extracts from them absolutely (they bear the cost of atrocity with no recourse to international protection). Directionality is maximal (d ≈ 0.95). The constraint's enforcement depends on great-power consensus; if consensus breaks, the constraint weakens.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate of the sovereignty-first reading was to prevent great-power re-colonization. By objective measures (military conquest, direct territorial acquisition, colonial administration), the mandate is accomplished: no great power has re-colonized another state under humanitarian pretext since 1950. Yet the constraint persists and even strengthens, because great powers discovered that the sovereignty-first reading also shields their allies and clients from humanitarian intervention. The constraint has been captured: it persists not to solve the founding problem but to serve the secondary benefit of protecting authoritarian allies. This is the classic mandatrophy pattern: founding problem solved, constraint repurposed, new beneficiaries entrenched, original purpose archived. The theater ratio (0.42) reflects this: significant rhetorical effort goes into affirming sovereignty and the founding problem's continuing threat, even though the threat is substantially addressed. The classification as snare reflects the mandatrophy: it was designed as a rope (genuine coordination preventing re-colonization) but has devolved into a snare (extraction-focused immunity for atrocities). A mandatrophy reading of this constraint would recommend conditional reactivation of the founding problem (re-making it a rope if re-colonization threats returned) or sunsetting it in favor of R2P thresholds.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_obsolescence,
    'Is the founding problem (preventing great-power re-colonization) still a live threat to international order, or has it been definitively solved by other mechanisms (nuclear deterrence, economic integration, institutional norms)?',
    'Assess whether any great power has attempted or seriously prepared for territorial conquest since 1950. If zero instances, the founding problem is dead and the constraint''s persistence is explained by secondary benefits (protecting authoritarian allies). If one or more instances exist, the founding problem is live.',
    'If the founding problem is dead, the constraint is a degraded institution (piton) or a snare, depending on whether extraction is captured or diffuse. If live, it remains a genuine rope/tangled_rope balancing sovereignty and legitimate intervention. Classification hinges on this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the constraint''s founding justification persists or has been superseded.').

omega_variable(
    atrocity_threshold_definition,
    'Can a bright-line rule for intervention in response to atrocity be defined and enforced without re-opening the door to re-colonization under humanitarian pretense?',
    'Comparative analysis of regional intervention frameworks that include atrocity thresholds (African Union, ECOWAS) and their outcomes. Test whether interventions meeting the threshold are geographically skewed toward weak states (re-colonization pattern) or distributed neutrally across power levels.',
    'If thresholds can be enforced neutrally, R2P becomes viable and the sovereignty-first reading becomes optional. If thresholds are systematically abused against weaker states, they reproduce re-colonization patterns and vindicate the sovereignty-first position.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(atrocity_threshold_definition, empirical, 'Whether conditional sovereignty can be operationalized without reproducing colonialism.').

omega_variable(
    consent_mechanism_accessibility,
    'Under the sovereignty-first reading, who provides state consent to intervention: the regime committing atrocity, the opposition, the transitional authority, or some combination? Is there a defensible mechanism to deem a state unable to consent because it is itself the violator?',
    'Legal scholarship and case law on recognizing non-state actor consent (exiled governments, transitional authorities) and on the doctrine of parens patriae (state acting in the interest of the people when the state itself is the threat). Survey which states accept non-regime consent and under what circumstances.',
    'If non-regime consent can be recognized, a loophole opens in the sovereignty-first reading: intervention can be authorized by a representative of the people rather than the regime. If only regime consent is valid, the reading maintains logical coherence but entraps atrocity victims. If the question is left unresolved, the reading carries internal contradiction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consent_mechanism_accessibility, conceptual, 'Whether the consent requirement has a defensible interpretation when the state is the atrocity perpetrator.').

omega_variable(
    sibling_reading_logical_relationship,
    'Does the sovereignty-first reading logically foreclose the R2P reading, or do they coexist as different normative commitments that different parties hold simultaneously?',
    'Test whether a party holding the sovereignty-first position can coherently also hold that atrocity creates an obligation to intervene. If yes, coexists_with. If no, forecloses or influences (depending on whether the relationship is unidirectional or mutual).',
    'If forecloses: the two readings are competitors and only one can eventually prevail in international practice. If coexists_with: both readings are live options for different parties, and the constraint landscape is inherently contested. This determines whether the kernel is in long-term transformation or stable pluralism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_logical_relationship, conceptual, 'Logical relationship between this reading and the R2P sibling reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement_basis(arti_tr_t0, observed).
narrative_ontology:measurement(arti_tr_t10, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 10, 0.39).
narrative_ontology:measurement_basis(arti_tr_t10, observed).
narrative_ontology:measurement(arti_tr_t20, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(arti_tr_t20, observed).
narrative_ontology:measurement(arti_tr_t40, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(arti_tr_t40, observed).
narrative_ontology:measurement(arti_tr_t60, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 60, 0.42).
narrative_ontology:measurement_basis(arti_tr_t60, observed).
narrative_ontology:measurement(arti_tr_t80, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 80, 0.42).
narrative_ontology:measurement_basis(arti_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement_basis(arti_be_t0, observed).
narrative_ontology:measurement(arti_be_t10, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 10, 0.71).
narrative_ontology:measurement_basis(arti_be_t10, observed).
narrative_ontology:measurement(arti_be_t20, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 20, 0.74).
narrative_ontology:measurement_basis(arti_be_t20, observed).
narrative_ontology:measurement(arti_be_t40, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 40, 0.76).
narrative_ontology:measurement_basis(arti_be_t40, observed).
narrative_ontology:measurement(arti_be_t60, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 60, 0.77).
narrative_ontology:measurement_basis(arti_be_t60, observed).
narrative_ontology:measurement(arti_be_t80, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 80, 0.78).
narrative_ontology:measurement_basis(arti_be_t80, observed).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement_basis(arti_su_t0, observed).
narrative_ontology:measurement(arti_su_t10, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 10, 0.69).
narrative_ontology:measurement_basis(arti_su_t10, observed).
narrative_ontology:measurement(arti_su_t20, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(arti_su_t20, observed).
narrative_ontology:measurement(arti_su_t40, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(arti_su_t40, observed).
narrative_ontology:measurement(arti_su_t60, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 60, 0.71).
narrative_ontology:measurement_basis(arti_su_t60, observed).
narrative_ontology:measurement(arti_su_t80, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 80, 0.71).
narrative_ontology:measurement_basis(arti_su_t80, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_2_7_chapter_vii_tension__sovereignty_first_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.18).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__sovereignty_first_reading, article_2_7_chapter_vii_tension__r2p_reading).

% DUAL FORMULATION NOTE:
% The article_2_7_chapter_vii_tension kernel generates two distinct constraint stories: sovereignty_first_reading (this file) and r2p_reading. The readings coexist as different normative commitments held by different institutional actors. The kernel codifies the tension in the UN Charter; the readings represent the two poles of interpretation. Sovereignty-first reading prioritizes Article 2(7) and holds that intervention requires explicit consent or Chapter VII authorization limited to inter-state aggression. R2P reading prioritizes Chapter VII's implicit authorization for action in response to threats to peace and interprets systematic atrocity as such a threat. The readings have the same referent (when, if ever, intervention is permitted) but different ε values: sovereignty-first reading assesses the standing arrangement (current practice of non-intervention in internal atrocities) as highly extractive; R2P reading assesses the same arrangement as failing to protect populations. The ε difference is reading-indexed, not observable-dependent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_2_7_chapter_vii_tension__sovereignty_first_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
