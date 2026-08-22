% ============================================================================
% CONSTRAINT STORY: article_9_war_renunciation__strict_pacifist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_9_war_renunciation__strict_pacifist_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: article_9_war_renunciation__strict_pacifist_reading
 *   human_readable: Article 9 Strict Pacifist Reading — Categorical Military Prohibition
 *   domain: constitutional_law/security_policy
 *
 * SUMMARY:
 *   This story instantiates the strict pacifist reading of Article 9 of the
 *   Japanese Constitution: the position that the clause 'land, sea, and air
 *   forces, as well as other war potential, will never be maintained' is a
 *   categorical, textually unambiguous prohibition on any organized military
 *   force, defensive or otherwise. Under this reading, war renunciation is
 *   absolute and self-defense must be achieved exclusively through
 *   non-military means (diplomacy, police-level capacity) or through
 *   dependence on an external security guarantor. This reading stands in
 *   acknowledged tension with 70+ years of continuous Self-Defense Forces
 *   existence, which the reading treats as an ongoing constitutional
 *   violation rather than a resolved question. This is a KERNEL READING: two
 *   sibling constraints (inherent_right_reading,
 *   collective_self_defense_reading) exist as separate files describing the
 *   same textual kernel read differently; each has its own epsilon, its own
 *   stakeholders, and its own classification, linked here only through
 *   network edges and cs_structure.reading_relations, never folded into this
 *   constraint's own metrics.
 *
 * KEY AGENTS:
 *   - pacifist_civil_society_organizations: beneficiary (organized/mobile) — gains legitimacy and institutional standing from strict textualism
 *   - constitutional_originalist_scholars: beneficiary/agenda_setter (institutional/constrained) — professional authority rides on textual consistency
 *   - self_defense_forces_personnel: payer (moderate/constrained) — bear permanent legitimacy uncertainty
 *   - frontline_island_populations: payer (powerless/trapped) — most exposed to the practical security gap the reading refuses to close
 *   - united_states_security_guarantor: beneficiary/agenda_setter (institutional/arbitrage) — structural beneficiary of Japan's alliance dependence
 *   - constitutional_courts: observer (institutional/analytical) — repeatedly declines to resolve the underlying contradiction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__strict_pacifist_reading, 0.58).
domain_priors:suppression_score(article_9_war_renunciation__strict_pacifist_reading, 0.62).
domain_priors:theater_ratio(article_9_war_renunciation__strict_pacifist_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__strict_pacifist_reading, tangled_rope).
narrative_ontology:human_readable(article_9_war_renunciation__strict_pacifist_reading, "Article 9 Strict Pacifist Reading — Categorical Military Prohibition").
narrative_ontology:topic_domain(article_9_war_renunciation__strict_pacifist_reading, "constitutional_law/security_policy").

domain_priors:requires_active_enforcement(article_9_war_renunciation__strict_pacifist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__strict_pacifist_reading, 'e3c0387a-2a65-4be4-b3e4-b0ca61e5f7f5').
narrative_ontology:cs_kernel_codification('e3c0387a-2a65-4be4-b3e4-b0ca61e5f7f5', fixed_text).
narrative_ontology:cs_authority_grounding('e3c0387a-2a65-4be4-b3e4-b0ca61e5f7f5', lineage).
narrative_ontology:cs_interpretation_layer_present('e3c0387a-2a65-4be4-b3e4-b0ca61e5f7f5').
narrative_ontology:cs_reading_relation('e3c0387a-2a65-4be4-b3e4-b0ca61e5f7f5', article_9_war_renunciation__inherent_right_reading, forecloses).
narrative_ontology:cs_reading_relation('e3c0387a-2a65-4be4-b3e4-b0ca61e5f7f5', article_9_war_renunciation__collective_self_defense_reading, forecloses).
narrative_ontology:cs_axiom('e3c0387a-2a65-4be4-b3e4-b0ca61e5f7f5', foundational, categorical_textual_prohibition_of_war_potential).
narrative_ontology:cs_axiom_status(categorical_textual_prohibition_of_war_potential, holdable).
narrative_ontology:cs_axiom_grounding('e3c0387a-2a65-4be4-b3e4-b0ca61e5f7f5', categorical_textual_prohibition_of_war_potential, conventional).
narrative_ontology:cs_axiom('e3c0387a-2a65-4be4-b3e4-b0ca61e5f7f5', foundational, no_inherent_sovereign_defense_exception_survives_the_text).
narrative_ontology:cs_axiom_status(no_inherent_sovereign_defense_exception_survives_the_text, holdable).
narrative_ontology:cs_axiom_grounding('e3c0387a-2a65-4be4-b3e4-b0ca61e5f7f5', no_inherent_sovereign_defense_exception_survives_the_text, conventional).
narrative_ontology:cs_reference_frame('e3c0387a-2a65-4be4-b3e4-b0ca61e5f7f5', occupation_era_categorical_prohibition).
narrative_ontology:cs_drift_state('e3c0387a-2a65-4be4-b3e4-b0ca61e5f7f5', contemporary_sdf_normalization, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('e3c0387a-2a65-4be4-b3e4-b0ca61e5f7f5', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__strict_pacifist_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, pacifist_civil_society_organizations).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, constitutional_originalist_scholars).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, regional_neighbors_wary_of_remilitarization).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, state_security_autonomy).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, self_defense_forces_personnel).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, frontline_island_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, united_states_security_guarantor).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocacy groups, war-memory associations, and legal scholars who organize around the literal text of Article 9 as a moral achievement and bulwark against remilitarization. They gain political legitimacy, funding, and institutional standing from defending the strict reading; their exit from this position would mean abandoning a decades-long identity project.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, pacifist_civil_society_organizations, beneficiary,
    organized, generational, mobile, national).

% Legal academics and constitutional court advisors whose scholarly authority rests on close textual reading of 'never be maintained.' They set interpretive agenda in court challenges to Self-Defense Force legality and gain professional standing from the strict reading's internal consistency, even as it produces an unresolved 70-year gap between text and practice.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, constitutional_originalist_scholars, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__strict_pacifist_reading, constitutional_originalist_scholars, agenda_setter).

% Neighboring states with historical memory of Japanese wartime aggression benefit diplomatically from Japan's constitutional inability to maintain armed forces, using it as a benchmark against which any expansion of Japanese military capacity is measured as constitutional violation.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, regional_neighbors_wary_of_remilitarization, beneficiary,
    institutional, generational, analytical, regional).

% The state's capacity to independently determine and execute its own defense posture is the structural casualty of the strict reading: under this reading, any organized force is categorically unconstitutional, so genuine security autonomy is foreclosed by the text itself rather than by policy choice.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, state_security_autonomy, payer,
    institutional, civilizational, trapped, national).
narrative_ontology:stakeholder_non_agent(article_9_war_renunciation__strict_pacifist_reading, state_security_autonomy).

% Approximately 250,000 uniformed personnel serve in an organization whose constitutional legality is, under this reading, permanently contested. They bear the psychological and professional cost of institutional illegitimacy — every deployment, budget line, and promotion occurs under an unresolved cloud that this reading refuses to lift by legal fiction.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, self_defense_forces_personnel, payer,
    moderate, biographical, constrained, national).

% Residents of the Nansei/Ryukyu island chain facing the most direct exposure to regional military pressure. Under the strict reading, their security depends entirely on non-military diplomacy or on US alliance forces stationed on their land, since indigenous defensive capacity is categorically prohibited — they cannot vote for a locally accountable defense posture.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, frontline_island_populations, payer,
    powerless, biographical, trapped, regional).

% Provides the alliance-based security substitute the strict reading requires since Japan cannot maintain its own military. Benefits from permanent basing rights, strategic leverage, and a dependent ally; can adjust troop levels and treaty terms unilaterally in ways Japan cannot reciprocate.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, united_states_security_guarantor, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__strict_pacifist_reading, united_states_security_guarantor, agenda_setter).

% Politicians and citizens favoring formal constitutional amendment to explicitly authorize defensive forces are structurally sidelined by a status quo where reinterpretation by cabinet legal opinion, not formal amendment, has resolved the practical question — leaving the strict textual reading intact in law while irrelevant in practice, denying revision advocates the debate they seek.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, constitutional_revision_advocates, excluded,
    organized, generational, constrained, national).

% Japanese courts have repeatedly declined to rule squarely on Self-Defense Force constitutionality, treating it as a political question. They observe the tension between the strict textual reading and sustained state practice without resolving it, effectively leaving the contradiction to persist unadjudicated.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_9_war_renunciation__strict_pacifist_reading, diffuse).
narrative_ontology:fixing_cost_class(article_9_war_renunciation__strict_pacifist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The strict reading coordinates a durable, internationally legible signal that Japan has permanently and categorically foreclosed a return to organized military capacity — a costly, credible commitment device that reassures neighbors and constrains future governments regardless of who is in power.
% TRANSFER_FUNCTION: Moves security-autonomy costs from the abstract national interest onto frontline populations and Self-Defense Forces personnel (who bear the practical and legitimacy costs of defense without constitutional cover), while moving diplomatic and reputational benefits to pacifist civil society, regional neighbors, and the U.S. as alliance guarantor.
% ABSENT_VOICES: Constitutional revision advocates and frontline island populations most exposed to regional security pressure are structurally absent from the interpretive process — the question is resolved by cabinet reinterpretation and unadjudicated judicial avoidance, not by the amendment process or a referendum that would surface their preferences directly.
% DISAPPEARANCE_RATIONALE: If the strict pacifist reading were abandoned (replaced by the inherent-right or collective-self-defense readings, or formal amendment), Japan's defense posture, alliance dynamics, regional diplomatic signaling, and the legal status of 250,000 SDF personnel would all shift immediately and substantially — this is not a dormant technicality but an actively load-bearing interpretive commitment.
% FOUNDING_PROBLEM: Drafted in 1946 under Allied Occupation to permanently prevent a defeated, recently ultranationalist-militarist Japan from rearming and threatening its neighbors again; the categorical text was meant to foreclose any interpretive path back to organized force.
% FOUNDING_PROBLEM_CORROBORATION: Pacifist civil society and originalist scholars (benefiting parties) attest the founding problem remains live — the risk of remilitarization is treated as permanent. Outside corroboration is mixed: independent constitutional historians and comparative-law scholars note that Japan's Self-Defense Forces have existed continuously since 1954 under sustained state practice and cabinet reinterpretation, and regional security analysts (not beneficiaries of either the SDF or pacifist civil society specifically) document Japan operating one of the world's larger defense budgets in practice — suggesting the textual prohibition's founding problem has been functionally superseded by state practice even as the strict reading refuses to concede this in law.
narrative_ontology:disappearance_verdict(article_9_war_renunciation__strict_pacifist_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_9_war_renunciation__strict_pacifist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__strict_pacifist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_9_war_renunciation__strict_pacifist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_9_war_renunciation__strict_pacifist_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_9_war_renunciation__strict_pacifist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_9_war_renunciation__strict_pacifist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_9_war_renunciation__strict_pacifist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.58) because the strict reading extracts real costs from SDF personnel and frontline populations — legitimacy uncertainty, dependence on an external guarantor, and foreclosure of locally accountable defense policy — while producing genuine coordination value (a costly, credible non-remilitarization signal) for pacifist organizations, scholars, and regional neighbors. Suppression is substantial (0.62) because maintaining the strict reading against 70 years of contrary state practice requires active judicial avoidance (courts treating SDF legality as a non-justiciable political question) and continuous reinterpretive labor rather than resolution. Theater ratio is notably high and rising (0.10 to 0.48) because an increasing share of the reading's maintenance is performative — everyone acts as if the prohibition binds while the state simultaneously operates a large defense budget and force structure, a textbook case of the letter being preserved while the practice diverges. Accessibility collapse is moderate (0.4): unlike a genuine mountain, workable alternative readings (inherent-right, collective self-defense) are visibly available and actively contested, so alternatives have not collapsed — they compete openly. Resistance is high (0.72): the reading is actively resisted by revision advocates, by the government's own cabinet reinterpretations, and by decades of state practice that runs contrary to the strict text.
 *
 * DIRECTIONALITY LOGIC:
 *   Pacifist civil society, originalist scholars, and regional neighbors sit near the beneficiary end: they collect legitimacy, professional standing, or diplomatic reassurance from the strict reading without bearing its practical security costs. Self-defense forces personnel and frontline island populations sit near the target end: they carry the reading's practical consequences (permanent legitimacy ambiguity, alliance dependence, foreclosed local defense autonomy) with limited exit — personnel cannot easily leave a career built inside a constitutionally contested institution, and island residents cannot relocate the geography that makes them frontline. The United States occupies an unusual beneficiary position with arbitrage-grade exit: it can adjust basing and treaty terms unilaterally, benefiting from Japan's structural dependence without symmetric obligation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing a defeated militarist state from rearming) is genuinely contested as live vs. dead: pacifist beneficiaries insist the risk remains permanent and justifies the categorical reading; independent historians and comparative-law scholars note the problem has been functionally superseded by 70+ years of stable democratic civilian control operating alongside a large defense establishment. Classifying this as tangled_rope rather than snare or mountain prevents two mislabeling errors: treating it as pure extraction would miss the genuine, still-functioning international credibility signal the strict reading provides; treating it as a mountain (natural, costless, uncontested) would miss that it requires continuous active suppression of a contrary reading (judicial avoidance, reinterpretive gymnastics) and produces identifiable victims among SDF personnel and frontline populations who bear its practical costs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    text_versus_practice_gap_resolution,
    'Is the 70-year gap between the strict textual prohibition and continuous Self-Defense Forces operation best understood as an unresolved constitutional violation (this reading''s own position), a de facto amendment through sustained state practice, or evidence that the strict reading itself misreads original drafter intent?',
    'A definitive Japanese Supreme Court ruling squarely addressing SDF constitutionality (rather than continued avoidance via the political-question doctrine), or a formal constitutional amendment process that would settle the textual question directly.',
    'A ruling upholding SDF legality under the existing text would effectively supersede the strict reading''s classification as tangled_rope in favor of treating it as a superseded/piton-adjacent historical position; a ruling striking down the SDF would validate the strict reading''s core claim and likely elevate its extractiveness score as enforcement costs of actual demobilization would materialize.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(text_versus_practice_gap_resolution, empirical, 'Whether the practice-versus-text gap resolves toward the strict reading''s own predicted consequences or against them.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Given that the same clause ''never be maintained'' produces three structurally distinct and materially different constraints (strict pacifist, inherent right, collective self-defense) depending on interpretive method, which reading''s classification should inform Japanese defense policy analysis, and does the SCOPE manifest''s selection of this reading over its siblings reflect genuine textual primacy or contingent political salience at the time of authoring?',
    'Comparative analysis of Diet debate records, cabinet legal bureau opinions across administrations, and academic commentary weighting each reading''s textual and historical support; document convergence or divergence across independent legal traditions (comparative constitutional scholars outside Japan).',
    'If the strict reading is shown to be a minority position among constitutional scholars despite majority support among pacifist civil society, this materially affects how much weight the reading''s classification should carry relative to its siblings in any aggregate policy analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether selecting the strict pacifist reading as a distinct constraint reflects genuine interpretive weight or reflects the salience of pacifist advocacy in the source material.').

omega_variable(
    state_security_autonomy_naturalness,
    'Is ''state security autonomy'' a legitimate victim category at all, or does treating its foreclosure as a cost presuppose a realist security paradigm that the strict pacifist reading explicitly rejects as its founding premise?',
    'Political theory analysis of whether security autonomy is a normatively neutral baseline (making its foreclosure a cost) or itself a contested value the pacifist tradition denies has independent worth (making its ''foreclosure'' not a cost from within that tradition''s own terms).',
    'If security autonomy is not a neutral baseline, the extractiveness score attributable to state_security_autonomy as victim may be substantially inflated by an outside frame the reading itself would reject; if it is a neutral analytic category independent of normative commitment, the victim declaration stands as authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_security_autonomy_naturalness, conceptual, 'Whether naming state security autonomy as a victim imports an external realist framework the reading contests.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__strict_pacifist_reading, 1946, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1946, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 1946, 0.1).
narrative_ontology:measurement(arti_tr_t1960, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 1960, 0.25).
narrative_ontology:measurement(arti_tr_t1980, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 1980, 0.35).
narrative_ontology:measurement(arti_tr_t2000, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 2000, 0.42).
narrative_ontology:measurement(arti_tr_t2015, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 2015, 0.46).
narrative_ontology:measurement(arti_tr_t2024, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 2024, 0.48).

% Extraction over time
narrative_ontology:measurement(arti_be_t1946, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 1946, 0.2).
narrative_ontology:measurement(arti_be_t1960, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 1960, 0.35).
narrative_ontology:measurement(arti_be_t1980, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 1980, 0.45).
narrative_ontology:measurement(arti_be_t2000, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 2000, 0.5).
narrative_ontology:measurement(arti_be_t2015, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 2015, 0.55).
narrative_ontology:measurement(arti_be_t2024, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1946, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 1946, 0.5).
narrative_ontology:measurement(arti_su_t1960, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 1960, 0.55).
narrative_ontology:measurement(arti_su_t1980, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 1980, 0.58).
narrative_ontology:measurement(arti_su_t2000, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(arti_su_t2015, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 2015, 0.61).
narrative_ontology:measurement(arti_su_t2024, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, article_9_war_renunciation__inherent_right_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, article_9_war_renunciation__collective_self_defense_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, us_japan_security_treaty).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the article_9_war_renunciation kernel, each authored as a separate file with its own epsilon per the epsilon-invariance principle. strict_pacifist_reading treats the constitutional text as categorically foreclosing military capacity (this file); inherent_right_reading treats it as foreclosing only aggressive war while preserving minimum defensive capacity; collective_self_defense_reading extends the inherent right to defending allies. The three readings are NOT averaged or reconciled into one constraint — they compete as live interpretive positions held by different institutional actors, and the network edges here exist to support contamination/influence analysis across the family, not to merge their classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
