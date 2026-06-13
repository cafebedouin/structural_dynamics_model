% ============================================================================
% CONSTRAINT STORY: us_constitution_interpretive__popular_constitutionalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_interpretive__popular_constitutionalism_reading, []).

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
 *   constraint_id: us_constitution_interpretive__popular_constitutionalism_reading
 *   human_readable: Constitutional Meaning via Popular Contestation (vs. Judicial Finality)
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   Popular constitutionalism is a political and jurisprudential reading that
 *   locates constitutional authority in democratic popular movements and
 *   political contestation rather than in judicial interpretation alone.
 *   Under this reading, the Constitution's meaning is shaped by what
 *   electoral coalitions, legislatures, and social movements can sustain and
 *   defend, not what judges declare. This creates a structural conflict:
 *   judicial finality (which historically protected minorities and
 *   constitutional settlements) is delegitimized in favor of majoritarian
 *   political power. The constraint operates as tangled_rope—it solves a
 *   genuine coordination problem (making constitutional meaning responsive to
 *   democracy) while simultaneously extracting from those who depend on
 *   counter-majoritarian judicial protection. The measurement series tracks
 *   the growth of extraction as the reading gains cultural and institutional
 *   influence, and the persistence of suppression as defenders of judicial
 *   finality are required to defend their methodological authority against
 *   allegations of elitism.
 *
 * KEY AGENTS:
 *   - popular_political_movements: primary beneficiary; mobilize constituencies to contest prior judicial doctrine; gain legitimacy from a frame that treats mass politics as constitutional authority
 *   - legislative_majorities: beneficiary; can assert legislation as popular constitutional vision and bypass judicial precedent if political support is strong enough
 *   - vulnerable_minorities_relying_on_counter_majoritarian_protection: primary victim; lose the finality and stability that judicial protection once offered; exposed to majoritarian challenges to rights previously settled by courts
 *   - constitutional_settlement_dependents: victim; institutional and individual actors whose plans depended on stable judicial doctrine now face uncertainty and re-litigation
 *   - judicial_finality_advocates: payer; their institutional prerogative is contested; must defend finality claims and cannot rely on their interpretive authority being automatically deferred to
 *   - constitutional_scholars_pluralist: observer; track how the reading reshapes the institutional landscape
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_interpretive__popular_constitutionalism_reading, 0.68).
domain_priors:suppression_score(us_constitution_interpretive__popular_constitutionalism_reading, 0.72).
domain_priors:theater_ratio(us_constitution_interpretive__popular_constitutionalism_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_interpretive__popular_constitutionalism_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_interpretive__popular_constitutionalism_reading, "Constitutional Meaning via Popular Contestation (vs. Judicial Finality)").
narrative_ontology:topic_domain(us_constitution_interpretive__popular_constitutionalism_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(us_constitution_interpretive__popular_constitutionalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_interpretive__popular_constitutionalism_reading, '6a00255b-897d-40c6-9154-d5cabb1ccff1').
narrative_ontology:cs_kernel_codification('6a00255b-897d-40c6-9154-d5cabb1ccff1', fixed_text).
narrative_ontology:cs_authority_grounding('6a00255b-897d-40c6-9154-d5cabb1ccff1', extraction).
narrative_ontology:cs_interpretation_layer_present('6a00255b-897d-40c6-9154-d5cabb1ccff1').
narrative_ontology:cs_reading_relation('6a00255b-897d-40c6-9154-d5cabb1ccff1', us_constitution_interpretive__us_constitution_interpretive_originalist, coexists_with).
narrative_ontology:cs_reading_relation('6a00255b-897d-40c6-9154-d5cabb1ccff1', us_constitution_interpretive__us_constitution_interpretive_living, coexists_with).
narrative_ontology:cs_axiom('6a00255b-897d-40c6-9154-d5cabb1ccff1', foundational, constitutional_meaning_is_democratic_contestation).
narrative_ontology:cs_axiom_status(constitutional_meaning_is_democratic_contestation, holdable).
narrative_ontology:cs_axiom_grounding('6a00255b-897d-40c6-9154-d5cabb1ccff1', constitutional_meaning_is_democratic_contestation, deontological).
narrative_ontology:cs_axiom('6a00255b-897d-40c6-9154-d5cabb1ccff1', foundational, judicial_supremacy_is_anti_democratic_elite_veto).
narrative_ontology:cs_axiom_status(judicial_supremacy_is_anti_democratic_elite_veto, holdable).
narrative_ontology:cs_axiom_grounding('6a00255b-897d-40c6-9154-d5cabb1ccff1', judicial_supremacy_is_anti_democratic_elite_veto, instrumental).
narrative_ontology:cs_reference_frame('6a00255b-897d-40c6-9154-d5cabb1ccff1', participatory_democratic_constitutional_authority).
narrative_ontology:cs_drift_state('6a00255b-897d-40c6-9154-d5cabb1ccff1', contemporary_institutional_contestation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6a00255b-897d-40c6-9154-d5cabb1ccff1', '').
narrative_ontology:cs_kernel_id(us_constitution_interpretive__popular_constitutionalism_reading, us_constitution_interpretive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, popular_political_movements).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, legislative_majorities).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, anti_elitist_constituencies).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, judicial_finality_advocates).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, constitutional_settlement_dependents).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, vulnerable_minorities_relying_on_counter_majoritarian_protection).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_interpretive__popular_constitutionalism_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(us_constitution_interpretive__popular_constitutionalism_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_interpretive__popular_constitutionalism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_interpretive__popular_constitutionalism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_interpretive__popular_constitutionalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.45 to 0.68 over the interval, tracking the growing cultural and institutional influence of popular-constitutionalism framing (shift in academic discourse, legislative assertions of constitutional authority, movement mobilization around 'the people's Constitution'). Suppression is high and stable (0.55→0.72): the constraint requires active suppression of counter-arguments that courts should protect minority rights and that constitutional stability requires judicial finality. Theater rises moderately (0.38→0.48) and plateaus: as the reading becomes more institutionalized, performative displays of 'the people's will' are necessary to sustain the frame, but the underlying political contestation remains real. Accessibility_collapse is moderate (0.61): once the reading gains currency, alternative frames (judicial supremacy, originalism, living-constitution) do not disappear; they persist as competing institutional and scholarly positions, but their cultural legitimacy is compromised. Resistance is substantial (0.58) because judicial elites and minority-rights advocates actively resist the frame. The claim/metric gap is intentional: claimed as tangled_rope (genuinely solves a coordination problem of democratic responsiveness, while extracting from minorities) yet the metrics describe a substantially extractive operation where suppression is required to hold it in place. This divergence is the measurement the engine computes—it flags potential false summits and falsifiable coordination claims.
 *
 * PERSPECTIVAL GAP:
 *   Beneficiary seats (popular movements, legislative majorities) experience this constraint as liberation and democratic empowerment: their political victories become constitutional victories, their mass mobilization becomes interpretive authority. Victim seats (vulnerable minorities, settlement dependents) experience it as exposure and dispossession: protections they relied on are now up for contestation, stability they planned around is now contingent on their ability to mobilize politically (which they cannot do). Judicial finality advocates experience it as delegitimization: their professional authority is under attack. The engine computes these divergences from the structural power and exit data: organized beneficiaries with mobile exit and institutional backing versus powerless victims with identity_locked or trapped exit and no political mobilizing capacity.
 *
 * DIRECTIONALITY LOGIC:
 *   Popular movements and legislative majorities benefit from the frame and have real political power to sustain it (low d, beneficiary end). Vulnerable minorities are trapped and powerless; the constraint extracts their counter-majoritarian protection in exchange for subjecting them to majoritarian political contestation (high d, target end). Judicial finality advocates are institutional (powerful) but their power derives from role, not from the political coalitions supporting popular constitutionalism (moderate-high d). The directionality is asymmetric across institutional power: powerful institutions (judiciary, legislatures) divide on the constraint; powerless individuals (minorities) suffer it uniformly. No override is needed because the beneficiary/victim declarations and exit-option asymmetries capture this structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy misclassification by being clearly tangled_rope rather than pure snare or pure rope. The coordination function (democratic responsiveness to popular will) is real and substantive—it solves a genuine problem of making constitutional interpretation accountable to electoral politics. The extraction (dispossession of minority protection, instability for settlement dependents) is also real and substantial, justifying the tangled-rope classification. A false snare reading would ignore the coordination function; a false rope reading would ignore the extraction and suppression. The mandatrophy resolution sits precisely at the junction: the constraint coordinates democratic contestation while extracting protection from the least politically powerful. This is not mandatrophy (function and extraction are both operative) but rather structural asymmetry: the coordination function benefits the powerful and organized; the extraction costs fall on the powerless and vulnerable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    majoritarian_tyranny_risk,
    'Does democratic contestation of constitutional meaning fundamentally expose vulnerable minorities to majoritarian tyranny, or does it create space for popular movements to expand rather than contract rights protections?',
    'Long-term empirical study of jurisdictions where popular-constitutionalism frameworks have influenced institutional practice: track whether electoral mobilization around constitutional meaning has expanded or contracted protections for vulnerable groups. Compare cases where courts were supreme (e.g., pre-Civil Rights) against cases where movements contested doctrine and won (Civil Rights Movement).',
    'If majoritarianism dominates, the constraint is closer to snare (pure extraction from minorities for benefit of majorities). If popular movements have expanded rights protections historically, the coordination function is real and the constraint remains tangled_rope but with evidence that beneficiaries are not limited to elites.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majoritarian_tyranny_risk, empirical, 'Whether popular-constitutionalism contestation systematically harms or protects vulnerable minorities').

omega_variable(
    judicial_finality_as_countermajoritarian_necessary,
    'Is judicial finality structurally necessary for the protection of individual rights against majorities, or have rights expanded without it (through political struggle and popular movements)?',
    'Comparative constitutional history: does the evidence from rights expansion show that judicial supremacy was necessary, or that popular movements and legislatures also drove rights protection without needing courts to have the last word? (E.g., did the 13th and 14th Amendments require judicial finality to survive, or did they survive because they reflected popular will?)',
    'If finality was necessary, then the extraction from vulnerable minorities who rely on counter-majoritarian courts is a genuine cost of the popular-constitutionalism reading, and the classification stands. If finality was not necessary and rights expanded through popular struggle, then the reading''s extraction is overstated and the coordination function may be even stronger than measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_finality_as_countermajoritarian_necessary, empirical, 'Whether counter-majoritarian judicial finality is necessary for rights protection or contingent on specific historical conditions').

omega_variable(
    elite_veto_versus_popular_empowerment,
    'Is the delegation of interpretive authority to courts a form of elite veto over democratic will, or a necessary institutional feature of limited constitutional government?',
    'Political-theory analysis of constitutional systems that lack judicial finality (e.g., UK parliamentary sovereignty, some civil-law systems) versus those that have it (US judicial review). Examine whether the absence of judicial finality correlates with better or worse protection for minorities and individual rights.',
    'If judicial finality is shown to be elite veto pure and simple, the beneficiary list should expand to include all constituencies harmed by elite interpretive gatekeeping, and extraction may be reassessed downward. If judicial finality is shown to correlate with minority protection, the extraction from vulnerable minorities is confirmed as real and substantial.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(elite_veto_versus_popular_empowerment, conceptual, 'Whether judicial finality functions primarily as democratic defense or as elite interpretive gatekeeping').

omega_variable(
    suppression_mechanism_internalized_versus_structural,
    'Is the suppression of alternative interpretive claims (e.g., that courts should have final word, that constitutional meaning is stable) due to structural exclusion from platforms and institutional power, or due to internalization of the popular-constitutionalism frame by subordinated actors who come to accept majoritarian contestation?',
    'Post-contestation surveys and qualitative research: if suppression is removed (e.g., courts reassert finality), do advocates of judicial supremacy emerge from latency, or has the frame been internalized such that they no longer mobilize? Also examine whether vulnerable minorities who lose counter-majoritarian protection internalize the popular-constitutionalism frame or remain latently opposed.',
    'If suppression is structural, it is exogenous and the constraint''s measured suppression (0.72) reflects real enforcement work. If suppression is internalized, the frame''s power is greater than the measured suppression suggests—the constraint persists because constituencies no longer believe alternatives are possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_versus_structural, empirical, 'Whether the suppression holding the popular-constitutionalism frame in place is structural or internalized').

omega_variable(
    reading_specificity_to_us_context,
    'Is popular constitutionalism a structurally coherent reading of the US Constitution specifically, or a more general claim about democratic legitimacy that happens to be applied to the US document and could apply equally to any constitutional text?',
    'Comparative analysis: does popular constitutionalism as a legitimizing frame appear in other constitutional democracies (e.g., France, Germany, Canada, India)? Does it have the same structural properties and beneficiary/victim distribution, or do those vary?',
    'If popular constitutionalism is specific to US constitutional structure (e.g., rooted in the Reconstruction Amendments'' popular ratification, the Civil Rights Movement''s specific history), then the constraint is culturally and institutionally rooted and may be less transferable. If the frame is more general, then sibling readings and the kernel decomposition apply across constitutional systems.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_specificity_to_us_context, conceptual, 'Whether popular constitutionalism is a US-specific reading or a general constitutional principle').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_interpretive__popular_constitutionalism_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(us_c_tr_t10, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 10, 0.42).
narrative_ontology:measurement(us_c_tr_t20, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 20, 0.45).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement(us_c_tr_t45, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 45, 0.49).
narrative_ontology:measurement(us_c_tr_t60, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 60, 0.48).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(us_c_be_t10, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(us_c_be_t20, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(us_c_be_t30, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(us_c_be_t45, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 45, 0.67).
narrative_ontology:measurement(us_c_be_t60, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 60, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(us_c_su_t10, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(us_c_su_t20, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(us_c_su_t30, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(us_c_su_t45, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 45, 0.72).
narrative_ontology:measurement(us_c_su_t60, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 60, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_interpretive__popular_constitutionalism_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(us_constitution_interpretive__popular_constitutionalism_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_interpretive__popular_constitutionalism_reading, us_constitution_interpretive_originalist).
narrative_ontology:affects_constraint(us_constitution_interpretive__popular_constitutionalism_reading, us_constitution_interpretive_living).
narrative_ontology:affects_constraint(us_constitution_interpretive__popular_constitutionalism_reading, supreme_court_finality_doctrine).
narrative_ontology:affects_constraint(us_constitution_interpretive__popular_constitutionalism_reading, legislative_constitutional_authority).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the us_constitution_interpretive kernel. The originalist_reading and living_constitution_reading are structurally distinct constraints with different ε values and beneficiary/victim structures, all instantiating different answers to 'who has authority to interpret the Constitution?' Each reading is a separate story with its own measurements and omegas. Popular constitutionalism influences and is influenced by doctrines of judicial finality and competing claims about legislative constitutional authority (affects_constraints). The sibling readings coexist as live positions held by different institutional and scholarly factions; no single reading forecloses the others within the political system as a whole, though each reading does constrain the others' space.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_interpretive__popular_constitutionalism_reading, institutional, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
