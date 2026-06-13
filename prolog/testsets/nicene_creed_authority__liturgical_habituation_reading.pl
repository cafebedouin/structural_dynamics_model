% ============================================================================
% CONSTRAINT STORY: nicene_creed_authority__liturgical_habituation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_creed_authority__liturgical_habituation_reading, []).

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
 *   constraint_id: nicene_creed_authority__liturgical_habituation_reading
 *   human_readable: Nicene Creed as Liturgical Identity Boundary (Habituation Reading)
 *   domain: religious/social/ecclesial
 *
 * SUMMARY:
 *   The Nicene Creed (325 CE, revised at Constantinople 381 CE) is a
 *   liturgical formula recited in corporate worship by Christians across most
 *   traditional denominations. This story traces ONE reading of the contested
 *   kernel 'nicene_creed_authority': the liturgical habituation reading,
 *   which frames the creed as a coordination mechanism for communal identity
 *   through embodied, rhythmic performance — independent of whether
 *   participants cognitively assent to or understand the metaphysical claims
 *   it makes. Under this reading, the creed functions as a rope — genuine
 *   coordination of communal boundaries — with minimal extractiveness (ε ≤
 *   0.10) and low suppression. The claim and the metrics are authored
 *   independently: the claimed type is rope; the metrics describe very low
 *   extraction and moderate-to-high theater ratio, consistent with a practice
 *   that is functionally more performative than extractive. The measurement
 *   series tracks nearly 1700 years: suppression requirement declines over
 *   time as enforcement of heretical exclusion becomes institutionalized and
 *   less actively contested; theater ratio rises as the creed's cognitive
 *   metaphysical content becomes more distant from liturgical function while
 *   the ritual practice itself becomes more entrenched.
 *
 * KEY AGENTS:
 *   - liturgical_communities: Participants in rhythmic recitation; mark and maintain identity through embodied performance independent of metaphysical assent.
 *   - theological_specialists: Defend or reinterpret the creed's content; constrained by need to justify why the community performs what remains doctrinally contested.
 *   - bishops_and_councils: Enforce the creed as a boundary of communion; the enforcement mechanism is recitation, not cognitive interrogation.
 *   - heretical_communities: Excluded by refusal to recite; their exclusion is structural to how the creed marks the boundary.
 *   - christian_individuals: Inhabit the creed as liturgical practice; benefit from identity-without-interrogation, voluntary participation.
 *   - secular_historians: Observe the creed's social and institutional function independent of truth-claims.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_creed_authority__liturgical_habituation_reading, 0.08).
domain_priors:suppression_score(nicene_creed_authority__liturgical_habituation_reading, 0.12).
domain_priors:theater_ratio(nicene_creed_authority__liturgical_habituation_reading, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_creed_authority__liturgical_habituation_reading, rope).
narrative_ontology:human_readable(nicene_creed_authority__liturgical_habituation_reading, "Nicene Creed as Liturgical Identity Boundary (Habituation Reading)").
narrative_ontology:topic_domain(nicene_creed_authority__liturgical_habituation_reading, "religious/social/ecclesial").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_creed_authority__liturgical_habituation_reading, '905f90a6-6973-40fc-9b43-13074efdc693').
narrative_ontology:cs_kernel_codification('905f90a6-6973-40fc-9b43-13074efdc693', fixed_text).
narrative_ontology:cs_authority_grounding('905f90a6-6973-40fc-9b43-13074efdc693', lineage).
narrative_ontology:cs_interpretation_layer_present('905f90a6-6973-40fc-9b43-13074efdc693').
narrative_ontology:cs_reading_relation('905f90a6-6973-40fc-9b43-13074efdc693', nicene_creed_authority__strict_orthodox_reading, influences).
narrative_ontology:cs_reading_relation('905f90a6-6973-40fc-9b43-13074efdc693', nicene_creed_authority__symbolic_confessional_reading, influences).
narrative_ontology:cs_axiom('905f90a6-6973-40fc-9b43-13074efdc693', foundational, liturgical_habituation_constitutes_identity).
narrative_ontology:cs_axiom_status(liturgical_habituation_constitutes_identity, holdable).
narrative_ontology:cs_axiom_grounding('905f90a6-6973-40fc-9b43-13074efdc693', liturgical_habituation_constitutes_identity, conventional).
narrative_ontology:cs_axiom('905f90a6-6973-40fc-9b43-13074efdc693', foundational, creedal_recitation_independent_of_metaphysical_assent).
narrative_ontology:cs_axiom_status(creedal_recitation_independent_of_metaphysical_assent, holdable).
narrative_ontology:cs_axiom_grounding('905f90a6-6973-40fc-9b43-13074efdc693', creedal_recitation_independent_of_metaphysical_assent, empirically_contingent).
narrative_ontology:cs_reference_frame('905f90a6-6973-40fc-9b43-13074efdc693', creed_as_liturgical_practice).
narrative_ontology:cs_drift_state('905f90a6-6973-40fc-9b43-13074efdc693', contemporary_academic_theology, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('905f90a6-6973-40fc-9b43-13074efdc693', '').
narrative_ontology:cs_kernel_id(nicene_creed_authority__liturgical_habituation_reading, nicene_creed_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_creed_authority__liturgical_habituation_reading, liturgical_communities).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__liturgical_habituation_reading, doctrinal_transmission_infrastructure).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_creed_authority__liturgical_habituation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(nicene_creed_authority__liturgical_habituation_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_creed_authority__liturgical_habituation_reading_tests).
:- end_tests(nicene_creed_authority__liturgical_habituation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.08 at interval end) because the constraint does not systematically move resources from one party to another; it coordinates identity through practice. Suppression is also low (0.12 at interval end) and DECLINING over time, because early enforcement against heretical communities (Arianism, Nestorianism) gradually becomes institutionalized as baseline expectation and does not require active suppression — the boundary is assumed, not defended. Theater ratio is high (0.72) and RISING over time, because the functional work of the creed shifts: in the early councils it was actively debated metaphysical content; over centuries it becomes rhythmic recitation whose metaphysical content is rarely interrogated. The rising theater ratio signals that the creed's function (marking identity) is increasingly independent of its cognitive content (metaphysical claim). This is not theatricality-as-deception; it is the normal operation of a liturgical practice that performs identity through repetition. Accessibility collapse is moderate (0.42) because individuals can exit the practice by ceasing to participate in liturgy or joining communities with different creeds — the boundary is real but mobile, not trapped. Resistance is low (0.28) because the constraint itself is widely accepted as legitimate by those who participate; resistance comes from those who reject it as metaphysically unsound or culturally contingent, and they typically exit rather than resist from within.
 *
 * PERSPECTIVAL GAP:
 *   The creed's function appears radically different from different seats. From the perspective of bishops and councils, the creed is a boundary-setting statement that defines orthodoxy and secures communion — it is successful coordination. From the perspective of theological specialists, the creed is a constraint they must justify or reinterpret, given that its metaphysical content remains contested — they bear labor to defend or rehabilitate it. From the perspective of individual participants, the creed is a practice they inhabit; its truth or falsity is often secondary to its role in marking belonging. The engine computes these divergences from the structural data: the bishops have institutional power and stable time horizons (constrained exit); theologians are powerful but constrained to defend something contentious; individuals have moderate power and mobile exit. These differences in power, time horizon, and exit produce different experienced constraint types — but the creed itself remains a single coordination mechanism whose function persists across the divergent readings.
 *
 * DIRECTIONALITY LOGIC:
 *   Liturgical communities and Christian individuals are net beneficiaries: they receive identity confirmation, communal belonging, and continuity with tradition — d toward the beneficiary end (0.1–0.3). Theological specialists are partly constrained: they benefit from the creed's authority anchoring their work, but they bear the labor of defending or reinterpreting something metaphysically contested — d near 0.5 (symmetric). The directionality_overrides entry (institutional power → d=0.45) adjusts bishops and councils slightly beneficiary-ward to capture that while they enforce the creed, they also depend on it for legitimacy and do not privately benefit from its extraction (because there is minimal extraction to benefit from). Heretical communities are excluded entirely — their exclusion is the structural point of the creed, not a side effect. The override reflects that institutional seats are constrained by administrative burden (maintaining the creed's legitimacy across divergent communities) and do not capture extraction rents; their role is coordinative, not extractive.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids the mandatrophy trap by locating the creed's function in coordination (identity marking) rather than in the adjudication of a metaphysical truth-claim. If the founding problem were 'settling the metaphysical content of Christ's divinity,' the creed would be degraded or zombie-like (the problem is not settled; it remains contested). But if the founding problem is 'maintaining institutional boundaries and communal identity across geographies and centuries,' the creed is fully functional — it solves that problem through the liturgical practice itself, independent of whether reciting participants agree on the metaphysical interpretation. The high theater ratio and low extractiveness are consistent: a constraint that is mostly performance (theater ~0.72) but extracts nothing (~0.08) is coordinate-functional, not extractive. This reading's logic prevents the mislabeling of a genuine social function as a degraded or false claim. The strict_orthodox_reading and symbolic_confessional_reading would interpret the theater and low extraction differently — the strict reading would see theater as a symptom of doctrinal drift (high theater = loss of real function); the symbolic reading would embrace theater as the authentic function all along (the creed was never meant to settle metaphysical content, only to witness communally). This reading treats theater as diagnostically neutral — it tracks how the creed's primary function has stabilized and internalized, not as a sign of decay or authenticity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    liturgical_vs_cognitive_boundary,
    'Is the creed''s function as communal identity marker independent of metaphysical truth-content, or does the liturgical practice presuppose and reinforce metaphysical assent beneath the surface?',
    'Ethnographic and interview study of actual communities: do participants experience the creed primarily as a marker of belonging (liturgical reading), or as a claim about reality that they cognitively assent to (metaphysical reading)? Do communities tolerate metaphysical dissent among those who recite, or is recitation understood to require assent?',
    'If independent, the constraint is a pure coordination mechanism (rope) with theater as a feature, not a defect. If the practice presupposes assent, suppression is higher than authored (internalized identity-fusion would raise d for many participants), and the constraint is a tangled rope with coordination and doctrinal enforcement coupled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liturgical_vs_cognitive_boundary, empirical, 'Whether the creed''s liturgical function is structurally independent of metaphysical assent or presupposes it.').

omega_variable(
    heretical_exclusion_mechanism,
    'Is the creed''s role in excluding heretical communities a constitutive feature of how it functions, or incidental to its primary role as identity marker?',
    'Institutional history: trace whether councils formulated the creed primarily to consolidate orthodoxy (exclude heresy) or to coordinate communion (unify the orthodox). Examine whether a given community ever maintained the creed while accepting theological diversity on its metaphysical content.',
    'If exclusion is incidental, the creed''s extractiveness remains low and it is a rope. If exclusion is constitutive, the creed coordinates via forced boundary-setting, raising its extractiveness and suppression — the constraint becomes a tangled rope (coordination of insiders + exclusion of outsiders).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(heretical_exclusion_mechanism, empirical, 'Whether the creed''s function includes active exclusion of heterodoxy or is primarily inclusive identity-marking.').

omega_variable(
    reading_indeterminacy_deep,
    'Is the choice between liturgical_habituation_reading, strict_orthodox_reading, and symbolic_confessional_reading resolvable by evidence, or is it fundamentally underdetermined by the facts?',
    'Test whether the three readings make different empirical predictions about how communities function, how dissent is handled, how metaphysical content drifts over time, how historical change in doctrine is explained. If predictions diverge, gather evidence. If predictions converge, the indeterminacy is conceptual, not empirical.',
    'If resolvable, the corpus should eventually narrow to one reading (or rank them by evidence quality). If underdetermined, the readings represent genuinely different framings of one kernel, and all three stories remain live in the corpus — the committer frame is correctly applied.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_indeterminacy_deep, conceptual, 'Whether the three readings of the creed kernel are empirically distinguishable or conceptually underdetermined.').

omega_variable(
    internalized_suppression_invisibility,
    'Does the measured decline in suppression_requirement over time reflect genuine change in how the creed is maintained, or does early external suppression (legal exclusion of heretics) migrate into internalized identity-fusion and become invisible in institutional records?',
    'Post-exit suppression trajectory: did individuals who left traditional Christianity report that suppression persisted after they stopped participating (indicating internalized identity-fusion), or did suppression cease (indicating structural suppression fully released)? Compare creed-related suppression in communities with high exit mobility (Reformation-era, modern Western) versus low exit mobility (medieval, geographically isolated).',
    'If suppression migrated to internalized form, the measured structural suppression (0.12 at t=1700) understates the actual suppressive force on participants whose identity is fused with creedal recitation. The constraint would compute higher d for many participants, and the classification might shift toward tangled rope (coordination + coerced identity-fusion). If suppression genuinely declined, the constraint remains a pure rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_invisibility, empirical, 'Whether the creed''s suppression mechanism is external and declining, or migrating into internalized identity-fusion and becoming invisible.').

omega_variable(
    kernel_reading_sibling_coexistence,
    'Can the three readings of the nicene_creed_authority kernel coexist within a single institutional framework, or does adopting one reading logically foreclose the others?',
    'Institutional history: examine whether councils and communities have simultaneously held positions consistent with more than one reading. Can a community affirm the creed''s binding authority (strict_orthodox) while also tolerating theological reinterpretation (symbolic_confessional) and downplaying cognitive assent (liturgical_habituation)?',
    'If coexistence is possible, the readings truly coexist_with each other (all three stories remain live, each valid from a different seat). If one reading forecloses the others, the constraint structure is simpler and one reading would replace the siblings. If readings influence each other without foreclosing, the network edges are correctly specified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_sibling_coexistence, empirical, 'Whether the three readings of the creed kernel can coexist in a single institutional framework or whether adoption of one logically forecloses the others.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_creed_authority__liturgical_habituation_reading, 0, 1700).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nice_tr_t0, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 0, 0.65).
narrative_ontology:measurement(nice_tr_t300, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 300, 0.68).
narrative_ontology:measurement(nice_tr_t700, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 700, 0.7).
narrative_ontology:measurement(nice_tr_t1100, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 1100, 0.72).
narrative_ontology:measurement(nice_tr_t1400, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 1400, 0.73).
narrative_ontology:measurement(nice_tr_t1700, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 1700, 0.72).

% Extraction over time
narrative_ontology:measurement(nice_be_t0, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 0, 0.06).
narrative_ontology:measurement(nice_be_t300, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 300, 0.07).
narrative_ontology:measurement(nice_be_t700, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 700, 0.08).
narrative_ontology:measurement(nice_be_t1100, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 1100, 0.08).
narrative_ontology:measurement(nice_be_t1400, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 1400, 0.09).
narrative_ontology:measurement(nice_be_t1700, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 1700, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(nice_su_t0, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(nice_su_t300, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 300, 0.28).
narrative_ontology:measurement(nice_su_t700, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 700, 0.18).
narrative_ontology:measurement(nice_su_t1100, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 1100, 0.12).
narrative_ontology:measurement(nice_su_t1400, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 1400, 0.1).
narrative_ontology:measurement(nice_su_t1700, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 1700, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_creed_authority__liturgical_habituation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(nicene_creed_authority__liturgical_habituation_reading, 0.06).
narrative_ontology:affects_constraint(nicene_creed_authority__liturgical_habituation_reading, nicene_creed_authority__strict_orthodox_reading).
narrative_ontology:affects_constraint(nicene_creed_authority__liturgical_habituation_reading, nicene_creed_authority__symbolic_confessional_reading).

% DUAL FORMULATION NOTE:
% The nicene_creed_authority kernel is instantiated by three structurally distinct readings: liturgical_habituation_reading (this story, ε≈0.08, rope), strict_orthodox_reading (high ε, tangled rope with metaphysical enforcement), and symbolic_confessional_reading (variable ε depending on how tradition authority is grounded). The three readings have different metrics and classifications because they attribute different functions to the creed. This story traces the creed's role as identity marker through embodied practice, independent of metaphysical resolution. The sibling readings trace its role as doctrinal boundary (strict) or as communal witness (symbolic). All three remain live in the institutional record; they represent different parties' framings of what the creed is for.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nicene_creed_authority__liturgical_habituation_reading, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
