% ============================================================================
% CONSTRAINT STORY: politeness_face_negotiation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_politeness_face_negotiation, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: politeness_face_negotiation
 *   human_readable: The Face Negotiation Constraint (Politeness Theory)
 *   domain: social/linguistics
 *
 * SUMMARY:
 *   The face negotiation constraint operates universally across human
 *   societies as a system for managing the inherent threat that social
 *   interaction poses to individual identity and autonomy. Brown & Levinson's
 *   politeness theory identifies two core face-needs — positive face (desire
 *   to be liked/accepted) and negative face (desire for freedom from
 *   imposition) — that generate unavoidable tension in any communicative act.
 *   Individuals use linguistic and behavioral strategies to maintain their
 *   own face while protecting others' faces, creating a complex negotiation
 *   system. However, this universal coordination mechanism is deeply
 *   asymmetrical: politeness norms systematically protect the face of
 *   status-maintainers (institutional authorities, high-status individuals)
 *   at the cost of subordinates, stigmatized groups, and those with less
 *   structural power. The constraint exhibits both genuine coordination
 *   function (managing conflict, enabling complex social cooperation) and
 *   extractive asymmetry (forcing subordinates to suppress authentic
 *   expression while protecting superiors). The theater ratio trend
 *   (0.35→0.58 over 10 years) reflects increasing performativity: digital
 *   communication enables alternatives to face-to-face politeness theater,
 *   yet formal institutions (schools, workplaces, government) maintain
 *   politeness performance increasingly as ritual rather than functional
 *   necessity. The measured extractiveness decline (0.42→0.38) reflects
 *   partially successful counter-politeness activism and normalization of
 *   direct speech in some social contexts, but suppression remains high
 *   (0.42) because exit from politeness norms carries significant social
 *   cost.
 *
 * KEY AGENTS:
 *   - Stigmatized Groups: Primary victims (powerless/trapped) — forced to absorb face-threatening acts without direct response; highest experienced extraction
 *   - Subordinates in Hierarchies: Secondary victims (moderate/constrained) — must use deferential language and suppress disagreement; bear significant extraction but have some exit options
 *   - Status-Maintaining Agents: Primary beneficiaries (powerful/mobile) — protected by asymmetric face-preservation norms; experience mixed coordination benefit and extraction advantage
 *   - Institutional Authorities: Institutional beneficiary (institutional/arbitrage) — use politeness as low-overhead coordination mechanism; faces minimal extraction
 *   - Social Justice Movements: Organized opponents (organized/constrained) — actively dismantling politeness norms through counter-politeness strategies; creating sunset pressure
 *   - Digital-Native Communities: Emergent alternative (moderate/mobile) — developing different politeness systems through platform affordances; may create genuine exit routes
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks seeing face-threat as immutable rather than culturally contingent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(politeness_face_negotiation, 0.38).
domain_priors:suppression_score(politeness_face_negotiation, 0.42).
domain_priors:theater_ratio(politeness_face_negotiation, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(politeness_face_negotiation, extractiveness, 0.38).
narrative_ontology:constraint_metric(politeness_face_negotiation, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(politeness_face_negotiation, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(politeness_face_negotiation, tangled_rope).
narrative_ontology:human_readable(politeness_face_negotiation, "The Face Negotiation Constraint (Politeness Theory)").
narrative_ontology:topic_domain(politeness_face_negotiation, "social/linguistics").

domain_priors:requires_active_enforcement(politeness_face_negotiation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(politeness_face_negotiation, status_maintainers).
narrative_ontology:constraint_beneficiary(politeness_face_negotiation, institutional_authority_holders).
narrative_ontology:constraint_victim(politeness_face_negotiation, autonomous_agents).
narrative_ontology:constraint_victim(politeness_face_negotiation, stigmatized_groups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STIGMATIZED AGENT (SNARE) — Trapped by politeness norms that require acceptance of face-threatening acts without direct confrontation. Must suppress authentic grievance expression to maintain social standing. High suppression (cannot voice objection directly), high extractiveness (bears cost of forced compliance). Cannot exit without severe social penalty.
constraint_indexing:constraint_classification(politeness_face_negotiation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SUBORDINATE IN HIERARCHY (SNARE) — Constrained by politeness requirements that privilege higher-status agent's face over their own. Must use indirect requests, deferential language, and suppress disagreement. Experiences extraction through forced deference. Exit options exist but carry significant cost (social ostracism, employment risk).
constraint_indexing:constraint_classification(politeness_face_negotiation, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATUS-MAINTAINING AGENT (TANGLED ROPE) — Benefits from politeness norms that protect their positive face and autonomy through enforcement against subordinates. Also benefits from coordination function: politeness enables complex social coordination and avoids conflict escalation. Active enforcement required to maintain asymmetry. Hybrid: genuine coordination need (conflict avoidance) mixed with extraction (asymmetric face protection).
constraint_indexing:constraint_classification(politeness_face_negotiation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: INSTITUTIONAL AUTHORITY (ROPE) — Benefits from politeness as a coordination mechanism that enables efficient social ordering without constant enforcement. Politeness allows delegation of face-management to individuals — reduces institutional overhead. Experiences constraint as pure coordination: politeness solves the collective problem of managing multiple face-threats simultaneously. Low suppression in this perspective because enforcement is delegated and internalized.
constraint_indexing:constraint_classification(politeness_face_negotiation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: SOCIAL JUSTICE MOVEMENT (SCAFFOLD) — Organized agents (feminist theory, critical discourse analysis, social movements) see politeness as a temporary constraint being actively dismantled through counter-politeness strategies, direct communication norms, and reframing of silence as complicity. High suppression historically (forced conformity to politeness rules), but with sunset clause: explicit politeness rejection is becoming normatively acceptable in activist spaces. Theater ratio declining as direct speech gains legitimacy.
constraint_indexing:constraint_classification(politeness_face_negotiation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CULTURAL TRANSMISSION SYSTEM (PITON) — Politeness persists through socialization and institutional maintenance despite degraded coordination function. Modern communication technology (text, email, voice messaging) allows asynchronous, low-stakes interaction where politeness theater becomes expensive. Yet politeness norms persist through institutional inertia — maintained in formal settings, schools, and corporate environments despite reduced functional need. Theater ratio high (0.58) because much politeness is now performative rather than functionally necessary for coordination.
constraint_indexing:constraint_classification(politeness_face_negotiation, piton,
    context(agent_power(moderate),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational perspective, politeness appears to be an immutable constraint: all human societies develop face-management norms because social coordination unavoidably involves face-threat. The universal presence of politeness across cultures (Brown & Levinson 1987) and the cognitive universality of positive/negative face suggest this is a natural law of human interaction. However, this perspective risks naturalizing what may be a contingent institutional arrangement — the specific mechanisms, asymmetries, and enforcement methods vary radically across cultures.
constraint_indexing:constraint_classification(politeness_face_negotiation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(politeness_face_negotiation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(politeness_face_negotiation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(politeness_face_negotiation, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(politeness_face_negotiation, TR),
    TR >= 0.70.

:- end_tests(politeness_face_negotiation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. Politeness generates both coordination benefits (conflict avoidance, social cooperation) and extraction costs (forced compliance, suppressed expression). The value reflects that extraction is real but not total — agents retain agency in politeness strategy selection and can deploy tactical politeness for advantage. Suppression (0.42): Moderate-high. Significant barriers to exiting politeness include social ostracism, employment loss, and institutional sanctions. However, suppression is declining due to normative shifts around direct communication. Theater ratio (0.58): Moderate-high. Increasing performativity reflects that digital communication has reduced the functional necessity for face-to-face politeness theater, yet politeness persists through institutional inertia. The trend from 0.35→0.58 indicates that politeness is increasingly maintained as ritual rather than genuine coordination mechanism — classic piton trajectory. Temporal measurements show extractiveness declining slightly (0.42→0.38) as counter-politeness activism gains traction, while theater ratio rises as the remaining politeness becomes more performative.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates radical perspectival divergence based on structural position. The stigmatized agent sees pure extraction (Snare) — they bear costs with no experienced benefit. The subordinate sees extraction with no exit (also Snare/high-extraction side). The status-maintainer sees coordination problem that they've solved, with pleasant extraction asymmetry — tangled rope where the 'rope' (coordination) feels real to them and the asymmetry feels justified. The institutional authority sees pure coordination (Rope) — politeness is an elegant low-overhead mechanism for ordering society. The social justice movement sees a temporary constraint with a sunset (Scaffold) — counter-politeness strategies are actively dismantling the constraint. The cultural transmission system sees a degraded ritual (Piton) — politeness persists through inertia despite reduced functional necessity. The analytical observer risks seeing an immutable law (Mountain) but the perspectival evidence reveals this as naturalization: the universality of face-management does not require the specific asymmetric mechanisms that exist.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) encodes each agent's structural relationship to this constraint. Status-maintainers with arbitrage options (can exit but benefit from staying) derive low d values — they experience politeness as beneficial coordination. Subordinates with constrained exit (cannot leave without cost) and victim status derive high d values — they experience politeness as extraction. Powerless agents with trapped exit and no structural benefit derive maximum d values approaching 1.0 — they experience pure extraction with no agency. Institutional authorities with arbitrage exit and beneficiary status derive negative or near-zero d values — they appear to subsidize the system (receive coordination benefit with minimal cost). The sigmoid function f(d) converts these d values to experienced extractiveness multipliers: trapped powerless agents experience 1.4x+ multipliers, while institutional beneficiaries experience near-zero or negative multipliers. This explains the perspectival gap: the same constraint generates dramatically different experienced extractiveness based on structural position.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint resolves the mandatrophy by distinguishing genuine coordination function from extractive asymmetry. The coordination component is real: politeness does solve genuine collective action problems (managing face-threat, enabling complex social cooperation). The extraction component is also real: the enforcement of politeness norms is systematically asymmetric, protecting high-status agents at the cost of low-status agents. The constraint is neither pure rope (because extraction is significant) nor pure snare (because coordination function is genuine). Tangled rope classification captures this hybrid: it requires both beneficiaries (institutional authorities, status-maintainers who benefit from face-protection asymmetry) and victims (subordinates, stigmatized groups who bear suppression costs). The constraint requires active enforcement (internalized through socialization, institutional training, and social penalty) to maintain the asymmetry. The analytical observer's false summit classification (mountain) is detected by the ε-invariance test: if you measure politeness as 'pure universal face-management' you get ε≈0.05 (rope); if you measure it as 'enforced face-protection asymmetry' you get ε≈0.38 (tangled rope). These are different constraints — the former describes what politeness could be, the latter describes what it structurally is.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    face_threat_universality,
    'Is face-threat itself a universal feature of all human communication, or is it a culturally contingent concept that emerged from Western politeness theory?',
    'Cross-cultural linguistic analysis of politeness violations and their perception in societies that predate Brown & Levinson''s framework; comparison of face-management mechanisms in honor cultures, shame cultures, and guilt cultures',
    'If universal: politeness constraint is mountain-adjacent (near-immutable). If contingent: politeness is a cultural-institutional formation that could have been otherwise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(face_threat_universality, conceptual, 'Whether face-threat is universal or culturally contingent').

omega_variable(
    asymmetry_necessity,
    'Is the asymmetry between status-maintainers and subordinates a necessary feature of politeness, or could politeness systems exist without hierarchical face-protection asymmetry?',
    'Study of egalitarian communities and peer-to-peer interaction norms; analysis of politeness systems in non-hierarchical social groups; examination of whether asymmetry persists or breaks down in conditions of genuine status equality',
    'If necessary: extraction is inherent to politeness (piton/snare perspectives confirmed). If contingent: politeness could be reorganized as symmetric coordination without extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asymmetry_necessity, empirical, 'Whether status asymmetry is inherent to politeness').

omega_variable(
    digital_native_politeness,
    'Do digital-native communities (Discord, Reddit, TikTok) develop functional alternatives to Brown & Levinson politeness, or do they reproduce the same face-management constraints through different mechanisms (pseudonymity, blocking, algorithmic reputation)?',
    'Ethnographic analysis of politeness violation handling in digital spaces; comparison of suppression levels and extraction asymmetries across digital vs in-person contexts; longitudinal tracking of politeness norm formation in new platforms',
    'If alternatives exist: politeness constraint is contingent and platform-dependent (scaffold/piton perspectives become primary). If reproduced: face-management is robust across media (mountain perspective gains support).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(digital_native_politeness, empirical, 'Whether digital spaces create genuine alternatives to politeness constraints').

omega_variable(
    enforcement_internalization_threshold,
    'At what point does politeness shift from external enforcement (social penalty) to internalized norm (shame/guilt), and does this internalization reduce or amplify extractiveness?',
    'Psychological measurement of intrinsic vs extrinsic motivation for politeness compliance; neuroscientific data on violation processing; cross-generational variation in enforcement mechanisms vs internalized shame',
    'If internalization reduces externally-perceived extraction: politeness appears as rope/scaffold from beneficiary perspective. If internalization deepens extracted cost: suppression increases despite appearance of voluntary compliance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_internalization_threshold, empirical, 'How enforcement internalization affects perceived extractiveness').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(politeness_face_negotiation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(polite_tr_t0, politeness_face_negotiation, theater_ratio, 0, 0.35).
narrative_ontology:measurement(polite_tr_t5, politeness_face_negotiation, theater_ratio, 5, 0.48).
narrative_ontology:measurement(polite_tr_t10, politeness_face_negotiation, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(polite_be_t0, politeness_face_negotiation, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(polite_be_t5, politeness_face_negotiation, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(polite_be_t10, politeness_face_negotiation, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(politeness_face_negotiation, enforcement_mechanism).
narrative_ontology:affects_constraint(politeness_face_negotiation, status_hierarchy_reproduction).
narrative_ontology:affects_constraint(politeness_face_negotiation, authentic_self_suppression).
narrative_ontology:affects_constraint(politeness_face_negotiation, institutional_compliance_theater).

% DUAL FORMULATION NOTE:
% The face negotiation constraint decomposes into two related but distinct claims: (1) Face-management is a universal feature of human communication (ε≈0.05, Mountain), and (2) Politeness norms systematically enforce asymmetric face-protection benefiting high-status agents (ε≈0.38, Tangled Rope). The first describes an intrinsic property of social communication; the second describes a contingent institutional structure built on top of universal face-management. This story focuses on the second claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(politeness_face_negotiation, institutional, 0.08).
constraint_indexing:directionality_override(politeness_face_negotiation, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
